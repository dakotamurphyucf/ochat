open Core
module CM = Prompt.Chat_markdown
module Moderation = Moderation
module Moderator_manager = Moderator_manager
module Res = Openai.Responses
module Output = Res.Tool_output.Output

type post_stream =
  sw:Eio.Switch.t
  -> inputs:Openai.Responses.Item.t list
  -> Openai.Responses.Response_stream.t Seq.t

module Tool_dispatch = struct
  type rejection =
    | Invalid_input
    | Pre_tool
    | Pre_tool_failed
    | Session_ended

  type request =
    { kind : Tool_call.Kind.t
    ; original_name : string
    ; original_payload : string
    ; name : string
    ; payload : string
    ; rejection : rejection option
    ; call : History_entry.t
    ; history : History_entry.t list
    ; source : string option
    ; parent_call_id : string option
    }

  type result =
    { output : Output.t
    ; commit_output : (History_entry.t -> unit) option
    ; runtime_requests : Moderation.Runtime_request.t list
    }

  type t =
    { commit_call : request -> bool
    ; prepare_call :
        (request -> (Moderation.Tool_moderation.t option, string) Result.t) option
    ; validate_original :
        kind:Tool_call.Kind.t -> name:string -> payload:string -> (unit, string) Result.t
    ; run : request -> authorize:(unit -> unit) -> result option
    }

  let chain services =
    let prepare_call =
      match List.filter_map services ~f:(fun service -> service.prepare_call) with
      | [] -> None
      | [ prepare ] -> Some prepare
      | _ -> invalid_arg "tool dispatch requires a single host preparation policy"
    in
    { prepare_call
    ; commit_call =
        (fun request ->
          List.exists services ~f:(fun service -> service.commit_call request))
    ; validate_original =
        (fun ~kind ~name ~payload ->
          List.fold_result services ~init:() ~f:(fun () service ->
            service.validate_original ~kind ~name ~payload))
    ; run =
        (fun request ~authorize ->
          List.find_map services ~f:(fun service -> service.run request ~authorize))
    }
  ;;

  let with_preparation t ~prepare =
    match t.prepare_call with
    | None -> { t with prepare_call = Some prepare }
    | Some _ -> invalid_arg "tool dispatch already has a host preparation policy"
  ;;
end

exception Post_tool_moderation_failed of History_entry.t * string

(* --------------------------------------------------------------------------- *)
(* Internal helper – record used for keeping track of running tool invocations *)
(* --------------------------------------------------------------------------- *)

type driver_pending_call_kind =
  [ `Function
  | `Custom
  ]
[@@deriving equal]

type driver_pending_call =
  { seq : int
  ; call_id : string
  ; kind : driver_pending_call_kind
  ; name : string
  ; promise : Tool_dispatch.result Eio.Promise.or_exn
  }

module SM = Map.M (String)

type tool_info =
  { name : string
  ; call_id : string
  ; kind : driver_pending_call_kind
  }

type tool_completion =
  | Function_done of string
  | Custom_done of string

module Safe_point_input = struct
  type batch =
    { entries : History_entry.t list
    ; user_input : bool
    ; request_turn : bool
    }

  let empty = { entries = []; user_input = false; request_turn = false }

  let user_entries entries =
    { entries; user_input = not (List.is_empty entries); request_turn = false }
  ;;

  let notification_entries ~request_turn entries =
    { entries; user_input = false; request_turn }
  ;;

  let append first second =
    { entries = first.entries @ second.entries
    ; user_input = first.user_input || second.user_input
    ; request_turn = first.request_turn || second.request_turn
    }
  ;;

  type t =
    { consume_entries : unit -> batch
    ; consume_compatibility_text : unit -> string option
    }
end

type stream_state =
  { func_info : tool_info SM.t
  ; tool_completions : tool_completion SM.t
  ; new_entries_rev : History_entry.t list
  ; pending_calls_rev : driver_pending_call list
  ; next_seq : int
  ; run_again : bool
  }

type moderator_event_handlers =
  { before_model_call : unit -> (unit, string) result
  ; handle :
      history:History_entry.t list
      -> available_tools:Openai.Responses.Request.Tool.t list
      -> now_ms:int
      -> event:Moderation.Event.t
      -> (Moderation.Outcome.t option, string) result
  ; drain :
      history:History_entry.t list
      -> available_tools:Openai.Responses.Request.Tool.t list
      -> now_ms:int
      -> max_events:int
      -> (Moderation.Outcome.t list, string) result
  }

type moderator =
  { manager : Moderator_manager.t
  ; session_id : string
  ; session_meta : Jsonaf.t
  ; runtime_policy : Runtime_semantics.policy
  ; event_handlers : moderator_event_handlers option
  }

type pending_ui_request = Moderator_manager.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of
      { prompt : string
      ; choices : string array
      }

type moderated_tool_call =
  { call_item : Res.Item.t
  ; kind : Tool_call.Kind.t
  ; name : string
  ; payload : string
  ; synthetic_result : Res.Tool_output.Output.t option
  ; runtime_requests : Moderation.Runtime_request.t list
  }

let pending_ui_request (moderator : moderator) =
  Moderator_manager.pending_ui_request moderator.manager
;;

let resume_ui_request (moderator : moderator) ~response =
  Moderator_manager.resume_ui_request moderator.manager ~response
;;

type prepared_turn =
  { inputs : Res.Item.t list
  ; runtime_requests : Moderation.Runtime_request.t list
  }

type ctx =
  { env : Eio_unix.Stdenv.base
  ; sw : Eio.Switch.t
  ; datadir : Eio.Fs.dir_ty Eio.Path.t
  ; tools : Openai.Responses.Request.Tool.t list
  ; tool_tbl : (string, Ochat_function.runner) Hashtbl.t
  ; temperature : float option
  ; max_output_tokens : int option
  ; reasoning : Openai.Responses.Request.Reasoning.t option
  ; moderator : moderator option
  ; before_model_call : unit -> unit
  ; prepare_model_input :
      (history:History_entry.t list
       -> effective:Moderation.Effective_entry.t list
       -> History_entry.t list)
        option
  ; runtime_policy : Runtime_semantics.policy option
  ; on_runtime_request : Moderation.Runtime_request.t -> unit
  ; history_compaction : bool
  ; parallel_tool_calls : bool
  ; model : Openai.Responses.Request.model
  ; prompt_cache_key : string option
  ; prompt_cache_retention : string option
  ; safe_point_input : Safe_point_input.t option
  ; on_event : Openai.Responses.Response_stream.t -> unit
  ; on_sourced_event : Sourced_response_event.t -> unit
  ; on_history_event : History_stream_event.t -> unit
  ; on_history_item_appended : History_entry.t -> unit
  ; on_history_tool_out : History_entry.t -> unit
  ; allocator : History_entry.Allocator.t
  ; id_source : History_entry.Id_source.t
  ; registry : History_stream_event.Registry.t
  ; mutable scope : int
  ; source : string option
  ; parent_call_id : string option
  ; on_fn_out : Openai.Responses.Function_call_output.t -> unit
  ; on_tool_out : Openai.Responses.Item.t -> unit
  ; on_tool_execution : (Tool_execution_event.t -> unit) option
  ; authorize_tool :
      kind:Tool_call.Kind.t -> name:string -> payload:string -> call_id:string -> unit
  ; dispatch_tool : Tool_dispatch.t option
  ; redact_tool_payload : name:string -> string -> string
  ; injected_post_stream :
      (sw:Eio.Switch.t
       -> inputs:Openai.Responses.Item.t list
       -> Openai.Responses.Response_stream.t Seq.t)
        option
  }

type args =
  { env : Eio_unix.Stdenv.base
  ; datadir : Eio.Fs.dir_ty Eio.Path.t option
  ; history : History_entry.t list
  ; on_event : Openai.Responses.Response_stream.t -> unit
  ; on_sourced_event : Sourced_response_event.t -> unit
  ; on_history_event : History_stream_event.t -> unit
  ; on_history_item_appended : History_entry.t -> unit
  ; on_fn_out : Openai.Responses.Function_call_output.t -> unit
  ; on_tool_out : Openai.Responses.Item.t -> unit
  ; on_history_tool_out : History_entry.t -> unit
  ; allocator : History_entry.Allocator.t
  ; id_source : History_entry.Id_source.t
  ; on_tool_execution : (Tool_execution_event.t -> unit) option
  ; authorize_tool :
      kind:Tool_call.Kind.t -> name:string -> payload:string -> call_id:string -> unit
  ; dispatch_tool : Tool_dispatch.t option
  ; redact_tool_payload : name:string -> string -> string
  ; tools : Openai.Responses.Request.Tool.t list option
  ; tool_tbl : (string, Ochat_function.runner) Hashtbl.t option
  ; temperature : float option
  ; max_output_tokens : int option
  ; reasoning : Openai.Responses.Request.Reasoning.t option
  ; moderator : moderator option
  ; before_model_call : unit -> unit
  ; prepare_model_input :
      (history:History_entry.t list
       -> effective:Moderation.Effective_entry.t list
       -> History_entry.t list)
        option
  ; runtime_policy : Runtime_semantics.policy option
  ; on_runtime_request : Moderation.Runtime_request.t -> unit
  ; history_compaction : bool
  ; parallel_tool_calls : bool
  ; meta_refine : bool
  ; safe_point_input : Safe_point_input.t option
  ; model : Openai.Responses.Request.model
  ; prompt_cache_key : string option
  ; prompt_cache_retention : string option
  ; injected_post_stream :
      (sw:Eio.Switch.t
       -> inputs:Openai.Responses.Item.t list
       -> Openai.Responses.Response_stream.t Seq.t)
        option
  ; source : string option
  ; parent_call_id : string option
  }

let derive_datadir ~env = function
  | Some d -> d
  | None ->
    let cwd = Eio.Stdenv.cwd env in
    Io.ensure_chatmd_dir ~cwd
;;

let derive_tools_tool_tbl ~tools ~tool_tbl =
  match tools, tool_tbl with
  | Some t, Some tbl -> t, tbl
  | _ ->
    let comp_tools, tbl = Ochat_function.functions [] in
    Tool.convert_tools comp_tools, tbl
;;

let payload_of_jsonaf ~(kind : Tool_call.Kind.t) (payload : Jsonaf.t) : string =
  match kind with
  | Function -> Jsonaf.to_string payload
  | Custom ->
    (match payload with
     | `String text -> text
     | _ -> Jsonaf.to_string payload)
;;

let requests_end_session (outcome : Moderation.Outcome.t) =
  List.exists outcome.runtime_requests ~f:(function
    | Moderation.Runtime_request.End_session _ -> true
    | Request_compaction -> false
    | Request_turn -> false)
;;

let outcomes_to_list
      (outer : Moderation.Outcome.t option)
      ~(drained : Moderation.Outcome.t list)
  =
  Option.to_list outer @ drained
;;

let runtime_requests_of_outcomes (outcomes : Moderation.Outcome.t list) =
  List.concat_map outcomes ~f:(fun outcome -> outcome.runtime_requests)
;;

let report_runtime_requests
      ~(on_runtime_request : Moderation.Runtime_request.t -> unit)
      (outcomes : Moderation.Outcome.t list)
  =
  List.iter outcomes ~f:(fun outcome ->
    List.iter outcome.runtime_requests ~f:on_runtime_request)
;;

let unexpected_tool_moderation ~(source : string) (outcomes : Moderation.Outcome.t list)
  : (unit, string) result
  =
  match List.find_map outcomes ~f:(fun outcome -> outcome.tool_moderation) with
  | None -> Ok ()
  | Some action ->
    Error
      (Printf.sprintf
         "%s returned an unexpected tool moderation action: %s"
         source
         ([%sexp_of: Moderation.Tool_moderation.t] action |> Sexp.to_string_hum))
;;

let run_moderation_event
      ~(moderator : moderator option)
      ~available_tools
      ~now_ms
      ~history
      ~(event : Moderation.Event.t)
  : (Moderation.Outcome.t option, string) result
  =
  match moderator with
  | None -> Ok None
  | Some { event_handlers = Some _; _ } ->
    Error "owned moderator events require identity-bearing history"
  | Some moderator ->
    Result.map
      (Moderator_manager.handle_event
         ~skip_if_halted:true
         moderator.manager
         ~session_id:moderator.session_id
         ~now_ms
         ~history
         ~available_tools
         ~session_meta:moderator.session_meta
         ~event)
      ~f:Option.some
;;

let run_moderation_event_entries
      ~(moderator : moderator option)
      ~available_tools
      ~now_ms
      ~history
      ~(event : Moderation.Event.t)
  =
  match moderator with
  | None -> Ok None
  | Some { event_handlers = Some handlers; _ } ->
    handlers.handle ~history ~available_tools ~now_ms ~event
  | Some moderator ->
    Result.map
      (Moderator_manager.handle_event_entries
         ~skip_if_halted:true
         moderator.manager
         ~session_id:moderator.session_id
         ~now_ms
         ~history
         ~available_tools
         ~session_meta:moderator.session_meta
         ~event)
      ~f:Option.some
;;

let ensure_not_waiting_on_ui (moderator : moderator option) : (unit, string) result =
  match moderator with
  | None -> Ok ()
  | Some moderator ->
    (match pending_ui_request moderator with
     | None -> Ok ()
     | Some _ -> Error "Session is waiting for UI input.")
;;

type safe_point =
  | Turn_start_boundary
  | Post_tool_result_boundary
  | Turn_end_boundary

let string_of_safe_point = function
  | Turn_start_boundary -> "turn_start"
  | Post_tool_result_boundary -> "post_tool_result"
  | Turn_end_boundary -> "turn_end"
;;

let drain_moderator_safe_point
      ~(moderator : moderator option)
      ~available_tools
      ~now_ms
      ~history
      ~(safe_point : safe_point)
  : (Moderation.Outcome.t list, string) result
  =
  let _ = safe_point in
  match moderator with
  | None -> Ok []
  | Some { event_handlers = Some _; _ } ->
    Error "owned moderator events require identity-bearing history"
  | Some moderator ->
    Moderator_manager.drain_internal_events
      ~max_events:moderator.runtime_policy.budget.max_internal_event_drain
      moderator.manager
      ~session_id:moderator.session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta:moderator.session_meta
;;

let drain_moderator_safe_point_entries
      ~(moderator : moderator option)
      ~available_tools
      ~now_ms
      ~history
      ~(safe_point : safe_point)
  =
  let _ = safe_point in
  match moderator with
  | None -> Ok []
  | Some ({ event_handlers = Some handlers; _ } as moderator) ->
    handlers.drain
      ~history
      ~available_tools
      ~now_ms
      ~max_events:moderator.runtime_policy.budget.max_internal_event_drain
  | Some moderator ->
    Moderator_manager.drain_internal_events_entries
      ~max_events:moderator.runtime_policy.budget.max_internal_event_drain
      moderator.manager
      ~session_id:moderator.session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta:moderator.session_meta
;;

let projected_appended_item (history : Res.Item.t list)
  : (Moderation.Item.t, string) result
  =
  let _, items =
    Moderation.Projection.project_history Moderation.Projection.empty history
  in
  match List.last items with
  | Some item -> Ok item
  | None -> Error "Expected appended history item when emitting moderation event."
;;

let projected_appended_entry history =
  match List.last history with
  | None -> Error "Expected appended history entry when emitting moderation event."
  | Some entry -> Ok (Moderation.Entry_projection.project_item entry)
;;

let handle_item_appended_entries
      ~(moderator : moderator option)
      ~(on_runtime_request : Moderation.Runtime_request.t -> unit)
      ~available_tools
      ~now_ms
      ~history
  =
  let open Result.Let_syntax in
  match moderator with
  | None -> Ok ()
  | Some _ ->
    let%bind item = projected_appended_entry history in
    let%bind outer =
      run_moderation_event_entries
        ~moderator
        ~available_tools
        ~now_ms
        ~history
        ~event:(Moderation.Event.Item_appended item)
    in
    let%bind () = ensure_not_waiting_on_ui moderator in
    let outcomes = outcomes_to_list outer ~drained:[] in
    report_runtime_requests ~on_runtime_request outcomes;
    unexpected_tool_moderation
      ~source:(Moderation.Phase.to_string Moderation.Phase.Message_appended)
      outcomes
;;

let handle_item_appended
      ~(moderator : moderator option)
      ~(on_runtime_request : Moderation.Runtime_request.t -> unit)
      ~available_tools
      ~now_ms
      ~history
  =
  let open Result.Let_syntax in
  match moderator with
  | None -> Ok ()
  | Some _ ->
    let%bind item = projected_appended_item history in
    let%bind outer =
      run_moderation_event
        ~moderator
        ~available_tools
        ~now_ms
        ~history
        ~event:(Moderation.Event.Item_appended item)
    in
    let%bind () = ensure_not_waiting_on_ui moderator in
    let outcomes = outcomes_to_list outer ~drained:[] in
    report_runtime_requests ~on_runtime_request outcomes;
    unexpected_tool_moderation
      ~source:(Moderation.Phase.to_string Moderation.Phase.Message_appended)
      outcomes
;;

let runtime_requests_of_outcomes_result ~(source : string) outcomes =
  let open Result.Let_syntax in
  let%map () = unexpected_tool_moderation ~source outcomes in
  runtime_requests_of_outcomes outcomes
;;

let make_safe_point_input_item text =
  Res.Item.Input_message
    { role = Res.Input_message.Developer
    ; content = [ Res.Input_message.Text { text; _type = "input_text" } ]
    ; _type = "message"
    }
;;

let log_safe_point_input_consumed ~(safe_point : safe_point) = function
  | None -> ()
  | Some text ->
    Log.emit
      `Debug
      (Printf.sprintf
         "Consumed deferred safe-point input at %s (%d bytes)"
         (string_of_safe_point safe_point)
         (String.length text))
;;

let append_safe_point_input ~(safe_point : safe_point) ~inputs ~safe_point_input =
  match safe_point_input with
  | None -> inputs
  | Some (safe_point_input : Safe_point_input.t) ->
    let text = safe_point_input.consume_compatibility_text () in
    log_safe_point_input_consumed ~safe_point text;
    (match text with
     | None -> inputs
     | Some text when String.is_empty text -> inputs
     | Some text -> inputs @ [ make_safe_point_input_item text ])
;;

let consume_safe_point_entries ~(safe_point : safe_point) = function
  | None -> Safe_point_input.empty
  | Some (safe_point_input : Safe_point_input.t) ->
    let batch = safe_point_input.consume_entries () in
    if not (List.is_empty batch.entries)
    then
      Log.emit
        `Debug
        (Printf.sprintf
           "Consumed %d deferred canonical entries at %s"
           (List.length batch.entries)
           (string_of_safe_point safe_point));
    batch
;;

let now_ms (env : Eio_unix.Stdenv.base) : int =
  Eio.Time.now (Eio.Stdenv.clock env) *. 1000. |> Int.of_float
;;

let append_deferred_entries (c : ctx) ~(history : History_entry.t list) entries =
  let requests = ref [] in
  let on_runtime_request request =
    requests := request :: !requests;
    c.on_runtime_request request
  in
  let history =
    List.fold entries ~init:history ~f:(fun history entry ->
      let history = history @ [ entry ] in
      (match Runtime_semantics.should_end_session !requests with
       | Some _ -> ()
       | None ->
         handle_item_appended_entries
           ~moderator:c.moderator
           ~on_runtime_request
           ~available_tools:c.tools
           ~now_ms:(now_ms c.env)
           ~history
         |> Result.ok_or_failwith);
      history)
  in
  history, List.rev !requests
;;

let prepare_turn_request
      ~(moderator : moderator option)
      ~(safe_point_input : Safe_point_input.t option)
      ~available_tools
      ~now_ms
      ~history
  =
  let open Result.Let_syntax in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind outer =
    run_moderation_event
      ~moderator
      ~available_tools
      ~now_ms
      ~history
      ~event:Moderation.Event.Turn_start
  in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind drained =
    drain_moderator_safe_point
      ~moderator
      ~available_tools
      ~now_ms
      ~history
      ~safe_point:Turn_start_boundary
  in
  let outcomes = outcomes_to_list outer ~drained in
  let%bind runtime_requests =
    runtime_requests_of_outcomes_result ~source:"turn_start" outcomes
  in
  let%map inputs =
    match moderator with
    | None -> Ok history
    | Some moderator -> Moderator_manager.effective_history moderator.manager history
  in
  let inputs =
    if Option.is_some (Runtime_semantics.should_end_session runtime_requests)
    then inputs
    else append_safe_point_input ~safe_point:Turn_start_boundary ~inputs ~safe_point_input
  in
  { inputs; runtime_requests }
;;

let prepare_turn_request_entries
      ~(moderator : moderator option)
      ~(safe_point_input : Safe_point_input.t option)
      ~available_tools
      ~now_ms
      ~history
  =
  let open Result.Let_syntax in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind outer =
    run_moderation_event_entries
      ~moderator
      ~available_tools
      ~now_ms
      ~history
      ~event:Moderation.Event.Turn_start
  in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind drained =
    drain_moderator_safe_point_entries
      ~moderator
      ~available_tools
      ~now_ms
      ~history
      ~safe_point:Turn_start_boundary
  in
  let outcomes = outcomes_to_list outer ~drained in
  let%bind runtime_requests =
    runtime_requests_of_outcomes_result ~source:"turn_start" outcomes
  in
  let effective =
    match moderator with
    | None ->
      List.map history ~f:(fun entry ->
        Moderation.Effective_entry.{ entry; provenance = Canonical })
    | Some moderator -> Moderator_manager.effective_entries moderator.manager history
  in
  let inputs =
    List.map effective ~f:(fun entry ->
      History_entry.item entry.Moderation.Effective_entry.entry)
  in
  let inputs =
    if Option.is_some (Runtime_semantics.should_end_session runtime_requests)
    then inputs
    else append_safe_point_input ~safe_point:Turn_start_boundary ~inputs ~safe_point_input
  in
  Ok ({ inputs; runtime_requests }, effective)
;;

let finish_turn_entries ~(moderator : moderator option) ~available_tools ~now_ms ~history =
  let open Result.Let_syntax in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind outer =
    run_moderation_event_entries
      ~moderator
      ~event:Moderation.Event.Turn_end
      ~available_tools
      ~now_ms
      ~history
  in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind drained =
    drain_moderator_safe_point_entries
      ~moderator
      ~available_tools
      ~now_ms
      ~history
      ~safe_point:Turn_end_boundary
  in
  runtime_requests_of_outcomes_result ~source:"turn_end" (outcomes_to_list outer ~drained)
;;

let prepare_turn_inputs
      ~(moderator : moderator option)
      ?safe_point_input
      ~available_tools
      ~now_ms
      ~history
      ()
  =
  let open Result.Let_syntax in
  let%map prepared =
    prepare_turn_request ~moderator ~safe_point_input ~available_tools ~now_ms ~history
  in
  prepared.inputs
;;

let finish_turn ~(moderator : moderator option) ~available_tools ~now_ms ~history =
  let open Result.Let_syntax in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind outer =
    run_moderation_event
      ~moderator
      ~event:Moderation.Event.Turn_end
      ~available_tools
      ~now_ms
      ~history
  in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind drained =
    drain_moderator_safe_point
      ~moderator
      ~available_tools
      ~now_ms
      ~history
      ~safe_point:Turn_end_boundary
  in
  runtime_requests_of_outcomes_result ~source:"turn_end" (outcomes_to_list outer ~drained)
;;

let moderate_tool_call_with_event
      ~(moderator : moderator option)
      ~run_event
      ~(kind : Tool_call.Kind.t)
      ~(name : string)
      ~(payload : string)
      ~(call_id : string)
      ~(item_id : string option)
  : (moderated_tool_call, string) result
  =
  let open Result.Let_syntax in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let original_call_item =
    Tool_call.call_item ~kind ~name ~payload ~call_id ~id:item_id
  in
  let tool_call =
    match Moderation.Tool_call.of_response_item original_call_item with
    | None ->
      failwith "Expected tool call item when moderating a pending tool invocation."
    | Some tool_call -> tool_call
  in
  let%bind outer =
    run_event ~original_call_item ~event:(Moderation.Event.Pre_tool_call tool_call)
  in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind runtime_requests, action =
    match outer with
    | None -> Ok ([], None)
    | Some (outer : Moderation.Outcome.t) ->
      Ok (outer.runtime_requests, outer.tool_moderation)
  in
  Ok
    (match action with
     | None | Some Moderation.Tool_moderation.Approve ->
       { call_item = original_call_item
       ; kind
       ; name
       ; payload
       ; synthetic_result = None
       ; runtime_requests
       }
     | Some (Reject reason) ->
       { call_item = original_call_item
       ; kind
       ; name
       ; payload
       ; synthetic_result = Some (Output.Text reason)
       ; runtime_requests
       }
     | Some (Rewrite_args args) ->
       let payload = payload_of_jsonaf ~kind args in
       { call_item = Tool_call.call_item ~kind ~name ~payload ~call_id ~id:item_id
       ; kind
       ; name
       ; payload
       ; synthetic_result = None
       ; runtime_requests
       }
     | Some (Redirect (redirected_name, args)) ->
       let payload = payload_of_jsonaf ~kind args in
       { call_item =
           Tool_call.call_item ~kind ~name:redirected_name ~payload ~call_id ~id:item_id
       ; kind
       ; name = redirected_name
       ; payload
       ; synthetic_result = None
       ; runtime_requests
       })
;;

let moderate_tool_call ~moderator ~available_tools ~now_ms ~history =
  moderate_tool_call_with_event ~moderator ~run_event:(fun ~original_call_item ~event ->
    run_moderation_event
      ~moderator
      ~available_tools
      ~now_ms
      ~history:(history @ [ original_call_item ])
      ~event)
;;

let moderate_tool_call_entries ~moderator ~available_tools ~now_ms ~history =
  moderate_tool_call_with_event ~moderator ~run_event:(fun ~original_call_item:_ ~event ->
    run_moderation_event_entries ~moderator ~available_tools ~now_ms ~history ~event)
;;

let handle_tool_result_with_events
      ~(moderator : moderator option)
      ~run_event
      ~project_appended
      ~drain
      ~(name : string)
      ~(kind : Tool_call.Kind.t)
      ~(item : Res.Item.t)
  : (Moderation.Runtime_request.t list, string) result
  =
  let open Result.Let_syntax in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let tool_result =
    match
      Moderation.Tool_result.of_output_item
        ~name
        ~kind:
          (match kind with
           | Function -> Moderation.Tool_call.Function
           | Custom -> Moderation.Tool_call.Custom)
        item
    with
    | None -> failwith "Expected tool output item when handling a moderated tool result."
    | Some tool_result -> tool_result
  in
  let%bind outer = run_event ~event:(Moderation.Event.Post_tool_response tool_result) in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind item_appended =
    match moderator, outer with
    | None, _ -> Ok None
    | Some _, Some outer when requests_end_session outer -> Ok None
    | Some _, _ ->
      let%bind appended_item = project_appended () in
      run_event ~event:(Moderation.Event.Item_appended appended_item)
  in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind drained = drain () in
  let outcomes =
    outcomes_to_list outer ~drained:(outcomes_to_list item_appended ~drained)
  in
  runtime_requests_of_outcomes_result ~source:"post_tool_response" outcomes
;;

let handle_tool_result ~moderator ~available_tools ~now_ms ~history =
  handle_tool_result_with_events
    ~moderator
    ~run_event:(run_moderation_event ~moderator ~available_tools ~now_ms ~history)
    ~project_appended:(fun () -> projected_appended_item history)
    ~drain:(fun () ->
      drain_moderator_safe_point
        ~moderator
        ~available_tools
        ~now_ms
        ~history
        ~safe_point:Post_tool_result_boundary)
;;

let handle_tool_result_entries ~moderator ~available_tools ~now_ms ~history =
  handle_tool_result_with_events
    ~moderator
    ~run_event:(run_moderation_event_entries ~moderator ~available_tools ~now_ms ~history)
    ~project_appended:(fun () -> projected_appended_entry history)
    ~drain:(fun () ->
      drain_moderator_safe_point_entries
        ~moderator
        ~available_tools
        ~now_ms
        ~history
        ~safe_point:Post_tool_result_boundary)
;;

let log_parsing_error ~env ~datadir json exn =
  let msg =
    Printf.sprintf "Error parsing JSON from line: %s" (Core.Exn.to_string exn)
    ^ "\n"
    ^ Jsonaf.to_string json
    ^ "\n"
  in
  Io.log ~dir:datadir ~file:"raw-openai-streaming-response-json-parsing-error.txt" msg;
  Io.log
    ~dir:(Eio.Stdenv.cwd env)
    ~file:"raw-openai-streaming-response-json-parsing-error.txt"
    msg
;;

let emit_tool_output
      ~(on_fn_out : Openai.Responses.Function_call_output.t -> unit)
      ~(on_tool_out : Openai.Responses.Item.t -> unit)
      ~(kind : [ `Function | `Custom ])
      ~(call_id : string)
      ~(result : Output.t)
  : Openai.Responses.Item.t
  =
  match kind with
  | `Function ->
    let fn_out = Tool_call.function_call_output ~call_id ~output:result in
    let item = Openai.Responses.Item.Function_call_output fn_out in
    on_fn_out fn_out;
    on_tool_out item;
    item
  | `Custom ->
    let out = Tool_call.custom_tool_call_output ~call_id ~output:result in
    let item = Openai.Responses.Item.Custom_tool_call_output out in
    on_tool_out item;
    item
;;

let add_entry st entry = { st with new_entries_rev = entry :: st.new_entries_rev }
let history_with_new_entries ~hist st = List.append hist (List.rev st.new_entries_rev)

let append_history_item
      (c : ctx)
      ?commit_entry
      ?prepare_entry
      ~(moderator : moderator option)
      ~(on_runtime_request : Moderation.Runtime_request.t -> unit)
      ~available_tools
      ~now_ms
      ~(hist : History_entry.t list)
      (st : stream_state)
      (item : Res.Item.t)
  : stream_state
  =
  let id =
    History_stream_event.Registry.find_item
      c.registry
      ~scope:c.scope
      ~source:c.source
      item
    |> Option.value_or_thunk ~default:(fun () ->
      History_entry.Id_source.allocate c.id_source |> Result.ok_or_failwith)
  in
  let is_finalized =
    List.exists st.new_entries_rev ~f:(fun entry ->
      History_entry.Id.equal (History_entry.id entry) id)
  in
  if is_finalized
  then st
  else (
    let entry = History_entry.create_with_id ~id item in
    let entry = Option.value_map prepare_entry ~default:entry ~f:(fun f -> f entry) in
    (Option.value commit_entry ~default:c.on_history_item_appended) entry;
    let st = add_entry st entry in
    handle_item_appended_entries
      ~moderator
      ~on_runtime_request
      ~available_tools
      ~now_ms
      ~history:(history_with_new_entries ~hist st)
    |> Result.ok_or_failwith;
    st)
;;

let history_so_far ~history_compaction ~(hist : History_entry.t list) ~(st : stream_state)
  =
  let items_so_far = List.rev st.new_entries_rev in
  let combined = List.append hist items_so_far in
  if history_compaction
  then Compact_history.collapse_read_file_entries combined
  else combined
;;

let request_items_so_far ~history_compaction ~hist ~st =
  let entries = history_so_far ~history_compaction ~hist ~st in
  History_entry.items entries
;;

exception Openai_stream_idle_timeout of float

let openai_stream_idle_timeout () =
  let configured =
    Option.first_some
      (Sys.getenv "OCHAT_OPENAI_IDLE_TIMEOUT_SECONDS")
      (Sys.getenv "OCHAT_STREAM_TIMEOUT_SECONDS")
  in
  match configured with
  | None -> 600.
  | Some value ->
    (match Float.of_string value with
     | seconds when Float.is_finite seconds && Float.(seconds > 0.) ->
       Float.min 3600. seconds
     | _ -> 600.
     | exception _ -> 600.)
;;

let with_stream_idle_timeout ~clock ~seconds stream =
  let rec next stream () =
    let node =
      try Eio.Time.with_timeout_exn clock seconds (fun () -> stream ()) with
      | Eio.Time.Timeout -> raise (Openai_stream_idle_timeout seconds)
    in
    match node with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (event, rest) -> Seq.Cons (event, next rest)
  in
  next stream
;;

let provider_post_stream (c : ctx) ~sw ~(inputs : Openai.Responses.Item.t list) =
  Openai.Responses.post_response
    Openai.Responses.Stream
    ?max_output_tokens:c.max_output_tokens
    ?temperature:c.temperature
    ~tools:c.tools
    ~parallel_tool_calls:c.parallel_tool_calls
    ~model:c.model
    ?reasoning:c.reasoning
    ?prompt_cache_key:c.prompt_cache_key
    ?prompt_cache_retention:c.prompt_cache_retention
    ~dir:c.datadir
    ~sw
    c.env#net
    ~inputs
;;

let post_stream (c : ctx) ~sw ~inputs =
  match c.injected_post_stream with
  | Some post -> post ~sw ~inputs
  | None -> provider_post_stream c ~sw ~inputs
;;

let make_tool_promise
      ~(sw : Eio.Switch.t)
      ~(parallel : bool)
      ~(sem : Eio.Semaphore.t Lazy.t)
      f
  =
  if not parallel
  then (
    let res = f () in
    let p, r = Eio.Promise.create () in
    Eio.Promise.resolve_ok r res;
    p)
  else
    Eio.Fiber.fork_promise ~sw (fun () ->
      let s = Lazy.force sem in
      Eio.Semaphore.acquire s;
      Fun.protect ~finally:(fun () -> Eio.Semaphore.release s) f)
;;

let notify_each observers value =
  List.iter observers ~f:(fun observer ->
    try observer value with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ -> ())
;;

let report_event (c : ctx) event =
  let history_event =
    History_stream_event.observe c.registry ~scope:c.scope ~source:c.source event
  in
  Option.iter history_event ~f:c.on_history_event;
  notify_each [ c.on_event ] event;
  notify_each
    [ c.on_sourced_event ]
    { entry_id = Option.map history_event ~f:(fun event -> event.entry_id)
    ; invocation_id = c.source
    ; parent_call_id = c.parent_call_id
    ; event
    }
;;

let redacted_stream_item (c : ctx) = function
  | Res.Response_stream.Item.Function_call call ->
    Res.Response_stream.Item.Function_call
      { call with arguments = c.redact_tool_payload ~name:call.name call.arguments }
  | Custom_function call ->
    Custom_function { call with input = c.redact_tool_payload ~name:call.name call.input }
  | item -> item
;;

let pending_stream_item = function
  | Res.Response_stream.Item.Function_call call ->
    Res.Response_stream.Item.Function_call { call with arguments = "" }
  | Custom_function call -> Custom_function { call with input = "" }
  | item -> item
;;

let tool_name (st : stream_state) item_id =
  Option.map (Map.find st.func_info item_id) ~f:(fun info -> info.name)
;;

let redacted_completion (c : ctx) st item_id payload =
  Option.value_map (tool_name st item_id) ~default:"<redacted>" ~f:(fun name ->
    c.redact_tool_payload ~name payload)
;;

let redacted_event (c : ctx) st = function
  | Res.Response_stream.Output_item_added event ->
    Res.Response_stream.Output_item_added
      { event with item = pending_stream_item event.item }
  | Output_item_done event ->
    Output_item_done { event with item = redacted_stream_item c event.item }
  | Function_call_arguments_delta event ->
    Function_call_arguments_delta { event with delta = "" }
  | Function_call_arguments_done event ->
    Function_call_arguments_done
      { event with arguments = redacted_completion c st event.item_id event.arguments }
  | Custom_tool_call_input_delta event ->
    Custom_tool_call_input_delta { event with delta = "" }
  | Custom_tool_call_input_done event ->
    Custom_tool_call_input_done
      { event with input = redacted_completion c st event.item_id event.input }
  | event -> event
;;

let completed_arguments_delta = function
  | Res.Response_stream.Function_call_arguments_done event ->
    Some
      (Res.Response_stream.Function_call_arguments_delta
         { item_id = event.item_id
         ; output_index = event.output_index
         ; delta = event.arguments
         ; type_ = "response.function_call_arguments.delta"
         })
  | Custom_tool_call_input_done event ->
    Some
      (Res.Response_stream.Custom_tool_call_input_delta
         { item_id = event.item_id
         ; output_index = event.output_index
         ; delta = event.input
         ; type_ = "response.custom_tool_call_input.delta"
         })
  | _ -> None
;;

let report_redacted_event c st = function
  | Res.Response_stream.Function_call_arguments_delta _ | Custom_tool_call_input_delta _
    -> ()
  | event ->
    let event = redacted_event c st event in
    Option.iter (completed_arguments_delta event) ~f:(report_event c);
    report_event c event
;;

let make_run_fork ~turn ~(ctx : ctx) ~history_so_far ~invocation ~call_id ~arguments =
  let invocation_id = Fork.Invocation_id.create () in
  let child_allocator =
    Fork.allocator
      ~parent_namespace:(History_entry.Allocator.namespace ctx.allocator)
      invocation_id
  in
  let child_registry = History_stream_event.Registry.create ~allocator:child_allocator in
  let trace =
    Agent_trace.create
      ~emit:(Ochat_function.Invocation.emit invocation)
      ~emit_trace:(Ochat_function.Invocation.emit_trace invocation)
  in
  let child_ctx =
    { ctx with
      allocator = child_allocator
    ; id_source = History_entry.Id_source.of_allocator child_allocator
    ; registry = child_registry
    ; source = Some (Fork.Invocation_id.to_string invocation_id)
    ; parent_call_id = Some call_id
    ; moderator = None
    ; before_model_call = (fun () -> ())
    ; prepare_model_input = None
    ; runtime_policy = None
    ; safe_point_input = None
    ; on_runtime_request = (fun _ -> ())
    ; on_history_item_appended = (fun _ -> ())
    ; on_history_tool_out = (fun _ -> ())
    ; on_fn_out = (fun _ -> ())
    ; on_tool_out = (fun _ -> ())
    ; on_event =
        (fun event -> notify_each [ ctx.on_event; Agent_trace.on_event trace ] event)
    ; on_tool_execution = Some (Agent_trace.on_tool_execution trace)
    }
  in
  let res =
    turn child_ctx
    @@ Fork.history_entries
         ~allocator:child_allocator
         ~history:history_so_far
         ~arguments
         ~call_id
  in
  let txt =
    [ History_entry.item (List.last_exn res) ]
    |> List.filter_map ~f:(function
      | Res.Item.Output_message o ->
        Some (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" ")
      | _ -> None)
    |> String.concat ~sep:"\n"
  in
  Output.Text txt
;;

let add_pending
      (st : stream_state)
      ~(call_id : string)
      ~(kind : [ `Function | `Custom ])
      ~(name : string)
      promise
  =
  let pending = { seq = st.next_seq; call_id; kind; name; promise } in
  { st with
    pending_calls_rev = pending :: st.pending_calls_rev
  ; next_seq = st.next_seq + 1
  ; run_again = true
  }
;;

let dispatch_tool
      (c : ctx)
      ~hist
      ~st
      ~kind
      ~original_name
      ~original_payload
      ~name
      ~payload
      ~call_id
      ~item_id
      ~synthetic_result
      ~rejection
      ~runtime_requests
      run_native
  =
  let halted () =
    Option.value_map c.moderator ~default:false ~f:(fun moderator ->
      Moderator_manager.is_halted moderator.manager |> Result.ok_or_failwith)
  in
  let rejection, synthetic_result =
    if Option.is_none rejection && halted ()
    then Some Tool_dispatch.Session_ended, Some (Output.Text "The session has ended.")
    else rejection, synthetic_result
  in
  let authorize () =
    if Option.is_some synthetic_result
    then failwith "pre-tool moderation rejected this invocation"
    else c.authorize_tool ~kind ~name ~payload ~call_id
  in
  let routed =
    Option.bind c.dispatch_tool ~f:(fun dispatch ->
      let call_id =
        History_stream_event.Registry.find_item
          c.registry
          ~scope:c.scope
          ~source:c.source
          (Tool_call.call_item ~kind ~name ~payload ~call_id ~id:(Some item_id))
        |> Option.value_exn
      in
      let call =
        List.find_exn st.new_entries_rev ~f:(fun entry ->
          History_entry.Id.equal (History_entry.id entry) call_id)
      in
      dispatch.run
        Tool_dispatch.
          { kind
          ; original_name
          ; original_payload
          ; name
          ; payload
          ; rejection
          ; call
          ; history = history_with_new_entries ~hist st
          ; source = c.source
          ; parent_call_id = c.parent_call_id
          }
        ~authorize)
  in
  let result =
    match routed with
    | Some result -> result
    | None
      when Option.exists c.dispatch_tool ~f:(fun service ->
             Option.is_some service.prepare_call) ->
      failwith "host-prepared tool call requires its owned dispatcher"
    | None ->
      let output =
        match synthetic_result with
        | Some output -> output
        | None ->
          authorize ();
          if halted () then Output.Text "The session has ended." else run_native ()
      in
      Tool_dispatch.{ output; commit_output = None; runtime_requests = [] }
  in
  { result with runtime_requests = runtime_requests @ result.runtime_requests }
;;

let prepare_tool_call (c : ctx) ~hist ~st ~kind ~name ~payload ~call_id ~item_id =
  let reject reason message =
    ( { call_item = Tool_call.call_item ~kind ~name ~payload ~call_id ~id:(Some item_id)
      ; kind
      ; name
      ; payload
      ; synthetic_result = Some (Output.Text message)
      ; runtime_requests = []
      }
    , Some reason )
  in
  let validation =
    match c.dispatch_tool with
    | None -> Ok ()
    | Some service -> service.validate_original ~kind ~name ~payload
  in
  match validation with
  | Error _ -> reject Tool_dispatch.Invalid_input "Invalid tool arguments."
  | Ok () ->
    let result =
      try
        let moderate =
          match c.moderator with
          | Some { event_handlers = Some _; _ } ->
            moderate_tool_call_entries
              ~moderator:c.moderator
              ~available_tools:c.tools
              ~now_ms:(now_ms c.env)
              ~history:(history_with_new_entries ~hist st)
          | _ ->
            moderate_tool_call
              ~moderator:c.moderator
              ~available_tools:c.tools
              ~now_ms:(now_ms c.env)
              ~history:(History_entry.items hist)
        in
        moderate ~kind ~name ~payload ~call_id ~item_id:(Some item_id)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
        if Option.is_none c.dispatch_tool
        then raise exn
        else Error "pre-tool host failure"
    in
    (match result with
     | Error message when Option.is_none c.dispatch_tool -> failwith message
     | Error _ -> reject Tool_dispatch.Pre_tool_failed "Pre-tool moderation failed."
     | Ok moderated
       when Option.is_none moderated.synthetic_result
            && Option.is_some
                 (Runtime_semantics.should_end_session moderated.runtime_requests) ->
       let rejected, reason =
         reject Tool_dispatch.Session_ended "The session has ended."
       in
       { rejected with runtime_requests = moderated.runtime_requests }, reason
     | Ok moderated ->
       ( moderated
       , Option.map moderated.synthetic_result ~f:(fun _ -> Tool_dispatch.Pre_tool) ))
;;

let prepare_host_tool_entry
      (c : ctx)
      ~hist
      ~st
      ~original_name
      ~original_payload
      ~call_id
      ~item_id
      (prepared : (moderated_tool_call * Tool_dispatch.rejection option) ref)
      entry
  =
  let moderated, rejection = !prepared in
  match
    Option.bind c.dispatch_tool ~f:(fun dispatch -> dispatch.prepare_call), rejection
  with
  | None, _ | _, Some _ -> entry
  | Some _, None when Option.is_some moderated.synthetic_result -> entry
  | Some prepare, None ->
    let request =
      Tool_dispatch.
        { kind = moderated.kind
        ; original_name
        ; original_payload
        ; name = moderated.name
        ; payload = moderated.payload
        ; rejection = None
        ; call = entry
        ; history = history_with_new_entries ~hist st @ [ entry ]
        ; source = c.source
        ; parent_call_id = c.parent_call_id
        }
    in
    let decision =
      match request.source, request.parent_call_id with
      | Some _, _ | _, Some _ -> Error "host preparation requires its persisted owner"
      | None, None ->
        (try prepare request with
         | Eio.Cancel.Cancelled _ as exn -> raise exn
         | _ -> Error "host preparation failed")
    in
    let moderated, rejection =
      match decision with
      | Ok (None | Some Moderation.Tool_moderation.Approve) -> moderated, None
      | Ok (Some (Reject reason)) ->
        ( { moderated with synthetic_result = Some (Output.Text reason) }
        , Some Tool_dispatch.Pre_tool )
      | Error _ ->
        ( { moderated with
            synthetic_result = Some (Output.Text "Pre-tool moderation failed.")
          }
        , Some Tool_dispatch.Pre_tool_failed )
      | Ok (Some (Rewrite_args args)) ->
        { moderated with payload = payload_of_jsonaf ~kind:moderated.kind args }, None
      | Ok (Some (Redirect (name, args))) ->
        ( { moderated with name; payload = payload_of_jsonaf ~kind:moderated.kind args }
        , None )
    in
    let call_item =
      Tool_call.call_item
        ~kind:moderated.kind
        ~name:moderated.name
        ~payload:moderated.payload
        ~call_id
        ~id:(Some item_id)
    in
    prepared := { moderated with call_item }, rejection;
    let displayed =
      Tool_call.call_item
        ~kind:moderated.kind
        ~name:moderated.name
        ~payload:(c.redact_tool_payload ~name:moderated.name moderated.payload)
        ~call_id
        ~id:(Some item_id)
    in
    History_entry.create_with_id ~id:(History_entry.id entry) displayed
;;

let commit_tool_call
      (c : ctx)
      ~hist
      ~st
      ~kind
      ~original_name
      ~original_payload
      ~name
      ~payload
      ~rejection
      entry
  =
  let request =
    Tool_dispatch.
      { kind
      ; original_name
      ; original_payload
      ; name
      ; payload
      ; rejection
      ; call = entry
      ; history = history_with_new_entries ~hist st @ [ entry ]
      ; source = c.source
      ; parent_call_id = c.parent_call_id
      }
  in
  if not (Option.exists c.dispatch_tool ~f:(fun service -> service.commit_call request))
  then (
    if
      Option.exists c.dispatch_tool ~f:(fun service ->
        Option.is_some service.prepare_call)
    then failwith "host-prepared tool call requires owned invocation admission";
    c.on_history_item_appended entry)
;;

let schedule_function_done
      ~turn
      (c : ctx)
      ~(hist : History_entry.t list)
      ~(st : stream_state)
      ~(item_id : string)
      ~(arguments : string)
      ~sem
  =
  match Map.find st.func_info item_id with
  | None -> st
  | Some { kind = `Custom; _ } ->
    failwithf "Function completion conflicts with custom tool item %s" item_id ()
  | Some { name = _; call_id; kind = `Function }
    when List.exists st.pending_calls_rev ~f:(fun pending ->
           String.equal pending.call_id call_id) -> st
  | Some { name; call_id; kind = `Function } ->
    let original_name = name in
    let original_payload = arguments in
    let moderated, rejection =
      prepare_tool_call
        c
        ~hist
        ~st
        ~kind:Tool_call.Kind.Function
        ~name
        ~payload:arguments
        ~call_id
        ~item_id
    in
    let prepared = ref (moderated, rejection) in
    let history_payload = c.redact_tool_payload ~name:moderated.name moderated.payload in
    let history_item =
      Tool_call.call_item
        ~kind:moderated.kind
        ~name:moderated.name
        ~payload:history_payload
        ~call_id
        ~id:(Some item_id)
    in
    let st =
      append_history_item
        c
        ~prepare_entry:
          (prepare_host_tool_entry
             c
             ~hist
             ~st
             ~original_name
             ~original_payload
             ~call_id
             ~item_id
             prepared)
        ~commit_entry:(fun entry ->
          let moderated, rejection = !prepared in
          commit_tool_call
            c
            ~hist
            ~st
            ~kind:Tool_call.Kind.Function
            ~original_name
            ~original_payload
            ~name:moderated.name
            ~payload:moderated.payload
            ~rejection
            entry)
        ~moderator:
          (if
             Option.is_some
               (Runtime_semantics.should_end_session moderated.runtime_requests)
           then None
           else c.moderator)
        ~on_runtime_request:c.on_runtime_request
        ~available_tools:c.tools
        ~now_ms:(now_ms c.env)
        ~hist
        st
        history_item
    in
    let moderated, rejection = !prepared in
    let name = moderated.name in
    let arguments = moderated.payload in
    let hs = history_so_far ~history_compaction:c.history_compaction ~hist ~st in
    let run_tool () =
      dispatch_tool
        c
        ~hist
        ~st
        ~kind:Tool_call.Kind.Function
        ~original_name
        ~original_payload
        ~name
        ~payload:arguments
        ~call_id
        ~item_id
        ~synthetic_result:moderated.synthetic_result
        ~rejection
        ~runtime_requests:moderated.runtime_requests
        (fun () ->
           Tool_call.run_tool
             ~kind:Tool_call.Kind.Function
             ~name
             ~payload:arguments
             ~call_id
             ~tool_tbl:c.tool_tbl
             ?on_tool_execution:c.on_tool_execution
             ~on_fork:
               (Some
                  (fun ~invocation ~call_id ~arguments ->
                    make_run_fork
                      ~turn
                      ~ctx:c
                      ~history_so_far:hs
                      ~invocation
                      ~call_id
                      ~arguments))
             ())
    in
    let p = make_tool_promise ~sw:c.sw ~parallel:c.parallel_tool_calls ~sem run_tool in
    add_pending st ~call_id ~kind:`Function ~name p
;;

let schedule_custom_done
      (c : ctx)
      ~(hist : History_entry.t list)
      ~(st : stream_state)
      ~(item_id : string)
      ~(input : string)
      ~sem
  =
  match Map.find st.func_info item_id with
  | None -> st
  | Some { kind = `Function; _ } ->
    failwithf "Custom completion conflicts with function tool item %s" item_id ()
  | Some { name = _; call_id; kind = `Custom }
    when List.exists st.pending_calls_rev ~f:(fun pending ->
           String.equal pending.call_id call_id) -> st
  | Some { name; call_id; kind = `Custom } ->
    let original_name = name in
    let original_payload = input in
    let moderated, rejection =
      prepare_tool_call
        c
        ~hist
        ~st
        ~kind:Tool_call.Kind.Custom
        ~name
        ~payload:input
        ~call_id
        ~item_id
    in
    let prepared = ref (moderated, rejection) in
    let history_payload = c.redact_tool_payload ~name:moderated.name moderated.payload in
    let history_item =
      Tool_call.call_item
        ~kind:moderated.kind
        ~name:moderated.name
        ~payload:history_payload
        ~call_id
        ~id:(Some item_id)
    in
    let st =
      append_history_item
        c
        ~prepare_entry:
          (prepare_host_tool_entry
             c
             ~hist
             ~st
             ~original_name
             ~original_payload
             ~call_id
             ~item_id
             prepared)
        ~commit_entry:(fun entry ->
          let moderated, rejection = !prepared in
          commit_tool_call
            c
            ~hist
            ~st
            ~kind:Tool_call.Kind.Custom
            ~original_name
            ~original_payload
            ~name:moderated.name
            ~payload:moderated.payload
            ~rejection
            entry)
        ~moderator:
          (if
             Option.is_some
               (Runtime_semantics.should_end_session moderated.runtime_requests)
           then None
           else c.moderator)
        ~on_runtime_request:c.on_runtime_request
        ~available_tools:c.tools
        ~now_ms:(now_ms c.env)
        ~hist
        st
        history_item
    in
    let moderated, rejection = !prepared in
    let name = moderated.name in
    let input = moderated.payload in
    let run_tool () =
      dispatch_tool
        c
        ~hist
        ~st
        ~kind:Tool_call.Kind.Custom
        ~original_name
        ~original_payload
        ~name
        ~payload:input
        ~call_id
        ~item_id
        ~synthetic_result:moderated.synthetic_result
        ~rejection
        ~runtime_requests:moderated.runtime_requests
        (fun () ->
           Tool_call.run_tool
             ~kind:Tool_call.Kind.Custom
             ~name
             ~payload:input
             ~call_id
             ~tool_tbl:c.tool_tbl
             ~on_fork:None
             ?on_tool_execution:c.on_tool_execution
             ())
    in
    let p = make_tool_promise ~sw:c.sw ~parallel:c.parallel_tool_calls ~sem run_tool in
    add_pending st ~call_id ~kind:`Custom ~name p
;;

let add_tool_info st ~item_id info =
  match Map.find st.func_info item_id with
  | None -> { st with func_info = Map.set st.func_info ~key:item_id ~data:info }
  | Some existing
    when String.equal existing.name info.name
         && String.equal existing.call_id info.call_id
         && equal_driver_pending_call_kind existing.kind info.kind -> st
  | Some _ -> failwithf "Conflicting metadata for streamed tool item %s" item_id ()
;;

let record_completion st ~item_id completion =
  match Map.find st.tool_completions item_id, completion with
  | None, _ ->
    { st with
      tool_completions = Map.set st.tool_completions ~key:item_id ~data:completion
    }
  | Some (Function_done existing), Function_done completion
    when String.equal existing completion -> st
  | Some (Custom_done existing), Custom_done completion
    when String.equal existing completion -> st
  | Some _, _ -> failwithf "Conflicting completion for streamed tool item %s" item_id ()
;;

let handle_done
      (c : ctx)
      ~(hist : History_entry.t list)
      (st : stream_state)
      (item : Openai.Responses.Response_stream.Item.t)
  =
  match item with
  | Output_message om ->
    append_history_item
      c
      ~moderator:c.moderator
      ~on_runtime_request:c.on_runtime_request
      ~available_tools:c.tools
      ~now_ms:(now_ms c.env)
      ~hist
      st
      (Openai.Responses.Item.Output_message om)
  | Reasoning r ->
    append_history_item
      c
      ~moderator:c.moderator
      ~on_runtime_request:c.on_runtime_request
      ~available_tools:c.tools
      ~now_ms:(now_ms c.env)
      ~hist
      st
      (Openai.Responses.Item.Reasoning r)
  | _ -> st
;;

let fold_stream ~turn (c : ctx) ~(hist : History_entry.t list) ~sem stream =
  let st0 =
    { func_info = Map.empty (module String)
    ; tool_completions = Map.empty (module String)
    ; new_entries_rev = []
    ; pending_calls_rev = []
    ; next_seq = 0
    ; run_again = false
    }
  in
  Seq.fold_left
    (fun st ev ->
       report_redacted_event c st ev;
       match ev with
       | Openai.Responses.Response_stream.Output_item_added { item; _ } ->
         (match item with
          | Function_call fc ->
            let item_id = Option.value fc.id ~default:fc.call_id in
            let st =
              add_tool_info
                st
                ~item_id
                { name = fc.name; call_id = fc.call_id; kind = `Function }
            in
            (match Map.find st.tool_completions item_id with
             | Some (Function_done arguments) ->
               schedule_function_done ~turn c ~hist ~st ~item_id ~arguments ~sem
             | Some (Custom_done _) ->
               failwithf
                 "Function metadata conflicts with custom completion %s"
                 item_id
                 ()
             | None -> st)
          | Custom_function tc ->
            let item_id = Option.value tc.id ~default:tc.call_id in
            let st =
              add_tool_info
                st
                ~item_id
                { name = tc.name; call_id = tc.call_id; kind = `Custom }
            in
            (match Map.find st.tool_completions item_id with
             | Some (Custom_done input) ->
               schedule_custom_done c ~hist ~st ~item_id ~input ~sem
             | Some (Function_done _) ->
               failwithf
                 "Custom metadata conflicts with function completion %s"
                 item_id
                 ()
             | None -> st)
          | _ -> st)
       | Openai.Responses.Response_stream.Output_item_done { item; _ } ->
         handle_done c ~hist st item
       | Function_call_arguments_done { item_id; arguments; _ } ->
         let st = record_completion st ~item_id (Function_done arguments) in
         schedule_function_done ~turn c ~hist ~st ~item_id ~arguments ~sem
       | Custom_tool_call_input_done { item_id; input; _ } ->
         let st = record_completion st ~item_id (Custom_done input) in
         schedule_custom_done c ~hist ~st ~item_id ~input ~sem
       | Function_call_arguments_delta _
       | Custom_tool_call_input_delta _
       | Reasoning_summary_text_delta _
       | Output_text_delta _ -> st
       | _ -> st)
    st0
    stream
;;

let retry_stream_start ~sleep create_stream =
  Response_loop.For_testing.retry_request ~sleep ~f:(fun () ->
    let stream = create_stream () in
    match stream () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (event, rest) -> fun () -> Seq.Cons (event, rest))
;;

let await_calls (c : ctx) ~(hist : History_entry.t list) (st : stream_state) =
  let sorted =
    List.sort (List.rev st.pending_calls_rev) ~compare:(fun a b ->
      Int.compare a.seq b.seq)
  in
  List.foldi
    sorted
    ~init:(st.new_entries_rev, [])
    ~f:(fun _ (entries_rev, requests_rev) { seq = _; call_id; kind; name; promise } ->
      let completed = Eio.Promise.await_exn promise in
      let result = completed.Tool_dispatch.output in
      let tool_kind =
        match kind with
        | `Function -> Tool_call.Kind.Function
        | `Custom -> Tool_call.Kind.Custom
      in
      let candidate_item =
        Tool_call.output_item ~kind:tool_kind ~call_id ~output:result
      in
      let id =
        History_stream_event.Registry.tool_output
          c.registry
          ~scope:c.scope
          ~source:c.source
          ~call_id
      in
      let candidate_entry = History_entry.create_with_id ~id candidate_item in
      (* Host persistence must precede canonical publication and observation.
         An extension commit replaces the generic history append so its outcome
         receipt and output can be saved in one transaction. *)
      (match completed.commit_output with
       | Some commit -> commit candidate_entry
       | None -> c.on_history_item_appended candidate_entry);
      ignore
        (emit_tool_output
           ~on_fn_out:c.on_fn_out
           ~on_tool_out:c.on_tool_out
           ~kind
           ~call_id
           ~result
         : Res.Item.t);
      c.on_history_tool_out candidate_entry;
      let history = List.append hist (List.rev (candidate_entry :: entries_rev)) in
      let runtime_requests =
        if
          Option.is_some
            (Runtime_semantics.should_end_session
               (completed.runtime_requests @ requests_rev))
        then []
        else (
          try
            let handle_result =
              match c.moderator with
              | Some { event_handlers = Some _; _ } ->
                handle_tool_result_entries
                  ~moderator:c.moderator
                  ~available_tools:c.tools
                  ~now_ms:(now_ms c.env)
                  ~history
              | _ ->
                handle_tool_result
                  ~moderator:c.moderator
                  ~available_tools:c.tools
                  ~now_ms:(now_ms c.env)
                  ~history:(History_entry.items history)
            in
            handle_result ~name ~kind:tool_kind ~item:candidate_item
            |> Result.map_error ~f:(fun message ->
              Post_tool_moderation_failed (candidate_entry, message))
            |> function
            | Ok requests -> requests
            | Error exn -> raise exn
          with
          | Eio.Cancel.Cancelled _ as exn -> raise exn
          | Post_tool_moderation_failed _ as exn -> raise exn
          | _ ->
            raise
              (Post_tool_moderation_failed
                 (candidate_entry, "post-tool observer raised an exception")))
      in
      let runtime_requests = completed.runtime_requests @ runtime_requests in
      List.iter runtime_requests ~f:c.on_runtime_request;
      candidate_entry :: entries_rev, List.rev_append runtime_requests requests_rev)
  |> fun (entries, requests_rev) -> entries, List.rev requests_rev
;;

let log_request (c : ctx) ~(inputs : Openai.Responses.Item.t list) =
  Io.log
    ~dir:c.datadir
    ~file:"raw-openai-streaming-response-json-parsing-error.txt"
    (Sexp.to_string_hum
       [%sexp
         (("Requesting OpenAI streaming response with inputs:", inputs)
          : string * Openai.Responses.Item.t list)])
;;

let run_turn (root_ctx : ctx) ~sw ~(history : History_entry.t list) =
  let sem = lazy (Eio.Semaphore.make 8) in
  let rec turn_with_budget
            (c : ctx)
            (hist : History_entry.t list)
            ~(request_turn_budget : int)
    =
    (* fold_stream needs a (history -> history) function; forked calls should not
       consume the request_turn budget, so we reset it to 0 for those subcalls. *)
    let turn_for_fork (fork_ctx : ctx) (fork_hist : History_entry.t list)
      : History_entry.t list
      =
      turn_with_budget fork_ctx fork_hist ~request_turn_budget:0
    in
    let prepared, effective =
      prepare_turn_request_entries
        ~moderator:c.moderator
        ~safe_point_input:c.safe_point_input
        ~available_tools:c.tools
        ~now_ms:(now_ms c.env)
        ~history:hist
      |> Result.ok_or_failwith
    in
    List.iter prepared.runtime_requests ~f:c.on_runtime_request;
    if Option.is_some (Runtime_semantics.should_end_session prepared.runtime_requests)
    then hist
    else (
      let inputs = prepared.inputs in
      (match c.moderator with
       | Some { event_handlers = Some handlers; _ } ->
         handlers.before_model_call () |> Result.ok_or_failwith
       | _ -> ());
      c.before_model_call ();
      let additions =
        match c.prepare_model_input with
        | None -> []
        | Some prepare -> prepare ~history:hist ~effective
      in
      let hist = hist @ additions in
      (match additions with
       | [] -> ()
       | _ -> History_entry.Id_source.validate c.id_source hist |> Result.ok_or_failwith);
      let inputs = inputs @ History_entry.items additions in
      log_request c ~inputs;
      c.scope <- History_stream_event.Registry.create_scope c.registry;
      let events =
        retry_stream_start
          ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock c.env))
          (fun () ->
             post_stream c ~sw ~inputs
             |> with_stream_idle_timeout
                  ~clock:(Eio.Stdenv.clock c.env)
                  ~seconds:(openai_stream_idle_timeout ()))
      in
      let st = fold_stream ~turn:turn_for_fork c ~hist ~sem events in
      let new_entries_rev, tool_requests = await_calls c ~hist st in
      let hist = List.append hist (List.rev new_entries_rev) in
      if Option.is_some (Runtime_semantics.should_end_session tool_requests)
      then hist
      else (
        let deferred =
          consume_safe_point_entries ~safe_point:Turn_start_boundary c.safe_point_input
        in
        let hist, appended_requests =
          append_deferred_entries c ~history:hist deferred.entries
        in
        let finish_requests =
          match Runtime_semantics.should_end_session appended_requests with
          | Some _ -> []
          | None ->
            finish_turn_entries
              ~moderator:c.moderator
              ~available_tools:c.tools
              ~now_ms:(now_ms c.env)
              ~history:hist
            |> Result.ok_or_failwith
        in
        let finish_requests =
          match
            deferred.request_turn, Runtime_semantics.should_end_session appended_requests
          with
          | true, None -> Runtime_semantics.collapse (Request_turn :: finish_requests)
          | _ -> finish_requests
        in
        List.iter finish_requests ~f:c.on_runtime_request;
        let policy =
          Option.value_or_thunk c.runtime_policy ~default:(fun () ->
            match c.moderator with
            | None -> Runtime_semantics.default_policy
            | Some m -> m.runtime_policy)
        in
        let decision =
          Runtime_semantics.decide_after_turn_end
            ~policy
            ~tool_followup:(st.run_again || deferred.user_input)
            (tool_requests @ appended_requests @ finish_requests)
        in
        match decision.end_session_reason with
        | Some _ -> hist
        | None ->
          (match decision.continue with
           | `Stop -> hist
           | `Continue ->
             if st.run_again || deferred.user_input
             then turn_with_budget c hist ~request_turn_budget:0
             else (
               let next_budget =
                 Runtime_semantics.next_self_triggered_turn_budget
                   ~policy
                   ~request_turn_budget
                 |> Result.ok_or_failwith
               in
               turn_with_budget c hist ~request_turn_budget:next_budget))))
  in
  turn_with_budget root_ctx history ~request_turn_budget:0
;;

let setup_ctx ~(sw : Eio.Switch.t) (a : args) =
  let datadir = derive_datadir ~env:a.env a.datadir in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1_000 () in
  let tools, tool_tbl = derive_tools_tool_tbl ~tools:a.tools ~tool_tbl:a.tool_tbl in
  let c =
    { env = a.env
    ; sw
    ; datadir
    ; tools
    ; tool_tbl
    ; temperature = a.temperature
    ; max_output_tokens = a.max_output_tokens
    ; reasoning = a.reasoning
    ; moderator = a.moderator
    ; before_model_call = a.before_model_call
    ; prepare_model_input = a.prepare_model_input
    ; runtime_policy = a.runtime_policy
    ; on_runtime_request = a.on_runtime_request
    ; history_compaction = a.history_compaction
    ; parallel_tool_calls = a.parallel_tool_calls
    ; model = a.model
    ; prompt_cache_key = a.prompt_cache_key
    ; prompt_cache_retention = a.prompt_cache_retention
    ; safe_point_input = a.safe_point_input
    ; on_event = a.on_event
    ; on_sourced_event = a.on_sourced_event
    ; on_history_event = a.on_history_event
    ; on_history_item_appended = a.on_history_item_appended
    ; on_history_tool_out = a.on_history_tool_out
    ; allocator = a.allocator
    ; id_source = a.id_source
    ; registry = History_stream_event.Registry.create_with_source ~id_source:a.id_source
    ; scope = 0
    ; source = a.source
    ; parent_call_id = a.parent_call_id
    ; on_fn_out = a.on_fn_out
    ; on_tool_out = a.on_tool_out
    ; on_tool_execution = a.on_tool_execution
    ; authorize_tool = a.authorize_tool
    ; dispatch_tool = a.dispatch_tool
    ; redact_tool_payload = a.redact_tool_payload
    ; injected_post_stream = a.injected_post_stream
    }
  in
  c, cache_file, cache
;;

let run_completion_stream_in_memory_entries_impl ~sw (a : args) : History_entry.t list =
  if a.meta_refine then Caml_unix.putenv "OCHAT_META_REFINE" "1";
  let c, cache_file, cache = setup_ctx ~sw a in
  let full_history = run_turn c ~sw ~history:a.history in
  Cache.save ~file:cache_file cache;
  full_history
;;

let run_completion_stream_in_memory_entries
      ~env
      ?datadir
      ~allocator
      ?id_source
      ~(history : History_entry.t list)
      ?(on_event = fun _ -> ())
      ?(on_sourced_event = fun _ -> ())
      ?(on_history_event = fun _ -> ())
      ?(on_history_item_appended = fun _ -> ())
      ?(on_fn_out = fun _ -> ())
      ?(on_tool_out = fun _ -> ())
      ?(on_history_tool_out = fun _ -> ())
      ?on_tool_execution
      ?(authorize_tool = fun ~kind:_ ~name:_ ~payload:_ ~call_id:_ -> ())
      ?dispatch_tool
      ?(redact_tool_payload = fun ~name:_ payload -> payload)
      ~tools
      ?tool_tbl
      ?temperature
      ?max_output_tokens
      ?reasoning
      ?moderator
      ?(before_model_call = fun () -> ())
      ?prepare_model_input
      ?runtime_policy
      ?(on_runtime_request = fun _ -> ())
      ?(history_compaction = false)
      ?(parallel_tool_calls = true)
      ?(meta_refine = false)
      ?safe_point_input
      ?(model = Openai.Responses.Request.O3)
      ?prompt_cache_key
      ?prompt_cache_retention
      ?post_stream
      ?source
      ?parent_call_id
      ?sw
      ()
  =
  let id_source =
    Option.value id_source ~default:(History_entry.Id_source.of_allocator allocator)
  in
  let args =
    { env
    ; datadir
    ; history
    ; on_event
    ; on_sourced_event
    ; on_history_event
    ; on_history_item_appended
    ; on_fn_out
    ; on_tool_out
    ; on_history_tool_out
    ; allocator
    ; id_source
    ; on_tool_execution
    ; authorize_tool
    ; dispatch_tool
    ; redact_tool_payload
    ; tools
    ; tool_tbl
    ; temperature
    ; max_output_tokens
    ; reasoning
    ; moderator
    ; before_model_call
    ; prepare_model_input
    ; runtime_policy
    ; on_runtime_request
    ; history_compaction
    ; parallel_tool_calls
    ; meta_refine
    ; safe_point_input
    ; model
    ; prompt_cache_key
    ; prompt_cache_retention
    ; injected_post_stream = post_stream
    ; source
    ; parent_call_id
    }
  in
  let entries =
    match sw with
    | Some sw -> run_completion_stream_in_memory_entries_impl ~sw args
    | None ->
      Eio.Switch.run (fun sw -> run_completion_stream_in_memory_entries_impl ~sw args)
  in
  History_entry.Id_source.validate id_source entries |> Result.ok_or_failwith;
  entries
;;

module For_testing = struct
  let with_stream_idle_timeout = with_stream_idle_timeout
  let retry_stream_start = retry_stream_start
  let notify_each = notify_each
  let emit_tool_output = emit_tool_output
end
