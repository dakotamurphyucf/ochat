open Core
module CM = Prompt.Chat_markdown
module Moderation = Moderation
module Moderator_manager = Moderator_manager
module Res = Openai.Responses
module Output = Res.Tool_output.Output

(* --------------------------------------------------------------------------- *)
(* Internal helper – record used for keeping track of running tool invocations *)
(* --------------------------------------------------------------------------- *)

type driver_pending_call_kind =
  [ `Function
  | `Custom
  ]

type driver_pending_call =
  { seq : int
  ; call_id : string
  ; kind : driver_pending_call_kind
  ; name : string
  ; promise : Openai.Responses.Tool_output.Output.t Eio.Promise.or_exn
  }

module SM = Map.M (String)

module Safe_point_input = struct
  type t = { consume : unit -> string option }
end

type stream_state =
  { func_info : (string * string) SM.t
  ; new_items_rev : Openai.Responses.Item.t list
  ; pending_calls_rev : driver_pending_call list
  ; next_seq : int
  ; run_again : bool
  }

type moderator =
  { manager : Moderator_manager.t
  ; session_id : string
  ; session_meta : Jsonaf.t
  ; runtime_policy : Runtime_semantics.policy
  }

type pending_ui_request = Moderator_manager.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of { prompt : string; choices : string array }

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
  ; tool_tbl : (string, string -> Openai.Responses.Tool_output.Output.t) Hashtbl.t
  ; temperature : float option
  ; max_output_tokens : int option
  ; reasoning : Openai.Responses.Request.Reasoning.t option
  ; moderator : moderator option
  ; on_runtime_request : Moderation.Runtime_request.t -> unit
  ; history_compaction : bool
  ; parallel_tool_calls : bool
  ; model : Openai.Responses.Request.model
  ; prompt_cache_key : string option
  ; prompt_cache_retention : string option
  ; safe_point_input : Safe_point_input.t option
  ; on_event : Openai.Responses.Response_stream.t -> unit
  ; on_fn_out : Openai.Responses.Function_call_output.t -> unit
  ; on_tool_out : Openai.Responses.Item.t -> unit
  }

type args =
  { env : Eio_unix.Stdenv.base
  ; datadir : Eio.Fs.dir_ty Eio.Path.t option
  ; history : Openai.Responses.Item.t list
  ; on_event : Openai.Responses.Response_stream.t -> unit
  ; on_fn_out : Openai.Responses.Function_call_output.t -> unit
  ; on_tool_out : Openai.Responses.Item.t -> unit
  ; tools : Openai.Responses.Request.Tool.t list option
  ; tool_tbl : (string, string -> Openai.Responses.Tool_output.Output.t) Hashtbl.t option
  ; temperature : float option
  ; max_output_tokens : int option
  ; reasoning : Openai.Responses.Request.Reasoning.t option
  ; moderator : moderator option
  ; on_runtime_request : Moderation.Runtime_request.t -> unit
  ; history_compaction : bool
  ; parallel_tool_calls : bool
  ; meta_refine : bool
  ; safe_point_input : Safe_point_input.t option
  ; model : Openai.Responses.Request.model
  ; prompt_cache_key : string option
  ; prompt_cache_retention : string option
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
  | Some moderator ->
    Result.map
      (Moderator_manager.handle_event
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
    { role = Res.Input_message.System
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
    let text = safe_point_input.consume () in
    log_safe_point_input_consumed ~safe_point text;
    (match text with
     | None -> inputs
     | Some text when String.is_empty text -> inputs
     | Some text -> inputs @ [ make_safe_point_input_item text ])
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
    else
      append_safe_point_input
        ~safe_point:Turn_start_boundary
        ~inputs
        ~safe_point_input
  in
  { inputs; runtime_requests }
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
    prepare_turn_request
      ~moderator
      ~safe_point_input
      ~available_tools
      ~now_ms
      ~history
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

let moderate_tool_call
      ~(moderator : moderator option)
      ~available_tools
      ~now_ms
      ~history
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
  let history_with_call = history @ [ original_call_item ] in
  let tool_call =
    match Moderation.Tool_call.of_response_item original_call_item with
    | None ->
      failwith "Expected tool call item when moderating a pending tool invocation."
    | Some tool_call -> tool_call
  in
  let%bind outer =
    run_moderation_event
      ~moderator
      ~available_tools
      ~now_ms
      ~history:history_with_call
      ~event:(Moderation.Event.Pre_tool_call tool_call)
  in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind runtime_requests, action =
    match outer with
    | None -> Ok ([], None)
    | Some outer ->
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

let handle_tool_result
      ~(moderator : moderator option)
      ~available_tools
      ~now_ms
      ~history
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
  let%bind outer =
    run_moderation_event
      ~moderator
      ~available_tools
      ~now_ms
      ~history
      ~event:(Moderation.Event.Post_tool_response tool_result)
  in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind item_appended =
    match moderator, outer with
    | None, _ -> Ok None
    | Some _, Some outer when requests_end_session outer -> Ok None
    | Some _, _ ->
      let%bind appended_item = projected_appended_item history in
      run_moderation_event
        ~moderator
        ~available_tools
        ~now_ms
        ~history
        ~event:(Moderation.Event.Item_appended appended_item)
  in
  let%bind () = ensure_not_waiting_on_ui moderator in
  let%bind drained =
    drain_moderator_safe_point
      ~moderator
      ~available_tools
      ~now_ms
      ~history
      ~safe_point:Post_tool_result_boundary
  in
  let outcomes =
    outcomes_to_list outer ~drained:(outcomes_to_list item_appended ~drained) in
  runtime_requests_of_outcomes_result ~source:"post_tool_response" outcomes
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

let add_item (st : stream_state) (it : Openai.Responses.Item.t) =
  { st with new_items_rev = it :: st.new_items_rev }
;;

let history_with_new_items ~(hist : Res.Item.t list) (st : stream_state) : Res.Item.t list
  =
  List.append hist (List.rev st.new_items_rev)
;;

let append_history_item
      ~(moderator : moderator option)
      ~(on_runtime_request : Moderation.Runtime_request.t -> unit)
      ~available_tools
      ~now_ms
      ~(hist : Res.Item.t list)
      (st : stream_state)
      (item : Res.Item.t)
  : stream_state
  =
  let st = add_item st item in
  handle_item_appended
    ~moderator
    ~on_runtime_request
    ~available_tools
    ~now_ms
    ~history:(history_with_new_items ~hist st)
  |> Result.ok_or_failwith;
  st
;;

let history_so_far
      ~history_compaction
      ~(hist : Openai.Responses.Item.t list)
      ~(st : stream_state)
  =
  let items_so_far = List.rev st.new_items_rev in
  let combined = List.append hist items_so_far in
  if history_compaction
  then Compact_history.collapse_read_file_history combined
  else combined
;;

let now_ms (env : Eio_unix.Stdenv.base) : int =
  Eio.Time.now (Eio.Stdenv.clock env) *. 1000. |> Int.of_float
;;

let post_stream (c : ctx) ~sw ~(inputs : Openai.Responses.Item.t list) =
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

let make_run_fork ~turn ~history_so_far ~call_id ~arguments =
  let res =
    turn @@ Fork.history ~history:(List.append history_so_far []) ~arguments call_id
  in
  let txt =
    [ List.last_exn res ]
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

let schedule_function_done
      ~turn
      (c : ctx)
      ~(hist : Openai.Responses.Item.t list)
      ~(st : stream_state)
      ~(item_id : string)
      ~(arguments : string)
      ~sem
  =
  match Map.find st.func_info item_id with
  | None -> st
  | Some (name, call_id) ->
    let moderated =
      moderate_tool_call
        ~moderator:c.moderator
        ~available_tools:c.tools
        ~now_ms:(now_ms c.env)
        ~history:hist
        ~kind:Tool_call.Kind.Function
        ~name
        ~payload:arguments
        ~call_id
        ~item_id:(Some item_id)
      |> Result.ok_or_failwith
    in
    List.iter moderated.runtime_requests ~f:c.on_runtime_request;
    let st =
      append_history_item
        ~moderator:c.moderator
        ~on_runtime_request:c.on_runtime_request
        ~available_tools:c.tools
        ~now_ms:(now_ms c.env)
        ~hist
        st
        moderated.call_item
    in
    let name = moderated.name in
    let arguments = moderated.payload in
    let hs = history_so_far ~history_compaction:c.history_compaction ~hist ~st in
    let run_tool () =
      match moderated.synthetic_result with
      | Some result -> result
      | None ->
        Tool_call.run_tool
          ~kind:Tool_call.Kind.Function
          ~name
          ~payload:arguments
          ~call_id
          ~tool_tbl:c.tool_tbl
          ~on_fork:
            (Some
               (fun ~call_id ~arguments ->
                 make_run_fork ~turn ~history_so_far:hs ~call_id ~arguments))
    in
    let p =
      match moderated.synthetic_result with
      | Some result ->
        let promise, resolver = Eio.Promise.create () in
        Eio.Promise.resolve_ok resolver result;
        promise
      | None -> make_tool_promise ~sw:c.sw ~parallel:c.parallel_tool_calls ~sem run_tool
    in
    add_pending st ~call_id ~kind:`Function ~name p
;;

let schedule_custom_done
      (c : ctx)
      ~(hist : Openai.Responses.Item.t list)
      ~(st : stream_state)
      ~(item_id : string)
      ~(input : string)
      ~sem
  =
  match Map.find st.func_info item_id with
  | None -> st
  | Some (name, call_id) ->
    let moderated =
      moderate_tool_call
        ~moderator:c.moderator
        ~available_tools:c.tools
        ~now_ms:(now_ms c.env)
        ~history:hist
        ~kind:Tool_call.Kind.Custom
        ~name
        ~payload:input
        ~call_id
        ~item_id:(Some item_id)
      |> Result.ok_or_failwith
    in
    List.iter moderated.runtime_requests ~f:c.on_runtime_request;
    let st =
      append_history_item
        ~moderator:c.moderator
        ~on_runtime_request:c.on_runtime_request
        ~available_tools:c.tools
        ~now_ms:(now_ms c.env)
        ~hist
        st
        moderated.call_item
    in
    let name = moderated.name in
    let input = moderated.payload in
    let run_tool () =
      match moderated.synthetic_result with
      | Some result -> result
      | None ->
        Tool_call.run_tool
          ~kind:Tool_call.Kind.Custom
          ~name
          ~payload:input
          ~call_id
          ~tool_tbl:c.tool_tbl
          ~on_fork:None
    in
    let p =
      match moderated.synthetic_result with
      | Some result ->
        let promise, resolver = Eio.Promise.create () in
        Eio.Promise.resolve_ok resolver result;
        promise
      | None -> make_tool_promise ~sw:c.sw ~parallel:c.parallel_tool_calls ~sem run_tool
    in
    add_pending st ~call_id ~kind:`Custom ~name p
;;

let handle_added (st : stream_state) (item : Openai.Responses.Response_stream.Item.t) =
  match item with
  | Function_call fc ->
    let idx = Option.value fc.id ~default:fc.call_id in
    { st with func_info = Map.set st.func_info ~key:idx ~data:(fc.name, fc.call_id) }
  | Custom_function tc ->
    let idx = Option.value tc.id ~default:tc.call_id in
    { st with func_info = Map.set st.func_info ~key:idx ~data:(tc.name, tc.call_id) }
  | _ -> st
;;

let handle_done
      (c : ctx)
      ~(hist : Res.Item.t list)
      (st : stream_state)
      (item : Openai.Responses.Response_stream.Item.t)
  =
  match item with
  | Output_message om ->
    append_history_item
      ~moderator:c.moderator
      ~on_runtime_request:c.on_runtime_request
      ~available_tools:c.tools
      ~now_ms:(now_ms c.env)
      ~hist
      st
      (Openai.Responses.Item.Output_message om)
  | Reasoning r ->
    append_history_item
      ~moderator:c.moderator
      ~on_runtime_request:c.on_runtime_request
      ~available_tools:c.tools
      ~now_ms:(now_ms c.env)
      ~hist
      st
      (Openai.Responses.Item.Reasoning r)
  | _ -> st
;;

let fold_stream ~turn (c : ctx) ~(hist : Openai.Responses.Item.t list) ~sem stream =
  let st0 =
    { func_info = Map.empty (module String)
    ; new_items_rev = []
    ; pending_calls_rev = []
    ; next_seq = 0
    ; run_again = false
    }
  in
  Seq.fold_left
    (fun st ev ->
       match ev with
       | Openai.Responses.Response_stream.Output_item_added { item; _ } ->
         c.on_event ev;
         handle_added st item
       | Openai.Responses.Response_stream.Output_item_done { item; _ } ->
         c.on_event ev;
         handle_done c ~hist st item
       | Function_call_arguments_done { item_id; arguments; _ } ->
         c.on_event ev;
         schedule_function_done ~turn c ~hist ~st ~item_id ~arguments ~sem
       | Custom_tool_call_input_done { item_id; input; _ } ->
         c.on_event ev;
         schedule_custom_done c ~hist ~st ~item_id ~input ~sem
       | Function_call_arguments_delta _
       | Custom_tool_call_input_delta _
       | Reasoning_summary_text_delta _
       | Output_text_delta _ ->
         c.on_event ev;
         st
       | _ -> st)
    st0
    stream
;;

let await_calls (c : ctx) ~(hist : Res.Item.t list) (st : stream_state) =
  let sorted =
    List.sort (List.rev st.pending_calls_rev) ~compare:(fun a b ->
      Int.compare a.seq b.seq)
  in
  List.foldi
    sorted
    ~init:st.new_items_rev
    ~f:(fun _ items_rev { seq = _; call_id; kind; name; promise } ->
      let result = Eio.Promise.await_exn promise in
      let tool_kind =
        match kind with
        | `Function -> Tool_call.Kind.Function
        | `Custom -> Tool_call.Kind.Custom
      in
      let candidate_item =
        Tool_call.output_item ~kind:tool_kind ~call_id ~output:result
      in
      let history = List.append hist (List.rev (candidate_item :: items_rev)) in
      let runtime_requests =
        handle_tool_result
          ~moderator:c.moderator
          ~available_tools:c.tools
          ~now_ms:(now_ms c.env)
          ~history
          ~name
          ~kind:tool_kind
          ~item:candidate_item
        |> Result.ok_or_failwith
      in
      List.iter runtime_requests ~f:c.on_runtime_request;
      let item =
        emit_tool_output
          ~on_fn_out:c.on_fn_out
          ~on_tool_out:c.on_tool_out
          ~kind
          ~call_id
          ~result
      in
      item :: items_rev)
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

let run_turn (c : ctx) ~sw ~(history : Openai.Responses.Item.t list) =
  let sem = lazy (Eio.Semaphore.make 8) in
  let rec turn_with_budget
            (hist : Openai.Responses.Item.t list)
            ~(request_turn_budget : int)
    =
    (* fold_stream needs a (history -> history) function; forked calls should not
       consume the request_turn budget, so we reset it to 0 for those subcalls. *)
    let turn_for_fork (fork_hist : Openai.Responses.Item.t list)
      : Openai.Responses.Item.t list
      =
      turn_with_budget fork_hist ~request_turn_budget:0
    in
    let prepared =
      prepare_turn_request
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
      let inputs =
        if c.history_compaction
        then Compact_history.collapse_read_file_history inputs
        else inputs
      in
      log_request c ~inputs;
      try
        let st = fold_stream ~turn:turn_for_fork c ~hist ~sem (post_stream c ~sw ~inputs) in
        let new_items_rev = await_calls c ~hist st in
        let hist = List.append hist (List.rev new_items_rev) in
        let finish_requests =
          finish_turn
            ~moderator:c.moderator
            ~available_tools:c.tools
            ~now_ms:(now_ms c.env)
            ~history:hist
          |> Result.ok_or_failwith
        in
        List.iter finish_requests ~f:c.on_runtime_request;
        let policy =
          match c.moderator with
          | None -> Runtime_semantics.default_policy
          | Some m -> m.runtime_policy
        in
        let decision =
          Runtime_semantics.decide_after_turn_end
            ~policy
            ~tool_followup:st.run_again
            finish_requests
        in
        match decision.end_session_reason with
        | Some _ -> hist
        | None ->
          (match decision.continue with
           | `Stop -> hist
           | `Continue ->
             if st.run_again
             then turn_with_budget hist ~request_turn_budget:0
             else (
               let next_budget =
                 Runtime_semantics.next_self_triggered_turn_budget
                   ~policy
                   ~request_turn_budget
                 |> Result.ok_or_failwith
               in
               turn_with_budget hist ~request_turn_budget:next_budget))
    with
    | Openai.Responses.Response_stream_parsing_error (json, exn) ->
      log_parsing_error ~env:c.env ~datadir:c.datadir json exn;
      Eio.Time.sleep (Eio.Stdenv.clock c.env) 0.1;
      failwith (Core.Exn.to_string exn))
  in
  turn_with_budget history ~request_turn_budget:0
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
    ; on_runtime_request = a.on_runtime_request
    ; history_compaction = a.history_compaction
    ; parallel_tool_calls = a.parallel_tool_calls
    ; model = a.model
    ; prompt_cache_key = a.prompt_cache_key
    ; prompt_cache_retention = a.prompt_cache_retention
    ; safe_point_input = a.safe_point_input
    ; on_event = a.on_event
    ; on_fn_out = a.on_fn_out
    ; on_tool_out = a.on_tool_out
    }
  in
  c, cache_file, cache
;;

let run_completion_stream_in_memory_v1_impl (a : args) : Openai.Responses.Item.t list =
  if a.meta_refine then Caml_unix.putenv "OCHAT_META_REFINE" "1";
  Eio.Switch.run
  @@ fun sw ->
  let c, cache_file, cache = setup_ctx ~sw a in
  let full_history = run_turn c ~sw ~history:a.history in
  Cache.save ~file:cache_file cache;
  full_history
;;

(* MAIN definition <= 25 lines (signature is long but body is tiny) *)
let run_completion_stream_in_memory_v1
      ~env
      ?datadir
      ~(history : Openai.Responses.Item.t list)
      ?(on_event = fun _ -> ())
      ?(on_fn_out = fun _ -> ())
      ?(on_tool_out = fun _ -> ())
      ~tools
      ?tool_tbl
      ?temperature
      ?max_output_tokens
      ?reasoning
      ?moderator
      ?(on_runtime_request = fun _ -> ())
      ?(history_compaction = false)
      ?(parallel_tool_calls = true)
      ?(meta_refine = false)
      ?safe_point_input
      ?(model = Openai.Responses.Request.O3)
      ?prompt_cache_key
      ?prompt_cache_retention
      ()
  =
  run_completion_stream_in_memory_v1_impl
    { env
    ; datadir
    ; history
    ; on_event
    ; on_fn_out
    ; on_tool_out
    ; tools
    ; tool_tbl
    ; temperature
    ; max_output_tokens
    ; reasoning
    ; moderator
    ; on_runtime_request
    ; history_compaction
    ; parallel_tool_calls
    ; meta_refine
    ; safe_point_input
    ; model
    ; prompt_cache_key
    ; prompt_cache_retention
    }
;;
