open Core
module CM = Prompt.Chat_markdown
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
  ; promise : Openai.Responses.Tool_output.Output.t Eio.Promise.or_exn
  }

module SM = Map.M (String)

type stream_state =
  { func_info : (string * string) SM.t
  ; new_items_rev : Openai.Responses.Item.t list
  ; pending_calls_rev : driver_pending_call list
  ; next_seq : int
  ; run_again : bool
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
  ; history_compaction : bool
  ; parallel_tool_calls : bool
  ; model : Openai.Responses.Request.model
  ; prompt_cache_key : string option
  ; prompt_cache_retention : string option
  ; system_event : string Eio.Stream.t option
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
  ; history_compaction : bool
  ; parallel_tool_calls : bool
  ; meta_refine : bool
  ; system_event : string Eio.Stream.t option
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

let read_system_event_msg ~(i : int) ~(system_event : string Eio.Stream.t option) : string
  =
  match i, system_event with
  | 0, Some stream ->
    let rec loop acc =
      match Eio.Stream.take_nonblocking stream with
      | None -> acc
      | Some m ->
        loop @@ acc ^ Printf.sprintf "\n<system-reminder>\n%s\n</system-reminder>\n" m
    in
    loop ""
  | _, None | _, Some _ -> ""
;;

let augment_result_with_system_event ~result ~(system_event_msg : string) =
  if String.is_empty system_event_msg
  then result
  else (
    match result with
    | Output.Text t ->
      Output.Text (Printf.sprintf "%s\n\n-------------\n\n%s" t system_event_msg)
    | Content c ->
      let rendered =
        String.concat
          ~sep:"\n"
          (List.map c ~f:(function
             | Input_text { text } -> text
             | Input_image { image_url; _ } ->
               Printf.sprintf "<image src=\"%s\" />" image_url))
      in
      let text = Printf.sprintf "%s\n\n-------------\n\n%s" rendered system_event_msg in
      Content (Input_text { text } :: c))
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

let post_stream (c : ctx) ~(inputs : Openai.Responses.Item.t list) =
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
      promise
  =
  let pending = { seq = st.next_seq; call_id; kind; promise } in
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
    let st =
      add_item
        st
        (Tool_call.call_item
           ~kind:Tool_call.Kind.Function
           ~name
           ~payload:arguments
           ~call_id
           ~id:(Some item_id))
    in
    let hs = history_so_far ~history_compaction:c.history_compaction ~hist ~st in
    let run_tool () =
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
    let p = make_tool_promise ~sw:c.sw ~parallel:c.parallel_tool_calls ~sem run_tool in
    add_pending st ~call_id ~kind:`Function p
;;

let schedule_custom_done
      (c : ctx)
      ~(st : stream_state)
      ~(item_id : string)
      ~(input : string)
      ~sem
  =
  match Map.find st.func_info item_id with
  | None -> st
  | Some (name, call_id) ->
    let st =
      add_item
        st
        (Tool_call.call_item
           ~kind:Tool_call.Kind.Custom
           ~name
           ~payload:input
           ~call_id
           ~id:(Some item_id))
    in
    let run_tool () =
      Tool_call.run_tool
        ~kind:Tool_call.Kind.Custom
        ~name
        ~payload:input
        ~call_id
        ~tool_tbl:c.tool_tbl
        ~on_fork:None
    in
    let p = make_tool_promise ~sw:c.sw ~parallel:c.parallel_tool_calls ~sem run_tool in
    add_pending st ~call_id ~kind:`Custom p
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

let handle_done (st : stream_state) (item : Openai.Responses.Response_stream.Item.t) =
  match item with
  | Output_message om -> add_item st (Openai.Responses.Item.Output_message om)
  | Reasoning r -> add_item st (Openai.Responses.Item.Reasoning r)
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
         handle_done st item
       | Function_call_arguments_done { item_id; arguments; _ } ->
         c.on_event ev;
         schedule_function_done ~turn c ~hist ~st ~item_id ~arguments ~sem
       | Custom_tool_call_input_done { item_id; input; _ } ->
         c.on_event ev;
         schedule_custom_done c ~st ~item_id ~input ~sem
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

let await_calls (c : ctx) (st : stream_state) =
  let sorted =
    List.sort (List.rev st.pending_calls_rev) ~compare:(fun a b ->
      Int.compare a.seq b.seq)
  in
  List.foldi
    sorted
    ~init:st.new_items_rev
    ~f:(fun i items_rev { seq = _; call_id; kind; promise } ->
      let result = Eio.Promise.await_exn promise in
      let msg = read_system_event_msg ~i ~system_event:c.system_event in
      let result = augment_result_with_system_event ~result ~system_event_msg:msg in
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

let run_turn (c : ctx) ~(history : Openai.Responses.Item.t list) =
  let sem = lazy (Eio.Semaphore.make 8) in
  let rec turn (hist : Openai.Responses.Item.t list) =
    let inputs =
      if c.history_compaction
      then Compact_history.collapse_read_file_history hist
      else hist
    in
    log_request c ~inputs;
    try
      let st = fold_stream ~turn c ~hist ~sem (post_stream c ~inputs) in
      let new_items_rev = await_calls c st in
      let hist = List.append hist (List.rev new_items_rev) in
      if st.run_again then turn hist else hist
    with
    | Openai.Responses.Response_stream_parsing_error (json, exn) ->
      log_parsing_error ~env:c.env ~datadir:c.datadir json exn;
      Eio.Time.sleep (Eio.Stdenv.clock c.env) 0.1;
      failwith (Core.Exn.to_string exn)
  in
  turn history
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
    ; history_compaction = a.history_compaction
    ; parallel_tool_calls = a.parallel_tool_calls
    ; model = a.model
    ; prompt_cache_key = a.prompt_cache_key
    ; prompt_cache_retention = a.prompt_cache_retention
    ; system_event = a.system_event
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
  let full_history = run_turn c ~history:a.history in
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
      ?(history_compaction = false)
      ?(parallel_tool_calls = true)
      ?(meta_refine = false)
      ?system_event
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
    ; history_compaction
    ; parallel_tool_calls
    ; meta_refine
    ; system_event
    ; model
    ; prompt_cache_key
    ; prompt_cache_retention
    }
;;
