open Core
module Res = Openai.Responses
module Output = Res.Tool_output.Output

type observer =
  { on_event : Res.Response_stream.t -> unit
  ; on_tool_execution : Tool_execution_event.t -> unit
  }

type post_stream =
  sw:Eio.Switch.t
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> inputs:Res.Item.t list
  -> Res.Response_stream.t Seq.t

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

let compatibility_namespace =
  let next = Atomic.make 0 in
  fun () ->
    let sequence = Atomic.fetch_and_add next 1 in
    Printf.sprintf "agent-response-loop-%d" sequence
;;

let create_entries ~allocator items =
  List.map items ~f:(History_entry.create ~allocator)
  |> Result.all
  |> Result.ok_or_failwith
;;

let notify f value =
  try f value with
  | _ -> ()
;;

let item_of_stream_item : Res.Response_stream.Item.t -> Res.Item.t = function
  | Input_message message -> Input_message message
  | Output_message message -> Output_message message
  | Function_call call -> Function_call call
  | Custom_function call -> Custom_tool_call call
  | Reasoning reasoning -> Reasoning reasoning
;;

let collect_response
      ~post_stream
      ~observer
      ~on_sourced_event
      ~source
      ~parent_call_id
      ~dir
      ~inputs
  =
  Eio.Switch.run
  @@ fun sw ->
  Seq.fold_left
    (fun items event ->
       notify observer.on_event event;
       notify
         on_sourced_event
         { Sourced_response_event.entry_id = None
         ; invocation_id = source
         ; parent_call_id
         ; event
         };
       match event with
       | Res.Response_stream.Output_item_done { item; _ } ->
         item_of_stream_item item :: items
       | _ -> items)
    []
    (post_stream ~sw ~dir ~inputs)
  |> List.rev
;;

let final_message items =
  [ List.last_exn items ]
  |> List.filter_map ~f:(function
    | Res.Item.Output_message output ->
      Some
        (List.map output.content ~f:(fun content -> content.text)
         |> String.concat ~sep:" ")
    | _ -> None)
  |> String.concat ~sep:"\n"
;;

let rec run_entries
          ~(ctx : < clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > Ctx.t)
          ~allocator
          ?temperature
          ?max_output_tokens
          ?tools
          ?reasoning
          ?(fork_depth = 0)
          ?(history_compaction = false)
          ?response_dir
          ?(on_sourced_event = fun _ -> ())
          ?source
          ?parent_call_id
          ~model
          ~tool_tbl
          ~observer
          ?post_stream
          (history : History_entry.t list)
  =
  let request_entries =
    if history_compaction
    then Compact_history.collapse_read_file_entries history
    else history
  in
  let inputs = History_entry.items request_entries in
  let response_dir = Option.value response_dir ~default:(Ctx.dir ctx) in
  let post_stream =
    Option.value post_stream ~default:(fun ~sw ~dir ~inputs ->
      Res.post_response
        Res.Stream
        ~dir
        ~model
        ~parallel_tool_calls:true
        ?temperature
        ?max_output_tokens
        ?tools
        ?reasoning
        ~sw
        (Ctx.net ctx)
        ~inputs)
  in
  let new_items =
    Response_loop.For_testing.retry_request
      ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock (Ctx.env ctx)))
      ~f:(fun () ->
        let post_stream ~sw ~dir ~inputs =
          post_stream ~sw ~dir ~inputs
          |> with_stream_idle_timeout
               ~clock:(Eio.Stdenv.clock (Ctx.env ctx))
               ~seconds:(openai_stream_idle_timeout ())
        in
        collect_response
          ~post_stream
          ~observer
          ~on_sourced_event
          ~source
          ~parent_call_id
          ~dir:response_dir
          ~inputs)
  in
  let new_entries = create_entries ~allocator new_items in
  let tool_calls =
    List.filter_map new_items ~f:(function
      | Res.Item.Function_call call -> Some (`Function call)
      | Custom_tool_call call -> Some (`Custom call)
      | _ -> None)
  in
  if List.is_empty tool_calls
  then history @ new_entries
  else (
    let history_with_response = history @ new_entries in
    let outputs =
      List.map tool_calls ~f:(fun call ->
        let kind, name, call_id, payload =
          match call with
          | `Function call ->
            Tool_call.Kind.Function, call.name, call.call_id, call.arguments
          | `Custom call -> Tool_call.Kind.Custom, call.name, call.call_id, call.input
        in
        let on_fork =
          match kind with
          | Tool_call.Kind.Custom -> None
          | Function ->
            Some
              (fun ~invocation ~call_id ~arguments ->
                match fork_depth with
                | 0 | 1 ->
                  let trace =
                    Agent_trace.create
                      ~emit:(Ochat_function.Invocation.emit invocation)
                      ~emit_trace:(Ochat_function.Invocation.emit_trace invocation)
                  in
                  let observer =
                    { on_event = Agent_trace.on_event trace
                    ; on_tool_execution = Agent_trace.on_tool_execution trace
                    }
                  in
                  let fork_entries =
                    if history_compaction
                    then Compact_history.collapse_read_file_entries history_with_response
                    else history_with_response
                  in
                  let invocation_id = Fork.Invocation_id.create () in
                  let child_allocator =
                    Fork.allocator
                      ~parent_namespace:(History_entry.Allocator.namespace allocator)
                      invocation_id
                  in
                  let fork_history =
                    Fork.history_entries
                      ~allocator:child_allocator
                      ~history:fork_entries
                      ~arguments
                      ~call_id
                  in
                  run_entries
                    ~ctx
                    ~allocator:child_allocator
                    ?temperature
                    ?max_output_tokens
                    ?tools
                    ?reasoning
                    ~fork_depth:(fork_depth + 1)
                    ~history_compaction
                    ~response_dir
                    ~model
                    ~tool_tbl
                    ~observer
                    ~on_sourced_event
                    ~source:(Fork.Invocation_id.to_string invocation_id)
                    ~parent_call_id:call_id
                    ~post_stream
                    fork_history
                  |> History_entry.items
                  |> final_message
                  |> fun text -> Output.Text text
                | _ ->
                  Output.Text
                    "Error: Called the [fork] tool in a forked process! Remember that if \
                     you are running in a forked process that you must Respond with a \
                     message in the required Format when finished with the task.")
        in
        let output =
          Tool_call.run_tool
            ~kind
            ~name
            ~payload
            ~call_id
            ~tool_tbl
            ~on_fork
            ~on_tool_execution:observer.on_tool_execution
            ()
        in
        Tool_call.output_item ~kind ~call_id ~output)
    in
    let output_entries = create_entries ~allocator outputs in
    run_entries
      ~ctx
      ~allocator
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      ~fork_depth
      ~history_compaction
      ~response_dir
      ~on_sourced_event
      ?source
      ?parent_call_id
      ~model
      ~tool_tbl
      ~observer
      ~post_stream
      (history_with_response @ output_entries))
;;

let run
      ~ctx
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      ?fork_depth
      ?history_compaction
      ?response_dir
      ?on_sourced_event
      ?source
      ?parent_call_id
      ~model
      ~tool_tbl
      ~observer
      ?post_stream
      history
  =
  let allocator =
    History_entry.Allocator.create
      ~namespace:(compatibility_namespace ())
      ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let history = create_entries ~allocator history in
  run_entries
    ~ctx
    ~allocator
    ?temperature
    ?max_output_tokens
    ?tools
    ?reasoning
    ?fork_depth
    ?history_compaction
    ?response_dir
    ?on_sourced_event
    ?source
    ?parent_call_id
    ~model
    ~tool_tbl
    ~observer
    ?post_stream
    history
  |> History_entry.items
;;
