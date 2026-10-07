open Core
open Eio.Std
module Res = Openai.Responses
module Req = Res.Request
module Config = Chat_response.Config
module Execution = Chat_response.Tool_execution_event

exception Cancelled

module Context = struct
  type t =
    { shared : App_context.Resources.t
    ; allocator : History_entry.Allocator.t
    ; cfg : Config.t
    ; tools : Req.Tool.t list
    ; tool_tbl : (string, Ochat_function.runner) Core.Hashtbl.t
    ; moderator : Chat_response.In_memory_stream.moderator option
    ; safe_point_input : Chat_response.In_memory_stream.Safe_point_input.t option
    ; parallel_tool_calls : bool
    ; history_compaction : bool
    }
end

type transport_event =
  | Transcript of Transcript.Stream.t
  | History_committed of History_entry.t
  | Tool_execution of Execution.t
  | Tool_output of History_entry.t
  | Runtime_request of Chat_response.Moderation.Runtime_request.t

type transport_command =
  | Event of transport_event
  | Flush
  | Finish of History_entry.t list * unit Eio.Promise.u

let batch_delay () =
  let milliseconds =
    match Sys.getenv "OCHAT_STREAM_BATCH_MS" with
    | Some value ->
      (try Float.min 50. (Float.max 1. (Float.of_string value)) with
       | _ -> 12.)
    | None -> 12.
  in
  milliseconds /. 1000.
;;

let run_with_terminal_event ~on_done ~on_error f =
  try on_done (f ()) with
  | ex -> on_error ex
;;

let equal_channel (left : Ochat_function.Progress.channel) right =
  match left, right with
  | `Assistant, `Assistant
  | `Reasoning, `Reasoning
  | `Stdout, `Stdout
  | `Stderr, `Stderr
  | `Activity, `Activity -> true
  | (`Assistant | `Reasoning | `Stdout | `Stderr | `Activity), _ -> false
;;

let merge_execution previous current =
  match previous, current with
  | ( Execution.Progress
        { call_id; progress = { channel; update = Ochat_function.Progress.Append left } }
    , Execution.Progress
        { call_id = other_id
        ; progress = { channel = other; update = Ochat_function.Progress.Append right }
        } )
    when String.equal call_id other_id && equal_channel channel other ->
    Some
      (Execution.Progress
         { call_id
         ; progress = { channel; update = Ochat_function.Progress.Append (left ^ right) }
         })
  | ( Execution.Progress
        { call_id; progress = { channel; update = Ochat_function.Progress.Replace _ } }
    , Execution.Progress
        { call_id = other_id
        ; progress = { channel = other; update = Ochat_function.Progress.Replace text }
        } )
    when String.equal call_id other_id && equal_channel channel other ->
    Some
      (Execution.Progress
         { call_id
         ; progress = { channel; update = Ochat_function.Progress.Replace text }
         })
  | _ -> None
;;

let coalesce_events events =
  let rec loop acc = function
    | [] -> List.rev acc
    | Tool_execution current :: rest ->
      (match acc with
       | Tool_execution previous :: acc_rest ->
         (match merge_execution previous current with
          | Some merged -> loop (Tool_execution merged :: acc_rest) rest
          | None -> loop (Tool_execution current :: acc) rest)
       | _ -> loop (Tool_execution current :: acc) rest)
    | event :: rest -> loop (event :: acc) rest
  in
  loop [] events
;;

let emit_events ~internal_stream ~op_id events =
  let flush reversed =
    match List.rev reversed with
    | [] -> ()
    | [ event ] -> Eio.Stream.add internal_stream (`Transcript (op_id, event))
    | events -> Eio.Stream.add internal_stream (`Transcript_batch (op_id, events))
  in
  let rec loop reversed = function
    | Transcript event :: rest -> loop (event :: reversed) rest
    | events ->
      flush reversed;
      (match events with
       | [] -> ()
       | History_committed entry :: rest ->
         Eio.Stream.add internal_stream (`History_committed (op_id, entry));
         loop [] rest
       | Tool_execution event :: rest ->
         Eio.Stream.add internal_stream (`Tool_execution (op_id, event));
         loop [] rest
       | Tool_output entry :: rest ->
         Eio.Stream.add internal_stream (`Tool_output (op_id, entry));
         loop [] rest
       | Runtime_request event :: rest ->
         Eio.Stream.add internal_stream (`Moderator_runtime_request (op_id, event));
         loop [] rest
       | Transcript _ :: _ -> assert false)
  in
  loop [] (coalesce_events events)
;;

let finish_transport ~internal_stream ~op_id ~events ~items =
  emit_events ~internal_stream ~op_id events;
  Eio.Stream.add internal_stream (`Streaming_done (op_id, items))
;;

let run_transport ~env ~sw ~internal_stream ~op_id stream =
  let rec loop pending window_open =
    match Eio.Stream.take stream with
    | Event event ->
      if not window_open
      then
        Fiber.fork ~sw (fun () ->
          Eio.Time.sleep (Eio.Stdenv.clock env) (batch_delay ());
          Eio.Stream.add stream Flush);
      loop (event :: pending) true
    | Flush ->
      emit_events ~internal_stream ~op_id (List.rev pending);
      loop [] false
    | Finish (items, resolver) ->
      finish_transport ~internal_stream ~op_id ~events:(List.rev pending) ~items;
      Eio.Promise.resolve resolver ()
  in
  loop [] false
;;

module For_testing = struct
  type event = transport_event =
    | Transcript of Transcript.Stream.t
    | History_committed of History_entry.t
    | Tool_execution of Execution.t
    | Tool_output of History_entry.t
    | Runtime_request of Chat_response.Moderation.Runtime_request.t

  let finish ~internal_stream ~op_id ~events ~items =
    finish_transport ~internal_stream ~op_id ~events ~items
  ;;

  let run_with_terminal_event = run_with_terminal_event
end

let run_driver (ctx : Context.t) ~history ~stream =
  Chat_response.In_memory_stream.run_completion_stream_in_memory_entries
    ~env:ctx.shared.services.env
    ~inference_context:ctx.shared.services.inference_context
    ~inference_identity:ctx.shared.services.inference_identity
    ~on_inference_attempt:ctx.shared.services.on_inference_attempt
    ~on_inference_completion:ctx.shared.services.on_inference_completion
    ~on_inference_observation:ctx.shared.services.on_inference_observation
    ~datadir:ctx.shared.services.datadir
    ~allocator:ctx.allocator
    ~history
    ~tools:(Some ctx.tools)
    ~tool_tbl:ctx.tool_tbl
    ?safe_point_input:ctx.safe_point_input
    ?moderator:ctx.moderator
    ~on_transcript_event:(fun event -> Eio.Stream.add stream (Event (Transcript event)))
    ~on_history_item_appended:(fun entry ->
      Eio.Stream.add stream (Event (History_committed entry)))
    ~on_tool_execution:(fun event -> Eio.Stream.add stream (Event (Tool_execution event)))
    ~on_history_tool_out:(fun entry -> Eio.Stream.add stream (Event (Tool_output entry)))
    ~on_runtime_request:(fun request ->
      Eio.Stream.add stream (Event (Runtime_request request)))
    ~history_compaction:ctx.history_compaction
    ~parallel_tool_calls:ctx.parallel_tool_calls
    ()
;;

let start (ctx : Context.t) ~history ~op_id =
  let env = ctx.shared.services.env in
  let internal_stream = ctx.shared.streams.internal in
  run_with_terminal_event
    ~on_done:(fun () -> ())
    ~on_error:(fun ex -> Eio.Stream.add internal_stream (`Streaming_error (op_id, ex)))
  @@ fun () ->
  Switch.run
  @@ fun streaming_sw ->
  Eio.Stream.add internal_stream (`Streaming_started (op_id, streaming_sw));
  let stream = Eio.Stream.create 256 in
  Fiber.fork ~sw:streaming_sw (fun () ->
    run_transport ~env ~sw:streaming_sw ~internal_stream ~op_id stream);
  let items = run_driver ctx ~history ~stream in
  let finished, resolve_finished = Eio.Promise.create () in
  Eio.Stream.add stream (Finish (items, resolve_finished));
  Eio.Promise.await finished
;;
