open Core
open Eio.Std
module Res = Openai.Responses
module Req = Res.Request
module Config = Chat_response.Config

exception Cancelled

module Context = struct
  type t =
    { shared : App_context.Resources.t
    ; cfg : Config.t
    ; tools : Req.Tool.t list
    ; tool_tbl : (string, string -> Res.Tool_output.Output.t) Core.Hashtbl.t
    ; parallel_tool_calls : bool
    ; history_compaction : bool
    }
end

let start (ctx : Context.t) ~history ~op_id =
  let env = ctx.shared.services.env in
  let internal_stream = ctx.shared.streams.internal in
  let system_event = ctx.shared.streams.system in
  let cfg = ctx.cfg in
  let tools = ctx.tools in
  let tool_tbl = ctx.tool_tbl in
  let datadir = ctx.shared.services.datadir in
  let parallel_tool_calls = ctx.parallel_tool_calls in
  let history_compaction = ctx.history_compaction in
  try
    Switch.run
    @@ fun streaming_sw ->
    Eio.Stream.add internal_stream (`Streaming_started (op_id, streaming_sw));
    let stream = Eio.Stream.create Int.max_value in
    let on_event ev = Eio.Stream.add internal_stream (`Stream (op_id, ev)) in
    let on_event_batch evs =
      Eio.Stream.add internal_stream (`Stream_batch (op_id, evs))
    in
    let on_tool_out item = Eio.Stream.add internal_stream (`Tool_output (op_id, item)) in
    Eio.Fiber.fork_daemon ~sw:streaming_sw (fun () ->
      let clock = Eio.Stdenv.clock env in
      let batch_ms =
        match Sys.getenv "OCHAT_STREAM_BATCH_MS" with
        | Some s ->
          (try Float.min 50. (Float.max 1. (Float.of_string s)) with
           | _ -> 12.)
        | None -> 12.
      in
      let dt = batch_ms /. 1000. in
      let rec loop s_acc t_acc window_open =
        match Eio.Stream.take stream with
        | `Stream ev ->
          let s_acc = ev :: s_acc in
          if not window_open
          then (
            Fiber.fork ~sw:streaming_sw (fun () ->
              Eio.Time.sleep clock dt;
              Eio.Stream.add stream `Flush);
            loop s_acc t_acc true)
          else loop s_acc t_acc true
        | `Tool_output item ->
          if not window_open
          then (
            Fiber.fork ~sw:streaming_sw (fun () ->
              Eio.Time.sleep clock dt;
              Eio.Stream.add stream `Flush);
            loop s_acc (item :: t_acc) true)
          else loop s_acc (item :: t_acc) true
        | `Flush ->
          (match s_acc with
           | [] -> ()
           | [ ev1 ] -> on_event ev1
           | _ -> on_event_batch (List.rev s_acc));
          List.iter (List.rev t_acc) ~f:on_tool_out;
          loop [] [] false
      in
      loop [] [] false);
    let on_tool_out item = Eio.Stream.add stream (`Tool_output item) in
    let on_event ev = Eio.Stream.add stream (`Stream ev) in
    let items =
      Chat_response.Driver.run_completion_stream_in_memory_v1
        ~env
        ~datadir
        ~history
        ~tools:(Some tools)
        ~tool_tbl
        ~system_event
        ?temperature:cfg.temperature
        ?max_output_tokens:cfg.max_tokens
        ?reasoning:
          (Option.map cfg.reasoning_effort ~f:(fun eff ->
             Req.Reasoning.
               { effort = Some (Req.Reasoning.Effort.of_str_exn eff)
               ; summary = Some Req.Reasoning.Summary.Detailed
               }))
        ~history_compaction
        ?model:(Option.map cfg.model ~f:Req.model_of_str_exn)
        ~on_event
        ~on_tool_out
        ~parallel_tool_calls
        ()
    in
    Eio.Stream.add internal_stream (`Streaming_done (op_id, items))
  with
  | ex -> Eio.Stream.add internal_stream (`Streaming_error (op_id, ex))
;;
