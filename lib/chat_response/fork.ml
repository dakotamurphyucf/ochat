open! Core
module Invocation_id = Fork_history.Invocation_id

let allocator = Fork_history.allocator
let history_entries = Fork_history.history_entries

type transcript_observer =
  { parent : Transcript.Scope.parent
  ; observe : Transcript.Stream.t -> unit
  }

let execute_entries
      ~ctx
      ~allocator
      ~history
      ~invocation_id:_
      ~call_id
      ~arguments
      ~tools
      ~tool_tbl
      ?on_tool_execution
      ?transcript_observer
      ~on_fn_out
      ?temperature
      ?max_output_tokens
      ?reasoning
      ()
  =
  let clone_history = history_entries ~allocator ~history ~arguments ~call_id in
  let relation =
    Option.value_map
      transcript_observer
      ~default:ctx.Ctx.inference_relation
      ~f:(fun observer -> Transcript.Scope.Nested observer.parent)
  in
  let all_entries =
    In_memory_stream.run_completion_stream_in_memory_entries
      ~env:(Ctx.env ctx)
      ~inference_context:ctx.inference_context
      ~inference_identity:ctx.inference_identity
      ~on_inference_attempt:ctx.on_inference_attempt
      ~on_inference_completion:ctx.on_inference_completion
      ~on_inference_observation:ctx.on_inference_observation
      ~inference_relation:relation
      ~datadir:(Ctx.dir ctx)
      ~allocator
      ~history:clone_history
      ~tools:(Some tools)
      ~tool_tbl
      ?on_tool_execution
      ?on_transcript_event:
        (Option.map transcript_observer ~f:(fun observer -> observer.observe))
      ?temperature
      ?max_output_tokens
      ?reasoning
      ()
  in
  let generated = List.drop all_entries (List.length clone_history) in
  let text =
    List.filter_map generated ~f:(fun entry ->
      match
        History_entry.Payload.Semantic.view
          (History_entry.Payload.semantic (History_entry.payload entry))
      with
      | Message { role = Assistant; content; _ } ->
        Some
          (List.filter_map content ~f:(function
             | History_entry.Payload.Content.Text { text; _ } -> Some text
             | Image _ | Refusal _ | Unknown _ -> None)
           |> String.concat ~sep:" ")
      | Message _ | Call _ | Result _ | Reasoning _ | Unknown _ -> None)
    |> String.concat ~sep:"\n"
  in
  on_fn_out (Tool_call.function_call_output ~call_id ~output:(Text text));
  text
;;
