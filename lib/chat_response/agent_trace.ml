open Core
module Progress = Ochat_function.Progress
module Event = Tool_execution_event

type t =
  { emit : Progress.t -> unit
  ; emit_trace : Ochat_function.Trace.t -> unit
  }

let create ~emit ~emit_trace = { emit; emit_trace }
let emit_append t channel text = t.emit { Progress.channel; update = Append text }

let on_event t = function
  | Openai.Responses.Response_stream.Output_text_delta { delta; _ } ->
    emit_append t `Assistant delta
  | Reasoning_summary_text_delta { delta; _ } -> emit_append t `Reasoning delta
  | _ -> ()
;;

let on_tool_execution t = function
  | Event.Started { call_id; name; kind; payload } ->
    t.emit_trace (Ochat_function.Trace.Tool_started { call_id; name; kind; payload })
  | Progress { call_id; progress } -> t.emit_trace (Tool_progress { call_id; progress })
  | Finished { call_id; outcome; output } ->
    t.emit_trace (Tool_finished { call_id; outcome; output })
  | Trace { trace; call_id = _ } -> t.emit_trace trace
;;

let on_transcript_event t event =
  t.emit_trace (Ochat_function.Trace.Inference_live event);
  match Transcript.Stream.view event with
  | Changed { target = Content part; change = Append text } ->
    let channel =
      match part.kind with
      | Transcript.Part.Reasoning_summary | Reasoning_text -> `Reasoning
      | Text | Refusal -> `Assistant
      | Image | Unknown _ -> `Activity
    in
    emit_append t channel text
  | Source_started _ | Item_announced _ | Part_announced _
  | Changed { target = Content _; change = Replace _ }
  | Changed { target = Call_input _; _ }
  | Item_finalized _ | Source_finished _ | Unknown_event _ -> ()
;;
