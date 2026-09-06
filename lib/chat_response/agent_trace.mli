type t

(** [create ~emit ~emit_trace] creates an invocation-local nested Agent trace
    adapter. Text and reasoning deltas use [emit]; nested tool lifecycle uses
    [emit_trace]. *)
val create
  :  emit:(Ochat_function.Progress.t -> unit)
  -> emit_trace:(Ochat_function.Trace.t -> unit)
  -> t

val on_event : t -> Openai.Responses.Response_stream.t -> unit

(** These callbacks preserve nested response and tool execution events as
    transient invocation-local activity. *)
val on_tool_execution : t -> Tool_execution_event.t -> unit
