(** [run ~kind ~call_id ~name ~payload ~runner ?on_tool_execution ()]
    executes [runner] and returns its canonical final output.

    Legacy observation emits one [Started], zero or more [Progress] or nested
    [Trace] values, and one [Finished]. Strict delivery can be interrupted by an
    observer failure or cancellation. [Returned] means only that [runner]
    returned normally; it does not imply semantic success, moderation,
    canonical output publication, history insertion, or turn completion.

    Legacy [on_tool_execution] observer exceptions are suppressed.
    [on_execution_event] is the strict typed-delivery channel: observer errors
    and cancellation propagate. Runner failures retain their original backtrace;
    a second non-cancellation terminal-observer failure is preserved by
    Exn.protect. Cancellation emits only the protected legacy Finished event;
    strict terminal delivery is skipped so the original cancellation/backtrace
    propagates. The authoritative operation terminal fences its live activity. *)
val run
  :  kind:[ `Function | `Custom ]
  -> call_id:string
  -> name:string
  -> payload:string
  -> runner:Ochat_function.runner
  -> ?inference_parent:Transcript.Scope.parent
  -> ?on_tool_execution:(Tool_execution_event.t -> unit)
  -> ?on_execution_event:(Tool_execution_event.t -> unit)
  -> unit
  -> Openai.Responses.Tool_output.Output.t
