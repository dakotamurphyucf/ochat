(** [run ~kind ~call_id ~name ~payload ~runner ?on_tool_execution ()]
    executes [runner] and returns its canonical final output.

    When observed, execution emits one [Started], zero or more [Progress] or
    nested [Trace] values, and exactly one [Finished]. [Returned] means only that [runner]
    returned normally; it does not imply semantic success, moderation,
    canonical output publication, history insertion, or turn completion.

    Observer exceptions are suppressed. Runner exceptions and cancellation
    are re-raised unchanged after the terminal event is emitted. *)
val run
  :  kind:[ `Function | `Custom ]
  -> call_id:string
  -> name:string
  -> payload:string
  -> runner:Ochat_function.runner
  -> ?on_tool_execution:(Tool_execution_event.t -> unit)
  -> unit
  -> Openai.Responses.Tool_output.Output.t
