(** Private durable queue frame, captured by the host from a claimed owned timer.
    It retains immutable identity, source and subscription epoch across enqueue,
    restart and handler admission. JSON supplied by scripts is always nested in
    [Internal_event] and cannot become this top-level frame.

    Decoding is structural validation only. The actor must authorize the frame
    against its retained schedule, subscription and prior execution receipts
    before permitting the manager to project its public [Internal_event] data. *)
val capture : Agent_protocol.Schedule.t -> (Chatml.Chatml_lang.value, string) result

val decode : Chatml.Chatml_lang.value -> (Agent_protocol.Schedule.t option, string) result

(** Preserve the script-facing event contract while retaining the original frame
    in the actual queue and durable execution receipt. *)
val script_event : Chatml.Chatml_lang.value -> (Chatml.Chatml_lang.value, string) result
