(** One implementation of an actual runtime closer. Reporting and raw
    projection execute the same function; this value does not cache, retry,
    choose custody or consume diagnostic delivery. Concrete resource owners
    retain their own lifecycle and idempotence rules. *)
type t

(** Wrap an actual reported closer. Incoming immutable groups and explicit
    primary are forwarded unchanged. An unforeseen escaping exception is
    captured once at this actual invocation boundary with its raw backtrace. *)
val reported : (unit -> unit Close_observation.t) -> t

(** Adapt one legacy raw closer. Each actual invocation owns any new exception
    observation; repeated reads of the returned observation do not run it. *)
val legacy : (unit -> unit) -> t

val run_reported : t -> unit Close_observation.t

(** Raw projection of [run_reported], preserving the original exception and raw
    backtrace. This is not one-shot diagnostic presentation. *)
val run : t -> unit
