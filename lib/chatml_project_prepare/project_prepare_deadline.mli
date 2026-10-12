open Core
module Error = Project_prepare_error

(** Immutable invocation start timestamp and positive duration. No clock, callback
    or mutable failure state is retained. All consumers supply the same monotonic
    clock used at creation. *)
type t

val create : clock:_ Eio.Time.Mono.t -> wall_time:Time_ns.Span.t -> t Or_error.t

(** Remaining duration in seconds; may be nonpositive. This query does not poll
    cancellation or impose preemption. *)
val seconds_left : t -> clock:_ Eio.Time.Mono.t -> float

(** Timed_out when the remaining duration is nonpositive. Consumers separately
    poll cancellation and preserve checkpoint exceptions; invocation poisoning
    belongs to their operation owner. *)
val check : t -> clock:_ Eio.Time.Mono.t -> (unit, Error.t) Result.t
