(** Immutable bounded configuration for the recurrence due owner. This module
    does not select missed work, compact history, advance a cursor or dispatch.
    Execution is serial; target admission may impose stricter current ceilings. *)
type t [@@deriving equal, sexp_of]

type misfire =
  | Skip
  | Coalesce
  | Catch_up of int
[@@deriving equal, sexp_of]

(** Grace is nonnegative integer milliseconds with a checked exact int64
    nanosecond representation. Catch_up is 1..32, pending 1..32, detailed history
    1..256 and calendar steps per pass 1..4096. These are finite supported maxima,
    not permission to exceed actual host capacity. Due equality remains inclusive. *)
val create
  :  misfire:misfire
  -> grace_ms:int64
  -> max_pending:int
  -> max_history:int
  -> max_calendar_steps:int
  -> (t, Agent_protocol.Error.t) result

(** Serial/coalesce, grace 60000 ms, one pending occurrence, detailed history 64 and
    calendar 256 steps per pass. No implicit infinite queue or lateness window. *)
val default : t

val misfire : t -> misfire
val grace_ms : t -> int64
val grace_ns : t -> int64
val max_pending : t -> int
val max_history : t -> int
val max_calendar_steps : t -> int
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
