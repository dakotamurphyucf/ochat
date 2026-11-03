(** Nonnegative capacity accounting. These values describe encoded JSON growth,
    never evidence or runtime authority. All arithmetic and traversal is bounded. *)
type t

val zero : t
val create : bytes:int -> fields:int -> nodes:int -> (t, Agent_protocol.Error.t) result
val encoded_bytes : t -> int
val fields : t -> int
val nodes : t -> int
val add : t -> t -> (t, Agent_protocol.Error.t) result
val positive_difference : t -> previous:t -> t
val maximum : t -> t -> t

val measure
  :  Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> (t, Agent_protocol.Error.t) result

(** Subtract reservations from a fixed owner's actual profile, preserving depth.
    Exhaustion fails closed; no limit is increased. *)
val remaining_limits
  :  t
  -> Document_schema.Limits.t
  -> (Document_schema.Limits.t, Agent_protocol.Error.t) result
