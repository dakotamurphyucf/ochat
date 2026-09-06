(** Durable-ticket-shaped FIFO start queue with bounded key fairness. *)

type ticket =
  { session_id : Agent_protocol.Id.Session.t
  ; accepted_command_sequence : int64
  ; quota_key : Quota_key.t
  ; created_at : Agent_protocol.Timestamp.t
  }
[@@deriving sexp]

type t

val create : unit -> t
val enqueue : t -> ticket -> (unit, Agent_protocol.Error.t) result

(** [requeue] restores a previously removed retry ticket to the front of its
    quota-key queue without undoing cross-key rotation. *)
val requeue : t -> ticket -> (unit, Agent_protocol.Error.t) result

val cancel : t -> Agent_protocol.Id.Session.t -> bool

(** [heads] snapshots the oldest ticket for each quota key in fair key order
    without consuming or rotating any ticket. *)
val heads : t -> ticket list

(** [complete] removes a terminally handled ticket and rotates its quota key
    behind other keys when that key still has queued work. *)
val complete : t -> ticket -> bool

(** [take_eligible] returns the oldest eligible ticket while rotating quota
    keys to avoid starvation. *)
val take_eligible : t -> eligible:(ticket -> bool) -> ticket option

val length : t -> int
