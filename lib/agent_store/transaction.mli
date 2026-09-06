(** Versioned durable session transactions stored inside journal frames. *)

type t = private
  { schema_version : int
  ; session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; transaction_sequence : int64
  ; previous_transaction_hash : string option
  ; session_revision : int64
  ; first_event_sequence : int64 option
  ; last_event_sequence : int64 option
  ; accepted_at_ns : int64
  ; command_audit : string option
  ; delta : string
  ; durable_events : string list
  }
[@@deriving sexp]

val current_schema_version : int
val validate : t -> (unit, Store_error.t) result

(** [create] validates nonnegative monotonic fields and event ranges. *)
val create
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> transaction_sequence:int64
  -> previous_transaction_hash:string option
  -> session_revision:int64
  -> first_event_sequence:int64 option
  -> last_event_sequence:int64 option
  -> accepted_at_ns:int64
  -> command_audit:string option
  -> delta:string
  -> durable_events:string list
  -> (t, Store_error.t) result

val encode : t -> string
val decode : string -> (t, Store_error.t) result

(** [hash t] is the lowercase SHA-256 digest of the canonical encoded value. *)
val hash : t -> string
