(** Bounded pure queue validation before snapshot conversion or hashing. *)
val digests : Session.Snapshot.t list -> (string list, Agent_protocol.Error.t) result

(** Existing receipt envelope; these helpers do not classify origin or confer
    permission. A non-internal/missing historical snapshot returns None;
    malformed captured snapshot data rejects. *)
val encoded_event : Session.Snapshot.t -> Jsonaf.t

val receipt_snapshot
  :  Agent_protocol.Moderator_execution.t
  -> (Session.Snapshot.t option, Agent_protocol.Error.t) result
