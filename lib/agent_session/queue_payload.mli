(** Bounded pure queue validation before snapshot conversion or hashing. *)
val digests : Session.Snapshot.t list -> (string list, Agent_protocol.Error.t) result
