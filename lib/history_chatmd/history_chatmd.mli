(** Pure neutral ChatMD presentation; no provider decoder, protocol or I/O.
    Opaque payloads are inspectable. Rendered markup is not replay authority. *)
val render : History_entry.t list -> string

val render_payload : History_entry.Id.t -> History_entry.Payload.t -> string
val history_id : History_entry.Id.t -> string
val role : History_entry.Payload.Role.t -> string
val raw : string -> string

(** Legacy moderator companion content without an invented host occurrence. *)
val render_anonymous : History_entry.Payload.t -> string
