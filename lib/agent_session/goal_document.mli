(** Pure universal-document carrier for one goal; no actor, IO, scheduler or
    inference rows. The complete document, including envelope and protected unknown
    fields, is bounded to 256 KiB, depth64. A valid Goal projection close to its
    own bound can therefore fail carrier admission. Every entry/update/adoption
    charges unknown fields and complete JSON overhead plus reserved known-owned
    byte/field/node growth for an open request. Preflight this carrier before
    staging its Run Finish; preserved unknowns cannot consume that reservation.
    Existing variant/container extension conflicts remain explicit guards. The
    actor must also preflight future-schema compatibility before effects; quota
    reservation never grants permission to discard protected future fields. *)
type t

val create : Agent_protocol.Goal.t -> (t, Document_schema.Error.t) result
val value : t -> Agent_protocol.Goal.t
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) result

(** Preserve compatible unknown fields and original presence. Closed metadata,
    identity/revision rewinds and unsafe extension ownership changes reject. *)
val update : t -> Agent_protocol.Goal.t -> (t, Document_schema.Error.t) result

val adopt : t -> incoming:t -> (t, Document_schema.Error.t) result
val shape : Document_schema.Shape.t
val limits : Document_schema.Limits.t
