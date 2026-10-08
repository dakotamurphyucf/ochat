(** Attachment-scoped terminal notification for a failed event subscription.
    This has no durable sequence and cannot substitute for a missing event.
    The server supplies a sanitized error; the client must obtain a fresh
    snapshot before treating the session projection as current again. *)
type t = private
  { session_id : Id.Session.t
  ; attachment_id : Id.Attachment.t
  ; error : Error.t
  }
[@@deriving sexp_of]

val create : session_id:Id.Session.t -> attachment_id:Id.Attachment.t -> Error.t -> t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val to_notification : t -> Envelope.t
