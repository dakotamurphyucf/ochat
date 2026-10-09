(** A complete replacement of one canonical user-text occurrence. This intent
    grants no writer, runtime or archive authority. Text is bounded UTF-8; empty
    text is valid. The content revision identifies the exact saved occurrence. *)
module Mode : sig
  type t =
    | Save_only
    | Edit_and_continue
  [@@deriving equal, sexp]
end

module Unsupported_target : sig
  type t =
    | Not_plain_user_text
    | Overlay_override
    | Tool_pair_crosses_boundary
    | Initial_instruction
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t [@@deriving sexp]

val create
  :  history_id:History.Id.t
  -> expected_content_revision:History.Content_revision.t
  -> text:string
  -> mode:Mode.t
  -> (t, Error.t) result

val history_id : t -> History.Id.t
val expected_content_revision : t -> History.Content_revision.t
val text : t -> string
val mode : t -> Mode.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Edit_request : sig
  type intent = t [@@deriving sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_revision : int64
    ; edit : intent
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  (** Decode validates nonnegative generation/session revision and complete edit. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Continue_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_revision : int64
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Continuation : sig
  type unavailable =
    | Stopped
    | Runtime_unavailable
  [@@deriving equal, sexp]

  type t =
    | Not_requested
    | Started of Id.Operation.t
    | Not_started of unavailable
  [@@deriving equal, sexp]

  (** Started means a committed actual host Turn operation, never provider paid
      submission. Not_started creates no latent intent or runtime activation. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) result
end
