(** Validated by [Blob_metadata_document] before durable admission. Raw blob
    length/digest describe original content bytes, never reserialized JSON. *)
open! Core

type t =
  { blob : Agent_protocol.Blob.Metadata.t
  ; creating_principal : Agent_protocol.Id.Principal.t
  ; target_session : Agent_protocol.Id.Session.t option
  ; allowed_use : string
  ; created_at : Agent_protocol.Timestamp.t
  ; expires_at : Agent_protocol.Timestamp.t option
  ; durable : bool
  }
[@@deriving sexp]

val equal : t -> t -> bool
val blob_equal : Agent_protocol.Blob.Metadata.t -> Agent_protocol.Blob.Metadata.t -> bool
