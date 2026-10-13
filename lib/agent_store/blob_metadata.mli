(** Validated by [Blob_metadata_document] before durable admission. Raw blob
    length/digest describe original content bytes, never reserialized JSON. *)
open! Core

(** Pure comparison coordinates stored in a blob publication. Neither form
    substitutes for an issuing Store handle or grants a result read. *)
module Target : sig
  type developer =
    { host_id : Agent_protocol.Id.Server.t
    ; run_id : Agent_protocol.Id.Developer_run.t
    ; principal_id : Agent_protocol.Id.Principal.t
    }
  [@@deriving sexp]

  type t =
    | Unassigned
    | Session of Agent_protocol.Id.Session.t
    | Developer of developer
  [@@deriving sexp]

  val equal : t -> t -> bool
  val session_id : t -> Agent_protocol.Id.Session.t option
  val developer : t -> developer option
end

type t =
  { blob : Agent_protocol.Blob.Metadata.t
  ; creating_principal : Agent_protocol.Id.Principal.t
  ; target : Target.t
  ; allowed_use : string
  ; created_at : Agent_protocol.Timestamp.t
  ; expires_at : Agent_protocol.Timestamp.t option
  ; durable : bool
  }
[@@deriving sexp]

(** [Unassigned] is valid only for nondurable temporary uploads. Temporary
    conversation uploads retain their optional-session behavior; developer
    stages carry an explicit [Developer] target. No caller may infer developer
    custody from the absence of a session target. *)

val equal : t -> t -> bool
val blob_equal : Agent_protocol.Blob.Metadata.t -> Agent_protocol.Blob.Metadata.t -> bool
