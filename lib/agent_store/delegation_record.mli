(** Current delegation domain values. Construction alone does not admit a ledger
    record or confer authority; [Delegation_document] validates persistence. *)

module Key : sig
  type t =
    { parent_session_id : Agent_protocol.Id.Session.t
    ; parent_generation : int
    ; principal_id : Agent_protocol.Id.Principal.t
    ; idempotency_key : Agent_protocol.Idempotency_key.t
    }
  [@@deriving equal, sexp]
end

module Admission : sig
  type authored_tool = Captured_source_kind.Authored_tool.t =
    { name : string
    ; source_sha256 : string
    }
  [@@deriving equal, sexp]

  type lifetime =
    | Owned
    | Invocation_owned of { invocation_id : Agent_protocol.Id.Invocation.t }
    | Independent of { authorization_sha256 : string }
  [@@deriving equal, sexp]

  type t =
    { child_session_id : Agent_protocol.Id.Session.t
    ; revision_id : Agent_protocol.Id.Prompt_revision.t
    ; transaction_id : Agent_protocol.Id.Transaction.t
    ; manifest_sha256 : string
    ; parent_revision_id : Agent_protocol.Id.Prompt_revision.t
    ; parent_stop_epoch : int64 option [@sexp.option]
    ; authority_sha256 : string
    ; authored_tool : authored_tool option [@sexp.option]
    ; captured_recurrence : Delegation_captured_origin.t option [@sexp.option]
      (** Immutable lower custody, never permission. Mutually exclusive with
            [authored_tool]; the upper owner validates its actual domain types.
            Captured recurrence children may reuse the parent's exact artifact
            revision. They remain distinct sessions; the factory verifies the
            captured manifest and reciprocal origin before any creation effect. *)
    ; capability_pins : (string * string) list
    ; lifetime : lifetime
    ; created_at : Agent_protocol.Timestamp.t
    ; inference_target : (Inference.Request.Target.t[@sexp.opaque]) option [@sexp.option]
    }
  [@@deriving equal, sexp]

  (** Immutable metadata only; validates the shared selector and rejects both
      top-level source fields being present. This does not authenticate a record
      or resolve its actual live source/resources. *)
  val authored_source : t -> (authored_tool option, Store_error.t) result
end

type stage =
  | Reserved
  | Artifact_installed
  | Child_installed
  | Linked
[@@deriving equal, sexp]

module Reference : sig
  type t =
    { key : Key.t
    ; child_session_id : Agent_protocol.Id.Session.t
    ; revision_id : Agent_protocol.Id.Prompt_revision.t
    ; request_sha256 : string
    ; admission_sha256 : string
    }
  [@@deriving equal, sexp]
end

type revocation =
  | Parent_stopped
  | Parent_deleted
  | Authority_changed
  | Admission_failed
[@@deriving equal, sexp]

type artifact_collection = Prepared [@@deriving equal, sexp]

type t =
  { key : Key.t
  ; request_sha256 : string
  ; admission : Admission.t
  ; stage : stage
  ; revocation : revocation option
  ; artifact_collection : artifact_collection option [@sexp.option]
  ; preservation : (unit Document_schema.Extension_carrier.t[@sexp.opaque]) option
        [@sexp.option] [@equal.ignore]
  }
[@@deriving equal, sexp]

val validate_key : Key.t -> (unit, Store_error.t) result
val validate : t -> limits:Document_schema.Limits.t -> (unit, Store_error.t) result
