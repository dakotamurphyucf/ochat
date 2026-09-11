(** Private, durable child-creation ledger. Records are storage evidence, not
    bearer capabilities: callers must authenticate the parent and recheck current
    policy, generations and revocation before every effect or disclosure.

    Reserve before installing an artifact or child. A retry with the same scoped
    key and request digest returns the original identities. Records have no TTL
    and are not pruned by ordinary command-receipt retention. *)

module Key : sig
  type t =
    { parent_session_id : Agent_protocol.Id.Session.t
    ; parent_generation : int
    ; principal_id : Agent_protocol.Id.Principal.t
    ; idempotency_key : Agent_protocol.Idempotency_key.t
    }
  [@@deriving equal, sexp_of]
end

module Admission : sig
  type lifetime =
    | Owned
    | Independent of { authorization_sha256 : string }
  [@@deriving equal, sexp_of]

  type t =
    { child_session_id : Agent_protocol.Id.Session.t
    ; revision_id : Agent_protocol.Id.Prompt_revision.t
    ; transaction_id : Agent_protocol.Id.Transaction.t
    ; manifest_sha256 : string
    ; parent_revision_id : Agent_protocol.Id.Prompt_revision.t
    ; parent_stop_epoch : int64 option
      (** Stop counter observed at creation admission; legacy absence means zero
          and retains the original admission hash. New frames use ledger v2. *)
    ; authority_sha256 : string
    ; capability_pins : (string * string) list
    ; lifetime : lifetime
    ; created_at : Agent_protocol.Timestamp.t
    }
  [@@deriving equal, sexp_of]
end

type stage =
  | Reserved
  | Artifact_installed
  | Child_installed
  | Linked
[@@deriving equal, sexp_of]

type revocation =
  | Parent_stopped
  | Parent_deleted
  | Authority_changed
  | Admission_failed
[@@deriving equal, sexp_of]

type artifact_collection = Prepared [@@deriving equal, sexp_of]

type record = private
  { key : Key.t
  ; request_sha256 : string
  ; admission : Admission.t
  ; stage : stage
  ; revocation : revocation option
  ; artifact_collection : artifact_collection option
  }
[@@deriving equal, sexp_of]

(** Immutable locator embedded in a child's checkpoint. Binds the scoped request
    and full admitted configuration, independently of later stage/revocation.
    Decoding alone does not validate or grant authority. *)
module Reference : sig
  type t = private
    { key : Key.t
    ; child_session_id : Agent_protocol.Id.Session.t
    ; revision_id : Agent_protocol.Id.Prompt_revision.t
    ; request_sha256 : string
    ; admission_sha256 : string
    }
  [@@deriving equal, sexp]
end

val reference : record -> Reference.t
val validate_reference : Reference.t -> (unit, Store_error.t) result

type reservation =
  | New of record
  | Replay of record
  | Conflict of record

type t

(** One shared instance per exclusively owned data root. Session_store owns that
    instance. Construction performs no IO. Do not use after releasing root ownership. *)
val create : env:Eio_unix.Stdenv.base -> data_root:Data_root.t -> t

val find : t -> Key.t -> (record option, Store_error.t) result

(** Resolve all pinned identities against the current durable record. Returns
    revoked records too, for recovery/inspection; callers must check disposition,
    current host authority and session ownership before effects or disclosure. *)
val resolve : t -> Reference.t -> (record, Store_error.t) result

(** The request digest must cover all user-selected creation inputs. Admission
    comes from trusted validation and may contain fresh candidate IDs on retry;
    only a New result accepts those candidates. Replay reestablishes durability
    after a potentially ambiguous prior write acknowledgement.
    Scan budgets include every existing intent and atomic temporary entry. *)
val reserve
  :  t
  -> key:Key.t
  -> request_sha256:string
  -> admission:Admission.t
  -> max_records:int
  -> max_bytes:int
  -> (reservation, Store_error.t) result

(** Advance one durable stage, after verifying its actual store side effect.
    Repeating an already attained stage succeeds; skipping stages, substituting
    an admission, or advancing a revoked record fails. This does not execute or
    authorize the side effect. The coordinator must serialize parent stop/start
    with creation and reconcile any ambiguous child install before activation. *)
val advance : t -> record -> stage -> (record, Store_error.t) result

(** Revocation is terminal and first-writer-wins. It preserves every identity and
    attained stage, including records whose child installation is ambiguous. *)
val revoke : t -> record -> revocation -> (record, Store_error.t) result

(** Startup only, with exclusive root ownership and no active creation calls.
    Revalidate the private reservation and remove only its exact transaction's
    unpublished artifact/session staging directory when the corresponding final
    destination is absent and its stage has not been committed. Never remove a
    final artifact/session or staging beside an installed destination. Reject
    non-directory roots, do not follow links during recursive deletion, and sync
    the parent directory. The retained intent still owns the same retry IDs. *)
val discard_uninstalled_staging : t -> record -> (unit, Store_error.t) result

(** Fully validate the ledger and hold its mutex through f. This general reader
    does not authorize discarding any record's artifact; the startup collection
    policy is supplied separately by [with_artifact_retention]. Corruption, links
    and budget exhaustion prevent f entirely.
    f must not reenter the ledger or wait on an actor; acquire parent coordination
    before this lock. Exceptions propagate outside the mutex without poisoning it. *)
val with_records
  :  t
  -> max_records:int
  -> max_bytes:int
  -> f:(record list -> ('a, Store_error.t) result)
  -> ('a, Store_error.t) result

(** Startup-only retention decision, under exclusive root ownership and before
    accepting creation/management calls. Holds the ledger lock through [f]. A
    permanently revoked Reserved/Artifact_installed attempt can release its own
    artifact only when both final and staged child directories are absent and
    the complete artifact matches its admitted digest under bounded validation.
    Persist a collection intent before invoking [f]; future passes may finish
    partially deleted artifacts under that same irrevocable intent. This uses
    ledger v3 only for marked records; old records and reference hashes persist.
    Installed/linked children, unrevoked attempts and every parent revision remain
    protected. The immutable retry record is never removed or unrevoked.
    [f] receives protected revisions and must add all catalog/session references,
    including archives. Invalid ledger/destinations, failed verification and
    exhausted budgets prevent [f]. Marked partial deletions are resumable. *)
val with_artifact_retention
  :  t
  -> max_records:int
  -> max_bytes:int
  -> max_artifact_entries:int
  -> max_artifact_bytes:int
  -> f:(Agent_protocol.Id.Prompt_revision.t list -> ('a, Store_error.t) result)
  -> ('a, Store_error.t) result
