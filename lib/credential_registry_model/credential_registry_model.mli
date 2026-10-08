open! Core

(** DRAFT pure metadata contracts; no I/O, secret serializers or inference policy. *)
module Error : sig
  type t =
    | Invalid_identity
    | Invalid_grant
    | Invalid_document
    | Unsupported_schema
    | Missing_registry
    | Wrong_incarnation
    | Capacity
    | Epoch_exhausted
    | Stale_epoch
    | Stale_revision
    | Stale_operation
    | Disabled
    | Renewal_uncertain
    | Not_active
  [@@deriving equal, sexp_of]
end

module Id : sig
  (** Nonsecret safe ASCII <=48 bytes; random operation identities, no token hashes. *)
  type t [@@deriving compare, equal, sexp_of]

  val create : string -> (t, Error.t) result
  val to_string : t -> string
end

module Epoch : sig
  type t [@@deriving compare, equal, sexp_of]

  val of_int64 : int64 -> (t, Error.t) result
  val to_int64 : t -> int64
end

module Presence : sig
  type 'a t =
    | Absent
    | Null
    | Value of 'a
  [@@deriving sexp_of]
end

module Identity : sig
  type method_ =
    | Api_key of { key_reference : Id.t }
    | Oauth of
        { issuer : string
        ; client_registration : string
        ; resource : string
        ; verified_subject : string
        ; required_scopes : string list
        }
  [@@deriving equal, sexp_of]

  type t [@@deriving equal, sexp_of]

  (** API-key method has no fictitious issuer, OAuth subject or grant scopes. *)
  val api_key
    :  host:Id.t
    -> provider:string
    -> billing:string
    -> account:string option
    -> key_reference:Id.t
    -> (t, Error.t) result

  (** All issuer/client/resource/account/subject fields must already be verified
      by trusted66 policy; metadata constructor validates bounded syntax. *)
  val oauth
    :  host:Id.t
    -> provider:string
    -> billing:string
    -> issuer:string
    -> client_registration:string
    -> resource:string
    -> account:string
    -> verified_subject:string
    -> required_scopes:string list
    -> (t, Error.t) result

  val host : t -> Id.t
  val provider : t -> string
  val billing : t -> string
  val account : t -> string option
  val method_ : t -> method_
end

module Expectation : sig
  (** Trusted operator acquisition intent. Unknown subject/account is permitted
      only for initial interactive acquisition; successful grant establishes the
      exact Identity. It is never a captured inference binding or fallback. *)
  type t

  val exact : Identity.t -> t

  val oauth_acquisition
    :  host:Id.t
    -> provider:string
    -> billing:string
    -> issuer:string
    -> client_registration:string
    -> resource:string
    -> account:string option
    -> required_scopes:string list
    -> (t, Error.t) result

  (** Configured OAuth requirements, not the scopes granted by the provider.
      Exact OAuth expectations retain the original identity requirements; API-key
      expectations return [None]. Grant construction separately proves coverage. *)
  val oauth_required_scopes : t -> string list option

  val accepts : t -> Identity.t -> bool
end

module Grant : sig
  type refresh_policy =
    | Preserve_omitted
    | Require_rotated
  [@@deriving sexp_of]

  type provenance =
    | Declared
    | Qualified_prior_exact
    | Qualified_request
    | Qualified_token_claim
  [@@deriving equal, sexp_of]

  type expiry =
    | Unknown
    | Known of
        { at_ms : int64
        ; provenance : provenance
        }
  [@@deriving sexp_of]

  type unknown_expiry_policy =
    | Reject_unknown
    | Allow_qualified_unknown
  [@@deriving sexp_of]

  type effective =
    { scopes : string list
    ; scopes_provenance : provenance
    ; expiry : expiry
    ; unknown_expiry_policy : unknown_expiry_policy
    }
  [@@deriving sexp_of]

  type t [@@deriving sexp_of]

  (**66 verifies actual route/grant policy. Raw presence is retained; effective
      scope/expiry never guessed. Raw Value must match declared effective value.
      Prior derivation only under qualified exact-identity preservation policy.
      Qualified_token_claim is permitted only when trusted66 authenticated an
      access-token scope claim under explicit provider policy; the metadata
      constructor/codec do not perform or infer that verification. *)
  val create
    :  identity:Identity.t
    -> scopes:string list Presence.t
    -> expires_at_ms:int64 Presence.t
    -> refresh_policy:refresh_policy
    -> effective:effective
    -> (t, Error.t) result

  val identity : t -> Identity.t
  val scopes : t -> string list Presence.t
  val expires_at_ms : t -> int64 Presence.t
  val refresh_policy : t -> refresh_policy
  val effective : t -> effective
end

module Active : sig
  type source =
    | Protected_revision of Id.t
    | Environment_reference of
        { name : string
        ; configuration_revision : Id.t option
        }
  [@@deriving sexp_of]

  type t [@@deriving sexp_of]

  val identity : t -> Identity.t
  val epoch : t -> Epoch.t
  val source : t -> source
  val grant : t -> Grant.t option
end

module Expected : sig
  (** CAS includes absence of active revision for first login; operation binds the
      durable candidate/refresh intent. No caller-controlled epoch substitution. *)
  type t [@@deriving sexp_of]

  val epoch : t -> Epoch.t
  val revision : t -> Id.t option
  val operation : t -> Id.t
end

module Snapshot : sig
  type refresh =
    | Idle
    | Possibly_sent of Id.t
    | Renewal_uncertain of Id.t
  [@@deriving sexp_of]

  type disabled_reason =
    | Logout
    | Key_removed
    | Environment_disabled
    | Renewal_rejected
  [@@deriving sexp_of]

  type t [@@deriving sexp_of]

  val active : t -> Active.t option
  val epoch : t -> Epoch.t
  val disabled : t -> disabled_reason option
  val refresh : t -> refresh
end

module Removal : sig
  type drain =
    | Pending
    | Drained
  [@@deriving sexp_of]

  type revocation =
    | Not_requested
    | Unavailable
    | Possibly_sent
    | Revoked
  [@@deriving sexp_of]

  type t

  val operation : t -> Id.t
  val drain : t -> drain
  val revocation : t -> revocation
end

module Cleanup : sig
  type deletion =
    | Pending
    | Removed_durability_unknown
    | Confirmed_removed
    | Quarantined
  [@@deriving sexp_of]

  type retired

  val revision : retired -> Id.t
  val owned_by : retired -> Id.t
  val deletion : retired -> deletion
end

module Operation : sig
  type result =
    | Pending
    | Committed
    | Rejected
    | Unavailable
  [@@deriving sexp_of]

  (** Durable bounded operation receipt survives later unrelated transactions.
      Unresolved operations cannot be evicted; capacity rejects before effects. *)
  type t

  val id : t -> Id.t
  val result : t -> result
end

type t

val retired : t -> binding:Id.t -> (Cleanup.retired list, Error.t) result
val operation : t -> binding:Id.t -> operation:Id.t -> (Operation.t, Error.t) result

(** Explicit initial document; never used as missing/corrupt-load fallback.
    Universal document preserves unknown fields and absent/null/value. *)
val initialize : incarnation:Id.t -> host:Id.t -> (t, Error.t) result

val of_document : Document_schema.Document.t -> (t, Error.t) result
val to_document : t -> (Document_schema.Document.t, Error.t) result
val incarnation : t -> Id.t
val host : t -> Id.t
val binding_ids : t -> Id.t list
val find : t -> binding:Id.t -> (Snapshot.t, Error.t) result

(** Creates a distinct candidate without changing working active credentials. *)
val begin_candidate
  :  t
  -> binding:Id.t
  -> operation:Id.t
  -> expectation:Expectation.t
  -> (t * Expected.t, Error.t) result

val discard_candidate : t -> binding:Id.t -> expected:Expected.t -> (t, Error.t) result

(** Material has already been verified and staged; model carries only revision.
    Login/replacement advances epoch. Refresh does not.
    Stage revision is deterministically operation-owned and recorded BEFORE
    backend creation. Tombstone can retire it even after creator process dies.
    Unknown document fields remain attached to edited nested records. *)
val stage_candidate
  :  t
  -> binding:Id.t
  -> expected:Expected.t
  -> revision:Id.t
  -> (t, Error.t) result

val commit_candidate
  :  t
  -> binding:Id.t
  -> expected:Expected.t
  -> identity:Identity.t
  -> source:Active.source
  -> grant:Grant.t option
  -> (t, Error.t) result

val begin_refresh
  :  t
  -> binding:Id.t
  -> operation:Id.t
  -> (t * Expected.t, Error.t) result

val clear_definitely_unsent_refresh
  :  t
  -> binding:Id.t
  -> expected:Expected.t
  -> (t, Error.t) result

val mark_renewal_uncertain
  :  t
  -> binding:Id.t
  -> expected:Expected.t
  -> (t, Error.t) result

(** Like candidate staging, revision ownership is durable before secret create.
    Expected includes the exact Possibly_sent operation and old active revision. *)
val stage_refresh
  :  t
  -> binding:Id.t
  -> expected:Expected.t
  -> revision:Id.t
  -> (t, Error.t) result

val commit_refresh
  :  t
  -> binding:Id.t
  -> expected:Expected.t
  -> revision:Id.t
  -> grant:Grant.t
  -> (t, Error.t) result

(** Tombstone advances epoch and invalidates candidates/refresh CAS. Cleanup
    progress remains operation-owned; no secret deletion is implied. *)
val disable
  :  t
  -> binding:Id.t
  -> operation:Id.t
  -> reason:Snapshot.disabled_reason
  -> (t, Error.t) result

(** Retired revisions must be explicitly operation-owned and inactive; never
    infer ownership by token inspection or delete active/unknown orphan items. *)
val record_cleanup
  :  t
  -> binding:Id.t
  -> operation:Id.t
  -> revision:Id.t
  -> deletion:Cleanup.deletion
  -> (t, Error.t) result

(** Persisted tombstone progress; absence never proves an exclusive drain. *)
val removal : t -> binding:Id.t -> (Removal.t option, Error.t) result

val record_removal
  :  t
  -> binding:Id.t
  -> operation:Id.t
  -> drain:Removal.drain
  -> revocation:Removal.revocation
  -> (t, Error.t) result

val candidate_pending : t -> binding:Id.t -> (bool, Error.t) result

(** Unresolved remote outcomes remain nonsecret evidence after local erasure. *)
val revocation_history : t -> binding:Id.t -> (Removal.t list, Error.t) result

(** Foreign/existing staged secret is quarantined, never activated or deleted;
    prior active identity remains, rotating-token outcome remains uncertain. *)
val quarantine_refresh : t -> binding:Id.t -> expected:Expected.t -> (t, Error.t) result

val pending_candidate_operation : t -> binding:Id.t -> (Id.t option, Error.t) result
val cancel_pending_candidate : t -> binding:Id.t -> operation:Id.t -> (t, Error.t) result
