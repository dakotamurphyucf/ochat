open! Core

(** DRAFT Eio host lifecycle owner. References Model = Credential_registry_model. *)
module Error : sig
  type t =
    | Model of Credential_registry_model.Error.t
    | Storage of Private_storage.Error.code
    | Secret_store of Provider_secret_store.Error.code
    | Missing_secret
    | Binding_unavailable
    | Authorization_denied
    | Busy
    | Timed_out
    | Closed
    | Publication_uncertain
    | Renewal_rejected
    | Revision_quarantined
  [@@deriving sexp_of]
end

module Status : sig
  type availability =
    | Available
    | Missing
    | Disabled
    | Renewal_required
    | Renewal_uncertain
    | Secret_unavailable
    | Store_unavailable
  [@@deriving equal, sexp_of]

  type drain =
    | Drained
    | Drain_pending
  [@@deriving equal, sexp_of]

  type secret_cleanup =
    | Clean
    | Cleanup_pending
    | Cleanup_durability_unknown
    | Cleanup_quarantined
  [@@deriving equal, sexp_of]

  type revocation =
    | Not_requested
    | Revoked
    | Revocation_pending
    | Revocation_unavailable
  [@@deriving equal, sexp_of]

  type cleanup =
    { drain : drain
    ; secrets : secret_cleanup
    ; revocation : revocation
    }
  [@@deriving sexp_of]

  type candidate =
    | No_candidate
    | Candidate_pending
    | Candidate_cleanup_pending
  [@@deriving equal, sexp_of]

  type t

  (** Authorized projection only, bounded finite tags and nonsecret identity.
      No token/revision hashes/raw provider error strings. *)
  val availability : t -> availability

  val cleanup : t -> cleanup
  val candidate : t -> candidate
  val pending_candidate_operation : t -> Credential_registry_model.Id.t option
end

module Material : sig
  (** Protected token bundle has no sexp/equality/public JSON.
      163 private codec persists bundle through162 only;66 supplies verified
      access/refresh material and declared grant presence policy. Continuity is
      opaque protected provider proof (e.g. original nonce/auth_time).66 owns its
      bounded codec and verification. Refresh must supply complete continuity;
      an existing Value cannot be silently omitted/null-discarded. *)
  type t

  val api_key : Provider_secret_store.Secret.t -> t

  val oauth
    :  access:Provider_secret_store.Secret.t
    -> refresh:Provider_secret_store.Secret.t Credential_registry_model.Presence.t
    -> continuity:Provider_secret_store.Secret.t Credential_registry_model.Presence.t
    -> t

  (** Trusted renewal/revocation consumer only, borrowed protected material.
      API-key material is rejected; no serializer, token equality or logging. *)
  val with_oauth
    :  t
    -> f:
         (access:Provider_secret_store.Secret.t
          -> refresh:Provider_secret_store.Secret.t Credential_registry_model.Presence.t
          -> continuity:
               Provider_secret_store.Secret.t Credential_registry_model.Presence.t
          -> 'a)
    -> ('a, Error.t) result
end

module Verified : sig
  (** Constructed through trusted policy callback after66 validates actual
      issuer/client/resource/account/subject/scopes/expiry. Never caller JWT JSON.
      Constructor still validates matching identity, grant, material kind and
      refresh presence policy; structural trust never skips these invariants. *)
  type t

  val create
    :  identity:Credential_registry_model.Identity.t
    -> grant:Credential_registry_model.Grant.t option
    -> material:Material.t
    -> (t, Error.t) result
end

module Renewal : sig
  (** Trusted ports must use the supplied switch for every owned network/helper
      operation, honor cancellation, enforce qualified route response/time
      bounds, and join their work before returning/raising. No detached tasks.
      Unexpected exceptions propagate while the durable intent stays uncertain. *)
  type outcome =
    | Verified of Verified.t
    | Definitely_not_submitted
    | Authoritative_rejection
    | Possibly_consumed

  type t

  (** Invoked only after durable Possibly_sent intent. The port is trusted to
      certify Definitely_not_submitted; arbitrary exception/decode/timeout is
      uncertain. It performs no login or implicit fallback acquisition.
      Returned Verified must match exact expected identity and grant policy. *)
  val create
    :  exchange:
         (sw:Eio.Switch.t
          -> identity:Credential_registry_model.Identity.t
          -> grant:Credential_registry_model.Grant.t
          -> Material.t
          -> outcome)
    -> t
end

module Environment : sig
  type resolved

  (** Explicit trusted63 host port resolves only declared names for an exact API
      binding. It may inspect host environment;163 never performs ambient lookup.
      No serializer, process argument or secret-bearing error is provided. *)
  val resolved
    :  access:Provider_secret_store.Secret.t
    -> configuration_revision:Credential_registry_model.Id.t option
    -> check_current:(unit -> (unit, Error.t) result)
    -> resolved

  type t

  val create
    :  resolve:
         (sw:Eio.Switch.t
          -> binding:Credential_registry_model.Id.t
          -> identity:Credential_registry_model.Identity.t
          -> name:string
          -> expected_configuration_revision:Credential_registry_model.Id.t option
          -> (resolved, Error.t) result)
    -> status:
         (binding:Credential_registry_model.Id.t -> name:string -> Status.availability)
    -> t
end

(** One Eio domain owns each registry instance and its mutable cache/lifetime
    state. Independent instances and processes coordinate through native M/G/R;
    sharing one [t] across OCaml domains is unsupported. *)

type t

(** Borrow162 directory/secret backend; keep both open through close. Existing
    open never initializes or replaces absent/corrupt/unsupported authority. *)
val open_existing
  :  sw:Eio.Switch.t
  -> wall_clock:_ Eio.Time.clock
  -> new_operation:(unit -> Credential_registry_model.Id.t)
  -> directory:Private_storage.Directory.t
  -> secrets:Provider_secret_store.t
  -> environment:Environment.t option
  -> host:Credential_registry_model.Id.t
  -> (t, Error.t) result

(** Explicit trusted provisioning only; create-only metadata and stable locks.
    Requires otherwise uninitialized registry namespace; no secret migration. *)
val initialize_new
  :  sw:Eio.Switch.t
  -> wall_clock:_ Eio.Time.clock
  -> new_operation:(unit -> Credential_registry_model.Id.t)
  -> directory:Private_storage.Directory.t
  -> secrets:Provider_secret_store.t
  -> environment:Environment.t option
  -> host:Credential_registry_model.Id.t
  -> incarnation:Credential_registry_model.Id.t
  -> (t, Error.t) result

(** Nonblocking lock admission: Busy if M cannot be obtained immediately.
    Native storage calls are joined and byte-bounded, not falsely timed out.
    UTC expiry uses explicit wall_clock borrowed at open. No held locks returned. *)
val status : t -> binding:Credential_registry_model.Id.t -> (Status.t, Error.t) result

module Candidate : sig
  type t
end

(** These administrative calls use nonblocking M admission (Busy). They join
    owned native work without claiming a hard I/O deadline. Callers may cancel;
    metadata ambiguity must reconcile the original operation, never new-key retry. *)
val begin_candidate
  :  t
  -> binding:Credential_registry_model.Id.t
  -> operation:Credential_registry_model.Id.t
  -> expectation:Credential_registry_model.Expectation.t
  -> (Candidate.t, Error.t) result

val cancel_candidate : t -> Candidate.t -> (unit, Error.t) result

(** Explicit trusted-host recovery only. The service consumer must authorize the
    flow owner before exposing this operation. Exact original operation CAS;
    existing active login remains. Owned staged cleanup is reconciled separately. *)
val cancel_pending_candidate
  :  t
  -> binding:Credential_registry_model.Id.t
  -> operation:Credential_registry_model.Id.t
  -> (unit, Error.t) result

(** Optional host-only, non-yielding authorization check under the final metadata
    lease after secret staging and metadata reload, immediately before admitting
    the active-pointer transition. Defaults to an already-authorized trusted host
    call. A denied candidate is discarded by original CAS, preserving the old
    login and recording staged cleanup. Cleanup failure retains its typed error.
    Unexpected guard exceptions propagate. Publication admitted by this check may
    finish after wall-clock expiry; the guard must not perform I/O. *)
val commit_candidate
  :  ?authorize_commit:(unit -> bool)
  -> t
  -> Candidate.t
  -> Verified.t
  -> (unit, Error.t) result

(** Explicit63 configuration path, exact API-key Expectation only. Publishes
    reference metadata/epoch, no token copied into metadata or secret backend.
    Environment port must be present; declared variable syntax is validated.
    Configuration revision supplied by trusted host, never a token hash.
    Authorization admission has the same contract as [commit_candidate]. *)
val commit_environment_candidate
  :  ?authorize_commit:(unit -> bool)
  -> t
  -> Candidate.t
  -> identity:Credential_registry_model.Identity.t
  -> name:string
  -> configuration_revision:Credential_registry_model.Id.t option
  -> (unit, Error.t) result

module Admission : sig
  type t

  (** Exact admitted identity; consumers compare against their approved profile
      binding before borrowing material. Owner/epoch alone do not prove pairing. *)
  val identity : t -> Credential_registry_model.Identity.t

  val owner : t -> string
  val epoch : t -> int64
  val credential_revision : t -> string option

  (** Trusted driver adapter borrows access material; not a public serializer. *)
  val with_access : t -> f:(Provider_secret_store.Secret.t -> 'a) -> 'a

  (** Non-yielding cached-host guard; cross-process currentness was proved underG
      at admission. Compose with62 currentness, never erase either check. *)
  val check_current : t -> (unit, Error.t) result
end

(** SharedG retained in attempt switch; optionalR renews exact existing binding.
    Never login. expected owner/epoch comes from62 captured host registry identity.
    Idle WS must retain no Admission; every request admits afresh.
    Environment source requires explicit port, exact identity/config revision,
    rechecks composed configuration guard after yielding resolution and before
    credential publication; missing revision prohibits WS reuse. No OAuth/env
    fallback. Disabled metadata wins even if ambient variable still exists. *)
val admit
  :  t
  -> sw:Eio.Switch.t
  -> clock:_ Eio.Time.Mono.t
  -> maximum_wait:Time_ns.Span.t
  -> binding:Credential_registry_model.Id.t
  -> expected_owner:string
  -> expected_epoch:int64
  -> renewal:Renewal.t option
  -> (Admission.t, Error.t) result

val refresh
  :  t
  -> sw:Eio.Switch.t
  -> clock:_ Eio.Time.Mono.t
  -> maximum_wait:Time_ns.Span.t
  -> binding:Credential_registry_model.Id.t
  -> renewal:Renewal.t
  -> (unit, Error.t) result

module Revocation : sig
  type outcome =
    | Revoked
    | Definitely_not_submitted
    | Possibly_submitted
    | Unavailable

  type t

  (** Only a qualified provider route may construct this port. No endpoint is
      inferred. Uncertainty never undoes local disable and never auto-retries. *)
  val create
    :  revoke:
         (sw:Eio.Switch.t
          -> identity:Credential_registry_model.Identity.t
          -> grant:Credential_registry_model.Grant.t
          -> Material.t
          -> outcome)
    -> t
end

type removal =
  { disabled : bool
  ; cleanup : Status.cleanup
  }
[@@deriving sexp_of]

(** DurableM tombstone first, releaseM, then exclusiveG→R→M reconciliation.
    Timeout is a typed pending outcome; stale staged pointers cannot publish.
    Environment removal disables only this binding, not ambient process bytes. *)
val disable
  :  t
  -> sw:Eio.Switch.t
  -> clock:_ Eio.Time.Mono.t
  -> maximum_wait:Time_ns.Span.t
  -> binding:Credential_registry_model.Id.t
  -> revocation:Revocation.t option
  -> reason:Credential_registry_model.Snapshot.disabled_reason
  -> (removal, Error.t) result

(** Host durable-intent admission. Fresh is allowed only for a newly persisted
    original command intent; Reconcile never creates a missing operation. A proven
    committed original tombstone may finish its local drain/owned cleanup on retry
    after an exact recheck under the dispatch/refresh locks: no epoch increment,
    revocation retry or mutation of a later reauthorized binding. Superseded or
    unavailable proof returns Binding_unavailable. *)
type operation_mode =
  | Fresh
  | Reconcile
[@@deriving sexp_of]

val disable_with_operation
  :  t
  -> sw:Eio.Switch.t
  -> clock:_ Eio.Time.Mono.t
  -> maximum_wait:Time_ns.Span.t
  -> binding:Credential_registry_model.Id.t
  -> operation:Credential_registry_model.Id.t
  -> mode:operation_mode
  -> revocation:Revocation.t option
  -> reason:Credential_registry_model.Snapshot.disabled_reason
  -> (removal, Error.t) result

(** Reconcile pointer acknowledgement and operation-owned inactive cleanup; no
    external rotating-token retry. Uncertainty remains until qualified66 policy
    or explicit reauthorization. *)
val reconcile : t -> binding:Credential_registry_model.Id.t -> (Status.t, Error.t) result

(** Exact durable receipt readback after ambiguous pointer publication. Missing
    receipt or superseded proof means Unavailable, never presumed rollback or
    reuse of an older revision. Same original operation may be queried again. *)
val reconcile_operation
  :  t
  -> binding:Credential_registry_model.Id.t
  -> operation:Credential_registry_model.Id.t
  -> (Credential_registry_model.Operation.result, Error.t) result

module Host_snapshot : sig
  type availability =
    | Ready
    | Missing
    | Disabled
    | Renewal_required
    | Renewal_uncertain
  [@@deriving equal, sexp_of]

  (** Immutable nonsecret host view, no callbacks or second registry authority.
      incarnation/epoch drive62 identity; revision drives58 channel reuse. *)
  type binding

  type t

  val bindings : t -> binding list
  val id : binding -> Credential_registry_model.Id.t
  val owner : binding -> string
  val epoch : binding -> int64
  val credential_revision : binding -> string option
  val identity : binding -> Credential_registry_model.Identity.t option
  val source : binding -> Credential_registry_model.Active.source option
  val availability : binding -> availability
  val pending_candidate_operation : binding -> Credential_registry_model.Id.t option
end

(** Metadata-only nonblockingM reload returns the durable immutable view.
    Ready means configured, not probed secret availability; no secret read or
    environment callback occurs. Explicit status/admission require authorization.
 Trusted host owner
    applies epoch changes to62 before resolving a fresh context; refresh-only
    revision changes never invoke62 reauthorize from credential lookup. Admission
    mismatch returns typed stale identity; caller synchronizes and resolves again.
    No implicit mutable callbacks, watchers or duplicate credential authority. *)
val synchronize : t -> (Host_snapshot.t, Error.t) result

(** Validated metadata-only registry incarnation, including an empty authority.
    Used by trusted operator owner records; does not read credentials. *)
val incarnation : t -> (Credential_registry_model.Id.t, Error.t) result

(** Close rejects new calls, joins owned work. Active attempt locks release with
    caller switches; close does not silently undo an admitted request. *)
val close : t -> unit

module For_testing : sig
  (** Instance-local, absent by default. Called after actual native metadata
      replacement AND directory sync succeed, before cache acknowledgment. This
      models lost acknowledgment; it does not simulate directory-sync failure. *)
  val set_after_publication_hook : t -> (unit -> unit) option -> unit
end
