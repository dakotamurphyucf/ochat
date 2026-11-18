open Core

(** Current authority for a delegated runtime. The host supplies authoritative
    parent state, the private ledger and live delegable bindings. These callbacks
    must not use child-supplied identities or a broader administrative credential.
    This check does not replace coordinated stop/cancellation or resource leases. *)
type host =
  { state :
      Agent_protocol.Id.Session.t -> (Session_state.t, Agent_protocol.Error.t) result
  ; resolve :
      Agent_store.Delegation_store.Reference.t
      -> (Agent_store.Delegation_store.record, Agent_protocol.Error.t) result
  ; capabilities :
      Agent_protocol.Id.Session.t
      -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result
  }

(** Trusted host check at an already-bound shared checker frame. [expected] is
    that frame's ORIGINAL Actor snapshot and [record] its SAME resolved record.
    A host adding waits must validate its original identity/liveness read-set
    against fresh SAME-Actor state and recheck this record's specific Invocation
    ownership before returning. This issues no Source, custody or admission. *)
module Moderation_current : sig
  type t =
    Agent_store.Delegation_store.record
    -> expected:Session_state.t
    -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result
end

(** Factual comparison input captured from a trusted host's existing registry.
    Only the private ancestry walker can use it for pin and live identity checks. *)
module Binding_evidence : sig
  type t

  val of_registry : Chat_response.Tool_capability.t -> t
end

type t

(** Fingerprint the inherited policy/workspace/source context, excluding ordinary
    conversation changes and execution counters. The parent must be durable.
    A moderated parent requires the host's installed source identity in [moderator];
    it must match the persisted, non-halted checkpoint. Omission rejects moderated
    parents. Mutable moderator state is excluded from the fingerprint. This source
    proof does not replace owner-aware execution of the parent's policy. *)
val fingerprint
  :  ?moderator:Agent_protocol.Invocation.observer
  -> Session_state.t
  -> (string, Agent_protocol.Error.t) result

(** Bind a verified prepared child's exact live selection to its durable pointer.
    Does not grant permission or perform IO; [check_preparation] is mandatory before
    initialization and [check_execution] before every effectful runtime boundary.
    [parent_stop_epoch] pins the current loaded parent lifetime; omission uses the
    creation admission's epoch (legacy zero). A stop/restart invalidates that live
    owned guard even when the parent is running again.

    [authorize_independent] is a trusted host callback, omitted by default. It must
    validate the record's exact independent authorization digest against current
    host policy and independently owned resources. It is repeated after yielding
    ancestor/binding checks. Successful authorization removes execution-liveness
    and stop-counter dependence above that edge, including older Owned edges;
    Owned descendants below it still depend on their immediate running parent.
    Every ancestor's private identity, linkage, source, policy, workspace, selected
    bindings and revocation remain checked. Missing ancestors still deny. Existing
    parent-moderation requirements are unchanged; resource retention alone cannot
    authorize an unavailable policy handler. This does not install factory lifetime
    selection, stopped-ancestor resources or workspace retention.

    [authored_capabilities] resolves an ordinary or captured authored reservation's
    exact private tool closure, selected by the shared validated source accessor.
    It is a trusted host adapter, unavailable by default. It must verify
    the recorded authored name/source against the parent's current, source-bound
    tool registration in [public], then return only that implementation's approved
    private resources, under its retained resource lease and actual caller policy.
    It must not resolve arbitrary files/names or return an administrative registry.
    The durable pins narrow this closure and must match the prepared child's live
    selection. The adapter is rechecked after ancestor lookups; it grants no tool
    execution or approvals. Each ancestor's public registry stays separate from
    the closure selected for its authored child. Missing adapters reject authored
    edges, including authored ancestors of otherwise generated children.

    [policy_restoration_bindings] captures the Factory lookup for the original
    restoring owner. Omission uses the ordinary live capability lookup.

    [source_owner], when supplied by trusted Factory wiring, captures its original
    private issuer for [check_ancestry]. Omission leaves existing preparation and
    execution checks available, but ancestry observations unavailable. The issuer
    does not authenticate arbitrary host callbacks: consumers must match their
    original Factory issuer and independently re-resolve these records in their
    actual controller Store.

    [moderation_current], when supplied, exclusively replaces the id-only
    [moderation] callback at BOTH original shared checker callsites. Error and
    Ok None never fall back. Omission preserves the exact existing callback and
    ordering; no additional state read or permission check is introduced. *)
val create
  :  ?max_depth:int
  -> ?parent_stop_epoch:int64
  -> ?moderation:
       (Agent_protocol.Id.Session.t
        -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result)
  -> ?moderation_current:Moderation_current.t
  -> ?authorize_independent:
       (Agent_store.Delegation_store.record -> (unit, Agent_protocol.Error.t) result)
  -> ?authored_capabilities:
       (Agent_store.Delegation_store.record
        -> public:Chat_response.Tool_capability.t
        -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result)
  -> ?policy_restoration_bindings:
       (promoting:Agent_protocol.Id.Session.t
        -> Agent_protocol.Id.Session.t
        -> (Binding_evidence.t, Agent_protocol.Error.t) result)
  -> ?source_owner:Runtime_source_owner.t
  -> host:host
  -> reference:Agent_store.Delegation_store.Reference.t
  -> capabilities:Chat_response.Tool_capability.t
  -> unit
  -> t

val reference : t -> Agent_store.Delegation_store.Reference.t

(** Original exact selected binding DATA supplied at [create]. No lookup,
    rebinding, current grant or permission is inferred from this projection.
    Consumers must retain its actual resource lifetime and recheck execution
    and their own original/current authorization before effect/disclosure. *)
val original_capabilities : t -> Chat_response.Tool_capability.t

module Checked_ancestry : sig
  (** Bounded observation from the existing private-chain guard, never a current
      Goal policy grant or atomic multi-Actor snapshot. No decoder/constructor.
      Consumers must authenticate actual owner/Actor/Store routes independently. *)
  type t

  (** Nonempty leaf-to-root records, each retained after its frame's existing
      final yielding revalidation. Later changes do not mutate this observation;
      current root policy and private Store checks remain mandatory at consumers.
      Historical origin omissions are retained exactly, not reconstructed. *)
  val records : t -> Agent_store.Delegation_store.record list

  (** Compare against the private issuer captured at guard creation. Matching
      alone is not permission or proof that supplied host callbacks were genuine. *)
  val issued_by : t -> source_owner:Runtime_source_owner.t -> bool
end

(** Same private traversal and guards as [check_execution], including Linked,
    revocation, original lifetime/Independent handling and exact live bindings.
    Retains the revalidated leaf-to-root records and checks frozen Goal origins:
    known associated edges preserve every inherited scope exactly and add only
    scopes anchored to that actual local edge; explicit Unrelated is a birth
    boundary. Historical Unknown stays Unknown. A known Associated child above
    an Unknown delegated ancestor rejects rather than pretending a complete
    vector. Root/no-delegation termination needs no invented ancestor origin.

    Host lookups may yield: call outside Actor/Owner locks. This is an observation,
    not an atomic snapshot or a perpetually live grant. Requires [source_owner]
    captured at [create]; no issuer can be attached by this call. *)
val check_ancestry : t -> (Checked_ancestry.t, Agent_protocol.Error.t) result

(** Traverses the complete original authority during the lexical construction of
    a parent policy Product. It verifies private bindings and ancestry with the
    same walker as [check_ancestry]. The Factory supplies checked retained
    evidence for the actual restoring owner and normal live checks for all other
    ancestors. Actual policy effects require the ordinary current-use guards. *)
val check_policy_restoration_ancestry
  :  t
  -> promoting:Agent_protocol.Id.Session.t
  -> (Checked_ancestry.t, Agent_protocol.Error.t) result

(** Checks the actual private ancestry and the captured Artifact's immutable
    association to that same record before the normal preparation identity and
    profile checks. A decoded origin cannot replace this actual authority. *)
val check_captured_artifact_preparation
  :  t
  -> origin:Recurrence_child_origin.t
  -> session_id:Agent_protocol.Id.Session.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> manifest_sha256:string
  -> permission_profile:Permission_policy.t
  -> (unit, Agent_protocol.Error.t) result

(** Artifact-installed/child-installed/linked admission permits runtime preparation.
    Requires the exact inherited profile; children may narrow tools and add their
    own moderator restrictions but may not select a different permission profile. *)
val check_preparation
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> manifest_sha256:string
  -> permission_profile:Permission_policy.t
  -> (unit, Agent_protocol.Error.t) result

(** The same preparation gate, additionally requiring the exact ordinary or
    captured authored source selector and live prepared private registry. Prevents constructing a child with a
    different resource set than the one validated by this authority guard. *)
val check_authored_preparation
  :  t
  -> origin:Agent_store.Delegation_store.Admission.authored_tool
  -> capabilities:Chat_response.Tool_capability.t
  -> session_id:Agent_protocol.Id.Session.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> manifest_sha256:string
  -> permission_profile:Permission_policy.t
  -> (unit, Agent_protocol.Error.t) result

(** Execution requires Linked, unrevoked admission and a running, unchanged parent
    with the exact current binding selection. Generation/source/policy/workspace
    changes or a missing/stopped ancestor deny, except execution liveness above a
    host-authorized Independent edge as described above. Walks only private parent references,
    rejecting cycles and depth excess (configurable, default32). Checks do not consume approval grants;
    the child's actual invocation/permission service still performs authorization. *)
val check_execution : t -> (unit, Agent_protocol.Error.t) result

(** Check only the extra one-off invocation lifetime against an actual parent
    actor snapshot. Does not replace source, relationship, capability or policy
    authorization. Recovery uses this to stop a child whose invocation ended. *)
val check_invocation_owner
  :  Agent_store.Delegation_store.record
  -> Session_state.t
  -> (unit, Agent_protocol.Error.t) result

module Resource_path : sig
  type path

  (** Immutable bounded complete actual private path DATA, original leaf-to-root.
     Does not carry live capabilities, a Caller, effect permission or saved
     authorize closure. The actual Source leaf's saved parent stop epoch is
     immutable association data for this path issuance. Fixed Factory owns
     authentic Source_target separately; no whole State or consumer is retained. *)
  type t = path

  (** Validates nonempty/exact initial Reference, contiguous real records and
     bounds/cycles. ALL supplied records came from actual Factory metadata
     discovery. The SAME actual Factory leaf must match session, revision and
     private Reference; its saved parent_stop_epoch is captured for this Source
     preparation, rather than using the admission's birth epoch after a lawful
     restart. Issuer captures original Factory, not a new authority. *)
  val of_records
    :  source_owner:Runtime_source_owner.t
    -> max_depth:int
    -> actual_leaf:Session_state.t
    -> reference:Agent_store.Delegation_store.Reference.t
    -> records:Agent_store.Delegation_store.record list
    -> (t, Agent_protocol.Error.t) result

  val issued_by : t -> source_owner:Runtime_source_owner.t -> bool
  val reference : t -> Agent_store.Delegation_store.Reference.t
  val records : t -> Agent_store.Delegation_store.record list

  module Prefix : sig
    (** Exact nonempty prefix into SAME frozen path; no extra record lookup can
       grow membership. Endpoint is terminal record.parent_session_id, the selected
       resource Owner's actual session ID. *)
    type t

    val path : t -> path
    val records : t -> Agent_store.Delegation_store.record list

    (** TERMINAL actual record Reference of this prefix, not original leaf ref.
       Endpoint is that terminal record's parent, i.e. borrowed resource Owner. *)
    val reference : t -> Agent_store.Delegation_store.Reference.t

    val endpoint : t -> Agent_protocol.Id.Session.t
    val parent : t -> (t option, Agent_protocol.Error.t) result
  end

  val first : t -> Prefix.t

  module Host : sig
    (** SAME actual Factory adapters; no loader/permissions supplied by child. *)
    type t =
      { state :
          Agent_protocol.Id.Session.t -> (Session_state.t, Agent_protocol.Error.t) result
      ; resolve :
          Agent_store.Delegation_store.Reference.t
          -> (Agent_store.Delegation_store.record, Agent_protocol.Error.t) result
      ; moderation :
          Agent_protocol.Id.Session.t
          -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result
      ; authorize_independent :
          Agent_store.Delegation_store.record -> (unit, Agent_protocol.Error.t) result
      }
  end

  module Observation : sig
    (** Revalidated endpoint/read-set DATA, not a permanent native grant. *)
    type t

    module Stop_boundary : sig
      type t =
        | Ordinary
        | Independent_resource
    end

    val record : t -> Agent_store.Delegation_store.record
    val state : t -> Session_state.t

    (** Derived by real observe_prefix from the complete genuinely revalidated
       prefix INCLUDING terminal edge, and current Independent digest. Never
       inferred from terminal record lifetime alone.
       Basis takes THIS opaque Observation, not a freely supplied constructor
       or Boolean. Ordinary captures endpoint State stop/parent-stop fields;
       Independent_resource keeps SAME original resource-only Stop law. *)
    val stop_boundary : t -> Stop_boundary.t

    (** Pure exact issuance association, not permission or freshness. Requires
       the SAME frozen path object, prefix length and original source issuer.
       Another actual path to the same endpoint is refused. Prefixes separately
       advanced within the same path compare by that deliberate path identity
       and length. The observation retains only immutable prefix/record/State
       DATA; no leaf Handle, current callback or successful grant. *)
    val check_prefix
      :  t
      -> source_owner:Runtime_source_owner.t
      -> prefix:Prefix.t
      -> (unit, Agent_protocol.Error.t) result
  end

  (** Shared existing chain traversal, NATIVE purpose: exact frozen prefix,
     actual Linked/unrevoked records, actual parent.spec.delegation topology,
     source/generation/revision/profile/workspace/host Independent digest and
     unconditional Invocation_owned predicate; before Independent original
     ordinary liveness/first-parent Stop law, above it resource-only liveness.
     Existing frame-final revalidation order remains. Live selected bindings are
     not available at native metadata preflight, so are OBLIGATIONS, not approved
     by this call; real native-family check below plus actual consumer execution
     check must discharge them. This never returns Checked_ancestry.
     actual_leaf must be SAME genuine Factory Source_target snapshot and first
     reference, including the saved parent_stop_epoch captured at path creation;
     its actual tuple is checked by Factory before/after the call. A changed leaf
     epoch requires a new actual Source path, not mutation of an issued path. *)
  val observe_prefix
    :  ?moderation_current:Moderation_current.t
    -> Prefix.t
    -> source_owner:Runtime_source_owner.t
    -> host:Host.t
    -> actual_leaf:Session_state.t
    -> (Observation.t, Agent_protocol.Error.t) result
end

(** New NATIVE-ONLY preparation operation, on SAME ORIGINAL ordinary authority.
   It does not create/refresh t or change later check_execution/check_ancestry.
   Actual fixed Factory resource admission/Native_source_stage.check MUST run
   before and after via check_resource; matching path DATA alone is no grant.

   Reuses existing read_chain with an internal closed purpose, then exact SAME
   check_preparation_record. Requires same issuer and EXACT terminal
   Resource_path.Prefix.reference = Delegation_authority.reference t, not merely
   membership somewhere in full path. Terminal actual child_session_id and
   revision_id must equal SAME prepared child's actual session/revision supplied
   by Factory Native_setup and Source.validate_target; its source/profile/native
   generation association remains actual Factory/Cell validation. Only verified
   records STRICTLY LOWER than that terminal edge may justify relaxing this
   original authority's initial liveness (terminal edge retains its own ordinary
   Independent handling from existing read_chain). Reject truncated/foreign or
   wrong prepared-child prefix before invoking its source construction.
   The actual path leaf's captured current parent epoch applies only to its
   exact original leaf Reference. The terminal original authority's own saved
   epoch remains independent and takes precedence when those References coincide;
   a path never revives a stale original authority. Existing birth-epoch fallback
   is retained when that original authority did not save an epoch.
   Original initial expected capabilities, selected_bindings/authored closure,
   invocation predicate, source/profile/generation/private/revocation checks stay.
   Required execution liveness is derived only from genuinely revalidated lower
   Independent edge, never a caller Boolean. No native or model effect permission
   is issued. Returned unit cannot be retained as authorization. *)
val check_native_resource_preparation
  :  t
  -> prefix:Resource_path.Prefix.t
  -> check_resource:(unit -> (unit, Agent_protocol.Error.t) result)
  -> session_id:Agent_protocol.Id.Session.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> manifest_sha256:string
  -> permission_profile:Permission_policy.t
  -> (unit, Agent_protocol.Error.t) result

(** SAME exact terminal-authority/prepared-child/prefix law as
   check_native_resource_preparation above. Preserve existing authored origin,
   selected private capabilities, artifact/profile/source association validation
   from check_authored_preparation. Lexical native setup only; original ordinary
   check_authored_preparation and stored execution authority remain unchanged. *)
val check_native_resource_authored_preparation
  :  t
  -> prefix:Resource_path.Prefix.t
  -> check_resource:(unit -> (unit, Agent_protocol.Error.t) result)
  -> origin:Agent_store.Delegation_store.Admission.authored_tool
  -> capabilities:Chat_response.Tool_capability.t
  -> session_id:Agent_protocol.Id.Session.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> manifest_sha256:string
  -> permission_profile:Permission_policy.t
  -> (unit, Agent_protocol.Error.t) result

(** SAME exact terminal-authority/prepared-child/prefix law. Preserve existing
   captured Artifact's immutable actual origin/record association, followed by
   exact preparation identity/profile checks from
   check_captured_artifact_preparation. Lexical native setup only; original
   ordinary operation and stored Captured authority checkpoints stay unchanged.
   Neither method introduces a generic family decoder or Source/Actor dependency. *)
val check_native_resource_captured_artifact_preparation
  :  t
  -> prefix:Resource_path.Prefix.t
  -> check_resource:(unit -> (unit, Agent_protocol.Error.t) result)
  -> origin:Recurrence_child_origin.t
  -> session_id:Agent_protocol.Id.Session.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> manifest_sha256:string
  -> permission_profile:Permission_policy.t
  -> (unit, Agent_protocol.Error.t) result
