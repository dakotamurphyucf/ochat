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

    [authored_capabilities] resolves an authored reservation's exact private tool
    closure. It is a trusted host adapter, unavailable by default. It must verify
    the recorded authored name/source against the parent's current, source-bound
    tool registration in [public], then return only that implementation's approved
    private resources, under its retained resource lease and actual caller policy.
    It must not resolve arbitrary files/names or return an administrative registry.
    The durable pins narrow this closure and must match the prepared child's live
    selection. The adapter is rechecked after ancestor lookups; it grants no tool
    execution or approvals. Each ancestor's public registry stays separate from
    the closure selected for its authored child. Missing adapters reject authored
    edges, including authored ancestors of otherwise generated children. *)
val create
  :  ?max_depth:int
  -> ?parent_stop_epoch:int64
  -> ?moderation:
       (Agent_protocol.Id.Session.t
        -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result)
  -> ?authorize_independent:
       (Agent_store.Delegation_store.record -> (unit, Agent_protocol.Error.t) result)
  -> ?authored_capabilities:
       (Agent_store.Delegation_store.record
        -> public:Chat_response.Tool_capability.t
        -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result)
  -> host:host
  -> reference:Agent_store.Delegation_store.Reference.t
  -> capabilities:Chat_response.Tool_capability.t
  -> unit
  -> t

val reference : t -> Agent_store.Delegation_store.Reference.t

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

(** Execution requires Linked, unrevoked admission and a running, unchanged parent
    with the exact current binding selection. Generation/source/policy/workspace
    changes or a missing/stopped ancestor deny, except execution liveness above a
    host-authorized Independent edge as described above. Walks only private parent references,
    rejecting cycles and depth excess (configurable, default32). Checks do not consume approval grants;
    the child's actual invocation/permission service still performs authorization. *)
val check_execution : t -> (unit, Agent_protocol.Error.t) result
