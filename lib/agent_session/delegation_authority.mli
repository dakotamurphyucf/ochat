open Core

(** Current authority for a generated runtime. The host supplies authoritative
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
    Stateful moderator enforcement and independent lifetime still require their
    owner-aware host integration; this initial guard rejects those configurations
    explicitly rather than treating a captured registry as permission. *)
val fingerprint : Session_state.t -> (string, Agent_protocol.Error.t) result

(** Bind a verified prepared child's exact live selection to its durable pointer.
    Does not grant permission or perform IO; [check_preparation] is mandatory before
    initialization and [check_execution] before every effectful runtime boundary. *)
val create
  :  ?max_depth:int
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
    changes or a missing/stopped ancestor deny. Walks only private parent references,
    rejecting cycles and depth excess (configurable, default32). Checks do not consume approval grants;
    the child's actual invocation/permission service still performs authorization. *)
val check_execution : t -> (unit, Agent_protocol.Error.t) result
