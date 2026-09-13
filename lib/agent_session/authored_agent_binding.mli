open Core

(** Host-owned association between a captured authored declaration, its native
    wrapper registration and its exact approved private tool resources. Construct
    resources under host policy first; this module never grants new resources,
    constructs runners, loads files, or authorizes execution/approvals. *)
type t

(** Stable wrapper implementation identity includes the authored source identity
    and private permission/resource pins. Re-registration gets fresh capability
    IDs, while unchanged resources can retain this revision across restart.
    Declared delegation restrictions reject before binding. Managed handlers still
    require the host's managed-definition admission and dispatch services. *)
val implementation_revision
  :  source:Authored_agent_source.t
  -> capabilities:Chat_response.Tool_capability.t
  -> (string, Agent_protocol.Error.t) result

(** [public] must be the actual owning runtime registry containing the native
    wrapper with the revision above. [capabilities] must be the corresponding
    host-prepared private resource closure, not a caller-supplied registry.
    The private selection is never merged into [public]. *)
val bind
  :  source:Authored_agent_source.t
  -> public:Chat_response.Tool_capability.t
  -> reference:Chat_response.Tool_capability.reference
  -> capabilities:Chat_response.Tool_capability.t
  -> (t, Agent_protocol.Error.t) result

(** Resolve an already verified durable authored record against this exact
    source-bound wrapper and current live resources. Intended for
    Delegation_authority's authored-capabilities callback, including ancestry
    checks. The host must retain the resource lease and supply current registries.
    Same-name replacement, narrowed-away wrapper, changed source/private resources
    and generated records reject. This does not authenticate a record or caller;
    the common authority service still checks the ledger, parent, lifecycle,
    durable resource pins and actual execution permissions. *)
val resolve
  :  t
  -> record:Agent_store.Delegation_store.record
  -> public:Chat_response.Tool_capability.t
  -> current:Chat_response.Tool_capability.t
  -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result
