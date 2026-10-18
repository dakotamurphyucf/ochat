open Core

(** Trusted selected native executor issued from one physical developer root.
    The package binds actual actor/capture, stable invocation issuer, current
    selected registry and exact scope-bound Store writer. Caller coordinates
    cannot construct it, and an expiring child borrow cannot be used as Root. *)
type t

(** Comparison only: this package retains the exact physical Root. Equal
    owner coordinates grant no invocation or effect; every call repeats its
    own current actor, selected policy and parent checks. *)
val issued_by_root : t -> Developer_root_owner.run -> bool

val create : root:Developer_root_owner.t -> (t, Agent_protocol.Error.t) result

(** Compose one physically issued developer Script Job service with this same
    selected executor; the ordinary no-Job caller keeps the original path.
    Reattaching that exact physical service is idempotent, including after a
    managed definition is installed. A different service cannot replace it. *)
val with_job_service
  :  t
  -> Developer_script_job_service.t
  -> (t, Agent_protocol.Error.t) result

(** Install the exact managed definition forwarded by the trusted host's one
    prepared live-resource package, after this physical Root and its Job
    service exist. The private adapter checks Root/Job and selected binding
    affinity now; each call repeats genuine managed, terminal and scope
    admission. The host must not substitute a definition from another live
    package merely because its capability fingerprint compares equal after a
    source rebind. Installation rejects on an already configured receiver;
    the trusted host retains one configured executor value for this Root.
    No interpreter or handler runs at installation. *)
val with_managed_definition
  :  t
  -> definition:Chat_response.Managed_tool_registry.t
  -> env:Eio_unix.Stdenv.base
  -> (t, Agent_protocol.Error.t) result

(** Comparison-only installed definition for a physically bound Worker caller
    that must derive an exact managed revision/schema. This never authorizes
    dispatch and returns None when the trusted host installed no managed tools. *)
val managed_definition : t -> Chat_response.Managed_tool_registry.t option

module Child : sig
  type parent = t
  type t

  (** Enter only from the physically selected saved-Agent callback. The
      selected [target] is already the exact restored child: this constructor
      does not select a File role again. [prepared] is the joined child
      resource receipt from the same Live Root and target, never a record of
      caller-supplied registries. The first invocation uses the restricted
      Source_child issuer; nested invocations use that issuer's ordinary Child
      ancestry. A failed source/Root/Live proof denies without falling back to
      the parent Root registry or executor. *)
  val with_target
    :  parent:parent
    -> target:Agent_session.Captured_agent_runner.Claimed_developer.Selected.Target.t
    -> prepared:Developer_live_resources.Child_prepared.t
    -> (t -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result

  (** The same child-only native/managed/Job kernel, nested beneath the
      immediately admitted Saved or Content child. The lower Content target
      must match [parent]'s physical target and its original saved ancestry;
      [prepared] must be issued by that same Live Root for this exact Content
      target. Synchronous Content recursion need not enter a new native borrow:
      capability selection narrows from the immediate logical Child selection,
      while the source-child issuer derives from the ACTUAL entered native
      borrow and active Claim. A later real tool dispatch checks native
      ancestry against that physical borrow. No Root registry widening or new
      native/Store execution owner is created. The child issuer joins before
      its immediate parent retires. *)
  val with_content_target
    :  parent:t
    -> target:Agent_session.Captured_agent_runner.Selected_content_target.t
    -> prepared:Developer_live_resources.Child_prepared.t
    -> (t -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result

  (** The same child-only kernel for an authenticated Model-selected member.
      Exact immediate Child selection/Root/Target and physical borrowed scope
      are rechecked before issuer construction; a Model path is not a Content
      occurrence and cannot inherit the saved Tool registry by default. *)
  val with_model_target
    :  parent:t
    -> target:Agent_session.Captured_agent_runner.Selected_model_target.t
    -> prepared:Developer_live_resources.Child_prepared.t
    -> (t -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result

  (** Checked current projection for constructing this child's exact provider
      descriptors and Host_invocation fingerprint. It refreshes the same
      source/Root/Live admission used by [run], including descendant narrowing;
      it grants no execution without the subsequent physical [run] check. *)
  val current_capabilities
    :  t
    -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result

  (** The existing physical native policy, terminal, Job and managed kernel
      with the child-only admitted selection. [native_invocation] is the exact
      Driver tool-call observer and inference parent; the kernel lends it to
      the native implementation only during this invocation. Each attempt
      refreshes its source/Root witness and checks it after waits. *)
  val run
    :  t
    -> reference:Chat_response.Tool_capability.reference
    -> invocation:Agent_protocol.Host_invocation.t
    -> native_invocation:Ochat_function.Invocation.t
    -> authorize:
         (Agent_protocol.Host_invocation.t
          -> Chat_response.Tool_capability.binding
          -> (unit, Agent_protocol.Error.t) result)
    -> prepare_output:
         (Openai.Responses.Tool_output.Output.t
          -> (Jsonaf.t, Agent_protocol.Error.t) result)
    -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result
end

(** Function, Custom and owned managed calls use one existing native policy
    kernel. The host creates one bounded registered terminal scope per call;
    Dispatching is durable before effect and a terminal plus exact selected
    children is submitted through the root's one queue. Uncertain dispatch or
    terminal saves retain physical custody without handler replay. *)
val run_root
  :  t
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Host_invocation.t
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result

(** Direct child of the current selected native callback. Obtains the live
    physical parent proof from its expiring borrow, verifies this exact issuer,
    inherits any physical claimed-job ancestor, and narrows against that
    borrow's current capability ceiling. The child joins within the parent's
    Eio callback switch. *)
val run_child
  :  t
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Host_invocation.t
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result

(** Direct child of the actual claimed worker callback owned by this root.
    The root derives an abstract job-parent proof from its real Worker issuer;
    the same selected registry, policy, dispatch and terminal path is used as
    [run_root]. Invocation context must name the exact claimed job and no
    parent invocation. This grants no authority from a saved Running row.
    [selected], when supplied, is the request's already re-admitted pinned
    subset. Before every shared-policy registry read, each exact live binding
    must still resolve inside this root's current selected registry. The
    native kernel then uses only that subset as its caller ceiling; it cannot
    silently inherit unrelated newly selected tools. Ordinary callers omit
    it and retain the existing full-root selection behavior. *)
val run_job_child
  :  ?selected:Chat_response.Tool_capability.t
  -> t
  -> active:Agent_session.Developer_job_scope.active
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Host_invocation.t
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result
