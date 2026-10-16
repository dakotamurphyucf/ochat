open! Core
module P = Agent_protocol

(** Private per-Live owner of ONE matched Prompt, Workspace and native runtime.
    It is constructed by the daemon host from its real catalogs and shared
    workspace lease, never from independently supplied registry/instance IDs.
    Check/Fixture do not borrow it. It owns no Store writer or scheduler. *)
type t

type live

(** Configuration only. [prompts] and [catalogs] are the daemon's actual
    publication owners; [workspace_leases] is its shared conversation/developer
    conflict-domain ledger. [script_preparation] supplies the same bounded
    retained-program policy used by native preparation. [approval_broker]
    must be an installed daemon broker or absent until an operator
    response route exists; Assume_approved and Session grants are not developer
    authority. No prompt, workspace or native resource is opened here. *)
val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> prompts:Agent_session.Prompt_catalog.t
  -> catalogs:Session_factory.Developer_catalog.t
  -> workspace_leases:Agent_session.Workspace_lease.t
  -> physical_roots:string list
  -> workspace_cleanup:Agent_session.Workspace_cleanup.Limits.t
  -> script_preparation:Agent_session.Runtime_script_preparation.t option
  -> tool_dir:Eio.Fs.dir_ty Eio.Path.t
  -> home:Eio.Fs.dir_ty Eio.Path.t
  -> cache:Chat_response.Cache.t
  -> approval_broker:Shell_runtime.Approval_broker.t option
  -> max_native_bindings:int
  -> child_sources:Session_factory.Child_source_policy.t option
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> remote_content:Developer_child_remote_content.Policy.t option
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> agent_source_limits:Developer_agent_source_runtime.Limits.t
  -> (t, P.Error.t) result

(** After a fresh source capture and durable epoch admission, resolve the
    original's explicit configured Prompt/Workspace IDs. Require current Ready
    Prompt_revision, exact Prompt_definition.allows_workspace, current catalog
    admission and the original actor's developer.live.run scope. The pair and
    actor remain checked after every yielding preparation and at Root exposure;
    neither catalog membership nor the IDs grant method/source permission.

    Open Developer_workspace_lifetime from [handle], then derive shell paths,
    conflict_domain, prompt elements and retained programs from THAT revision
    and workspace. Plan the captured project with that exact immutable
    Prompt_definition.tool_contract and pass the SAME choice through managed
    registry preparation/rebind. Standard Live keeps Tool_v1 and cannot call
    Model.spawn; Developer_model_spawn selects Developer_tool_v1 for that
    separately authorized feature. Prepare Agent_runtime's native/managed definitions once
    under the same joined switch and selected root contract, always through
    its native-only preparation path. A selected provider is not an inference
    Context: inference-dependent native declarations, including Model_reviewer,
    reject before activation. Later managed Model.spawn and Agent.run effects
    use the genuine physical Root/provider/source-parent services. Authorize shell
    manifests only through current exact Operator_manifest_grant; an absent
    interactive broker yields the existing typed permission-required result.
    A developer owner label is not an Id.Session or Session grant.

    The returned native registry, narrowed selection, managed definition,
    source-artifact preparer and model Sources all derive from this SAME live
    package. The managed definition is not a dispatcher: only after the
    physical Root and its Job service exist may the owning native executor
    bind it inside an admitted terminal's lexical scope. Captured Agent.run registrations use the existing source-bound
    authored Agent preparer and the selected native/source-parent path, with
    their authentic retained source and exact binding; a name is not a
    registration and a raw v2 artifact reference is not a grant. The host
    must not replace one component with another package's equal-looking value.
    A single daemon-owned fiber opens this Live package's own Eio.Switch.run.
    Preparation returns only after its result promise resolves. On success,
    [live] retains a stop signal and joined-completion promise; native/shell
    resources stay inside that switch through Root/background work. The whole
    preparation, transfer and joined failure cleanup are serialized under one
    configuration gate. Another preparation cannot drain this owner's
    unresolved Workspace or borrow its successful but untransferred package.
    The daemon switch is checked immediately before forking so an inactive
    switch cannot strand either promise. The
    trusted nonyielding [retain] callback installs the exact successful [live]
    into Host custody under cancellation protection before [prepare] returns;
    a failed callback stops/joins the owner rather than losing it. A cancelled
    waiter similarly stops/joins before reraising the original cancellation.
    On Error or
    exception, preparation stops and joins that SAME switch before attempting
    retained Workspace cleanup. A missing or uncertain cleanup remains in [t]
    and refuses another preparation. Original exception/backtrace survives
    switch-release failure. The caller has already consumed its one durable
    Execution_start before this effectful constructor; partial preparation
    exposes no Root or language callback and must finish that existing command
    as a truthful started failure without replay. *)
val prepare
  :  t
  -> handle:Agent_store.Developer_run_store.handle
  -> original_actor:Operator_authorization.t
  -> prompt:P.Id.Prompt_definition.t
  -> workspace:P.Id.Workspace_definition.t
  -> checkpoint:(unit -> unit)
  -> retain:(live -> unit)
  -> (live, P.Error.t) result

(** The owner has already joined the failed preparation switch before this
    protected exact Workspace cleanup. The daemon completes it before another
    preparation or Store close. A failed lease/directory cleanup remains
    retained for retry; this creates no new resource or effect. *)
val drain_unresolved : t -> (unit, P.Error.t) result

val workspace : live -> Agent_session.Workspace_instance.t
val conflict_domain : live -> string
val native_runtime : live -> Chat_response.Agent_runtime.t
val native_registry : live -> Chat_response.Tool_capability.t
val selected_native : live -> Chat_response.Tool_capability.t
val managed_definition : live -> Chat_response.Managed_tool_registry.t
val model_sources : live -> Developer_model_job_runner.Sources.t

(** Bind only the saved-Agent registrations prepared inside this Live owner to
    its exact admitted Root, native runtime and selected Model package. The
    trusted host supplies one runner constructed from this same Live owner and
    the Root's actual Jobs/Native services after those services exist, before
    any executor is exposed. The callback is a trusted Host composition
    invariant; this module cannot infer its physical provenance from a
    function value. SourceRuntime was prepared in Bound_runner mode, and a
    selected Tool target never falls back to literal-only execution after a
    runner failure. Literal content children retain their existing strict
    path. A missing source policy is harmless only when there
    are no Agent declarations; genuine Agent registrations never become
    name-only grants. No separate source or provider context is supplied by
    the caller. *)
val bind_agent_sources
  :  live
  -> root:Developer_root_owner.run
  -> models:Developer_model_job_runner.t option
  -> run_selected_target:Developer_agent_source_runtime.Selected_target.runner
  -> checkpoint:(unit -> unit)
  -> (unit, P.Error.t) result

module Child_prepared : sig
  type t

  module Admission : sig
    type t

    (** The final, nonyielding check for one attempt-local refreshed Root
        witness. It repeats the exact active child switch, Target/descendant,
        SourceRuntime, selected Prompt/Workspace pair, original actor and Root
        effect-revision proofs. A caller
        must invoke it immediately before that attempt's effect entry. It
        cannot be shared as a refreshed mutable grant across sibling calls.
        The receipt is immutable rather than consumable: repeated checks may
        pass before a revision changes, but each new effect attempt refreshes
        after its own waits and checks immediately before effect entry. *)
    val check_now : t -> (unit, P.Error.t) result
  end

  (** Comparison only: this receipt belongs to the exact selected saved-Agent
      target lent by [with_saved_child]. It grants no source or Root access. *)
  val matches_target
    :  t
    -> Agent_session.Captured_agent_runner.Claimed_developer.Selected.Target.t
    -> bool

  val revision : t -> Agent_session.Prompt_revision.t
  val runtime : t -> Chat_response.Agent_runtime.t
  val selected_native : t -> Chat_response.Tool_capability.t
  val managed_definition : t -> Chat_response.Managed_tool_registry.t

  (** True only when the bounded parsed preflight found image/document source
      content. This structural choice is made before any attempted read. *)
  val needs_dynamic_source : t -> bool

  (** The daemon's explicitly configured bounded remote transport. No policy
      means remote content is denied before network I/O. *)
  val remote_content : t -> Developer_child_remote_content.t option

  val content_limits : t -> Chatmd_prompt_content.Limits.t option

  (** A selected callback-local source-read port, without a retained Scope.
      Every dynamic operation opens and closes its own original-source Scope;
      captured/literal conversion needs no live source. The caller also passes
      [refresh]/[Admission.check_now] as the projector's Root/currentness check
      around each yielding conversion and remote transport. *)
  val content_source
    :  t
    -> ( Agent_session.Captured_agent_runner.Source_binding.Content_source.t
         , P.Error.t )
         result

  val tool_contract : t -> Chatml_compilation.Tool_contract.t

  (** The exact Live-owned response directory already supplied to the saved
      Agent source and Model runner, for Driver.run_entries' explicit output
      routing. The trusted child runner borrows it only while this prepared
      child is active; it is not a source or execution admission. *)
  val response_dir : t -> Eio.Fs.dir_ty Eio.Path.t

  (** Refresh a direct saved-parent effect after any yielding work. The full
      source/Workspace/Root checks may yield; obtain a fresh Root witness and
      repeat them before issuing an attempt-local [Admission.t]. *)
  val refresh : t -> (Admission.t, P.Error.t) result

  (** Refresh inside an actual child selected borrow after any yielding work.
      The lower target and Root checks prove the saved parent ancestry and
      dispatched child terminal; no direct parent fiber binding is assumed. *)
  val refresh_descendant
    :  t
    -> child:Agent_session.Native_tool_invocation.selected_borrowed
    -> (Admission.t, P.Error.t) result
end

(** Lend resources for one exact restored saved-Agent target callback. Check
    Target, Root saved-source, Live actor/Prompt/Workspace and SourceRuntime's
    physical Bound Root/runtime before any effect and after yielding work.
    The bounded pure Developer_child_preflight scans the original parser view
    and callback [input] before constructing child MCP/shell resources; it
    creates no source or implementation authority. The child Shell host uses
    this target revision's elements, nested prompt directory and own retained
    manifest digest with the same original actor and configured pair. Borrowed
    [startup], [compilation_limits], [programs] and selected Tool contract are
    forwarded unchanged.

    [f] runs in a nested owner fiber that inherits the saved target's lexical
    fiber context. The owner preserves the original cancellation/exception and
    backtrace while canceling and joining the child switch's background
    resources. [Child_prepared] expires on every exit even
    if the outer saved source remains active. An installed selected handler
    never falls back to ordinary Driver on failure. Actual child effect
    dispatch still requires its physical native/managed terminal proof. *)
val with_saved_child
  :  live
  -> root:Developer_root_owner.run
  -> target:Agent_session.Captured_agent_runner.Claimed_developer.Selected.Target.t
  -> startup:Chat_response.Startup_compilation.t
  -> compilation_limits:Chatml_compilation.limits
  -> programs:Chatml_prompt_programs.Restored.t option
  -> input:Prompt.Chat_markdown.content_item list
  -> f:(Child_prepared.t -> (string, P.Error.t) result)
  -> (string, P.Error.t) result

(** Comparison/currentness only: exact workspace lease and directory, Prompt
    revision/catalog generation, original actor and native selection are still
    live. Root and each effect repeat their physical/provider checks;
    this projection cannot authorize a new effect by itself. *)
val check_current : live -> (unit, P.Error.t) result

(** After Root workers and command callbacks join, signal this package's
    switch to stop and await its exact joined completion. Eio release hooks run
    after child fibers and in reverse registration order: native/shell hooks
    precede the Workspace hook registered at open_live. A failed hook leaves
    [live] retaining the exact workspace for a bounded cleanup retry; neither
    callback nor native effect is replayed. Once explicit Workspace cleanup
    succeeds, a repeated close reports completion even if the prior joined
    switch reported its original release exception. *)
val close_and_wait : live -> (unit, P.Error.t) result
