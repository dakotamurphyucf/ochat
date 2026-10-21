open! Core

(** Deferred ordinary Agent.run functions from one authenticated captured Prompt
    revision. The owner constructs these functions before creating the developer
    Root, then binds them once to that exact Root/provider after native selection.
    This module owns no source resolver, provider, child scheduler or Store
    tracker. Literal-only selected children remain supported. A selected Tool
    child with effects requires an installed physical child capability owner. *)
type t

module Selected_target : sig
  type mode =
    | Literal_only
    | Bound_runner

  type runner =
    target:Agent_session.Captured_agent_runner.Claimed_developer.Selected.Target.t
    -> startup:Chat_response.Startup_compilation.t
    -> compilation_limits:Chatml_compilation.limits
    -> programs:Chatml_prompt_programs.Restored.t option
    -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
    -> input:Prompt.Chat_markdown.content_item list
    -> observer:Chat_response.Driver.agent_observer option
    -> source:string
    -> (string, Agent_protocol.Error.t) result
end

module Limits : sig
  type t

  (** Positive finite limits for this one revision's declaration, registration
      and physical comparison work. Lower parser/source and Root limits remain
      independently active; this policy cannot widen them. *)
  val create : max_agents:int -> max_work_steps:int -> t Or_error.t

  val max_agents : t -> int
  val max_work_steps : t -> int
end

(** Prepare the genuine retained revision and captured-source port using the
    existing original source lease, immutable programs and selected Tool
    contract. Build exact native Agent functions and Source(Some preparer)
    registrations through Captured_agent_runner.Deferred_developer. Source
    selection and parser membership occur before any registration can be
    returned; bounded child Tool preflight happens before each selected child
    enters Driver. No provider Ctx or Root is constructed;
    calls through the functions reject until [bind] succeeds. The borrowed
    source/store/approval owners must outlive [sw], which joins active callbacks.
    A newly acquired File child is checked under the same source/contract policy
    before its own runtime is constructed. Driver receives only the exact
    captured/selected child, never an unselected Fetch fallback.
    [Literal_only] passes no selected callback and preserves that strict path.
    [Bound_runner] passes one state-reading callback: it denies while Unbound
    or Revoked and never falls back after a selected handler error. The mode is
    fixed before native registrations can be exposed. *)
val prepare
  :  selected_target:Selected_target.mode
  -> source:Agent_session.Captured_agent_runner.Source_binding.t
  -> script_preparation:Agent_session.Runtime_script_preparation.t
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> env:Eio_unix.Stdenv.base
  -> revision:Agent_session.Prompt_revision.t
  -> sw:Eio.Switch.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) result

(** Immutable native registrations from [prepare]. Their exact implementation
    objects and source preparers must be admitted into the same staged runtime
    and selected Root passed to [bind]. This getter grants no dispatch. *)
val registrations : t -> Chat_response.Agent_runtime.native_registration list

(** Prepare ordinary Agent registrations for a physically entered Worker's
    restored issuing revision. Require this runtime still Bound to the exact
    [root] and installed [agent_runtime], in Bound_runner mode, and require
    [source] issued by this runtime's physically retained Source_binding. Use the same
    state-reading Context and selected-target callbacks as foreground Agent
    functions; no independent callback pair or raw binding/revision is
    accepted. Repeat Bound/binding/Claim currentness after yielding preparation. The
    lower preparer derives both from [source] and retains that exact source in
    its sealed result. This does not expose a new execution
    path, Root grant or native registration by itself. *)
val prepare_claimed
  :  t
  -> root:Developer_root_owner.run
  -> agent_runtime:Chat_response.Agent_runtime.t
  -> source:Agent_session.Captured_agent_runner.Source_binding.Claimed_source.t
  -> script_preparation:Agent_session.Runtime_script_preparation.t
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> checkpoint:(unit -> unit)
  -> ( Agent_session.Captured_agent_runner.Deferred_developer.Claimed.t
       , Agent_protocol.Error.t )
       result

(** One-time physical bind after the native runtime and selected Root exist, but
    before the host exposes the executor. Require [models] issued by [root] and
    the actual [agent_runtime] to retain every registration's same function and
    Source(Some preparer), with matching revision/contract. No name, digest or
    saved artifact is sufficient. A second bind, foreign Root/runtime, or a
    foreign runtime rejects. [Literal_only] requires no selected Tool callback;
    [Bound_runner] requires the trusted callback. The separate nested Content
    callback is installed by the same trusted Live owner only after this bind,
    when it can lend the exact selected child parser Port. Saved artifact selection
    remains valid after the
    original source file changes; a new File selection rechecks its original
    source lease at the effect boundary. No provider request is sent here.

    During an actual invocation, the registered function obtains the selected
    native borrow, the exact saved-Agent source/Worker proof, and the
    Root's with_claimed_agent scope before its source runner receives a Ctx.
    The Root rechecks original/current actor, selected provider, source and
    terminal across yields; its existing submission guard covers effects. *)
val bind
  :  t
  -> root:Developer_root_owner.run
  -> models:Developer_model_job_runner.t
  -> agent_runtime:Chat_response.Agent_runtime.t
  -> run_selected_target:Selected_target.runner option
  -> checkpoint:(unit -> unit)
  -> (unit, Agent_protocol.Error.t) result

(** Comparison-only, nonyielding proof that this source runtime remains Bound
    to the exact physical Root and native runtime. This grants no new source,
    child invocation or Store authority. *)
val bound_to
  :  t
  -> root:Developer_root_owner.run
  -> agent_runtime:Chat_response.Agent_runtime.t
  -> (unit, Agent_protocol.Error.t) result

(** Revoke before the host joins/closes the underlying switch and original
    source/Store/Root owners. Idempotent; never rebinds the package. A borrowed
    callback already running remains joined by its enclosing physical scope. *)
val revoke : t -> unit
