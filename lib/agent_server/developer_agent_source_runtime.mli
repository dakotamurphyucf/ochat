open! Core

(** Deferred ordinary Agent.run functions from one authenticated captured Prompt
    revision. The owner constructs these functions before creating the developer
    Root, then binds them once to that exact Root/provider after native selection.
    This module owns no source resolver, provider, child scheduler or Store
    tracker. Child Tool effects require a separate physical child capability
    owner and are rejected before exposure here. A child with ordinary authored
    literal content and no child effects remains executable. *)
type t

module Limits : sig
  type t

  (** Positive finite limits for this one revision's declaration, registration
      and physical comparison work. Lower parser/source and Root limits remain
      independently active; this policy cannot widen them. *)
  val create : max_agents:int -> max_work_steps:int -> t Or_error.t
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
    captured/selected child, never an unselected Fetch fallback. *)
val prepare
  :  source:Agent_session.Captured_agent_runner.Source_binding.t
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

(** One-time physical bind after the native runtime and selected Root exist, but
    before the host exposes the executor. Require [models] issued by [root] and
    the actual [agent_runtime] to retain every registration's same function and
    Source(Some preparer), with matching revision/contract. No name, digest or
    saved artifact is sufficient. A second bind, foreign Root/runtime, or a
    foreign runtime rejects. Saved artifact selection remains valid after the
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
  -> checkpoint:(unit -> unit)
  -> (unit, Agent_protocol.Error.t) result

(** Revoke before the host joins/closes the underlying switch and original
    source/Store/Root owners. Idempotent; never rebinds the package. A borrowed
    callback already running remains joined by its enclosing physical scope. *)
val revoke : t -> unit
