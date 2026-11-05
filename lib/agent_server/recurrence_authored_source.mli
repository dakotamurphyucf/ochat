open! Core

(** Captured recurrence source DATA derived from an actual selected authored
    specialist. The caller must retain its genuine runtime/resource lease across
    every call and any use of returned private capabilities. This module neither
    acquires resources nor authorizes a native effect, recurrence creation, Run
    admission or execution. Names, selectors and returned data are not permission.
    The existing Authored_resources index remains the sole binding lookup owner;
    this module introduces no registry, issuer, mutable lifetime or token. *)

(** Read only already-realized resources. Refuse an unrealized lazy value rather
    than running setup. The actual selected binding, original actor, lifecycle
    and current resource loan remain the caller's responsibility. *)
val capabilities
  :  Agent_session.Runtime_builder.authored_resources
  -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result

(** Check exact source selector and original artifact publication against this
    prepared specialist. Uses the supplied artifact's revision and ORIGINAL
    creation timestamp; never substitutes an ordinary authored publication.
    This is immutable consistency validation, not a live binding/resource check.
    Callers must separately use Authored_resources.find/resolve under their loan. *)
val check_artifact
  :  Agent_session.Runtime_builder.authored_resources
  -> selector:Agent_store.Captured_source_kind.Authored_tool.t
  -> artifact:Agent_store.Prompt_artifact_store.Artifact.t
  -> (unit, Agent_protocol.Error.t) result

(** Resolve the exact source-bound public wrapper through [resources], retain
    its existing private pins/compiled v1 moderator and capture a deterministic
    target. The real defining parent metadata remains unchanged, including its
    root and ABI; the specialist retains its actual prepared delegated child role.
    No source files, initializer, native setup, Manager or provider is run here.

    The caller supplies the actual current parent state and public registry under
    the same resource loan, checks its original read/native authority and physical
    entry association before/after this operation, and owns any publication or
    later admission. [checkpoint] is the existing interruption/current-authority
    check, not a grant or certificate; its exception/cancellation propagates.
    Source and already-realized private fingerprints are rechecked after it. *)
val capture
  :  Authored_resources.t
  -> state:Agent_session.Session_state.t
  -> public:Chat_response.Tool_capability.t
  -> name:string
  -> mode:Agent_protocol.Run.Mode.t
  -> checkpoint:(unit -> unit)
  -> (Agent_session.Recurrence_target.t, Agent_protocol.Error.t) result
