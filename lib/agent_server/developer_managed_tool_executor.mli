open! Core

(** One developer managed-tool interpreter, bound to a physically admitted
    Root and its lexical Job service. The owning host supplies [definition]
    from the SAME opaque prepared live-resource package used to construct the
    Root. This private constructor checks Root/Job and exact selected binding
    affinity; it cannot independently infer package provenance from a registry
    fingerprint, since a code rebind may preserve capability identities.
    Neither construction nor binding executes a handler or grants authority. *)
type t

val create
  :  root:Developer_root_owner.run
  -> jobs:Developer_script_job_service.t
  -> definition:Chat_response.Managed_tool_registry.t
  -> env:Eio_unix.Stdenv.base
  -> (t, Agent_protocol.Error.t) result

(** Child-only variant. The opaque selection supplies both current child
    capabilities and the exact managed definition minted by its Live owner;
    callers cannot pair a different definition with the child registry. The
    existing [create] remains the ordinary Root path. *)
val create_child
  :  root:Developer_root_owner.run
  -> jobs:Developer_script_job_service.t
  -> selection:Developer_child_selection.t
  -> env:Eio_unix.Stdenv.base
  -> (t, Agent_protocol.Error.t) result

(** Claim-bound child variant. Its selected registry and managed definition
    both come from the same internally prepared restored issuer receipt, not
    a raw definition paired with a copied request. The actual Worker Parent,
    current source/actor/Root and claimed native terminal are still checked by
    [bind] and the common managed policy at each effect boundary. No expired
    foreground Child_selection or Root definition is substituted on denial. *)
val create_claimed
  :  root:Developer_root_owner.run
  -> jobs:Developer_script_job_service.t
  -> active:Agent_session.Developer_job_scope.active
  -> issuer:Agent_session.Claimed_child_runtime.t
  -> env:Eio_unix.Stdenv.base
  -> (t, Agent_protocol.Error.t) result

val issued_by_root : t -> Developer_root_owner.run -> bool

(** Comparison-only prepared definition retained from the trusted live package.
    The Root/selected callback remains the effect authority. *)
val definition : t -> Chat_response.Managed_tool_registry.t

(** Bind only inside the actual Native_terminal and its SAME live lexical
    Scope. A foreign/expired scope, different Root or terminal rejects before
    returning a dispatch. The native policy kernel still performs its own
    managed admission against this definition's exact prepared implementation,
    selected binding, revision and dispatched Host_invocation at every call.
    The returned dispatch expires with the terminal callback. Its [run] checks
    the same live physical terminal, lexical Scope, current selected borrow,
    dispatched invocation and binding before interpreting; it repeats this
    guard after yielding checks before decoding or sealing a result.

    [call_child] is the owning Developer_native_executor's existing selected
    child path, with the original authorize/output policy retained by its
    closure. It obtains a fresh selected borrow; this adapter never invokes a
    capability implementation directly or creates a second invocation owner. *)
val bind
  :  t
  -> terminal:Developer_root_owner.Native_terminal.t
  -> scope:Developer_script_job_service.Scope.t
  -> call_child:
       (reference:Chat_response.Tool_capability.reference
        -> invocation:Agent_protocol.Host_invocation.t
        -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result)
  -> ( Agent_session.Native_tool_invocation.selected_managed_dispatch
       , Agent_protocol.Error.t )
       result
