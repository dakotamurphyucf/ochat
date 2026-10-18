open! Core

(** Claim-scoped executable resources prepared from one authentic restored
    issuing revision. This module borrows the exact captured source receipt and
    constructs the selected native/managed registry itself; a caller cannot
    pair a raw registry with unrelated issuer bytes. The module owns no Root,
    scheduler, Store writer or provider inference scope. *)
type t

(** The versioned v3 selected-runtime comparison used both when a child issues
    a request and when a Worker restores it. The base digest comes from the
    exact retained source binding and script/content preparation policy;
    [policy] is the Host's immutable selected moderator policy. This is pure
    comparison data and grants no execution. *)
val runtime_policy_identity
  :  base:Chatml_source.Digest.t
  -> policy:Chat_response.Runtime_semantics.policy
  -> Chatml_source.Digest.t

(** Compile and restore the exact saved Headless program bundle and child
    declarations under the installed tool contract and current manifest
    policy. Use the same bounded native-only preparation as selected children,
    with no current File read or inherited Root registrations. The given host,
    authorizer and approval provider come from the retained Live package;
    their Root/actor/pair affinity is checked by its enclosing Claim service.
    [check_current] is the Live owner's yielding Root/actor/source check;
    [check_local_now] is its nonyielding exact Live switch/pair/actor check
    after the last yield. Neither callback supplies a registry. Before lending
    the result, bind the authenticated Claim request's saved capability pins
    against THIS newly prepared source registry. Missing or changed bindings
    fail without a Root-registry retry. Resource initialization and callback
    children join in this function's nested switch before return.
    Expected errors are typed; cancellation and unexpected exceptions retain
    their original identity and backtrace. *)
val with_prepared
  :  source:Captured_agent_runner.Source_binding.Claimed_source.t
  -> script_preparation:Runtime_script_preparation.t
  -> env:Eio_unix.Stdenv.base
  -> host:Shell_runtime.Host.t
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> checkpoint:(unit -> unit)
  -> check_current:(unit -> (unit, Agent_protocol.Error.t) result)
  -> check_local_now:(unit -> (unit, Agent_protocol.Error.t) result)
  -> (t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

val selected : t -> Chat_response.Tool_capability.t

(** Exact saved-request selection, rebound to this receipt's selected source
    registry inside the Claim. This is comparison-only narrowing, not a second
    authority source. Recheck it after yields before Tool/Script effects;
    extra child bindings never widen a queued request. *)
val pinned_capabilities : t -> Chat_response.Tool_capability.t

val managed_definition : t -> Chat_response.Managed_tool_registry.t
val native_runtime : t -> Chat_response.Agent_runtime.t
val issuer_source : t -> Chat_response.Background_request.Issuer_source.t
val same_parent : t -> Developer_job_scope.Parent.t -> bool

(** First check the exact live Claim/source/switch lifetime without relying on
    an older Root witness; then refresh the yielding Live/Root admission and
    perform the final nonyielding check. An expired loan cannot refresh itself.
    Never falls back to Root-native capabilities. *)
val check : t -> (unit, Agent_protocol.Error.t) result

(** Final nonyielding Claim/source/switch check after the caller's last
    yielding Root and permission check. It is not a replacement for [check]. *)
val check_now : t -> (unit, Agent_protocol.Error.t) result
