open Core

(** Pure composition of a host-admitted authoring generation's exact captured
    project bytes. This module owns no context store, actor, source reader,
    transport, compiler execution or publication decision. The caller must
    check current context identity/generation and authentic contract before
    taking Inputs, and again before publishing a result. *)

module Overlay : sig
  (** Comparison receipt for exactly the Inputs generation that was validated.
      This is not a capability, a currentness claim or a semantic-analysis
      result. A later edit/refresh invalidates it even though the Build and
      source-owned diagnostics remain historical immutable data. *)
  type t

  val identity : t -> Chatml_authoring_context.Identity.t
  val generation : t -> Chatml_authoring_context.Generation.t
  val build : t -> Chatml_project_build.t
end

(** Delegate complete source/old-lock traversal, route/entry derivation and
    closure composition to [Chatml_project_build.Effective.recompose], which
    keeps one invocation-owned budget across every phase. This thin adapter
    only projects the Inputs identity/generation and retains the completed
    Build in an opaque comparison receipt. No editor-supplied route/origin or
    contract is accepted; [contract] must be chosen by the authentic host.
    A changed overlay never reuses the base Prepared as its validated graph.
    Git captures must satisfy the existing Build selection gate, including
    unchanged overlapping bytes. *)
val validate_overlay
  :  Chatml_authoring_context.Inputs.t
  -> lock_id:Chatml_source.Id.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> limits:Chatml_project_build.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Overlay.t, Chatml_project_build.Error.t) result
