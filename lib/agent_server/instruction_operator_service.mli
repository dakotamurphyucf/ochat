open! Core

(** Private server adapter. Per-request composition over the actual entry
    Actor and installed source facade. No mutable selection/capture mirror,
    runtime activation, provider call or additional source authority owner. *)
type t

val create : registry:Session_registry.t -> authoring:Authoring_installation.t option -> t

(** The dispatcher passes Connection_context.request_actor's actual proof,
    never a reconstructed principal. Reads recheck current proof, actual session
    visibility and current selected-root permissions before projection. Writes
    also require actual connection-owned writable attachment, bound generation
    and the shared Actor instruction admission. No generic response cache.

    All results describe stored selection or one fresh source observation.
    They never assert that a provider has actually consumed those bytes. *)
val execute
  :  t
  -> actor:Operator_authorization.t
  -> context:Connection_context.t
  -> Agent_protocol.Instruction_control.Request.t
  -> (Agent_protocol.Instruction_control.Result.t, Agent_protocol.Error.t) result
