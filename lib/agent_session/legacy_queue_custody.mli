open! Core

(** Bounded process-local loans for the actual acknowledged queue. This module
    makes no source, actor admission or permission decision and has no codec. *)
type t

module Row : sig
  type t

  (** Explicit Expired denies; absence of a row is separately unavailable. *)
  val transfer : t -> (Execution_custody.Admission.t, Agent_protocol.Error.t) result

  val check : t -> (unit, Agent_protocol.Error.t) result
end

val empty : t
val is_empty : t -> bool
val retire : t -> unit

(** Retain only exact full-source/Entry facts. This does not check permission or
    require an installed Manager, and preserves known Expired negative custody.
    Missing source/metadata retires no cells here and returns an empty candidate;
    the actual owner must install it at its acknowledged boundary. *)
val retain_facts
  :  t
  -> source:Agent_protocol.Run_source.t option
  -> metadata:Moderator_queue.t option
  -> t

(** Reconcile against the intended final physical resource at replacement.
    None removes all rows; Some retains only that exact Manager. This neither
    rebinds a row nor renews its permission. The owner installs the candidate. *)
val retain_runtime : t -> manager:Chat_response.Moderator_manager.t option -> t

(** Select only exact Manager resource/source/Entry matches, without mutation. *)
val matching
  :  t
  -> manager:Chat_response.Moderator_manager.t option
  -> metadata:Moderator_queue.t option
  -> t

(** Nonyielding post-ACK install. Retire prior rows not physically retained in
    incoming and return incoming. The actor must never invoke it before ACK. *)
val install : t -> incoming:t -> t

val find
  :  t
  -> manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> entry:Moderator_queue.Entry.t
  -> Row.t option

val without : t -> selected:Row.t option -> t

(** Stage independent transfers from the exact current parent. Close partial
    children on failure. The caller owns a successful result until actual ACK
    installation or explicit retirement; decoded facts cannot create a row. *)
val stage
  :  manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> entries:Moderator_queue.Entry.t list
  -> parent:Execution_custody.Admission.t
  -> (t, Agent_protocol.Error.t) result

(** Pure retained/staged reconciliation with the existing 256-row ordinal bound. *)
val candidate
  :  t
  -> manager:Chat_response.Moderator_manager.t
  -> metadata:Moderator_queue.t
  -> staged:t
  -> (t, Agent_protocol.Error.t) result

(** Take ownership of one completed-producer loan for the actual appended Entry.
    Unlike new-work stage, Expired is retained as a negative fact and must deny
    the later claim. The actor supplies the actual producer/source correlation;
    this function neither transfers nor derives permission from those DTOs. *)
val stage_delivery
  :  manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> entry:Moderator_queue.Entry.t
  -> continuation:Execution_custody.Continuation.t
  -> t

(** Validate only the actual constructor's acknowledged selected ordinals.
    Missing rows remain unavailable; contradictory retained rows reject.
    No row is created or rebound and no permission is renewed. *)
val validate_constructor_handoff
  :  t
  -> manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> ordinals:int64 list
  -> (unit, Agent_protocol.Error.t) result

(** Negative exact-owner cleanup. Remove only selected rows with the same
    physical Manager and full source; missing rows are idempotent. The owner
    installs the returned candidate without deleting durable queue facts. *)
val without_constructor
  :  t
  -> manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> ordinals:int64 list
  -> t
