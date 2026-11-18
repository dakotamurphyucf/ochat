(** Actor-owned original creator loans; no codec/recovery or DATA constructor.
    Repeat accepted receipts transfer independent children without renewing the
    original. Historical missing inventory remains unavailable. *)
type t

val empty : t
val is_empty : t -> bool
val retire : t -> unit
val retain_runtime : t -> manager:Chat_response.Moderator_manager.t option -> t

val retain
  :  t
  -> manager:Chat_response.Moderator_manager.t option
  -> source:Agent_protocol.Run_source.t option
  -> registrations:External_ingress.t list
  -> subscriptions:Agent_protocol.Subscription.t list
  -> now:Agent_protocol.Timestamp.t
  -> t

val install : t -> incoming:t -> t

val transfer_delivery
  :  t
  -> manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> registration:External_ingress.t
  -> (Execution_custody.Continuation.t option, Agent_protocol.Error.t) result

(** Process-local independent original from the actual new creator. DATA grants
    no admission. Failed/unselected stage retires the child; SAME ACK installs it. *)
module Fresh : sig
  type t

  val stage
    :  manager:Chat_response.Moderator_manager.t
    -> source:Agent_protocol.Run_source.t
    -> registration:External_ingress.t
    -> parent:Execution_custody.Admission.t
    -> (t, Agent_protocol.Error.t) result

  val discard : t -> unit
end

module Staged : sig
  type t

  val create : unit -> t

  val attach
    :  t
    -> owner:Agent_protocol.Job.launch_owner
    -> receipt:int
    -> fresh:Fresh.t
    -> (unit, Agent_protocol.Error.t) result

  (** Only actual selected DATA-stager receipts may be supplied by the Actor. *)
  val selected
    :  t
    -> owner:Agent_protocol.Job.launch_owner
    -> receipts:int list
    -> (External_ingress.t * Fresh.t) list

  (** Called only after successful actual DATA receipt abort; installed Fresh is inert. *)
  val abort : t -> owner:Agent_protocol.Job.launch_owner -> receipt:int -> unit

  val release_owner : t -> owner:Agent_protocol.Job.launch_owner -> unit
  val abort_all : t -> unit
end

module Update : sig
  type inventory := t
  type t

  (** The Actor supplies each exact creation followed by its actual validated
      selected DATA transitions in execution order; install retains the final
      value without inventing a direct creation-to-final transition. *)
  val prepare_selected
    :  inventory
    -> selected:(External_ingress.t list * Fresh.t) list
    -> (t, Agent_protocol.Error.t) result

  (** Pure physical inventory/fullsource/Manager + strict original check before writer. *)
  val check_before_commit
    :  t
    -> current:inventory
    -> manager:Chat_response.Moderator_manager.t option
    -> source:Agent_protocol.Run_source.t
    -> (unit, Agent_protocol.Error.t) result

  (** Marks exact Fresh installed before any parent release/publication. *)
  val install : t -> inventory

  val discard : t -> unit
end
