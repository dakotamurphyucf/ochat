open! Core

(** Process-local loans for genuinely admitted legacy timers. There is at most
    one row per retained Schedule. This module has no codec or authority issuer;
    the actor supplies an already checked parent admission and owns every ACK. *)
type t

val empty : t
val is_empty : t -> bool
val retire : t -> unit

(** Stage one independent child from a live admission. The caller must retire
    the result on every pre-ACK failure; durable Schedule data cannot lend it. *)
val stage
  :  manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> schedule:Agent_protocol.Schedule.t
  -> parent:Execution_custody.Admission.t
  -> (t, Agent_protocol.Error.t) result

(** Pure reconciliation rejects duplicate identities rather than replacing an
    existing loan. The actor's actual document admission bounds the inventory. *)
val candidate : t -> staged:t -> (t, Agent_protocol.Error.t) result

(** Retain only exact full-source and unchanged Schedule creation facts while
    Scheduled or Delivering. No Manager/permission check or transfer occurs;
    known expired custody remains negative custody. The owner installs this
    candidate after ACK; missing source returns an empty candidate. *)
val retain_facts
  :  t
  -> source:Agent_protocol.Run_source.t option
  -> schedules:Agent_protocol.Schedule.t list
  -> t

(** Negative reconciliation with the intended final physical Manager. None
    removes all rows; Some keeps only that same resource, without rebinding or
    renewing custody. The owner installs the result at actual replacement. *)
val retain_runtime : t -> manager:Chat_response.Moderator_manager.t option -> t

(** Select only unchanged captured identities/provenance under the same live
    Manager/source, while Scheduled or Delivering. No credentials are restored. *)
val retain
  :  t
  -> manager:Chat_response.Moderator_manager.t option
  -> source:Agent_protocol.Run_source.t option
  -> schedules:Agent_protocol.Schedule.t list
  -> t

(** Nonyielding installation retires removed rows. Actual creation/cancellation
    callers install only after ACK; explicit runtime retirement may clear it. *)
val install : t -> incoming:t -> t

(** At the actual completed delivery writer, stage a child of the exact admitted
    timer before terminal retention retires it. Known closed/expired custody is
    explicit Expired; missing historical/reopened inventory returns None. A
    conflicting existing identity/source is an error, not an unavailable loan.
    The caller owns a successful Current child until queue ACK or retirement. *)
val transfer_delivery
  :  t
  -> manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> schedule:Agent_protocol.Schedule.t
  -> (Execution_custody.Continuation.t option, Agent_protocol.Error.t) result

(** Check only this constructor's actual acknowledged selected IDs. Missing
    rows stay unavailable; conflicting retained resources/sources reject.
    This is binding validation, not a transfer or permission renewal. *)
val validate_constructor_handoff
  :  t
  -> manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> ids:Agent_protocol.Id.Schedule.t list
  -> (unit, Agent_protocol.Error.t) result

(** Remove only the selected exact Manager/full-source rows. The actual cleanup
    owner installs the candidate; no durable timer fact is removed. *)
val without_constructor
  :  t
  -> manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> ids:Agent_protocol.Id.Schedule.t list
  -> t

(** Process-local independent original from the actual new creator. DATA grants
    no admission. Failed/unselected stage retires the child; SAME ACK installs it. *)
module Fresh : sig
  type t

  val stage
    :  manager:Chat_response.Moderator_manager.t
    -> source:Agent_protocol.Run_source.t
    -> schedule:Agent_protocol.Schedule.t
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
    -> (Agent_protocol.Schedule.t * Fresh.t) list

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
      value without inventing a direct creation-to-final transition. [changes]
      is the complete ordered validated selection: existing creator rows advance
      through those same edges without transferring or renewing their admission. *)
  val prepare_selected
    :  inventory
    -> selected:(Agent_protocol.Schedule.t list * Fresh.t) list
    -> changes:Agent_protocol.Schedule.t list
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
