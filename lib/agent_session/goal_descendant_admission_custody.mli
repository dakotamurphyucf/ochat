open! Core

(** Private process-local descendant admission custody. The actual Session_actor
    remains the issuer of captures, routes, reservations and canonical ACKs.
    These internal representations are not public constructors, decoders,
    currentness evidence or an execution interface. All mutable owners stay on
    their issuing Eio domain. The public Actor signature seals the capability
    types; this module is Dune-private.

    Fresh, admitted and factual lifetimes remain distinct. Only the actual
    canonical install owner invokes ACK-plan [install], synchronously immediately
    after SAME candidate State assignment and before publication. No callback,
    I/O or policy check is introduced into those assignments. *)
module Child_accounting_basis : sig
  type t =
    { issuing_actor : unit ref
    ; domain : int
    ; state : Session_state.t
    ; controller : Recurrence_controller.t
    ; source_owner : Runtime_source_owner.t
    }
end

module Child_usage_observation : sig
  (** Immutable historical snapshot DATA with the original negative-only refresh
      guard. Refresh cannot replace the snapshot or issue execution permission. *)
  type t =
    { controller : Recurrence_controller.t
    ; source_owner : Runtime_source_owner.Identity.t
    ; reference : Agent_store.Delegation_store.Reference.t
    ; snapshot : Child_usage_snapshot.t option
    ; refresh_read :
        unit
        -> (unit -> (unit, Agent_protocol.Error.t) result, Agent_protocol.Error.t) result
    }
end

module Descendant_preparation : sig
  module Phase : sig
    type t =
      | Open
      | Acknowledged of unit ref
      | Sealed_before_writer
  end

  (** Only Actor capture supplies the original State/controller/issuer and the
      negative owner/before guards. One preparation owns exactly one Set claim;
      retirement, sealing and factual ACK never reset that claim. *)
  type t =
    { issuing_actor : unit ref
    ; domain : int
    ; source_owner : Runtime_source_owner.Identity.t
    ; before : Session_state.t
    ; session : Agent_protocol.Session_ref.t
    ; controller : Recurrence_controller.t
    ; origin : Agent_store.Delegation_goal_origin.t
    ; occurrence : Goal_descendant_occurrence.t
    ; writer_attempted : bool ref
    ; phase : Phase.t ref
    ; mutable reservation_set_claim : unit ref option
    ; check_owner : unit -> (unit, Agent_protocol.Error.t) result
    ; check_before : unit -> (unit, Agent_protocol.Error.t) result
    }

  val on_domain : t -> bool
  val occurrence : t -> Goal_descendant_occurrence.t
  val scopes : t -> Agent_store.Delegation_goal_origin.Scope.t list
  val delegation : t -> Agent_store.Delegation_store.Reference.t option
  val issued_by : t -> source_owner:Runtime_source_owner.t -> bool
  val same_store : t -> controller:Recurrence_controller.t -> bool
  val check_owner : t -> (unit, Agent_protocol.Error.t) result
  val check_basis : t -> (unit, Agent_protocol.Error.t) result
  val matches_delegation : t -> Agent_store.Delegation_store.Reference.t -> bool
  val acknowledged_by : t -> acknowledgement:unit ref -> bool

  module Acknowledgement_plan : sig
    type preparation = t

    type t =
      { preparation : preparation
      ; acknowledged : Phase.t
      }

    val prepare
      :  preparation
      -> acknowledgement:unit ref
      -> (t, Agent_protocol.Error.t) result

    val install : t -> unit
  end

  module Closed_before_writer : sig
    type preparation = t
    type t = { preparation : preparation }

    val belongs_to : t -> preparation:preparation -> bool
  end

  val seal_before_writer
    :  t
    -> owner:unit ref
    -> (Closed_before_writer.t, Agent_protocol.Error.t) result
end

module Descendant_admission_data : sig
  module Preparation = Descendant_preparation

  val metadata_only_candidate : Session_transition.t -> bool

  module Root_route : sig
    type t =
      { association : Goal_descendant_root_route.t
      ; controller : Recurrence_controller.t
      ; root_issuer : unit ref
      ; source_owner : Runtime_source_owner.t
      ; domain : int
      ; preparation : Preparation.t
      ; records : Agent_store.Delegation_store.record list
      }

    val scope : t -> Agent_store.Delegation_goal_origin.Scope.t
  end

  module Reservation : sig
    type t =
      { attempt_identity : unit ref
      ; route : Root_route.t
      ; preparation : Preparation.t
      ; policy : Goal_root_policy.t
      ; pending : Goal_descendant_pending.t
      ; mutable closed : Descendant_preparation.Closed_before_writer.t option
      }
  end

  module Reservation_attempt : sig
    type t =
      { identity : unit ref
      ; basis : Child_accounting_basis.t
      ; route : Root_route.t
      ; preparation : Preparation.t
      ; policy : Goal_root_policy.t
      ; writer_attempted : bool ref
      ; mutable planned_pending : Goal_descendant_pending.t option
      ; mutable acknowledged : Reservation.t option
      }

    type outcome =
      | Not_attempted
      | Acknowledged of Reservation.t
      | Attempt_unconfirmed

    val scope : t -> Agent_store.Delegation_goal_origin.Scope.t
    val acknowledged : t -> Reservation.t option
    val outcome : t -> outcome
  end

  module Reserve_result : sig
    type t =
      | Reserved of Reservation.t
      | Blocked of Goal_descendants.Availability.t
      | Attempt_unconfirmed of Reservation_attempt.t
  end

  module Reservation_set : sig
    type phase =
      | Acquiring
      | Complete

    type t =
      { identity : unit ref
      ; preparation : Preparation.t
      ; mutable attempts : Reservation_attempt.t list
      ; mutable phase : phase
      }

    val create : Preparation.t -> (t, Agent_protocol.Error.t) result
    val check_claim : t -> (unit, Agent_protocol.Error.t) result
    val attempts : t -> Reservation_attempt.t list
    val preparation : t -> Preparation.t

    val check_next
      :  t
      -> scope:Agent_store.Delegation_goal_origin.Scope.t
      -> (unit, Agent_protocol.Error.t) result

    (** Sole finite Begin retains the SAME attempt before returning or writing. *)
    val retain_next : t -> Reservation_attempt.t -> (unit, Agent_protocol.Error.t) result
  end

  module Root_before_writer : sig
    type t =
      { reservation : Reservation.t
      ; check_current : unit -> (unit, Agent_protocol.Error.t) result
      }
  end

  module Before_writer : sig
    type t =
      { preparation : Preparation.t
      ; reservations : Reservation_set.t
      ; views : Root_before_writer.t list
      }
  end

  module Canonical_edge : sig
    type t =
      | Attempt of
          { before : Inference_ledger.t
          ; after : Inference_ledger.t
          ; canonical : Goal_descendants.Canonical.t
          }
      | Host_turn of Host_turn_tracking.Inherited_edge.t

    val before : t -> Inference_ledger.t
    val after : t -> Inference_ledger.t
    val canonical : t -> Goal_descendants.Canonical.t
  end

  module Acknowledgement : sig
    (** Constructed privately with actual native Handle/Turn_handle and complete
        receipts; SAME preparation phase is installed before public exposure. *)
    type t =
      { preparation : Preparation.t
      ; identity : unit ref
      ; installed : Session_state.t
      ; candidate : Session_transition.t
      ; edge : Canonical_edge.t
      ; receipts : Goal_descendant_included.t list
      ; reservations : Reservation_set.t
      }

    val validate_content : t -> (unit, Agent_protocol.Error.t) result
  end

  module Import_acknowledgement : sig
    (** Positive actual immediate-parent import/re-observation, not duplicate
        disposition or decoded receipt. Factual lifetime has no fresh policy. *)
    type t =
      { issuing_actor : unit ref
      ; controller : Recurrence_controller.t
      ; source_owner : Runtime_source_owner.Identity.t
      ; observation : Child_usage_observation.t
      ; installed : Session_state.t
      ; included : Goal_descendant_included.t list
      }

    val delegation : t -> Agent_store.Delegation_store.Reference.t option
  end

  module Confirmation : sig
    type t =
      { acknowledgement : Acknowledgement.t
      ; imports : Import_acknowledgement.t list
      }
  end

  module Root_admitted : sig
    type t =
      { reservation : Reservation.t
      ; acknowledgement : Acknowledgement.t
      ; confirmation : Confirmation.t
      ; check_current : unit -> (unit, Agent_protocol.Error.t) result
      }
  end

  module Admitted_views : sig
    type t =
      { acknowledgement : Acknowledgement.t
      ; confirmation : Confirmation.t
      ; views : Root_admitted.t list
      }
  end
end

module Descendant_host_data : sig
  type t =
    { prepare :
        Descendant_admission_data.Preparation.t
        -> Descendant_admission_data.Reservation_set.t
        -> (Descendant_admission_data.Before_writer.t, Agent_protocol.Error.t) result
    ; close :
        Descendant_admission_data.Reservation_set.t
        -> Descendant_preparation.Closed_before_writer.t
        -> (unit, Agent_protocol.Error.t) result
    ; confirm :
        Descendant_admission_data.Acknowledgement.t
        -> (Descendant_admission_data.Confirmation.t, Agent_protocol.Error.t) result
    ; refresh_admitted :
        Descendant_admission_data.Acknowledgement.t
        -> Descendant_admission_data.Confirmation.t
        -> (Descendant_admission_data.Admitted_views.t, Agent_protocol.Error.t) result
    }

  module Binding : sig
    type host = t

    type t =
      { issuing_actor : unit ref
      ; domain : int
      ; source_owner : Runtime_source_owner.t
      ; controller : Recurrence_controller.t
      ; check_ancestry :
          (unit
           -> (Delegation_authority.Checked_ancestry.t, Agent_protocol.Error.t) result)
            option
      ; host : host
      ; mutable closed : bool
      }

    val close : t -> unit

    (** Original factual host remains available after fresh source retirement. *)
    val host : t -> host
  end
end

module Descendant_admission_slot : sig
  module R = Descendant_admission_data

  module Phase : sig
    type t =
      | Active
      | Sealed of Descendant_preparation.Closed_before_writer.t
      | Released of Descendant_preparation.Closed_before_writer.t
      | Acknowledged of unit ref
  end

  module Cleanup : sig
    type t =
      | Idle
      | Running of (unit, Agent_protocol.Error.t) result Eio.Promise.t
  end

  type t =
    { preparation : Descendant_preparation.t
    ; reservations : R.Reservation_set.t
    ; binding : Descendant_host_data.Binding.t
    ; mutable phase : Phase.t
    ; mutable cleanup : Cleanup.t
    }

  val preparation : t -> Descendant_preparation.t
  val reservations : t -> R.Reservation_set.t
  val binding : t -> Descendant_host_data.Binding.t
  val host : t -> Descendant_host_data.t

  module Acknowledgement_plan : sig
    type slot = t

    type t =
      { slot : slot
      ; acknowledgement : R.Acknowledgement.t
      ; preparation : Descendant_preparation.Acknowledgement_plan.t
      ; acknowledged : Phase.t
      }
  end
end

module Root_reserve_acknowledgement : sig
  module R = Descendant_admission_data

  type t =
    { attempt : R.Reservation_attempt.t
    ; previous : Session_state.t
    ; candidate : Session_transition.t
    ; positive_ack : R.Reservation.t option
    }

  val prepare
    :  R.Reservation_attempt.t
    -> previous:Session_state.t
    -> candidate:Session_transition.t
    -> (t, Agent_protocol.Error.t) result

  val check_basis
    :  t
    -> state:Session_state.t
    -> candidate:Session_transition.t
    -> (unit, Agent_protocol.Error.t) result

  val install : t -> unit
end

module Root_close_acknowledgement : sig
  module R = Descendant_admission_data
  module Closed = Descendant_preparation.Closed_before_writer

  type t =
    { reservation : R.Reservation.t
    ; previous : Session_state.t
    ; candidate : Session_transition.t
    ; closed : Closed.t option
    }

  val acknowledged
    :  R.Reservation.t
    -> closed:Closed.t
    -> (bool, Agent_protocol.Error.t) result

  val prepare
    :  R.Reservation.t
    -> closed:Closed.t
    -> previous:Session_state.t
    -> candidate:Session_transition.t
    -> (t, Agent_protocol.Error.t) result

  val check_basis
    :  t
    -> state:Session_state.t
    -> candidate:Session_transition.t
    -> (unit, Agent_protocol.Error.t) result

  val install : t -> unit
end

module Descendant_import_acknowledgement : sig
  type t =
    { previous : Session_state.t
    ; candidate : Session_transition.t
    ; acknowledgement : Descendant_admission_data.Import_acknowledgement.t option
    ; installed : Descendant_admission_data.Import_acknowledgement.t option ref
    }

  val check_basis
    :  t
    -> state:Session_state.t
    -> candidate:Session_transition.t
    -> (unit, Agent_protocol.Error.t) result

  val install : t -> unit
end

val descendant_reservations
  :  Descendant_admission_data.Reservation_set.t
  -> (Descendant_admission_data.Reservation.t list, Agent_protocol.Error.t) result

val check_descendant_before_writer
  :  Descendant_admission_data.Before_writer.t
  -> (unit, Agent_protocol.Error.t) result

val prepare_descendant_before_writer
  :  preparation:Descendant_preparation.t
  -> reservations:Descendant_admission_data.Reservation_set.t
  -> views:Descendant_admission_data.Root_before_writer.t list
  -> (Descendant_admission_data.Before_writer.t, Agent_protocol.Error.t) result

val check_descendant_admitted_views
  :  Descendant_admission_data.Admitted_views.t
  -> (unit, Agent_protocol.Error.t) result

val descendant_admitted_views
  :  acknowledgement:Descendant_admission_data.Acknowledgement.t
  -> confirmation:Descendant_admission_data.Confirmation.t
  -> views:Descendant_admission_data.Root_admitted.t list
  -> (Descendant_admission_data.Admitted_views.t, Agent_protocol.Error.t) result

val check_descendant_ack
  :  Descendant_admission_data.Acknowledgement.t
  -> (unit, Agent_protocol.Error.t) result

val descendant_confirmation_import
  :  Descendant_admission_data.Reservation.t
  -> Descendant_admission_data.Acknowledgement.t
  -> imports:Descendant_admission_data.Import_acknowledgement.t list
  -> (Descendant_admission_data.Import_acknowledgement.t, Agent_protocol.Error.t) result

val check_descendant_confirmation
  :  Descendant_admission_data.Reservation.t
  -> Descendant_admission_data.Acknowledgement.t
  -> Descendant_admission_data.Confirmation.t
  -> (unit, Agent_protocol.Error.t) result

(** One matching actual proof per canonical root reservation, physically deduped
    and bounded by frozen scope count. This does not mint an import ACK. *)
val descendant_confirmation
  :  Descendant_admission_data.Acknowledgement.t
  -> imports:Descendant_admission_data.Import_acknowledgement.t list
  -> (Descendant_admission_data.Confirmation.t, Agent_protocol.Error.t) result
