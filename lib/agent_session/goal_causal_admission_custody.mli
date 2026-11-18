(** Dune-private representation owned solely by the actual Actor. Mutable
    access requires its issuing Eio domain; physical identities are deliberate
    owner/ACK affinity, never decoded proof. Authentication, Store observation,
    policy and persistence remain actual finite Actor operations. One governed
    occupancy owns this slot and any combined physical slot; no parallel
    registry or independent cleanup driver is introduced here. *)
open! Core

module P = Agent_protocol
module C = Policy_cause_data
module PC = Policy_canonical_usage
module Book = Goal_policy_contributions
module Existing = Goal_descendant_admission_custody
module Basis = Existing.Child_accounting_basis
module Occurrence = Goal_descendant_occurrence

module Preparation : sig
  (* Created only from actual callee inference preparation, after original/source
     selection and actual extant caused-work membership checks. Fields do not
     themselves grant authority. Actual native/Graph/original stay in Actor Plan. *)
  type t =
    { identity : unit ref
    ; basis : Basis.t
    ; cause : C.Key.t
    ; work : C.Allocation_work.t
    ; occurrence : Occurrence.t
    ; mutable set_claim : unit ref option
    }

  val on_domain : t -> bool
  val check_owner : t -> issuer:unit ref -> (unit, P.Error.t) result
end

module Route : sig
  (* Negative matching evidence, copied from authenticated current governance.
     None of these DATA alternatives creates permission. Direct Local is selected
     by actual Caller issuer; forwarding revalidates 605's complete source export;
     Inherited retains exact Scope and actual lower records, never synthesized. *)
  type evidence =
    | Caller_local
    | Inherited_scope of Agent_store.Delegation_goal_origin.Scope.t
    | Forwarded of C.Forwarding.t

  type t =
    { preparation : Preparation.t
    ; owner : P.Session_ref.t
    ; target : C.Goal_target.t
    ; evidence : evidence
    ; controller : Recurrence_controller.t
    ; root_issuer : unit ref
    ; source_owner : Runtime_source_owner.t
    ; domain : int
    ; records : Agent_store.Delegation_store.record list
    }
end

module Terminal_operation : sig
  (* Shared immutable receipt. It never retains a prior settlement or write,
     so newest imported ACKs cannot retain predecessor correction chains. *)
  type t =
    { identity : unit ref
    ; previous : Session_state.t
    ; candidate : Session_transition.t
    ; writer : Session_commit_attempt.t
    }
end

module Remote : sig
  (* One recursion group is necessary: the installed reservation points to SAME
     attempt/write; the already-owned attempt phase then points to reservation.
     Construct attempt first, write and reservation later; prebuild next phase
     before writer. No recursive module, Actor import or initialization cycle. *)
  type attempt =
    { identity : unit ref
    ; set_identity : unit ref
    ; route : Route.t
    ; basis : Basis.t
    ; policy : Goal_root_policy.t
    ; request : Book.Request.t
    ; mutable phase : phase
    }

  and phase =
    | Begun
    | Writer_prepared of write
    | Reserved of reservation
    | Not_reserved of not_reserved

  and write =
    { previous : Session_state.t
    ; candidate : Session_transition.t
    ; kind : write_kind
    ; writer : Session_commit_attempt.t
    }

  and write_kind =
    | Reserve of Book.Pending.t
    | Policy_hold of P.Id.Goal.t

  and not_reserved =
    | Blocked of Goal_admission_policy.Count_availability.t
    | Rejected_before_writer of write * Session_commit_attempt.No_writer.t
    | Policy_hold_acknowledged of write

  and reservation =
    { attempt : attempt
    ; write : write
    ; pending : Book.Pending.t
    ; installed : Session_state.t
    ; mutable terminal : terminal
    }

  and terminal =
    | Settled of settled
    | Terminal_prepared of terminal_write

  and settled =
    | Pending
    | Confirmed of import_ack
    | No_writer_closed of close_ack

  and terminal_write =
    { operation : Terminal_operation.t
    ; reservation : reservation
    ; prior : settled
    ; kind : terminal_kind
    ; mutable acknowledged : terminal_ack option
    }

  and terminal_kind =
    | Import of PC.Projection.t
    | Close of no_writer
    | Correction of PC.Projection.t

  and terminal_ack =
    | Imported of import_ack
    | Close_installed of close_ack

  and import_ack =
    { reservation : reservation
    ; operation : Terminal_operation.t
    ; installed : Session_state.t
    ; projection : PC.Projection.t
    }

  and close_ack =
    { reservation : reservation
    ; previous : Session_state.t
    ; candidate : Session_transition.t
    ; installed : Session_state.t
    ; closed : no_writer
    }

  (* no_writer is an opaque actual callee seal, never serialized. It either seals
     a slot whose canonical candidate was never invoked, or carries the actual
     same-candidate commit kernel's observed no-writer token. *)
  and no_writer =
    { preparation : Preparation.t
    ; slot_identity : unit ref
    ; reason : no_writer_reason
    }

  and no_writer_reason =
    | Never_invoked
    | Observed_before_writer of Session_commit_attempt.No_writer.t
end

module Frozen_routes : sig
  (* Issued by finite callee selection AFTER actual physical receipts/native
     preview are available and BEFORE any causal Root Begin. Exact request
     order/cause/work/occurrence comes from this same PC selection. *)
  type t =
    { selection : PC.Selection.t
    ; requests : Book.Request.t list
    }
end

module Set : sig
  type phase =
    | Unselected
    | Acquiring of Frozen_routes.t
    | Complete of Frozen_routes.t

  type t =
    { identity : unit ref
    ; preparation : Preparation.t
    ; mutable attempts : Remote.attempt list
    ; mutable phase : phase
    }

  val create : Preparation.t -> (t, P.Error.t) result
  val check_claim : t -> (unit, P.Error.t) result

  (** Freeze the actual selection and derive ALL remote Overlay requests from
      SAME Cause/work/occurrence. Inline and covered choices never reserve. *)
  val freeze
    :  t
    -> cause:C.t
    -> selection:PC.Selection.t
    -> (Frozen_routes.t, P.Error.t) result

  val frozen : t -> (Frozen_routes.t, P.Error.t) result
  val next_request : t -> Route.t -> (Book.Request.t, P.Error.t) result

  (** Sole finite Root Begin retains exact next request/attempt before reply. *)
  val retain_next : t -> Remote.attempt -> (unit, P.Error.t) result

  val complete : t -> (unit, P.Error.t) result
end

module Canonical_write : sig
  type t =
    { previous : Session_state.t
    ; candidate : Session_transition.t
    ; canonical : Goal_descendants.Canonical.t
    ; selection : PC.Selection.t
    ; writer : Session_commit_attempt.t
    }
end

module Acknowledgement : sig
  type t =
    { identity : unit ref
    ; preparation : Preparation.t
    ; write : Canonical_write.t
    ; installed : Session_state.t
    ; reservations : Set.t
    }
end

module Slot : sig
  type phase =
    | Preparing
    | Writer_prepared of Canonical_write.t
    | Acknowledged of Acknowledgement.t
    | Sealed of Remote.no_writer
    | Released of release

  and release =
    | Unwritten of Remote.no_writer
    | Confirmed of Acknowledgement.t * Remote.import_ack list

  (** SAME actual Slot election. Followers receive the elected cleanup's exact
      Error or exception/backtrace. Failure restores Idle without dropping the
      retained Plan/unknown writer; no provider execution or new admission. *)
  type cleanup =
    | Idle
    | Running of unit Actor_reply.t Eio.Promise.t

  type t =
    { identity : unit ref
    ; preparation : Preparation.t
    ; reservations : Set.t
    ; mutable phase : phase
    ; mutable cancellation_requested : bool
    ; mutable cleanup : cleanup
    }

  val create : Preparation.t -> (t, P.Error.t) result
  val check_owner : t -> issuer:unit ref -> (unit, P.Error.t) result
  val check_open : t -> issuer:unit ref -> (unit, P.Error.t) result

  (** Monotone no-future-writer seal. Preparing never invoked; Writer_prepared
      needs the SAME kernel's observed no-writer token. Never fresh permission,
      unknown Error, driver join or semantic candidate equality. *)
  val seal : t -> issuer:unit ref -> (Remote.no_writer, P.Error.t) result
end

module Reserve_install : sig
  type t

  (* First validate original root/domain/issuer and Begun. Direct metadata leaf
     must be Book Reserve with SAME retained Request and exact resulting Pending.
     Prebuild reservation/next phase, create Context.Causal_reserve with exact
     attempt.identity/set_identity and original previous/candidate. Retain
     attempt.phase=Writer_prepared write BEFORE returning. No writer invoked. *)
  val prepare
    :  Remote.attempt
    -> issuer:unit ref
    -> previous:Session_state.t
    -> candidate:Session_transition.t
    -> (t, Agent_protocol.Error.t) result

  val write : t -> Remote.write

  val check_basis
    :  t
    -> issuer:unit ref
    -> state:Session_state.t
    -> candidate:Session_transition.t
    -> (unit, Agent_protocol.Error.t) result

  (* Only the private SAME State-ACK suffix, after check_basis and the exact
     kernel's ACK, may call install. Values and the prepare result are allocated
     before ownership publication. The one-shot suffix only assigns prebuilt
     values; a spent/displaced plan does nothing and cannot rewind a phase. *)
  val install : t -> unit

  (* Only after SAME actual failed invocation; exact sealed No_writer affinity.
     Retains Rejected_before_writer(write,token), never Begun or false/unknown. *)
  val reject_before_writer : t -> (unit, Agent_protocol.Error.t) result
end

module Hold_install : sig
  type t

  (* Same original root/domain/issuer/Begun retention law. Candidate's actual
     Goal hold must match route.target Goal; no new Book reserve/admission marks.
     Prebuild write.kind=Policy_hold and its acknowledged phase, retain exact
     Causal_reserve writer before return. If no Hold delta is required, Actor
     records Blocked without invoking a writer; it does not call this helper. *)
  val prepare
    :  Remote.attempt
    -> issuer:unit ref
    -> previous:Session_state.t
    -> candidate:Session_transition.t
    -> (t, Agent_protocol.Error.t) result

  val write : t -> Remote.write

  val check_basis
    :  t
    -> issuer:unit ref
    -> state:Session_state.t
    -> candidate:Session_transition.t
    -> (unit, Agent_protocol.Error.t) result

  val install : t -> unit
  val reject_before_writer : t -> (unit, Agent_protocol.Error.t) result
end

module Canonical_install : sig
  type t =
    { slot : Slot.t
    ; write : Canonical_write.t
    ; acknowledgement : Acknowledgement.t
    ; next : Slot.phase
    }
end

module Terminal_install : sig
  type t

  (* Exact root/domain/issuer and SAME currently installed reservation phase.
     Direct Book Import retains the original stream/Pending prefix law; Close
     carries SAME pending and actual callee no-future-writer seal (Preparation).
     Correction requires SAME already confirmed reservation and stream, with a
     prefix at least its original Pending prefix. The actual Book import must be
     Replaced or a complete-document-changing Duplicate; Added, Stale and exact
     Duplicate cannot prepare a writer. Candidate equals the complete import and
     repeated import is a complete-document fixed point. These factual checks
     never create a new admission or initial per-H confirmation.

     Preserve prior Settled Pending/Confirmed inside terminal write. Prebuild
     the shared immutable operation, import/close ACK, next/restore phases and
     successful result before publishing SAME Terminal_prepared. The receipt
     retains the exact Causal_terminal kernel and original previous/candidate;
     no ACK retains a predecessor write. Never replace a terminal unknown or
     No_writer close. Actual ordered transition validation remains required. *)
  val prepare
    :  Remote.reservation
    -> issuer:unit ref
    -> previous:Session_state.t
    -> candidate:Session_transition.t
    -> kind:Remote.terminal_kind
    -> (t, Agent_protocol.Error.t) result

  val write : t -> Remote.terminal_write

  val check_basis
    :  t
    -> issuer:unit ref
    -> state:Session_state.t
    -> candidate:Session_transition.t
    -> (unit, Agent_protocol.Error.t) result

  (* Same private one-shot State-ACK contract as Reserve_install.install.
     The ACK option and next/restore phases are prebuilt before publication. *)
  val install : t -> unit

  (* SAME original terminal kernel No_writer seal only. Restore prior settled
     factual phase, with old invocation permanently sealed. No State/current
     permission affinity, unknown refund, writer replay or pending removal. *)
  val reject_before_writer : t -> (unit, Agent_protocol.Error.t) result
end

module Root_terminal_write : sig
  (* Stored as SAME reservation.terminal=Terminal_prepared before invocation.
     Unknown cannot be overwritten by another factual request. Prior settled ACK
     remains retained inside the write. Later actual usage corrections use the
     existing factual observation/import path, never a new provider/Handle. *)
  type t = Remote.terminal_write
end

module Confirmation : sig
  type t =
    { canonical : Acknowledgement.t
    ; imports : Remote.import_ack list
    }
end
