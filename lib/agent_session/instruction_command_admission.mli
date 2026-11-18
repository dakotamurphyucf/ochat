open! Core

module Source : sig
  type t

  (** Fresh descriptor and genuine capture from the exact still-open Use. *)
  val fresh
    :  Instruction_source_port.Use.t
    -> request:Instruction_source_request.t
    -> captured:Instruction_source_port.Captured.t
    -> (t, Agent_protocol.Error.t) result

  (** For exact retained receipt retries only. This checks current source-root
      grants without reading active files. A fresh mutation cannot use it. *)
  val retry : Instruction_source_port.Use.t -> request:Instruction_source_request.t -> t
end

type decision =
  | Retained of Instruction_command.Receipt.t
  | Prepared of
      { selection : Instruction_selection.t
      ; receipt : Instruction_command.Receipt.t
      }

val prepare
  :  Session_state.t
  -> admission:Instruction_native_admission.t
  -> source:Source.t option
  -> request:Instruction_command.t
  -> now:Agent_protocol.Timestamp.t
  -> (decision, Agent_protocol.Error.t) result

val check_now
  :  Session_state.t
  -> admission:Instruction_native_admission.t
  -> source:Source.t option
  -> request:Instruction_command.t
  -> (unit, Agent_protocol.Error.t) result

(** Shared original-operator/native composition. The actual Actor supplies its
    non-reentrant writer predicate. Native admission has no writer attachment,
    so these entry points preserve the native check order without invoking it.
    Operator writer checks happen after current original authority and before
    receipt disclosure, and again in [check_now_admitted] before persistence. *)
val prepare_admitted
  :  Session_state.t
  -> admission:Instruction_admission.t
  -> authorize_writer:
       (Agent_protocol.Id.Attachment.t -> (unit, Agent_protocol.Error.t) result)
  -> source:Source.t option
  -> request:Instruction_command.t
  -> now:Agent_protocol.Timestamp.t
  -> (decision, Agent_protocol.Error.t) result

val check_now_admitted
  :  Session_state.t
  -> admission:Instruction_admission.t
  -> authorize_writer:
       (Agent_protocol.Id.Attachment.t -> (unit, Agent_protocol.Error.t) result)
  -> source:Source.t option
  -> request:Instruction_command.t
  -> (unit, Agent_protocol.Error.t) result
