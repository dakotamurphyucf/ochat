open! Core

(** Session-owned consumer of genuine native admission and the installation's
    real source facade. Callbacks are internal actor composition, not an
    alternative public authorization service. No capture/selection is cached. *)
type t

val create
  :  state:(unit -> (Session_state.t, Agent_protocol.Error.t) result)
  -> apply:
       (admission:Instruction_native_admission.t
        -> source:Instruction_command_admission.Source.t option
        -> request:Instruction_command.t
        -> (Instruction_command.Receipt.t, Agent_protocol.Error.t) result)
  -> sources:(unit -> Instruction_source_port.t option)
  -> selection_current:(Session_state.t -> bool)
  -> t

val refresh
  :  t
  -> admission:Instruction_native_admission.t
  -> selection:Instruction_source_request.Selection.t option
  -> (Jsonaf.t, Agent_protocol.Error.t) result

val read
  :  t
  -> admission:Instruction_native_admission.t
  -> request:Instruction_source_request.t
  -> (Jsonaf.t, Agent_protocol.Error.t) result

val activate
  :  t
  -> admission:Instruction_native_admission.t
  -> request:Instruction_source_request.t
  -> capture_id:string
  -> expected_revision:int64
  -> key:Agent_protocol.Idempotency_key.t
  -> (Instruction_command.Receipt.t, Agent_protocol.Error.t) result

val deactivate
  :  t
  -> admission:Instruction_native_admission.t
  -> change:Instruction_command.Change.t
  -> expected_revision:int64
  -> key:Agent_protocol.Idempotency_key.t
  -> (Instruction_command.Receipt.t, Agent_protocol.Error.t) result

val status
  :  t
  -> admission:Instruction_native_admission.t
  -> (Agent_protocol.Instruction_status.t, Agent_protocol.Error.t) result

val remove_skills
  :  t
  -> admission:Instruction_native_admission.t
  -> members:Instruction_source_request.Member.t list
  -> expected_revision:int64
  -> key:Agent_protocol.Idempotency_key.t
  -> (Instruction_command.Receipt.t, Agent_protocol.Error.t) result

(** Agent conversation ONLY. [borrow_actor] is the exact actual operation,
    child/native, claimed background job or initialization owner, never a
    started-runtime fallback. Its guarded proof retains that physical epoch.
    The source Use remains open across the complete callback/provider call. *)
val agent_guidance
  :  t
  -> borrow_actor:
       (unit -> (Operator_authorization.t option, Agent_protocol.Error.t) result)
  -> Chat_response.Agent_guidance.t

(** Internal immutable selected-state projection. It has no wire codec and
    grants neither source nor provider authority. Host wire adapters must expose
    only the safe selected identities, never these retained contents. *)
module Inspection : sig
  type t = private
    { session_id : Agent_protocol.Id.Session.t
    ; generation : int
    ; summary : Agent_protocol.Instruction_status.t
    ; project : Instruction_capture.t option
    ; skills : Instruction_content.Loaded.t list
    }
end

module Discovery : sig
  type t = private
    { roots : Instruction_content.Root_id.t list
    ; discovery : Instruction_source_request.Discovery.t option
    }
end

(** Shared composition over the exact same service and transaction owner.
    Original-operator calls use genuine original proof admission; native wrappers
    above keep their existing external signatures and native tokens. *)
module Shared : sig
  type nonrec t = t

  val create
    :  state:(unit -> (Session_state.t, Agent_protocol.Error.t) result)
    -> apply:
         (admission:Instruction_admission.t
          -> source:Instruction_command_admission.Source.t option
          -> request:Instruction_command.t
          -> (Instruction_command.Receipt.t, Agent_protocol.Error.t) result)
    -> sources:(unit -> Instruction_source_port.t option)
    -> selection_current:(Session_state.t -> bool)
    -> t

  (** Current selected-root grants precede disclosure, including an empty
      selected project chain. Missing/revoked access denies the whole view.
      No current source file is reread. *)
  val inspect
    :  t
    -> admission:Instruction_admission.t
    -> (Inspection.t, Agent_protocol.Error.t) result

  (** Discovery refresh never changes selected bytes. *)
  val discover
    :  t
    -> admission:Instruction_admission.t
    -> selection:Instruction_source_request.Selection.t option
    -> (Discovery.t, Agent_protocol.Error.t) result

  (** Pure immutable capture DATA may escape the Use; the genuine Captured
      authority cannot. Fresh activation must still capture under its own Use. *)
  val capture
    :  t
    -> admission:Instruction_admission.t
    -> request:Instruction_source_request.t
    -> (Instruction_capture.t, Agent_protocol.Error.t) result

  val activate
    :  t
    -> admission:Instruction_admission.t
    -> request:Instruction_source_request.t
    -> capture_id:string
    -> expected_revision:int64
    -> key:Agent_protocol.Idempotency_key.t
    -> (Instruction_command.Receipt.t, Agent_protocol.Error.t) result

  val deactivate
    :  t
    -> admission:Instruction_admission.t
    -> change:Instruction_command.Change.t
    -> expected_revision:int64
    -> key:Agent_protocol.Idempotency_key.t
    -> (Instruction_command.Receipt.t, Agent_protocol.Error.t) result

  val remove_skills
    :  t
    -> admission:Instruction_admission.t
    -> members:Instruction_source_request.Member.t list
    -> expected_revision:int64
    -> key:Agent_protocol.Idempotency_key.t
    -> (Instruction_command.Receipt.t, Agent_protocol.Error.t) result
end
