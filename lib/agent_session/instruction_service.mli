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
