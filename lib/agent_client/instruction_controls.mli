open! Core

(** Headless-client bound view. No local selection/capture mirror, source
    IO, automatic activation, request batching or provider state inference.
    Session_handle exposes [instructions : t -> Instruction_controls.t]. *)
type t

(** A thin client composition constructor; the attachment accessor follows the
    actual handle after reattach. IDs remain request DATA, not authority. *)
val create
  :  connection:Connection.t
  -> session_id:Agent_protocol.Id.Session.t
  -> attachment:(unit -> Agent_protocol.Id.Attachment.t)
  -> t

val inspect
  :  t
  -> (Agent_protocol.Instruction_control.View.t, Agent_protocol.Error.t) result

(** Refresh is discovery-only, exactly as the accepted native operation. It
    never replaces selected bytes; replacement requires explicit activation. *)
val refresh
  :  t
  -> selection:Agent_protocol.Instruction_control.Selection.t option
  -> (Agent_protocol.Instruction_control.Discovery.t, Agent_protocol.Error.t) result

val capture
  :  t
  -> Agent_protocol.Instruction_control.Source_request.t
  -> (Agent_protocol.Instruction_control.Capture.t, Agent_protocol.Error.t) result

(** Caller retains the exact key, generation, expected revision and Capture
    across a lost reply/reconnect. No method generates a replacement key on
    retry. Reattachment may replace attachment ID only; the selection command's
    existing semantic digest does not include transport attachment identity. *)
val activate
  :  t
  -> expected_generation:int
  -> expected_revision:int64
  -> key:Agent_protocol.Idempotency_key.t
  -> Agent_protocol.Instruction_control.Capture.t
  -> (Agent_protocol.Instruction_control.Receipt.t, Agent_protocol.Error.t) result

val remove
  :  t
  -> expected_generation:int
  -> expected_revision:int64
  -> key:Agent_protocol.Idempotency_key.t
  -> Agent_protocol.Instruction_control.Request.Remove.change
  -> (Agent_protocol.Instruction_control.Receipt.t, Agent_protocol.Error.t) result
