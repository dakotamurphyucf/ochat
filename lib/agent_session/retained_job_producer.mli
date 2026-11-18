(** One exact already-enqueued terminal occurrence from the existing validated
    Run index. DATA only: this is neither a live Job, a new attempt nor permission.
    Its exact original attempt does not follow a later retry. *)
type t

(** Missing original evidence is None, distinct from malformed/mismatched
    retained evidence. This never means an unrelated policy root. *)
val find
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> runs:Run_state.t option
  -> source:Agent_protocol.Run_source.t
  -> origin:Agent_protocol.Inference_origin.t option
  -> id:Agent_protocol.Id.Job.t
  -> job_generation:int
  -> attempt:int
  -> (t option, Agent_protocol.Error.t) result

val capture
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> runs:Run_state.t option
  -> source:Agent_protocol.Run_source.t
  -> origin:Agent_protocol.Inference_origin.t option
  -> id:Agent_protocol.Id.Job.t
  -> job_generation:int
  -> attempt:int
  -> (t, Agent_protocol.Error.t) result

val delivery : t -> Run_job_delivery.t
val frame : t -> Chat_response.Background_delivery.t

(** Compare the exact canonical queue payload hash to the retained frame. Shared
    by actual head validation and consumed-receipt interpretation. *)
val check_payload : t -> sha256:string -> (unit, Agent_protocol.Error.t) result

(** Validate the actual captured snapshot; no kind is inferred from its tag.
    The validated terminal index and canonical typed membership remain required. *)
val check_receipt
  :  t
  -> Agent_protocol.Moderator_execution.t
  -> (unit, Agent_protocol.Error.t) result

(** Reject a contradictory generic frame/producer tuple even when its original
    terminal witness is genuinely unavailable. Non-generic Model payloads supply
    no Async proof and are not interpreted as that family. *)
val check_receipt_subject
  :  session_id:Agent_protocol.Id.Session.t
  -> id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> source:Agent_protocol.Run_source.t
  -> Agent_protocol.Moderator_execution.t
  -> (unit, Agent_protocol.Error.t) result
