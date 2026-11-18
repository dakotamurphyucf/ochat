open! Core
module Effect = Instruction_native_admission.Effect

type t =
  | Refresh of Instruction_source_request.Selection.t option
  | Read of Instruction_source_request.t
  | Activate of
      { request : Instruction_source_request.t
      ; capture_id : string
      ; expected_revision : int64
      ; key : Agent_protocol.Idempotency_key.t
      }
  | Deactivate of
      { all : bool
      ; members : Instruction_source_request.Member.t list option
      ; expected_revision : int64
      ; key : Agent_protocol.Idempotency_key.t
      }
  | Status

(** Finite complete JSON admission precedes field lookup/list allocation. Unknown
    fields reject; source references never carry bytes, role or permission. *)
val decode : effect_kind:Effect.t -> Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Bounded JSON parsing for the existing [Ochat_function.input_of_string]
    raising adapter. Raises on malformed or over-limit input; the native tool
    wrapper owns conversion to the standard rejected-call outcome. *)
val decode_text_exn : string -> Jsonaf.t

val parameters : Effect.t -> Jsonaf.t
