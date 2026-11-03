open! Core

type reference =
  { capture : string
  ; file : string
  }

type t =
  { revision : int64
  ; project : string option
  ; skills : reference list
  ; captures : Instruction_capture.t list
  ; pool : Agent_store.Prompt_program_bundle.Payload.t String.Map.t
  ; receipts : Instruction_command.Receipt.t list
  }

val empty : t
val active : t -> bool
val project : t -> Instruction_content.Loaded.t list
val skills : t -> Instruction_content.Loaded.t list
val validate : t -> (unit, Agent_protocol.Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
