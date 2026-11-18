open! Core

(** Pure request text from exact retained active bytes. Does not authorize use,
    read files or create canonical history. Deterministic project root-to-leaf,
    followed by explicit selected skill order; metadata has no authority. *)
type t = private
  { guidance : Inference.Request.Guidance.t option
  ; roots : Instruction_content.Root_id.t list
  ; reserved_input_tokens : int
  }

val create
  :  Instruction_selection.t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> (t, Agent_protocol.Error.t) result
