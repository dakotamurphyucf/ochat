open Core
module B = Content_budget
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module Record = Content_record
module R = Content_rendered
module Storage = Agent_store.Prompt_program_bundle

(** Validated complete Root/occurrence inventory and exact rendered payload pool. *)
type t

val inputs : t -> Input.t list
val roots : t -> Input.t list
val records : t -> Record.t list
val payloads : t -> Storage.Payload.t list
val accounting : t -> B.Accounting.t
val input_table : Input.t list -> budget:B.t -> (Input.t String.Map.t, B.Error.t) Result.t

val create
  :  inputs:Input.t list
  -> roots:Input.t list
  -> records:Record.t list
  -> budget:B.t
  -> (t, B.Error.t) Result.t

val capture
  :  parsed_contexts:Parsed.t list
  -> rendered:R.t list
  -> budget:B.t
  -> (t, B.Error.t) Result.t

val materialize
  :  t
  -> parsed:Parsed.t
  -> budget:B.t
  -> (Prompt.Chat_markdown.Document_projection.t, B.Error.t) Result.t

val preserves : t -> previous:t -> budget:B.t -> (unit, B.Error.t) Result.t
