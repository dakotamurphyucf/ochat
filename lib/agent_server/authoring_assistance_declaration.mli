open! Core
module Parsed = Prompt.Chat_markdown.Parsed
module Declaration = Prompt.Chat_markdown.Declaration
module Error = Authoring_context_service_core.Error

type located =
  | Exact of Declaration.t option
  | Transformed

(** Locate at most one ACTUAL script declaration at the requested captured
    source's authored cursor, solely for selecting the host's already admitted
    per-script policy. [inputs] are the genuine same-run parser inputs associated
    with that exact requested source; repeated imports remain distinct. Their
    effective bytes must equal the authored capture before effective parser
    ranges can be used as authored coordinates. If conversion changed bytes,
    [Transformed] permits only independently sourced authored lexical help;
    it never equates byte offsets by displayed path. A declaration
    is eligible only if its actual Range.input is one of those [inputs] and
    exactly one real declaration covers the cursor. This is membership lookup,
    never a parser, source-map guess, Build grant or name inference; subsequent
    Script.assess/bound extraction independently proves the selected body and
    exact cursor map. Bound every input-byte comparison and declaration/input
    visit; callback failures preserve identity. *)
val at_authored_cursor
  :  parsed:Parsed.t
  -> inputs:Chatmd_parser_input.t list
  -> authored:Chatml_source.t
  -> cursor_byte:int
  -> max_source_bytes:int
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (located, Error.t) result
