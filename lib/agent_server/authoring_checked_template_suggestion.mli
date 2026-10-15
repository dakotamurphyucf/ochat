open Core
module Owner = Authoring_context_service_core
module Wire = Agent_protocol.Authoring_assistance.Assistance
module Ledger = Chatml_authoring_context

(** Comparison-only projection of one current Core-owned checked witness. The
    caller has already admitted the requested source and genuine selected
    Build/contract under its current owner. No compiler, parser, source read or
    cache mutation occurs. A mismatch/unmappable full replacement yields None;
    it never guesses an authored range from equal text. *)
val project
  :  Owner.Current.t
  -> inputs:Ledger.Inputs.t
  -> receipt:Chatml_authoring_effective.Overlay.t
  -> authored:Chatml_source.t
  -> request:Wire.Request.t
  -> checkpoint:(unit -> unit)
  -> (Wire.Suggestion.t option, Owner.Error.t) result

val document
  :  Owner.Current.t
  -> inputs:Ledger.Document.Inputs.t
  -> script:Chatmd_authoring_analysis.Script.t
  -> declaration:Prompt.Chat_markdown.Declaration.t
  -> authored_input:Chatmd_parser_input.t
  -> authored:Chatml_source.t
  -> request:Wire.Request.t
  -> checkpoint:(unit -> unit)
  -> (Wire.Suggestion.t option, Owner.Error.t) result

(** Reserve the checked example before optional lexical/structural candidates
    under the wire's one aggregate item/text/JSON budget. Preserve their order
    and report truthful truncation when any item cannot fit. *)
val include_checked
  :  Wire.Result.t
  -> Wire.Suggestion.t option
  -> (Wire.Result.t, Owner.Error.t) result
