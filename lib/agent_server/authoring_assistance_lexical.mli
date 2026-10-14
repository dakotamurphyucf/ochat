open! Core
module Wire = Agent_protocol.Authoring_assistance.Lexical
module Highlight = Language_highlighting.Highlight_snapshot
module Error = Authoring_context_service_core.Error

(** Project an actual bounded scanner snapshot into one portable lexical result.
    The host supplies an owner derived from the same admitted source/input;
    this adapter checks exact ID, digest, view and length before any wire range
    is built. It polls per emitted span/issue and leaves callback exceptions,
    including cancellation, unchanged. It neither rescans nor fetches bytes. *)
val of_snapshot
  :  request:Wire.Request.t
  -> owner:Wire.Owner.t
  -> Highlight.t
  -> checkpoint:(unit -> unit)
  -> (Wire.Result.t, Error.t) result

(** Exact current ChatML source scan, with no compiler or project prerequisite. *)
val scan_chatml
  :  request:Wire.Request.t
  -> source:Chatml_source.t
  -> limits:Highlight.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Wire.Result.t, Error.t) result

(** Scan an exact captured ChatMD authored/effective view. [input=None]
    constructs only a lexical Root input from current authored bytes and is
    valid for Authored view alone; it never claims parser success. [Some input]
    must match exact captured authored bytes, including imports. Effective
    view requires that genuine admitted input. *)
val scan_chatmd
  :  request:Wire.Request.t
  -> source:Chatml_source.t
  -> input:Chatmd_parser_input.t option
  -> max_parser_input_bytes:int
  -> limits:Highlight.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Wire.Result.t, Error.t) result
