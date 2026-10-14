open! Core
module Current = Chatml_authoring_context.Document.Inputs
module Sources = Authoring_source_host.Document_context
module Wire_source = Agent_protocol.Authoring.Source
module Chatmd = Chatmd_authoring_assistance
module Error = Authoring_context_service_core.Error

(** Comparison-only admission from an already retained, genuine explicit
    document capture. The context owner must first authenticate the current
    generation, source revision and actor; this module grants no import read or
    project selection. No operation here parses or reopens a host member. *)
type t

(** Bound one request to actual parser input(s) and their exact captured bytes.
    [capture] must be the generation-bound Core.Current.document_capture for
    [current]; a stale root revision is rejected even if one import happens to
    match the requested source. Every actual input/source association is
    admitted under one count, aggregate byte and work allowance before it can
    supply Parsed facts. Repeated genuine import occurrences may reuse one
    source ID with the same exact source revision; their distinct parser inputs
    remain distinct in [captured_inputs]. This returns all inputs for the
    requested source rather than guessing an occurrence from a path suffix.
    A requested source absent from the capture is unavailable, while a present
    ID with another digest/length is stale. Editor revision is checked by the
    surrounding current Context view; imported sources have no editor
    revision. Callback exceptions, including cancellation, retain identity. *)
val admit
  :  current:Current.t
  -> capture:Sources.Parsed_capture.t
  -> requested:Wire_source.t
  -> max_inputs:int
  -> max_source_bytes:int
  -> max_total_bytes:int
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

val requested_source : t -> Chatml_source.t
val requested_inputs : t -> Chatmd_parser_input.t list

(** One actual input for the requested source only when all repeated genuine
    occurrences have the same bounded parser-input identity. A differing
    occurrence-dependent effective view is unavailable, while exact authored
    bytes remain independently highlightable. *)
val unique_view_input : t -> Chatmd_parser_input.t option

val parsed : t -> Prompt.Chat_markdown.Parsed.t
val captured_inputs : t -> Chatmd.Captured_input.t list
