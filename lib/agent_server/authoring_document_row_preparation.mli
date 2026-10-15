open! Core

(** Effectful admission for ONE genuine declaration during an explicit Analyze.
    Rows.collect owns aggregate enumeration and retained-byte admission; the
    service owns publication. This module owns native project capture, existing host or
    independent Build selection, and the exact post-yield rechecks. It never
    caches a row or grants reads from a matching path/digest alone. *)
type t

val create
  :  owner:Authoring_context_service_core.t
  -> env:Eio_unix.Stdenv.base
  -> script_selection:Authoring_script_selection.Policy.t option
  -> project_metadata:Authoring_project_metadata.Policy.t option
  -> project_build_limits:Chatml_project_build.Limits.t
  -> max_document_bundle_files:int
  -> max_document_bundle_bytes:int
  -> max_parser_work_steps:int
  -> t Or_error.t

(** The exact caller-owned capture and Parsed unit must correspond to the
    declaration and declaring source. Rows.collect validates these physical
    associations before admitting the returned row. The returned nonyielding
    check repeats original/current actor, metadata route and raw selection;
    an incoherent selected Build yields No_validated_build without erasing
    separately authorized metadata. Callback exceptions/cancellation propagate. *)
val prepare
  :  t
  -> Authoring_context_service_core.Current.t
  -> actor:Operator_authorization.t
  -> inputs:Chatml_authoring_context.Document.Inputs.t
  -> capture:Authoring_source_host.Document_context.Parsed_capture.t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> Prompt.Chat_markdown.Declaration.t
  -> declaring_source:Chatml_source.t
  -> checkpoint:(unit -> unit)
  -> ( Authoring_document_selection_rows.Row.t
       * (unit -> (unit, Authoring_context_service_core.Error.t) result)
       , Authoring_context_service_core.Error.t )
       result
