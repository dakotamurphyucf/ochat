open! Core

(** Private finite classification of joined analysis admission errors. Invalid
    programs are returned as Failed and never enter this path. No compiler
    message, source path or native diagnostic is serialized by this mapping. *)
val compilation
  :  Chatml_compilation.Analysis_error.t
  -> Authoring_context_service_core.Error.t

val project : Chatml_authoring_analysis.Error.t -> Authoring_context_service_core.Error.t
val wire : Authoring_analysis_wire.Error.t -> Authoring_context_service_core.Error.t

(** A bounded pure selected-entry recomposition can exhaust finite work/bytes;
    other mismatches mean the original host-selected Build cannot represent the
    current parsed body. Neither error publishes a certified result. *)
val selected_entry_overlay
  :  Authoring_selected_script_overlay.Error.t
  -> Authoring_context_service_core.Error.t
