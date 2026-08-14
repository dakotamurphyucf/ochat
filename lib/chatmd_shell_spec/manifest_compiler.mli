open! Core

(** Pure namespace, inheritance, validation, and canonical manifest compiler. *)

type legacy_tool =
  { name : string
  ; description : string option
  ; command : string
  ; source : Source_ref.t
  }

type moderator_runtime =
  { runtime : string
  ; source : Source_ref.t
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type input =
  { runtimes : Shell_spec.t list
  ; tools : Shell_tool_spec.t list
  ; scripts : Chatmd_script_spec.t list
  ; legacy_tools : legacy_tool list
  ; moderator_runtime : moderator_runtime option
  ; platform : Shell_spec.platform
  ; supported_features : Feature.Set.t
  }

type material

(** [compile input] expands defaults and inheritance, validates references and
    requested behavior, removes secret values, and hashes canonical JSON.
    Diagnostics are returned in deterministic order. *)
val compile : input -> (Manifest.t, Diagnostic.t list) result

(** [compile_with_material input] additionally returns effective runtime
    declarations containing secret material needed for live instantiation.
    Callers must not serialize, log, or expose the returned declarations. *)
val compile_with_material : input -> (Manifest.t * material, Diagnostic.t list) result

(** [effective_runtimes material ~manifest] returns live declarations only
    when [material] was produced for exactly [manifest]. *)
val effective_runtimes
  :  material
  -> manifest:Manifest.t
  -> (Shell_spec.t list, Diagnostic.t list) result

(** [effective_scripts material ~manifest] returns referenced script source
    only when [material] belongs to [manifest]. *)
val effective_scripts
  :  material
  -> manifest:Manifest.t
  -> (Chatmd_script_spec.t list, Diagnostic.t list) result

(** [warnings material] returns non-fatal diagnostics such as unused script
    declarations. Unused scripts are excluded from executable material. *)
val warnings : material -> Diagnostic.t list
