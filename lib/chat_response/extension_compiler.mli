open Core
module Spec = Chatmd_shell_spec.Extension_spec

(** Prepared, non-evaluated implementation with its exact live capability subset.
    This internal compiler does not run initializers or tools, load files, perform
    preprocessing, or reconnect resources. Source capture/parser validation must
    precede it. Public untrusted validation must additionally use the isolated
    compilation budgets; this synchronous API alone does not enforce wall time. *)
type t

(** Validate captured script version, digest, source and execution-limit bounds
    without compiling or evaluating. Shared by tool and lifecycle admission. *)
val validate_script
  :  max_source_bytes:int
  -> Spec.script
  -> (unit, Chatmd_shell_spec.Diagnostic.t list) result

val prepare
  :  ?max_source_bytes:int
  -> scripts:Spec.script list
  -> capabilities:Tool_capability.t
  -> Spec.tool
  -> (t, Chatmd_shell_spec.Diagnostic.t list) result

val declaration : t -> Spec.tool
val program : t -> Chatml_host_runtime.compiled_script
val capabilities : t -> Tool_capability.t
val fingerprint : t -> string
val input_schema : t -> Chatmd_shell_spec.Tool_schema.t
val output_schema : t -> Chatmd_shell_spec.Tool_schema.t
val completion_schema : t -> Chatmd_shell_spec.Tool_schema.t option

(** Uses the trusted compiler worker and enforces compilation resource budgets.
    All captured-source/schema and selected live-capability checks from [prepare]
    still apply. Does not expose a tool or authorize generated native config. *)
val prepare_isolated
  :  ?limits:Chatml_compilation.limits
  -> env:Eio_unix.Stdenv.base
  -> worker:string
  -> scripts:Spec.script list
  -> capabilities:Tool_capability.t
  -> Spec.tool
  -> (t, Chatmd_shell_spec.Diagnostic.t list) result

type definition

(** Validate the complete parsed extension registry and compile every versioned
    script, including lifecycle-only and currently unused scripts, without
    evaluating initializers. Shared source/target pairs compile once. All worker
    calls share one wall deadline. Schemas are checked before launching workers
    and successful schemas are reused without trusting forged retained digests.
    Limits: 128 scripts, 4096 extension tools, 16384 elements, 8MiB distinct
    script/schema source. Legacy execution paths remain with existing hosts.

    [capabilities] contains the actual approved registrations available for
    nested tool selection. This service does not construct new tools, perform
    source loading, apply authoring context, or authorize generated definitions.
    A host must consume the prepared result before exposing extension runners;
    code editing or registry changes require fresh admission. *)
val prepare_definition_isolated
  :  ?limits:Chatml_compilation.limits
  -> env:Eio_unix.Stdenv.base
  -> worker:string
  -> capabilities:Tool_capability.t
  -> Prompt.Chat_markdown.top_level_elements list
  -> (definition, Chatmd_shell_spec.Diagnostic.t list) result

val prepared_tools : definition -> t list

val compiled_scripts
  :  definition
  -> (Spec.script * Chatml_host_runtime.compiled_script) list

val definition_fingerprint : definition -> string
