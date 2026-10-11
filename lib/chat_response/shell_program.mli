open Core

(** Already compiled native hooks plus the captured project requirements that
    still need admission against the actual staged execution pool. *)
type t

(** Compile the default role and selected custom matcher entrypoints against
    their individually authenticated captured contracts. [entrypoints] comes
    from the effective manifest's Script_reference values. Source selection and
    compilation do not initialize a runtime or grant any tools. Every selected
    variant must already be present in custody; no retargeting or file fallback. *)
val compile
  :  Chatml_prompt_programs.Restored.t
  -> script:Chatmd_shell_spec.Chatmd_script_spec.t
  -> entrypoints:string list
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> limits:Chatml_compilation.limits
  -> max_dependency_depth:int
  -> (t, Chatmd_shell_spec.Diagnostic.t list) Result.t

val compiled : t -> Shell_runtime.Chatml_extension.compiled

(** Mandatory before any initializer. Admit every selected role's declared tools
    against the actual execution pool; compilation alone does not suffice. *)
val admit_tools
  :  t
  -> capabilities:Tool_capability.t
  -> checkpoint:(unit -> unit)
  -> (unit, Chatml_compilation.error) Result.t
