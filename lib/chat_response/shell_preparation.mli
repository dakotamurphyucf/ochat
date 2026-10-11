open Core

(** Authorized native hook compilation and its deferred tool requirements.
    Compilation never initializes hooks. Captured custody, when supplied, is
    authoritative for every effective script and selected matcher entrypoint. *)
type t

val prepare
  :  ?programs:Chatml_prompt_programs.Restored.t
  -> env:Eio_unix.Stdenv.base
  -> admin_policy:Shell_runtime.Admin_policy.t
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> Chatmd_shell_spec.Manifest_compiler.material
  -> (t, Shell_runtime.Registry.error list) Result.t

val registry : t -> Shell_runtime.Registry.prepared

(** Check all captured requirements against the complete staged native/managed
    pool before activation. Legacy single-file hooks have no project requirements.
    This operation neither executes a hook nor grants additional capabilities. *)
val admit_tools
  :  t
  -> capabilities:Tool_capability.t
  -> checkpoint:(unit -> unit)
  -> (unit, Chatml_compilation.error) Result.t
