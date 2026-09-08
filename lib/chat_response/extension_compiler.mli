open Core
module Spec = Chatmd_shell_spec.Extension_spec

(** Prepared, non-evaluated implementation with its exact live capability subset.
    This internal compiler does not run initializers or tools, load files, perform
    preprocessing, or reconnect resources. Source capture/parser validation must
    precede it. Public untrusted validation must additionally use the isolated
    compilation budgets; this synchronous API alone does not enforce wall time. *)
type t

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
