open Core

(** Immutable, non-evaluated one-off program and its exact selected live native
    capability registry. A successful preparation grants no execution authority. *)
type t

val entrypoint : string

(** Compile submitted bytes as [main : json -> json task] using the dedicated
    one-off surface and Eio compiler domain. Select only named bindings from the
    caller's effective registry; duplicate/missing names fail. No source loading,
    initializer evaluation, tool call, history entry or session creation occurs.

    The host supplies the caller's actual effective capability ceiling and compiler
    policy. Model-visible registration must also install authoring guidance and
    readonly validation. The returned artifact is not itself a runnable tool. *)
val prepare_in_domain
  :  ?limits:Chatml_compilation.limits
  -> env:Eio_unix.Stdenv.base
  -> capabilities:Tool_capability.t
  -> tools:string list
  -> source:string
  -> unit
  -> (t, Chatmd_shell_spec.Diagnostic.t list) result

val source : t -> string
val source_ref : t -> Chatmd_shell_spec.Source_ref.t
val program : t -> Chatml_host_runtime.compiled_script
val capabilities : t -> Tool_capability.t

(** Includes exact source bytes, one-off surface/entrypoint version and selected
    live bindings (including schemas and implementation identities). No mutable
    evaluated environment is retained or shared. *)
val fingerprint : t -> string

(** Verify the current registry still contains the exact selected bindings.
    Additional current tools cannot widen this artifact. Call again after any
    authorizing wait, and retain normal per-call policy/disclosure checks. *)
val revalidate
  :  t
  -> capabilities:Tool_capability.t
  -> (unit, Tool_capability.error) result
