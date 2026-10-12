open Core

(** Active, restored source custody selected against the actual declaration and
    role. Missing, ambiguous, differently selected or conflicting sources reject;
    there is no filesystem lookup or single-file fallback. Exact entry bytes are
    compared in bounded chunks under the supplied checkpoint before admission. *)
type t

val select
  :  Chatml_prompt_programs.Restored.t
  -> script:Chatmd_shell_spec.Extension_spec.script
  -> target:Chatml_compilation.target
  -> capabilities:Tool_capability.t
  -> checkpoint:(unit -> unit)
  -> (t, Chatml_compilation.error) Result.t

(** Complete immutable graph identity, including dependencies and selected host
    contract. Tool authority is checked on every [select], before cache reuse. *)
val fingerprint : t -> Chatml_source.Digest.t

val input_bytes : t -> int

(** Exact admitted immutable input for shared invocation accounting. *)
val prepared : t -> Chatml_prepared_program.t

(** Use the existing standard-target joined compiler and its source/time policy.
    Does not initialize modules or grant any runtime tools. *)
val compile
  :  t
  -> env:Eio_unix.Stdenv.base
  -> limits:Chatml_compilation.limits
  -> max_dependency_depth:int
  -> (Script_program.t, Chatml_compilation.error) Result.t
