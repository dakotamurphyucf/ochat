open Core

(** Select immutable program custody against an actual runtime declaration and
    authentic host contract. Selection grants no native tool authority. *)
module Declaration : sig
  type t =
    | Extension of Chatmd_shell_spec.Extension_spec.script
    | Shell of Chatmd_shell_spec.Chatmd_script_spec.t
    | Moderator of Prompt.Chat_markdown.script
end

type t

(** Require one active matching binding/contract, exact entry bytes, and the
    original declaration's exact project/environment selection. No file access,
    fallback, compilation or initializer. Checkpoints propagate unchanged. *)
val select
  :  Chatml_prompt_programs.Restored.t
  -> declaration:Declaration.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> checkpoint:(unit -> unit)
  -> (t, Chatml_compilation.error) Result.t

val prepared : t -> Chatml_prepared_program.t
val fingerprint : t -> Chatml_source.Digest.t
val input_bytes : t -> int

(** Admit the selected lock's required tools against the actual execution pool.
    Hosts must call this for each selected handler/role before any initializer;
    source-only selection or compilation is not sufficient. *)
val admit_tools
  :  t
  -> capabilities:Tool_capability.t
  -> checkpoint:(unit -> unit)
  -> (unit, Chatml_compilation.error) Result.t
