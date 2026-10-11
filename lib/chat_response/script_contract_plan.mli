open Core

(** Pure authentic role planning over one actual parser context. No inference,
    compilation, source acquisition, security policy, tool grant or initializer. *)
module Limits : sig
  type t

  (** Positive program/byte/work ceilings. Contract bytes are canonical metadata,
      not heap accounting. Work counts calls to the existing helpers' cooperative
      checkpoints, including source-affinity validation and bounded hashing. *)
  val create
    :  max_programs:int
    -> max_metadata_bytes:int
    -> max_work_steps:int
    -> t Or_error.t
end

module Program : sig
  type t

  val declaration : t -> Prompt.Chat_markdown.Declaration.t
  val binding : t -> Chatml_prompt_programs.Binding.t
  val contract : t -> Chatml.Chatml_compiler_contract.t
  val compiler_contract : t -> Chatml_source.Digest.t
end

type t

(** Legacy scripts use the supplied actual UI/headless surface. Versioned tools
    and moderators use existing target helpers and the actual delegated choice.
    Native hooks use only the supplied material's effective scripts and actual
    default/custom matcher entrypoints. Authenticate every descriptor against an
    actual Parsed declaration; duplicate/surplus/foreign ownership rejects.
    Material is borrowed only during planning and never retained. Metadata/work
    limits precede retained rows and identity construction; original checkpoint
    exceptions propagate without conversion. No blanket role variants. *)
val create
  :  Prompt.Chat_markdown.Parsed.t
  -> moderator_surface:Chatml.Chatml_builtin_surface.surface
  -> delegated_moderator:bool
  -> shell:
       (Chatmd_shell_spec.Manifest.t * Chatmd_shell_spec.Manifest_compiler.material)
         option
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Chatmd_shell_spec.Diagnostic.t list) Result.t

val parsed : t -> Prompt.Chat_markdown.Parsed.t
val programs : t -> Program.t list
val has_projects : t -> bool

val contracts
  :  t
  -> (Chatml_prompt_programs.Binding.t * Chatml.Chatml_compiler_contract.t) list
