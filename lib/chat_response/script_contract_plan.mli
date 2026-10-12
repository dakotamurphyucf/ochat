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

  (** Bounded immutable numeric policy projection; contains no callback or authority. *)
  val policy_identity : t -> Sexp.t
end

module Program : sig
  module Role : sig
    module Moderator_invocation : sig
      (** Actual host invocation family, selected explicitly by the planning
          caller. Custom surfaces do not choose it and fingerprints cannot stand
          in for it. Ui and Headless may coexist for the SAME script slot. *)
      type t =
        | Ui
        | Headless
      [@@deriving equal, sexp]
    end

    (** The actual host role selected in the authoritative planner branches.
        Stable across source/contract revisions; names grant no authority.
        Native hook roles include actual kind and selected entrypoint, not merely
        the generic native compiler target. No blanket/invented variants. *)
    type t =
      | Standard of Chatml.Chatml_compiler_contract.Target.t
      | Legacy_moderator of Moderator_invocation.t
      | Native_hook of
          { kind : Chatmd_shell_spec.Chatmd_script_spec.kind
          ; entrypoint : string
          }
    [@@deriving equal, sexp_of]
  end

  type t

  val role : t -> Role.t
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
  -> moderator_invocation:Program.Role.Moderator_invocation.t
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
