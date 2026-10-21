open! Core

(** Private, pure admission of one actual ChatMD project declaration. The source
    host owns every native root/read/actor decision and supplies one shared
    budgeted checkpoint; this module cannot acquire a source or grant custody. *)
module Error : sig
  type t =
    | Foreign_input
    | Unavailable_route
    | Limit
  [@@deriving sexp_of]
end

module Admitted_input : sig
  (** Physical membership in one genuine Generated parse and its exact
      parser-input/captured-source pair. It is pure evidence, not a source
      owner or a project selection. *)
  type t

  val input : t -> Chatmd_parser_input.t
  val source : t -> Chatml_source.t
  val declaration : t -> Prompt.Chat_markdown.Declaration.t
end

(** Extract the existing declaration membership, same-run parser input
    association, expected Source.Id in the admitted package namespace and
    exact authored-byte comparison. No project spelling or route is required.
    Every list visit/byte comparison uses the caller's shared checkpoint. *)
val admit_input
  :  parsed:Prompt.Chat_markdown.Generated.receipt
  -> input_sources:(Chatmd_parser_input.t * Chatml_source.t) list
  -> declaration:Prompt.Chat_markdown.Declaration.t
  -> package_namespace:string
  -> checkpoint:(unit -> unit)
  -> (Admitted_input.t, Error.t) result

(** Resolve the project's authored spelling relative to an already admitted
    DECLARING input through the existing Source_loader rule. Check both
    reference and resolved-member component limits. Source-only script
    admission never calls this route; the existing [admit] wrapper does. *)
val route
  :  Admitted_input.t
  -> max_locator_bytes:int
  -> max_path_components:int
  -> checkpoint:(unit -> unit)
  -> (Source.Portable_path.t * Chatmd_shell_spec.Script_project.t, Error.t) result

(** Prove physical membership in the genuine generated parse and its captured
    input/source association, including exact authored bytes. Resolve the
    project's authored spelling relative to that declaring input using the
    existing Source_loader rule. Check the resolved member's component bound,
    not only the original reference spelling. Each traversal and byte compare
    calls the caller's shared budgeted checkpoint. *)
val admit
  :  parsed:Prompt.Chat_markdown.Generated.receipt
  -> input_sources:(Chatmd_parser_input.t * Chatml_source.t) list
  -> declaration:Prompt.Chat_markdown.Declaration.t
  -> package_namespace:string
  -> max_locator_bytes:int
  -> max_path_components:int
  -> checkpoint:(unit -> unit)
  -> ( Chatmd_parser_input.t * Source.Portable_path.t * Chatmd_shell_spec.Script_project.t
       , Error.t )
       result

(** Fixed-size digest from exact captured source and genuine declaration/input
    provenance. Size/visit checks happen before hashing; no authority is
    derived from the digest. *)
val identity
  :  source:Chatml_source.t
  -> input:Chatmd_parser_input.t
  -> declaration:Prompt.Chat_markdown.Declaration.t
  -> selection:Chatmd_shell_spec.Script_project.t
  -> max_identity_bytes:int
  -> checkpoint:(unit -> unit)
  -> (Chatml_source.Digest.t, Error.t) result

(** Pure exact content comparison with the Build's actual root project capture.
    The caller separately proves the host-selected Build belongs to the
    admitted root/member route. This operation neither guesses source IDs nor
    grants a read capability. *)
val matches_project_contents
  :  source:Chatml_source.t
  -> build:Chatml_project_build.t
  -> max_compare_bytes:int
  -> checkpoint:(unit -> unit)
  -> (bool, Error.t) result
