open Core

(** Immutable canonical runtime read descriptor. The source ID is the existing
    implementation identity, not a second module-ID representation. Core depends
    only on Chatml_source, never on the checker, graph driver or Eio adapter. *)
type t [@@deriving compare, equal, sexp_of]

(** Validate member path identifiers using the language's existing identifier
    rules. Empty path selects the whole public module. No source lookup or
    exported-member checking here; the actual checker establishes membership. *)
val create : Chatml_source.Id.t -> member_path:string list -> t Or_error.t

val implementation : t -> Chatml_source.Id.t
val member_path : t -> string list

module Binding_target : sig
  (** Shared frozen host-binding target for the compiler and linked artifact.
      A local name is looked up in the entry's private runtime environment;
      a configured member uses the canonical registry. Neither carries a value. *)
  type reference = t

  type t =
    | Local_binding of string
    | Configured_member of reference
end

(** If resolved AST sexp readers are retained, decode through Id.create/create.
    This admits a descriptor's shape only, never a compiled artifact, inventory
    membership, authority or trusted inference result. No unchecked deriving. *)
val t_of_sexp : Sexp.t -> t

(** Chatml_lang.resolved_expr addition:
      | REConfigured_read of Chatml_module_reference.t
    Existing constructors and source byte spans remain unchanged. The resolver
    emits this only from successful owned Value_read annotations. The runtime
    must read an explicit execution-owned canonical registry, never current
    configured names, globals with synthetic names, callbacks or source I/O. *)
