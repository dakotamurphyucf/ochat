(** Compiler-owned signature inventory for authoring references. This extracts
    metadata only: builtin implementations are never invoked. An inventory
    describes compile-time availability, not host installation or permission to
    execute an operation. Semantic prose and examples require separate coverage. *)

type kind =
  | Global
  | Module_export
  | Type_alias
  | Entrypoint
[@@deriving equal, sexp_of]

type item = private
  { kind : kind
  ; name : string
  ; scheme : Chatml_builtin_spec.ty
  }
[@@deriving sexp_of]

type t = private
  { surface_id : string
  ; modules : string list
  ; items : item list
  }
[@@deriving sexp_of]

(** Validate namespaces and take a deterministic metadata snapshot. Type schemes
    retain explicit function argument lists, row tails and recursive binders.
    Entrypoints are required script definitions, not installed builtin values. *)
val of_surface
  :  surface_id:string
  -> entrypoints:(string * Chatml_builtin_spec.ty) list
  -> Chatml_builtin_surface.surface
  -> (t, string) result

(** All named compiler surfaces, including legacy/UI/shell and the four
    extensibility surfaces. These are distinct contracts even when many bindings
    coincide. A retrieval host must select its actual target, never grant their
    union. No declaration here enables an experimental host feature. *)
val standard : unit -> (t list, string) result

(** Versioned structural data for generators/corpus identities. [scheme] uses
    the builtin type algebra's canonical S-expression encoding, not executable
    ChatML type syntax. In particular TTuple describes internal payload structure
    and is not a claim that arbitrary tuple expressions are supported. *)
val to_json : t -> Jsonaf.t
