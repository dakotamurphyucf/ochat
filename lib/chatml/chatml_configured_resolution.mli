open Core
module Lookup = Chatml_configured_lookup
module Origin = Lookup.Origin
module Reference = Lookup.Reference
module Inferred = Chatml_inferred_type
module Alias = Chatml_type_alias
module Scope = Chatml_value_scope

module Public_module : sig
  (** Compiler-owned raw public fields/static types. No inference engine,
      inventory, callback or runtime value. Weak-cell sharing is deliberate. *)
  type t

  val create : values:Inferred.typ Inferred.Env.t -> types:Alias.t -> t
  val value_scheme : t -> Inferred.typ
  val types : t -> Alias.t
  val members : t -> Chatml_semantic_binding.Members.t option
  val with_members : t -> Chatml_semantic_binding.Members.t -> t

  (** OCH-269's one-memo candidate copy replaces only the raw public scheme,
      retaining the exact closed alias templates and immutable member metadata.
      Reject a copied root whose shape is not a module row; never reconstruct
      through [create], which would lose provenance. No weak cell is copied here. *)
  val with_value_scheme : t -> Inferred.typ -> (t, [ `Invalid_module_scheme ]) Result.t
end

module Annotation : sig
  type t =
    | Value_read of Origin.t
    | Inert_open of Origin.t
end

(** One unit invocation's borrowed configured fallback. Holds only its admitted
    lookup context and the compiler callbacks. Must not enter returned aliases,
    annotations/public exports, graph facts or runtime artifacts. *)
type t

val create
  :  Lookup.t
  -> require:
       (source:Lookup.t -> Reference.t -> (Public_module.t, Chatml_diagnostic.t) Result.t)
  -> checkpoint:(unit -> unit)
  -> t

(** Immutable copy for nested compiler checkpoint exception protection. *)
val with_checkpoint : t -> checkpoint:(unit -> unit) -> t

(** Existing lexical Scope first. Return its RAW scheme and optional canonical
    read origin; the checker alone decides instantiation and writes annotations.
    Only imported bindings/genuine misses observe and require configured nodes. *)
val value
  :  t
  -> env:Scope.t
  -> name:string
  -> span:Source.span
  -> (Inferred.typ * Origin.t option, Chatml_diagnostic.t) Result.t

module Selection : sig
  (** Metadata follows the SAME successful lexical/configured selection before
      instantiation. No extra resolution, scanner or inference is performed. *)
  type t =
    { scheme : Inferred.typ
    ; origin : Origin.t option
    ; members : Chatml_semantic_binding.Members.t option
    }
end

val value_with_metadata
  :  t
  -> env:Scope.t
  -> name:string
  -> span:Source.span
  -> (Selection.t, Chatml_diagnostic.t) Result.t

(** Borrowed by existing alias conversion only. Static lexical namespaces win;
    a present lexical value root blocks configured fallback. Direct qualified
    occurrences emit one located full-path event. Alias freezes namespaces,
    never this returned callback record. *)
val type_lookup : t -> env:Scope.t -> Alias.Lookup.t

module Opened : sig
  type t =
    | Local of Inferred.typ
    | Configured of
        { scheme : Inferred.typ
        ; types : Alias.t
        ; reference : Reference.t
        ; members : Chatml_semantic_binding.Members.t option
        }

  (** Source/configuration-owned error using the validated configured open site;
      Local retains an ordinary unowned diagnostic for caller's legacy adapter. *)
  val diagnostic : t -> message:string -> Chatml_diagnostic.t
end

(** Inspect lexical raw binding without emitting a value read. Configured roots
    and imported nested modules emit Open only and return completed public static
    namespace. A value-only first-class module alias has no declared static
    namespace and contributes empty types, as ordinary open_module already does.
    A declared module's expected namespace must be supplied by the existing
    Type_alias.exports collection, which rejects missing declarations.
    Checker retains module-kind/type/value collision/merge decisions,
    raw insertion and per-unit inert-open annotation. *)
val open_module
  :  t
  -> env:Scope.t
  -> name:string
  -> span:Source.span
  -> (Opened.t, Chatml_diagnostic.t) Result.t
