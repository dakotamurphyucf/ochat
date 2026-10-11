open Core
open Chatml_inferred_type
module Syntax = Chatml.Chatml_type_expr

(** Persistent lexical type namespace. Declarations validate once with abstract
    parameters. Parameterized aliases retain their immutable syntax and defining
    namespace; application elaborates with actual arguments in that namespace,
    never in the caller's. This preserves legacy arrow normalization after
    substitution. Closed zero-parameter aliases reuse their compiled body.
    Source declarations retain only their exact captured defining owner; applying
    private lexical syntax uses that owner, while use-site argument/name/arity
    failures use the explicitly supplied caller source. Host definitions remain
    honestly unlocated. Errors share the canonical bounded diagnostic type.
    No initializer, tool or provider is ever executed. *)
type t

module Error : sig
  type t = Chatml_diagnostic.t
end

module Lookup : sig
  (** Borrowed resolver/observer for one owned syntax traversal. No callback is
      retained in namespaces/aliases. A resolved namespace is frozen separately
      from lexical bindings before private alias expansion. *)
  type namespace =
    { exports : t
    ; origin : Chatml_configured_lookup.Origin.t
    }

  type t =
    { resolve :
        name:string
        -> member_path:string list
        -> span:Source.span
        -> (namespace, Chatml_diagnostic.t) Result.t
    ; observe :
        Chatml_configured_lookup.Origin.t
        -> span:Source.span
        -> (unit, Chatml_diagnostic.t) Result.t
    }
end

val empty : t

(** Start a lexical type scope. Outer names remain visible, but declarations
    shadow outer names and duplicate declarations in this scope still reject. *)
val enter_scope : t -> t

(** Select locally declared type and nested-module bindings. Names introduced
    only by [open] are not implicitly re-exported. Unknown local names reject.
    Alias definitions retain their defining environment privately. *)
val exports
  :  t
  -> type_names:string list
  -> module_names:string list
  -> checkpoint:(unit -> unit)
  -> (t, string) result

(** Install a completed module's static type namespace. Like a non-recursive
    module value binding, this replaces any previous binding of the same name. *)
val add_module
  :  ?semantic_binding:Chatml_semantic_binding.t
  -> t
  -> name:string
  -> exports:t
  -> t

(** Optional immutable compiler metadata. The namespace never retains a Recorder,
    Elaboration frame, callback or inference machine. *)
val semantic_context : t -> Chatml_semantic_recorder.Context.t option

val with_semantic_context : t -> Chatml_semantic_recorder.Context.t -> t

(** Exact locally declared metadata; opened/outer names are not re-exports. *)
val semantic_exports
  :  t
  -> checkpoint:(unit -> unit)
  -> (string * Chatml_semantic_binding.t) list

(** Inspect declarations in this exact scope, without looking through outer or
    opened bindings. Used to check a module's explicit public type members. *)
val declared_arity : t -> name:string -> int option

val declared_module : t -> name:string -> t option

(** Establish universal equality of two locally declared transparent aliases.
    Expands their private lexical definitions and preserves parameter-dependent
    legacy-arrow normalization. Matching normalized symbolic expansions proves
    equality for every argument substitution. Different recursive unfoldings or
    other identities outside that normalization conservatively reject with an
    explicit diagnostic; this operation never samples argument instantiations.
    Neither namespace nor any inference graph is mutated. *)
val check_declared_equivalent
  :  ?source:Chatml_source.t
  -> t
  -> other:t
  -> name:string
  -> span:Source.span
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) result

(** Bring a module's public type and nested-module names into scope. Existing
    visible names cannot be shadowed by [open]. A module with no static type
    namespace (for example a builtin value-only module) contributes no types.
    Value/module existence and value-name collisions are checked by inference. *)
val open_module : t -> name:string -> checkpoint:(unit -> unit) -> (t, string) result

(** Install an already compiled host alias. The body must be closed and contain
    no mutable inference variables. Existing names and primitive names reject.
    The constructor also validates recursive binders and internal type kinds. *)
val add_host
  :  ?semantic_binding:Chatml_semantic_binding.t
  -> t
  -> name:string
  -> body:typ
  -> checkpoint:(unit -> unit)
  -> (t, string) result

(** Declare a transparent alias. Duplicate/free parameters, wrong arity and
    nonregular or unguarded recursion reject before installation.
    All traversals poll [checkpoint];
    its exceptions (including cancellation) propagate unchanged. *)
val declare
  :  ?semantic:Chatml_semantic_elaboration.t
  -> ?source:Chatml_source.t
  -> ?lookup:Lookup.t
  -> t
  -> Syntax.Declaration.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** Elaborate an annotation with no implicitly quantified parameters. *)
val elaborate
  :  ?semantic:Chatml_semantic_elaboration.t
  -> ?source:Chatml_source.t
  -> ?lookup:Lookup.t
  -> t
  -> Syntax.t
  -> checkpoint:(unit -> unit)
  -> (typ, Error.t) result

(** Elaborate a universally quantified value specification. Each parameter in
    the annotation denotes one fresh, shared [Generic] node within that
    specification. [fresh_name] must allocate from the compilation's variable
    namespace, including across value specifications: schemes are instantiated
    by variable name, so merely allocating distinct nodes is insufficient.
    [equality_parameters] names the parameters carrying a value-equality
    requirement. Repeated requirements and names absent from the annotation
    reject. This validates the declared type only, not its implementation's
    ability to satisfy the universal claim. Fully expanded universal function
    specifications require fixed function arity: legacy arrows whose argument
    erasure or result flattening depends on a quantified substitution reject,
    including through private lexical aliases. Explicit [fun(...) -> ...] and
    statically fixed arrows remain valid. This does not change ordinary alias
    declarations, concrete applications or annotation elaboration.
    Ordinary annotations still use
    [elaborate], which rejects unbound parameters. *)
val elaborate_scheme
  :  ?semantic:Chatml_semantic_elaboration.t
  -> ?source:Chatml_source.t
  -> ?lookup:Lookup.t
  -> t
  -> Syntax.t
  -> equality_parameters:string list
  -> fresh_name:(unit -> string)
  -> checkpoint:(unit -> unit)
  -> (typ, Error.t) result

(** Merge completed public types using the same collision policy as open_module.
    Imported names retain immutable provenance; actual use is observed only while
    elaborating caller syntax, never replayed when expanding private alias bodies. *)
val open_exports
  :  t
  -> name:string
  -> exports:t
  -> ?origin:Chatml_configured_lookup.Origin.t
  -> checkpoint:(unit -> unit)
  -> unit
  -> (t, string) Result.t

(** Existing lexical static lookup, including opened modules and outer scopes. *)
val visible_module : t -> name:string -> checkpoint:(unit -> unit) -> t option

(** Select actual public nested module paths from a completed namespace. *)
val nested_module : t -> path:string list -> checkpoint:(unit -> unit) -> t option
