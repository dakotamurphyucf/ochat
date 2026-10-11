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
val add_module : t -> name:string -> exports:t -> t

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
  :  t
  -> name:string
  -> body:typ
  -> checkpoint:(unit -> unit)
  -> (t, string) result

(** Declare a transparent alias. Duplicate/free parameters, wrong arity and
    nonregular or unguarded recursion reject before installation.
    All traversals poll [checkpoint];
    its exceptions (including cancellation) propagate unchanged. *)
val declare
  :  ?source:Chatml_source.t
  -> t
  -> Syntax.Declaration.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** Elaborate an annotation with no implicitly quantified parameters. *)
val elaborate
  :  ?source:Chatml_source.t
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
  :  ?source:Chatml_source.t
  -> t
  -> Syntax.t
  -> equality_parameters:string list
  -> fresh_name:(unit -> string)
  -> checkpoint:(unit -> unit)
  -> (typ, Error.t) result
