open Core
module Inferred = Chatml_inferred_type
module Binding = Chatml_semantic_binding
module Recorder = Chatml_semantic_recorder
module Syntax = Chatml.Chatml_type_expr
module Name = Chatml.Chatml_syntax_name

(** Borrowed metadata for ONE actual type-conversion scope. Not retained by
    aliases/namespaces. No resolver/checker/observer callback or inference machine.
    All methods borrow the current protected compiler checkpoint and raise only
    Recorder's private admission escape under [Recorder.with_recording]. *)
type t

val alias_exn
  :  Recorder.Frame.t
  -> context:Recorder.Context.t
  -> Syntax.Declaration.t
  -> checkpoint:(unit -> unit)
  -> t

val specification_exn
  :  Recorder.Frame.t
  -> context:Recorder.Context.t
  -> Name.t
  -> checkpoint:(unit -> unit)
  -> t

val annotation_exn
  :  Recorder.Frame.t
  -> context:Recorder.Context.t
  -> checkpoint:(unit -> unit)
  -> t

module Parameter : sig
  type definition =
    | Authored of Name.t
    | Implicit of Source.span

  (** Supplied by the actual conversion parameter map. No sampled alias
      argument or inferred value scheme is manufactured by this module. *)
  type t =
    { name : string
    ; inferred : Inferred.typ
    ; definition : definition
    }
end

(** Before body conversion, install exactly its actual parameter bindings into
    a private persistent Type_parameter context. Alias and parameter names may
    have equal spelling. Each parameter root and the final whole type belong to
    this one actual lexical quantification group. An implicit collector placeholder
    keeps its symbol identity; body contexts select its finalized grouped root.
    Earlier immutable collection contexts are not retroactively rewritten. *)
val parameters_exn : t -> Parameter.t list -> checkpoint:(unit -> unit) -> unit

(** Called by the EXISTING specification parameter collector. The first genuine
    occurrence defines an implicit parameter, admitting its metadata before map
    insertion; repeats reuse it. No additional syntax traversal is performed. *)
val implicit_parameter_exn
  :  t
  -> name:string
  -> span:Source.span
  -> checkpoint:(unit -> unit)
  -> unit

(** Genuine successful lookup hooks. Raw inference/alias resolution remains in
    Type_alias. Reference selection is the exact authored token/span. Structural
    fields/tags carry no nominal definition; private alias application never
    borrows a t, and therefore cannot replay source references. *)
val parameter_reference_exn
  :  t
  -> name:string
  -> span:Source.span
  -> checkpoint:(unit -> unit)
  -> unit

(** Equality-constraint parameter lookup after the actual specification collector
    has established the parameter map. The authored constraint token is separate
    from the implicit first-occurrence definition. *)
val constraint_parameter_exn : t -> Name.t -> checkpoint:(unit -> unit) -> unit

val name_reference_exn
  :  t
  -> Name.t
  -> role:Chatml.Chatml_syntax_sites.Role.t
  -> target:Recorder.Target.t
  -> checkpoint:(unit -> unit)
  -> unit

(** Record the ACTUAL whole validated type after recursive_body/arity checks.
    Never stage open Rec_var subtrees as standalone frozen roots. Original alias
    syntax and lexical parameter/body targets remain available for OCH212 scope,
    navigation and honest hover; no invented Mu wrapping or language restriction. *)
val finish_exn
  :  t
  -> Syntax.t
  -> inferred:Inferred.typ
  -> checkpoint:(unit -> unit)
  -> unit

val binding : t -> Binding.t option
val context : t -> Recorder.Context.t

(** After [finish_exn], install the completed alias/specification into its actual
    outer context. Private recursive-self/parameter contexts cannot escape into
    the following declaration's scope. An annotation returns its outer context. *)
val definition_context_exn : t -> checkpoint:(unit -> unit) -> Recorder.Context.t
