open Core
module Recorder = Chatml_semantic_recorder
module Binding = Chatml_semantic_binding
module Values = Chatml_value_scope
module Types = Chatml_type_alias
module Inferred = Chatml_inferred_type

(** Authentic static host introductions from the EXISTING conversion results.
    No surface/runtime implementation, callback, or native capability is retained.
    The common driver verifies the recorded contract fingerprint. Module roots
    and their projected members share ONE actual host module scheme group.
    Enumerate actual closed row layers stack-safely, respecting the checker's
    leftmost override semantics. Admit children/paths before copying, poll actual
    row/duplicate traversal under the recorder work limit. Known host members
    retain genuine contract/member paths; unknown shapes remain unlocated.
    Reuse already staged host bindings only for this invocation's physically
    identical immutable static surface. Converted schemes remain per compiler
    unit; each unit receives fresh source-owned lexical contexts. Built-in
    templates convert without mutable inference cells, so the first staged raw
    host roots cannot change during later inference. *)
val values_exn
  :  Recorder.Frame.t
  -> Values.t
  -> surface:Chatml.Chatml_static_surface.t
  -> globals:(string * Inferred.typ) list
  -> modules:(string * Inferred.typ) list
  -> checkpoint:(unit -> unit)
  -> Values.t

(** Stage a closed host alias from its actual converted template, without adding
    a fabricated source location or performing a second conversion/validation. *)
val alias_exn
  :  Recorder.Frame.t
  -> Types.t
  -> surface:Chatml.Chatml_static_surface.t
  -> ordinal:int
  -> name:string
  -> body:Inferred.typ
  -> checkpoint:(unit -> unit)
  -> Binding.t * Recorder.Context.t
