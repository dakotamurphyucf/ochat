open Core
module Recorder = Chatml_semantic_recorder
module Binding = Chatml_semantic_binding
module Values = Chatml_value_scope
module Types = Chatml_type_alias
module Inferred = Chatml_inferred_type
module Pattern = Chatml.Chatml_binding_pattern

(** Compiler-only borrowed operations over the REAL independently evolving
    lexical environments. No syntax scope reconstruction, resolver callback,
    inference machine or mutable current-context is retained. Raising admission
    operations run beneath [Recorder.with_recording]. *)
val context_exn
  :  Recorder.Frame.t
  -> values:Values.t
  -> types:Types.t
  -> checkpoint:(unit -> unit)
  -> Recorder.Context.t

(** Attach exactly the named leaves selected by the existing shape/projection
    operation, after the existing value restriction/generalization decision.
    One call is ONE actual whole-RHS/parameter quantification group. Authored
    sites alone define source binders; generated helper leaves have no authored
    definition. Preserves the already-installed raw scheme and configured origin. *)
val projected_exn
  :  ?members:Binding.Members.t
  -> Recorder.Frame.t
  -> values:Values.t
  -> types:Types.t
  -> pattern:Pattern.t
  -> projected:(string * Inferred.typ) list
  -> kind:Binding.Kind.t
  -> checkpoint:(unit -> unit)
  -> Values.t

val named_exn
  :  Recorder.Frame.t
  -> values:Values.t
  -> types:Types.t
  -> name:Chatml.Chatml_syntax_name.t
  -> kind:Binding.Kind.t
  -> role:Chatml.Chatml_syntax_sites.Role.t
  -> scheme:Inferred.typ
  -> members:Binding.Members.t option
  -> checkpoint:(unit -> unit)
  -> Values.t

(** Finalize the SAME recursive placeholder identity, after its actual separate
    generalise call. Does not reuse another recursive binding's group. *)
val finish_exn
  :  Recorder.Frame.t
  -> values:Values.t
  -> types:Types.t
  -> binding:Binding.t
  -> scheme:Inferred.typ
  -> checkpoint:(unit -> unit)
  -> Values.t

(** Actual open insertion: reuse the selected public member's identity/group
    without changing the existing raw scheme, weak cell or runtime provenance. *)
val imported_exn
  :  Recorder.Frame.t
  -> values:Values.t
  -> types:Types.t
  -> binding:Binding.t
  -> checkpoint:(unit -> unit)
  -> Values.t

(** Select metadata from the same explicit export row/static namespace already
    collected by the checker; opened names are not implicit exports. *)
val members_exn
  :  Recorder.Frame.t
  -> values:Values.t
  -> types:Types.t
  -> exports:Inferred.typ Inferred.Env.t
  -> checkpoint:(unit -> unit)
  -> Binding.Members.t

val opened_types_exn
  :  Recorder.Frame.t
  -> Types.t
  -> exports:Types.t
  -> checkpoint:(unit -> unit)
  -> Types.t

(** Named value/member/open references are emitted only for an authored parser
    site. Missing sites on generated constructs never fabricate token ownership. *)
val reference_exn
  :  Recorder.Frame.t
  -> values:Values.t
  -> types:Types.t
  -> Chatml.Chatml_syntax_name.t
  -> role:Chatml.Chatml_syntax_sites.Role.t
  -> target:Recorder.Target.t
  -> inferred:Inferred.typ option
  -> checkpoint:(unit -> unit)
  -> unit
