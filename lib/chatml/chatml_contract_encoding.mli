open Core

(** Compiler identity codec only. Build the exact existing Spec sexp schema with
    cooperative polls during real metadata conversion (not a preliminary walk). *)
val type_scheme : Chatml_builtin_spec.ty -> checkpoint:(unit -> unit) -> Sexp.t

(** Exact pinned Sexplib machine encoding. Poll actual list traversal, atom
    classification, escaping and bounded byte copies. Keeps compiler.v1 bytes,
    including adjacent list/quoted-atom spacing. No callback retained. *)
val to_string : Sexp.t -> checkpoint:(unit -> unit) -> string
