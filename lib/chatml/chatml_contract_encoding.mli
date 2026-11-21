open Core

(** Scoped compiler identity emitter. Both sinks traverse the same typed
    metadata and produce the pinned compiler identity schema. *)
type t

val atom : t -> string -> unit
val list : t -> (unit -> unit) -> unit
val type_scheme : t -> Chatml_builtin_spec.ty -> unit

(** Build a structural view for callers that explicitly need an S-expression.
    Poll each emitted node. *)
val to_sexp : checkpoint:(unit -> unit) -> (t -> unit) -> Sexp.t

(** Emit the exact pinned Sexplib machine bytes directly. Poll each emitted node
    and actual byte work in bounded chunks. Precharge a conservative allocation
    allowance for emitted nodes, text and buffer growth when supplied. Retains
    no callbacks or intermediate S-expression. *)
val to_string
  :  ?allocate:(int -> unit)
  -> checkpoint:(unit -> unit)
  -> (t -> unit)
  -> string
