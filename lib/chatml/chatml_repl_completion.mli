open Core
module Scope = Chatml_typechecker.Value_scope
module Inferred = Chatml_typechecker

type status =
  | Complete
  | Limited
  | Unavailable
[@@deriving equal, sexp_of]

type t

val names : t -> string list
val status : t -> status
val valid_name : string -> bool

(** Compiler-local read of committed schemes only. At most 4096 lexical or
    configured bindings, 4096 frozen nodes/8192 edges, and 8192 traversal steps.
    No inference, instantiation, unification, module check or runtime callback.
    Configured entries with no committed scheme report Unavailable when selected.
    Prefix/path/limit are validated by the owning typing API. *)
val query
  :  Scope.t
  -> configured:(string * Inferred.typ option) list
  -> configured_limited:bool
  -> path:string list
  -> prefix:string
  -> limit:int
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> t
