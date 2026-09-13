open! Core

(** Bounded, non-executing JSON-schema subset for extension tools. Compilation
    never resolves references, reads files, invokes tools or evaluates ChatML.
    Unknown validation keywords are rejected, rather than silently ignored. *)

type diagnostic =
  { code : string
  ; path : string list
  ; message : string
  }
[@@deriving sexp]

type t

(** Include this identity in schema/source compilation cache keys. *)
val dialect : string

(** Compile an already parsed schema. Boolean schemas and the documented subset
    of object, array, scalar and union constraints are supported. Schema and
    values have a 1 MiB / 128-level / 100,000-node ceiling. Validation additionally
    bounds branch/equality work to 1,000,000 steps. *)
val compile : Jsonaf.t -> (t, diagnostic list) result

(** Enforce byte/nesting bounds before invoking the JSON parser. *)
val of_string : string -> (t, diagnostic list) result

(** Parse JSON data with the same source byte/nesting bounds as schemas.
    Call [validate] afterward for node/value and schema constraints. *)
val parse_json : string -> (Jsonaf.t, diagnostic list) result

val to_json : t -> Jsonaf.t

(** Exact decimal comparisons, Unicode scalar string lengths and structural
    enum/const equality. Errors identify the failing value path. *)
val validate : t -> Jsonaf.t -> (unit, diagnostic list) result
