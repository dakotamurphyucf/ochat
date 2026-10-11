open Core

(** Bounded lexical absolute POSIX locators. No filesystem lookup or authority.
    Native components remain distinct from portable source-member paths. *)
type t

type failure =
  | Invalid
  | Limit_exceeded
[@@deriving equal, sexp_of]

val normalize
  :  string
  -> base:t option
  -> max_bytes:int
  -> max_components:int
  -> checkpoint:(unit -> unit)
  -> (t, failure) Result.t

val components : t -> string list
val key : t -> string
val byte_length : t -> int
val component_count : t -> int

(** Component-prefix selection; no string-prefix containment or normalization. *)
val suffix : t -> prefix:t -> checkpoint:(unit -> unit) -> string list option
