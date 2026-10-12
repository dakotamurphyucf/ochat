open Core
open Git_catalog_model

(** Mutable per-Reader semantic admission, private to the repository bracket.
    Each checkpoint admits one work unit before work, polls the SAME host bracket,
    then calls the original user callback. One unit admits a cooperative poll boundary before the next bounded chunk
    (<=4096-byte hash/copy/scan), element, comparison or stage. Shared document
    parsing is a byte/depth/node-bounded stage with pre/post polling, not internally
    preemptible. Units are not bytes, CPU cycles or a wall-clock guarantee.
    Nested shared APIs' actual checkpoints consume the same ledger too. *)
type t

(** Fresh generative expected-failure escape bridges unit checkpoints only.
    Catch only that tag; user exception/cancellation identity and backtrace survive.
    No budget value may be returned or retained outside [f]. *)
val with_budget
  :  Chatml_git_host.Repository.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> f:(t -> ('a, Error.t) Result.t)
  -> ('a * Accounting.t, Error.t) Result.t

val checkpoint : t -> unit
val equal_string : t -> string -> string -> bool
val context : t -> Project.Declared_at.t option -> unit
val snapshot : t -> Accounting.t
val error : t -> Error.problem -> Error.t
val object_allowance : t -> int
val admit_object : t -> (unit, Error.t) Result.t
val admit_body : t -> int -> unit
val tree_allowance : t -> int
val admit_tree : t -> int -> (unit, Error.t) Result.t
val admit_source : t -> int -> (unit, Error.t) Result.t
val limits : t -> Limits.t
