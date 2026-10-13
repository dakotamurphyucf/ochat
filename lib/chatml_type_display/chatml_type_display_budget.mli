open Core

(** Invocation-owned admission and byte emission; no graph or UI state. *)
module Limits : sig
  type t

  val create
    :  max_nodes:int
    -> max_edges:int
    -> max_depth:int
    -> max_work_steps:int
    -> max_text_bytes:int
    -> max_output_bytes:int
    -> t Or_error.t
end

module Completeness : sig
  type reason =
    | Node_limit
    | Edge_limit
    | Depth_limit
    | Work_limit
    | Byte_limit
  [@@deriving equal, sexp_of]

  type t =
    | Complete
    | Truncated of reason
  [@@deriving equal, sexp_of]
end

type session

exception Stop of Completeness.reason

val create : limits:Limits.t -> session
val max_depth : session -> int
val work : session -> checkpoint:(unit -> unit) -> unit
val node : session -> checkpoint:(unit -> unit) -> unit
val edge : session -> checkpoint:(unit -> unit) -> unit
val reverse_list : session -> 'a list -> checkpoint:(unit -> unit) -> 'a list

module Budgeted_labels : sig
  (** Hash every byte and compare collision candidates with checkpoints before
      inserting. No uncharged [String] hash/compare work or global cache. *)
  type 'a t

  val create : unit -> 'a t
  val find : 'a t -> string -> session:session -> checkpoint:(unit -> unit) -> 'a option

  val find_or_add
    :  'a t
    -> string
    -> session:session
    -> checkpoint:(unit -> unit)
    -> default:(unit -> 'a)
    -> 'a * bool
end

module Writer : sig
  type t

  val create : session -> checkpoint:(unit -> unit) -> t
  val emit : t -> string -> unit
  val escape_label : t -> string -> unit
  val finish : t -> string
  val truncated : t -> string
end
