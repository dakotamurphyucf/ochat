open Core

module type H = sig
  type t

  include Hashtbl.Key_plain with type t := t
  include Invariant.S with type t := t
end

module type S = sig
  type key
  type 'a t [@@deriving sexp_of]

  (** Creates an LRU cache.

      [destruct] is called on all elements removed from the cache, both implicilty (e.g.
      [set]) or explicitly (e.g. [remove], [clear]). [destruct] may raise; the exceptions
      pass through to the caller of the operation that triggered the removal. *)
  val create : ?destruct:((key * 'a) Queue.t -> unit) -> max_size:int -> unit -> 'a t

  (** Ordered from least- to most-recently used elements. *)
  val to_alist : 'a t -> (key * 'a) list

  val length : _ t -> int
  val is_empty : _ t -> bool
  val stats : ?sexp_of_key:(key -> Sexp.t) -> _ t -> Sexp.t
  val max_size : _ t -> int

  (** [hit_rate] is the ratio of calls to [mem], [find], and similar functions that
      queried for a key that was in the cache. *)
  val hit_rate : _ t -> float

  include Invariant.S1 with type 'a t := 'a t

  (** [mem] and [find] are considered as uses of the key, thus these operation refresh
      the priority of the key for the computation of the lru heuristic. *)
  val mem : _ t -> key -> bool

  val find : 'a t -> key -> 'a option

  (** Write operations on the [t] may drop some of the least recently used elements if the
      size exceeds the maximum size authorized. *)
  val clear : _ t -> [ `Dropped of int ]

  val set_max_size : _ t -> max_size:int -> [ `Dropped of int ]
  val remove : _ t -> key -> [ `Ok | `No_such_key ]
  val set : 'a t -> key:key -> data:'a -> unit
  val find_or_add : 'a t -> key -> default:(unit -> 'a) -> 'a
  val find_and_remove : 'a t -> key -> 'a option
end

module type Lru_cache = sig
  module type S = S
  module type H = H

  module Make (H : H) : S with type key = H.t
end
