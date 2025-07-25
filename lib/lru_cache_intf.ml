open Core

(** Bounded least-recently-used (LRU) caches.

    This interface is consumed by {!module:Lru_cache}.  The functor {!Make}
    instantiates a cache for any key type that can live in a {!Core.Hashtbl} and whose
    values satisfy an invariant.

    A cache created with {!create} behaves as follows:

    • Bindings are promoted to *most-recently-used* on every successful
      read-operation – {!mem}, {!find}, {!find_and_remove} – and on every write.

    • When inserting or resizing would make the cache exceed its [max_size], the
      least-recently-used bindings are evicted first.

    • An optional clean-up callback ([destruct]) can observe or dispose of evicted
      bindings.

    All public operations run in amortised O(1) time.
*)

module type H = sig
  type t

  include Hashtbl.Key_plain with type t := t
  include Invariant.S with type t := t
end

module type S = sig
  type key
  type 'a t [@@deriving sexp_of]

  (** [create ?destruct ~max_size ()] returns a fresh cache that can store at most
      [max_size] bindings.

      A binding is an association [(key, data)].  When an insertion would grow the
      cache beyond [max_size], the least-recently-used bindings are evicted until the
      size constraint is satisfied.

      If provided, [destruct] is invoked once for every batch of evicted bindings.  It
      receives the bindings in LRU-to-MRU order wrapped in a {!Core.Queue.t}.  The queue
      can be safely traversed or drained by the callback.  Exceptions raised by
      [destruct] are re-raised in the context of the operation that triggered the
      eviction so that callers can react (e.g. log, retry, abort).

      @param destruct     clean-up callback executed on every eviction or explicit
                          removal.  Optional.
      @param max_size     upper bound on the number of bindings that may be stored.
                          Must be non-negative.  Zero disables caching entirely. *)
  val create : ?destruct:((key * 'a) Queue.t -> unit) -> max_size:int -> unit -> 'a t

  (** [to_alist t] returns the contents of [t] as an association list ordered from the
      least-recently-used binding to the most-recently-used.  The list is a snapshot – it
      is not affected by subsequent cache mutations. *)
  val to_alist : 'a t -> (key * 'a) list

  (** [length t] returns the current number of bindings. *)
  val length : _ t -> int

  (** [is_empty t] is [true] iff [length t = 0]. *)
  val is_empty : _ t -> bool

  (** [stats ?sexp_of_key t] exposes internal statistics useful for debugging and
      testing.  The resulting S-expression contains the current length, [max_size],
      {!hit_rate}, and the list of cached keys.  [sexp_of_key] allows callers to control
      the representation of keys in the output. *)
  val stats : ?sexp_of_key:(key -> Sexp.t) -> _ t -> Sexp.t

  (** [max_size t] returns the current capacity.  Use {!set_max_size} to modify it. *)
  val max_size : _ t -> int

  (** [hit_rate t] is the ratio of successful look-ups to total look-ups since [t] was
      created.  A *look-up* is any call to {!mem}, {!find}, or {!find_and_remove}.  The
      value is in the inclusive range [[0.; 1.]]. *)
  val hit_rate : _ t -> float

  include Invariant.S1 with type 'a t := 'a t

  (** {1 Read-only operations}

      All read-only operations count as uses: they promote the accessed binding to the
      back of the internal LRU queue so that it is considered *most recently used*. *)

  (** [mem t key] is [true] iff [key] is present in [t]. *)
  val mem : _ t -> key -> bool

  (** [find t key] returns the data bound to [key], or [None] if the key is absent. *)
  val find : 'a t -> key -> 'a option

  (** {1 Mutating operations}

      Mutations may evict bindings so that the cache respects its size constraint.  All
      evictions happen *before* the corresponding function returns. *)

  (** [clear t] removes every binding from [t] and returns [`Dropped n] where [n] is the
      number of bindings that were present.  The [destruct] callback is invoked once (if
      any bindings were present). *)
  val clear : _ t -> [ `Dropped of int ]

  (** [set_max_size t ~max_size] changes the capacity of [t] and immediately evicts
      bindings if the new capacity is smaller than [length t].  The result [`Dropped n]
      reports how many bindings were removed. *)
  val set_max_size : _ t -> max_size:int -> [ `Dropped of int ]

  (** [remove t key] deletes [key] from [t] and returns:
      - [`Ok]            – the key was present and has been removed;
      - [`No_such_key]   – no binding for [key] existed.

      When a binding is removed, [destruct] is invoked with a queue containing exactly
      that binding. *)
  val remove : _ t -> key -> [ `Ok | `No_such_key ]

  (** [set t ~key ~data] adds the binding [(key, data)].  If [key] was already present
      its previous value is discarded (after the [destruct] callback).  The binding is
      considered most-recently-used after the call. *)
  val set : 'a t -> key:key -> data:'a -> unit

  (** [find_or_add t key ~default] is equivalent to:
      {[
        match find t key with
        | Some v -> v                       (* cache hit *)
        | None ->                           (* cache miss *)
          let v = default () in
          set t ~key ~data:v;
          v
      ]}

      The final value is returned to the caller.  The binding is considered
      most-recently-used. *)
  val find_or_add : 'a t -> key -> default:(unit -> 'a) -> 'a

  (** [find_and_remove t key] returns the data bound to [key] if present and immediately
      removes the binding.  The call counts as a cache hit for the purpose of
      {!hit_rate}. *)
  val find_and_remove : 'a t -> key -> 'a option
end

module type Lru_cache = sig
  module type S = S
  module type H = H

  module Make (H : H) : S with type key = H.t
end
