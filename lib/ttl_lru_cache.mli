open Core

(** Time-to-live (TTL) wrapper around {!module:Lru_cache}.  Each binding is
    associated with an absolute expiration timestamp – expressed as a
    {!Core.Time_ns.Span.t} since the Unix epoch – and is considered invalid once
    [now >= expires_at].  Expired bindings are never returned by read
    operations and may be physically removed from the underlying cache by
    {!remove_expired} or implicitly during reads.

    The functor {!Make} reuses all the efficient O(1) operations provided by
    {!Lru_cache.Make}.  Only the semantics change: look-ups that hit an expired
    binding behave as misses and *atomically* purge the stale binding so that
    future accesses do not scan it again.

    {1 Usage at a glance}

    {[
      module String_cache = Ttl_lru_cache.Make (String)

      let%expect_test "basic" =
        let ttl   = Time_ns.Span.of_sec 10. in
        let cache = String_cache.create ~max_size:2 () in
        String_cache.set_with_ttl cache ~key:"k" ~data:"v" ~ttl;
        assert (String_cache.find cache "k" = Some "v");
        (* after 10 s: *)
        assert (String_cache.find cache "k" = None)
      ;;
    ]}

    @inline *)

module Make (H : Lru_cache.H) : sig
  (** {1 Types} *)

  (** Serializable record stored in the cache.  The [data] payload is user
      supplied.  [expires_at] is an absolute deadline – the binding is treated
      as stale for all [now >= expires_at]. *)
  type 'a entry =
    { data : 'a
    ; expires_at : Time_ns.Span.t
    }
  [@@deriving sexp, bin_io, hash, compare]

  (** Cache handle.  All operations have the same amortised complexity as
      their {!Lru_cache} counterparts. *)
  type 'a t

  (** {1 Construction} *)

  (** [create ~max_size ()] returns an empty cache that can grow to at most
      [max_size] bindings (same semantics as
      {!Lru_cache.Make.create}).  Negative sizes are rejected with an
      exception. *)
  val create : max_size:int -> unit -> 'a t

  (** {1 TTL helpers} *)

  (** [is_expired entry ~now] is [true] iff [now >= entry.expires_at].  Useful
      when post-processing a cache snapshot loaded from disk. *)
  val is_expired : 'a entry -> now:Time_ns.Span.t -> bool

  (** [remove_expired t] iterates over the cache and purges *all* bindings that
      are already expired at the time of the call.

      Warning: the traversal is O(length t).  Prefer calling it sparingly or in
      maintenance windows for large caches. *)
  val remove_expired : 'a t -> unit

  (** {1 Write operations with TTL} *)

  (** [set_with_ttl t ~key ~data ~ttl] stores [(key, data)] and marks it as
      valid for the next [ttl] seconds.  Internally, [ttl] is added to
      [Time_ns.Span.since_unix_epoch ()] to compute [expires_at].  A negative
      [ttl] immediately expires the binding. *)
  val set_with_ttl : 'a t -> key:H.t -> data:'a -> ttl:Time_ns.Span.t -> unit

  (** {1 Low-level accessors}
      The following functions expose the underlying {!Lru_cache} API so that a
      cache can be persisted and re-loaded.  They make no attempt to refresh
      TTLs. *)

  (** Hydrate [t] with a previously serialised [entry].  The call respects the
      LRU semantics (the binding becomes most-recently-used) but does *not*
      validate the freshness – callers are responsible for discarding stale
      entries. *)
  val set : 'a t -> key:H.t -> data:'a entry -> unit

  (** {1 Read operations}
      All reads promote the binding to most-recently-used *if and only if* it
      is still fresh.  An expired binding is removed and the read behaves as a
      cache miss. *)

  (** [find t key] returns [`Some data] if [key] is present **and** the binding
      has not expired, [`None] otherwise.  The function has the side effect of
      deleting the binding when it is found to be stale. *)
  val find : 'a t -> H.t -> 'a option

  (** [mem t key] is equivalent to [Option.is_some (find t key)]. *)
  val mem : 'a t -> H.t -> bool

  (** {1 Mutating operations inherited from {!module:Lru_cache}}
      These functions have unchanged semantics and complexity. *)

  val remove : 'a t -> H.t -> [ `No_such_key | `Ok ]
  val clear : 'a t -> [ `Dropped of int ]
  val length : 'a t -> int
  val max_size : 'a t -> int
  val hit_rate : 'a t -> float
  val stats : ?sexp_of_key:(H.t -> Sexp.t) -> 'a t -> Sexp.t
  val is_empty : 'a t -> bool
  val to_alist : 'a t -> (H.t * 'a entry) list
  val set_max_size : 'a t -> max_size:int -> [ `Dropped of int ]

  (** [find_or_add t key ~default ~ttl] is equivalent to:

      {[
        match find t key with
        | Some v -> v                         (* cache hit *)
        | None ->                              (* miss or expired *)
          let v = default () in
          set_with_ttl t ~key ~data:v ~ttl;
          v
      ]}

      The newly inserted binding expires after [ttl]. *)
  val find_or_add : 'a t -> H.t -> default:(unit -> 'a) -> ttl:Time_ns.Span.t -> 'a

  (** [find_and_remove t key] returns the data bound to [key] if present and
      fresh, then deletes the binding unconditionally (expired or not).  The
      operation counts as a cache hit when the result is [`Some _]. *)
  val find_and_remove : 'a t -> H.t -> 'a option
end
