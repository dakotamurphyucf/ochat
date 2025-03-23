module Make : (H : Lru_cache.H) -> sig
  (* exposed so a user can serialize the state of a cache and store to disk *)
  type 'a entry [@@deriving sexp, bin_io, hash, compare]
  type 'a t

  val create : max_size:int -> unit -> 'a t

  (* Helper to determine if an entry is expired relative to [now]. 
     Useful when say you are trying to filter out expired entries from a snapshot of a cache that was stored of disk *)
  val is_expired : 'a entry -> now:Core.Time_ns.Span.t -> bool
  val remove_expired : 'a t -> unit

  (* Insert with a TTL (i.e. partial-lifetime). [ttl] is added to [Time.now ()].
     For example, if ttl = 10s, then the item expires 10s from now. *)
  val set_with_ttl : 'a t -> key:H.t -> data:'a -> ttl:Core.Time_ns.Span.t -> unit

  (* Use to hydrate a cache with entry data that was stored on disk *)
  val set : 'a t -> key:H.t -> data:'a entry -> unit
  val find : 'a t -> H.t -> 'a option
  val mem : 'a t -> H.t -> bool
  val remove : 'a t -> H.t -> [ `No_such_key | `Ok ]
  val clear : 'a t -> [ `Dropped of int ]
  val length : 'a t -> int
  val max_size : 'a t -> int
  val hit_rate : 'a t -> float
  val stats : ?sexp_of_key:(H.t -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
  val is_empty : 'a t -> bool
  val to_alist : 'a t -> (H.t * 'a entry) list
  val set_max_size : 'a t -> max_size:int -> [ `Dropped of int ]
  val find_or_add : 'a t -> H.t -> default:(unit -> 'a) -> ttl:Core.Time_ns.Span.t -> 'a
  val find_and_remove : 'a t -> H.t -> 'a option
end
