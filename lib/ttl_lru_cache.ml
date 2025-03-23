open Core

module Make (H : Lru_cache.H) = struct
  let time_now () = Time_ns.Span.since_unix_epoch ()

  (* Each stored value is augmented with an 'expires_at' time. *)
  type 'a entry =
    { data : 'a
    ; expires_at : Time_ns.Span.t
    }
  [@@deriving sexp, bin_io, hash, compare]

  module Base_lru = Lru_cache.Make (H)

  (* Our TTL LRU is just the base LRU from H.t to an ['a entry]. *)
  type 'a t = 'a entry Base_lru.t

  let create ~max_size () : 'a t = Base_lru.create ~max_size ()

  (* Helper to determine if an entry is expired relative to [now]. *)
  let is_expired (entry : _ entry) ~now = Time_ns.Span.(now >= entry.expires_at)

  (* One approach: remove all expired items immediately.  This may be fine
     for smaller caches or if you call it infrequently. *)
  let remove_expired t =
    let now = time_now () in
    Base_lru.to_alist t
    |> List.iter ~f:(fun (key, entry) ->
      if is_expired entry ~now
      then ignore (Base_lru.remove t key : [ `Ok | `No_such_key ]));
    ()
  ;;

  (* Insert with a TTL (i.e. partial-lifetime). [ttl] is added to [Time.now ()].
     For example, if ttl = 10s, then the item expires 10s from now. *)
  let set_with_ttl t ~key ~data ~ttl =
    let now = time_now () in
    let expires_at = Time_ns.Span.(now + ttl) in
    let entry = { data; expires_at } in
    Base_lru.set t ~key ~data:entry
  ;;

  let set (t : 'a entry Base_lru.t) ~key ~data = Base_lru.set t ~key ~data

  let find t key =
    match Base_lru.find t key with
    | None -> None
    | Some entry ->
      let now = time_now () in
      if is_expired entry ~now
      then (
        ignore (Base_lru.remove t key : [ `Ok | `No_such_key ]);
        None)
      else Some entry.data
  ;;

  let mem t key = Option.is_some (find t key)
  let remove (t : 'a entry Base_lru.t) key = Base_lru.remove t key
  let clear (t : 'a entry Base_lru.t) = Base_lru.clear t
  let length (t : 'a entry Base_lru.t) = Base_lru.length t
  let max_size (t : 'a entry Base_lru.t) = Base_lru.max_size t
  let hit_rate (t : 'a entry Base_lru.t) = Base_lru.hit_rate t
  let stats ?sexp_of_key (t : 'a entry Base_lru.t) = Base_lru.stats ?sexp_of_key t
  let is_empty (t : 'a entry Base_lru.t) = Base_lru.is_empty t
  let to_alist (t : 'a entry Base_lru.t) = Base_lru.to_alist t
  let set_max_size (t : 'a entry Base_lru.t) ~max_size = Base_lru.set_max_size t ~max_size

  let find_or_add t key ~default ~ttl =
    match find t key with
    | Some data -> data
    | None ->
      let data = default () in
      set_with_ttl t ~key ~data ~ttl;
      data
  ;;

  let find_and_remove t key =
    match Base_lru.find_and_remove t key with
    | None -> None
    | Some entry ->
      let now = time_now () in
      if is_expired entry ~now then None else Some entry.data
  ;;
end
