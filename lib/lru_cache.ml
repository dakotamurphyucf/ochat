open Core
include Lru_cache_intf

module Make (H : H) = struct
  module Hq = struct
    include Hash_queue.Make (H)

    let to_alist t =
      foldi t ~init:[] ~f:(fun acc ~key ~data -> (key, data) :: acc) |> List.rev
    ;;

    let to_queue t =
      foldi
        t
        ~init:(Queue.create () ~capacity:(length t))
        ~f:(fun q ~key ~data ->
          Queue.enqueue q (key, data);
          q)
    ;;

    let sexp_of_t (type a) (sexp_of_a : a -> Sexp.t) t =
      t |> to_alist |> [%sexp_of: (H.t * a) list]
    ;;
  end

  type key = H.t

  type 'a t =
    { mutable max_size : int
    ; destruct_exn : ((key * 'a) Queue.t -> unit) option
      (** make sure this is called after the internals are updated and in a good state *)
    ; items : 'a Hq.t (** we evict from the front of the queue *)
    ; mutable num_queries : int
    ; mutable num_hits : int
    }

  let sexp_of_t (type a) (sexp_of_a : a -> Sexp.t) (t : a t) =
    [%sexp
      { max_size = (t.max_size : int)
      ; length = (Hq.length t.items : int)
      ; items = (t.items : a Hq.t)
      }]
  ;;

  let hit_rate t =
    if t.num_queries = 0
    then 0.
    else Float.of_int t.num_hits /. Float.of_int t.num_queries
  ;;

  let stats ?(sexp_of_key = [%sexp_of: H.t]) t =
    [%sexp
      { max_size = (t.max_size : int)
      ; length = (Hq.length t.items : int)
      ; hit_rate = (hit_rate t : float)
      ; keys = (Hq.keys t.items : key list)
      }]
  ;;

  let max_size_lower_bound = 0

  let invariant invariant_a t =
    Invariant.invariant [%here] (stats t) [%sexp_of: Sexp.t] (fun () ->
      assert (Hq.length t.items <= t.max_size);
      assert (t.max_size >= max_size_lower_bound);
      assert (t.num_queries >= t.num_hits);
      Hq.iteri t.items ~f:(fun ~key ~data ->
        Invariant.invariant [%here] key [%sexp_of: H.t] (fun () ->
          H.invariant key;
          invariant_a data)))
  ;;

  let check_max_size_exn max_size =
    if max_size < max_size_lower_bound
    then
      raise_s
        [%sexp
          "invalid Lru.max_size argument"
        , { requested_max_size = (max_size : int)
          ; smallest_value_allowed = (max_size_lower_bound : int)
          }]
  ;;

  let create ?destruct:destruct_exn ~max_size () =
    check_max_size_exn max_size;
    { max_size; destruct_exn; items = Hq.create (); num_queries = 0; num_hits = 0 }
  ;;

  let to_alist t = Hq.to_alist t.items
  let length t = Hq.length t.items
  let is_empty t = Hq.is_empty t.items
  let max_size t = t.max_size

  let find t key =
    t.num_queries <- t.num_queries + 1;
    match Hq.lookup_and_move_to_back t.items key with
    | None -> None
    | Some _ as result ->
      t.num_hits <- t.num_hits + 1;
      result
  ;;

  let mem t key = Option.is_some (find t key)

  let clear t =
    let len = length t in
    (match len > 0 with
     | false -> ()
     | true ->
       (match t.destruct_exn with
        | Some destruct_exn ->
          let evicted = Hq.to_queue t.items in
          Hq.clear t.items;
          destruct_exn evicted
        | None -> Hq.clear t.items));
    `Dropped len
  ;;

  let drop_lru_items_exn (type a) (t : a t) =
    let max_size = max 0 t.max_size in
    let num_evictions = length t - max_size in
    match num_evictions > 0 with
    | false -> ()
    | true ->
      (match t.destruct_exn with
       | Some destruct_exn ->
         let evicted = Queue.create () ~capacity:num_evictions in
         for _ = 1 to num_evictions do
           let item_with_key = Hq.dequeue_front_with_key_exn t.items in
           Queue.enqueue evicted item_with_key
         done;
         destruct_exn evicted
       | None ->
         for _ = 1 to num_evictions do
           ignore (Hq.dequeue_front_exn t.items : a)
         done)
  ;;

  let remove t key =
    match t.destruct_exn with
    | None -> Hq.remove t.items key
    | Some destruct_exn ->
      (match Hq.lookup_and_remove t.items key with
       | None -> `No_such_key
       | Some data ->
         destruct_exn (Queue.singleton (key, data));
         `Ok)
  ;;

  let set t ~key ~data =
    match remove t key with
    | `No_such_key ->
      Hq.enqueue_back_exn t.items key data;
      drop_lru_items_exn t
    (* In the [`Ok] and [exception] cases, there is no need to drop more items - an
       element was already in the hash queue and by removing it, we just made room for the
       new one. *)
    | `Ok -> Hq.enqueue_back_exn t.items key data
    | exception exn ->
      Hq.enqueue_back_exn t.items key data;
      raise exn
  ;;

  let find_or_add t key ~default =
    match find t key with
    | Some data -> data
    | None ->
      let data = default () in
      set t ~key ~data;
      data
  ;;

  let find_and_remove t key =
    t.num_queries <- t.num_queries + 1;
    match Hq.lookup_and_remove t.items key with
    | None -> None
    | Some data as result ->
      t.num_hits <- t.num_hits + 1;
      (match t.destruct_exn with
       | None -> ()
       | Some destruct_exn -> destruct_exn (Queue.singleton (key, data)));
      result
  ;;

  let set_max_size t ~max_size =
    check_max_size_exn max_size;
    let len = length t in
    t.max_size <- max_size;
    drop_lru_items_exn t;
    let len' = length t in
    `Dropped (len - len')
  ;;
end
