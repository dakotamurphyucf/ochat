Querying indexed OCaml code with text: **functions for lru caches**
Using vector database data from folder: **./vector-core**
Returning top **20** results

**Result 1:**
```ocaml
(** 
Location: File "memo.ml", line 46, characters 0-770
Module Path: Memo
OCaml Source: Implementation
*)


let lru (type a) ?(hashable = Hashtbl.Hashable.poly) ~max_cache_size f =
  if max_cache_size <= 0
  then failwithf "Memo.lru: max_cache_size of %i <= 0" max_cache_size ();
  let module Cache =
    Hash_queue.Make (struct
      type t = a

      let { Hashtbl.Hashable.hash; compare; sexp_of_t } = hashable
    end)
  in
  let cache = Cache.create () in
  fun arg ->
    Result.return
      (match Cache.lookup_and_move_to_back cache arg with
       | Some result -> result
       | None ->
         let result = Result.capture f arg in
         Cache.enqueue_back_exn cache arg result;
         (* eject least recently used cache entry *)
         if Cache.length cache > max_cache_size
         then ignore (Cache.dequeue_front_exn cache : _ Result.t);
         result)
```

**Result 2:**
```ocaml
(** 
Location: File "memo.ml", line 70, characters 0-155
Module Path: Memo
OCaml Source: Implementation
*)


let general ?hashable ?cache_size_bound f =
  match cache_size_bound with
  | None -> unbounded ?hashable f
  | Some n -> lru ?hashable ~max_cache_size:n f
```

**Result 3:**
```ocaml
(** 
Location: File "hashtbl.ml", line 75, characters 2-211
Module Path: Hashtbl.Using_hashable
OCaml Source: Implementation
*)


let group ?growth_allowed ?size ~hashable ~get_key ~get_data ~combine l =
    group
      ?growth_allowed
      ?size
      (Base.Hashable.to_key hashable)
      ~get_key
      ~get_data
      ~combine
      l
```

**Result 4:**
```ocaml
(** 
Location: File "hashtbl.ml", line 48, characters 2-203
Module Path: Hashtbl.Using_hashable
OCaml Source: Implementation
*)


let create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data l =
    create_mapped
      ?growth_allowed
      ?size
      (Base.Hashable.to_key hashable)
      ~get_key
      ~get_data
      l
```

**Result 5:**
```ocaml
(** 
Location: File "hashtbl.ml", line 58, characters 2-151
Module Path: Hashtbl.Using_hashable
OCaml Source: Implementation
*)


let create_with_key ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key l
```

**Result 6:**
```ocaml
(** 
Location: File "hash_queue.ml", line 13, characters 2-10074
Module Path: Hash_queue.Make_backend
OCaml Source: Implementation
*)


module Backend : Backend = struct
    module Key_value = struct
      module T = struct
        type ('key, 'value) t =
          { key : 'key
          ; mutable value : 'value
          }
      end

      include T

      let key t = t.key
      let value t = t.value

      let sexp_of_t sexp_of_key sexp_of_data { key; value } =
        [%sexp_of: key * data] (key, value)
      ;;
    end

    open Key_value.T
    module Elt = Doubly_linked.Elt

    type ('key, 'data) t =
      { mutable num_readers : int
      ; queue : ('key, 'data) Key_value.t Doubly_linked.t
      ; table : ('key, ('key, 'data) Key_value.t Elt.t) Table.t
      }

    let sexp_of_t sexp_of_key sexp_of_data t =
      [%sexp_of: (key, data) Key_value.t Doubly_linked.t] t.queue
    ;;

    let invariant t =
      assert (Doubly_linked.length t.queue = Table.length t.table);
      (* Look at each element in the queue, checking:
       *   - every element in the queue is in the hash table
       *   - there are no duplicate keys
      *)
      let keys = Table.create ~size:(Table.length t.table) (Table.hashable_s t.table) in
      Doubly_linked.iter t.queue ~f:(fun kv ->
        let key = kv.key in
        match Table.find t.table key with
        | None -> assert false
        | Some _ ->
          assert (not (Table.mem keys key));
          Table.set keys ~key ~data:())
    ;;

    let create ?(growth_allowed = true) ?(size = 16) hashable =
      { num_readers = 0
      ; queue = Doubly_linked.create ()
      ; table = Table.create ~growth_allowed ~size (Table.Hashable.to_key hashable)
      }
    ;;

    let read t f =
      t.num_readers <- t.num_readers + 1;
      Exn.protect ~f ~finally:(fun () -> t.num_readers <- t.num_readers - 1)
    ;;

    let ensure_can_modify t =
      if t.num_readers > 0
      then failwith "It is an error to modify a Hash_queue.t while iterating over it."
    ;;

    let clear t =
      ensure_can_modify t;
      Doubly_linked.clear t.queue;
      Table.clear t.table
    ;;

    let length t = Table.length t.table
    let is_empty t = length t = 0

    let lookup t k =
      match Table.find t.table k with
      | None -> None
      | Some elt -> Some (Elt.value elt).value
    ;;

    let lookup_exn t k = (Elt.value (Table.find_exn t.table k)).value
    let mem t k = Table.mem t.table k

    (* Note that this is the tail-recursive Core_list.map *)
    let to_list t = List.map (Doubly_linked.to_list t.queue) ~f:Key_value.value
    let to_array t = Array.map (Doubly_linked.to_array t.queue) ~f:Key_value.value

    let for_all t ~f =
      read t (fun () -> Doubly_linked.for_all t.queue ~f:(fun kv -> f kv.value))
    ;;

    let exists t ~f =
      read t (fun () -> Doubly_linked.exists t.queue ~f:(fun kv -> f kv.value))
    ;;

    let find_map t ~f =
      read t (fun () -> Doubly_linked.find_map t.queue ~f:(fun kv -> f kv.value))
    ;;

    let find t ~f =
      read t (fun () ->
        Option.map
          (Doubly_linked.find t.queue ~f:(fun kv -> f kv.value))
          ~f:Key_value.value)
    ;;

    let enqueue t back_or_front key value =
      ensure_can_modify t;
      if Table.mem t.table key
      then `Key_already_present
      else (
        let contents = { Key_value.key; value } in
        let elt =
          match back_or_front with
          | `back -> Doubly_linked.insert_last t.queue contents
          | `front -> Doubly_linked.insert_first t.queue contents
        in
        Table.set t.table ~key ~data:elt;
        `Ok)
    ;;

    let enqueue_back t = enqueue t `back
    let enqueue_front t = enqueue t `front

    let raise_enqueue_duplicate_key t key =
      raise_s
        [%message
          "Hash_queue.enqueue_exn: duplicate key"
            ~_:(Table.sexp_of_key t.table key : Sexp.t)]
    ;;

    let enqueue_exn t back_or_front key value =
      match enqueue t back_or_front key value with
      | `Key_already_present -> raise_enqueue_duplicate_key t key
      | `Ok -> ()
    ;;

    let enqueue_back_exn t = enqueue_exn t `back
    let enqueue_front_exn t = enqueue_exn t `front

    (* Performance hack: we implement this version separately to avoid allocation from the
       option. *)
    let lookup_and_move_to_back_exn t key =
      ensure_can_modify t;
      let elt = Table.find_exn t.table key in
      Doubly_linked.move_to_back t.queue elt;
      Key_value.value (Elt.value elt)
    ;;

    let lookup_and_move_to_back t key =
      let open Option.Let_syntax in
      ensure_can_modify t;
      let%map elt = Table.find t.table key in
      Doubly_linked.move_to_back t.queue elt;
      Key_value.value (Elt.value elt)
    ;;

    let lookup_and_move_to_front_exn t key =
      ensure_can_modify t;
      let elt = Table.find_exn t.table key in
      Doubly_linked.move_to_front t.queue elt;
      Key_value.value (Elt.value elt)
    ;;

    let lookup_and_move_to_front t key =
      let open Option.Let_syntax in
      ensure_can_modify t;
      let%map elt = Table.find t.table key in
      Doubly_linked.move_to_front t.queue elt;
      Key_value.value (Elt.value elt)
    ;;

    let dequeue_with_key t back_or_front =
      ensure_can_modify t;
      let maybe_kv =
        match back_or_front with
        | `back -> Doubly_linked.remove_last t.queue
        | `front -> Doubly_linked.remove_first t.queue
      in
      match maybe_kv with
      | None -> None
      | Some kv ->
        Table.remove t.table kv.key;
        Some (kv.key, kv.value)
    ;;

    let raise_dequeue_with_key_empty () =
      raise_s [%message "Hash_queue.dequeue_with_key: empty queue"]
    ;;

    let dequeue_with_key_exn t back_or_front =
      match dequeue_with_key t back_or_front with
      | None -> raise_dequeue_with_key_empty ()
      | Some (k, v) -> k, v
    ;;

    let dequeue_back_with_key t = dequeue_with_key t `back
    let dequeue_back_with_key_exn t = dequeue_with_key_exn t `back
    let dequeue_front_with_key t = dequeue_with_key t `front
    let dequeue_front_with_key_exn t = dequeue_with_key_exn t `front

    let dequeue t back_or_front =
      match dequeue_with_key t back_or_front with
      | None -> None
      | Some (_, v) -> Some v
    ;;

    let dequeue_back t = dequeue t `back
    let dequeue_front t = dequeue t `front

    let last_with_key t =
      match Doubly_linked.last t.queue with
      | None -> None
      | Some { key; value } -> Some (key, value)
    ;;

    let last t =
      match Doubly_linked.last t.queue with
      | None -> None
      | Some kv -> Some kv.value
    ;;

    let first_with_key t =
      match Doubly_linked.first t.queue with
      | None -> None
      | Some { key; value } -> Some (key, value)
    ;;

    let first t =
      match Doubly_linked.first t.queue with
      | None -> None
      | Some kv -> Some kv.value
    ;;

    let raise_dequeue_empty () = raise_s [%message "Hash_queue.dequeue_exn: empty queue"]

    let dequeue_exn t back_or_front =
      match dequeue t back_or_front with
      | None -> raise_dequeue_empty ()
      | Some v -> v
    ;;

    let dequeue_back_exn t = dequeue_exn t `back
    let dequeue_front_exn t = dequeue_exn t `front

    let keys t =
      (* Return the keys in the order of the queue. *)
      List.map (Doubly_linked.to_list t.queue) ~f:Key_value.key
    ;;

    let iteri t ~f =
      read t (fun () ->
        Doubly_linked.iter t.queue ~f:(fun kv -> f ~key:kv.key ~data:kv.value))
    ;;

    let iter t ~f = iteri t ~f:(fun ~key:_ ~data -> f data)

    let foldi t ~init ~f =
      read t (fun () ->
        Doubly_linked.fold t.queue ~init ~f:(fun ac kv ->
          f ac ~key:kv.key ~data:kv.value))
    ;;

    let fold t ~init ~f = foldi t ~init ~f:(fun ac ~key:_ ~data -> f ac data)
    let count t ~f = Container.count ~fold t ~f
    let sum m t ~f = Container.sum m ~fold t ~f
    let min_elt t ~compare = Container.min_elt ~fold t ~compare
    let max_elt t ~compare = Container.max_elt ~fold t ~compare
    let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
    let fold_until t ~init ~f = Container.fold_until ~fold ~init ~f t

    let dequeue_all t ~f =
      let rec loop () =
        match dequeue_front t with
        | None -> ()
        | Some v ->
          f v;
          loop ()
      in
      loop ()
    ;;

    let remove t k =
      ensure_can_modify t;
      match Table.find_and_remove t.table k with
      | None -> `No_such_key
      | Some elt ->
        Doubly_linked.remove t.queue elt;
        `Ok
    ;;

    let raise_remove_unknown_key t key =
      raise_s
        [%message
          "Hash_queue.remove_exn: unknown key" ~_:(Table.sexp_of_key t.table key : Sexp.t)]
    ;;

    let remove_exn t k =
      ensure_can_modify t;
      match remove t k with
      | `No_such_key -> raise_remove_unknown_key t k
      | `Ok -> ()
    ;;

    let lookup_and_remove t k =
      ensure_can_modify t;
      match Table.find_and_remove t.table k with
      | None -> None
      | Some elt ->
        Doubly_linked.remove t.queue elt;
        Some (Elt.value elt).value
    ;;

    let replace t k v =
      ensure_can_modify t;
      match Table.find t.table k with
      | None -> `No_such_key
      | Some elt ->
        (Elt.value elt).value <- v;
        `Ok
    ;;

    let raise_replace_unknown_key t key =
      raise_s
        [%message
          "Hash_queue.replace_exn: unknown key"
            ~_:(Table.sexp_of_key t.table key : Sexp.t)]
    ;;

    let replace_exn t k v =
      ensure_can_modify t;
      match replace t k v with
      | `No_such_key -> raise_replace_unknown_key t k
      | `Ok -> ()
    ;;

    let drop ?(n = 1) t back_or_front =
      if n >= length t
      then clear t
      else
        for _ = 1 to n do
          ignore (dequeue_with_key t back_or_front : _ option)
        done
    ;;

    let drop_back ?n t = drop ?n t `back
    let drop_front ?n t = drop ?n t `front

    let copy t =
      let copied = create ~size:(length t) (Table.hashable t.table) in
      iteri t ~f:(fun ~key ~data -> enqueue_back_exn copied key data);
      copied
    ;;
  end
```

**Result 7:**
```ocaml
(** 
Location: File "memo.ml", line 28, characters 0-533
Module Path: Memo
OCaml Source: Implementation
*)


let unbounded (type a) ?(hashable = Hashtbl.Hashable.poly) f =
  let cache =
    let module A =
      Hashable.Make_plain_and_derive_hash_fold_t (struct
        type t = a

        let { Hashtbl.Hashable.hash; compare; sexp_of_t } = hashable
      end)
    in
    A.Table.create () ~size:0
  in
  (* Allocate this closure at the call to [unbounded], not at each call to the memoized
     function. *)
  let really_call_f arg = Result.capture f arg in
  fun arg -> Result.return (Hashtbl.findi_or_add cache arg ~default:really_call_f)
```

**Result 8:**
```ocaml
(** 
Location: File "memo.ml", line 23, characters 0-66
Module Path: Memo
OCaml Source: Implementation
*)


let unit f =
  let l = Lazy.from_fun f in
  fun () -> Lazy.force l
```

**Result 9:**
```ocaml
(** 
Location: File "memo.ml", line 98, characters 0-185
Module Path: Memo
OCaml Source: Implementation
*)


let recursive ~hashable ?cache_size_bound f_onestep =
  let rec memoized =
    lazy (general ~hashable ?cache_size_bound (f_onestep (fun x -> (force memoized) x)))
  in
  force memoized
```

**Result 10:**
```ocaml
(** 
Location: File "hashtbl.ml", line 71, characters 2-159
Module Path: Hashtbl.Using_hashable
OCaml Source: Implementation
*)


let create_with_key_exn ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key_exn ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key l
```

**Result 11:**
```ocaml
(** 
Location: File "hashtbl.ml", line 62, characters 2-199
Module Path: Hashtbl.Using_hashable
OCaml Source: Implementation
*)


let create_with_key_or_error ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key_or_error
      ?growth_allowed
      ?size
      (Base.Hashable.to_key hashable)
      ~get_key
      l
```

**Result 12:**
```ocaml
(** 
Location: File "hashtbl.ml", line 44, characters 2-131
Module Path: Hashtbl.Using_hashable
OCaml Source: Implementation
*)


let of_alist_multi ?growth_allowed ?size ~hashable l =
    of_alist_multi ?growth_allowed ?size (Base.Hashable.to_key hashable) l
```

**Result 13:**
```ocaml
(** 
Location: File "hash_queue.ml", line 185, characters 4-375
Module Path: Hash_queue.Make_backend.Backend
OCaml Source: Implementation
*)


let dequeue_with_key t back_or_front =
      ensure_can_modify t;
      let maybe_kv =
        match back_or_front with
        | `back -> Doubly_linked.remove_last t.queue
        | `front -> Doubly_linked.remove_first t.queue
      in
      match maybe_kv with
      | None -> None
      | Some kv ->
        Table.remove t.table kv.key;
        Some (kv.key, kv.value)
```

**Result 14:**
```ocaml
(** 
Location: File "hashtbl.ml", line 21, characters 0-1872
Module Path: Hashtbl
OCaml Source: Implementation
*)


module Using_hashable = struct
  type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving sexp_of]

  let create ?growth_allowed ?size ~hashable () =
    create ?growth_allowed ?size (Base.Hashable.to_key hashable)
  ;;

  let of_alist ?growth_allowed ?size ~hashable l =
    of_alist ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;

  let of_alist_report_all_dups ?growth_allowed ?size ~hashable l =
    of_alist_report_all_dups ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;

  let of_alist_or_error ?growth_allowed ?size ~hashable l =
    of_alist_or_error ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;

  let of_alist_exn ?growth_allowed ?size ~hashable l =
    of_alist_exn ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;

  let of_alist_multi ?growth_allowed ?size ~hashable l =
    of_alist_multi ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;

  let create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data l =
    create_mapped
      ?growth_allowed
      ?size
      (Base.Hashable.to_key hashable)
      ~get_key
      ~get_data
      l
  ;;

  let create_with_key ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key l
  ;;

  let create_with_key_or_error ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key_or_error
      ?growth_allowed
      ?size
      (Base.Hashable.to_key hashable)
      ~get_key
      l
  ;;

  let create_with_key_exn ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key_exn ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key l
  ;;

  let group ?growth_allowed ?size ~hashable ~get_key ~get_data ~combine l =
    group
      ?growth_allowed
      ?size
      (Base.Hashable.to_key hashable)
      ~get_key
      ~get_data
      ~combine
      l
  ;;
end
```

**Result 15:**
```ocaml
(** 
Location: File "hashtbl.ml", line 28, characters 2-119
Module Path: Hashtbl.Using_hashable
OCaml Source: Implementation
*)


let of_alist ?growth_allowed ?size ~hashable l =
    of_alist ?growth_allowed ?size (Base.Hashable.to_key hashable) l
```

**Result 16:**
```ocaml
(** 
Location: File "hashtbl.ml", line 36, characters 2-137
Module Path: Hashtbl.Using_hashable
OCaml Source: Implementation
*)


let of_alist_or_error ?growth_allowed ?size ~hashable l =
    of_alist_or_error ?growth_allowed ?size (Base.Hashable.to_key hashable) l
```

**Result 17:**
```ocaml
(** 
Location: File "hash_queue.ml", line 119, characters 4-446
Module Path: Hash_queue.Make_backend.Backend
OCaml Source: Implementation
*)


let enqueue t back_or_front key value =
      ensure_can_modify t;
      if Table.mem t.table key
      then `Key_already_present
      else (
        let contents = { Key_value.key; value } in
        let elt =
          match back_or_front with
          | `back -> Doubly_linked.insert_last t.queue contents
          | `front -> Doubly_linked.insert_first t.queue contents
        in
        Table.set t.table ~key ~data:elt;
        `Ok)
```

**Result 18:**
```ocaml
(** 
Location: File "hash_queue_intf.ml", line 188, characters 2-149
Module Path: Hash_queue_intf.S0
OCaml Source: Implementation
*)


include
    S1
    with type 'key create_key := key
    with type 'key create_arg := unit
    with type ('key, 'data) t := ('key, 'data) hash_queue
```

**Result 19:**
```ocaml
(** 
Location: File "hash_queue.ml", line 229, characters 4-114
Module Path: Hash_queue.Make_backend.Backend
OCaml Source: Implementation
*)


let last t =
      match Doubly_linked.last t.queue with
      | None -> None
      | Some kv -> Some kv.value
```

**Result 20:**
```ocaml
(** 
Location: File "hash_queue.ml", line 14, characters 4-359
Module Path: Hash_queue.Make_backend.Backend
OCaml Source: Implementation
*)


module Key_value = struct
      module T = struct
        type ('key, 'value) t =
          { key : 'key
          ; mutable value : 'value
          }
      end

      include T

      let key t = t.key
      let value t = t.value

      let sexp_of_t sexp_of_key sexp_of_data { key; value } =
        [%sexp_of: key * data] (key, value)
      ;;
    end
```
