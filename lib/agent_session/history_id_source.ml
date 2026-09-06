open! Core

type reservation =
  { first_sequence : int64
  ; reserved_through : int64
  }

type t =
  { namespace : string
  ; block_size : int
  ; reserve : count:int -> (reservation, Agent_protocol.Error.t) result
  ; mutex : Eio.Mutex.t
  ; mutable next : int64
  ; mutable reserved_through : int64
  }

let invalid message =
  Agent_protocol.Error.create Invalid_request ~message ~retryable:false ()
;;

let create ~namespace ~block_size ~reserve =
  if String.is_empty namespace
  then Error (invalid "history namespace must be nonempty")
  else if block_size <= 0
  then Error (invalid "history block size must be positive")
  else
    Ok
      { namespace
      ; block_size
      ; reserve
      ; mutex = Eio.Mutex.create ()
      ; next = 0L
      ; reserved_through = 0L
      }
;;

let refill t =
  Result.map (t.reserve ~count:t.block_size) ~f:(fun reservation ->
    t.next <- reservation.first_sequence;
    t.reserved_through <- reservation.reserved_through)
;;

let allocate_locked t =
  let open Result.Let_syntax in
  let%bind () = if Int64.(t.next < t.reserved_through) then Ok () else refill t in
  let sequence = t.next in
  t.next <- Int64.(t.next + 1L);
  if Int64.(sequence > of_int Int.max_value)
  then Error (invalid "history sequence exceeds platform allocation range")
  else
    History_entry.Id.create ~namespace:t.namespace ~sequence:(Int64.to_int_exn sequence)
    |> Result.map_error ~f:invalid
;;

let allocate t = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> allocate_locked t)

let discard_reserved t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.next <- t.reserved_through)
;;

let remaining t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Int64.(t.reserved_through - t.next) |> Int64.to_int_exn)
;;

let namespace t = t.namespace

let next_reserved_sequence t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    if Int64.(t.reserved_through > of_int Int.max_value)
    then Int.max_value
    else Int64.to_int_exn t.reserved_through)
;;

let validate t entries =
  let ids = Hash_set.create (module History_entry.Id) in
  let reserved_through = Eio.Mutex.use_ro t.mutex (fun () -> t.reserved_through) in
  List.fold_result entries ~init:() ~f:(fun () entry ->
    let id = History_entry.id entry in
    let sequence = History_entry.Id.sequence id in
    if Hash_set.mem ids id
    then Error (invalid "history contains a duplicate entry ID")
    else (
      Hash_set.add ids id;
      if
        String.equal (History_entry.Id.namespace id) t.namespace
        && Int64.(of_int sequence >= reserved_through)
      then Error (invalid "history entry is outside the committed ID reservation")
      else Ok ()))
;;

let as_history_entry_source t =
  History_entry.Id_source.create
    ~namespace:t.namespace
    ~allocate:(fun () -> Result.map_error (allocate t) ~f:(fun error -> error.message))
    ~validate:(fun entries ->
      Result.map_error (validate t entries) ~f:(fun error -> error.message))
;;
