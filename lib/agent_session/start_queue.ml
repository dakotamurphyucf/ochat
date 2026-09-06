open Core

type ticket =
  { session_id : Agent_protocol.Id.Session.t
  ; accepted_command_sequence : int64
  ; quota_key : Quota_key.t
  ; created_at : Agent_protocol.Timestamp.t
  }
[@@deriving sexp]

type t =
  { mutex : Eio.Mutex.t
  ; mutable queues : (Quota_key.t, ticket Fqueue.t) Map.Poly.t
  ; mutable key_order : Quota_key.t Fqueue.t
  ; mutable sessions : (Agent_protocol.Id.Session.t, unit) Map.Poly.t
  }

let create () =
  { mutex = Eio.Mutex.create ()
  ; queues = Map.Poly.empty
  ; key_order = Fqueue.empty
  ; sessions = Map.Poly.empty
  }
;;

let duplicate_error () =
  Agent_protocol.Error.create
    Conflict
    ~message:"session already has a queued start"
    ~retryable:false
    ()
;;

let enqueue t ticket =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Map.mem t.sessions ticket.session_id
    then Error (duplicate_error ())
    else (
      let existing =
        Map.find t.queues ticket.quota_key |> Option.value ~default:Fqueue.empty
      in
      if Fqueue.is_empty existing
      then t.key_order <- Fqueue.enqueue t.key_order ticket.quota_key;
      t.queues
      <- Map.set t.queues ~key:ticket.quota_key ~data:(Fqueue.enqueue existing ticket);
      t.sessions <- Map.set t.sessions ~key:ticket.session_id ~data:();
      Ok ()))
;;

let requeue t ticket =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Map.mem t.sessions ticket.session_id
    then Error (duplicate_error ())
    else (
      let existing = Map.find t.queues ticket.quota_key in
      let queue =
        match existing with
        | None -> Fqueue.singleton ticket
        | Some queue -> Fqueue.of_list (ticket :: Fqueue.to_list queue)
      in
      if Option.is_none existing
      then t.key_order <- Fqueue.enqueue t.key_order ticket.quota_key;
      t.queues <- Map.set t.queues ~key:ticket.quota_key ~data:queue;
      t.sessions <- Map.set t.sessions ~key:ticket.session_id ~data:();
      Ok ()))
;;

let filter_queue queue ~f = Fqueue.to_list queue |> List.filter ~f |> Fqueue.of_list

let cancel t session_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if not (Map.mem t.sessions session_id)
    then false
    else (
      t.queues
      <- Map.filter_map t.queues ~f:(fun queue ->
           let queue =
             filter_queue queue ~f:(fun ticket ->
               Agent_protocol.Id.Session.compare ticket.session_id session_id <> 0)
           in
           if Fqueue.is_empty queue then None else Some queue);
      t.key_order <- filter_queue t.key_order ~f:(fun key -> Map.mem t.queues key);
      t.sessions <- Map.remove t.sessions session_id;
      true))
;;

let head_for_key t key =
  Map.find t.queues key |> Option.bind ~f:(fun queue -> Fqueue.to_list queue |> List.hd)
;;

let heads t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Fqueue.to_list t.key_order |> List.filter_map ~f:(head_for_key t))
;;

let without_key key_order key =
  filter_queue key_order ~f:(fun candidate -> Quota_key.compare candidate key <> 0)
;;

let remove_ticket queue session_id =
  filter_queue queue ~f:(fun ticket ->
    Agent_protocol.Id.Session.compare ticket.session_id session_id <> 0)
;;

let complete_locked t ticket =
  match Map.find t.queues ticket.quota_key with
  | None -> false
  | Some queue ->
    let remaining = remove_ticket queue ticket.session_id in
    if Fqueue.length remaining = Fqueue.length queue
    then false
    else (
      t.key_order <- without_key t.key_order ticket.quota_key;
      t.queues
      <- (if Fqueue.is_empty remaining
          then Map.remove t.queues ticket.quota_key
          else (
            t.key_order <- Fqueue.enqueue t.key_order ticket.quota_key;
            Map.set t.queues ~key:ticket.quota_key ~data:remaining));
      t.sessions <- Map.remove t.sessions ticket.session_id;
      true)
;;

let complete t ticket =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> complete_locked t ticket)
;;

let take_from_key t key ~eligible =
  match Map.find t.queues key with
  | None -> None
  | Some queue ->
    let tickets = Fqueue.to_list queue in
    (match List.findi tickets ~f:(fun _ ticket -> eligible ticket) with
     | None -> None
     | Some (index, ticket) ->
       let remaining = List.filteri tickets ~f:(fun candidate _ -> candidate <> index) in
       t.queues
       <- (if List.is_empty remaining
           then Map.remove t.queues key
           else Map.set t.queues ~key ~data:(Fqueue.of_list remaining));
       t.sessions <- Map.remove t.sessions ticket.session_id;
       Some ticket)
;;

let rec take_round t remaining ~eligible =
  if remaining = 0
  then None
  else (
    match Fqueue.dequeue t.key_order with
    | None -> None
    | Some (key, rest) ->
      t.key_order <- rest;
      let ticket = take_from_key t key ~eligible in
      if Map.mem t.queues key then t.key_order <- Fqueue.enqueue t.key_order key;
      (match ticket with
       | Some _ -> ticket
       | None -> take_round t (remaining - 1) ~eligible))
;;

let take_eligible t ~eligible =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    take_round t (Fqueue.length t.key_order) ~eligible)
;;

let length t = Eio.Mutex.use_ro t.mutex (fun () -> Map.length t.sessions)
