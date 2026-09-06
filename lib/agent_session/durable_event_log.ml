open! Core

type replay =
  | Available of Agent_protocol.Event.Durable.t list
  | Snapshot_required

type t =
  { capacity : int
  ; mutex : Eio.Mutex.t
  ; mutable events : Agent_protocol.Event.Durable.t list
  }

let error message = Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()

let is_contiguous events =
  let rec loop previous = function
    | [] -> true
    | event :: rest ->
      Int64.equal event.Agent_protocol.Event.Durable.sequence Int64.(previous + 1L)
      && loop event.sequence rest
  in
  match events with
  | [] | [ _ ] -> true
  | (first : Agent_protocol.Event.Durable.t) :: rest -> loop first.sequence rest
;;

let retain capacity events =
  let excess = List.length events - capacity in
  if excess > 0 then List.drop events excess else events
;;

let create ~capacity events =
  if capacity <= 0
  then Error (error "durable event replay capacity must be positive")
  else if not (is_contiguous events)
  then Error (error "initial durable events are not contiguous")
  else Ok { capacity; mutex = Eio.Mutex.create (); events = retain capacity events }
;;

let append t appended =
  if not (List.is_empty appended)
  then
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.events <- retain t.capacity (t.events @ appended))
;;

let oldest events =
  List.hd events
  |> Option.map ~f:(fun (event : Agent_protocol.Event.Durable.t) -> event.sequence)
;;

let latest events =
  List.last events
  |> Option.map ~f:(fun (event : Agent_protocol.Event.Durable.t) -> event.sequence)
;;

let replay_events events ~after_sequence ~through_sequence =
  match oldest events with
  | None -> Snapshot_required
  | Some first when Int64.(after_sequence + 1L < first) -> Snapshot_required
  | Some _ ->
    Available
      (List.filter events ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
         Int64.(event.sequence > after_sequence && event.sequence <= through_sequence)))
;;

let replay t ~after_sequence ~through_sequence =
  Eio.Mutex.use_ro t.mutex (fun () ->
    if Int64.(after_sequence >= through_sequence)
    then Available []
    else replay_events t.events ~after_sequence ~through_sequence)
;;

let oldest_sequence t = Eio.Mutex.use_ro t.mutex (fun () -> oldest t.events)
let latest_sequence t = Eio.Mutex.use_ro t.mutex (fun () -> latest t.events)
