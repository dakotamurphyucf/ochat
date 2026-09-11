open! Core

type replay =
  | Available of Agent_protocol.Event.Durable.t list
  | Snapshot_required

type t =
  { capacity : int
  ; mutex : Eio.Mutex.t
  ; mutable events : Agent_protocol.Event.Durable.t list
  ; mutable changed : unit Eio.Promise.t * unit Eio.Promise.u
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
  else
    Ok
      { capacity
      ; mutex = Eio.Mutex.create ()
      ; events = retain capacity events
      ; changed = Eio.Promise.create ()
      }
;;

let append t appended =
  if not (List.is_empty appended)
  then
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.events <- retain t.capacity (t.events @ appended);
      let _, notify = t.changed in
      t.changed <- Eio.Promise.create ();
      Eio.Promise.resolve notify ())
;;

let changed t = Eio.Mutex.use_ro t.mutex (fun () -> fst t.changed)

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

type history_epoch =
  | Replacement of int64
  | Window_start of int64
[@@deriving equal, sexp]

let history_epoch t ~through_sequence =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match oldest t.events, latest t.events with
    | Some first, _ when Int64.(first > through_sequence) ->
      Error
        (Agent_protocol.Error.create
           Snapshot_required
           ~message:"The history replay window moved beyond this output snapshot."
           ~retryable:false
           ~data:(`Object [ "snapshot_required", `True ])
           ())
    | _, None -> Ok (Window_start through_sequence)
    | _, Some last when Int64.(last < through_sequence) ->
      (* A restored snapshot may have no corresponding retained replay suffix.
         A later window change expires this conservative fresh-snapshot anchor. *)
      Ok (Window_start through_sequence)
    | first, Some _ ->
      let replacement =
        List.fold t.events ~init:None ~f:(fun found event ->
          match event.Agent_protocol.Event.Durable.kind with
          | History_replaced when Int64.(event.sequence <= through_sequence) ->
            Some event.sequence
          | _ -> found)
      in
      Ok
        (match replacement with
         | Some sequence -> Replacement sequence
         | None -> Window_start (Option.value first ~default:through_sequence)))
;;

let retained_references t ~session_id ~candidates ~max_events ~max_bytes =
  Eio.Mutex.use_ro t.mutex (fun () ->
    let open Result.Let_syntax in
    let%bind () =
      match
        max_events >= 0
        && max_bytes >= 0
        && List.length t.events <= max_events
        && is_contiguous t.events
      with
      | true -> Ok ()
      | false -> Error (error "retained replay window exceeds its limit or has a gap")
    in
    let%bind scan = Agent_store.Blob_reference_scan.create candidates in
    let%map _ =
      List.fold_result t.events ~init:max_bytes ~f:(fun remaining event ->
        let%bind () =
          match
            ( Agent_protocol.Id.Session.equal
                event.Agent_protocol.Event.Durable.session_id
                session_id
            , event.visibility )
          with
          | true, Full -> Ok ()
          | _ ->
            Error (error "retention requires the owning session's full replay events")
        in
        let%bind _ =
          Agent_protocol.Event.Durable.of_json
            (Agent_protocol.Event.Durable.to_json event)
        in
        let%bind _ =
          Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
        in
        let%bind _ = Agent_protocol.Event.Durable.extension_status event in
        let%bind _ = Agent_protocol.Event.Durable.replacement_snapshot event in
        let encoded =
          Agent_protocol.Event.Durable.sexp_of_t event |> Sexp.to_string_mach
        in
        match String.length encoded <= remaining with
        | false -> Error (error "retained replay window exceeds its byte budget")
        | true ->
          Agent_store.Blob_reference_scan.begin_root scan;
          Agent_store.Blob_reference_scan.feed scan encoded;
          Ok (remaining - String.length encoded))
    in
    Agent_store.Blob_reference_scan.references scan)
;;
