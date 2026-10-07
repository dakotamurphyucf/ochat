open! Core

type replay =
  | Available of Agent_protocol.Event.Durable.t list
  | Snapshot_required

type t =
  { capacity : int
  ; mutex : Eio.Mutex.t
  ; mutable documents : Document_schema.Document.t Int64.Map.t
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

let validate_documents events documents =
  let seen = Hash_set.create (module Int64) in
  List.fold_result documents ~init:() ~f:(fun () document ->
    let event = Durable_event_document.value document in
    if Hash_set.mem seen event.sequence
    then Error (error "duplicate replay event document sequence")
    else (
      Hash_set.add seen event.sequence;
      match
        List.find events ~f:(fun candidate ->
          Int64.equal candidate.Agent_protocol.Event.Durable.sequence event.sequence)
      with
      | Some candidate
        when Document_schema.Json.equal
               (Agent_protocol.Event.Durable.to_json candidate)
               (Agent_protocol.Event.Durable.to_json event) -> Ok ()
      | _ -> Error (error "replay event document does not match its full event")))
;;

let create ?(documents = []) ~capacity events =
  let%bind.Result () = validate_documents events documents in
  if capacity <= 0
  then Error (error "durable event replay capacity must be positive")
  else if not (is_contiguous events)
  then Error (error "initial durable events are not contiguous")
  else (
    let events = retain capacity events in
    let retained =
      Int64.Set.of_list
        (List.map events ~f:(fun event -> event.Agent_protocol.Event.Durable.sequence))
    in
    let documents =
      List.filter documents ~f:(fun document ->
        Set.mem retained (Durable_event_document.value document).sequence)
    in
    Ok
      { capacity
      ; mutex = Eio.Mutex.create ()
      ; documents =
          List.fold documents ~init:Int64.Map.empty ~f:(fun values document ->
            Map.set
              values
              ~key:(Durable_event_document.value document).sequence
              ~data:(Durable_event_document.document document))
      ; events = retain capacity events
      ; changed = Eio.Promise.create ()
      })
;;

let append ?(documents = []) t appended =
  let%bind.Result () = validate_documents appended documents in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if not (is_contiguous (t.events @ appended))
    then Error (error "appended durable events are not contiguous")
    else if List.is_empty appended
    then Ok ()
    else (
      t.events <- retain t.capacity (t.events @ appended);
      t.documents
      <- List.fold documents ~init:t.documents ~f:(fun values document ->
           Map.set
             values
             ~key:(Durable_event_document.value document).sequence
             ~data:(Durable_event_document.document document));
      let retained =
        Int64.Set.of_list
          (List.map t.events ~f:(fun event -> event.Agent_protocol.Event.Durable.sequence))
      in
      t.documents <- Map.filter_keys t.documents ~f:(Set.mem retained);
      let _, notify = t.changed in
      t.changed <- Eio.Promise.create ();
      Eio.Promise.resolve notify ();
      Ok ()))
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
        let%bind limits =
          Persistence_codec.limits ~max_bytes:(Int.max 1 max_bytes)
          |> Result.map_error ~f:(fun value ->
            error (Sexp.to_string_hum (Document_schema.Error.sexp_of_t value)))
        in
        let%bind () = Durable_event_document.validate ~limits event in
        let json =
          match Map.find t.documents event.sequence with
          | Some document -> Document_schema.Document.json document
          | None -> Durable_event_document.to_jsonaf event
        in
        let encoded = Jsonaf.to_string json in
        match String.length encoded <= remaining with
        | false -> Error (error "retained replay window exceeds its byte budget")
        | true ->
          Agent_store.Document_fields.iter_strings json ~f:(fun value ->
            Agent_store.Blob_reference_scan.begin_root scan;
            Agent_store.Blob_reference_scan.feed scan value);
          Ok (remaining - String.length encoded))
    in
    Agent_store.Blob_reference_scan.references scan)
;;
