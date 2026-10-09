open! Core
module Store = Agent_store
module D = Document_schema
module P = Agent_protocol

module Restored = struct
  type t =
    { state_document : Session_state_document.t
    ; snapshot : Store.Snapshot.t option
    }

  let authored state =
    { state_document = Session_state_document.authored state; snapshot = None }
  ;;

  let state t = Session_state_document.value t.state_document
  let state_document t = t.state_document

  let with_state t state =
    { t with state_document = Session_state_document.with_value t.state_document state }
  ;;
end

type t =
  { archive :
      Session_state.Compaction_archive.t -> D.Document.t -> (unit, P.Error.t) result
  ; command_accepted : D.Document.t -> int64 -> unit
  ; before_commit : (Session_state.t -> (unit, P.Error.t) result) option
  ; writer : Store.Commit_writer.t
  ; durability : Store.Journal_segment.durability
  ; limits : D.Limits.t
  ; archive_limits : D.Limits.t
  ; retention_preflight : Store.Recovery.Retention_preflight.t option
  ; mutable restored : Restored.t
  ; mutable replay_documents : Durable_event_document.t list
  ; mutable previous_transaction_hash : string option
  }

let create
      ~archive
      ~before_commit
      ~command_accepted
      ~writer
      ~durability
      ~limits
      ~archive_limits
      ~retention_preflight
      ~restored
      ~previous_transaction_hash
  =
  { archive
  ; command_accepted
  ; before_commit
  ; writer
  ; durability
  ; limits
  ; archive_limits
  ; retention_preflight
  ; restored
  ; replay_documents = []
  ; previous_transaction_hash
  }
;;

let protocol_error error =
  P.Error.create
    Persistence_error
    ~message:(Sexp.to_string_hum (Store.Store_error.sexp_of_t error))
    ~retryable:true
    ()
;;

let document_error error = Store.Store_error.Document error

let event_range events =
  match events with
  | [] -> None, None
  | first :: rest ->
    let last = List.last rest |> Option.value ~default:first in
    Some first.P.Event.Durable.sequence, Some last.sequence
;;

let accepted_at_ns state =
  state.Session_state.identity.updated_at
  |> P.Timestamp.to_time_ns
  |> Time_ns.to_int_ns_since_epoch
  |> Int64.of_int
;;

let transaction t ~command_audit previous transition =
  let open Result.Let_syntax in
  let%bind delta =
    Session_delta_document.create
      transition.Session_transition.delta
      ~limits:t.limits
      ~state_document:(Session_state_document.with_value t.restored.state_document)
    |> Result.map_error ~f:document_error
  in
  let%bind events =
    List.map transition.events ~f:(fun event ->
      Durable_event_document.create event ~limits:t.limits)
    |> Result.all
    |> Result.map_error ~f:document_error
  in
  let first_event_sequence, last_event_sequence = event_range transition.events in
  let%map transaction =
    Store.Transaction.create
      ~limits:t.limits
      ~session_id:previous.Session_state.identity.session_id
      ~generation:transition.state.identity.generation
      ~transaction_sequence:transition.state.counters.transaction_sequence
      ~previous_transaction_hash:t.previous_transaction_hash
      ~session_revision:transition.state.counters.revision
      ~first_event_sequence
      ~last_event_sequence
      ~accepted_at_ns:(accepted_at_ns transition.state)
      ~command_audit
      ~delta:(Session_delta_document.document delta)
      ~durable_events:(List.map events ~f:Durable_event_document.document)
  in
  transaction, events
;;

let archive_reference t ~previous ~kind operation_id =
  Compaction_archive.reference_for
    (Session_state_document.with_value t.restored.state_document previous)
    ~limits:t.archive_limits
    ~kind
    operation_id
;;

let admit_state_document state_document ~limits =
  let open Result.Let_syntax in
  (* Encode validates the original native value before any normalization. Keep
     the complete admitted layout as the next preservation basis, just as
     journal replay does, including newly present optional fields. *)
  let%bind payload = Session_state_document.encode state_document ~limits in
  let%map state_document = Session_state_document.decode ~limits payload in
  payload, state_document
;;

let commit t ~command_audit ~(previous : Session_state.t) transition =
  let open Result.Let_syntax in
  let%bind state_document =
    History_retirement.admit
      t.restored.state_document
      ~delta:transition.Session_transition.delta
      ~next:transition.state
      ~limits:t.archive_limits
    |> Result.map_error ~f:(fun error -> protocol_error (document_error error))
  in
  let next = { t.restored with state_document } in
  (* Validate the complete merge and immutable transaction before any archive or
    journal write. Unknown-bearing deletion fails with all durable owners intact. *)
  let%bind _, state_document =
    admit_state_document next.state_document ~limits:t.archive_limits
    |> Result.map_error ~f:(fun e -> protocol_error (document_error e))
  in
  let next = { next with state_document } in
  let%bind transaction, events =
    transaction t ~command_audit previous transition |> Result.map_error ~f:protocol_error
  in
  let references =
    List.filter transition.state.conversation.compaction_archives ~f:(fun reference ->
      not
        (List.exists previous.conversation.compaction_archives ~f:(fun old ->
           Int64.equal old.revision reference.revision)))
  in
  let%bind archive =
    match references with
    | [] -> Ok None
    | _ ->
      Compaction_archive.archive_document
        (Session_state_document.with_value t.restored.state_document previous)
        ~limits:t.archive_limits
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:(fun e -> protocol_error (document_error e))
  in
  let%bind () =
    Option.value_map t.before_commit ~default:(Ok ()) ~f:(fun prepare ->
      prepare transition.state)
  in
  let%bind () =
    List.fold_result references ~init:() ~f:(fun () reference ->
      match archive with
      | None -> assert false
      | Some document -> t.archive reference document)
  in
  let%map committed =
    Store.Commit_writer.commit t.writer ~durability:t.durability transaction
    |> Result.map_error ~f:protocol_error
  in
  t.previous_transaction_hash <- Some committed.transaction_hash;
  t.restored <- next;
  t.replay_documents <- t.replay_documents @ events;
  Option.iter command_audit ~f:(fun encoded ->
    t.command_accepted encoded committed.transaction_sequence)
;;

let actor_persistence t =
  Session_actor.
    { commit =
        (fun ~command_audit ~previous transition ->
          commit t ~command_audit ~previous transition)
    ; archive_reference =
        (fun ~previous ~kind id -> archive_reference t ~previous ~kind id)
    }
;;

let transaction_hash t = t.previous_transaction_hash
let restored t = t.restored
let retention_preflight t = t.retention_preflight

let restore_replay_documents t documents =
  t.replay_documents <- documents @ t.replay_documents
;;

let take_replay_documents t =
  let documents = t.replay_documents in
  t.replay_documents <- [];
  documents
;;

let install_snapshot_at t ~env ~directory ~max_payload_length ~transaction_hash state =
  let open Result.Let_syntax in
  let%bind limits =
    Persistence_codec.limits ~max_bytes:max_payload_length
    |> Result.map_error ~f:document_error
  in
  let restored = Restored.with_state t.restored state in
  let%bind payload, state_document =
    admit_state_document restored.state_document ~limits
    |> Result.map_error ~f:document_error
  in
  let restored = { restored with state_document } in
  let args create =
    create
      ~limits
      ~transaction_sequence:state.Session_state.counters.transaction_sequence
      ~transaction_hash
      ~event_sequence:state.counters.event_sequence
      ~created_at:state.identity.updated_at
      ~prompt_artifact:(P.Id.Prompt_revision.to_string state.spec.prompt_revision_id)
      ~workspace_identity:state.spec.workspace_instance.conflict_domain
      ~payload
  in
  let%bind snapshot =
    match restored.snapshot with
    | None -> args (Store.Snapshot.create ~session_id:state.identity.session_id)
    | Some snapshot -> args (Store.Snapshot.update snapshot)
  in
  let%map installed =
    Store.Snapshot.install ~env ~directory ~max_payload_length snapshot
  in
  t.restored <- { restored with snapshot = Some snapshot };
  installed
;;

let install_snapshot t ~env ~handle =
  install_snapshot_at
    t
    ~env
    ~directory:(Store.Session_store.Handle.snapshot_directory handle)
;;

let restore_snapshot ~limits snapshot =
  let open Result.Let_syntax in
  let%map state_document =
    Session_state_document.decode ~limits snapshot.Store.Snapshot.payload
    |> Result.map_error ~f:document_error
  in
  ({ state_document; snapshot = Some snapshot } : Restored.t)
;;

let timestamp_of_ns value =
  match Int64.to_int value with
  | None ->
    Error (Store.Store_error.Corrupt "transaction timestamp exceeds platform range")
  | Some value -> Ok (Time_ns.of_int_ns_since_epoch value |> P.Timestamp.of_time_ns)
;;

let event_documents ~limits transaction =
  let open Result.Let_syntax in
  let%bind documents =
    List.map
      transaction.Store.Transaction.durable_events
      ~f:(Durable_event_document.decode ~limits)
    |> Result.all
    |> Result.map_error ~f:document_error
  in
  let events = List.map documents ~f:Durable_event_document.value in
  let first, last = event_range events in
  let%bind () =
    if
      Option.equal Int64.equal first transaction.first_event_sequence
      && Option.equal Int64.equal last transaction.last_event_sequence
      && (let rec contiguous previous = function
            | [] -> true
            | event :: rest ->
              Int64.equal event.P.Event.Durable.sequence Int64.(previous + 1L)
              && contiguous event.sequence rest
          in
          match events with
          | [] -> true
          | first :: rest -> contiguous first.sequence rest)
      && List.for_all events ~f:(fun event ->
        P.Id.Session.equal event.P.Event.Durable.session_id transaction.session_id
        && Int64.equal event.revision transaction.session_revision)
    then Ok ()
    else
      Error
        (Store.Store_error.Corrupt
           "durable event owner, revision or sequence range mismatch")
  in
  Ok documents
;;

let durable_events ~limits transaction =
  event_documents ~limits transaction
  |> Result.map ~f:(List.map ~f:Durable_event_document.value)
;;

let validate_transaction ~limits transaction =
  let open Result.Let_syntax in
  let%bind delta =
    Session_delta_document.decode ~limits transaction.Store.Transaction.delta
    |> Result.map_error ~f:document_error
  in
  let rec owners = function
    | Session_delta.Batch changes ->
      List.fold_result changes ~init:() ~f:(fun () change -> owners change)
    | Created state ->
      if
        P.Id.Session.equal state.identity.session_id transaction.session_id
        && Int.equal state.identity.generation transaction.generation
      then Ok ()
      else
        Error (Store.Store_error.Corrupt "created state belongs to another transaction")
    | _ -> Ok ()
  in
  let%bind () = owners (Session_delta_document.value delta) in
  let%bind _ = durable_events ~limits transaction in
  match transaction.command_audit with
  | None -> Ok ()
  | Some audit ->
    Store.Idempotency_store.Command_audit.decode audit |> Result.map ~f:ignore
;;

let apply_document state_document ~limits transaction =
  let open Result.Let_syntax in
  let%bind () = validate_transaction ~limits transaction in
  let%bind delta =
    Session_delta_document.decode ~limits transaction.Store.Transaction.delta
    |> Result.map_error ~f:document_error
  in
  let%bind updated_at = timestamp_of_ns transaction.accepted_at_ns in
  let%bind transaction_metadata =
    Session_delta_document.Transaction_metadata.create
      ~updated_at
      ~revision:transaction.session_revision
      ~transaction_sequence:transaction.transaction_sequence
      ~last_event_sequence:transaction.last_event_sequence
    |> Result.map_error ~f:document_error
  in
  let%bind state_document =
    Session_delta_document.apply delta ~transaction_metadata ~limits state_document
    |> Result.map_error ~f:document_error
  in
  let state = Session_state_document.value state_document in
  if
    P.Id.Session.equal state.identity.session_id transaction.session_id
    && Int.equal state.identity.generation transaction.generation
  then Ok state_document
  else Error (Store.Store_error.Corrupt "replayed state differs from transaction owner")
;;

let apply_transaction ~limits (restored : Restored.t) transaction =
  let open Result.Let_syntax in
  let%map state_document = apply_document restored.state_document ~limits transaction in
  { restored with state_document }
;;

let validate restored =
  Session_state.validate (Restored.state restored)
  |> Result.map_error ~f:(fun e -> Store.Store_error.Corrupt e.P.Error.message)
;;
