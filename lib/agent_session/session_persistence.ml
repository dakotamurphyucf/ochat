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

  let of_state_document state_document = { state_document; snapshot = None }
  let state t = Session_state_document.value t.state_document
  let state_document t = t.state_document

  let with_state t state =
    { t with state_document = Session_state_document.with_value t.state_document state }
  ;;
end

type t =
  { archive :
      Session_state.Compaction_archive.t -> D.Document.t -> (unit, P.Error.t) result
  ; pending_archive : (Pending_archive.t -> (unit, P.Error.t) result) option
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
      ~pending_archive
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
  ; pending_archive
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

let transaction t ~state_document ~command_audit previous transition =
  let open Result.Let_syntax in
  let%bind delta =
    Session_delta_document.create
      transition.Session_transition.delta
      ~limits:t.limits
      ~state_document:(Session_state_document.with_value state_document)
    |> Result.map_error ~f:document_error
  in
  let contains_withdrawal (state : Session_state.t) =
    Option.exists state.recovery_windows ~f:Recovery_window_ledger.has_withdrawals
  in
  let%bind delta =
    Session_storage_semantics.require_recovery_withdrawals
      (Session_delta_document.document delta)
      ~required:(contains_withdrawal previous || contains_withdrawal transition.state)
      ~limits:t.limits
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
      ~delta
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
  let%map state_document =
    Session_state_document.decode_after state_document ~limits payload
  in
  payload, state_document
;;

let snapshot_at_payload
      restored
      ~limits
      ~transaction_hash
      (state : Session_state.t)
      payload
  =
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
  match restored.Restored.snapshot with
  | None -> args (Store.Snapshot.create ~session_id:state.identity.session_id)
  | Some snapshot -> args (Store.Snapshot.update snapshot)
;;

let admit_recovery_storage
      t
      ~snapshot_payload_limit
      ~previous
      ~transition
      ~admitted
      transaction
  =
  let open Result.Let_syntax in
  let state = transition.Session_transition.state in
  if
    Option.equal phys_equal previous.Session_state.recovery_notes state.recovery_notes
    && Option.equal phys_equal previous.recovery_windows state.recovery_windows
  then Ok ()
  else (
    let%bind snapshot_payload_limit =
      match snapshot_payload_limit with
      | Some limit -> Ok limit
      | None ->
        Error (Store.Store_error.Corrupt "recovery writer has no bound snapshot profile")
    in
    let%bind limits =
      Persistence_codec.limits ~max_bytes:snapshot_payload_limit
      |> Result.map_error ~f:document_error
    in
    let%bind _ =
      snapshot_at_payload
        t.restored
        ~limits
        ~transaction_hash:(Some (Store.Transaction.hash transaction))
        state
        (Session_state_document.Admitted.document admitted)
    in
    if
      String.length (Store.Transaction.encode transaction)
      > Store.Commit_writer.max_payload_length t.writer
    then Error (document_error (D.Error.Limit_exceeded "recovery transaction payload"))
    else Ok ())
;;

let commit_with_profile
      t
      ~snapshot_payload_limit
      ?observation
      ~command_audit
      ~(previous : Session_state.t)
      transition
  =
  let open Result.Let_syntax in
  let%bind observation =
    Option.value_map observation ~default:(Ok None) ~f:(fun observation ->
      Session_commit_observation.Host.observe_entry
        observation
        ~previous
        ~candidate:transition
      |> Result.map ~f:Option.some)
  in
  let%bind admitted =
    Session_document_transition.admit_encoded
      t.restored.state_document
      ~delta:transition.Session_transition.delta
      ~next:transition.state
      ~limits:t.archive_limits
    |> Result.map_error ~f:(fun error -> protocol_error (document_error error))
  in
  (* Ordered admission already validated and encoded this exact original native
     candidate. Decode that immutable encoding once to materialize the complete
     next preservation basis. All admission and transaction validation still
     precedes archive/journal effects. *)
  let%bind state_document =
    Session_state_document.decode_after
      (Session_state_document.Admitted.state_document admitted)
      ~limits:t.archive_limits
      (Session_state_document.Admitted.document admitted)
    |> Result.map_error ~f:(fun e -> protocol_error (document_error e))
  in
  let next = { t.restored with state_document } in
  let%bind terminal =
    Session_state_document.terminal_capacity
      state_document
      ~document:(Session_state_document.Admitted.document admitted)
      ~limits:t.archive_limits
    |> Result.map_error ~f:(fun error -> protocol_error (document_error error))
  in
  let%bind recurrence =
    Session_state_document.recurrence_capacity
      state_document
      ~document:(Session_state_document.Admitted.document admitted)
      ~limits:t.archive_limits
    |> Result.map_error ~f:(fun error -> protocol_error (document_error error))
  in
  let%bind () =
    Session_terminal_journal.validate
      transition.state
      ~terminal
      ~recurrence:(Some recurrence)
      ~limits:t.limits
      ~max_payload_length:(Store.Commit_writer.max_payload_length t.writer)
  in
  let%bind transaction, events =
    transaction t ~state_document ~command_audit previous transition
    |> Result.map_error ~f:protocol_error
  in
  let%bind () =
    admit_recovery_storage
      t
      ~snapshot_payload_limit
      ~previous
      ~transition
      ~admitted
      transaction
    |> Result.map_error ~f:protocol_error
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
  let%bind pending_archives =
    Pending_archive_transition.collect
      previous
      ~delta:transition.delta
      ~limits:t.archive_limits
  in
  let%bind () =
    Option.value_map t.before_commit ~default:(Ok ()) ~f:(fun prepare ->
      prepare transition.state)
  in
  let%bind () =
    List.fold_result pending_archives ~init:() ~f:(fun () archive ->
      match t.pending_archive with
      | Some publish -> publish archive
      | None ->
        Error
          (P.Error.create
             Persistence_error
             ~message:"pending disposition archive publisher is unavailable"
             ~retryable:false
             ()))
  in
  let%bind () =
    List.fold_result references ~init:() ~f:(fun () reference ->
      match archive with
      | None -> assert false
      | Some document -> t.archive reference document)
  in
  let commit () =
    Store.Commit_writer.commit t.writer ~durability:t.durability transaction
    |> Result.map_error ~f:protocol_error
    |> Result.map ~f:(fun committed ->
      Session_commit_observation.Returned.
        { sequence = committed.transaction_sequence; hash = committed.transaction_hash })
  in
  let%map committed =
    match observation with
    | None -> commit ()
    | Some observation ->
      Session_commit_observation.Host.with_writer
        observation
        ~transaction:
          { session_id = transaction.session_id
          ; generation = transaction.generation
          ; sequence = transaction.transaction_sequence
          ; revision = transaction.session_revision
          ; previous_hash = transaction.previous_transaction_hash
          ; payload = Store.Transaction.encode transaction
          ; hash = Store.Transaction.hash transaction
          ; max_payload_length = Store.Commit_writer.max_payload_length t.writer
          }
        ~commit
  in
  t.previous_transaction_hash <- Some committed.hash;
  t.restored <- next;
  t.replay_documents <- t.replay_documents @ events;
  Option.iter command_audit ~f:(fun encoded ->
    t.command_accepted encoded committed.sequence)
;;

let commit t ?observation ~command_audit ~previous transition =
  commit_with_profile
    t
    ~snapshot_payload_limit:None
    ?observation
    ~command_audit
    ~previous
    transition
;;

let actor_persistence_with_profile t ~snapshot_payload_limit =
  Session_actor.
    { commit =
        (fun ?observation ~command_audit ~previous transition ->
          commit_with_profile
            t
            ~snapshot_payload_limit
            ?observation
            ~command_audit
            ~previous
            transition)
    ; archive_reference =
        (fun ~previous ~kind id -> archive_reference t ~previous ~kind id)
    }
;;

let actor_persistence t = actor_persistence_with_profile t ~snapshot_payload_limit:None

let actor_persistence_with_startup_disposal ?recovery_notes t =
  let persistence =
    actor_persistence_with_profile
      t
      ~snapshot_payload_limit:(Option.map recovery_notes ~f:snd)
  in
  let binding =
    Canonical_backend_binding_data.create
      ~writer:t.writer
      ~commit:persistence.commit
      ~recovery_note_owner:(Option.map recovery_notes ~f:fst)
  in
  persistence, binding
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

let prepare_restored_snapshot restored ~max_payload_length ~transaction_hash state =
  let open Result.Let_syntax in
  let%bind limits =
    Persistence_codec.limits ~max_bytes:max_payload_length
    |> Result.map_error ~f:document_error
  in
  let restored = Restored.with_state restored state in
  let%bind payload, state_document =
    admit_state_document restored.state_document ~limits
    |> Result.map_error ~f:document_error
  in
  let restored = { restored with state_document } in
  let%bind snapshot =
    snapshot_at_payload restored ~limits ~transaction_hash state payload
  in
  Ok (restored, snapshot)
;;

let install_restored_snapshot_at
      restored
      ~env
      ~directory
      ~max_payload_length
      ~transaction_hash
      state
  =
  let open Result.Let_syntax in
  let%bind restored, snapshot =
    prepare_restored_snapshot restored ~max_payload_length ~transaction_hash state
  in
  let%map installed =
    Store.Snapshot.install ~env ~directory ~max_payload_length snapshot
  in
  { restored with snapshot = Some installed.snapshot }, installed
;;

let install_first_restored_snapshot_at
      restored
      ~env
      ~reader
      ~max_payload_length
      ~transaction_hash
      ~checkpoint
      state
  =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind restored, snapshot =
    prepare_restored_snapshot restored ~max_payload_length ~transaction_hash state
  in
  let%map installed =
    Store.Snapshot.install_first_checkpoint
      snapshot
      ~env
      ~reader
      ~max_payload_length
      ~checkpoint
  in
  { restored with snapshot = Some installed.snapshot }, installed
;;

let install_snapshot_at t ~env ~directory ~max_payload_length ~transaction_hash state =
  let open Result.Let_syntax in
  let%map restored, installed =
    install_restored_snapshot_at
      t.restored
      ~env
      ~directory
      ~max_payload_length
      ~transaction_hash
      state
  in
  t.restored <- restored;
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
