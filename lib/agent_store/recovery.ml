open Core

type 'state t =
  { state : 'state
  ; snapshot : Snapshot.installed option
  ; transactions : Transaction.t list
  ; latest_transaction_sequence : int64
  ; latest_transaction_hash : string option
  ; latest_session_revision : int64
  ; latest_event_sequence : int64
  ; repaired_crash_tail : bool
  }

type counters =
  { transaction_sequence : int64
  ; transaction_hash : string option
  ; session_revision : int64
  ; event_sequence : int64
  ; generation : int
  }

let same_session left right = Agent_protocol.Id.Session.compare left right = 0

let decode_entries entries =
  let decode entry =
    match Frame.flags entry.Journal.frame with
    | 0 -> Transaction.decode (Frame.payload entry.frame) |> Result.map ~f:Option.some
    | 1 -> Ok None
    | flags ->
      Error (Store_error.Corrupt (sprintf "unknown journal frame flags: %d" flags))
  in
  Result.all (List.map entries ~f:decode) |> Result.map ~f:List.filter_opt
;;

let event_sequence counters transaction =
  match transaction.Transaction.first_event_sequence, transaction.last_event_sequence with
  | None, None -> Ok counters.event_sequence
  | Some first, Some last ->
    if Int64.equal first Int64.(counters.event_sequence + one)
    then Ok last
    else Error (Store_error.Corrupt "durable event sequence is discontinuous")
  | _ -> Error (Store_error.Corrupt "durable event range is incomplete")
;;

let validate_transaction ~session_id counters transaction =
  let open Result.Let_syntax in
  if not (same_session transaction.Transaction.session_id session_id)
  then Error (Store_error.Corrupt "journal transaction belongs to another session")
  else if
    not
      (Int64.equal
         transaction.transaction_sequence
         Int64.(counters.transaction_sequence + one))
  then Error (Store_error.Corrupt "journal transaction sequence is discontinuous")
  else if
    not
      (Option.equal
         String.equal
         transaction.previous_transaction_hash
         counters.transaction_hash)
  then Error (Store_error.Corrupt "journal transaction hash chain is discontinuous")
  else if transaction.generation < counters.generation
  then Error (Store_error.Corrupt "journal transaction generation moved backwards")
  else if Int64.(transaction.session_revision < counters.session_revision)
  then Error (Store_error.Corrupt "session revision moved backwards")
  else (
    let%map event_sequence = event_sequence counters transaction in
    { transaction_sequence = transaction.transaction_sequence
    ; transaction_hash = Some (Transaction.hash transaction)
    ; session_revision = transaction.session_revision
    ; event_sequence
    ; generation = transaction.generation
    })
;;

let validate_chain ~session_id transactions =
  let initial =
    { transaction_sequence = Int64.zero
    ; transaction_hash = None
    ; session_revision = Int64.zero
    ; event_sequence = Int64.zero
    ; generation = 0
    }
  in
  List.fold_result transactions ~init:initial ~f:(validate_transaction ~session_id)
;;

let validate_snapshot_anchor ~session_id snapshot transaction =
  let open Result.Let_syntax in
  let%bind () = Transaction.validate transaction in
  if not (same_session transaction.Transaction.session_id session_id)
  then Error (Store_error.Corrupt "snapshot transaction belongs to another session")
  else if
    not
      (Int64.equal
         transaction.transaction_sequence
         snapshot.Snapshot.transaction_sequence)
  then Error (Store_error.Corrupt "snapshot transaction sequence differs from journal")
  else if
    not
      (Option.equal
         String.equal
         snapshot.transaction_hash
         (Some (Transaction.hash transaction)))
  then Error (Store_error.Corrupt "snapshot transaction hash differs from journal")
  else
    Ok
      { transaction_sequence = transaction.transaction_sequence
      ; transaction_hash = snapshot.transaction_hash
      ; session_revision = transaction.session_revision
      ; event_sequence = snapshot.event_sequence
      ; generation = transaction.generation
      }
;;

let validate_from_snapshot ~session_id installed transactions =
  let snapshot = installed.Snapshot.snapshot in
  if Int64.equal snapshot.transaction_sequence Int64.zero
  then validate_chain ~session_id transactions
  else (
    match
      List.findi transactions ~f:(fun _ transaction ->
        Int64.equal
          transaction.Transaction.transaction_sequence
          snapshot.transaction_sequence)
    with
    | None ->
      Error (Store_error.Corrupt "snapshot transaction is absent from retained journals")
    | Some (index, anchor) ->
      let open Result.Let_syntax in
      let%bind counters = validate_snapshot_anchor ~session_id snapshot anchor in
      List.drop transactions (index + 1)
      |> List.fold_result ~init:counters ~f:(validate_transaction ~session_id))
;;

let validate_recovery_chain ~session_id snapshot transactions =
  match snapshot with
  | None -> validate_chain ~session_id transactions
  | Some installed -> validate_from_snapshot ~session_id installed transactions
;;

let replay_from snapshot transactions =
  let covered =
    Option.value_map snapshot ~default:Int64.zero ~f:(fun installed ->
      installed.Snapshot.snapshot.transaction_sequence)
  in
  List.filter transactions ~f:(fun transaction ->
    Int64.(transaction.Transaction.transaction_sequence > covered))
;;

let restore_state ~snapshot ~initial ~restore_snapshot =
  match snapshot with
  | None -> Ok initial
  | Some installed -> restore_snapshot installed.Snapshot.snapshot.payload
;;

let repair_tail journal scan =
  match scan.Journal.crash_tail with
  | None -> Ok false
  | Some _ -> Result.map (Journal.repair_current_tail journal scan) ~f:(fun () -> true)
;;

let load
      ~env
      ~journal
      ~snapshot_directory
      ~max_snapshot_payload_length
      ~session_id
      ~initial
      ~restore_snapshot
      ~apply
      ~validate
  =
  let open Result.Let_syntax in
  let%bind snapshot =
    Snapshot.load_current
      ~env
      ~directory:snapshot_directory
      ~max_payload_length:max_snapshot_payload_length
  in
  let%bind scan = Journal.scan journal in
  let%bind repaired_crash_tail = repair_tail journal scan in
  let%bind transactions = decode_entries scan.entries in
  let%bind counters = validate_recovery_chain ~session_id snapshot transactions in
  let%bind state = restore_state ~snapshot ~initial ~restore_snapshot in
  let%bind state =
    List.fold_result (replay_from snapshot transactions) ~init:state ~f:apply
  in
  let%map () = validate state in
  { state
  ; snapshot
  ; transactions
  ; latest_transaction_sequence = counters.transaction_sequence
  ; latest_transaction_hash = counters.transaction_hash
  ; latest_session_revision = counters.session_revision
  ; latest_event_sequence = counters.event_sequence
  ; repaired_crash_tail
  }
;;
