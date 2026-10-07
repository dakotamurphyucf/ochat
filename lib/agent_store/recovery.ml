open! Core
module F = Document_fields

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

let corrupt message = Error (Store_error.Corrupt message)

let generation value =
  if Int64.(value <= of_int Int.max_value)
  then Ok (Int64.to_int_exn value)
  else corrupt "stored generation exceeds host integer range"
;;

let decode_entries entries ~limits =
  let decode entry =
    match Frame.flags entry.Journal.frame with
    | 0 ->
      let open Result.Let_syntax in
      let%bind record =
        Document_record.of_frame entry.frame ~limits ~expected_digest:None
        |> Result.map_error ~f:F.record_error
      in
      let%map stored = Transaction.Stored.of_record record in
      Some stored
    | 1 -> Ok None
    | flags -> corrupt (sprintf "unknown journal frame flags: %d" flags)
  in
  Result.all (List.map entries ~f:decode) |> Result.map ~f:List.filter_opt
;;

let event_sequence counters (metadata : Transaction.Stored.metadata) =
  match metadata.first_event_sequence, metadata.last_event_sequence with
  | None, None -> Ok counters.event_sequence
  | Some first, Some last ->
    if
      Int64.(counters.event_sequence < max_value)
      && Int64.equal first Int64.(counters.event_sequence + one)
    then Ok last
    else corrupt "durable event sequence is discontinuous"
  | _ -> corrupt "durable event range is incomplete"
;;

let validate_transaction ~session_id counters transaction =
  let metadata = Transaction.Stored.metadata transaction in
  let open Result.Let_syntax in
  if not (String.equal metadata.session_id session_id)
  then corrupt "journal transaction belongs to another session"
  else if
    not
      (Int64.(counters.transaction_sequence < max_value)
       && Int64.equal
            metadata.transaction_sequence
            Int64.(counters.transaction_sequence + one))
  then corrupt "journal transaction sequence is discontinuous"
  else if
    not
      (Option.equal
         String.equal
         metadata.previous_transaction_hash
         counters.transaction_hash)
  then corrupt "journal transaction hash chain is discontinuous"
  else if Int64.(metadata.generation < of_int counters.generation)
  then corrupt "journal transaction generation moved backwards"
  else if Int64.(metadata.session_revision < counters.session_revision)
  then corrupt "session revision moved backwards"
  else (
    let%bind generation = generation metadata.generation in
    let%map event_sequence = event_sequence counters metadata in
    { transaction_sequence = metadata.transaction_sequence
    ; transaction_hash = Some (Transaction.Stored.digest transaction)
    ; session_revision = metadata.session_revision
    ; event_sequence
    ; generation
    })
;;

let initial =
  { transaction_sequence = 0L
  ; transaction_hash = None
  ; session_revision = 0L
  ; event_sequence = 0L
  ; generation = 0
  }
;;

let validate_chain ~session_id transactions =
  List.fold_result transactions ~init:initial ~f:(validate_transaction ~session_id)
;;

let validate_snapshot_anchor ~session_id snapshot transaction =
  let metadata = Transaction.Stored.metadata transaction in
  let snapshot = Snapshot.Stored.metadata snapshot in
  let open Result.Let_syntax in
  if
    not
      (String.equal metadata.session_id session_id
       && String.equal snapshot.session_id session_id)
  then corrupt "snapshot transaction belongs to another session"
  else if not (Int64.equal metadata.transaction_sequence snapshot.transaction_sequence)
  then corrupt "snapshot transaction sequence differs from journal"
  else if
    not
      (Option.equal
         String.equal
         snapshot.transaction_hash
         (Some (Transaction.Stored.digest transaction)))
  then corrupt "snapshot transaction hash differs from journal"
  else if
    not
      (Int64.equal metadata.session_revision snapshot.session_revision
       && Int64.equal metadata.generation snapshot.generation)
  then corrupt "snapshot state differs from its journal anchor"
  else (
    let%map generation = generation metadata.generation in
    { transaction_sequence = metadata.transaction_sequence
    ; transaction_hash = snapshot.transaction_hash
    ; session_revision = metadata.session_revision
    ; event_sequence = snapshot.event_sequence
    ; generation
    })
;;

let validate_from_snapshot ~session_id installed transactions =
  let snapshot = installed.Snapshot.stored in
  let metadata = Snapshot.Stored.metadata snapshot in
  if not (String.equal metadata.session_id session_id)
  then corrupt "snapshot belongs to another session"
  else if Int64.equal metadata.transaction_sequence 0L
  then
    if
      Int64.equal metadata.session_revision 0L
      && Int64.equal metadata.event_sequence 0L
      && Option.is_none metadata.transaction_hash
    then validate_chain ~session_id transactions
    else corrupt "initial retained snapshot has noninitial counters"
  else (
    match
      List.findi transactions ~f:(fun _ transaction ->
        Int64.equal
          (Transaction.Stored.metadata transaction).transaction_sequence
          metadata.transaction_sequence)
    with
    | None -> corrupt "snapshot transaction is absent from retained journals"
    | Some (index, anchor) ->
      let open Result.Let_syntax in
      let%bind counters = validate_snapshot_anchor ~session_id snapshot anchor in
      List.drop transactions (index + 1)
      |> List.fold_result ~init:counters ~f:(validate_transaction ~session_id))
;;

let validate_stored_retained ~session_id ~snapshots ~transactions =
  let session_id = Agent_protocol.Id.Session.to_string session_id in
  let open Result.Let_syntax in
  let%bind _ =
    List.fold_result
      transactions
      ~init:(None, None)
      ~f:(fun (previous, last_event) transaction ->
        let metadata = Transaction.Stored.metadata transaction in
        let%bind () =
          if String.equal metadata.session_id session_id
          then Ok ()
          else corrupt "retained transaction belongs to another session"
        in
        let%bind () =
          match previous with
          | None -> Ok ()
          | Some previous ->
            let previous_metadata = Transaction.Stored.metadata previous in
            if
              Int64.(previous_metadata.transaction_sequence < max_value)
              && Int64.equal
                   metadata.transaction_sequence
                   Int64.(previous_metadata.transaction_sequence + one)
              && Option.equal
                   String.equal
                   metadata.previous_transaction_hash
                   (Some (Transaction.Stored.digest previous))
              && Int64.(metadata.generation >= previous_metadata.generation)
              && Int64.(metadata.session_revision >= previous_metadata.session_revision)
            then Ok ()
            else corrupt "retained transaction chain is discontinuous"
        in
        let%bind () =
          match last_event, metadata.first_event_sequence with
          | Some previous, Some next ->
            if Int64.(previous < max_value) && Int64.equal next Int64.(previous + one)
            then Ok ()
            else corrupt "retained event sequence is discontinuous"
          | None, _ | Some _, None -> Ok ()
        in
        Ok (Some transaction, Option.first_some metadata.last_event_sequence last_event))
  in
  let anchor =
    List.max_elt snapshots ~compare:(fun a b ->
      Int64.compare
        (Snapshot.Stored.metadata a.Snapshot.stored).transaction_sequence
        (Snapshot.Stored.metadata b.Snapshot.stored).transaction_sequence)
  in
  let%bind head =
    match anchor with
    | None -> validate_chain ~session_id transactions
    | Some anchor -> validate_from_snapshot ~session_id anchor transactions
  in
  let%map () =
    List.fold_result snapshots ~init:() ~f:(fun () snapshot ->
      let%bind restored_head = validate_from_snapshot ~session_id snapshot transactions in
      if
        Int64.equal head.transaction_sequence restored_head.transaction_sequence
        && Option.equal String.equal head.transaction_hash restored_head.transaction_hash
        && Int64.equal head.session_revision restored_head.session_revision
        && Int64.equal head.event_sequence restored_head.event_sequence
        && Int.equal head.generation restored_head.generation
      then Ok ()
      else corrupt "fallback snapshots recover different journal heads")
  in
  head
;;

let validate_retained ~session_id ~snapshots ~transactions =
  let snapshots =
    List.map snapshots ~f:(fun (snapshot : Snapshot.installed) ->
      { Snapshot.filename = snapshot.filename
      ; stored = Snapshot.stored snapshot.snapshot
      })
  in
  let transactions = List.map transactions ~f:Transaction.stored in
  validate_stored_retained ~session_id ~snapshots ~transactions
;;

type 'state proven_snapshot =
  { filename : string
  ; original : Document_record.t
  ; covered : int64
  ; replayed : 'state
  }

type 'state replay_proof =
  { head : counters
  ; head_record : Document_record.t option
  ; snapshots : 'state proven_snapshot list
  }

type 'state replay_mode =
  | Sequential
  | Shared of ('state -> 'state -> (bool, Store_error.t) Result.t)
  | Certified of
      { equivalent : 'state -> 'state -> (bool, Store_error.t) Result.t
      ; previous : 'state replay_proof option
      }

let same_original_record left right =
  Int.equal (Document_record.flags left) (Document_record.flags right)
  && String.equal
       (Document_record.stored_digest left)
       (Document_record.stored_digest right)
  && String.equal (Document_record.stored_bytes left) (Document_record.stored_bytes right)
;;

let proof_head_reachable proof transactions =
  match proof.head.transaction_sequence, proof.head_record with
  | 0L, None -> Option.is_none proof.head.transaction_hash
  | _, None -> false
  | _, Some original ->
    List.exists transactions ~f:(fun transaction ->
      let metadata = Transaction.Stored.metadata transaction in
      Int64.equal metadata.transaction_sequence proof.head.transaction_sequence
      && Int64.equal metadata.session_revision proof.head.session_revision
      && Int64.equal metadata.generation (Int64.of_int proof.head.generation)
      && Option.equal
           String.equal
           (Some (Transaction.Stored.digest transaction))
           proof.head.transaction_hash
      && same_original_record original (Transaction.Stored.record transaction))
;;

let replay_shared ~equivalent ~transactions ~apply ~validate restored_snapshots =
  let open Result.Let_syntax in
  let restored_snapshots =
    List.sort restored_snapshots ~compare:(fun (_, left, _) (_, right, _) ->
      Int64.compare right left)
  in
  let%map completed =
    List.fold_result
      restored_snapshots
      ~init:[]
      ~f:(fun completed (filename, covered, original) ->
        let later =
          List.filter transactions ~f:(fun transaction ->
            Int64.(transaction.Transaction.transaction_sequence > covered))
        in
        let rec replay state later = function
          | [] -> List.fold_result later ~init:state ~f:apply
          | (_, anchor, restored, replayed) :: remaining ->
            let prefix, later =
              List.split_while later ~f:(fun transaction ->
                Int64.(transaction.Transaction.transaction_sequence <= anchor))
            in
            let%bind state = List.fold_result prefix ~init:state ~f:apply in
            let%bind same = equivalent state restored in
            if same then Ok replayed else replay state later remaining
        in
        (* Completed anchors are ordered from the closest newer checkpoint to the
         newest. Reuse requires its complete prefix, an exact domain/carrier
         equivalence proof, and that anchor's already successful later replay. *)
        let%bind state = replay original later completed in
        let%map () = validate state in
        (filename, covered, original, state) :: completed)
  in
  List.map completed ~f:(fun (filename, _, _, state) -> filename, state)
;;

let prepare
      ~env
      ~journal
      ~snapshot_directory
      ~max_snapshot_payload_length
      ~session_id
      ~initial
      ~restore_snapshot
      ~apply
      ~validate_transaction:validate_transaction_domain
      ~validate
      ~replay_mode
  =
  let open Result.Let_syntax in
  let%bind selected =
    Snapshot.load_current_stored
      ~env
      ~directory:snapshot_directory
      ~max_payload_length:max_snapshot_payload_length
  in
  let%bind snapshots =
    Snapshot.retained_stored
      ~env
      ~directory:snapshot_directory
      ~max_payload_length:max_snapshot_payload_length
      ~allow_incomplete:true
  in
  let%bind () =
    match selected, snapshots with
    | None, [] -> Ok ()
    | None, _ -> corrupt "retained snapshots have no usable current pointer"
    | Some _, _ -> Ok ()
  in
  let%bind scan = Journal.scan journal in
  let%bind transaction_limits =
    F.limits ~max_bytes:(Journal.max_payload_length journal) |> F.store
  in
  let%bind stored_transactions = decode_entries scan.entries ~limits:transaction_limits in
  let%bind counters =
    validate_stored_retained ~session_id ~snapshots ~transactions:stored_transactions
  in
  let%bind snapshot_limits = F.limits ~max_bytes:max_snapshot_payload_length |> F.store in
  (* All original-byte anchors are checked before any current-domain constructor. *)
  let%bind restored_snapshots =
    Result.all
      (List.map snapshots ~f:(fun installed ->
         let%bind snapshot =
           Snapshot.restore installed.Snapshot.stored ~limits:snapshot_limits
         in
         let%bind state = restore_snapshot snapshot in
         let%map () = validate state in
         installed.filename, { Snapshot.filename = installed.filename; snapshot }, state))
  in
  let%bind snapshot, state =
    match selected with
    | None -> Ok (None, initial)
    | Some selected ->
      (match
         List.find restored_snapshots ~f:(fun (filename, _, _) ->
           String.equal filename selected.filename)
       with
       | None -> corrupt "selected snapshot disappeared from retained preflight"
       | Some (_, snapshot, state) -> Ok (Some snapshot, state))
  in
  let%bind transactions =
    Result.all
      (List.map stored_transactions ~f:(fun stored ->
         let%bind transaction = Transaction.restore stored ~limits:transaction_limits in
         let%map () = validate_transaction_domain transaction in
         transaction))
  in
  let replay ~covered state =
    let later =
      List.filter transactions ~f:(fun transaction ->
        Int64.(transaction.Transaction.transaction_sequence > covered))
    in
    let%bind state = List.fold_result later ~init:state ~f:apply in
    let%map () = validate state in
    state
  in
  (* A usable selected checkpoint does not authorize repairing or retiring an
     unreplayable retained fallback. Apply every tail under the same domain and
     preservation contracts before any mutation. *)
  let%bind replayed_snapshots =
    match replay_mode with
    | Sequential ->
      Result.all
        (List.map restored_snapshots ~f:(fun (filename, installed, state) ->
           let%map state =
             replay ~covered:installed.Snapshot.snapshot.transaction_sequence state
           in
           filename, state))
    | Shared equivalent | Certified { equivalent; previous = None } ->
      let routes =
        List.map restored_snapshots ~f:(fun (filename, installed, state) ->
          filename, installed.Snapshot.snapshot.transaction_sequence, state)
      in
      replay_shared ~equivalent ~transactions ~apply ~validate routes
    | Certified { equivalent; previous = Some proof } ->
      (* Fresh framing, stored metadata, every anchor, current-domain restoration
         and transaction validation above are prerequisites for consulting a
         prior proof. A matching head record plus the freshly verified original
         hash chain binds every byte of the already replayed prefix. *)
      let reachable = proof_head_reachable proof stored_transactions in
      let routes =
        List.map restored_snapshots ~f:(fun (filename, installed, state) ->
          let covered = installed.Snapshot.snapshot.transaction_sequence in
          let original = Snapshot.Stored.record (Snapshot.stored installed.snapshot) in
          match
            List.find proof.snapshots ~f:(fun previous ->
              reachable
              && String.equal previous.filename filename
              && Int64.equal previous.covered covered
              && Int64.(covered <= proof.head.transaction_sequence)
              && same_original_record previous.original original)
          with
          | None -> filename, covered, state
          | Some previous -> filename, proof.head.transaction_sequence, previous.replayed)
      in
      (* Cached routes are anchored at the certified HEAD, not the original
         snapshot. Equal effective heads may share the newly appended suffix. *)
      replay_shared ~equivalent ~transactions ~apply ~validate routes
  in
  let%bind state =
    match snapshot with
    | None -> replay ~covered:0L state
    | Some installed ->
      List.Assoc.find replayed_snapshots installed.filename ~equal:String.equal
      |> Result.of_option
           ~error:
             (Store_error.Corrupt "selected snapshot disappeared from replay preflight")
  in
  let%bind proof =
    match replay_mode with
    | Sequential | Shared _ -> Ok None
    | Certified _ ->
      let%bind head_record =
        if Int64.equal counters.transaction_sequence 0L
        then Ok None
        else
          List.find stored_transactions ~f:(fun transaction ->
            Int64.equal
              (Transaction.Stored.metadata transaction).transaction_sequence
              counters.transaction_sequence)
          |> Result.of_option
               ~error:(Store_error.Corrupt "validated journal head disappeared")
          |> Result.map ~f:(fun transaction ->
            Some (Transaction.Stored.record transaction))
      in
      let%map snapshots =
        List.map snapshots ~f:(fun installed ->
          let%map replayed =
            List.Assoc.find
              replayed_snapshots
              installed.Snapshot.filename
              ~equal:String.equal
            |> Result.of_option
                 ~error:(Store_error.Corrupt "validated snapshot route disappeared")
          in
          { filename = installed.filename
          ; original = Snapshot.Stored.record installed.stored
          ; covered = (Snapshot.Stored.metadata installed.stored).transaction_sequence
          ; replayed
          })
        |> Result.all
      in
      Some { head = counters; head_record; snapshots }
  in
  Ok
    ( ( { state
        ; snapshot
        ; transactions
        ; latest_transaction_sequence = counters.transaction_sequence
        ; latest_transaction_hash = counters.transaction_hash
        ; latest_session_revision = counters.session_revision
        ; latest_event_sequence = counters.event_sequence
        ; repaired_crash_tail = false
        }
      , scan )
    , proof )
;;

let preflight
      ~env
      ~journal
      ~snapshot_directory
      ~max_snapshot_payload_length
      ~session_id
      ~initial
      ~restore_snapshot
      ~apply
      ~validate_transaction
      ~validate
  =
  prepare
    ~env
    ~journal
    ~snapshot_directory
    ~max_snapshot_payload_length
    ~session_id
    ~initial
    ~restore_snapshot
    ~apply
    ~validate_transaction
    ~validate
    ~replay_mode:Sequential
  |> Result.map ~f:(fun _ -> ())
;;

let preflight_shared
      ~env
      ~journal
      ~snapshot_directory
      ~max_snapshot_payload_length
      ~session_id
      ~initial
      ~restore_snapshot
      ~apply
      ~equivalent
      ~validate_transaction
      ~validate
  =
  prepare
    ~env
    ~journal
    ~snapshot_directory
    ~max_snapshot_payload_length
    ~session_id
    ~initial
    ~restore_snapshot
    ~apply
    ~validate_transaction
    ~validate
    ~replay_mode:(Shared equivalent)
  |> Result.map ~f:(fun _ -> ())
;;

module Retention_preflight = struct
  type 'state owner =
    { env : Eio_unix.Stdenv.base
    ; journal : Journal.t
    ; snapshot_directory : string
    ; max_snapshot_payload_length : int
    ; session_id : Agent_protocol.Id.Session.t
    ; initial : 'state
    ; restore_snapshot : Snapshot.t -> ('state, Store_error.t) Result.t
    ; apply : 'state -> Transaction.t -> ('state, Store_error.t) Result.t
    ; equivalent : 'state -> 'state -> (bool, Store_error.t) Result.t
    ; validate_transaction : Transaction.t -> (unit, Store_error.t) Result.t
    ; validate : 'state -> (unit, Store_error.t) Result.t
    ; mutex : Eio.Mutex.t
    ; mutable proof : 'state replay_proof option
    }

  type t = Owner : 'state owner -> t

  let create
        ~env
        ~journal
        ~snapshot_directory
        ~max_snapshot_payload_length
        ~session_id
        ~initial
        ~restore_snapshot
        ~apply
        ~equivalent
        ~validate_transaction
        ~validate
    =
    Owner
      { env
      ; journal
      ; snapshot_directory
      ; max_snapshot_payload_length
      ; session_id
      ; initial
      ; restore_snapshot
      ; apply
      ; equivalent
      ; validate_transaction
      ; validate
      ; mutex = Eio.Mutex.create ()
      ; proof = None
      }
  ;;

  let check (Owner owner) =
    Eio.Mutex.use_rw ~protect:false owner.mutex (fun () ->
      let open Result.Let_syntax in
      let%map _, proof =
        prepare
          ~env:owner.env
          ~journal:owner.journal
          ~snapshot_directory:owner.snapshot_directory
          ~max_snapshot_payload_length:owner.max_snapshot_payload_length
          ~session_id:owner.session_id
          ~initial:owner.initial
          ~restore_snapshot:owner.restore_snapshot
          ~apply:owner.apply
          ~validate_transaction:owner.validate_transaction
          ~validate:owner.validate
          ~replay_mode:
            (Certified { equivalent = owner.equivalent; previous = owner.proof })
      in
      owner.proof <- proof)
  ;;
end

let load
      ~env
      ~journal
      ~snapshot_directory
      ~max_snapshot_payload_length
      ~session_id
      ~initial
      ~restore_snapshot
      ~apply
      ~validate_transaction
      ~validate
  =
  let open Result.Let_syntax in
  let%bind (recovered, scan), _ =
    prepare
      ~env
      ~journal
      ~snapshot_directory
      ~max_snapshot_payload_length
      ~session_id
      ~initial
      ~restore_snapshot
      ~apply
      ~validate_transaction
      ~validate
      ~replay_mode:Sequential
  in
  (* Physical crash-tail repair is the final effect, after complete restoration. *)
  let%map repaired_crash_tail =
    match scan.Journal.crash_tail with
    | None -> Ok false
    | Some _ -> Journal.repair_current_tail journal scan |> Result.map ~f:(fun () -> true)
  in
  { recovered with repaired_crash_tail }
;;
