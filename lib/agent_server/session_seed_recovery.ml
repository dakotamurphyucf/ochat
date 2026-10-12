open! Core

let protocol_of_store error =
  Agent_protocol.Error.create
    Persistence_error
    ~message:(Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
    ~retryable:false
    ()
;;

let unavailable code message =
  Agent_protocol.Error.create code ~message ~retryable:false ()
;;

let corrupt message = protocol_of_store (Agent_store.Store_error.Corrupt message)

type storage_limits =
  { max_journal_payload : int
  ; max_segment_bytes : int64
  ; max_segment_frames : int
  ; snapshot_payload_limit : int
  }

type t =
  { env : Eio_unix.Stdenv.base
  ; store : Agent_store.Session_store.t
  ; registry : Session_registry.t
  ; idempotency_store : Agent_store.Idempotency_store.t
  ; journal_document_limits : Document_schema.Limits.t
  ; storage_limits : storage_limits
  ; max_work_steps : int
  }

let create
      ~env
      ~store
      ~registry
      ~idempotency_store
      ~journal_document_limits
      ~storage_limits
      ~max_work_steps
  =
  { env
  ; store
  ; registry
  ; idempotency_store
  ; journal_document_limits
  ; storage_limits
  ; max_work_steps
  }
;;

module Limits = struct
  type t =
    { max_sessions : int
    ; max_transactions : int
    ; max_input_bytes : int
    ; max_work_steps : int
    }

  let create ~max_sessions ~max_transactions ~max_input_bytes ~max_work_steps =
    if
      max_sessions <= 0
      || max_transactions <= 0
      || max_input_bytes <= 0
      || max_work_steps <= 0
    then Error (unavailable Invalid_request "seed recovery limits must be positive")
    else Ok { max_sessions; max_transactions; max_input_bytes; max_work_steps }
  ;;

  let default =
    { max_sessions = 4096
    ; max_transactions = 4096
    ; max_input_bytes = 256 * 1024 * 1024
    ; max_work_steps = 2_000_000
    }
  ;;

  let max_sessions t = t.max_sessions
  let max_transactions t = t.max_transactions
  let max_input_bytes t = t.max_input_bytes
  let max_work_steps t = t.max_work_steps
end

let with_work ~max_work_steps ~checkpoint f =
  let module Invocation = struct
    exception Work_limit
  end
  in
  if max_work_steps <= 0
  then Error (unavailable Invalid_request "seed work bound must be positive")
  else (
    let steps = ref 0 in
    let poll () =
      if !steps >= max_work_steps then raise Invocation.Work_limit;
      Int.incr steps;
      Eio.Fiber.check ();
      checkpoint ();
      Eio.Fiber.check ()
    in
    try f poll with
    | Invocation.Work_limit ->
      Error (unavailable Invalid_state "seed work bound exceeded"))
;;

let seed_evidence t reader ~checkpoint =
  let open Result.Let_syntax in
  let%bind first =
    Agent_store.Journal.initial_entry
      reader
      ~env:t.env
      ~max_payload_length:t.storage_limits.max_journal_payload
      ~checkpoint
    |> Result.map_error ~f:protocol_of_store
  in
  match first with
  | None -> Ok None
  | Some first ->
    Session_seed_receipt.of_initial_frame
      first.Agent_store.Journal.frame
      ~limits:t.journal_document_limits
      ~checkpoint
    |> Result.map_error ~f:protocol_of_store
;;

let seed_record t evidence =
  let audit = Session_seed_receipt.audit evidence in
  match
    Agent_store.Idempotency_store.lookup
      t.idempotency_store
      ~key:audit.key
      ~request_digest:audit.request_digest
  with
  | Missing | Conflict _ ->
    Error (unavailable Persistence_error "seed Created has no matching protected receipt")
  | Replay record ->
    Session_seed_receipt.validate_receipt evidence record
    |> Result.map_error ~f:protocol_of_store
    |> Result.map ~f:(fun () -> record)
;;

let seed_frame_allowance t =
  let overhead = Agent_store.Frame.header_length + Agent_store.Frame.checksum_length in
  if
    t.storage_limits.max_journal_payload <= 0
    || t.storage_limits.max_journal_payload > Int.max_value - overhead
  then Error (unavailable Invalid_request "seed journal payload bound is invalid")
  else Ok (t.storage_limits.max_journal_payload + overhead)
;;

let require_success t handle =
  let open Result.Let_syntax in
  with_work ~max_work_steps:t.max_work_steps ~checkpoint:ignore (fun checkpoint ->
    let%bind max_bytes = seed_frame_allowance t in
    let%bind reader =
      Agent_store.Retention_reader.create
        ~env:t.env
        ~root:(Agent_store.Session_store.Handle.journal_directory handle)
        ~max_entries:8
        ~max_bytes
      |> Result.map_error ~f:protocol_of_store
    in
    let%bind evidence = seed_evidence t reader ~checkpoint in
    match evidence with
    | None -> Ok ()
    | Some evidence ->
      let%bind () =
        if
          Agent_protocol.Id.Session.equal
            (Session_seed_receipt.session_id evidence)
            (Agent_store.Session_store.Handle.session_id handle)
        then Ok ()
        else
          Error (unavailable Persistence_error "seed journal belongs to another session")
      in
      let%bind record = seed_record t evidence in
      (match record.Agent_store.Idempotency_store.outcome with
       | Success _ -> Ok ()
       | Pending ->
         Error (unavailable Invalid_state "seed creation acknowledgment is incomplete")
       | Failure _ ->
         Error (unavailable Persistence_error "seed creation has a rejected receipt")))
;;

let reconcile t ~limits ~checkpoint =
  let module Store = Agent_store in
  let module Receipt = Session_seed_receipt in
  let open Result.Let_syntax in
  let max_sessions = Limits.max_sessions limits in
  let max_transactions = Limits.max_transactions limits in
  let max_input_bytes = Limits.max_input_bytes limits in
  let max_work_steps = Limits.max_work_steps limits in
  if max_sessions <= 0 || max_transactions <= 0 || max_input_bytes <= 0
  then Error (unavailable Invalid_request "seed recovery limits must be positive")
  else
    with_work ~max_work_steps ~checkpoint (fun checkpoint ->
      let%bind entries =
        Store.Session_index.list_checked_bounded
          (Store.Session_store.session_index t.store)
          ~max_entries:max_sessions
          ~checkpoint
        |> Result.map_error ~f:protocol_of_store
      in
      let%bind reader =
        Store.Retention_reader.create
          ~env:t.env
          ~root:(Agent_store.Data_root.path (Store.Session_store.data_root t.store))
          ~max_entries:max_work_steps
          ~max_bytes:max_input_bytes
        |> Result.map_error ~f:protocol_of_store
      in
      let transactions = ref 0 in
      let with_handle ~repair session_id f =
        Session_registry.with_lifecycle t.registry session_id (fun reservation ->
          match Session_registry.Lifecycle_reservation.target reservation with
          | Loaded entry ->
            if repair
            then
              Error
                (unavailable Invalid_state "Pending seed has an already-published actor")
            else (
              match entry.Session_registry.store_handle with
              | None -> Error (corrupt "indexed durable owner has no Store handle")
              | Some handle -> f handle)
          | Indexed _ | Absent ->
            Eio.Switch.run (fun sw ->
              let%bind handle =
                Store.Session_store.open_session
                  t.store
                  ~sw
                  ~actor_lock_nonce:Agent_protocol.Id.Transaction.(create () |> to_string)
                  session_id
                |> Result.map_error ~f:protocol_of_store
              in
              Session_registry.with_recovery_handle
                t.registry
                ~store:t.store
                handle
                (fun () -> f handle)))
      in
      let journal_reader handle =
        Store.Retention_reader.at_root
          reader
          ~root:(Store.Session_store.Handle.journal_directory handle)
        |> Result.map_error ~f:protocol_of_store
      in
      let observed handle =
        let%bind owned = journal_reader handle in
        let%bind first =
          Store.Journal.initial_entry
            owned
            ~env:t.env
            ~max_payload_length:t.storage_limits.max_journal_payload
            ~checkpoint
          |> Result.map_error ~f:protocol_of_store
        in
        match first with
        | None -> Ok None
        | Some first ->
          if !transactions >= max_transactions
          then
            Error (unavailable Invalid_state "seed recovery transaction bound exceeded")
          else (
            Int.incr transactions;
            Receipt.of_initial_frame
              first.Store.Journal.frame
              ~limits:t.journal_document_limits
              ~checkpoint
            |> Result.map_error ~f:protocol_of_store)
      in
      (* Admit all identities before completing any receipt. Two layouts for one
       command are ambiguous even when their complete bodies happen to agree. *)
      let%bind _, pending =
        List.fold_result
          entries
          ~init:(Map.empty (module Store.Idempotency_store.Key), [])
          ~f:(fun (keys, pending) indexed ->
            checkpoint ();
            let session_id = indexed.Store.Session_index.Entry.session.id in
            with_handle ~repair:false session_id (fun handle ->
              let%bind evidence = observed handle in
              match evidence with
              | None -> Ok (keys, pending)
              | Some evidence ->
                let%bind () =
                  if
                    Agent_protocol.Id.Session.equal
                      session_id
                      (Receipt.session_id evidence)
                  then Ok ()
                  else Error (corrupt "seed journal has another identity")
                in
                let audit = Receipt.audit evidence in
                let%bind keys =
                  match Map.find keys audit.key with
                  | None -> Ok (Map.set keys ~key:audit.key ~data:session_id)
                  | Some original ->
                    if Agent_protocol.Id.Session.equal original session_id
                    then Ok keys
                    else
                      Error (corrupt "one seed command has multiple Created identities")
                in
                let%map record = seed_record t evidence in
                (match record.Store.Idempotency_store.outcome with
                 | Pending -> keys, evidence :: pending
                 | Success _ -> keys, pending
                 | Failure _ -> assert false)))
      in
      List.fold_result (List.rev pending) ~init:() ~f:(fun () previous ->
        checkpoint ();
        let%bind indexed =
          with_handle ~repair:true (Receipt.session_id previous) (fun handle ->
            let%bind owned = journal_reader handle in
            let%bind proof =
              Store.Journal.Initial_creation.inspect
                owned
                ~env:t.env
                ~max_payload_length:t.storage_limits.max_journal_payload
                ~checkpoint
              |> Result.map_error ~f:protocol_of_store
            in
            let%bind proof =
              Result.of_option
                proof
                ~error:(corrupt "Pending seed no longer has its first Created")
            in
            let%bind evidence =
              Receipt.of_initial_frame
                (Store.Journal.Initial_creation.entry proof).frame
                ~limits:t.journal_document_limits
                ~checkpoint
              |> Result.map_error ~f:protocol_of_store
            in
            let%bind evidence =
              Result.of_option
                evidence
                ~error:(corrupt "Pending seed first command changed")
            in
            let%bind () =
              if
                Agent_protocol.Id.Session.equal
                  (Receipt.session_id previous)
                  (Receipt.session_id evidence)
                && String.equal
                     (Receipt.transaction_hash previous)
                     (Receipt.transaction_hash evidence)
              then Ok ()
              else Error (corrupt "Pending seed evidence changed")
            in
            let%bind record = seed_record t evidence in
            match record.Store.Idempotency_store.outcome with
            | Success _ ->
              Ok
                (Session_projection.index_entry
                   (Agent_session.Session_persistence.Restored.state
                      (Receipt.restored evidence)))
            | Failure _ -> assert false
            | Pending ->
              let%bind _journal =
                Store.Journal.Initial_creation.open_journal
                  proof
                  ~max_segment_bytes:t.storage_limits.max_segment_bytes
                  ~max_segment_frames:t.storage_limits.max_segment_frames
                  ~checkpoint
                |> Result.map_error ~f:protocol_of_store
              in
              let%bind snapshot_reader =
                Store.Retention_reader.at_root
                  reader
                  ~root:(Store.Session_store.Handle.snapshot_directory handle)
                |> Result.map_error ~f:protocol_of_store
              in
              let state =
                Agent_session.Session_persistence.Restored.state
                  (Receipt.restored evidence)
              in
              let%bind _, _installed =
                Agent_session.Session_persistence.install_first_restored_snapshot_at
                  (Receipt.restored evidence)
                  ~env:t.env
                  ~reader:snapshot_reader
                  ~max_payload_length:t.storage_limits.snapshot_payload_limit
                  ~transaction_hash:(Some (Receipt.transaction_hash evidence))
                  ~checkpoint
                  state
                |> Result.map_error ~f:protocol_of_store
              in
              let%bind () =
                Store.Session_store.write_metadata
                  ~entry:(Session_projection.index_entry state)
                  t.store
                  handle
                  (Session_projection.metadata state)
                |> Result.map_error ~f:protocol_of_store
              in
              checkpoint ();
              let audit = Receipt.audit evidence in
              let%bind completed =
                Store.Idempotency_store.complete
                  t.idempotency_store
                  ~key:audit.key
                  ~request_digest:audit.request_digest
                  ~accepted_transaction_sequence:
                    (Some (Receipt.transaction_sequence evidence))
                  ~outcome:
                    (Success
                       (Agent_protocol.Session_seed.Result.to_json
                          (Receipt.result evidence)))
                |> Result.map_error ~f:protocol_of_store
              in
              let%map () =
                Receipt.validate_receipt evidence completed
                |> Result.map_error ~f:protocol_of_store
              in
              Session_projection.index_entry state)
        in
        (* The reservation must release before the registry can admit this
           stopped identity. Durable Success already owns the original result. *)
        Session_registry.index t.registry indexed;
        Ok ()))
;;
