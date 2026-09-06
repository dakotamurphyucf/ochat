open Core

module Key = struct
  type t =
    { principal_id : Agent_protocol.Id.Principal.t
    ; session_id : Agent_protocol.Id.Session.t option
    ; method_name : string
    ; idempotency_key : Agent_protocol.Idempotency_key.t
    }
  [@@deriving compare, sexp]
end

module Command_audit = struct
  type t =
    { key : Key.t
    ; request_digest : string
    ; protected_record : bool
    }
  [@@deriving sexp]

  let encode t = Sexp.to_string_mach ([%sexp_of: t] t)

  let decode encoded =
    try Ok ([%of_sexp: t] (Sexp.of_string encoded)) with
    | exn ->
      Error
        (Store_error.Corrupt ("command audit receipt decode failed: " ^ Exn.to_string exn))
  ;;
end

type outcome =
  | Pending
  | Success of Jsonaf.t
  | Failure of Agent_protocol.Error.t

type retention =
  | Standard
  | Protected
[@@deriving compare, equal, sexp]

type record =
  { key : Key.t
  ; request_digest : string
  ; accepted_transaction_sequence : int64 option
  ; outcome : outcome
  ; created_at : Agent_protocol.Timestamp.t
  ; expires_at : Agent_protocol.Timestamp.t option
  ; retention : retention
  }

type lookup =
  | Missing
  | Replay of record
  | Conflict of record

module Persisted = struct
  type outcome =
    | Pending
    | Success of string
    | Failure of Agent_protocol.Error.t
  [@@deriving sexp]

  type record =
    { key : Key.t
    ; request_digest : string
    ; accepted_transaction_sequence : int64 option
    ; outcome : outcome
    ; created_at : Agent_protocol.Timestamp.t
    ; expires_at : Agent_protocol.Timestamp.t option
    ; retention : retention
    }
  [@@deriving sexp]

  type t =
    { version : int
    ; records : record list
    }
  [@@deriving sexp]
end

type t =
  { env : Eio_unix.Stdenv.base
  ; path : string
  ; mutex : Eio.Mutex.t
  ; mutable records : (Key.t, Persisted.record) Map.Poly.t
  }

let version = 1

let persist_outcome = function
  | Pending -> Persisted.Pending
  | Success json -> Persisted.Success (Jsonaf.to_string json)
  | Failure error -> Persisted.Failure error
;;

let restore_outcome = function
  | Persisted.Pending -> Ok Pending
  | Persisted.Success encoded ->
    (try Ok (Success (Jsonaf.of_string encoded)) with
     | exn ->
       Error
         (Store_error.Corrupt ("idempotency result JSON is invalid: " ^ Exn.to_string exn)))
  | Persisted.Failure error -> Ok (Failure error)
;;

let persist_record (record : record) =
  Persisted.
    { key = record.key
    ; request_digest = record.request_digest
    ; accepted_transaction_sequence = record.accepted_transaction_sequence
    ; outcome = persist_outcome record.outcome
    ; created_at = record.created_at
    ; expires_at = record.expires_at
    ; retention = record.retention
    }
;;

let restore_record (record : Persisted.record) =
  Result.map (restore_outcome record.Persisted.outcome) ~f:(fun outcome ->
    { key = record.key
    ; request_digest = record.request_digest
    ; accepted_transaction_sequence = record.accepted_transaction_sequence
    ; outcome
    ; created_at = record.created_at
    ; expires_at = record.expires_at
    ; retention = record.retention
    })
;;

let map_of_records records =
  List.fold records ~init:Map.Poly.empty ~f:(fun map record ->
    Map.set map ~key:record.key ~data:record)
;;

let save t records =
  let persisted = Persisted.{ version; records = Map.data records } in
  Durable_file.replace
    ~env:t.env
    ~durability:Flush_file_and_directory
    ~path:t.path
    (Sexp.to_string_mach ([%sexp_of: Persisted.t] persisted))
;;

let load ~env ~path =
  let open Result.Let_syntax in
  let%bind contents = Durable_file.load ~env ~path in
  try
    let persisted = [%of_sexp: Persisted.t] (Sexp.of_string contents) in
    if persisted.version > version
    then Error (Store_error.Schema_too_new persisted.version)
    else if persisted.version < version
    then Error (Store_error.Migration_required persisted.version)
    else
      Result.all (List.map persisted.records ~f:restore_record)
      |> Result.map ~f:(fun records ->
        map_of_records records |> Map.map ~f:persist_record)
  with
  | exn ->
    Error (Store_error.Corrupt ("idempotency store decode failed: " ^ Exn.to_string exn))
;;

let open_or_create ~env ~path =
  if not (Filename.is_absolute path)
  then
    Error
      (Store_error.Io
         { operation = "open idempotency store"; path; message = "path must be absolute" })
  else (
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let records = if Eio.Path.is_file file then load ~env ~path else Ok Map.Poly.empty in
    Result.bind records ~f:(fun records ->
      let t = { env; path; mutex = Eio.Mutex.create (); records } in
      if Eio.Path.is_file file then Ok t else Result.map (save t records) ~f:(fun () -> t)))
;;

let restore_record_exn record =
  match restore_record record with
  | Ok record -> record
  | Error error ->
    raise_s [%sexp "invalid cached idempotency record", (error : Store_error.t)]
;;

let lookup t ~key ~request_digest =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match Map.find t.records key with
    | None -> Missing
    | Some record when String.equal record.request_digest request_digest ->
      Replay (restore_record_exn record)
    | Some record -> Conflict (restore_record_exn record))
;;

let record t record =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Map.find t.records record.key with
    | Some existing when String.equal existing.request_digest record.request_digest ->
      Ok (restore_record_exn existing)
    | Some _ ->
      Error (Store_error.Corrupt "idempotency key conflicts with another request")
    | None ->
      let records = Map.set t.records ~key:record.key ~data:(persist_record record) in
      Result.map (save t records) ~f:(fun () ->
        t.records <- records;
        record))
;;

let complete t ~key ~request_digest ~accepted_transaction_sequence ~outcome =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Map.find t.records key with
    | None -> Error (Store_error.Corrupt "idempotency completion has no pending record")
    | Some existing when not (String.equal existing.request_digest request_digest) ->
      Error (Store_error.Corrupt "idempotency completion conflicts with another request")
    | Some ({ outcome = Success _ | Failure _; _ } as existing) ->
      Ok (restore_record_exn existing)
    | Some existing ->
      let accepted_transaction_sequence =
        Option.first_some
          accepted_transaction_sequence
          existing.accepted_transaction_sequence
      in
      let completed =
        { existing with accepted_transaction_sequence; outcome = persist_outcome outcome }
      in
      let records = Map.set t.records ~key ~data:completed in
      Result.map (save t records) ~f:(fun () ->
        t.records <- records;
        restore_record_exn completed))
;;

let mark_accepted t ~key ~request_digest ~transaction_sequence =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Map.find t.records key with
    | None -> Error (Store_error.Corrupt "accepted command has no idempotency record")
    | Some existing when not (String.equal existing.request_digest request_digest) ->
      Error (Store_error.Corrupt "accepted command conflicts with another request")
    | Some existing ->
      let accepted_transaction_sequence =
        match existing.accepted_transaction_sequence with
        | None -> Some transaction_sequence
        | Some current -> Some (Int64.max current transaction_sequence)
      in
      let accepted = { existing with accepted_transaction_sequence } in
      let records = Map.set t.records ~key ~data:accepted in
      Result.map (save t records) ~f:(fun () ->
        t.records <- records;
        restore_record_exn accepted))
;;

let reconcile_one
      (records : (Key.t, Persisted.record) Map.Poly.t)
      (audit : Command_audit.t)
      transaction_sequence
  =
  match Map.find records audit.key with
  | None when audit.protected_record ->
    Error (Store_error.Corrupt "protected journal command has no idempotency record")
  | None -> Ok (records, 0)
  | Some existing when not (String.equal existing.request_digest audit.request_digest) ->
    Error (Store_error.Corrupt "journal command conflicts with its idempotency record")
  | Some existing ->
    let accepted_transaction_sequence =
      match existing.accepted_transaction_sequence with
      | None -> Some transaction_sequence
      | Some current -> Some (Int64.max current transaction_sequence)
    in
    let changed =
      not
        (Option.equal
           Int64.equal
           existing.accepted_transaction_sequence
           accepted_transaction_sequence)
    in
    Ok
      ( Map.set
          records
          ~key:audit.key
          ~data:{ existing with accepted_transaction_sequence }
      , Bool.to_int changed )
;;

let reconcile_accepted t accepted =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let open Result.Let_syntax in
    let%bind records, changed =
      List.fold_result
        accepted
        ~init:(t.records, 0)
        ~f:(fun (records, changed) (audit, sequence) ->
          Result.map
            (reconcile_one records audit sequence)
            ~f:(fun (records, increment) -> records, changed + increment))
    in
    if changed = 0
    then Ok 0
    else
      Result.map (save t records) ~f:(fun () ->
        t.records <- records;
        changed))
;;

let is_expired ~now (record : Persisted.record) =
  match record.retention, record.expires_at with
  | Protected, _ | Standard, None -> false
  | Standard, Some expires_at -> Agent_protocol.Timestamp.compare expires_at now <= 0
;;

let prune_expired t ~now =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let records = Map.filter t.records ~f:(fun record -> not (is_expired ~now record)) in
    let removed = Map.length t.records - Map.length records in
    if removed = 0
    then Ok 0
    else
      Result.map (save t records) ~f:(fun () ->
        t.records <- records;
        removed))
;;
