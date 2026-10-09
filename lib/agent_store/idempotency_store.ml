open! Core
module D = Document_schema
module F = Document_fields

module Key = struct
  module T = struct
    type t =
      { principal_id : Agent_protocol.Id.Principal.t
      ; session_id : Agent_protocol.Id.Session.t option
      ; method_name : string
      ; idempotency_key : Agent_protocol.Idempotency_key.t
      }
    [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

let key_json (key : Key.t) =
  `Object
    [ "principal_id", Agent_protocol.Id.Principal.to_json key.principal_id
    ; "session_id", F.option_json key.session_id ~f:Agent_protocol.Id.Session.to_json
    ; "method_name", `String key.method_name
    ; "idempotency_key", Agent_protocol.Idempotency_key.to_json key.idempotency_key
    ]
;;

let key_decode json =
  let open Result.Let_syntax in
  let%bind principal_id =
    F.required json "principal_id" (fun json ->
      Agent_protocol.Id.Principal.of_json json |> F.protocol)
  in
  let%bind session_id =
    F.optional json "session_id" (fun json ->
      Agent_protocol.Id.Session.of_json json |> F.protocol)
  in
  let%bind method_name = F.required json "method_name" F.string in
  let%bind idempotency_key =
    F.required json "idempotency_key" (fun json ->
      Agent_protocol.Idempotency_key.of_json json |> F.protocol)
  in
  let%map () =
    if String.is_empty method_name
    then F.invalid "method_name" "must be nonempty"
    else Ok ()
  in
  Key.{ principal_id; session_id; method_name; idempotency_key }
;;

let key_shape =
  F.shape
    (List.map
       [ "principal_id"; "session_id"; "method_name"; "idempotency_key" ]
       ~f:(fun name -> name, D.Shape.value))
;;

module Command_audit = struct
  type t =
    { key : Key.t
    ; request_digest : string
    ; protected_record : bool
    }
  [@@deriving sexp]

  let kind = "session.command_audit"

  let codec =
    let decode json =
      let open Result.Let_syntax in
      let%bind key = F.required json "key" key_decode in
      let%bind request_digest = F.required json "request_digest" F.string in
      let%map protected_record = F.required json "protected_record" F.boolean in
      { key; request_digest; protected_record }
    in
    let encode value =
      Ok
        (`Object
            [ "key", key_json value.key
            ; "request_digest", `String value.request_digest
            ; ("protected_record", if value.protected_record then `True else `False)
            ])
    in
    match
      D.Domain_codec.create
        ~limits:D.Limits.default
        ~kind
        ~version:1
        ~shape:
          (F.shape
             [ "key", key_shape
             ; "request_digest", D.Shape.value
             ; "protected_record", D.Shape.value
             ])
        ~supported_semantics:[]
        ~decode
        ~encode
    with
    | Ok codec -> codec
    | Error error ->
      raise_s [%sexp "invalid static command audit codec", (error : D.Error.t)]
  ;;

  let restore document =
    let open Result.Let_syntax in
    let%bind document = F.upgrade document ~limits:D.Limits.default ~kind |> F.store in
    D.Domain_codec.decode codec document |> F.store
  ;;

  let encode_carrier carrier = D.Domain_codec.encode codec carrier |> F.store
  let encode value = encode_carrier (D.Extension_carrier.of_authored_value value)
  let decode document = restore document |> Result.map ~f:D.Extension_carrier.value
end

let cache_limits =
  match F.limits ~max_bytes:(D.Limits.max_bytes D.Limits.default) with
  | Ok limits -> limits
  | Error error -> raise_s [%sexp "invalid static cache limits", (error : D.Error.t)]
;;

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
end

type t =
  { env : Eio_unix.Stdenv.base
  ; path : string
  ; mutex : Eio.Mutex.t
  ; mutable records : (Key.t, Persisted.record, Key.comparator_witness) Map.t
  ; mutable carrier : Persisted.record list D.Extension_carrier.t
  }

let version = 2

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

let record_id (key : Key.t) = Document_record.digest (Jsonaf.to_string (key_json key))

let persisted_json (record : Persisted.record) =
  let open Result.Let_syntax in
  let%map outcome =
    match record.outcome with
    | Pending -> Ok (`Object [ "tag", `String "pending" ])
    | Success encoded ->
      let%map value = D.Json.decode ~limits:cache_limits encoded in
      `Object [ "tag", `String "success"; "value", value ]
    | Failure error ->
      Ok
        (`Object [ "tag", `String "failure"; "error", Agent_protocol.Error.to_json error ])
  in
  `Object
    [ "record_id", `String (record_id record.key)
    ; "key", key_json record.key
    ; "request_digest", `String record.request_digest
    ; ( "accepted_transaction_sequence"
      , F.option_json record.accepted_transaction_sequence ~f:F.decimal_json )
    ; "outcome", outcome
    ; "created_at", Agent_protocol.Timestamp.to_json record.created_at
    ; "expires_at", F.option_json record.expires_at ~f:Agent_protocol.Timestamp.to_json
    ; ( "retention"
      , `String
          (match record.retention with
           | Standard -> "standard"
           | Protected -> "protected") )
    ]
;;

let decode_persisted json =
  let open Result.Let_syntax in
  let%bind key = F.required json "key" key_decode in
  let%bind identity = F.required json "record_id" F.digest in
  let%bind () =
    if String.equal identity (record_id key)
    then Ok ()
    else F.invalid "record_id" "does not match owned key"
  in
  let%bind request_digest = F.required json "request_digest" F.string in
  let%bind accepted_transaction_sequence =
    F.optional json "accepted_transaction_sequence" F.decimal
  in
  let%bind outcome =
    F.required json "outcome" (fun json ->
      let%bind tag = F.required json "tag" F.string in
      match tag with
      | "pending" -> Ok Persisted.Pending
      | "success" ->
        let%map value = F.required json "value" Result.return in
        Persisted.Success (Jsonaf.to_string value)
      | "failure" ->
        let%map error =
          F.required json "error" (fun value ->
            Agent_protocol.Error.of_json value |> F.protocol)
        in
        Persisted.Failure error
      | _ -> F.invalid "outcome" "unsupported outcome tag")
  in
  let%bind created_at =
    F.required json "created_at" (fun value ->
      Agent_protocol.Timestamp.of_json value |> F.protocol)
  in
  let%bind expires_at =
    F.optional json "expires_at" (fun value ->
      Agent_protocol.Timestamp.of_json value |> F.protocol)
  in
  let%bind retention =
    F.required json "retention" (fun json ->
      let%bind tag = F.string json in
      match tag with
      | "standard" -> Ok Standard
      | "protected" -> Ok Protected
      | _ -> F.invalid "retention" "unsupported retention policy")
  in
  let%map () =
    match expires_at with
    | Some expires when Agent_protocol.Timestamp.compare expires created_at < 0 ->
      F.invalid "expires_at" "precedes creation"
    | None | Some _ -> Ok ()
  in
  Persisted.
    { key
    ; request_digest
    ; accepted_transaction_sequence
    ; outcome
    ; created_at
    ; expires_at
    ; retention
    }
;;

let codec =
  let outcome_shape =
    D.Shape.tagged_object
      ~discriminator:"tag"
      [ "pending", F.shape [ "tag", D.Shape.value ]
      ; "success", F.shape [ "tag", D.Shape.value; "value", D.Shape.value ]
      ; "failure", F.shape [ "tag", D.Shape.value; "error", D.Shape.value ]
      ]
  in
  let outcome_shape =
    match outcome_shape with
    | Ok shape -> shape
    | Error error -> raise_s [%sexp (error : D.Error.t)]
  in
  let record_shape =
    F.shape
      [ "record_id", D.Shape.value
      ; "key", key_shape
      ; "request_digest", D.Shape.value
      ; "accepted_transaction_sequence", D.Shape.value
      ; "outcome", outcome_shape
      ; "created_at", D.Shape.value
      ; "expires_at", D.Shape.value
      ; "retention", D.Shape.value
      ]
  in
  let records_shape =
    match D.Shape.array record_shape ~identity_field:(Some "record_id") with
    | Ok shape -> shape
    | Error error -> raise_s [%sexp (error : D.Error.t)]
  in
  let decode json =
    let open Result.Let_syntax in
    let%bind values = F.required json "records" F.array in
    Result.all (List.map values ~f:decode_persisted)
  in
  let encode records =
    Result.all (List.map records ~f:persisted_json)
    |> Result.map ~f:(fun values -> `Object [ "records", `Array values ])
  in
  match
    D.Domain_codec.create
      ~limits:cache_limits
      ~kind:"store.idempotency_cache"
      ~version
      ~shape:(F.shape [ "records", records_shape ])
      ~supported_semantics:[]
      ~decode
      ~encode
  with
  | Ok codec -> codec
  | Error error -> raise_s [%sexp "invalid static cache codec", (error : D.Error.t)]
;;

let map_of_records records =
  match
    Map.of_alist
      (module Key)
      (List.map records ~f:(fun (record : Persisted.record) -> record.key, record))
  with
  | `Ok map -> Ok map
  | `Duplicate_key _ -> Error (Store_error.Corrupt "duplicate persisted idempotency key")
;;

let upgrade document =
  let open Result.Let_syntax in
  let map_field json name f =
    let%bind value = F.required json name f in
    match json with
    | `Object fields ->
      Ok
        (`Object
            (List.map fields ~f:(fun (key, old) ->
               key, if String.equal key name then value else old)))
    | _ -> F.invalid name "must be an object"
  in
  let%bind step =
    D.Conversion.Step.of_function
      ~kind:"store.idempotency_cache"
      ~from_version:1
      ~f:(fun json ->
        map_field json "records" (fun json ->
          let%bind records = F.array json in
          let%map records =
            Result.all
              (List.map records ~f:(fun record ->
                 let%bind key = F.required record "key" key_decode in
                 map_field record "outcome" (fun outcome ->
                   let%bind tag = F.required outcome "tag" F.string in
                   if String.equal tag "success"
                   then
                     map_field outcome "value" (fun value ->
                       History_revision_conversion.initialize_method_result
                         value
                         ~method_name:key.method_name)
                   else Ok outcome)))
          in
          `Array records))
  in
  let%bind conversion =
    D.Conversion.create
      ~limits:cache_limits
      ~targets:[ "store.idempotency_cache", version ]
      ~max_steps:1
      ~max_operations:100_000
      ~steps:[ step ]
  in
  D.Conversion.upgrade conversion document
;;

let restore_document document =
  let open Result.Let_syntax in
  let%bind document = upgrade document |> F.store in
  D.Domain_codec.decode codec document |> F.store
;;

(* Expiration is an explicit owner operation retiring exactly these record
   identities, while preserving every other envelope/nested unknown field. *)
let retire_carrier carrier retiring =
  match D.Extension_carrier.template carrier with
  | None -> Ok carrier
  | Some document ->
    let rec retain = function
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             if String.equal name "payload"
             then name, retain value
             else if String.equal name "records"
             then (
               match value with
               | `Array records ->
                 ( name
                 , `Array
                     (List.filter records ~f:(fun record ->
                        match D.Json.field record ~name:"record_id" with
                        | Value (`String id) ->
                          not (List.mem retiring id ~equal:String.equal)
                        | Absent | Null | Value _ -> true)) )
               | _ -> name, value)
             else name, value))
      | json -> json
    in
    let open Result.Let_syntax in
    let%bind document =
      D.Document.inspect ~limits:cache_limits (retain (D.Document.json document))
      |> F.store
    in
    restore_document document
;;

let save t ?(retiring = []) records =
  let open Result.Let_syntax in
  let%bind carrier = retire_carrier t.carrier retiring in
  let carrier = D.Extension_carrier.with_value carrier (Map.data records) in
  let%bind document = D.Domain_codec.encode codec carrier |> F.store in
  let%bind restored = restore_document document in
  let%map () =
    Durable_file.replace
      ~env:t.env
      ~durability:Flush_file_and_directory
      ~path:t.path
      (D.Document.to_string document)
  in
  t.carrier <- restored
;;

let load ~env ~path =
  let open Result.Let_syntax in
  let%bind contents =
    Durable_file.load_bounded ~env ~path ~max_bytes:(D.Limits.max_bytes cache_limits)
  in
  let%bind document = D.Document.decode ~limits:cache_limits contents |> F.store in
  let%bind carrier = restore_document document in
  let%map records = map_of_records (D.Extension_carrier.value carrier) in
  records, carrier
;;

let open_or_create ~env ~path =
  if not (Filename.is_absolute path)
  then
    Error
      (Store_error.Io
         { operation = "open idempotency store"; path; message = "path must be absolute" })
  else (
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let exists = Eio.Path.is_file file in
    let loaded =
      if exists
      then load ~env ~path
      else Ok (Map.empty (module Key), D.Extension_carrier.of_authored_value [])
    in
    Result.bind loaded ~f:(fun (records, carrier) ->
      let t = { env; path; mutex = Eio.Mutex.create (); records; carrier } in
      if exists then Ok t else Result.map (save t records) ~f:(fun () -> t)))
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

let with_retained_references t ~candidates ~max_records ~max_bytes ~f =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let open Result.Let_syntax in
    let%bind () =
      if max_records >= 0 && max_bytes >= 0 && Map.length t.records <= max_records
      then Ok ()
      else
        Error (Store_error.Corrupt "cached response retention exceeds its record budget")
    in
    let%bind reader =
      Retention_reader.create
        ~env:t.env
        ~root:(Filename.dirname t.path)
        ~max_entries:1
        ~max_bytes
    in
    let%bind bytes =
      Retention_reader.read reader ~path:(Filename.basename t.path) ~max_bytes
    in
    let%bind document = D.Document.decode ~limits:cache_limits bytes |> F.store in
    let%bind disk_carrier = restore_document document in
    let disk = D.Extension_carrier.value disk_carrier in
    let%bind _ = map_of_records disk in
    let%bind () =
      if List.length disk <= max_records - Map.length t.records
      then Ok ()
      else
        Error (Store_error.Corrupt "cached response retention exceeds its record budget")
    in
    let%bind scan =
      Blob_reference_scan.create candidates
      |> Result.map_error ~f:(fun error ->
        Store_error.Corrupt error.Agent_protocol.Error.message)
    in
    let feed text =
      Blob_reference_scan.begin_root scan;
      Blob_reference_scan.feed scan text
    in
    let inspect_document document =
      feed (D.Document.to_string document);
      F.iter_strings (D.Document.json document) ~f:feed
    in
    inspect_document document;
    let%bind memory_document =
      D.Domain_codec.encode
        codec
        (D.Extension_carrier.with_value t.carrier (Map.data t.records))
      |> F.store
    in
    let memory_bytes = D.Document.to_string memory_document in
    let%bind () = Retention_reader.charge_bytes reader (String.length memory_bytes) in
    inspect_document memory_document;
    let pending records =
      List.exists records ~f:(fun (record : Persisted.record) ->
        match record.outcome with
        | Pending -> true
        | Success _ | Failure _ -> false)
    in
    if pending disk || pending (Map.data t.records)
    then Ok None
    else Result.map (f (Blob_reference_scan.references scan)) ~f:Option.some)
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
      (records : (Key.t, Persisted.record, Key.comparator_witness) Map.t)
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
    else (
      let retiring =
        Map.fold t.records ~init:[] ~f:(fun ~key ~data:record ids ->
          if is_expired ~now record then record_id key :: ids else ids)
      in
      Result.map (save t ~retiring records) ~f:(fun () ->
        t.records <- records;
        removed)))
;;
