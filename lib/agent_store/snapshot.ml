open! Core
module D = Document_schema
module F = Document_fields

let kind = "session.snapshot"
let current_schema_version = 1

module Value = struct
  type t =
    { schema_version : int
    ; session_id : Agent_protocol.Id.Session.t
    ; transaction_sequence : int64
    ; transaction_hash : string option
    ; event_sequence : int64
    ; created_at : Agent_protocol.Timestamp.t
    ; prompt_artifact : string
    ; workspace_identity : string
    ; payload : D.Document.t
    }
end

module Stored = struct
  type metadata =
    { session_id : string
    ; transaction_sequence : int64
    ; transaction_hash : string option
    ; event_sequence : int64
    ; prompt_artifact : string
    ; workspace_identity : string
    ; generation : int64
    ; session_revision : int64
    }

  type t =
    { record : Document_record.t
    ; metadata : metadata
    }

  let record t = t.record
  let metadata t = t.metadata

  let project ~limits payload =
    let open Result.Let_syntax in
    let%bind session_id = F.required payload "session_id" F.string in
    let%bind transaction_sequence = F.required payload "transaction_sequence" F.decimal in
    let%bind transaction_hash = F.optional payload "transaction_hash" F.digest in
    let%bind event_sequence = F.required payload "event_sequence" F.decimal in
    let%bind prompt_artifact = F.required payload "prompt_artifact" F.string in
    let%bind workspace_identity = F.required payload "workspace_identity" F.string in
    let%bind () =
      if Int64.equal transaction_sequence 0L
      then
        if Option.is_none transaction_hash && Int64.equal event_sequence 0L
        then Ok ()
        else F.invalid "transaction_hash" "initial snapshot must have initial anchors"
      else if Option.is_some transaction_hash
      then Ok ()
      else F.invalid "transaction_hash" "noninitial snapshot requires a digest"
    in
    let%bind state = F.required payload "state" (F.document ~limits) in
    let%bind () = F.expect_versions state ~kind:"session.state" ~versions:[ 1; 2; 3 ] in
    let state = D.Document.payload state in
    let%bind identity = F.required state "identity" Result.return in
    let%bind counters = F.required state "counters" Result.return in
    let%bind spec = F.required state "spec" Result.return in
    let%bind stored_session = F.required identity "session_id" F.string in
    let%bind generation = F.required identity "generation" F.decimal in
    let%bind session_revision = F.required counters "revision" F.decimal in
    let%bind stored_transaction = F.required counters "transaction_sequence" F.decimal in
    let%bind stored_event = F.required counters "event_sequence" F.decimal in
    let%bind stored_prompt = F.required spec "prompt_revision_id" F.string in
    let%bind workspace = F.required spec "workspace_instance" Result.return in
    let%bind stored_workspace = F.required workspace "conflict_domain" F.string in
    let%map () =
      if
        String.equal session_id stored_session
        && Int64.equal transaction_sequence stored_transaction
        && Int64.equal event_sequence stored_event
        && String.equal prompt_artifact stored_prompt
        && String.equal workspace_identity stored_workspace
      then Ok ()
      else F.invalid "state" "stored snapshot metadata differs from embedded state"
    in
    { session_id
    ; transaction_sequence
    ; transaction_hash
    ; event_sequence
    ; prompt_artifact
    ; workspace_identity
    ; generation
    ; session_revision
    }
  ;;

  let of_record record =
    let document = Document_record.document record in
    let open Result.Let_syntax in
    let%bind () = F.expect document ~kind ~version:current_schema_version |> F.store in
    let%bind limits =
      F.limits ~max_bytes:(String.length (Document_record.stored_bytes record)) |> F.store
    in
    let%map metadata = project ~limits (D.Document.payload document) |> F.store in
    { record; metadata }
  ;;
end

type provenance =
  { stored : Stored.t
  ; carrier : Value.t D.Extension_carrier.t
  }

type t =
  { schema_version : int
  ; session_id : Agent_protocol.Id.Session.t
  ; transaction_sequence : int64
  ; transaction_hash : string option
  ; event_sequence : int64
  ; created_at : Agent_protocol.Timestamp.t
  ; prompt_artifact : string
  ; workspace_identity : string
  ; payload : D.Document.t
  ; provenance : provenance
  }

type installed =
  { filename : string
  ; snapshot : t
  }

type installed_stored =
  { filename : string
  ; stored : Stored.t
  }

let value t = D.Extension_carrier.value t.provenance.carrier
let carrier t = t.provenance.carrier
let stored t = t.provenance.stored
let filename transaction_sequence = sprintf "snapshot-%016Ld.bin" transaction_sequence
let current_path directory = Filename.concat directory "CURRENT"
let eio_path env path = Eio.Path.(Eio.Stdenv.fs env / path)

let of_carrier stored carrier =
  let value = D.Extension_carrier.value carrier in
  { schema_version = value.Value.schema_version
  ; session_id = value.session_id
  ; transaction_sequence = value.transaction_sequence
  ; transaction_hash = value.transaction_hash
  ; event_sequence = value.event_sequence
  ; created_at = value.created_at
  ; prompt_artifact = value.prompt_artifact
  ; workspace_identity = value.workspace_identity
  ; payload = value.payload
  ; provenance = { stored; carrier }
  }
;;

let encode_value (value : Value.t) =
  if value.schema_version <> current_schema_version
  then F.invalid "schema_version" "snapshot envelope version differs"
  else
    Ok
      (`Object
          [ "session_id", `String (Agent_protocol.Id.Session.to_string value.session_id)
          ; "transaction_sequence", F.decimal_json value.transaction_sequence
          ; ( "transaction_hash"
            , F.option_json value.transaction_hash ~f:(fun hash -> `String hash) )
          ; "event_sequence", F.decimal_json value.event_sequence
          ; "created_at", `String (Agent_protocol.Timestamp.to_string value.created_at)
          ; "prompt_artifact", `String value.prompt_artifact
          ; "workspace_identity", `String value.workspace_identity
          ; "state", D.Document.json value.payload
          ])
;;

let decode_value ~limits payload =
  let open Result.Let_syntax in
  let%bind metadata = Stored.project ~limits payload in
  let%bind session_id =
    Agent_protocol.Id.Session.of_string metadata.session_id |> F.protocol
  in
  let%bind created_at = F.required payload "created_at" F.string in
  let%bind created_at = Agent_protocol.Timestamp.of_string created_at |> F.protocol in
  let%bind state = F.required payload "state" (F.document ~limits) in
  let%map () =
    if String.equal (D.Document.kind state) "session.state"
    then Ok ()
    else
      Error
        (D.Error.Wrong_kind { expected = "session.state"; actual = D.Document.kind state })
  in
  Value.
    { schema_version = current_schema_version
    ; session_id
    ; transaction_sequence = metadata.transaction_sequence
    ; transaction_hash = metadata.transaction_hash
    ; event_sequence = metadata.event_sequence
    ; created_at
    ; prompt_artifact = metadata.prompt_artifact
    ; workspace_identity = metadata.workspace_identity
    ; payload = state
    }
;;

let codec ~limits =
  D.Domain_codec.create
    ~limits
    ~kind
    ~version:current_schema_version
    ~shape:
      (F.shape
         (List.map
            [ "session_id"
            ; "transaction_sequence"
            ; "transaction_hash"
            ; "event_sequence"
            ; "created_at"
            ; "prompt_artifact"
            ; "workspace_identity"
            ; "state"
            ]
            ~f:(fun name -> name, D.Shape.value)))
    ~supported_semantics:[]
    ~decode:(decode_value ~limits)
    ~encode:encode_value
;;

let restore stored ~limits =
  let open Result.Let_syntax in
  let%bind codec = codec ~limits |> F.store in
  let%bind document =
    F.upgrade (Document_record.document (Stored.record stored)) ~limits ~kind |> F.store
  in
  let%map carrier = D.Domain_codec.decode codec document |> F.store in
  of_carrier stored carrier
;;

let of_carrier_value ~limits carrier =
  let open Result.Let_syntax in
  let%bind codec = codec ~limits |> F.store in
  let%bind document = D.Domain_codec.encode codec carrier |> F.store in
  let%bind record =
    Document_record.of_document document ~limits |> Result.map_error ~f:F.record_error
  in
  let%bind stored = Stored.of_record record in
  restore stored ~limits
;;

let create
      ~limits
      ~session_id
      ~transaction_sequence
      ~transaction_hash
      ~event_sequence
      ~created_at
      ~prompt_artifact
      ~workspace_identity
      ~payload
  =
  let value =
    Value.
      { schema_version = current_schema_version
      ; session_id
      ; transaction_sequence
      ; transaction_hash
      ; event_sequence
      ; created_at
      ; prompt_artifact
      ; workspace_identity
      ; payload
      }
  in
  of_carrier_value ~limits (D.Extension_carrier.of_authored_value value)
;;

let with_value t ~limits value =
  of_carrier_value ~limits (D.Extension_carrier.with_value (carrier t) value)
;;

let update
      t
      ~limits
      ~transaction_sequence
      ~transaction_hash
      ~event_sequence
      ~created_at
      ~prompt_artifact
      ~workspace_identity
      ~payload
  =
  with_value
    t
    ~limits
    { (value t) with
      transaction_sequence
    ; transaction_hash
    ; event_sequence
    ; created_at
    ; prompt_artifact
    ; workspace_identity
    ; payload
    }
;;

let decode_stored_file ~max_payload_length contents =
  let open Result.Let_syntax in
  let%bind limits = F.limits ~max_bytes:max_payload_length |> F.store in
  let%bind record =
    Document_record.decode_file ~limits ~expected_digest:None contents
    |> Result.map_error ~f:F.record_error
  in
  let%bind () =
    if Document_record.flags record = 0
    then Ok ()
    else Error (Store_error.Corrupt "snapshot has nonzero frame flags")
  in
  Stored.of_record record
;;

let decode_file ~max_payload_length contents =
  let open Result.Let_syntax in
  let%bind stored = decode_stored_file ~max_payload_length contents in
  let%bind limits = F.limits ~max_bytes:max_payload_length |> F.store in
  restore stored ~limits
;;

let filename_sequence filename =
  match
    String.chop_prefix filename ~prefix:"snapshot-"
    |> Option.bind ~f:(String.chop_suffix ~suffix:".bin")
  with
  | None -> Error (Store_error.Corrupt ("invalid snapshot filename: " ^ filename))
  | Some digits ->
    (match Int64.of_string_opt digits with
     | Some sequence
       when Int64.(sequence >= zero)
            && String.equal filename (sprintf "snapshot-%016Ld.bin" sequence) ->
       Ok sequence
     | _ -> Error (Store_error.Corrupt ("invalid snapshot filename: " ^ filename)))
;;

let read_stored_file ~env ~directory ~max_payload_length ~filename:installed_filename =
  let open Result.Let_syntax in
  let%bind _ = filename_sequence installed_filename in
  let path = Filename.concat directory installed_filename in
  let%bind max_bytes =
    if max_payload_length < 0 || max_payload_length > Int.max_value - 52
    then Error (Store_error.Corrupt "snapshot frame byte budget is invalid")
    else Ok (max_payload_length + 52)
  in
  let%bind contents = Durable_file.load_bounded ~env ~path ~max_bytes in
  let%bind stored = decode_stored_file ~max_payload_length contents in
  let%map () =
    if
      String.equal
        installed_filename
        (filename (Stored.metadata stored).transaction_sequence)
    then Ok ()
    else Error (Store_error.Corrupt "snapshot filename differs from stored counter")
  in
  { filename = installed_filename; stored }
;;

let read_file ~env ~directory ~max_payload_length ~filename =
  let open Result.Let_syntax in
  let%bind installed = read_stored_file ~env ~directory ~max_payload_length ~filename in
  let%bind limits = F.limits ~max_bytes:max_payload_length |> F.store in
  let%map snapshot = restore installed.stored ~limits in
  { filename; snapshot }
;;

let write_exclusive ~env ~path contents =
  try
    Eio.Path.with_open_out ~create:(`Exclusive 0o600) (eio_path env path) (fun flow ->
      Eio.Flow.copy_string contents flow;
      Eio.File.sync flow);
    Ok ()
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Store_error.of_exn ~operation:"write snapshot" ~path exn)
;;

let install ~env ~directory ~max_payload_length snapshot =
  let open Result.Let_syntax in
  let%bind limits = F.limits ~max_bytes:max_payload_length |> F.store in
  let%bind codec = codec ~limits |> F.store in
  let%bind document = D.Domain_codec.encode codec (carrier snapshot) |> F.store in
  let%bind encoded =
    Document_record.encode document ~limits ~flags:0 |> Result.map_error ~f:F.record_error
  in
  let%bind () =
    try
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path env directory);
      Ok ()
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      Error
        (Store_error.of_exn ~operation:"create snapshot directory" ~path:directory exn)
  in
  let installed_filename = filename snapshot.transaction_sequence in
  let path = Filename.concat directory installed_filename in
  let%bind () = write_exclusive ~env ~path encoded in
  let%bind installed =
    read_file ~env ~directory ~max_payload_length ~filename:installed_filename
  in
  let%map () =
    Durable_file.replace
      ~env
      ~durability:Flush_file_and_directory
      ~path:(current_path directory)
      (installed_filename ^ "\n")
  in
  installed
;;

let snapshot_filenames ~env directory =
  try
    (match Eio.Path.kind ~follow:true (eio_path env directory) with
     | `Not_found -> []
     | _ -> Eio.Path.read_dir (eio_path env directory))
    |> List.filter ~f:(fun name ->
      String.is_prefix name ~prefix:"snapshot-" && String.is_suffix name ~suffix:".bin")
    |> List.map ~f:(fun filename ->
      Result.map (filename_sequence filename) ~f:(fun sequence -> sequence, filename))
    |> Result.all
    |> Result.map ~f:(fun files ->
      List.sort files ~compare:(fun (left, _) (right, _) -> Int64.compare right left)
      |> List.map ~f:snd)
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Store_error.of_exn ~operation:"list snapshots" ~path:directory exn)
;;

let fallback_stored ~env ~directory ~max_payload_length ~excluding =
  let open Result.Let_syntax in
  let%bind filenames = snapshot_filenames ~env directory in
  let rec loop = function
    | [] -> Ok None
    | filename :: rest ->
      if String.equal filename excluding
      then loop rest
      else (
        match read_stored_file ~env ~directory ~max_payload_length ~filename with
        | Ok installed -> Ok (Some installed)
        | Error (Store_error.Missing _) -> loop rest
        | Error error -> Error error)
  in
  loop filenames
;;

let load_current_stored ~env ~directory ~max_payload_length =
  let current = current_path directory in
  if not (Eio.Path.is_file (eio_path env current))
  then Ok None
  else
    let open Result.Let_syntax in
    let%bind filename =
      Durable_file.load_bounded ~env ~path:current ~max_bytes:256
      |> Result.map ~f:String.strip
    in
    match read_stored_file ~env ~directory ~max_payload_length ~filename with
    | Ok installed -> Ok (Some installed)
    | Error (Store_error.Missing _) ->
      fallback_stored ~env ~directory ~max_payload_length ~excluding:filename
    | Error error -> Error error
;;

let load_current ~env ~directory ~max_payload_length =
  let open Result.Let_syntax in
  let%bind installed = load_current_stored ~env ~directory ~max_payload_length in
  match installed with
  | None -> Ok None
  | Some installed ->
    let%bind limits = F.limits ~max_bytes:max_payload_length |> F.store in
    let%map snapshot = restore installed.stored ~limits in
    Some { filename = installed.filename; snapshot }
;;

let retained_stored ~env ~directory ~max_payload_length ~allow_incomplete =
  let open Result.Let_syntax in
  let%bind filenames = snapshot_filenames ~env directory in
  List.fold_result filenames ~init:[] ~f:(fun retained filename ->
    match read_stored_file ~env ~directory ~max_payload_length ~filename with
    | Ok installed -> Ok (installed :: retained)
    | Error (Store_error.Missing _) when allow_incomplete -> Ok retained
    | Error error -> Error error)
;;

module Pruned = struct
  type t =
    { removed_count : int
    ; retention_floor : int64
    }

  let removed_count t = t.removed_count
  let retention_floor t = t.retention_floor
end

let prune_older_with_floor ~max_payload_length ~env ~directory ~keep =
  if keep <= 0
  then Error (Store_error.Corrupt "snapshot retention count must be positive")
  else
    let open Result.Let_syntax in
    let%bind retained =
      retained_stored ~env ~directory ~max_payload_length ~allow_incomplete:false
    in
    let retained =
      List.sort retained ~compare:(fun left right ->
        Int64.compare
          (Stored.metadata right.stored).transaction_sequence
          (Stored.metadata left.stored).transaction_sequence)
    in
    let preserved = List.take retained keep in
    let filenames = List.map retained ~f:(fun installed -> installed.filename) in
    let%bind oldest_sequence =
      match List.last preserved with
      | None -> Error (Store_error.Missing "snapshot retention anchor")
      | Some installed -> Ok (Stored.metadata installed.stored).transaction_sequence
    in
    let%bind current =
      Durable_file.load_bounded ~env ~path:(current_path directory) ~max_bytes:256
      |> Result.map ~f:String.strip
    in
    let%bind () =
      if List.mem (List.take filenames keep) current ~equal:String.equal
      then Ok ()
      else Error (Store_error.Corrupt "snapshot pruning would remove CURRENT")
    in
    let removable = List.drop filenames keep in
    let%bind () =
      Result.all_unit
        (List.map removable ~f:(fun filename ->
           let path = Filename.concat directory filename in
           try
             Eio.Path.unlink (eio_path env path);
             Ok ()
           with
           | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
             Error (Store_error.of_exn ~operation:"prune snapshot" ~path exn)))
    in
    let%map () =
      if List.is_empty removable
      then Ok ()
      else Durable_file.sync_directory ~env ~path:directory
    in
    Pruned.{ removed_count = List.length removable; retention_floor = oldest_sequence }
;;

let prune_older ~max_payload_length ~env ~directory ~keep =
  prune_older_with_floor ~max_payload_length ~env ~directory ~keep
  |> Result.map ~f:Pruned.removed_count
;;

let retention_floor ~env ~directory ~max_payload_length =
  let open Result.Let_syntax in
  let%bind retained =
    retained_stored ~env ~directory ~max_payload_length ~allow_incomplete:false
  in
  match
    List.min_elt retained ~compare:(fun a b ->
      Int64.compare
        (Stored.metadata a.stored).transaction_sequence
        (Stored.metadata b.stored).transaction_sequence)
  with
  | None -> Error (Store_error.Missing "snapshot retention anchor")
  | Some installed -> Ok (Stored.metadata installed.stored).transaction_sequence
;;
