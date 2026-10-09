open Core
module P = Agent_protocol
module D = Document_schema
module F = Document_fields
module R = Delegation_record
module C = Delegation_document
module Key = R.Key
module Admission = R.Admission
module Reference = R.Reference

type stage = R.stage =
  | Reserved
  | Artifact_installed
  | Child_installed
  | Linked
[@@deriving equal, sexp]

type revocation = R.revocation =
  | Parent_stopped
  | Parent_deleted
  | Authority_changed
  | Admission_failed
[@@deriving equal, sexp]

type artifact_collection = R.artifact_collection = Prepared [@@deriving equal, sexp]

type record = R.t =
  { key : Key.t
  ; request_sha256 : string
  ; admission : Admission.t
  ; stage : stage
  ; revocation : revocation option
  ; artifact_collection : artifact_collection option [@sexp.option]
  ; preservation : (unit D.Extension_carrier.t[@sexp.opaque]) option
        [@sexp.option] [@equal.ignore]
  }
[@@deriving equal, sexp]

type reservation =
  | New of record
  | Replay of record
  | Conflict of record

type t =
  { env : Eio_unix.Stdenv.base
  ; root : Data_root.t
  ; mutex : Eio.Mutex.t
  }

let create ~env ~data_root = { env; root = data_root; mutex = Eio.Mutex.create () }
let max_payload_length = C.max_payload_length
let corrupt message = Error (Store_error.Corrupt message)
let digest text = Digestif.SHA256.(digest_string text |> to_hex)
let directory t = Filename.concat (Data_root.path t.root) "delegations"
let filename key = digest (Key.sexp_of_t key |> Sexp.to_string_mach) ^ ".frame"
let path t key = Filename.concat (directory t) (filename key)

let locked t f =
  let outcome =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      try Ok (f ()) with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok value -> value
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let sha256 value =
  String.length value = 64
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let protocol result =
  Result.map_error result ~f:(fun error -> Store_error.Corrupt error.P.Error.message)
;;

let validate_key = R.validate_key
let validate record = R.validate record ~limits:C.limits

let reference (record : record) =
  let document =
    C.of_record record
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  Reference.
    { key = record.key
    ; child_session_id = record.admission.child_session_id
    ; revision_id = record.admission.revision_id
    ; request_sha256 = record.request_sha256
    ; admission_sha256 = C.admission_sha256 document
    }
;;

let validate_reference (reference : Reference.t) =
  let open Result.Let_syntax in
  let%bind () = validate_key reference.key in
  let%bind _ =
    protocol (P.Id.Session.of_string (P.Id.Session.to_string reference.child_session_id))
  in
  let%bind _ =
    protocol
      (P.Id.Prompt_revision.of_string
         (P.Id.Prompt_revision.to_string reference.revision_id))
  in
  match
    sha256 reference.request_sha256
    && sha256 reference.admission_sha256
    && not (P.Id.Session.equal reference.key.parent_session_id reference.child_session_id)
  with
  | true -> Ok ()
  | false -> corrupt "invalid delegated session reference"
;;

let encode record =
  let open Result.Let_syntax in
  let%bind carrier = C.of_record record |> F.store in
  let%bind document = C.to_document carrier |> F.store in
  Document_record.encode document ~limits:C.limits ~flags:0
  |> Result.map_error ~f:F.record_error
;;

let decode ~name contents =
  let open Result.Let_syntax in
  match Frame.decode ~max_payload_length ~contents ~offset:0 with
  | Ok (Complete { frame; next_offset })
    when next_offset = String.length contents && Frame.flags frame = 0 ->
    let%bind stored =
      Document_record.of_frame frame ~limits:C.limits ~expected_digest:None
      |> Result.map_error ~f:F.record_error
    in
    let original = Document_record.document stored in
    let%bind key = C.stored_key original |> F.store in
    if not (String.equal name (filename key))
    then corrupt "original delegation intent filename differs from its scoped key"
    else (
      let%map carrier = C.of_document original |> F.store in
      C.to_record carrier)
  | _ -> corrupt "delegation intent frame is incomplete or corrupt"
;;

let exists t =
  let path = directory t in
  try
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs t.env / path) with
    | `Not_found -> Ok false
    | `Directory -> Ok true
    | _ -> corrupt "delegation ledger is not a regular directory"
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation:"inspect delegation ledger" ~path exn)
;;

let ensure_directory t =
  let open Result.Let_syntax in
  let%bind present = exists t in
  let path = directory t in
  try
    (match present with
     | true -> ()
     | false -> Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs t.env / path));
    Durable_file.sync_directory ~env:t.env ~path:(Data_root.path t.root)
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation:"create delegation ledger" ~path exn)
;;

let reader t ~max_entries ~max_bytes =
  Retention_reader.create ~env:t.env ~root:(directory t) ~max_entries ~max_bytes
;;

let read reader name =
  let open Result.Let_syntax in
  let%bind contents =
    Retention_reader.read reader ~path:name ~max_bytes:(max_payload_length + 4096)
  in
  decode ~name contents
;;

let find_locked t key =
  let open Result.Let_syntax in
  let%bind () = validate_key key in
  let%bind present = exists t in
  match present with
  | false -> Ok None
  | true ->
    (try
       match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs t.env / path t key) with
       | `Not_found -> Ok None
       | _ ->
         let%bind reader =
           reader t ~max_entries:8 ~max_bytes:(max_payload_length + 4096)
         in
         Result.map (read reader (filename key)) ~f:Option.some
     with
     | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
     | exn ->
       Error
         (Store_error.of_exn ~operation:"find delegation intent" ~path:(path t key) exn))
;;

let find t key = locked t (fun () -> find_locked t key)

let resolve t expected =
  locked t (fun () ->
    let open Result.Let_syntax in
    let%bind () = validate_reference expected in
    let%bind found = find_locked t expected.Reference.key in
    match found with
    | Some record when Reference.equal (reference record) expected -> Ok record
    | Some _ -> corrupt "delegated session reference differs from its creation record"
    | None -> Error (Store_error.Missing "delegated session creation record"))
;;

let records_locked t ~max_records ~max_bytes =
  let open Result.Let_syntax in
  let%bind () =
    match
      max_records >= 0 && max_records <= (Int.max_value - 16) / 4 && max_bytes >= 0
    with
    | true -> Ok ()
    | false -> corrupt "invalid delegation scan budget"
  in
  let%bind present = exists t in
  match present with
  | false -> Ok []
  | true ->
    let%bind reader = reader t ~max_entries:((max_records * 4) + 16) ~max_bytes in
    let%bind names = Retention_reader.list reader ~directory:"." in
    let valid_name name =
      Option.exists (String.chop_suffix name ~suffix:".frame") ~f:sha256
    in
    let%bind names =
      List.fold_result names ~init:[] ~f:(fun files name ->
        match valid_name name, Durable_file.temporary_target name with
        | true, _ -> Ok (name :: files)
        | false, Some target when valid_name target ->
          let%bind kind = Retention_reader.kind reader ~path:name in
          (match kind with
           | `File -> Ok files
           | `Directory -> corrupt "invalid delegation temporary entry")
        | _ -> corrupt "unknown entry in delegation ledger")
    in
    let%bind () =
      match List.length names <= max_records with
      | true -> Ok ()
      | false -> corrupt "delegation scan exceeds its record budget"
    in
    let%bind records = List.map names ~f:(read reader) |> Result.all in
    let unique projection compare =
      List.map records ~f:projection
      |> List.sort ~compare
      |> List.is_sorted_strictly ~compare
    in
    (match
       unique (fun r -> r.admission.child_session_id) P.Id.Session.compare
       && unique (fun r -> r.admission.revision_id) P.Id.Prompt_revision.compare
       && unique (fun r -> r.admission.transaction_id) P.Id.Transaction.compare
     with
     | true -> Ok records
     | false ->
       corrupt "delegation ledger reuses a child, artifact or transaction identity")
;;

let with_records t ~max_records ~max_bytes ~f =
  locked t (fun () -> Result.bind (records_locked t ~max_records ~max_bytes) ~f)
;;

let save t record =
  let open Result.Let_syntax in
  let%bind contents = encode record in
  let%bind () = ensure_directory t in
  Durable_file.replace
    ~env:t.env
    ~durability:Flush_file_and_directory
    ~path:(path t record.key)
    contents
;;

let with_artifact_retention
      t
      ~max_records
      ~max_bytes
      ~max_artifact_entries
      ~max_artifact_bytes
      ~f
  =
  locked t (fun () ->
    let open Result.Let_syntax in
    let%bind records = records_locked t ~max_records ~max_bytes in
    let%bind reader =
      Retention_reader.create
        ~env:t.env
        ~root:(Data_root.path t.root)
        ~max_entries:max_artifact_entries
        ~max_bytes:max_artifact_bytes
    in
    let directory path =
      let%bind kind = Retention_reader.kind reader ~path in
      match kind with
      | `Directory -> Ok ()
      | `File -> corrupt "retention root is not a directory"
    in
    let%bind () = directory "sessions" in
    let%bind () = directory "prompt-artifacts" in
    let%bind artifacts =
      Prompt_artifact_store.create
        ~env:t.env
        ~root:(Data_root.prompt_artifacts_path t.root)
    in
    let presence path =
      try
        match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs t.env / path) with
        | `Not_found -> Ok false
        | `Directory -> Ok true
        | _ -> corrupt "delegation retention found a linked or invalid destination"
      with
      | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
      | exn ->
        Error (Store_error.of_exn ~operation:"inspect abandoned delegation" ~path exn)
    in
    let%bind retained, collected =
      List.fold_result records ~init:([], []) ~f:(fun (retained, collected) record ->
        let%map keep =
          match record.stage, record.revocation with
          | (Reserved | Artifact_installed), Some _ ->
            let%bind installed =
              presence (Data_root.session_path t.root record.admission.child_session_id)
            in
            let%bind staged =
              presence
                (Filename.concat
                   (Data_root.sessions_path t.root)
                   (".creating-"
                    ^ P.Id.Transaction.to_string record.admission.transaction_id))
            in
            (match installed || staged with
             | true -> Ok `Retain
             | false ->
               let%bind exists =
                 presence
                   (Filename.concat
                      (Data_root.prompt_artifacts_path t.root)
                      (P.Id.Prompt_revision.to_string record.admission.revision_id))
               in
               let%map () =
                 match exists, record.artifact_collection with
                 | false, _ | true, Some Prepared -> Ok ()
                 | true, None ->
                   Prompt_artifact_store.verify_retained
                     artifacts
                     ~reader
                     ~revision_id:record.admission.revision_id
                     ~manifest_sha256:record.admission.manifest_sha256
               in
               `Collect exists)
          | _, _ -> Ok `Retain
        in
        match keep with
        | `Collect true -> retained, record :: collected
        | `Collect false -> retained, collected
        | `Retain -> record.admission.revision_id :: retained, collected)
    in
    (* Persist the verified ownership before any deletion. A later pass can then
       finish partially removed directories without treating them as new artifacts.
       Reflush existing intent too, including a prior ambiguous acknowledgement. *)
    let%bind () =
      List.fold_result collected ~init:() ~f:(fun () record ->
        save t { record with artifact_collection = Some Prepared })
    in
    (* Even a revoked record may retain an ancestor's source identity. The host
       adds catalog and session references before choosing any deletion. *)
    f (retained @ List.map records ~f:(fun record -> record.admission.parent_revision_id)))
;;

let same_invocation_scope left right =
  match left, right with
  | Admission.Invocation_owned a, Admission.Invocation_owned b ->
    P.Id.Invocation.equal a.invocation_id b.invocation_id
  | Invocation_owned _, _ | _, Invocation_owned _ -> false
  | (Owned | Independent _), (Owned | Independent _) -> true
;;

let reserve t ~key ~request_sha256 ~admission ~max_records ~max_bytes =
  locked t (fun () ->
    let open Result.Let_syntax in
    let candidate =
      { key
      ; request_sha256
      ; admission
      ; stage = Reserved
      ; revocation = None
      ; artifact_collection = None
      ; preservation = None
      }
    in
    let%bind () = validate candidate in
    let%bind records = records_locked t ~max_records ~max_bytes in
    match List.find records ~f:(fun r -> Key.equal key r.key) with
    | Some record
      when String.equal request_sha256 record.request_sha256
           && Option.equal
                Admission.equal_authored_tool
                admission.authored_tool
                record.admission.authored_tool
           && same_invocation_scope admission.lifetime record.admission.lifetime ->
      let%map () = save t record in
      Replay record
    | Some record -> Ok (Conflict record)
    | None ->
      let%bind () =
        match
          List.length records < max_records
          && not
               (List.exists records ~f:(fun r ->
                  P.Id.Session.equal
                    admission.child_session_id
                    r.admission.child_session_id
                  || P.Id.Prompt_revision.equal
                       admission.revision_id
                       r.admission.revision_id
                  || P.Id.Transaction.equal
                       admission.transaction_id
                       r.admission.transaction_id))
        with
        | true -> Ok ()
        | false -> corrupt "delegation reservation exceeds capacity or reuses an identity"
      in
      let%map () = save t candidate in
      New candidate)
;;

let current t expected =
  let open Result.Let_syntax in
  let%bind found = find_locked t expected.key in
  match found with
  | Some record
    when String.equal expected.request_sha256 record.request_sha256
         && Admission.equal expected.admission record.admission
         && Reference.equal (reference expected) (reference record) -> Ok record
  | Some _ -> corrupt "delegation admission changed"
  | None -> Error (Store_error.Missing "delegation intent")
;;

let rank = function
  | Reserved -> 0
  | Artifact_installed -> 1
  | Child_installed -> 2
  | Linked -> 3
;;

let advance t expected stage =
  locked t (fun () ->
    let open Result.Let_syntax in
    let%bind record = current t expected in
    match record.revocation with
    | Some _ -> corrupt "delegation has been revoked"
    | None ->
      let%bind next =
        match rank stage - rank record.stage with
        | distance when distance <= 0 -> Ok record
        | 1 -> Ok { record with stage }
        | _ -> corrupt "delegation creation stage skipped"
      in
      let%map () = save t next in
      next)
;;

let revoke t expected reason =
  locked t (fun () ->
    let open Result.Let_syntax in
    let%bind record = current t expected in
    let next =
      match record.revocation with
      | Some _ -> record
      | None -> { record with revocation = Some reason }
    in
    let%map () = save t next in
    next)
;;

let discard_uninstalled_staging t expected =
  locked t (fun () ->
    let open Result.Let_syntax in
    let%bind record = current t expected in
    let transaction = P.Id.Transaction.to_string record.admission.transaction_id in
    let discard ~parent ~name ~destination =
      let stage = Filename.concat parent name in
      let path value = Eio.Path.(Eio.Stdenv.fs t.env / value) in
      try
        match Eio.Path.kind ~follow:false (path destination) with
        | `Directory -> Ok ()
        | `Not_found ->
          (match Eio.Path.kind ~follow:false (path stage) with
           | `Not_found -> Ok ()
           | `Directory ->
             Eio.Path.rmtree (path stage);
             Durable_file.sync_directory ~env:t.env ~path:parent
           | _ -> corrupt "delegation staging entry is not a directory")
        | _ -> corrupt "delegation destination is not a directory"
      with
      | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
      | exn ->
        Error
          (Store_error.of_exn
             ~operation:"discard uninstalled delegation staging"
             ~path:stage
             exn)
    in
    match record.stage with
    | Reserved ->
      discard
        ~parent:(Data_root.prompt_artifacts_path t.root)
        ~name:(".install-" ^ transaction)
        ~destination:
          (Filename.concat
             (Data_root.prompt_artifacts_path t.root)
             (P.Id.Prompt_revision.to_string record.admission.revision_id))
    | Artifact_installed ->
      discard
        ~parent:(Data_root.sessions_path t.root)
        ~name:(".creating-" ^ transaction)
        ~destination:(Data_root.session_path t.root record.admission.child_session_id)
    | Child_installed | Linked -> Ok ())
;;

let reference_to_jsonaf (t : Reference.t) =
  `Object
    [ ( "key"
      , `Object
          [ "parent_session_id", P.Id.Session.to_json t.key.parent_session_id
          ; "parent_generation", `Number (Int.to_string t.key.parent_generation)
          ; "principal_id", P.Id.Principal.to_json t.key.principal_id
          ; "idempotency_key", P.Idempotency_key.to_json t.key.idempotency_key
          ] )
    ; "child_session_id", P.Id.Session.to_json t.child_session_id
    ; "revision_id", P.Id.Prompt_revision.to_json t.revision_id
    ; "request_sha256", `String t.request_sha256
    ; "admission_sha256", `String t.admission_sha256
    ]
;;

let reference_of_jsonaf json =
  let module J = P.Json_codec in
  let decoded =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind key =
      J.required_as fields "key" (fun json ->
        let%bind fields = J.fields json in
        let%bind parent_session_id =
          J.required_as fields "parent_session_id" P.Id.Session.of_json
        in
        let%bind parent_generation =
          J.required_as
            fields
            "parent_generation"
            (J.bounded_int ~min:0 ~max:Int.max_value)
        in
        let%bind principal_id =
          J.required_as fields "principal_id" P.Id.Principal.of_json
        in
        let%map idempotency_key =
          J.required_as fields "idempotency_key" P.Idempotency_key.of_json
        in
        Key.{ parent_session_id; parent_generation; principal_id; idempotency_key })
    in
    let%bind child_session_id =
      J.required_as fields "child_session_id" P.Id.Session.of_json
    in
    let%bind revision_id =
      J.required_as fields "revision_id" P.Id.Prompt_revision.of_json
    in
    let%bind request_sha256 = J.required_as fields "request_sha256" J.string in
    let%map admission_sha256 = J.required_as fields "admission_sha256" J.string in
    Reference.{ key; child_session_id; revision_id; request_sha256; admission_sha256 }
  in
  let open Result.Let_syntax in
  let%bind t =
    decoded |> Result.map_error ~f:(fun e -> Store_error.Corrupt e.P.Error.message)
  in
  let%map () = validate_reference t in
  t
;;

let reference_shape =
  let module S = Document_schema.Shape in
  let object_ fields =
    match S.object_ fields with
    | Ok shape -> shape
    | Error error ->
      raise_s
        [%sexp "invalid delegation reference shape", (error : Document_schema.Error.t)]
  in
  object_
    [ ( "key"
      , object_
          [ "parent_session_id", S.value
          ; "parent_generation", S.value
          ; "principal_id", S.value
          ; "idempotency_key", S.value
          ] )
    ; "child_session_id", S.value
    ; "revision_id", S.value
    ; "request_sha256", S.value
    ; "admission_sha256", S.value
    ]
;;
