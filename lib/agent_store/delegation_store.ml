open Core
module P = Agent_protocol

module Key = struct
  type t =
    { parent_session_id : P.Id.Session.t
    ; parent_generation : int
    ; principal_id : P.Id.Principal.t
    ; idempotency_key : P.Idempotency_key.t
    }
  [@@deriving equal, sexp]
end

module Admission = struct
  type lifetime =
    | Owned
    | Independent of { authorization_sha256 : string }
  [@@deriving equal, sexp]

  type t =
    { child_session_id : P.Id.Session.t
    ; revision_id : P.Id.Prompt_revision.t
    ; transaction_id : P.Id.Transaction.t
    ; manifest_sha256 : string
    ; parent_revision_id : P.Id.Prompt_revision.t
    ; parent_stop_epoch : int64 option [@sexp.option]
    ; authority_sha256 : string
    ; capability_pins : (string * string) list
    ; lifetime : lifetime
    ; created_at : P.Timestamp.t
    }
  [@@deriving equal, sexp]
end

type stage =
  | Reserved
  | Artifact_installed
  | Child_installed
  | Linked
[@@deriving equal, sexp]

module Reference = struct
  type t =
    { key : Key.t
    ; child_session_id : P.Id.Session.t
    ; revision_id : P.Id.Prompt_revision.t
    ; request_sha256 : string
    ; admission_sha256 : string
    }
  [@@deriving equal, sexp]
end

type revocation =
  | Parent_stopped
  | Parent_deleted
  | Authority_changed
  | Admission_failed
[@@deriving equal, sexp]

type record =
  { key : Key.t
  ; request_sha256 : string
  ; admission : Admission.t
  ; stage : stage
  ; revocation : revocation option
  }
[@@deriving equal, sexp]

type reservation =
  | New of record
  | Replay of record
  | Conflict of record

module Persisted = struct
  type t =
    { version : int
    ; record : record
    }
  [@@deriving sexp]
end

type t =
  { env : Eio_unix.Stdenv.base
  ; root : Data_root.t
  ; mutex : Eio.Mutex.t
  }

let create ~env ~data_root = { env; root = data_root; mutex = Eio.Mutex.create () }
let max_payload_length = 262144
let corrupt message = Error (Store_error.Corrupt message)
let digest text = Digestif.SHA256.(digest_string text |> to_hex)
let directory t = Filename.concat (Data_root.path t.root) "delegations"
let filename key = digest (Key.sexp_of_t key |> Sexp.to_string_mach) ^ ".frame"
let path t key = Filename.concat (directory t) (filename key)

let locked t f =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> Result.try_with f)
  |> function
  | Ok value -> value
  | Error exn -> Exn.reraise exn "delegation ledger"
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

let validate_key (key : Key.t) =
  let open Result.Let_syntax in
  let%bind _ =
    protocol (P.Id.Session.of_string (P.Id.Session.to_string key.parent_session_id))
  in
  let%bind _ =
    protocol (P.Id.Principal.of_string (P.Id.Principal.to_string key.principal_id))
  in
  let%bind _ =
    protocol
      (P.Idempotency_key.of_string (P.Idempotency_key.to_string key.idempotency_key))
  in
  match key.parent_generation >= 0 with
  | true -> Ok ()
  | false -> corrupt "negative delegation parent generation"
;;

let validate (record : record) =
  let open Result.Let_syntax in
  let a = record.admission in
  let%bind () = validate_key record.key in
  let%bind _ =
    protocol (P.Id.Session.of_string (P.Id.Session.to_string a.child_session_id))
  in
  let%bind _ =
    protocol
      (P.Id.Prompt_revision.of_string (P.Id.Prompt_revision.to_string a.revision_id))
  in
  let%bind _ =
    protocol
      (P.Id.Prompt_revision.of_string
         (P.Id.Prompt_revision.to_string a.parent_revision_id))
  in
  let%bind _ =
    protocol (P.Id.Transaction.of_string (P.Id.Transaction.to_string a.transaction_id))
  in
  let%bind _ = protocol (P.Timestamp.of_string (P.Timestamp.to_string a.created_at)) in
  let independent_valid =
    match a.lifetime with
    | Owned -> true
    | Independent { authorization_sha256 } -> sha256 authorization_sha256
  in
  match
    (not (P.Id.Session.equal record.key.parent_session_id a.child_session_id))
    && (not (P.Id.Prompt_revision.equal a.parent_revision_id a.revision_id))
    && sha256 record.request_sha256
    && sha256 a.manifest_sha256
    && sha256 a.authority_sha256
    && Option.for_all a.parent_stop_epoch ~f:(fun epoch -> Int64.(epoch >= 0L))
    && independent_valid
    && List.is_sorted_strictly a.capability_pins ~compare:(fun (left, _) (right, _) ->
      String.compare left right)
    && List.for_all a.capability_pins ~f:(fun (name, pin) ->
      (not (String.is_empty name))
      && String.length name <= 1024
      && (not
            (String.exists name ~f:(fun c -> Char.to_int c < 32 || Char.equal c '\127')))
      && sha256 pin)
  with
  | true -> Ok ()
  | false -> corrupt "invalid delegated creation identity or capability pins"
;;

let reference (record : record) =
  Reference.
    { key = record.key
    ; child_session_id = record.admission.child_session_id
    ; revision_id = record.admission.revision_id
    ; request_sha256 = record.request_sha256
    ; admission_sha256 =
        Admission.sexp_of_t record.admission |> Sexp.to_string_mach |> digest
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
  let%bind () = validate record in
  Frame.encode
    ~max_payload_length
    ~flags:0
    (Persisted.sexp_of_t { version = 2; record } |> Sexp.to_string_mach)
  |> Result.map_error ~f:(fun _ ->
    Store_error.Corrupt "delegation intent exceeds its frame limit")
;;

let decode ~name contents =
  let open Result.Let_syntax in
  match Frame.decode ~max_payload_length ~contents ~offset:0 with
  | Ok (Complete { frame; next_offset })
    when next_offset = String.length contents && Frame.flags frame = 0 ->
    let%bind persisted =
      Result.try_with (fun () ->
        Frame.payload frame |> Sexp.of_string |> Persisted.t_of_sexp)
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "invalid delegation intent payload")
    in
    let%bind () =
      match persisted.version with
      | 1 when Option.is_none persisted.record.admission.parent_stop_epoch -> Ok ()
      | 2 -> Ok ()
      | version when version > 2 -> Error (Store_error.Schema_too_new version)
      | _ -> corrupt "invalid delegation intent version"
    in
    let%bind () = validate persisted.record in
    (match String.equal name (filename persisted.record.key) with
     | true -> Ok persisted.record
     | false -> corrupt "delegation intent filename differs from its scoped key")
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

let reserve t ~key ~request_sha256 ~admission ~max_records ~max_bytes =
  locked t (fun () ->
    let open Result.Let_syntax in
    let candidate =
      { key; request_sha256; admission; stage = Reserved; revocation = None }
    in
    let%bind () = validate candidate in
    let%bind records = records_locked t ~max_records ~max_bytes in
    match List.find records ~f:(fun r -> Key.equal key r.key) with
    | Some record when String.equal request_sha256 record.request_sha256 ->
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
         && Admission.equal expected.admission record.admission -> Ok record
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
