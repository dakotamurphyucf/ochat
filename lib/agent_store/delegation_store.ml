open Core
module P = Agent_protocol
module D = Document_schema
module F = Document_fields

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
  type authored_tool =
    { name : string
    ; source_sha256 : string
    }
  [@@deriving equal, sexp]

  type lifetime =
    | Owned
    | Invocation_owned of { invocation_id : P.Id.Invocation.t }
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
    ; authored_tool : authored_tool option [@sexp.option]
    ; capability_pins : (string * string) list
    ; lifetime : lifetime
    ; created_at : P.Timestamp.t
    ; inference_target : (Inference.Request.Target.t[@sexp.opaque]) option [@sexp.option]
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

type artifact_collection = Prepared [@@deriving equal, sexp]

type record =
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
  let%bind limits = F.limits ~max_bytes:max_payload_length |> F.store in
  let%bind () =
    Option.value_map a.inference_target ~default:(Ok ()) ~f:(fun target ->
      Inference.Request.Target.validate target ~limits
      |> Result.map_error ~f:(fun error ->
        Store_error.Corrupt (Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))))
  in
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
  let lifetime_valid =
    match a.lifetime with
    | Owned -> true
    | Invocation_owned { invocation_id } ->
      Option.is_some a.authored_tool
      && Result.is_ok
           (P.Id.Invocation.of_string (P.Id.Invocation.to_string invocation_id))
    | Independent { authorization_sha256 } -> sha256 authorization_sha256
  in
  let authored_valid =
    Option.for_all a.authored_tool ~f:(fun authored ->
      (not (String.is_empty authored.name))
      && String.length authored.name <= 1024
      && (not
            (String.exists authored.name ~f:(fun c ->
               Char.to_int c < 32 || Char.equal c '\127')))
      && sha256 authored.source_sha256)
  in
  match
    (not (P.Id.Session.equal record.key.parent_session_id a.child_session_id))
    && (not (P.Id.Prompt_revision.equal a.parent_revision_id a.revision_id))
    && sha256 record.request_sha256
    && sha256 a.manifest_sha256
    && sha256 a.authority_sha256
    && Option.for_all a.parent_stop_epoch ~f:(fun epoch -> Int64.(epoch >= 0L))
    && lifetime_valid
    && authored_valid
    && (match record.artifact_collection, record.stage, record.revocation with
        | None, _, _ -> true
        | Some Prepared, (Reserved | Artifact_installed), Some _ -> true
        | Some Prepared, _, _ -> false)
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

let text value = `String value
let nullable value ~f = F.option_json value ~f

let key_to_json (key : Key.t) =
  `Object
    [ "parent_session_id", text (P.Id.Session.to_string key.parent_session_id)
    ; "parent_generation", F.decimal_json (Int64.of_int key.parent_generation)
    ; "principal_id", text (P.Id.Principal.to_string key.principal_id)
    ; "idempotency_key", text (P.Idempotency_key.to_string key.idempotency_key)
    ]
;;

let lifetime_to_json = function
  | Admission.Owned -> `Object [ "kind", text "owned" ]
  | Invocation_owned { invocation_id } ->
    `Object
      [ "kind", text "invocation_owned"
      ; "invocation_id", text (P.Id.Invocation.to_string invocation_id)
      ]
  | Independent { authorization_sha256 } ->
    `Object
      [ "kind", text "independent"; "authorization_sha256", text authorization_sha256 ]
;;

let admission_to_json (a : Admission.t) =
  `Object
    [ "child_session_id", text (P.Id.Session.to_string a.child_session_id)
    ; "revision_id", text (P.Id.Prompt_revision.to_string a.revision_id)
    ; "transaction_id", text (P.Id.Transaction.to_string a.transaction_id)
    ; "manifest_sha256", text a.manifest_sha256
    ; "parent_revision_id", text (P.Id.Prompt_revision.to_string a.parent_revision_id)
    ; "parent_stop_epoch", nullable a.parent_stop_epoch ~f:F.decimal_json
    ; "authority_sha256", text a.authority_sha256
    ; ( "authored_tool"
      , nullable a.authored_tool ~f:(fun authored ->
          `Object
            [ "name", text authored.name; "source_sha256", text authored.source_sha256 ])
      )
    ; ( "capability_pins"
      , `Array
          (List.map a.capability_pins ~f:(fun (name, pin) ->
             `Object [ "name", text name; "pin", text pin ])) )
    ; "lifetime", lifetime_to_json a.lifetime
    ; "created_at", text (P.Timestamp.to_string a.created_at)
    ; "inference_target", nullable a.inference_target ~f:Inference.Request.Target.to_json
    ]
;;

let stage_to_string = function
  | Reserved -> "reserved"
  | Artifact_installed -> "artifact_installed"
  | Child_installed -> "child_installed"
  | Linked -> "linked"
;;

let revocation_to_string = function
  | Parent_stopped -> "parent_stopped"
  | Parent_deleted -> "parent_deleted"
  | Authority_changed -> "authority_changed"
  | Admission_failed -> "admission_failed"
;;

let record_to_json (record : record) =
  `Object
    [ "key", key_to_json record.key
    ; "request_sha256", text record.request_sha256
    ; "admission", admission_to_json record.admission
    ; "stage", text (stage_to_string record.stage)
    ; ( "revocation"
      , nullable record.revocation ~f:(fun value -> text (revocation_to_string value)) )
    ; ( "artifact_collection"
      , nullable record.artifact_collection ~f:(fun Prepared -> text "prepared") )
    ]
;;

let identifier decode json =
  Result.bind (F.string json) ~f:(fun value -> F.protocol (decode value))
;;

let generation json =
  Result.bind (F.decimal json) ~f:(fun value ->
    if Int64.(value > of_int Int.max_value)
    then F.invalid "parent_generation" "generation exceeds native range"
    else Ok (Int64.to_int_exn value))
;;

let key_of_json json =
  let open Result.Let_syntax in
  let%bind parent_session_id =
    F.required json "parent_session_id" (identifier P.Id.Session.of_string)
  in
  let%bind parent_generation = F.required json "parent_generation" generation in
  let%bind principal_id =
    F.required json "principal_id" (identifier P.Id.Principal.of_string)
  in
  let%map idempotency_key =
    F.required json "idempotency_key" (identifier P.Idempotency_key.of_string)
  in
  Key.{ parent_session_id; parent_generation; principal_id; idempotency_key }
;;

let lifetime_of_json json =
  let open Result.Let_syntax in
  match%bind F.required json "kind" F.string with
  | "owned" -> Ok Admission.Owned
  | "invocation_owned" ->
    let%map invocation_id =
      F.required json "invocation_id" (identifier P.Id.Invocation.of_string)
    in
    Admission.Invocation_owned { invocation_id }
  | "independent" ->
    let%map authorization_sha256 = F.required json "authorization_sha256" F.digest in
    Admission.Independent { authorization_sha256 }
  | _ -> F.invalid "lifetime" "unknown lifetime"
;;

let admission_of_json ~limits json =
  let open Result.Let_syntax in
  let%bind child_session_id =
    F.required json "child_session_id" (identifier P.Id.Session.of_string)
  in
  let%bind revision_id =
    F.required json "revision_id" (identifier P.Id.Prompt_revision.of_string)
  in
  let%bind transaction_id =
    F.required json "transaction_id" (identifier P.Id.Transaction.of_string)
  in
  let%bind manifest_sha256 = F.required json "manifest_sha256" F.digest in
  let%bind parent_revision_id =
    F.required json "parent_revision_id" (identifier P.Id.Prompt_revision.of_string)
  in
  let%bind parent_stop_epoch = F.optional json "parent_stop_epoch" F.decimal in
  let%bind authority_sha256 = F.required json "authority_sha256" F.digest in
  let%bind authored_tool =
    F.optional json "authored_tool" (fun json ->
      let%bind name = F.required json "name" F.string in
      let%map source_sha256 = F.required json "source_sha256" F.digest in
      Admission.{ name; source_sha256 })
  in
  let%bind pins = F.required json "capability_pins" F.array in
  let%bind capability_pins =
    List.map pins ~f:(fun json ->
      let%bind name = F.required json "name" F.string in
      let%map pin = F.required json "pin" F.digest in
      name, pin)
    |> Result.all
  in
  let%bind lifetime = F.required json "lifetime" lifetime_of_json in
  let%bind created_at = F.required json "created_at" (identifier P.Timestamp.of_string) in
  let%map inference_target =
    F.required json "inference_target" (fun json ->
      Inference.Request.Target.of_json json ~limits
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:(fun error ->
        D.Error.Invalid_field
          { path = [ "inference_target" ]
          ; reason = Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error)
          }))
  in
  Admission.
    { child_session_id
    ; revision_id
    ; transaction_id
    ; manifest_sha256
    ; parent_revision_id
    ; parent_stop_epoch
    ; authority_sha256
    ; authored_tool
    ; capability_pins
    ; lifetime
    ; created_at
    ; inference_target
    }
;;

let record_of_json ~limits json =
  let open Result.Let_syntax in
  let%bind key = F.required json "key" key_of_json in
  let%bind request_sha256 = F.required json "request_sha256" F.digest in
  let%bind admission = F.required json "admission" (admission_of_json ~limits) in
  let%bind stage =
    F.required json "stage" (fun json ->
      match%bind F.string json with
      | "reserved" -> Ok Reserved
      | "artifact_installed" -> Ok Artifact_installed
      | "child_installed" -> Ok Child_installed
      | "linked" -> Ok Linked
      | _ -> F.invalid "stage" "unknown stage")
  in
  let%bind revocation =
    F.optional json "revocation" (fun json ->
      match%bind F.string json with
      | "parent_stopped" -> Ok Parent_stopped
      | "parent_deleted" -> Ok Parent_deleted
      | "authority_changed" -> Ok Authority_changed
      | "admission_failed" -> Ok Admission_failed
      | _ -> F.invalid "revocation" "unknown revocation")
  in
  let%bind artifact_collection =
    F.optional json "artifact_collection" (fun json ->
      match%bind F.string json with
      | "prepared" -> Ok Prepared
      | _ -> F.invalid "artifact_collection" "unknown collection state")
  in
  let record =
    { key
    ; request_sha256
    ; admission
    ; stage
    ; revocation
    ; artifact_collection
    ; preservation = None
    }
  in
  let%map () =
    validate record
    |> Result.map_error ~f:(fun error ->
      D.Error.Invalid_field
        { path = []; reason = Sexp.to_string_hum (Store_error.sexp_of_t error) })
  in
  record
;;

let record_shape =
  let scalar = D.Shape.value in
  let key =
    F.shape
      [ "parent_session_id", scalar
      ; "parent_generation", scalar
      ; "principal_id", scalar
      ; "idempotency_key", scalar
      ]
  in
  let authored = F.shape [ "name", scalar; "source_sha256", scalar ] in
  let pins =
    D.Shape.array
      (F.shape [ "name", scalar; "pin", scalar ])
      ~identity_field:(Some "name")
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let lifetime =
    D.Shape.tagged_object
      ~discriminator:"kind"
      [ "owned", F.shape [ "kind", scalar ]
      ; "invocation_owned", F.shape [ "kind", scalar; "invocation_id", scalar ]
      ; "independent", F.shape [ "kind", scalar; "authorization_sha256", scalar ]
      ]
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let admission =
    F.shape
      [ "child_session_id", scalar
      ; "revision_id", scalar
      ; "transaction_id", scalar
      ; "manifest_sha256", scalar
      ; "parent_revision_id", scalar
      ; "parent_stop_epoch", scalar
      ; "authority_sha256", scalar
      ; "authored_tool", D.Shape.nullable authored
      ; "capability_pins", pins
      ; "lifetime", lifetime
      ; "created_at", scalar
      ; "inference_target", scalar
      ]
  in
  F.shape
    [ "key", key
    ; "request_sha256", scalar
    ; "admission", admission
    ; "stage", scalar
    ; "revocation", scalar
    ; "artifact_collection", scalar
    ]
;;

let record_codec ~limits =
  D.Domain_codec.create
    ~limits
    ~kind:"delegation.intent"
    ~version:6
    ~shape:record_shape
    ~supported_semantics:[]
    ~decode:(record_of_json ~limits)
    ~encode:(fun record -> Ok (record_to_json record))
;;

let encode_document record ~limits =
  let open Result.Let_syntax in
  let%bind codec = record_codec ~limits in
  let carrier =
    Option.value_map
      record.preservation
      ~default:(D.Extension_carrier.of_authored_value record)
      ~f:(fun previous -> D.Extension_carrier.with_value previous record)
  in
  D.Domain_codec.encode codec carrier
;;

let original_admission_json (record : record) =
  match Option.bind record.preservation ~f:D.Extension_carrier.template with
  | None -> admission_to_json record.admission
  | Some document ->
    (match D.Json.field (D.Document.payload document) ~name:"admission" with
     | Value admission -> admission
     | Absent | Null -> raise_s [%sexp "validated delegation admission is absent"])
;;

let reference (record : record) =
  Reference.
    { key = record.key
    ; child_session_id = record.admission.child_session_id
    ; revision_id = record.admission.revision_id
    ; request_sha256 = record.request_sha256
    ; admission_sha256 =
        (match record.admission.inference_target with
         | None -> Admission.sexp_of_t record.admission |> Sexp.to_string_mach |> digest
         | Some _ -> original_admission_json record |> Jsonaf.to_string |> digest)
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

let encode_legacy record =
  let open Result.Let_syntax in
  let%bind () = validate record in
  Frame.encode
    ~max_payload_length
    ~flags:0
    (Persisted.sexp_of_t
       { version =
           (match
              ( record.admission.lifetime
              , record.admission.authored_tool
              , record.artifact_collection )
            with
            | Invocation_owned _, _, _ -> 5
            | _, Some _, _ -> 4
            | _, None, Some _ -> 3
            | _, None, None -> 2)
       ; record
       }
     |> Sexp.to_string_mach)
  |> Result.map_error ~f:(fun _ ->
    Store_error.Corrupt "delegation intent exceeds its frame limit")
;;

let encode record =
  match record.admission.inference_target with
  | None -> encode_legacy record
  | Some _ ->
    let open Result.Let_syntax in
    let%bind () = validate record in
    let%bind limits = F.limits ~max_bytes:max_payload_length |> F.store in
    let%bind document = encode_document record ~limits |> F.store in
    Document_record.encode document ~limits ~flags:0 |> Result.map_error ~f:F.record_error
;;

let decode ~name contents =
  let open Result.Let_syntax in
  match Frame.decode ~max_payload_length ~contents ~offset:0 with
  | Ok (Complete { frame; next_offset })
    when next_offset = String.length contents && Frame.flags frame = 0 ->
    let payload = Frame.payload frame in
    if String.is_prefix (String.lstrip payload) ~prefix:"{"
    then (
      let%bind limits = F.limits ~max_bytes:max_payload_length |> F.store in
      let%bind stored =
        Document_record.of_frame frame ~limits ~expected_digest:None
        |> Result.map_error ~f:F.record_error
      in
      let%bind codec = record_codec ~limits |> F.store in
      let%bind carrier =
        D.Domain_codec.decode codec (Document_record.document stored) |> F.store
      in
      let record =
        { (D.Extension_carrier.value carrier) with
          preservation = Some (D.Extension_carrier.with_value carrier ())
        }
      in
      if String.equal name (filename record.key)
      then Ok record
      else corrupt "delegation intent filename differs from its scoped key")
    else (
      let%bind persisted =
        Result.try_with (fun () ->
          Frame.payload frame |> Sexp.of_string |> Persisted.t_of_sexp)
        |> Result.map_error ~f:(fun _ ->
          Store_error.Corrupt "invalid delegation intent payload")
      in
      let%bind () =
        match persisted.version, persisted.record.admission.lifetime with
        | version, _ when version > 5 -> Error (Store_error.Schema_too_new version)
        | 5, Invocation_owned _ -> Ok ()
        | _, Invocation_owned _ ->
          corrupt "invocation-owned delegation requires version 5"
        | 5, _ -> corrupt "version 5 requires invocation ownership"
        | _, (Owned | Independent _) ->
          (match persisted.version, persisted.record.admission.authored_tool with
           | 1, None
             when Option.is_none persisted.record.admission.parent_stop_epoch
                  && Option.is_none persisted.record.artifact_collection -> Ok ()
           | 2, None when Option.is_none persisted.record.artifact_collection -> Ok ()
           | 3, None when Option.is_some persisted.record.artifact_collection -> Ok ()
           | 4, Some _ -> Ok ()
           | _ -> corrupt "invalid delegation intent version")
      in
      let%bind () =
        if
          Option.is_none persisted.record.admission.inference_target
          && Option.is_none persisted.record.preservation
        then Ok ()
        else corrupt "captured inference target requires named JSON ledger version 6"
      in
      let%bind () = validate persisted.record in
      match String.equal name (filename persisted.record.key) with
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
