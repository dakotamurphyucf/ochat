open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

type t = Session_index_entry.t list

let count json =
  let open Result.Let_syntax in
  let%bind value = F.decimal json in
  match Int64.to_int value with
  | Some value -> Ok value
  | None -> F.invalid "count" "machine integer overflow"
;;

let nullable decode = function
  | `Null -> Ok None
  | json -> Result.map (decode json) ~f:Option.some
;;

let entry_decode json =
  let open Result.Let_syntax in
  let%bind id =
    F.required json "session_id" (fun value -> P.Id.Session.of_json value |> F.protocol)
  in
  let%bind session = F.required json "session" Session_record_document.of_json in
  let%bind runnable_job_count = F.required json "runnable_job_count" count in
  let%bind deliverable_job_count = F.required json "deliverable_job_count" count in
  let%bind earliest_schedule_due =
    F.required
      json
      "earliest_schedule_due"
      (nullable (fun value -> P.Timestamp.of_json value |> F.protocol))
  in
  let%bind owner_grace_deadline =
    F.required
      json
      "owner_grace_deadline"
      (nullable (fun value -> P.Timestamp.of_json value |> F.protocol))
  in
  let%bind pending_initial_start = F.required json "pending_initial_start" F.boolean in
  let%bind archived = F.required json "archived" F.boolean in
  let%bind lifecycle_revision =
    F.required json "lifecycle_revision" (fun json ->
      F.decimal json
      |> Result.bind ~f:(fun value ->
        Session_archive_record.Revision.of_int64 value |> F.protocol))
  in
  let%bind admission =
    F.required json "admission" (function
      | `String "automatic" -> Ok Session_archive_record.Admission.Automatic
      | `String "explicit_resume_required" -> Ok Explicit_resume_required
      | _ -> F.invalid "admission" "unsupported session index admission")
  in
  if not (P.Id.Session.equal id session.id)
  then F.invalid "session_id" "entry identity differs from session"
  else (
    let entry =
      Session_index_entry.
        { session
        ; runnable_job_count
        ; deliverable_job_count
        ; earliest_schedule_due
        ; owner_grace_deadline
        ; pending_initial_start
        ; archived
        ; lifecycle_revision
        ; admission
        }
    in
    match Session_index_entry.validate entry with
    | Ok () -> Ok entry
    | Error _ -> F.invalid "lifecycle" "inconsistent archive/admission/revision")
;;

let entry_encode (value : Session_index_entry.t) =
  let open Result.Let_syntax in
  let%bind () =
    match Session_index_entry.validate value with
    | Ok () -> Ok ()
    | Error _ -> F.invalid "lifecycle" "invalid lifecycle or scheduling projection"
  in
  let%map session = Session_record_document.to_json value.session in
  `Object
    [ "session_id", P.Id.Session.to_json value.session.id
    ; "session", session
    ; "runnable_job_count", F.decimal_json (Int64.of_int value.runnable_job_count)
    ; "deliverable_job_count", F.decimal_json (Int64.of_int value.deliverable_job_count)
    ; ( "earliest_schedule_due"
      , F.option_json value.earliest_schedule_due ~f:P.Timestamp.to_json )
    ; ( "owner_grace_deadline"
      , F.option_json value.owner_grace_deadline ~f:P.Timestamp.to_json )
    ; ("pending_initial_start", if value.pending_initial_start then `True else `False)
    ; ("archived", if value.archived then `True else `False)
    ; ( "lifecycle_revision"
      , F.decimal_json (Session_archive_record.Revision.to_int64 value.lifecycle_revision)
      )
    ; ( "admission"
      , match value.admission with
        | Automatic -> `String "automatic"
        | Explicit_resume_required -> `String "explicit_resume_required" )
    ]
;;

let entry_shape =
  F.shape
    [ "session_id", D.Shape.value
    ; "session", Session_record_document.shape
    ; "runnable_job_count", D.Shape.value
    ; "deliverable_job_count", D.Shape.value
    ; "earliest_schedule_due", D.Shape.value
    ; "owner_grace_deadline", D.Shape.value
    ; "pending_initial_start", D.Shape.value
    ; "archived", D.Shape.value
    ; "lifecycle_revision", D.Shape.value
    ; "admission", D.Shape.value
    ]
;;

let decode json =
  let open Result.Let_syntax in
  let%bind entries = F.required json "entries" F.array in
  List.map entries ~f:entry_decode |> Result.all
;;

let encode entries =
  Result.all (List.map entries ~f:entry_encode)
  |> Result.map ~f:(fun entries -> `Object [ "entries", `Array entries ])
;;

let entries_shape =
  D.Shape.array entry_shape ~identity_field:(Some "session_id")
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let limits =
  F.limits ~max_bytes:(64 * 1024 * 1024)
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind:"store.session_index"
    ~version:2
    ~shape:(F.shape [ "entries", entries_shape ])
    ~supported_semantics:[]
    ~decode
    ~encode
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind () =
    F.expect_versions document ~kind:"store.session_index" ~versions:[ 1; 2 ]
  in
  let%bind step =
    D.Conversion.Step.of_function
      ~kind:"store.session_index"
      ~from_version:1
      ~f:(fun payload ->
        let%bind entries = F.required payload "entries" F.array in
        let%bind entries =
          List.map entries ~f:(fun entry ->
            let%bind archived = F.required entry "archived" F.boolean in
            match entry with
            | `Object fields ->
              let%map fields =
                List.fold_result
                  [ "lifecycle_revision", `String (if archived then "1" else "0")
                  ; ( "admission"
                    , `String
                        (if archived then "explicit_resume_required" else "automatic") )
                  ]
                  ~init:fields
                  ~f:(fun fields (name, value) ->
                    match List.Assoc.find fields name ~equal:String.equal with
                    | None -> Ok (fields @ [ name, value ])
                    | Some _ ->
                      F.invalid
                        name
                        "original index field collides with lifecycle semantics")
              in
              `Object fields
            | _ -> F.invalid "entry" "index entry must be an object")
          |> Result.all
        in
        match payload with
        | `Object fields ->
          Ok
            (`Object
                (List.map fields ~f:(fun (name, value) ->
                   name, if String.equal name "entries" then `Array entries else value)))
        | _ -> F.invalid "payload" "index payload must be an object")
  in
  let%bind conversion =
    D.Conversion.create
      ~limits
      ~targets:[ "store.session_index", 2 ]
      ~max_steps:1
      ~max_operations:1
      ~steps:[ step ]
  in
  let%bind document = D.Conversion.upgrade conversion document in
  D.Domain_codec.decode codec document
;;

let to_document value = D.Domain_codec.encode codec value
