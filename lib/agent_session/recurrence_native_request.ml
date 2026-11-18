open! Core
module P = Agent_protocol
module J = P.Json_codec
module Effect = Recurrence_native_admission.Effect

module Target = struct
  type t =
    | Existing_session of
        { session : P.Session_ref.t
        ; input : P.Session.Message_content.t
        }
    | Authored_child of
        { name : string
        ; mode : P.Run.Mode.t
        }
  [@@deriving sexp_of]
end

module Basis = struct
  type t =
    { generation : int
    ; expected_catalog_revision : int64
    ; expected_control_revision : int64 option
    ; source : P.Run_source.t option
    ; expected_lifecycle_revision : P.Session_lifecycle.Revision.t option
    ; key : P.Idempotency_key.t
    }
end

module Capture = struct
  type t =
    { schedule : Recurrence_definition.Schedule.t
    ; target : Target.t
    ; policy : Recurrence_policy.t
    }
end

type t =
  | Create of
      { id : P.Id.Recurrence.t
      ; basis : Basis.t
      ; capture : Capture.t
      }
  | Edit of
      { id : P.Id.Recurrence.t
      ; basis : Basis.t
      ; capture : Capture.t
      }
  | Pause of
      { id : P.Id.Recurrence.t
      ; basis : Basis.t
      }
  | Resume of
      { id : P.Id.Recurrence.t
      ; basis : Basis.t
      }
  | Cancel of
      { id : P.Id.Recurrence.t
      ; basis : Basis.t
      }
  | List
  | Get of P.Id.Recurrence.t
  | Status of P.Id.Recurrence.t
  | History of P.Id.Recurrence.t

let invalid message = Error (P.Error.invalid_request message)

let limits =
  Document_schema.Limits.create
    ~max_bytes:393216
    ~max_depth:32
    ~max_fields:4096
    ~max_nodes:8192
  |> function
  | Ok limits -> limits
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
;;

let decode_text_exn text =
  Document_schema.Json.decode ~limits text
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let fields json ~names =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  if
    List.exists (J.to_alist fields) ~f:(fun (name, _) ->
      not (List.mem names name ~equal:String.equal))
  then invalid "unknown recurrence request field"
  else Ok fields
;;

let revision json =
  let%bind.Result text = J.string json in
  match Int64.of_string_opt text with
  | Some n when Int64.(n >= 0L) && String.equal (Int64.to_string n) text -> Ok n
  | Some _ | None -> invalid "recurrence revision must be canonical nonnegative decimal"
;;

let source json =
  let%bind.Result _ =
    fields
      json
      ~names:[ "script_id"; "source_sha256"; "generation"; "installation_epoch" ]
  in
  P.Run_source.of_json json
;;

let session json =
  let%bind.Result _ = fields json ~names:[ "server_id"; "session_id" ] in
  P.Session_ref.of_json json
;;

let attachment json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind kind = J.required_as f "source" J.string in
  let%bind _ =
    fields
      json
      ~names:
        ([ "kind"; "media_type"; "byte_length"; "digest"; "display_name" ]
         @
         match kind with
         | "stored" -> [ "source"; "blob_id" ]
         | "inline_base64" -> [ "source"; "inline_base64" ]
         | _ -> [])
  in
  P.Blob.Input.of_json json
;;

let input json =
  let open Result.Let_syntax in
  let%bind _ = fields json ~names:[ "kind"; "text"; "attachments" ] in
  let%bind f = J.fields json in
  let%bind _ = J.required_as f "attachments" (J.list attachment) in
  let%bind encoded = J.canonical_string json in
  if String.length encoded > 262144
  then invalid "recurrence input exceeds 256 KiB"
  else P.Session.Message_content.of_json json
;;

let target json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind kind = J.required_as f "kind" J.string in
  match kind with
  | "existing_session" ->
    let%bind f = fields json ~names:[ "kind"; "session"; "input" ] in
    let%bind session = J.required_as f "session" session in
    let%map input = J.required_as f "input" input in
    Target.Existing_session { session; input }
  | "authored_child" ->
    let%bind f = fields json ~names:[ "kind"; "name"; "mode" ] in
    let%bind name = J.required_as f "name" J.string in
    let%bind () =
      if
        String.is_empty name
        || String.length name > 1024
        || String.exists name ~f:(fun char ->
          Char.to_int char < 32 || Char.to_int char = 127)
      then invalid "invalid authored recurrence worker name"
      else Ok ()
    in
    let%map mode = J.required_as f "mode" P.Run.Mode.of_json in
    Target.Authored_child { name; mode }
  | _ -> invalid "unsupported recurrence target kind"
;;

let schedule json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind kind = J.required_as f "kind" J.string in
  let%bind _ =
    match kind with
    | "interval" -> fields json ~names:[ "kind"; "rule"; "first_due" ]
    | "calendar" -> fields json ~names:[ "kind"; "rule" ]
    | _ -> invalid "unsupported recurrence schedule kind"
  in
  Recurrence_definition.Schedule.of_json json
;;

let policy json =
  let open Result.Let_syntax in
  let%bind f =
    fields
      json
      ~names:[ "misfire"; "grace_ms"; "max_pending"; "max_history"; "max_calendar_steps" ]
  in
  let%bind _ = J.required_as f "grace_ms" revision in
  let%bind raw = J.required f "misfire" in
  let%bind mf = J.fields raw in
  let%bind kind = J.required_as mf "kind" J.string in
  let%bind _ =
    fields
      raw
      ~names:(if String.equal kind "catch_up" then [ "kind"; "limit" ] else [ "kind" ])
  in
  Recurrence_policy.of_json json
;;

let basis json ~create ~needs_source =
  let open Result.Let_syntax in
  let%bind f =
    fields
      json
      ~names:
        [ "generation"
        ; "expected_catalog_revision"
        ; "expected_control_revision"
        ; "source"
        ; "expected_lifecycle_revision"
        ; "key"
        ]
  in
  let%bind generation =
    J.required_as f "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind expected_catalog_revision =
    J.required_as f "expected_catalog_revision" revision
  in
  let%bind expected_control_revision =
    J.required_as f "expected_control_revision" (Persistence_codec.nullable revision)
  in
  let%bind source = J.required_as f "source" (Persistence_codec.nullable source) in
  let%bind expected_lifecycle_revision =
    J.required_as
      f
      "expected_lifecycle_revision"
      (Persistence_codec.nullable (fun json ->
         Result.bind (revision json) ~f:P.Session_lifecycle.Revision.of_int64))
  in
  let%bind key = J.required_as f "key" P.Idempotency_key.of_json in
  if
    Bool.equal create (Option.is_none expected_control_revision)
    && Bool.equal needs_source (Option.is_some source)
    && Bool.equal needs_source (Option.is_some expected_lifecycle_revision)
    && Option.for_all source ~f:(fun source ->
      Int.equal generation source.P.Run_source.generation)
  then
    Ok
      Basis.
        { generation
        ; expected_catalog_revision
        ; expected_control_revision
        ; source
        ; expected_lifecycle_revision
        ; key
        }
  else invalid "recurrence command kind and CAS/source basis differ"
;;

let decode ~effect_kind json =
  let open Result.Let_syntax in
  let%bind () = Document_schema.Json.validate ~limits json |> domain in
  let%bind f =
    fields
      json
      ~names:
        (match effect_kind with
         | Effect.List -> []
         | Get | Status | History -> [ "id" ]
         | Pause | Resume | Cancel -> [ "id"; "basis" ]
         | Create | Edit -> [ "id"; "basis"; "capture" ])
  in
  let id () = J.required_as f "id" P.Id.Recurrence.of_json in
  let basis ~create ~needs_source =
    J.required_as f "basis" (basis ~create ~needs_source)
  in
  match effect_kind with
  | List -> Ok List
  | Get -> Result.map (id ()) ~f:(fun id -> Get id)
  | Status -> Result.map (id ()) ~f:(fun id -> Status id)
  | History -> Result.map (id ()) ~f:(fun id -> History id)
  | Pause ->
    let%bind id = id () in
    let%map basis = basis ~create:false ~needs_source:false in
    Pause { id; basis }
  | Resume ->
    let%bind id = id () in
    let%map basis = basis ~create:false ~needs_source:true in
    Resume { id; basis }
  | Cancel ->
    let%bind id = id () in
    let%map basis = basis ~create:false ~needs_source:false in
    Cancel { id; basis }
  | Create | Edit ->
    let%bind id = id () in
    let%bind basis = basis ~create:(Effect.equal effect_kind Create) ~needs_source:true in
    let%bind capture =
      J.required_as f "capture" (fun json ->
        let%bind f = fields json ~names:[ "schedule"; "target"; "policy" ] in
        let%bind schedule = J.required_as f "schedule" schedule in
        let%bind target = J.required_as f "target" target in
        let%map policy = J.required_as f "policy" policy in
        Capture.{ schedule; target; policy })
    in
    if Effect.equal effect_kind Create
    then Ok (Create { id; basis; capture })
    else Ok (Edit { id; basis; capture })
;;

let parameters = Recurrence_native_schema.parameters
