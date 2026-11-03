open! Core
module P = Agent_protocol
module X = Persistence_codec

type kind =
  | Create
  | Edit
  | Enable
  | Pause
  | Retire
[@@deriving equal, sexp_of]

let kind_string = function
  | Create -> "create"
  | Edit -> "edit"
  | Enable -> "enable"
  | Pause -> "pause"
  | Retire -> "retire"
;;

let kind_of_json =
  P.Json_codec.enum
    ~name:"recurrence command"
    [ "create", Create; "edit", Edit; "enable", Enable; "pause", Pause; "retire", Retire ]
;;

let lifecycle_json revision =
  X.int64_json (P.Session_lifecycle.Revision.to_int64 revision)
;;

let lifecycle_of_json json =
  Result.bind (X.nonnegative_int64 json) ~f:P.Session_lifecycle.Revision.of_int64
;;

type t =
  { principal_id : P.Id.Principal.t
  ; key : P.Idempotency_key.t
  ; request_sha256 : string
  ; recurrence_id : P.Id.Recurrence.t
  ; definition_revision : int64
  ; control_revision : int64
  ; catalog_revision : int64
  ; kind : kind
  ; source : P.Run_source.t option
  ; birth_lifecycle_revision : P.Session_lifecycle.Revision.t option
  ; accepted_at : P.Timestamp.t
  }
[@@deriving equal, sexp_of]

let create
      ~principal_id
      ~key
      ~request_sha256
      ~recurrence_id
      ~definition_revision
      ~control_revision
      ~catalog_revision
      ~kind
      ~birth_lifecycle_revision
      ~source
      ~accepted_at
  =
  let open Result.Let_syntax in
  let%bind () =
    match kind, source, birth_lifecycle_revision with
    | (Create | Edit | Enable), Some _, Some birth ->
      Result.map
        (P.Session_lifecycle.Revision.of_int64
           (P.Session_lifecycle.Revision.to_int64 birth))
        ~f:(fun _ -> ())
    | (Pause | Retire), None, None -> Ok ()
    | _ ->
      Error
        (P.Error.invalid_request
           "recurrence receipt authority metadata disagrees with kind")
  in
  let%bind () =
    match source with
    | None -> Ok ()
    | Some source -> P.Run_source.validate source
  in
  let%bind _ = P.Id.Principal.of_json (P.Id.Principal.to_json principal_id) in
  let%bind _ = P.Id.Recurrence.of_json (P.Id.Recurrence.to_json recurrence_id) in
  let%bind _ = P.Idempotency_key.of_json (P.Idempotency_key.to_json key) in
  let%bind _ = P.Timestamp.of_json (P.Timestamp.to_json accepted_at) in
  if
    Int64.(definition_revision < 0L || control_revision < 0L || catalog_revision <= 0L)
    || String.length request_sha256 <> 64
    || not
         (String.for_all request_sha256 ~f:(function
            | '0' .. '9' | 'a' .. 'f' -> true
            | _ -> false))
  then Error (P.Error.invalid_request "invalid recurrence command receipt")
  else
    Ok
      { principal_id
      ; key
      ; request_sha256
      ; recurrence_id
      ; definition_revision
      ; control_revision
      ; catalog_revision
      ; kind
      ; birth_lifecycle_revision
      ; source
      ; accepted_at
      }
;;

let to_json t =
  `Object
    [ "principal_id", P.Id.Principal.to_json t.principal_id
    ; "key", P.Idempotency_key.to_json t.key
    ; "request_sha256", `String t.request_sha256
    ; "recurrence_id", P.Id.Recurrence.to_json t.recurrence_id
    ; "definition_revision", X.int64_json t.definition_revision
    ; "control_revision", X.int64_json t.control_revision
    ; "kind", `String (kind_string t.kind)
    ; "birth_lifecycle_revision", X.option_json lifecycle_json t.birth_lifecycle_revision
    ; "catalog_revision", X.int64_json t.catalog_revision
    ; "source", X.option_json P.Run_source.to_json t.source
    ; "accepted_at", P.Timestamp.to_json t.accepted_at
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    Document_schema.Json.validate ~limits:Agent_store.Document_fields.limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  let%bind f = X.object_ json in
  let%bind principal_id = X.required f "principal_id" P.Id.Principal.of_json in
  let%bind key = X.required f "key" P.Idempotency_key.of_json in
  let%bind request_sha256 = X.required f "request_sha256" P.Json_codec.string in
  let%bind recurrence_id = X.required f "recurrence_id" P.Id.Recurrence.of_json in
  let%bind definition_revision = X.required f "definition_revision" X.nonnegative_int64 in
  let%bind control_revision = X.required f "control_revision" X.nonnegative_int64 in
  let%bind kind = X.required f "kind" kind_of_json in
  let%bind birth_lifecycle_revision =
    X.required f "birth_lifecycle_revision" (X.nullable lifecycle_of_json)
  in
  let%bind catalog_revision = X.required f "catalog_revision" X.nonnegative_int64 in
  let%bind source = X.required f "source" (X.nullable P.Run_source.of_json) in
  let%bind accepted_at = X.required f "accepted_at" P.Timestamp.of_json in
  create
    ~principal_id
    ~key
    ~request_sha256
    ~recurrence_id
    ~definition_revision
    ~control_revision
    ~catalog_revision
    ~kind
    ~birth_lifecycle_revision
    ~source
    ~accepted_at
;;

let shape =
  X.shape_exn
    [ "principal_id", Document_schema.Shape.value
    ; "key", Document_schema.Shape.value
    ; "request_sha256", Document_schema.Shape.value
    ; "recurrence_id", Document_schema.Shape.value
    ; "definition_revision", Document_schema.Shape.value
    ; "control_revision", Document_schema.Shape.value
    ; "kind", Document_schema.Shape.value
    ; "birth_lifecycle_revision", Document_schema.Shape.value
    ; "catalog_revision", Document_schema.Shape.value
    ; "source", X.nullable_shape Run_record_shapes.source
    ; "accepted_at", Document_schema.Shape.value
    ]
;;
