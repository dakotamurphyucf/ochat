open! Core
module P = Agent_protocol
module X = Persistence_codec

module Change = struct
  type t =
    | Replace_project of Instruction_capture.t
    | Replace_skills of Instruction_capture.t
    | Remove_project
    | Remove_skills of Instruction_content.Id.t list
    | Clear

  let to_json change =
    let kind, fields =
      match change with
      | Replace_project capture ->
        "replace_project", [ "capture", `String (Instruction_capture.id capture) ]
      | Replace_skills capture ->
        "replace_skills", [ "capture", `String (Instruction_capture.id capture) ]
      | Remove_project -> "remove_project", []
      | Remove_skills ids ->
        ( "remove_skills"
        , [ ( "files"
            , `Array
                (List.map ids ~f:(fun id -> `String (Instruction_capture.file_key id))) )
          ] )
      | Clear -> "clear", []
    in
    `Object (("kind", `String kind) :: fields)
  ;;
end

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; expected_revision : int64
  ; key : P.Idempotency_key.t
  ; change : Change.t
  ; digest : string
  }

let session_id t = t.session_id
let generation t = t.generation
let expected_revision t = t.expected_revision
let key t = t.key
let change t = t.change
let digest t = t.digest

let create ~session_id ~generation ~expected_revision ~key ~change =
  let open Result.Let_syntax in
  let%bind _ = P.Id.Session.of_json (P.Id.Session.to_json session_id) in
  let%bind _ = P.Idempotency_key.of_json (P.Idempotency_key.to_json key) in
  let%bind () =
    match change with
    | Change.Replace_project capture ->
      if Instruction_capture.Kind.equal (Instruction_capture.kind capture) Project
      then Ok ()
      else Error (P.Error.invalid_request "expected project capture")
    | Replace_skills capture ->
      if Instruction_capture.Kind.equal (Instruction_capture.kind capture) Skills
      then Ok ()
      else Error (P.Error.invalid_request "expected skills capture")
    | Remove_skills ids ->
      if
        List.length ids <= 64
        && List.length ids
           = Set.length
               (String.Set.of_list (List.map ids ~f:Instruction_capture.file_key))
      then Ok ()
      else Error (P.Error.invalid_request "invalid removed skill identities")
    | Remove_project | Clear -> Ok ()
  in
  if generation < 0 || Int64.(expected_revision < 0L)
  then Error (P.Error.invalid_request "invalid instruction command frontier")
  else (
    let raw =
      `Object
        [ "kind", `String "instruction_command_v1"
        ; "session_id", P.Id.Session.to_json session_id
        ; "generation", X.host_counter_to_json generation
        ; "expected_revision", X.int64_json expected_revision
        ; "key", P.Idempotency_key.to_json key
        ; "change", Change.to_json change
        ]
    in
    let digest = Digestif.SHA256.(digest_string (Jsonaf.to_string raw) |> to_hex) in
    Ok { session_id; generation; expected_revision; key; change; digest })
;;

type request = t

module Receipt = struct
  type t =
    { principal_id : P.Id.Principal.t
    ; key : P.Idempotency_key.t
    ; request_sha256 : string
    ; session_id : P.Id.Session.t
    ; generation : int
    ; revision : int64
    ; accepted_at : P.Timestamp.t
    }
  [@@deriving equal, sexp_of]

  let checked
        ~principal_id
        ~key
        ~request_sha256
        ~session_id
        ~generation
        ~revision
        ~accepted_at
    =
    let open Result.Let_syntax in
    let%bind _ = P.Id.Principal.of_json (P.Id.Principal.to_json principal_id) in
    let%bind _ = P.Id.Session.of_json (P.Id.Session.to_json session_id) in
    let%bind _ = P.Idempotency_key.of_json (P.Idempotency_key.to_json key) in
    let%bind _ = P.Timestamp.of_json (P.Timestamp.to_json accepted_at) in
    if
      generation < 0
      || Int64.(revision <= 0L)
      || String.length request_sha256 <> 64
      || not
           (String.for_all request_sha256 ~f:(function
              | '0' .. '9' | 'a' .. 'f' -> true
              | _ -> false))
    then Error (P.Error.invalid_request "invalid instruction receipt")
    else
      Ok
        { principal_id
        ; key
        ; request_sha256
        ; session_id
        ; generation
        ; revision
        ; accepted_at
        }
  ;;

  let create ~principal_id ~(request : request) ~revision ~accepted_at =
    checked
      ~principal_id
      ~key:request.key
      ~request_sha256:request.digest
      ~session_id:request.session_id
      ~generation:request.generation
      ~revision
      ~accepted_at
  ;;

  let identity t =
    Jsonaf.to_string
      (`Array [ P.Id.Principal.to_json t.principal_id; P.Idempotency_key.to_json t.key ])
  ;;

  let to_json t =
    `Object
      [ "principal_id", P.Id.Principal.to_json t.principal_id
      ; "key", P.Idempotency_key.to_json t.key
      ; "request_sha256", `String t.request_sha256
      ; "session_id", P.Id.Session.to_json t.session_id
      ; "generation", X.host_counter_to_json t.generation
      ; "revision", X.int64_json t.revision
      ; "accepted_at", P.Timestamp.to_json t.accepted_at
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = X.object_ raw in
    let%bind principal_id = X.required f "principal_id" P.Id.Principal.of_json in
    let%bind key = X.required f "key" P.Idempotency_key.of_json in
    let%bind request_sha256 = X.required f "request_sha256" P.Json_codec.string in
    let%bind session_id = X.required f "session_id" P.Id.Session.of_json in
    let%bind generation = X.required f "generation" X.host_counter_of_json in
    let%bind revision = X.required f "revision" X.nonnegative_int64 in
    let%bind accepted_at = X.required f "accepted_at" P.Timestamp.of_json in
    checked
      ~principal_id
      ~key
      ~request_sha256
      ~session_id
      ~generation
      ~revision
      ~accepted_at
  ;;

  let shape =
    X.fields_shape
      [ "principal_id"
      ; "key"
      ; "request_sha256"
      ; "session_id"
      ; "generation"
      ; "revision"
      ; "accepted_at"
      ]
  ;;
end
