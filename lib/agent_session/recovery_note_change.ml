open! Core
module C = Recovery_notes.Catalog
module X = Persistence_codec
module P = Agent_protocol
module J = P.Json_codec

type t =
  { owner : P.Session_ref.t
  ; identity : C.Mutation_identity.t
  ; operation : C.mutation
  }

let create ~owner ~identity operation = { owner; identity; operation }
let error code message = P.Error.create code ~message ~retryable:false ()

let mutation_error = function
  | C.Note_precondition Note_not_found -> error Conflict "recovery note is unavailable"
  | Note_precondition (Stale_note_revision _) ->
    error Conflict "recovery note revision changed"
  | Stale_catalog_revision _ -> error Conflict "recovery note catalog revision changed"
  | Path_already_exists -> error Conflict "recovery note path already exists"
  | Empty_append -> error Invalid_request "recovery note append must be nonempty"
  | Note_byte_limit | Catalog_byte_limit | Catalog_entry_limit ->
    error Resource_limit "recovery note storage capacity exceeded"
  | Revision_exhausted -> error Resource_limit "recovery note revision exhausted"
  | Idempotency_conflict ->
    error
      Idempotency_conflict
      "recovery note mutation identity was used for different arguments"
  | Invalid_identity -> error Invalid_request "invalid recovery note mutation identity"
  | Document_error _ ->
    error
      Persistence_error
      "recovery note mutation could not be admitted to durable storage"
;;

let evaluate t ~previous ~session_id =
  let open Result.Let_syntax in
  let%bind catalog = Recovery_note_session.for_owner previous ~owner:t.owner in
  let%bind () = Recovery_note_session.validate catalog ~session_id in
  C.apply catalog ~identity:t.identity t.operation |> Result.map_error ~f:mutation_error
;;

let apply t ~previous ~session_id =
  match%bind.Result evaluate t ~previous ~session_id with
  | Applied (catalog, _) -> Ok catalog
  | Replayed _ ->
    Error (error Invalid_request "recovery note journal operation was already applied")
;;

let counter_json counter = `String (Int64.to_string counter)

let counter decode json =
  let open Result.Let_syntax in
  let%bind text = J.string json in
  match Int64.of_string_opt text with
  | Some value when String.equal (Int64.to_string value) text -> decode value
  | Some _ | None -> Error (P.Error.invalid_request "invalid recovery note counter")
;;

let selection_json { C.id; revision } =
  `Object
    [ "id", counter_json (C.Note_id.to_int64 id)
    ; "revision", counter_json (C.Note_revision.to_int64 revision)
    ]
;;

let selection json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind id = X.required fields "id" (counter C.Note_id.of_int64) in
  let%map revision = X.required fields "revision" (counter C.Note_revision.of_int64) in
  { C.id; revision }
;;

let identity_json = function
  | C.Mutation_identity.Client { principal; key } ->
    `Object
      [ "kind", `String "client"
      ; "principal", P.Id.Principal.to_json principal
      ; "key", P.Idempotency_key.to_json key
      ]
  | Native invocation ->
    `Object [ "kind", `String "native"; "invocation", P.Id.Invocation.to_json invocation ]
;;

let identity json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  match%bind X.required fields "kind" J.string with
  | "client" ->
    let%bind principal = X.required fields "principal" P.Id.Principal.of_json in
    let%map key = X.required fields "key" P.Idempotency_key.of_json in
    C.Mutation_identity.Client { principal; key }
  | "native" ->
    let%map invocation = X.required fields "invocation" P.Id.Invocation.of_json in
    C.Mutation_identity.Native invocation
  | _ -> Error (P.Error.invalid_request "unknown recovery note identity kind")
;;

let operation_json operation =
  let content t = `String (C.Content.to_string t) in
  let edit kind { C.expected; content = body } =
    [ "kind", `String kind; "expected", selection_json expected; "content", content body ]
  in
  `Object
    (match operation with
     | C.Create { expected_catalog_revision; path; content = body } ->
       [ "kind", `String "create"
       ; ( "expected_catalog_revision"
         , counter_json (C.Catalog_revision.to_int64 expected_catalog_revision) )
       ; "path", `String (C.Note_path.to_string path)
       ; "content", content body
       ]
     | Replace request -> edit "replace" request
     | Append request -> edit "append" request
     | Delete expected ->
       [ "kind", `String "delete"; "expected", selection_json expected ])
;;

let operation json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let string decode raw = Result.bind (J.string raw) ~f:decode in
  let content () = X.required fields "content" (string C.Content.of_string) in
  let expected () = X.required fields "expected" selection in
  match%bind X.required fields "kind" J.string with
  | "create" ->
    let%bind expected_catalog_revision =
      X.required fields "expected_catalog_revision" (counter C.Catalog_revision.of_int64)
    in
    let%bind path = X.required fields "path" (string C.Note_path.of_string) in
    let%map content = content () in
    C.Create { expected_catalog_revision; path; content }
  | ("replace" | "append") as kind ->
    let%bind expected = expected () in
    let%map content = content () in
    let edit = { C.expected; content } in
    if String.equal kind "replace" then C.Replace edit else C.Append edit
  | "delete" ->
    let%map expected = expected () in
    C.Delete expected
  | _ -> Error (P.Error.invalid_request "unknown recovery note journal operation")
;;

let to_json t =
  `Object
    [ "owner", P.Session_ref.to_json t.owner
    ; "identity", identity_json t.identity
    ; "operation", operation_json t.operation
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind owner = X.required fields "owner" P.Session_ref.of_json in
  let%bind identity = X.required fields "identity" identity in
  let%map operation = X.required fields "operation" operation in
  { owner; identity; operation }
;;

let shape =
  let identity =
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ "client", X.fields_shape [ "kind"; "principal"; "key" ]
      ; "native", X.fields_shape [ "kind"; "invocation" ]
      ]
  in
  let selection = X.fields_shape [ "id"; "revision" ] in
  let kind = Document_schema.Shape.value in
  let edit = X.shape_exn [ "kind", kind; "expected", selection; "content", kind ] in
  let operation =
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ ( "create"
        , X.fields_shape [ "kind"; "expected_catalog_revision"; "path"; "content" ] )
      ; "replace", edit
      ; "append", edit
      ; "delete", X.shape_exn [ "kind", kind; "expected", selection ]
      ]
  in
  X.shape_exn
    [ "owner", X.fields_shape [ "server_id"; "session_id" ]
    ; "identity", identity
    ; "operation", operation
    ]
;;
