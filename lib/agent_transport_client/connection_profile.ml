open! Core

type t =
  { name : string
  ; endpoint : string
  ; home : string option
  ; expected_server : Agent_protocol.Id.Server.t option
  ; daemon_credential_file : string option
  ; document : Document_schema.Document.t option
  ; description : string
  }

let create ~home ~name ~endpoint ~expected_server ~daemon_credential_file =
  let open Result.Let_syntax in
  let%bind () =
    if String.is_empty (String.strip name) || String.length name > 256
    then Error (Agent_protocol.Error.invalid_request "invalid connection profile name")
    else Ok ()
  in
  let%bind validated = Endpoint.create ~home ~bearer_token:None endpoint in
  let%bind () =
    match Endpoint.kind validated, daemon_credential_file with
    | Unix_socket, Some _ ->
      Error (Agent_protocol.Error.invalid_request "daemon credentials require HTTP")
    | Http, Some file when String.is_empty file ->
      Error (Agent_protocol.Error.invalid_request "empty daemon credential reference")
    | Unix_socket, None | Http, None | Http, Some _ -> Ok ()
  in
  Ok
    { name
    ; endpoint
    ; home
    ; expected_server
    ; daemon_credential_file
    ; document = None
    ; description = Endpoint.description validated
    }
;;

let name t = t.name
let description t = t.description
let expected_server t = t.expected_server

let connect t ~sw ~env ~notification_capacity =
  let open Result.Let_syntax in
  let%bind bearer_token =
    match t.daemon_credential_file with
    | None -> Ok None
    | Some path -> Endpoint.load_bearer_token ~env ~path |> Result.map ~f:Option.some
  in
  let%bind endpoint = Endpoint.create ~home:t.home ~bearer_token t.endpoint in
  let%bind connection = Endpoint.connect endpoint ~sw ~env ~notification_capacity in
  let keep = ref false in
  Exn.protect
    ~finally:(fun () -> if not !keep then Agent_client.Connection.close connection)
    ~f:(fun () ->
      let%bind initialized =
        Agent_client.Session_handle.initialize
          connection
          ~implementation_name:"ochat-selected-host"
          ~implementation_version:"dev"
      in
      let%bind () =
        match t.expected_server with
        | None -> Ok ()
        | Some expected when Agent_protocol.Id.Server.equal expected initialized.server_id
          -> Ok ()
        | Some _ ->
          Error
            (Agent_protocol.Error.create
               Permission_denied
               ~message:"selected connection profile resolved to a different host"
               ~retryable:false
               ())
      in
      keep := true;
      Ok connection)
;;

let document_error error =
  Agent_protocol.Error.invalid_request
    (Sexp.to_string_hum ([%sexp_of: Document_schema.Error.t] error))
;;

let document_kind = "ochat.client.connection_profile"

let to_document t =
  match t.document with
  | Some document -> Ok document
  | None ->
    Document_schema.Document.create
      ~limits:Document_schema.Limits.default
      ~kind:document_kind
      ~version:1
      ~payload:
        (`Object
            [ "name", `String t.name
            ; "endpoint", `String t.endpoint
            ; ( "expected_server"
              , Option.value_map
                  t.expected_server
                  ~default:`Null
                  ~f:Agent_protocol.Id.Server.to_json )
            ; ( "daemon_credential_file"
              , Option.value_map t.daemon_credential_file ~default:`Null ~f:(fun value ->
                  `String value) )
            ])
    |> Result.map_error ~f:document_error
;;

let optional_nullable fields name decode =
  match Agent_protocol.Json_codec.optional fields name with
  | None | Some `Null -> Ok None
  | Some json -> Result.map (decode json) ~f:Option.some
;;

let of_document ~home document =
  let open Result.Let_syntax in
  let%bind () =
    Document_schema.Document.validate document ~limits:Document_schema.Limits.default
    |> Result.map_error ~f:document_error
  in
  let%bind () =
    if
      String.equal (Document_schema.Document.kind document) document_kind
      && Int.equal (Document_schema.Document.version document) 1
      && List.is_empty (Document_schema.Document.required_semantics document)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request "unsupported connection profile document")
  in
  let%bind fields =
    Agent_protocol.Json_codec.fields (Document_schema.Document.payload document)
  in
  let%bind name =
    Agent_protocol.Json_codec.required_as fields "name" Agent_protocol.Json_codec.string
  in
  let%bind endpoint =
    Agent_protocol.Json_codec.required_as
      fields
      "endpoint"
      Agent_protocol.Json_codec.string
  in
  let%bind expected_server =
    optional_nullable fields "expected_server" Agent_protocol.Id.Server.of_json
  in
  let%bind daemon_credential_file =
    optional_nullable fields "daemon_credential_file" Agent_protocol.Json_codec.string
  in
  let%map profile =
    create ~home ~name ~endpoint ~expected_server ~daemon_credential_file
  in
  { profile with document = Some document }
;;

let store_error error =
  Agent_protocol.Error.create
    Persistence_error
    ~message:(Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
    ~retryable:false
    ()
;;

let load ~home ~env ~path =
  let open Result.Let_syntax in
  let%bind () =
    if Filename.is_absolute path
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request "connection profile path must be absolute")
  in
  let%bind contents =
    Agent_store.Durable_file.load_bounded ~env ~path ~max_bytes:65536
    |> Result.map_error ~f:store_error
  in
  let%bind document =
    Document_schema.Document.decode ~limits:Document_schema.Limits.default contents
    |> Result.map_error ~f:document_error
  in
  of_document ~home document
;;

let save t ~env ~path =
  let open Result.Let_syntax in
  let%bind () =
    if Filename.is_absolute path
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request "connection profile path must be absolute")
  in
  let%bind document = to_document t in
  let contents = Document_schema.Document.to_string document in
  if String.length contents > 65536
  then
    Error
      (Agent_protocol.Error.invalid_request "connection profile exceeds its byte bound")
  else
    Agent_store.Durable_file.replace
      ~env
      ~durability:Flush_file_and_directory
      ~path
      contents
    |> Result.map_error ~f:store_error
;;
