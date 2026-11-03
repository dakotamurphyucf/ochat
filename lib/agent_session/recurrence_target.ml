open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module Parent = Recurrence_parent
module Artifact = Agent_store.Captured_prompt_artifact

type view =
  | Existing_session of
      { session : P.Session_ref.t
      ; source : P.Run_source.t
      ; configuration_revision : int64
      ; input : P.Session.Message_content.t
      }
  | Fresh_child of
      { artifact : Artifact.t
      ; definition : Prompt_definition.t
      ; parent : Parent.t
      }

type t =
  { projection : view
  ; document : D.Document.t
  ; sha256 : string
  }

let view t = t.projection
let invalid message = Error (P.Error.invalid_request message)
let document_result = X.document_result

let protocol_result result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create
    ~max_bytes:(24 * 1024 * 1024)
    ~max_depth:128
    ~max_fields:8192
    ~max_nodes:16384
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let input_limits =
  D.Limits.create ~max_bytes:262144 ~max_depth:32 ~max_fields:1024 ~max_nodes:2048
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let bounded_count values =
  List.fold_result values ~init:0 ~f:(fun count _ ->
    if count >= 256 then invalid "too many recurring input attachments" else Ok (count + 1))
  |> Result.map ~f:(fun _ -> ())
;;

let input_of_json json =
  let open Result.Let_syntax in
  let%bind () = D.Json.validate ~limits:input_limits json |> protocol_result in
  let%bind fields = X.object_ json in
  let%bind attachments =
    X.required fields "attachments" (function
      | `Array values -> Ok values
      | _ -> invalid "expected input attachment array")
  in
  let%bind () = bounded_count attachments in
  P.Session.Message_content.of_json json
;;

let validate_input (input : P.Session.Message_content.t) =
  let open Result.Let_syntax in
  let%bind () = bounded_count input.attachments in
  if String.length input.text > D.Limits.max_bytes input_limits
  then invalid "recurring input text exceeds byte ceiling"
  else (
    let%map _ = input_of_json (P.Session.Message_content.to_json input) in
    ())
;;

let validate_view = function
  | Existing_session { session; source; configuration_revision; input } ->
    let open Result.Let_syntax in
    let%bind _ = P.Session_ref.of_json (P.Session_ref.to_json session) in
    let%bind () = P.Run_source.validate source in
    let%bind () = validate_input input in
    if Int64.(configuration_revision < 0L)
    then invalid "negative target configuration revision"
    else Ok ()
  | Fresh_child { artifact; definition; parent } ->
    let open Result.Let_syntax in
    let%bind _ = Prompt_definition.to_jsonaf definition in
    let%bind _ = Parent.to_jsonaf parent in
    let%bind _ = Artifact.to_document artifact |> protocol_result in
    let captured = Artifact.value artifact in
    let definition_matches =
      Option.for_all
        captured.prompt_definition_id
        ~f:(P.Id.Prompt_definition.equal definition.id)
      && Option.for_all captured.source_receipt ~f:(fun receipt ->
        P.Id.Prompt_definition.equal
          (Agent_store.Prompt_source_receipt.definition_id receipt)
          definition.id
        && String.equal
             (Agent_store.Prompt_source_receipt.root_file receipt)
             definition.root_file)
    in
    let workspace_matches =
      Option.for_all
        (Parent.workspace parent).definition_id
        ~f:(Prompt_definition.allows_workspace definition)
    in
    if
      definition_matches
      && workspace_matches
      && String.equal definition.permission_profile (Parent.permission_profile parent)
      && Option.equal
           String.equal
           definition.runtime_policy
           (Parent.runtime_policy parent)
    then Ok ()
    else invalid "captured prompt artifact, definition and parent selectors differ"
;;

let encode = function
  | Existing_session { session; source; configuration_revision; input } ->
    Ok
      (`Object
          [ "kind", `String "existing_session"
          ; "session", P.Session_ref.to_json session
          ; "source", P.Run_source.to_json source
          ; "configuration_revision", X.int64_json configuration_revision
          ; "input", P.Session.Message_content.to_json input
          ])
  | Fresh_child { artifact; definition; parent } ->
    let open Result.Let_syntax in
    let%bind artifact = Artifact.to_document artifact |> protocol_result in
    let%bind definition = Prompt_definition.to_jsonaf definition in
    let%map parent = Parent.to_jsonaf parent in
    `Object
      [ "kind", `String "fresh_child"
      ; "artifact", D.Document.json artifact
      ; "definition", definition
      ; "parent", parent
      ]
;;

let decode json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind kind = X.required fields "kind" P.Json_codec.string in
  let%bind target =
    match kind with
    | "existing_session" ->
      let%bind session = X.required fields "session" P.Session_ref.of_json in
      let%bind source = X.required fields "source" P.Run_source.of_json in
      let%bind configuration_revision =
        X.required fields "configuration_revision" (function
          | `String text when String.length text <= 19 ->
            X.nonnegative_int64 (`String text)
          | _ -> invalid "invalid target configuration revision")
      in
      let%map input = X.required fields "input" input_of_json in
      Existing_session { session; source; configuration_revision; input }
    | "fresh_child" ->
      let%bind artifact =
        X.required fields "artifact" (fun json ->
          let%bind document =
            D.Document.inspect ~limits:Artifact.limits json |> protocol_result
          in
          Artifact.of_document document |> protocol_result)
      in
      let%bind definition = X.required fields "definition" Prompt_definition.of_jsonaf in
      let%map parent = X.required fields "parent" Parent.of_jsonaf in
      Fresh_child { artifact; definition; parent }
    | _ -> invalid "unsupported recurrence target kind"
  in
  let%map () = validate_view target in
  target
;;

let input_shape =
  let v = D.Shape.value in
  let common =
    [ "kind", v
    ; "media_type", v
    ; "byte_length", v
    ; "digest", v
    ; "display_name", v
    ; "source", v
    ]
  in
  let attachment =
    X.tagged_shape_exn
      ~discriminator:"source"
      [ "stored", X.shape_exn (common @ [ "blob_id", v ])
      ; "inline_base64", X.shape_exn (common @ [ "inline_base64", v ])
      ]
  in
  X.shape_exn [ "kind", v; "text", v; "attachments", X.array_shape_exn attachment ]
;;

let shape =
  let v = D.Shape.value in
  X.tagged_shape_exn
    ~discriminator:"kind"
    [ ( "existing_session"
      , X.shape_exn
          [ "kind", v
          ; "session", X.fields_shape [ "server_id"; "session_id" ]
          ; "source", Run_record_shapes.source
          ; "configuration_revision", v
          ; "input", input_shape
          ] )
    ; ( "fresh_child"
      , X.shape_exn
          [ "kind", v
            (* The artifact owns its entire nested carrier, including unknowns. *)
          ; "artifact", v
          ; "definition", Prompt_definition.shape
          ; "parent", Parent.shape
          ] )
    ]
;;

let codec =
  D.Domain_codec.create_validated
    ~limits
    ~kind:"ochat.recurrence_target"
    ~version:1
    ~shape
    ~supported_semantics:[]
    ~validate:(fun target -> document_result (validate_view target))
    ~decode:(fun json -> document_result (decode json))
    ~encode:(fun target -> document_result (encode target))
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

(* Validate complete preserved metadata slices before outer shape splitting can
   move their unknowns into its template. Child-owned carriers remain opaque. *)
let validate_slices document =
  let open Result.Let_syntax in
  let%bind () = D.Document.validate document ~limits in
  let json = D.Document.payload document in
  let%bind kind =
    Agent_store.Document_fields.required json "kind" Agent_store.Document_fields.string
  in
  let metadata_limits =
    D.Limits.create ~max_bytes:262144 ~max_depth:64 ~max_fields:4096 ~max_nodes:8192
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let slice name limits =
    Agent_store.Document_fields.required json name (D.Json.validate ~limits)
  in
  match kind with
  | "existing_session" -> slice "input" input_limits
  | "fresh_child" ->
    let%bind () = slice "definition" metadata_limits in
    slice "parent" metadata_limits
  | _ ->
    Error
      (D.Error.Invalid_field
         { path = [ "kind" ]; reason = "unsupported recurrence target kind" })
;;

let admitted projection document =
  let%map.Result canonical =
    P.Json_codec.canonical_string (D.Document.json document) |> document_result
  in
  { projection; document; sha256 = Digestif.SHA256.(digest_string canonical |> to_hex) }
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind () = validate_slices document in
  let%bind carrier = D.Domain_codec.decode codec document in
  admitted (C.value carrier) document
;;

(* The private immutable value and exact document were jointly admitted once.
   Returning their retained custody neither normalizes unknowns nor rehashes an
   unchanged multi-megabyte source on every later control transition. *)
let to_document t = Ok t.document

let create target =
  let open Result.Let_syntax in
  let%bind document =
    D.Domain_codec.encode codec (C.of_authored_value target) |> protocol_result
  in
  let%bind () = validate_slices document |> protocol_result in
  admitted target document |> protocol_result
;;

let create_existing ~session ~source ~configuration_revision ~input =
  create (Existing_session { session; source; configuration_revision; input })
;;

let create_fresh ~artifact ~definition ~parent =
  create (Fresh_child { artifact; definition; parent })
;;

let digest t = Ok t.sha256
