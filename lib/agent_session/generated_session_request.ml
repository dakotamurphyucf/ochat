open Core
module P = Agent_protocol

type lifetime =
  | Owned
  | Independent
[@@deriving equal, sexp_of]

type t =
  { bundle : Chatmd_source_bundle.t
  ; tools : string list
  ; start_immediately : bool
  ; lifetime : lifetime
  ; display_name : string option
  ; idempotency_key : P.Idempotency_key.t
  }

type created =
  { session : P.Session.t
  ; parent_session_id : P.Id.Session.t
  ; tools : string list
  }

type service =
  { limits : Chatmd_source_bundle.limits
  ; create :
      Native_tool_invocation.borrowed -> t -> (created, P.Invocation.tool_error) result
  }

let string = `Object [ "type", `String "string" ]

let object_schema properties required =
  `Object
    [ "type", `String "object"
    ; "properties", `Object properties
    ; "required", `Array (List.map required ~f:(fun field -> `String field))
    ; "additionalProperties", `False
    ]
;;

let parameters =
  object_schema
    [ "version", `Object [ "type", `String "integer"; "enum", `Array [ `Number "1" ] ]
    ; "root_file", string
    ; ( "sources"
      , `Object
          [ "type", `String "array"
          ; "items", object_schema [ "path", string; "text", string ] [ "path"; "text" ]
          ] )
    ; ( "tools"
      , `Object [ "type", `String "array"; "items", string; "maxItems", `Number "4096" ] )
    ; "start_immediately", `Object [ "type", `String "boolean" ]
    ; ( "lifetime"
      , `Object
          [ "type", `String "string"
          ; "enum", `Array [ `String "owned"; `String "independent" ]
          ] )
    ; "display_name", string
    ; "idempotency_key", string
    ]
    [ "version"; "root_file"; "sources"; "tools"; "idempotency_key" ]
;;

let schema =
  match Chatmd_shell_spec.Tool_schema.compile parameters with
  | Ok schema -> schema
  | Error _ -> assert false
;;

let invalid message =
  Error
    P.Invocation.
      { code = "agent.create.invalid_request"
      ; message
      ; retryable = false
      ; details = `Null
      }
;;

let object_fields = function
  | `Object fields
    when not (List.contains_dup (List.map fields ~f:fst) ~compare:String.compare) ->
    Ok fields
  | _ -> invalid "Expected an object with unique field names."
;;

let get fields name = List.Assoc.find fields name ~equal:String.equal

let text fields name =
  match get fields name with
  | Some (`String value) -> Ok value
  | _ -> invalid ("Expected string field: " ^ name)
;;

let boolean fields name default =
  match get fields name with
  | None -> Ok default
  | Some `True -> Ok true
  | Some `False -> Ok false
  | _ -> invalid ("Expected boolean field: " ^ name)
;;

let decode ~limits json =
  let open Result.Let_syntax in
  let%bind fields = object_fields json in
  let%bind () =
    match Chatmd_shell_spec.Tool_schema.validate schema json with
    | Ok () -> Ok ()
    | Error _ -> invalid "Request does not match the version 1 agent_create schema."
  in
  let%bind root_file = text fields "root_file" in
  let%bind sources =
    match get fields "sources" with
    | Some (`Array sources)
      when List.length sources <= limits.Chatmd_source_bundle.max_files ->
      List.map sources ~f:(fun source ->
        let%bind source = object_fields source in
        let%bind path = text source "path" in
        let%map contents = text source "text" in
        path, contents)
      |> Result.all
    | _ -> invalid "Source file count exceeds the host limit."
  in
  let%bind bundle =
    Chatmd_source_bundle.create ~limits ~root_file ~sources ()
    |> Result.map_error ~f:(fun message ->
      P.Invocation.
        { code = "agent.create.invalid_source"
        ; message
        ; retryable = false
        ; details = `Null
        })
  in
  let%bind tools =
    match get fields "tools" with
    | Some (`Array values) ->
      List.map values ~f:(function
        | `String name -> Ok name
        | _ -> invalid "Tool names must be strings.")
      |> Result.all
    | _ -> invalid "An explicit tool selection is required."
  in
  let%bind () =
    match List.contains_dup tools ~compare:String.compare with
    | true -> invalid "Tool selection contains duplicate names."
    | false -> Ok ()
  in
  let%bind start_immediately = boolean fields "start_immediately" false in
  let%bind lifetime =
    match get fields "lifetime" with
    | None | Some (`String "owned") -> Ok Owned
    | Some (`String "independent") -> Ok Independent
    | _ -> invalid "Unsupported child lifetime."
  in
  let display_name =
    match get fields "display_name" with
    | Some (`String value) -> Some value
    | _ -> None
  in
  let%bind encoded_key = text fields "idempotency_key" in
  let%map idempotency_key =
    P.Idempotency_key.of_string encoded_key
    |> Result.map_error ~f:(fun _ ->
      P.Invocation.
        { code = "agent.create.invalid_request"
        ; message = "Invalid idempotency key."
        ; retryable = false
        ; details = `Null
        })
  in
  { bundle; tools; start_immediately; lifetime; display_name; idempotency_key }
;;

let to_json created =
  `Object
    [ "version", `Number "1"
    ; "session_id", `String (P.Id.Session.to_string created.session.id)
    ; ( "definition_revision"
      , match created.session.prompt_revision with
        | None -> `Null
        | Some id -> `String (P.Id.Prompt_revision.to_string id) )
    ; "session", P.Session.to_json created.session
    ; "tools", `Array (List.map created.tools ~f:(fun name -> `String name))
    ; ( "management"
      , `Object
          [ ( "parent_session_id"
            , `String (P.Id.Session.to_string created.parent_session_id) )
          ; "child_session_id", `String (P.Id.Session.to_string created.session.id)
          ] )
    ]
;;
