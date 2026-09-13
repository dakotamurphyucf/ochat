open Core
module P = Agent_protocol

type operation =
  | Create
  | Send
  | Read
  | Status
  | Wait
  | Stop
  | Reference
  | Validate
[@@deriving equal, sexp]

let operation_to_string = function
  | Create -> "create"
  | Send -> "send"
  | Read -> "read"
  | Status -> "status"
  | Wait -> "wait"
  | Stop -> "stop"
  | Reference -> "reference"
  | Validate -> "validate"
;;

let operation_of_json =
  P.Json_codec.enum
    ~name:"session management operation"
    [ "create", Create
    ; "send", Send
    ; "read", Read
    ; "status", Status
    ; "wait", Wait
    ; "stop", Stop
    ; "reference", Reference
    ; "validate", Validate
    ]
;;

type t =
  { borrowed : Native_tool_invocation.borrowed
  ; allowed : operation list
  ; creation : Generated_session_request.service option
  ; sessions : Managed_session_service.t option
  ; authoring : Authoring_services.t option
  }

let create ~borrowed ~allowed ~creation ~sessions ~authoring =
  { borrowed; allowed; creation; sessions; authoring }
;;

let failure code message =
  P.Invocation.{ code; message; retryable = false; details = `Null }
;;

let fields ~names json =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json in
  match
    List.for_all (P.Json_codec.to_alist fields) ~f:(fun (name, _) ->
      List.mem names name ~equal:String.equal)
  with
  | true -> Ok fields
  | false -> Error (P.Error.invalid_request "Unexpected request field.")
;;

let run t operation json =
  let open Result.Let_syntax in
  let%bind () =
    match List.mem t.allowed operation ~equal:equal_operation with
    | true -> Ok ()
    | false ->
      Error
        (failure
           "agent.management.denied"
           "This session operation is not delegated to the caller.")
  in
  let%bind _ =
    Native_tool_invocation.borrowed_capabilities t.borrowed
    |> Result.map_error ~f:(fun _ ->
      failure "agent.management.denied" "The session-management invocation has expired.")
  in
  match operation with
  | Reference | Validate ->
    let%bind service =
      Result.of_option
        t.authoring
        ~error:
          (failure
             "authoring.unavailable"
             "Authoring services are unavailable for this session.")
    in
    (match operation with
     | Reference -> Authoring_services.reference service t.borrowed json
     | Validate -> Authoring_services.validate service t.borrowed json
     | Create | Send | Read | Status | Wait | Stop -> assert false)
  | Create ->
    let%bind service =
      t.creation
      |> Result.of_option
           ~error:
             (failure
                "capability_unavailable"
                "Persisted child creation requires a durable Ochat host.")
    in
    let%bind request = Generated_session_request.decode ~limits:service.limits json in
    Result.map (service.create t.borrowed request) ~f:Generated_session_request.to_json
  | (Send | Read | Status | Wait | Stop) as operation ->
    let invalid error =
      let code =
        match operation with
        | Status -> "agent.management.denied"
        | _ -> "agent." ^ operation_to_string operation ^ ".invalid_request"
      in
      failure code error.P.Error.message
    in
    let decode result = Result.map_error result ~f:invalid in
    let names =
      match operation with
      | Send -> [ "session_id"; "message"; "idempotency_key" ]
      | Read -> [ "session_id"; "receipt_id"; "cursor"; "limit" ]
      | Status -> [ "session_id" ]
      | Wait -> [ "session_id"; "receipt_id"; "cursor"; "timeout_ms" ]
      | Stop -> [ "session_id"; "idempotency_key"; "mode" ]
      | Create | Reference | Validate -> assert false
    in
    let%bind fields = fields ~names json |> decode in
    let%bind id =
      P.Json_codec.required_as fields "session_id" P.Id.Session.of_json |> decode
    in
    let%bind service =
      t.sessions
      |> Result.of_option
           ~error:
             (failure
                "capability_unavailable"
                "Session management requires a durable Ochat host.")
    in
    (match operation with
     | Status -> service.status t.borrowed id
     | Send ->
       let%bind key =
         P.Json_codec.required_as fields "idempotency_key" P.Idempotency_key.of_json
         |> decode
       in
       let%bind message =
         P.Json_codec.required_as fields "message" P.Json_codec.string |> decode
       in
       service.send t.borrowed id ~key ~message
     | Read ->
       let%bind receipt_id =
         P.Json_codec.optional_as fields "receipt_id" P.History.Id.of_json |> decode
       in
       let%bind cursor =
         P.Json_codec.optional_as fields "cursor" P.Page.Cursor.of_json |> decode
       in
       let%bind limit =
         P.Json_codec.optional_as
           fields
           "limit"
           (P.Json_codec.bounded_int ~min:1 ~max:128)
         |> decode
       in
       service.read
         t.borrowed
         id
         ~receipt_id
         ~cursor
         ~limit:(Option.value limit ~default:16)
     | Wait ->
       let%bind receipt_id =
         P.Json_codec.optional_as fields "receipt_id" P.History.Id.of_json |> decode
       in
       let%bind cursor =
         P.Json_codec.optional_as fields "cursor" P.Page.Cursor.of_json |> decode
       in
       let%bind timeout =
         P.Json_codec.optional_as
           fields
           "timeout_ms"
           (P.Json_codec.bounded_int ~min:0 ~max:30000)
         |> decode
       in
       let%bind target =
         match cursor, receipt_id with
         | Some cursor, receipt_id ->
           Ok (Managed_session_service.Output { cursor; receipt_id })
         | None, Some id -> Ok (Managed_session_service.Receipt id)
         | None, None ->
           Error
             (invalid (P.Error.invalid_request "Supply receipt_id or an output cursor."))
       in
       service.wait
         t.borrowed
         id
         ~target
         ~timeout_ms:(Option.value timeout ~default:10000)
     | Stop ->
       let%bind key =
         P.Json_codec.required_as fields "idempotency_key" P.Idempotency_key.of_json
         |> decode
       in
       let%bind mode =
         P.Json_codec.required_as
           fields
           "mode"
           (P.Json_codec.enum
              ~name:"stop mode"
              [ "graceful", P.Session.Graceful; "cancel", Cancel ])
         |> decode
       in
       service.stop t.borrowed id ~key ~mode
     | Create | Reference | Validate -> assert false)
;;

let decode_request json =
  let decode result =
    Result.map_error result ~f:(fun error ->
      failure "agent.bridge.invalid_request" error.P.Error.message)
  in
  let open Result.Let_syntax in
  let%bind fields =
    fields ~names:[ "version"; "operation"; "arguments" ] json |> decode
  in
  let%bind _ =
    P.Json_codec.required_as fields "version" (P.Json_codec.bounded_int ~min:1 ~max:1)
    |> decode
  in
  let%bind operation =
    P.Json_codec.required_as fields "operation" operation_of_json |> decode
  in
  let%bind arguments =
    P.Json_codec.required_as fields "arguments" (fun json -> Ok json) |> decode
  in
  Ok (operation, arguments)
;;

let dispatch t json =
  let result =
    Result.bind (decode_request json) ~f:(fun (operation, arguments) ->
      run t operation arguments)
  in
  match result with
  | Ok value -> P.Invocation.Complete value
  | Error error -> Fail error
;;
