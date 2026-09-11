open Core
module P = Agent_protocol
module N = Native_tool_invocation
module Contract = Chat_response.Agent_tool_contract

type host =
  { create :
      N.borrowed
      -> key:P.Idempotency_key.t
      -> (P.Id.Session.t, P.Invocation.tool_error) result
  ; validate : N.borrowed -> P.Id.Session.t -> (unit, P.Invocation.tool_error) result
  ; one_off : N.borrowed -> input:string -> (Jsonaf.t, P.Invocation.tool_error) result
  }

let failure code message =
  P.Invocation.{ code; message; retryable = false; details = `Null }
;;

let invalid _ =
  failure "agent.authored.invalid_request" "Invalid authored agent tool arguments."
;;

let live borrowed =
  N.borrowed_capabilities borrowed
  |> Result.map ~f:ignore
  |> Result.map_error ~f:(fun _ ->
    failure "agent.authored.denied" "The authored agent invocation has expired.")
;;

let key borrowed operation =
  let context = (N.borrowed_invocation borrowed).context in
  let digest =
    [%sexp
      ("ochat.authored-agent-call.v1" : string)
    , (context.session_id : P.Id.Session.t)
    , (context.generation : int)
    , (context.id : P.Id.Invocation.t)
    , (operation : string)]
    |> Sexp.to_string_mach
    |> Chatmd_shell_spec.Source_ref.digest
  in
  P.Idempotency_key.of_string ("authored/" ^ operation ^ "/" ^ digest)
  |> Result.map_error ~f:invalid
;;

let contract result =
  Result.map_error result ~f:(fun _ ->
    failure "agent.authored.service_contract" "Invalid session service response.")
;;

let receipt_id ~session_id json =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json in
  let%bind actual = P.Json_codec.required_as fields "session_id" P.Id.Session.of_json in
  let%bind () =
    match P.Id.Session.equal actual session_id with
    | true -> Ok ()
    | false -> Error (P.Error.invalid_request "Mismatched session receipt.")
  in
  P.Json_codec.required_as fields "receipt_id" P.History.Id.of_json
;;

let response ~session_id ~expected_receipt page =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields page in
  let%bind actual = P.Json_codec.required_as fields "session_id" P.Id.Session.of_json in
  let%bind receipt = P.Json_codec.required fields "receipt" in
  let%bind actual_receipt = receipt_id ~session_id receipt in
  let%bind () =
    match
      P.Id.Session.equal actual session_id
      && P.History.Id.equal actual_receipt expected_receipt
    with
    | true -> Ok ()
    | false -> Error (P.Error.invalid_request "Mismatched output page.")
  in
  let%bind receipt_fields = P.Json_codec.fields receipt in
  let%bind status =
    P.Json_codec.required_as
      receipt_fields
      "status"
      (P.Json_codec.enum
         ~name:"submission status"
         [ "deferred", "pending"
         ; "ready", "pending"
         ; "assigned", "pending"
         ; "completed", "completed"
         ; "failed", "failed"
         ; "cancelled", "cancelled"
         ; "interrupted", "interrupted"
         ; "invalidated", "invalidated"
         ])
  in
  Ok
    (`Object
        [ "version", `Number "1"
        ; "session_id", P.Id.Session.to_json session_id
        ; "status", `String status
        ; "receipt", receipt
        ; "output", page
        ])
;;

let run
      ?(wait_timeout_ms = 10000)
      ~host
      ~(sessions : Managed_session_service.t)
      ~borrowed
      ~policy
      json
  =
  let open Result.Let_syntax in
  let%bind call = Contract.decode policy json |> Result.map_error ~f:invalid in
  let%bind () =
    match wait_timeout_ms >= 0 && wait_timeout_ms <= 30000 with
    | true -> Ok ()
    | false -> Error (invalid ())
  in
  let%bind () = live borrowed in
  match call.mode with
  | One_off ->
    let%bind value = host.one_off borrowed ~input:call.input in
    let%map () = live borrowed in
    value
  | Persistent ->
    let%bind session_id =
      match call.session_id with
      | Some session_id -> Ok session_id
      | None ->
        let%bind key = key borrowed "create" in
        host.create borrowed ~key
    in
    let validate () =
      let%bind () = live borrowed in
      host.validate borrowed session_id
    in
    let%bind () = validate () in
    let%bind key = key borrowed "send" in
    let%bind receipt = sessions.send borrowed session_id ~key ~message:call.input in
    let%bind receipt_id = receipt_id ~session_id receipt |> contract in
    let%bind () = validate () in
    let%bind _ =
      sessions.wait
        borrowed
        session_id
        ~target:(Receipt receipt_id)
        ~timeout_ms:wait_timeout_ms
    in
    let%bind () = validate () in
    let%bind page =
      sessions.read
        borrowed
        session_id
        ~receipt_id:(Some receipt_id)
        ~cursor:None
        ~limit:16
    in
    (* The read is the freshest snapshot: a timeout may race with completion or
       a compaction may invalidate the receipt after the wait. Preserve its status
       and cursor rather than guessing completion from output presence. *)
    let%bind () = validate () in
    response ~session_id ~expected_receipt:receipt_id page |> contract
;;
