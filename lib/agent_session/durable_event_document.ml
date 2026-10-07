open! Core
module P = Agent_protocol
module E = P.Event.Durable
module X = Persistence_codec
module J = P.Json_codec
module D = Document_schema

type t =
  { value : E.t
  ; document : D.Document.t
  }

let value t = t.value
let document t = t.document

let replace json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> assert false
;;

let visibility = function
  | E.Full -> `String "full"
  | Redacted -> `String "redacted"
  | Hidden -> `String "hidden"
;;

let to_jsonaf (event : E.t) =
  E.to_json event
  |> fun json ->
  replace json "sequence" (X.int64_json event.sequence)
  |> fun json ->
  replace json "revision" (X.int64_json event.revision)
  |> fun json -> replace json "visibility" (visibility event.visibility)
;;

let validate_known event =
  let open Result.Let_syntax in
  let validate_history entries =
    let%bind entries = History_codec.all_of_protocol entries in
    History_entry.validate_relations entries
    |> Result.map_error ~f:P.Error.invalid_request
  in
  let%bind payload = E.Payload.of_json ~kind:event.E.kind event.payload in
  let%bind () =
    match payload with
    | E.Payload.History_message_deferred entry ->
      History_codec.of_canonical entry |> Result.map ~f:ignore
    | History_appended entries -> validate_history entries
    | History_replaced window -> validate_history window.P.History.Window.entries
    | _ -> Ok ()
  in
  let%bind _ = E.extension_status event in
  let%bind snapshot = E.replacement_snapshot event in
  match snapshot with
  | None -> Ok ()
  | Some snapshot ->
    let%bind () = validate_history snapshot.canonical_history.entries in
    let%bind () = validate_history snapshot.deferred_entries in
    (match snapshot.effective_history with
     | None -> Ok ()
     | Some history -> validate_history history.entries)
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind sequence = X.required fields "sequence" X.nonnegative_int64 in
  let%bind revision = X.required fields "revision" X.nonnegative_int64 in
  let%bind _ =
    X.required
      fields
      "visibility"
      (J.enum
         ~name:"event visibility"
         [ "full", E.Full; "redacted", Redacted; "hidden", Hidden ])
  in
  let%bind event =
    E.of_json
      (replace
         (replace json "sequence" (`Number (Int64.to_string sequence)))
         "revision"
         (`Number (Int64.to_string revision)))
  in
  let%map () = validate_known event in
  event
;;

let shape =
  let module S = Session_record_shapes in
  let cases =
    [ "session.created", S.event_session
    ; "session.updated", S.event_session
    ; ( "session.state_changed"
      , X.shape_exn [ "desired_state", D.Shape.value; "observed_state", S.observed ] )
    ; "attachment.owner_changed", D.Shape.nullable S.attachment
    ; "history.message_deferred", S.history_entry
    ; "history.appended", X.array_shape_exn ~identity_field:"id" S.history_entry
    ; "history.replaced", S.history_window
    ; "moderator.overlay_changed", D.Shape.value
    ; "moderator.notification", D.Shape.value
    ; "permission.requested", S.permission
    ; "permission.resolved", S.permission
    ; "grant.created", S.grant
    ; "grant.revoked", S.grant
    ; "operation.started", S.operation
    ; "operation.completed", S.operation
    ; "operation.failed", S.operation
    ; "operation.cancelled", S.operation
    ; "operation.interrupted", S.operation
    ; "job.state_changed", S.job
    ; "schedule.created", S.public_schedule
    ; "schedule.state_changed", S.public_schedule
    ; "schedule.cancelled", S.public_schedule
    ; ( "prompt.upgraded"
      , X.fields_shape [ "prompt_id"; "previous_revision"; "current_revision" ] )
    ; "workspace.state_changed", S.workspace
    ; "session.error", S.error
    ]
  in
  X.tagged_shape_exn
    ~discriminator:"kind"
    (List.map cases ~f:(fun (kind, payload) ->
       ( kind
       , X.shape_exn
           [ "session_id", D.Shape.value
           ; "sequence", D.Shape.value
           ; "revision", D.Shape.value
           ; "timestamp", D.Shape.value
           ; "kind", D.Shape.value
           ; "visibility", D.Shape.value
           ; "payload", payload
           ] )))
;;

let codec ~limits =
  X.codec_exn ~limits ~kind:"session.event" ~shape ~decode:of_jsonaf ~encode:(fun _ ->
    Error (P.Error.invalid_request "immutable event document is captured at construction"))
;;

let decode ~limits document =
  let open Result.Let_syntax in
  let%bind document = X.upgrade document ~limits ~kind:"session.event" in
  let%bind carrier = D.Domain_codec.decode (codec ~limits) document in
  let%bind fields = X.object_ (D.Document.payload document) |> X.document_result in
  let%map payload = X.required fields "payload" X.raw |> X.document_result in
  let value = D.Extension_carrier.value carrier in
  { value = { value with payload }; document }
;;

let validate ?(limits = D.Limits.default) event =
  let protocol_error error =
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error))
  in
  let open Result.Let_syntax in
  let%bind document =
    D.Document.create ~limits ~kind:"session.event" ~version:1 ~payload:(to_jsonaf event)
    |> Result.map_error ~f:protocol_error
  in
  decode ~limits document |> Result.map ~f:ignore |> Result.map_error ~f:protocol_error
;;

let create value ~limits =
  let open Result.Let_syntax in
  let%bind document =
    D.Document.create ~limits ~kind:"session.event" ~version:1 ~payload:(to_jsonaf value)
  in
  decode ~limits document
;;
