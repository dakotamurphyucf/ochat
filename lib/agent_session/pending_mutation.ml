open! Core
module P = Agent_protocol
module X = Persistence_codec
module D = Document_schema
module Change = Pending_plan.Change

type t =
  { generation : int
  ; expected_revision : P.Pending_input.Revision.t
  ; change : Change.t
  ; max_records : int
  }
[@@deriving sexp]

let create (state : Session_state.t) ~change ~retention =
  { generation = state.identity.generation
  ; expected_revision = state.conversation.pending_revision
  ; change
  ; max_records = Pending_disposition.Retention.max_records retention
  }
;;

let prepare t state ~limits =
  let open Result.Let_syntax in
  let%bind retention = Pending_disposition.Retention.create ~max_records:t.max_records in
  if not (Int.equal t.generation state.Session_state.identity.generation)
  then
    Error
      (P.Error.create
         Conflict
         ~message:"pending mutation generation differs"
         ~retryable:false
         ())
  else
    Pending_plan.prepare
      state
      ~expected_pending_revision:t.expected_revision
      ~change:t.change
      ~limits
      ~retention
;;

let document_result result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let boundary_to_json = function
  | Pending_eligibility.Boundary.Idle_start -> `Object [ "kind", `String "idle" ]
  | Worker id ->
    `Object [ "kind", `String "worker"; "operation_id", P.Id.Operation.to_json id ]
;;

let boundary_of_json json =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json in
  let%bind kind = X.required fields "kind" P.Json_codec.string in
  match kind with
  | "idle" -> Ok Pending_eligibility.Boundary.Idle_start
  | "worker" ->
    X.required fields "operation_id" P.Id.Operation.of_json
    |> Result.map ~f:(fun id -> Pending_eligibility.Boundary.Worker id)
  | _ -> Error (P.Error.invalid_request "unknown pending adoption boundary")
;;

let reason_to_json = function
  | Pending_disposition.Retirement_reason.Source_reset -> `String "source_reset"
  | Source_replaced -> `String "source_replaced"
  | Canonical_history_retired -> `String "canonical_history_retired"
;;

let reason_of_json =
  P.Json_codec.enum
    ~name:"pending retirement reason"
    [ "source_reset", Pending_disposition.Retirement_reason.Source_reset
    ; "source_replaced", Source_replaced
    ; "canonical_history_retired", Canonical_history_retired
    ]
;;

let change_to_json change ~limits =
  let open Result.Let_syntax in
  match change with
  | Change.Enqueue documents ->
    let%map entries =
      List.map documents ~f:(fun document ->
        Pending_input_document.to_jsonaf document ~limits |> document_result)
      |> Result.all
    in
    `Object [ "kind", `String "enqueue"; "entries", `Array entries ]
  | Adopt { boundary; runtime_admission_open } ->
    Ok
      (`Object
          [ "kind", `String "adopt"
          ; "boundary", boundary_to_json boundary
          ; "runtime_admission_open", X.bool_json runtime_admission_open
          ])
  | Cancel { history_id; expected_content_revision } ->
    Ok
      (`Object
          [ "kind", `String "cancel"
          ; "history_id", P.History.Id.to_json history_id
          ; ( "expected_content_revision"
            , P.History.Content_revision.to_json expected_content_revision )
          ])
  | Replace_text { history_id; expected_content_revision; text } ->
    Ok
      (`Object
          [ "kind", `String "replace_text"
          ; "history_id", P.History.Id.to_json history_id
          ; ( "expected_content_revision"
            , P.History.Content_revision.to_json expected_content_revision )
          ; "text", `String text
          ])
  | Release proof ->
    Ok
      (`Object
          [ "kind", `String "release"
          ; "proof", P.Pending_input.Terminal_proof.to_json proof
          ])
  | Retire reason ->
    Ok (`Object [ "kind", `String "retire"; "reason", reason_to_json reason ])
;;

let change_of_json json ~limits =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json in
  let%bind kind = X.required fields "kind" P.Json_codec.string in
  match kind with
  | "enqueue" ->
    X.required
      fields
      "entries"
      (X.list (fun json ->
         Pending_input_document.of_jsonaf json ~limits |> document_result))
    |> Result.map ~f:(fun entries -> Change.Enqueue entries)
  | "adopt" ->
    let%bind boundary = X.required fields "boundary" boundary_of_json in
    let%map runtime_admission_open =
      X.required fields "runtime_admission_open" P.Json_codec.bool
    in
    Change.Adopt { boundary; runtime_admission_open }
  | "cancel" ->
    let%bind history_id = X.required fields "history_id" P.History.Id.of_json in
    let%map expected_content_revision =
      X.required fields "expected_content_revision" P.History.Content_revision.of_json
    in
    Change.Cancel { history_id; expected_content_revision }
  | "replace_text" ->
    let%bind history_id = X.required fields "history_id" P.History.Id.of_json in
    let%bind expected_content_revision =
      X.required fields "expected_content_revision" P.History.Content_revision.of_json
    in
    let%bind text = X.required fields "text" P.Json_codec.string in
    let%map _ =
      P.History_edit.create ~history_id ~expected_content_revision ~text ~mode:Save_only
    in
    Change.Replace_text { history_id; expected_content_revision; text }
  | "release" ->
    X.required fields "proof" P.Pending_input.Terminal_proof.of_json
    |> Result.map ~f:(fun proof -> Change.Release proof)
  | "retire" ->
    X.required fields "reason" reason_of_json
    |> Result.map ~f:(fun reason -> Change.Retire reason)
  | _ -> Error (P.Error.invalid_request "unknown pending mutation")
;;

let to_jsonaf t ~limits =
  let%map.Result change = change_to_json t.change ~limits in
  `Object
    [ "generation", X.integer_json t.generation
    ; "expected_revision", P.Pending_input.Revision.to_json t.expected_revision
    ; "change", change
    ; "max_records", X.integer_json t.max_records
    ]
;;

let of_jsonaf json ~limits =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json in
  let%bind generation =
    X.required fields "generation" (P.Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind expected_revision =
    X.required fields "expected_revision" P.Pending_input.Revision.of_json
  in
  let%bind max_records =
    X.required fields "max_records" (P.Json_codec.bounded_int ~min:1 ~max:Int.max_value)
  in
  let%map change = X.required fields "change" (fun json -> change_of_json json ~limits) in
  { generation; expected_revision; change; max_records }
;;

let unchecked_of_sexp = t_of_sexp

let t_of_sexp sexp =
  let value = unchecked_of_sexp sexp in
  if value.generation < 0 || value.max_records <= 0
  then Sexplib.Conv.of_sexp_error "invalid pending mutation generation/retention" sexp
  else value
;;

let shape =
  X.shape_exn
    [ "generation", D.Shape.value
    ; "expected_revision", D.Shape.value
    ; "max_records", D.Shape.value
    ; ( "change"
      , X.shape_exn
          [ "kind", D.Shape.value
          ; "entries", X.array_shape_exn ~identity_field:"id" Pending_input_document.shape
          ; "boundary", X.fields_shape [ "kind"; "operation_id" ]
          ; "runtime_admission_open", D.Shape.value
          ; "history_id", D.Shape.value
          ; "expected_content_revision", D.Shape.value
          ; "text", D.Shape.value
          ; "proof", X.fields_shape [ "operation_id"; "generation"; "outcome" ]
          ; "reason", D.Shape.value
          ] )
    ]
;;
