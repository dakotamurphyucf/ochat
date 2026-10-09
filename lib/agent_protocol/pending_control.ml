open! Core
module Error = Protocol_error
module J = Json_codec

module Cancel_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_pending_revision : Pending_input.Revision.t
    ; history_id : History.Id.t
    ; expected_content_revision : History.Content_revision.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let create
        ~session_id
        ~attachment_id
        ~expected_generation
        ~expected_pending_revision
        ~history_id
        ~expected_content_revision
        ~idempotency_key
    =
    if expected_generation < 0
    then Error (Error.invalid_request "pending expected generation must be nonnegative")
    else
      Ok
        { session_id
        ; attachment_id
        ; expected_generation
        ; expected_pending_revision
        ; history_id
        ; expected_content_revision
        ; idempotency_key
        }
  ;;

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "expected_generation", `Number (Int.to_string t.expected_generation)
      ; ( "expected_pending_revision"
        , Pending_input.Revision.to_json t.expected_pending_revision )
      ; "history_id", History.Id.to_json t.history_id
      ; ( "expected_content_revision"
        , History.Content_revision.to_json t.expected_content_revision )
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind session_id = J.required_as fields "session_id" Id.Session.of_json in
    let%bind attachment_id = J.required_as fields "attachment_id" Id.Attachment.of_json in
    let%bind expected_generation =
      J.required_as fields "expected_generation" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind expected_pending_revision =
      J.required_as fields "expected_pending_revision" Pending_input.Revision.of_json
    in
    let%bind history_id = J.required_as fields "history_id" History.Id.of_json in
    let%bind expected_content_revision =
      J.required_as fields "expected_content_revision" History.Content_revision.of_json
    in
    let%bind idempotency_key =
      J.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    create
      ~session_id
      ~attachment_id
      ~expected_generation
      ~expected_pending_revision
      ~history_id
      ~expected_content_revision
      ~idempotency_key
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    match of_json (to_json (unchecked_of_sexp sexp)) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Replace_request = struct
  type t =
    { target : Cancel_request.t
    ; text : string
    }
  [@@deriving sexp]

  let create ~target ~text =
    let%map.Result _ =
      History_edit.create
        ~history_id:target.Cancel_request.history_id
        ~expected_content_revision:target.expected_content_revision
        ~text
        ~mode:Save_only
    in
    { target; text }
  ;;

  let to_json t =
    match Cancel_request.to_json t.target with
    | `Object fields -> `Object (fields @ [ "text", `String t.text ])
    | _ -> assert false
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind target = Cancel_request.of_json json in
    let%bind fields = J.fields json in
    let%bind text = J.required_as fields "text" J.string in
    create ~target ~text
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    match of_json (to_json (unchecked_of_sexp sexp)) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Result = struct
  type t =
    { pending_revision : Pending_input.Revision.t
    ; outcome : Pending_query.Outcome.t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "pending_revision", Pending_input.Revision.to_json t.pending_revision
      ; "outcome", Pending_query.Outcome.to_json t.outcome
      ; "mutation", Mutation_result.to_json t.mutation
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Projection_codec.validate json in
    let%bind fields = J.fields json in
    let%bind pending_revision =
      J.required_as fields "pending_revision" Pending_input.Revision.of_json
    in
    let%bind outcome = J.required_as fields "outcome" Pending_query.Outcome.of_json in
    let%map mutation = J.required_as fields "mutation" Mutation_result.of_json in
    { pending_revision; outcome; mutation }
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    match of_json (to_json (unchecked_of_sexp sexp)) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end
