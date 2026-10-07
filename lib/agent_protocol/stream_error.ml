open Core

type t =
  { session_id : Id.Session.t
  ; attachment_id : Id.Attachment.t
  ; error : Protocol_error.t
  }
[@@deriving sexp_of]

let create ~session_id ~attachment_id error = { session_id; attachment_id; error }

let to_json t =
  `Object
    [ "session_id", Id.Session.to_json t.session_id
    ; "attachment_id", Id.Attachment.to_json t.attachment_id
    ; "error", Protocol_error.to_json t.error
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Projection_codec.validate json in
  let%bind fields = Json_codec.fields json in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%bind attachment_id =
    Json_codec.required_as fields "attachment_id" Id.Attachment.of_json
  in
  let%map error = Json_codec.required_as fields "error" Protocol_error.of_json in
  create ~session_id ~attachment_id error
;;

let to_notification t =
  Envelope.notification ~method_:"session.stream_error" ~params:(to_json t) ()
;;
