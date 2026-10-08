open! Core

type t =
  { server_id : Id.Server.t
  ; session_id : Id.Session.t
  }
[@@deriving compare, equal, sexp_of]

let create ~server_id ~session_id = { server_id; session_id }
let server_id t = t.server_id
let session_id t = t.session_id

let to_json t =
  `Object
    [ "server_id", Id.Server.to_json t.server_id
    ; "session_id", Id.Session.to_json t.session_id
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind server_id = Json_codec.required_as fields "server_id" Id.Server.of_json in
  let%map session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  create ~server_id ~session_id
;;
