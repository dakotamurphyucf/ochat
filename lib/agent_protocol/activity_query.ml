open! Core
module Error = Protocol_error
module J = Json_codec

type t =
  { server_id : Id.Server.t
  ; catalog : Session.List_request.t
  ; reasons : Session_activity.Reason.t list
  ; scan_limit : int
  }
[@@deriving sexp]

let create ~server_id ~catalog ~reasons ~scan_limit =
  let open Result.Let_syntax in
  let%bind catalog =
    Session.List_request.of_json (Session.List_request.to_json catalog)
  in
  if scan_limit < 1 || scan_limit > 4096
  then Error (Error.invalid_request "activity scan_limit must be between 1 and 4096")
  else if List.contains_dup reasons ~compare:Session_activity.Reason.compare
  then Error (Error.invalid_request "duplicate activity reason filter")
  else
    Ok
      { server_id
      ; catalog
      ; reasons = List.sort reasons ~compare:Session_activity.Reason.compare
      ; scan_limit
      }
;;

let to_json t =
  match Session.List_request.to_json t.catalog with
  | `Object fields ->
    `Object
      (("server_id", Id.Server.to_json t.server_id)
       :: ("reasons", `Array (List.map t.reasons ~f:Session_activity.Reason.to_json))
       :: ("scan_limit", `Number (Int.to_string t.scan_limit))
       :: fields)
  | _ -> assert false
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind server_id = J.required_as fields "server_id" Id.Server.of_json in
  let%bind catalog = Session.List_request.of_json json in
  let%bind reasons =
    J.required_as fields "reasons" (J.list Session_activity.Reason.of_json)
  in
  let%bind scan_limit =
    J.required_as fields "scan_limit" (J.bounded_int ~min:1 ~max:4096)
  in
  create ~server_id ~catalog ~reasons ~scan_limit
;;

let t_of_sexp sexp =
  let raw = t_of_sexp sexp in
  match of_json (to_json raw) with
  | Ok t -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;
