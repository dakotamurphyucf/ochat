open! Core
module C = Recovery_notes.Catalog
module D = Document_schema
module P = Agent_protocol

type t = C.t

let invalid message = P.Error.invalid_request message

let validate t ~session_id =
  if P.Id.Session.equal (P.Session_ref.session_id (C.owner t)) session_id
  then Ok ()
  else Error (invalid "recovery note catalog belongs to another session")
;;

let check_owner t ~owner =
  match t with
  | None -> Ok ()
  | Some catalog when P.Session_ref.equal (C.owner catalog) owner -> Ok ()
  | Some _ -> Error (invalid "recovery note catalog belongs to another host or session")
;;

let for_owner t ~owner =
  let open Result.Let_syntax in
  let%bind () = check_owner t ~owner in
  match t with
  | None -> C.empty ~owner
  | Some catalog -> Ok catalog
;;

let document_result result =
  Result.map_error result ~f:(fun _ -> invalid "invalid durable recovery note catalog")
;;

let to_json t = C.to_document t |> document_result |> Result.map ~f:D.Document.json

let of_json json ~session_id =
  let open Result.Let_syntax in
  let%bind document =
    D.Document.inspect ~limits:D.Limits.default json |> document_result
  in
  let%bind fields = P.Json_codec.fields (D.Document.payload document) in
  let%bind owner = P.Json_codec.required_as fields "owner" P.Session_ref.of_json in
  let%bind catalog = C.of_document document ~owner |> document_result in
  let%map () = validate catalog ~session_id in
  catalog
;;

let check_unchanged previous ~incoming =
  let open Result.Let_syntax in
  let discarded () =
    Error (invalid "replacement changed durable recovery note custody")
  in
  match previous, incoming with
  | None, None -> Ok ()
  | Some previous, Some incoming when phys_equal previous incoming -> Ok ()
  | Some previous, Some incoming ->
    let%bind previous = to_json previous in
    let%bind incoming = to_json incoming in
    if D.Json.equal previous incoming then Ok () else discarded ()
  | None, Some _ | Some _, None -> discarded ()
;;
