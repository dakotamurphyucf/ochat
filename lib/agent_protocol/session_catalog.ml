open! Core

type t =
  { session : Session.t
  ; active_owner_principal_id : Id.Principal.t option
  ; archived : bool
  }
[@@deriving sexp]

let to_json t =
  match Session.to_json t.session with
  | `Object fields ->
    let owner =
      match t.active_owner_principal_id with
      | None -> []
      | Some id -> [ "active_owner_principal_id", Id.Principal.to_json id ]
    in
    `Object (fields @ [ ("archived", if t.archived then `True else `False) ] @ owner)
  | _ -> assert false
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind session = Session.of_json json in
  let%bind fields = Json_codec.fields json in
  let%bind archived = Json_codec.optional_as fields "archived" Json_codec.bool in
  let%map active_owner_principal_id =
    Json_codec.optional_as fields "active_owner_principal_id" Id.Principal.of_json
  in
  { session; active_owner_principal_id; archived = Option.value archived ~default:false }
;;
