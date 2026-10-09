open! Core
module P = Agent_protocol

type t =
  { connection : Connection.t
  ; server_id : P.Id.Server.t
  }

let create connection ~server_id = { connection; server_id }

let check_host t query =
  if not (P.Id.Server.equal t.server_id (P.Search_query.server_id query))
  then Error (P.Error.invalid_request "search query belongs to another host")
  else (
    match Connection.initialization t.connection with
    | Some initialized when P.Id.Server.equal initialized.server_id t.server_id -> Ok ()
    | Some _ -> Error (P.Error.invalid_request "search connection names another host")
    | None -> Error (P.Error.invalid_request "search requires an initialized connection"))
;;

let page t query =
  let open Result.Let_syntax in
  let%bind () = check_host t query in
  match Connection.request_without_history t.connection (Session_search query) with
  | Ok (Session_search page) -> Ok page
  | Ok _ -> Error (P.Error.invalid_request "unexpected session.search response")
  | Error _ as error -> error
;;

let navigate t request =
  let open Result.Let_syntax in
  let%bind () = check_host t (P.Search_navigation.Request.query request) in
  match
    Connection.request_without_history t.connection (Session_search_navigate request)
  with
  | Ok (Session_search_navigate response) -> Ok response
  | Ok _ -> Error (P.Error.invalid_request "unexpected session.search.navigate response")
  | Error _ as error -> error
;;
