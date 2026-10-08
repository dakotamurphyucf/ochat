open! Core

let invalid message = Agent_protocol.Error.invalid_request message

let list_sessions_page connection request =
  match Connection.request_without_history connection (Session_list request) with
  | Ok (Session_list page) -> Ok page
  | Ok _ -> Error (invalid "unexpected session.list result")
  | Error _ as failure -> failure
;;

let enumerate_sessions connection ~query ~max_sessions ~max_pages =
  let open Result.Let_syntax in
  if
    max_sessions <= 0
    || max_pages <= 0
    || Option.is_some query.Agent_protocol.Session.List_request.page.cursor
  then Error (invalid "enumeration requires positive bounds and a fresh query")
  else (
    let rec loop request pages count reversed =
      let%bind page = list_sessions_page connection request in
      let count = count + List.length page.items in
      if count > max_sessions
      then Error (invalid "session enumeration exceeds max_sessions")
      else (
        let reversed = List.rev_append page.items reversed in
        match page.next_cursor with
        | None -> Ok (List.rev reversed)
        | Some cursor ->
          if pages >= max_pages
          then Error (invalid "session enumeration exceeds max_pages")
          else
            loop
              { request with page = { request.page with cursor = Some cursor } }
              (pages + 1)
              count
              reversed)
    in
    loop query 1 0 [])
;;

let list_sessions connection =
  let open Result.Let_syntax in
  let%bind page = Agent_protocol.Page.Request.create ~limit:1000 () in
  let query =
    Agent_protocol.Session.List_request.
      { page
      ; desired_state = None
      ; prompt_id = None
      ; workspace_id = None
      ; owner_principal_id = None
      ; creator_principal_id = None
      ; active_owner_principal_id = None
      ; labels = []
      ; sort = Agent_protocol.Session_catalog_query.Sort.default
      ; archive = Active
      }
  in
  let%map entries =
    enumerate_sessions connection ~query ~max_sessions:100_000 ~max_pages:100
  in
  List.map entries ~f:(fun entry -> entry.Agent_protocol.Session_catalog.session)
;;

let get_session connection session_id =
  match Connection.request connection (Session_get { session_id; history = None }) with
  | Ok (Session_get snapshot) -> Ok snapshot
  | Ok _ -> Error (invalid "unexpected session.get result")
  | Error _ as failure -> failure
;;
