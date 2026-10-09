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
      { organization = Agent_protocol.Session_organization.Query.default
      ; page
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

let create_project connection request =
  match Connection.request_without_history connection (Project_create request) with
  | Ok (Project_create value) -> Ok value
  | Ok _ -> Error (invalid "unexpected project.create result")
  | Error _ as failure -> failure
;;

let get_project connection request =
  match Connection.request_without_history connection (Project_get request) with
  | Ok (Project_get value) -> Ok value
  | Ok _ -> Error (invalid "unexpected project.get result")
  | Error _ as failure -> failure
;;

let list_projects_page connection request =
  match Connection.request_without_history connection (Project_list request) with
  | Ok (Project_list value) -> Ok value
  | Ok _ -> Error (invalid "unexpected project.list result")
  | Error _ as failure -> failure
;;

let update_project connection request =
  match Connection.request_without_history connection (Project_update request) with
  | Ok (Project_update value) -> Ok value
  | Ok _ -> Error (invalid "unexpected project.update result")
  | Error _ as failure -> failure
;;

let delete_project connection request =
  match Connection.request_without_history connection (Project_delete request) with
  | Ok (Project_delete value) -> Ok value
  | Ok _ -> Error (invalid "unexpected project.delete result")
  | Error _ as failure -> failure
;;

let create_collection connection request =
  match Connection.request_without_history connection (Collection_create request) with
  | Ok (Collection_create value) -> Ok value
  | Ok _ -> Error (invalid "unexpected collection.create result")
  | Error _ as failure -> failure
;;

let get_collection connection request =
  match Connection.request_without_history connection (Collection_get request) with
  | Ok (Collection_get value) -> Ok value
  | Ok _ -> Error (invalid "unexpected collection.get result")
  | Error _ as failure -> failure
;;

let list_collections_page connection request =
  match Connection.request_without_history connection (Collection_list request) with
  | Ok (Collection_list value) -> Ok value
  | Ok _ -> Error (invalid "unexpected collection.list result")
  | Error _ as failure -> failure
;;

let update_collection connection request =
  match Connection.request_without_history connection (Collection_update request) with
  | Ok (Collection_update value) -> Ok value
  | Ok _ -> Error (invalid "unexpected collection.update result")
  | Error _ as failure -> failure
;;

let delete_collection connection request =
  match Connection.request_without_history connection (Collection_delete request) with
  | Ok (Collection_delete value) -> Ok value
  | Ok _ -> Error (invalid "unexpected collection.delete result")
  | Error _ as failure -> failure
;;

let enumerate_organization connection ~query ~max_groups ~max_pages ~list_page =
  let open Result.Let_syntax in
  if
    max_groups <= 0
    || max_pages <= 0
    || Option.is_some query.Agent_protocol.Organization_request.List.page.cursor
  then Error (invalid "organization enumeration requires positive bounds and fresh query")
  else (
    let rec loop (query : Agent_protocol.Organization_request.List.t) pages count reverse =
      let%bind page = list_page connection query in
      let count = count + List.length page.Agent_protocol.Page.items in
      if count > max_groups
      then Error (invalid "organization enumeration exceeds max_groups")
      else (
        let reverse = List.rev_append page.items reverse in
        match page.next_cursor with
        | None -> Ok (List.rev reverse)
        | Some cursor when pages >= max_pages ->
          Error (invalid "organization enumeration exceeds max_pages")
        | Some cursor ->
          loop
            { query with page = { query.page with cursor = Some cursor } }
            (pages + 1)
            count
            reverse)
    in
    loop query 1 0 [])
;;

let enumerate_projects connection ~query ~max_groups ~max_pages =
  enumerate_organization
    connection
    ~query
    ~max_groups
    ~max_pages
    ~list_page:list_projects_page
;;

let enumerate_collections connection ~query ~max_groups ~max_pages =
  enumerate_organization
    connection
    ~query
    ~max_groups
    ~max_pages
    ~list_page:list_collections_page
;;

let restore_session connection request =
  match Connection.request_without_history connection (Session_restore request) with
  | Ok (Session_restore value) -> Ok value
  | Ok _ -> Error (invalid "unexpected session.restore result")
  | Error _ as failure -> failure
;;

let resume_session connection request =
  match Connection.request_without_history connection (Session_resume request) with
  | Ok (Session_resume value) -> Ok value
  | Ok _ -> Error (invalid "unexpected session.resume result")
  | Error _ as failure -> failure
;;
