open! Core

let invalid message = Agent_protocol.Error.invalid_request message

let list_sessions connection =
  let open Result.Let_syntax in
  let%bind page = Agent_protocol.Page.Request.create ~limit:10_000 () in
  let request =
    Agent_protocol.Session.List_request.
      { page
      ; desired_state = None
      ; prompt_id = None
      ; workspace_id = None
      ; owner_principal_id = None
      ; labels = []
      }
  in
  match Connection.request connection (Session_list request) with
  | Ok (Session_list page) -> Ok page.items
  | Ok _ -> Error (invalid "unexpected session.list result")
  | Error _ as failure -> failure
;;

let get_session connection session_id =
  match Connection.request connection (Session_get { session_id; history = None }) with
  | Ok (Session_get snapshot) -> Ok snapshot
  | Ok _ -> Error (invalid "unexpected session.get result")
  | Error _ as failure -> failure
;;
