open! Core

let invalid message = Agent_protocol.Error.invalid_request message

let prompts_page connection request =
  match Connection.request_without_history connection (Prompt_list request) with
  | Ok (Prompt_list result) -> Ok result
  | Ok _ -> Error (invalid "unexpected prompt.list result")
  | Error _ as failure -> failure
;;

let workspaces_page connection request =
  match Connection.request_without_history connection (Workspace_list request) with
  | Ok (Workspace_list result) -> Ok result
  | Ok _ -> Error (invalid "unexpected workspace.list result")
  | Error _ as failure -> failure
;;

let enumerate_prompts
      connection
      ~(query : Agent_protocol.Prompt.List_request.t)
      ~max_prompts
      ~max_pages
  =
  Page_enumeration.collect query.page ~max_items:max_prompts ~max_pages ~read:(fun page ->
    prompts_page connection { query with page })
;;

let enumerate_workspaces
      connection
      ~(query : Agent_protocol.Workspace.List_request.t)
      ~max_workspaces
      ~max_pages
  =
  Page_enumeration.collect
    query.page
    ~max_items:max_workspaces
    ~max_pages
    ~read:(fun page -> workspaces_page connection { query with page })
;;

let prompts connection =
  let open Result.Let_syntax in
  let%bind page = Agent_protocol.Page.Request.create ~limit:1000 () in
  let query =
    Agent_protocol.Prompt.List_request.{ page; enabled = None; available = None }
  in
  enumerate_prompts connection ~query ~max_prompts:100_000 ~max_pages:100
;;

let workspaces connection =
  let open Result.Let_syntax in
  let%bind page = Agent_protocol.Page.Request.create ~limit:1000 () in
  let query =
    Agent_protocol.Workspace.List_request.
      { page; kind = None; access = None; available = None }
  in
  enumerate_workspaces connection ~query ~max_workspaces:100_000 ~max_pages:100
;;

let find_unique values ~name ~name_of ~kind =
  match List.filter values ~f:(fun value -> String.equal (name_of value) name) with
  | [ value ] -> Ok value
  | [] -> Error (invalid (Printf.sprintf "%s %S was not found" kind name))
  | _ -> Error (invalid (Printf.sprintf "%s name %S is ambiguous" kind name))
;;

let resolve_prompt connection ~name =
  Result.bind (prompts connection) ~f:(fun values ->
    find_unique values ~name ~kind:"prompt" ~name_of:(fun value ->
      value.Agent_protocol.Prompt.name))
;;

let resolve_workspace connection ~name =
  Result.bind (workspaces connection) ~f:(fun values ->
    find_unique values ~name ~kind:"workspace" ~name_of:(fun value ->
      value.Agent_protocol.Workspace.name))
;;
