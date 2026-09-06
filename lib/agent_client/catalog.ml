open! Core

let invalid message = Agent_protocol.Error.invalid_request message

let page_request () =
  Agent_protocol.Page.Request.create ~limit:10_000 ()
  |> Result.map_error ~f:(fun error -> error)
;;

let prompts connection =
  let open Result.Let_syntax in
  let%bind page = page_request () in
  let request =
    Agent_protocol.Prompt.List_request.{ page; enabled = None; available = None }
  in
  match Connection.request connection (Prompt_list request) with
  | Ok (Prompt_list result) -> Ok result.items
  | Ok _ -> Error (invalid "unexpected prompt.list result")
  | Error _ as failure -> failure
;;

let workspaces connection =
  let open Result.Let_syntax in
  let%bind page = page_request () in
  let request =
    Agent_protocol.Workspace.List_request.
      { page; kind = None; access = None; available = None }
  in
  match Connection.request connection (Workspace_list request) with
  | Ok (Workspace_list result) -> Ok result.items
  | Ok _ -> Error (invalid "unexpected workspace.list result")
  | Error _ as failure -> failure
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
