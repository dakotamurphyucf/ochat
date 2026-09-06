open! Core

type t = Hook_worker.t

let create worker = worker

let scope request name expires_at =
  match name with
  | "exact_session" -> Ok (Shell_access.Approval.Exact_session { expires_at })
  | "prefix_session" ->
    Ok
      (Prefix_session
         { prefix = Shell_access.Command.to_argv request.Shell_access.Approval.context.command
         ; expires_at
         })
  | "durable_exact" -> Ok (Durable_exact { expires_at })
  | _ -> Error "hook returned an unsupported approval scope"
;;

let command = function
  | [] -> Error "hook returned an empty rewrite"
  | program :: arguments -> Ok (Shell_access.Command.create program arguments)
;;

let review t request =
  let open Result.Let_syntax in
  let%bind action = Hook_worker.invoke t (Hook_payload.approval_request request) in
  match action with
  | Hook_protocol.Defer -> Ok None
  | Approve_once -> Ok (Some Shell_access.Approval.Approve)
  | Approve_scope { scope = name; expires_at } ->
    Result.map_error (scope request name expires_at) ~f:(fun message ->
      Hook_worker.{ code = "shell.hook_scope"; message; stderr = None })
    |> Result.map ~f:(fun scope -> Some (Shell_access.Approval.Approve_for scope))
  | Deny reason -> Ok (Some (Shell_access.Approval.Deny reason))
  | Reviewer_rewrite argv ->
    Result.map_error (command argv) ~f:(fun message ->
      Hook_worker.{ code = "shell.hook_rewrite"; message; stderr = None })
    |> Result.map ~f:(fun command -> Some (Shell_access.Approval.Rewrite command))
  | _ ->
    Error
      Hook_worker.
        { code = "shell.hook_action"; message = "invalid reviewer action"; stderr = None }
;;
