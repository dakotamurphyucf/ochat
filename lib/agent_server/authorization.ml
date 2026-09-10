open Core

let required_scope = function
  | Agent_protocol.Command.Protocol_initialize _
  | Protocol_ping _
  | Server_info
  | Server_health _ -> None
  | Prompt_list _ | Prompt_get _ -> Some Agent_protocol.Scope.List_prompts
  | Workspace_list _ | Workspace_get _ -> Some List_workspaces
  | Blob_read _ -> Some View_session_transcript
  | Session_create _ -> Some Create_sessions
  | Session_list _
  | Session_get _
  | Session_attach _
  | Session_detach _
  | Session_renew_owner _
  | Session_export _ -> Some View_session_transcript
  | Session_start _
  | Session_send_message _
  | Session_compact _
  | Session_delete_history _
  | Session_cancel_operation _ -> Some Send_messages
  | Session_stop _ -> Some Stop_sessions
  | Session_reset _ | Session_rebuild _ | Session_upgrade_prompt _ -> Some Own_sessions
  | Session_delete _ -> Some Delete_sessions
  | Permission_list _ -> Some View_security_state
  | Permission_respond _ -> Some Answer_approvals
  | Grant_list _ | Grant_revoke _ -> Some Manage_grants
  | Audit_read _ -> Some Read_audit
  | Ingress_submit _ -> Some Submit_ingress
  | Job_list _
  | Job_get _
  | Job_cancel _
  | Schedule_list _
  | Schedule_get _
  | Schedule_create _
  | Schedule_cancel _ -> Some Send_messages
;;

let authorize principal command =
  match required_scope command with
  | None -> Ok ()
  | Some scope when Agent_protocol.Principal.has_scope principal scope -> Ok ()
  | Some scope ->
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:("missing scope " ^ Agent_protocol.Scope.to_string scope)
         ~retryable:false
         ())
;;
