open Core

let required_scope = function
  | Agent_protocol.Command.Provider_status _ -> Some Agent_protocol.Scope.Provider_view
  | Provider_select _ -> Some Agent_protocol.Scope.Provider_select
  | Provider_setup _
  | Provider_login_begin _
  | Provider_login_challenge _
  | Provider_login_cancel _
  | Provider_logout _
  | Provider_configure_environment _ -> Some Agent_protocol.Scope.Provider_manage
  | Agent_protocol.Command.Protocol_initialize _
  | Command_receipt _
  | Protocol_ping _
  | Server_info
  | Server_health _ -> None
  | Session_configuration_get _ -> Some Agent_protocol.Scope.View_session_transcript
  | Session_configuration_update _ -> Some Agent_protocol.Scope.Send_messages
  | Session_inference_summary _ -> None
  | Session_inference_observations _ -> Some Agent_protocol.Scope.View_session_transcript
  | Prompt_list _ | Prompt_get _ -> Some Agent_protocol.Scope.List_prompts
  | Workspace_list _ | Workspace_get _ -> Some List_workspaces
  | Blob_read _ -> Some View_session_transcript
  | Session_create _ -> Some Create_sessions
  | Activity_list _
  | Session_work _
  | Session_search _
  | Session_search_navigate _
  | Session_list _
  | Session_get _
  | Session_attach _
  | Session_detach _
  | Session_renew_owner _
  | Session_export _ -> Some View_session_transcript
  | Project_get _ | Project_list _ | Collection_get _ | Collection_list _ ->
    Some Agent_protocol.Scope.View_organization
  | Project_create _
  | Project_update _
  | Project_delete _
  | Collection_create _
  | Collection_update _
  | Collection_delete _ -> Some Agent_protocol.Scope.Manage_organization
  | Session_update_organization _
  | Session_update_metadata _
  | Session_start _
  | Session_send_message _
  | Session_compact _
  | Session_edit_history _
  | Session_continue_history _
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

let authorize_attachment_mode principal mode =
  if not (Agent_protocol.Principal.has_scope principal View_session_transcript)
  then
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:"attachment requires transcript scope"
         ~retryable:false
         ())
  else (
    match mode with
    | Agent_protocol.Session.Read_only -> Ok ()
    | Read_write ->
      if Agent_protocol.Principal.has_scope principal Send_messages
      then Ok ()
      else
        Error
          (Agent_protocol.Error.create
             Permission_denied
             ~message:"read/write attachment requires send_messages"
             ~retryable:false
             ())
    | Owner_read_write ->
      if
        Agent_protocol.Principal.has_scope principal Send_messages
        && Agent_protocol.Principal.has_scope principal Own_sessions
      then Ok ()
      else
        Error
          (Agent_protocol.Error.create
             Permission_denied
             ~message:"owner attachment requires send_messages and own_sessions"
             ~retryable:false
             ()))
;;

let authorize principal command =
  let open Result.Let_syntax in
  let%bind () =
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
  in
  match command with
  | (Agent_protocol.Command.Activity_list _ | Session_work _)
    when not (Agent_protocol.Principal.has_scope principal View_security_state) ->
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:"activity requires security visibility"
         ~retryable:false
         ())
  | Agent_protocol.Command.Session_create { requested_mode = Some mode; _ } ->
    authorize_attachment_mode principal mode
  | Session_attach request -> authorize_attachment_mode principal request.requested_mode
  | Session_update_organization _
    when not (Agent_protocol.Principal.has_scope principal Manage_organization) ->
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:"membership mutation requires organization.manage"
         ~retryable:false
         ())
  | Session_configuration_update request
    when Option.is_some (Agent_protocol.Session_configuration.Patch.profile request.patch)
         && not (Agent_protocol.Principal.has_scope principal Provider_select) ->
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:"profile selection requires provider.select"
         ~retryable:false
         ())
  | Session_inference_observations request
    when (request.include_configuration || request.include_diagnostics)
         && not (Agent_protocol.Principal.has_scope principal Diagnostics) ->
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:"detailed inference observations require diagnostics scope"
         ~retryable:false
         ())
  | _ -> Ok ()
;;
