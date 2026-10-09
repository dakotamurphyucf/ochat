open Core

type t =
  | Protocol_initialize of Initialize.Request.t
  | Command_receipt of Command_receipt.Request.t
  | Provider_setup of Provider_operator.Setup_request.t
  | Provider_status of Provider_operator.Status_request.t
  | Provider_login_begin of Provider_operator.Login_request.t
  | Provider_login_challenge of Provider_operator.Challenge_request.t
  | Provider_login_cancel of Provider_operator.Cancel_request.t
  | Provider_logout of Provider_operator.Logout_request.t
  | Provider_select of Provider_operator.Select_request.t
  | Provider_configure_environment of Provider_operator.Environment_request.t
  | Protocol_ping of Ping.Request.t
  | Server_info
  | Server_health of Health.Request.t
  | Prompt_list of Prompt.List_request.t
  | Prompt_get of Prompt.Get_request.t
  | Workspace_list of Workspace.List_request.t
  | Workspace_get of Workspace.Get_request.t
  | Blob_read of Blob.Read_request.t
  | Session_create of Session.Create_request.t
  | Session_list of Session.List_request.t
  | Session_configuration_get of Session_configuration.Get_request.t
  | Session_configuration_update of Session_configuration.Update_request.t
  | Session_get of Session.Get_request.t
  | Session_inference_summary of Inference_query.Summary_request.t
  | Session_inference_observations of Inference_query.Request.t
  | Session_attach of Session.Attach_request.t
  | Session_detach of Session.Detach_request.t
  | Session_renew_owner of Session.Renew_owner_request.t
  | Session_start of Session.Start_request.t
  | Project_create of Organization_request.Create.t
  | Project_get of Organization_request.Project.Get.t
  | Project_list of Organization_request.List.t
  | Project_update of Organization_request.Project.Update.t
  | Project_delete of Organization_request.Project.Delete.t
  | Collection_create of Organization_request.Create.t
  | Collection_get of Organization_request.Collection.Get.t
  | Collection_list of Organization_request.List.t
  | Collection_update of Organization_request.Collection.Update.t
  | Collection_delete of Organization_request.Collection.Delete.t
  | Session_update_metadata of Session_metadata.Request.t
  | Session_update_organization of Session_organization.Request.t
  | Session_stop of Session.Stop_request.t
  | Session_cancel_operation of Session.Cancel_operation_request.t
  | Session_send_message of Session.Send_message_request.t
  | Session_compact of Session.Compact_request.t
  | Session_delete_history of Session.Delete_history_request.t
  | Session_export of Session.Export_request.t
  | Session_reset of Session.Reset_request.t
  | Session_rebuild of Session.Rebuild_request.t
  | Session_upgrade_prompt of Session.Upgrade_prompt_request.t
  | Session_delete of Session.Delete_request.t
  | Permission_list of Permission.List_request.t
  | Permission_respond of Permission.Respond_request.t
  | Grant_list of Grant.List_request.t
  | Grant_revoke of Grant.Revoke_request.t
  | Audit_read of Audit.Read_request.t
  | Job_list of Job.List_request.t
  | Job_get of Job.Get_request.t
  | Job_cancel of Job.Cancel_request.t
  | Schedule_list of Schedule.List_request.t
  | Schedule_get of Schedule.Get_request.t
  | Schedule_create of Schedule.Create_request.t
  | Schedule_cancel of Schedule.Cancel_request.t
  | Ingress_submit of Ingress.Submit_request.t
[@@deriving sexp]

let method_name = function
  | Protocol_initialize _ -> "protocol.initialize"
  | Command_receipt _ -> "command.receipt"
  | Provider_setup _ -> "provider.setup"
  | Provider_status _ -> "provider.status"
  | Provider_login_begin _ -> "provider.login.begin"
  | Provider_login_challenge _ -> "provider.login.challenge"
  | Provider_login_cancel _ -> "provider.login.cancel"
  | Provider_logout _ -> "provider.logout"
  | Provider_select _ -> "provider.select"
  | Provider_configure_environment _ -> "provider.configure_environment"
  | Protocol_ping _ -> "protocol.ping"
  | Server_info -> "server.info"
  | Server_health _ -> "server.health"
  | Prompt_list _ -> "prompt.list"
  | Prompt_get _ -> "prompt.get"
  | Workspace_list _ -> "workspace.list"
  | Workspace_get _ -> "workspace.get"
  | Blob_read _ -> "blob.read"
  | Session_create _ -> "session.create"
  | Session_list _ -> "session.list"
  | Session_configuration_get _ -> "session.configuration_get"
  | Session_configuration_update _ -> "session.configuration_update"
  | Session_get _ -> "session.get"
  | Session_inference_summary _ -> "session.inference_summary"
  | Session_inference_observations _ -> "session.inference_observations"
  | Session_attach _ -> "session.attach"
  | Session_detach _ -> "session.detach"
  | Session_renew_owner _ -> "session.renew_owner"
  | Session_start _ -> "session.start"
  | Project_create _ -> "project.create"
  | Project_get _ -> "project.get"
  | Project_list _ -> "project.list"
  | Project_update _ -> "project.update"
  | Project_delete _ -> "project.delete"
  | Collection_create _ -> "collection.create"
  | Collection_get _ -> "collection.get"
  | Collection_list _ -> "collection.list"
  | Collection_update _ -> "collection.update"
  | Collection_delete _ -> "collection.delete"
  | Session_update_metadata _ -> "session.update_metadata"
  | Session_update_organization _ -> "session.update_organization"
  | Session_stop _ -> "session.stop"
  | Session_cancel_operation _ -> "session.cancel_operation"
  | Session_send_message _ -> "session.send_message"
  | Session_compact _ -> "session.compact"
  | Session_delete_history _ -> "session.delete_history"
  | Session_export _ -> "session.export"
  | Session_reset _ -> "session.reset"
  | Session_rebuild _ -> "session.rebuild"
  | Session_upgrade_prompt _ -> "session.upgrade_prompt"
  | Session_delete _ -> "session.delete"
  | Permission_list _ -> "permission.list"
  | Permission_respond _ -> "permission.respond"
  | Grant_list _ -> "grant.list"
  | Grant_revoke _ -> "grant.revoke"
  | Audit_read _ -> "audit.read"
  | Job_list _ -> "job.list"
  | Job_get _ -> "job.get"
  | Job_cancel _ -> "job.cancel"
  | Schedule_list _ -> "schedule.list"
  | Schedule_get _ -> "schedule.get"
  | Schedule_create _ -> "schedule.create"
  | Schedule_cancel _ -> "schedule.cancel"
  | Ingress_submit _ -> "ingress.submit"
;;

let params = function
  | Protocol_initialize request -> Initialize.Request.to_json request
  | Command_receipt request -> Command_receipt.Request.to_json request
  | Provider_setup request -> Provider_operator.Setup_request.to_json request
  | Provider_status request -> Provider_operator.Status_request.to_json request
  | Provider_login_begin request -> Provider_operator.Login_request.to_json request
  | Provider_login_challenge request ->
    Provider_operator.Challenge_request.to_json request
  | Provider_login_cancel request -> Provider_operator.Cancel_request.to_json request
  | Provider_logout request -> Provider_operator.Logout_request.to_json request
  | Provider_select request -> Provider_operator.Select_request.to_json request
  | Provider_configure_environment request ->
    Provider_operator.Environment_request.to_json request
  | Protocol_ping request -> Ping.Request.to_json request
  | Server_info -> `Object []
  | Server_health request -> Health.Request.to_json request
  | Prompt_list request -> Prompt.List_request.to_json request
  | Prompt_get request -> Prompt.Get_request.to_json request
  | Workspace_list request -> Workspace.List_request.to_json request
  | Workspace_get request -> Workspace.Get_request.to_json request
  | Blob_read request -> Blob.Read_request.to_json request
  | Session_create request -> Session.Create_request.to_json request
  | Session_list request -> Session.List_request.to_json request
  | Session_configuration_get request -> Session_configuration.Get_request.to_json request
  | Session_configuration_update request ->
    Session_configuration.Update_request.to_json request
  | Session_get request -> Session.Get_request.to_json request
  | Session_inference_summary request -> Inference_query.Summary_request.to_json request
  | Session_inference_observations request -> Inference_query.Request.to_json request
  | Session_attach request -> Session.Attach_request.to_json request
  | Session_detach request -> Session.Detach_request.to_json request
  | Session_renew_owner request -> Session.Renew_owner_request.to_json request
  | Session_start request -> Session.Start_request.to_json request
  | Project_create value -> Organization_request.Create.to_json value
  | Project_get value -> Organization_request.Project.Get.to_json value
  | Project_list value -> Organization_request.List.to_json value
  | Project_update value -> Organization_request.Project.Update.to_json value
  | Project_delete value -> Organization_request.Project.Delete.to_json value
  | Collection_create value -> Organization_request.Create.to_json value
  | Collection_get value -> Organization_request.Collection.Get.to_json value
  | Collection_list value -> Organization_request.List.to_json value
  | Collection_update value -> Organization_request.Collection.Update.to_json value
  | Collection_delete value -> Organization_request.Collection.Delete.to_json value
  | Session_update_metadata request -> Session_metadata.Request.to_json request
  | Session_update_organization request -> Session_organization.Request.to_json request
  | Session_stop request -> Session.Stop_request.to_json request
  | Session_cancel_operation request -> Session.Cancel_operation_request.to_json request
  | Session_send_message request -> Session.Send_message_request.to_json request
  | Session_compact request -> Session.Compact_request.to_json request
  | Session_delete_history request -> Session.Delete_history_request.to_json request
  | Session_export request -> Session.Export_request.to_json request
  | Session_reset request -> Session.Reset_request.to_json request
  | Session_rebuild request -> Session.Rebuild_request.to_json request
  | Session_upgrade_prompt request -> Session.Upgrade_prompt_request.to_json request
  | Session_delete request -> Session.Delete_request.to_json request
  | Permission_list request -> Permission.List_request.to_json request
  | Permission_respond request -> Permission.Respond_request.to_json request
  | Grant_list request -> Grant.List_request.to_json request
  | Grant_revoke request -> Grant.Revoke_request.to_json request
  | Audit_read request -> Audit.Read_request.to_json request
  | Job_list request -> Job.List_request.to_json request
  | Job_get request -> Job.Get_request.to_json request
  | Job_cancel request -> Job.Cancel_request.to_json request
  | Schedule_list request -> Schedule.List_request.to_json request
  | Schedule_get request -> Schedule.Get_request.to_json request
  | Schedule_create request -> Schedule.Create_request.to_json request
  | Schedule_cancel request -> Schedule.Cancel_request.to_json request
  | Ingress_submit request -> Ingress.Submit_request.to_json request
;;

let method_not_found method_ =
  Protocol_error.create
    Protocol_error.Method_not_found
    ~message:("unknown protocol method: " ^ method_)
    ~retryable:false
    ()
;;

let decode_server_info params =
  let open Result.Let_syntax in
  let%map fields = Json_codec.fields params in
  ignore (fields : Json_codec.fields);
  Server_info
;;

let map decode wrap params = Result.map (decode params) ~f:wrap

let decoders =
  [ "protocol.initialize", map Initialize.Request.of_json (fun x -> Protocol_initialize x)
  ; "command.receipt", map Command_receipt.Request.of_json (fun x -> Command_receipt x)
  ; ( "provider.setup"
    , map Provider_operator.Setup_request.of_json (fun x -> Provider_setup x) )
  ; ( "provider.status"
    , map Provider_operator.Status_request.of_json (fun x -> Provider_status x) )
  ; ( "provider.login.begin"
    , map Provider_operator.Login_request.of_json (fun x -> Provider_login_begin x) )
  ; ( "provider.login.challenge"
    , map Provider_operator.Challenge_request.of_json (fun x ->
        Provider_login_challenge x) )
  ; ( "provider.login.cancel"
    , map Provider_operator.Cancel_request.of_json (fun x -> Provider_login_cancel x) )
  ; ( "provider.logout"
    , map Provider_operator.Logout_request.of_json (fun x -> Provider_logout x) )
  ; ( "provider.select"
    , map Provider_operator.Select_request.of_json (fun x -> Provider_select x) )
  ; ( "provider.configure_environment"
    , map Provider_operator.Environment_request.of_json (fun x ->
        Provider_configure_environment x) )
  ; "protocol.ping", map Ping.Request.of_json (fun x -> Protocol_ping x)
  ; "server.info", decode_server_info
  ; "server.health", map Health.Request.of_json (fun x -> Server_health x)
  ; "prompt.list", map Prompt.List_request.of_json (fun x -> Prompt_list x)
  ; "prompt.get", map Prompt.Get_request.of_json (fun x -> Prompt_get x)
  ; "workspace.list", map Workspace.List_request.of_json (fun x -> Workspace_list x)
  ; "workspace.get", map Workspace.Get_request.of_json (fun x -> Workspace_get x)
  ; "blob.read", map Blob.Read_request.of_json (fun x -> Blob_read x)
  ; "session.create", map Session.Create_request.of_json (fun x -> Session_create x)
  ; "session.list", map Session.List_request.of_json (fun x -> Session_list x)
  ; ( "session.configuration_get"
    , map Session_configuration.Get_request.of_json (fun x -> Session_configuration_get x)
    )
  ; ( "session.configuration_update"
    , map Session_configuration.Update_request.of_json (fun x ->
        Session_configuration_update x) )
  ; "session.get", map Session.Get_request.of_json (fun x -> Session_get x)
  ; ( "session.inference_summary"
    , map Inference_query.Summary_request.of_json (fun x -> Session_inference_summary x) )
  ; ( "session.inference_observations"
    , map Inference_query.Request.of_json (fun x -> Session_inference_observations x) )
  ; "session.attach", map Session.Attach_request.of_json (fun x -> Session_attach x)
  ; "session.detach", map Session.Detach_request.of_json (fun x -> Session_detach x)
  ; ( "session.renew_owner"
    , map Session.Renew_owner_request.of_json (fun x -> Session_renew_owner x) )
  ; ( "project.create"
    , map Organization_request.Create.of_json (fun value -> Project_create value) )
  ; ( "project.get"
    , map Organization_request.Project.Get.of_json (fun value -> Project_get value) )
  ; ( "project.list"
    , map Organization_request.List.of_json (fun value -> Project_list value) )
  ; ( "project.update"
    , map Organization_request.Project.Update.of_json (fun value -> Project_update value)
    )
  ; ( "project.delete"
    , map Organization_request.Project.Delete.of_json (fun value -> Project_delete value)
    )
  ; ( "collection.create"
    , map Organization_request.Create.of_json (fun value -> Collection_create value) )
  ; ( "collection.get"
    , map Organization_request.Collection.Get.of_json (fun value -> Collection_get value)
    )
  ; ( "collection.list"
    , map Organization_request.List.of_json (fun value -> Collection_list value) )
  ; ( "collection.update"
    , map Organization_request.Collection.Update.of_json (fun value ->
        Collection_update value) )
  ; ( "collection.delete"
    , map Organization_request.Collection.Delete.of_json (fun value ->
        Collection_delete value) )
  ; "session.start", map Session.Start_request.of_json (fun x -> Session_start x)
  ; ( "session.update_organization"
    , map Session_organization.Request.of_json (fun x -> Session_update_organization x) )
  ; ( "session.update_metadata"
    , map Session_metadata.Request.of_json (fun x -> Session_update_metadata x) )
  ; "session.stop", map Session.Stop_request.of_json (fun x -> Session_stop x)
  ; ( "session.cancel_operation"
    , map Session.Cancel_operation_request.of_json (fun x -> Session_cancel_operation x) )
  ; ( "session.send_message"
    , map Session.Send_message_request.of_json (fun x -> Session_send_message x) )
  ; "session.compact", map Session.Compact_request.of_json (fun x -> Session_compact x)
  ; ( "session.delete_history"
    , map Session.Delete_history_request.of_json (fun x -> Session_delete_history x) )
  ; "session.export", map Session.Export_request.of_json (fun x -> Session_export x)
  ; "session.reset", map Session.Reset_request.of_json (fun x -> Session_reset x)
  ; "session.rebuild", map Session.Rebuild_request.of_json (fun x -> Session_rebuild x)
  ; ( "session.upgrade_prompt"
    , map Session.Upgrade_prompt_request.of_json (fun x -> Session_upgrade_prompt x) )
  ; "session.delete", map Session.Delete_request.of_json (fun x -> Session_delete x)
  ; "permission.list", map Permission.List_request.of_json (fun x -> Permission_list x)
  ; ( "permission.respond"
    , map Permission.Respond_request.of_json (fun x -> Permission_respond x) )
  ; "grant.list", map Grant.List_request.of_json (fun x -> Grant_list x)
  ; "grant.revoke", map Grant.Revoke_request.of_json (fun x -> Grant_revoke x)
  ; "audit.read", map Audit.Read_request.of_json (fun x -> Audit_read x)
  ; "job.list", map Job.List_request.of_json (fun x -> Job_list x)
  ; "job.get", map Job.Get_request.of_json (fun x -> Job_get x)
  ; "job.cancel", map Job.Cancel_request.of_json (fun x -> Job_cancel x)
  ; "schedule.list", map Schedule.List_request.of_json (fun x -> Schedule_list x)
  ; "schedule.get", map Schedule.Get_request.of_json (fun x -> Schedule_get x)
  ; "schedule.create", map Schedule.Create_request.of_json (fun x -> Schedule_create x)
  ; "schedule.cancel", map Schedule.Cancel_request.of_json (fun x -> Schedule_cancel x)
  ; "ingress.submit", map Ingress.Submit_request.of_json (fun x -> Ingress_submit x)
  ]
;;

let of_method_and_params ~method_ ~params =
  match List.Assoc.find decoders method_ ~equal:String.equal with
  | Some decode -> decode params
  | None -> Error (method_not_found method_)
;;

let supported_methods = List.map decoders ~f:fst
