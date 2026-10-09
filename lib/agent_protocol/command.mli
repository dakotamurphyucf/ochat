(** Typed method dispatch for transport-neutral protocol requests. *)

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
  | Session_search of Search_query.t
  | Session_search_navigate of Search_navigation.Request.t
  | Activity_list of Activity_query.t
  | Session_work of Session_work.Query.t
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
  | Session_edit_history of History_edit.Edit_request.t
  | Session_continue_history of History_edit.Continue_request.t
  | Session_delete_history of Session.Delete_history_request.t
  | Session_export of Session.Export_request.t
  | Session_reset of Session.Reset_request.t
  | Session_rebuild of Session.Rebuild_request.t
  | Session_upgrade_prompt of Session.Upgrade_prompt_request.t
  | Session_delete of Session.Delete_request.t
  | Session_restore of Session_lifecycle.Request.t
  | Session_resume of Session_lifecycle.Request.t
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

(** [method_name t] returns the stable protocol method. *)
val method_name : t -> string

(** [params t] encodes the command parameters. *)
val params : t -> Jsonaf.t

(** [of_method_and_params ~method_ ~params] performs closed typed dispatch. *)
val of_method_and_params : method_:string -> params:Jsonaf.t -> (t, Error.t) result

(** [supported_methods] contains every method accepted by the closed dispatcher. *)
val supported_methods : string list
