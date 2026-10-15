open! Core
module P = Agent_protocol
module Service = Authoring_context_service

module Source : sig
  type t =
    { read : Authoring_source_host.Read_policy.t
    ; document : Authoring_source_host.Document_policy.t
    ; limits : Authoring_source_host.Limits.t
    }

  val create
    :  read:Authoring_source_host.Read_policy.t
    -> document:Authoring_source_host.Document_policy.t
    -> limits:Authoring_source_host.Limits.t
    -> t
end

module Upload : sig
  type t =
    { limits : Agent_store.Authoring_upload_store.Limits.t
    ; grant : Authoring_upload_host.Upload_policy.t
    ; publication : Authoring_upload_host.Publication.t option
    ; rebind : Authoring_upload_host.Rebind.t option
    }

  val create
    :  limits:Agent_store.Authoring_upload_store.Limits.t
    -> grant:Authoring_upload_host.Upload_policy.t
    -> publication:Authoring_upload_host.Publication.t option
    -> rebind:Authoring_upload_host.Rebind.t option
    -> t
end

module Semantic : sig
  type t =
    { script_selection : Service.Semantic.Script_selection.Policy.t option
    ; project_metadata : Service.Semantic.Project_metadata.Policy.t option
    ; project_build_limits : Chatml_project_build.Limits.t
    ; limits : Service.Semantic.Limits.t
    ; prefix_limits : Service.Semantic.Prefix_limits.t
    ; query_limits : Service.Semantic.Query_limits.t
    ; document_dir : Eio.Fs.dir_ty Eio.Path.t
    }

  val create
    :  script_selection:Service.Semantic.Script_selection.Policy.t option
    -> project_metadata:Service.Semantic.Project_metadata.Policy.t option
    -> project_build_limits:Chatml_project_build.Limits.t
    -> limits:Service.Semantic.Limits.t
    -> prefix_limits:Service.Semantic.Prefix_limits.t
    -> query_limits:Service.Semantic.Query_limits.t
    -> document_dir:Eio.Fs.dir_ty Eio.Path.t
    -> t
end

module Assistance : sig
  type t =
    { lexical : bool
    ; assistance : bool
    ; selected_tools : Service.Assistance.Selected_tool_policy.t option
    ; project_build_limits : Chatml_project_build.Limits.t
    ; limits : Service.Assistance.Limits.t
    }

  val create
    :  lexical:bool
    -> assistance:bool
    -> selected_tools:Service.Assistance.Selected_tool_policy.t option
    -> project_build_limits:Chatml_project_build.Limits.t
    -> limits:Service.Assistance.Limits.t
    -> t Or_error.t
end

type t =
  { core : bool
  ; source : Source.t option
  ; upload : Upload.t option
  ; supplied_document : Service.Supplied_document_policy.t option
  ; selection : Service.Selection_policy.t
  ; context_limits : Chatml_authoring_context.Limits.t
  ; analysis_policy : Chatml_authoring_analysis.Policy.t
  ; protocol_limits : P.Authoring.Limits.t
  ; service_limits : Service.Limits.t
  ; semantic : Semantic.t option
  ; assistance : Assistance.t option
  ; configured_projects : Authoring_configured_project_policy.t option
  }

val validate_initialize : t -> P.Initialize.Limits.t -> (unit, P.Error.t) result

val create
  :  core:bool
  -> source:Source.t option
  -> upload:Upload.t option
  -> supplied_document:Service.Supplied_document_policy.t option
  -> selection:Service.Selection_policy.t
  -> context_limits:Chatml_authoring_context.Limits.t
  -> analysis_policy:Chatml_authoring_analysis.Policy.t
  -> protocol_limits:P.Authoring.Limits.t
  -> service_limits:Service.Limits.t
  -> semantic:Semantic.t option
  -> assistance:Assistance.t option
  -> initialize_limits:P.Initialize.Limits.t
  -> (t, P.Error.t) result

val basic_supplied : initialize_limits:P.Initialize.Limits.t -> (t, P.Error.t) result

val upload_only
  :  initialize_limits:P.Initialize.Limits.t
  -> upload:Upload.t
  -> (t, P.Error.t) result

val with_configured_projects
  :  t
  -> bound:Authoring_configured_project_policy.t
  -> source_limits:Authoring_source_host.Limits.t
  -> analysis_policy:Chatml_authoring_analysis.Policy.t
  -> (t, P.Error.t) result
