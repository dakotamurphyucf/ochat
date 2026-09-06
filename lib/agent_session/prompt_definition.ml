open! Core

type t =
  { id : Agent_protocol.Id.Prompt_definition.t
  ; config_name : string
  ; root_file : string
  ; allowed_workspaces : Agent_protocol.Id.Workspace_definition.t list
  ; permission_profile : string
  ; runtime_policy : string option
  ; enabled : bool
  ; description : string option
  }
[@@deriving sexp]

let create
      ~id
      ~config_name
      ~root_file
      ~allowed_workspaces
      ~permission_profile
      ~runtime_policy
      ~enabled
      ~description
  =
  if String.is_empty config_name
  then Error (Agent_store.Store_error.Corrupt "prompt config name must be nonempty")
  else if not (Filename.is_absolute root_file)
  then Error (Agent_store.Store_error.Corrupt "prompt root file must be absolute")
  else if String.is_empty permission_profile
  then
    Error (Agent_store.Store_error.Corrupt "prompt permission profile must be nonempty")
  else if
    List.contains_dup
      allowed_workspaces
      ~compare:Agent_protocol.Id.Workspace_definition.compare
  then
    Error (Agent_store.Store_error.Corrupt "prompt allowed workspaces contain duplicates")
  else
    Ok
      { id
      ; config_name
      ; root_file
      ; allowed_workspaces
      ; permission_profile
      ; runtime_policy
      ; enabled
      ; description
      }
;;

let allows_workspace t workspace_id =
  List.mem t.allowed_workspaces workspace_id ~equal:(fun left right ->
    Agent_protocol.Id.Workspace_definition.compare left right = 0)
;;
