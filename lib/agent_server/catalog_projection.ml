open Core

let prompt (entry : Agent_session.Prompt_catalog.entry) =
  let availability, current_revision =
    match entry.availability with
    | Ready revision ->
      Agent_protocol.Prompt.Available, Some (Agent_session.Prompt_revision.id revision)
    | Unavailable diagnostics ->
      let reason =
        List.map diagnostics ~f:(fun value ->
          value.Agent_session.Prompt_revision_builder.Diagnostic.message)
        |> String.concat ~sep:"; "
      in
      Unavailable { reason }, None
    | Disabled -> Unavailable { reason = "prompt is disabled" }, None
  in
  Agent_protocol.Prompt.
    { id = entry.definition.id
    ; name = entry.definition.config_name
    ; description = entry.definition.description
    ; enabled = entry.definition.enabled
    ; availability
    ; current_revision
    ; allowed_workspaces = entry.definition.allowed_workspaces
    ; permission_profile = entry.definition.permission_profile
    ; runtime_policy = entry.definition.runtime_policy
    }
;;

let access = function
  | Agent_session.Workspace_definition.Read_only -> Agent_protocol.Workspace.Read_only
  | Shared_write -> Shared_write
  | Exclusive -> Exclusive
;;

let cleanup = function
  | Agent_session.Workspace_definition.On_session_stop ->
    Agent_protocol.Workspace.On_session_stop
  | On_session_delete -> On_session_delete
  | Retain -> Retain
;;

let overflow = function
  | Agent_session.Workspace_definition.Reject -> Agent_protocol.Workspace.Reject
  | Queue -> Queue
;;

let workspace (definition : Agent_session.Workspace_definition.t) =
  let kind, temporary_location, cleanup_policy =
    match definition.source with
    | Physical _ -> Agent_protocol.Workspace.Physical, None, None
    | Temporary { location; cleanup = policy; _ } ->
      let location =
        match location with
        | Agent_session.Workspace_definition.System_tmp ->
          Agent_protocol.Workspace.System_tmp
        | Session_dir -> Session_dir
      in
      Temporary, Some location, Some (cleanup policy)
  in
  Agent_protocol.Workspace.
    { id = definition.id
    ; name = definition.config_name
    ; kind
    ; temporary_location
    ; cleanup = cleanup_policy
    ; access = access definition.access
    ; conflict_domain = definition.conflict_domain
    ; prompt_limits =
        List.map definition.prompt_limits ~f:(fun limit ->
          { prompt_id = limit.prompt_id
          ; max_root_agents = limit.max_root_agents
          ; overflow = overflow limit.overflow
          })
    ; availability = Available
    }
;;
