open! Core

type t =
  { workspaces : Agent_session.Workspace_catalog.t
  ; prompts : Agent_session.Prompt_definition.t list
  ; permission_profiles : Agent_session.Permission_policy.t list
  ; manifest_grants : Operator_manifest_grant.t list
  }

type reviewer_resolver =
  Agent_session.Permission_reviewer.kind
  -> string
  -> Agent_session.Permission_reviewer.t option

type policy_evaluator_resolver =
  string -> (string * Agent_session.Permission_policy.evaluator) option

let prompt_id name = Catalog_identity.prompt_definition name
let workspace_id name = Catalog_identity.workspace_definition name

let workspace_source = function
  | Config.Workspace.Physical configured_root ->
    Agent_session.Workspace_definition.Physical { configured_root }
  | Temporary { location; cleanup; managed_root } ->
    let location =
      match location with
      | Config.Workspace.System_tmp -> Agent_session.Workspace_definition.System_tmp
      | Session_dir -> Session_dir
    in
    let cleanup =
      match cleanup with
      | Config.Workspace.On_session_stop ->
        Agent_session.Workspace_definition.On_session_stop
      | On_session_delete -> On_session_delete
      | Retain -> Retain
    in
    Temporary { location; cleanup; managed_root }
;;

let workspace_access = function
  | Config.Workspace.Read_only -> Agent_session.Workspace_definition.Read_only
  | Shared_write -> Shared_write
  | Exclusive -> Exclusive
;;

let prompt_limit (limit : Config.Workspace.prompt_limit) =
  let overflow =
    match limit.overflow with
    | Config.Workspace.Reject -> Agent_session.Workspace_definition.Reject
    | Queue -> Queue
  in
  Agent_session.Workspace_definition.
    { prompt_id = prompt_id limit.prompt
    ; max_root_agents = limit.max_root_agents
    ; overflow
    }
;;

let workspace (value : Config.Workspace.t) =
  Agent_session.Workspace_definition.create
    ~id:(workspace_id value.id)
    ~config_name:value.id
    ~source:(workspace_source value.source)
    ~access:(workspace_access value.access)
    ~conflict_domain:value.conflict_domain
    ~prompt_limits:(List.map value.prompt_limits ~f:prompt_limit)
;;

let prompt (value : Config.Prompt.t) =
  Agent_session.Prompt_definition.create
    ~id:(prompt_id value.id)
    ~config_name:value.id
    ~root_file:value.path
    ~allowed_workspaces:(List.map value.allowed_workspaces ~f:workspace_id)
    ~permission_profile:value.permission_profile
    ~runtime_policy:value.runtime_policy
    ~enabled:value.enabled
    ~description:value.description
;;

let resolve_reviewer reviewer_resolver kind id =
  Option.bind reviewer_resolver ~f:(fun resolve -> resolve kind id)
  |> Option.filter ~f:(fun reviewer ->
    String.equal id (Agent_session.Permission_reviewer.id reviewer)
    && Agent_session.Permission_reviewer.equal_kind
         kind
         (Agent_session.Permission_reviewer.kind reviewer))
  |> Option.value
       ~default:
         (Agent_session.Permission_reviewer.unavailable
            ~id
            ~kind
            ~message:"configured permission reviewer is not installed")
;;

let fallback reviewer_resolver = function
  | Config.Permission_profile.Allow ->
    Agent_session.Permission_policy.Fallback_allow, None
  | Deny -> Fallback_deny, None
  | Allow_if_policy -> Fallback_allow_if_policy, None
  | Model_reviewer id ->
    ( Fallback_reviewer id
    , Some (resolve_reviewer reviewer_resolver Agent_session.Permission_reviewer.Model id)
    )
  | External_reviewer id ->
    ( Fallback_reviewer id
    , Some
        (resolve_reviewer reviewer_resolver Agent_session.Permission_reviewer.External id)
    )
;;

let unavailable_policy_evaluator =
  "unavailable-v1", fun _ -> Error "no external permission policy evaluator is configured"
;;

let policy_evaluator resolver (value : Config.Permission_profile.t) =
  match Option.bind resolver ~f:(fun resolve -> resolve value.id) with
  | Some (revision, evaluator) -> Some evaluator, Some revision
  | None when Poly.equal value.tool_default Config.Permission_profile.Policy ->
    let revision, evaluator = unavailable_policy_evaluator in
    Some evaluator, Some revision
  | None -> None, None
;;

let permission_profile
      reviewer_resolver
      policy_evaluator_resolver
      (value : Config.Permission_profile.t)
  =
  let tool_default =
    match value.tool_default with
    | Config.Permission_profile.Ask -> Agent_session.Permission_policy.Ask
    | Policy -> Policy
    | Allow -> Allow
    | Deny -> Deny
  in
  let fallback, reviewer = fallback reviewer_resolver value.approval_fallback in
  let manifest_authorization =
    match value.manifest_authorization with
    | Config.Permission_profile.Require_grant ->
      Agent_session.Permission_policy.Require_grant
    | Assume_authorized -> Assume_authorized
    | Deny -> Deny_manifest
  in
  let evaluator, evaluator_revision = policy_evaluator policy_evaluator_resolver value in
  Agent_session.Permission_policy.create
    ~id:value.id
    ~tool_default
    ~approval_timeout_ms:value.approval_timeout_ms
    ~fallback
    ~manifest_authorization
    ~evaluator
    ~evaluator_revision
    ~reviewer
;;

let build ?reviewer_resolver ?policy_evaluator_resolver config =
  let open Result.Let_syntax in
  let%bind workspace_definitions =
    Result.all (List.map config.Config.workspaces ~f:workspace)
  in
  let%bind workspaces = Agent_session.Workspace_catalog.create workspace_definitions in
  let%bind prompts = Result.all (List.map config.prompts ~f:prompt) in
  let%bind permission_profiles =
    Result.all
      (List.map config.permission_profiles ~f:(fun profile ->
         permission_profile reviewer_resolver policy_evaluator_resolver profile))
    |> Result.map_error ~f:(fun error ->
      Agent_store.Store_error.Corrupt error.Agent_protocol.Error.message)
  in
  let%map manifest_grants =
    Result.all (List.map config.manifest_grants ~f:Operator_manifest_grant.create)
    |> Result.map_error ~f:(fun error ->
      Agent_store.Store_error.Corrupt error.Agent_protocol.Error.message)
  in
  { workspaces; prompts; permission_profiles; manifest_grants }
;;
