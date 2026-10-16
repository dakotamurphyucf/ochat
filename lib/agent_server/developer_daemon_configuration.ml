open! Core

module Remote_content = struct
  type t = Developer_child_remote_content.Policy.t

  let create = Developer_child_remote_content.Policy.create
end

module Limits = struct
  type t =
    { store : Agent_store.Developer_run_store.Limits.t
    ; host : Developer_host.Limits.t
    ; budgets : Developer_command_budgets.Configuration.t
    ; agent_sources : Developer_agent_source_runtime.Limits.t
    ; max_native_bindings : int
    ; workspace_cleanup : Agent_session.Workspace_cleanup.Limits.t
    ; max_agent_cache_entries : int
    }

  let create
        ~store
        ~roots
        ~max_active_commands
        ~disclosure
        ~compilation
        ~max_dependency_depth
        ~execution
        ~max_output_bytes
        ~receipt
        ~max_agent_sources
        ~max_agent_source_work_steps
        ~max_native_bindings
        ~workspace_cleanup
        ~max_agent_cache_entries
    =
    let open Result.Let_syntax in
    let%bind host =
      Developer_host.Limits.create ~roots ~max_active_commands ~disclosure
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request (Error.to_string_hum error))
    in
    let%bind budgets =
      Developer_command_budgets.Configuration.create
        ~compilation
        ~max_dependency_depth
        ~execution
        ~max_output_bytes
        ~receipt
    in
    let%bind agent_sources =
      Developer_agent_source_runtime.Limits.create
        ~max_agents:max_agent_sources
        ~max_work_steps:max_agent_source_work_steps
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request (Error.to_string_hum error))
    in
    if max_native_bindings <= 0 || max_agent_cache_entries <= 0
    then
      Error
        (Agent_protocol.Error.invalid_request
           "developer native and cache limits must be positive")
    else
      Ok
        { store
        ; host
        ; budgets
        ; agent_sources
        ; max_native_bindings
        ; workspace_cleanup
        ; max_agent_cache_entries
        }
  ;;
end

type t =
  { capture_policy : Authoring_context_service.Developer.Target_policy.t
  ; capture_limits : Authoring_context_service.Developer.Limits.t
  ; limits : Limits.t
  ; result_disclosure : Developer_result_disclosure.Policy.t
  ; receipt_read : Operator_authorization.t -> Agent_store.Developer_command_row.t -> bool
  ; command_control :
      Operator_authorization.t -> Agent_store.Developer_command_row.t -> bool
  ; publisher_limits : Agent_store.Developer_result_publisher.Limits.t
  ; publisher_storage : Agent_store.Developer_result_publisher.Storage.t
  ; model_target : Inference.Request.Target.t option
  ; remote_content : Remote_content.t option
  ; authorize_native :
      Agent_protocol.Host_invocation.t
      -> Chat_response.Tool_capability.binding
      -> (unit, Agent_protocol.Error.t) result
  ; prepare_output :
      Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, Agent_protocol.Error.t) result
  }

let create
      ~capture_policy
      ~capture_limits
      ~limits
      ~result_disclosure
      ~receipt_read
      ~command_control
      ~publisher_limits
      ~publisher_storage
      ~model_target
      ~authorize_native
      ~prepare_output
  =
  Ok
    { capture_policy
    ; capture_limits
    ; limits
    ; result_disclosure
    ; receipt_read
    ; command_control
    ; publisher_limits
    ; publisher_storage
    ; model_target
    ; remote_content = None
    ; authorize_native
    ; prepare_output
    }
;;

let with_remote_content t remote_content = { t with remote_content = Some remote_content }
