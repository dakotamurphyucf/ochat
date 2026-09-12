open Core

module Diagnostic = struct
  type t =
    { code : string
    ; config_path : string
    ; source_file : string
    ; message : string
    ; remediation : string
    }
  [@@deriving compare, equal, sexp]
end

module Server = struct
  type reverse_proxy =
    { trusted_addresses : string list
    ; principal_header : string
    ; scopes_header : string
    }
  [@@deriving compare, equal, sexp]

  type journal_flush =
    | Each
    | Interval
    | Unsafe_buffered
  [@@deriving compare, equal, sexp]

  type http =
    { enabled : bool
    ; address : string
    ; port : int
    ; require_auth : bool
    ; static_tokens_file : string option
    ; oauth_validator : string option
    ; reverse_proxy : reverse_proxy option
    ; max_connections : int
    ; idle_connection_timeout_ms : int
    }
  [@@deriving compare, equal, sexp]

  type durability =
    { journal_flush : journal_flush
    ; journal_flush_ms : int
    ; snapshot_every_events : int
    ; snapshot_every_ms : int
    }
  [@@deriving compare, equal, sexp]

  type event_retention =
    { completed_stream_ms : int
    ; response_artifact_ms : int
    ; max_events_per_session : int
    }
  [@@deriving compare, equal, sexp]

  type job_limits =
    { daemon_total : int
    ; per_principal : int
    ; per_prompt : int
    ; per_workspace : int
    ; per_session : int
    ; per_kind : int
    ; max_nested_depth : int
    }
  [@@deriving compare, equal, sexp]

  type t =
    { data_dir : string
    ; session_helpers : Session_helper_policy.t list [@sexp.list]
    ; authoring_packages : Chat_response.Authoring_package_file.t list [@sexp.list]
    ; authoring_budget : Chat_response.Authoring_validation.context_budget option
          [@sexp.option]
    ; unix_socket : string
    ; http : http
    ; shutdown_grace_ms : int
    ; max_attachments_per_session : int
    ; subscriber_queue_capacity : int
    ; event_retention : event_retention
    ; durability : durability
    ; job_limits : job_limits
    ; unsafe_allow_unauthenticated_remote_http : bool
    }
  [@@deriving compare, equal, sexp]
end

module Workspace = struct
  type temporary_location =
    | System_tmp
    | Session_dir
  [@@deriving compare, equal, sexp]

  type cleanup =
    | On_session_stop
    | On_session_delete
    | Retain
  [@@deriving compare, equal, sexp]

  type source =
    | Physical of string
    | Temporary of
        { location : temporary_location
        ; cleanup : cleanup
        ; managed_root : string option
        }
  [@@deriving compare, equal, sexp]

  type access =
    | Read_only
    | Shared_write
    | Exclusive
  [@@deriving compare, equal, sexp]

  type overflow =
    | Reject
    | Queue
  [@@deriving compare, equal, sexp]

  type prompt_limit =
    { prompt : string
    ; max_root_agents : int
    ; overflow : overflow
    }
  [@@deriving compare, equal, sexp]

  type t =
    { id : string
    ; source : source
    ; access : access
    ; conflict_domain : string option
    ; prompt_limits : prompt_limit list
    }
  [@@deriving compare, equal, sexp]
end

module Prompt = struct
  type t =
    { id : string
    ; path : string
    ; description : string option
    ; allowed_workspaces : string list
    ; permission_profile : string
    ; runtime_policy : string option
    ; enabled : bool
    }
  [@@deriving compare, equal, sexp]
end

module Permission_profile = struct
  type tool_default =
    | Ask
    | Policy
    | Allow
    | Deny
  [@@deriving compare, equal, sexp]

  type approval_fallback =
    | Deny
    | Allow
    | Allow_if_policy
    | Model_reviewer of string
    | External_reviewer of string
  [@@deriving compare, equal, sexp]

  type manifest_authorization =
    | Require_grant
    | Assume_authorized
    | Deny
  [@@deriving compare, equal, sexp]

  type t =
    { id : string
    ; tool_default : tool_default
    ; approval_timeout_ms : int option
    ; approval_fallback : approval_fallback
    ; manifest_authorization : manifest_authorization
    }
  [@@deriving compare, equal, sexp]
end

module Manifest_grant = struct
  type t =
    { id : string
    ; prompt : string
    ; workspaces : string list
    ; manifest_sha256 : string
    ; source_sha256 : string
    ; principals : string list
    }
  [@@deriving compare, equal, sexp]
end

type t =
  { version : int
  ; source_file : string
  ; server : Server.t
  ; workspaces : Workspace.t list
  ; prompts : Prompt.t list
  ; permission_profiles : Permission_profile.t list
  ; manifest_grants : Manifest_grant.t list
  }
[@@deriving compare, equal, sexp]

let current_version = 1

let find_workspace t id =
  List.find t.workspaces ~f:(fun value -> String.equal value.id id)
;;

let find_prompt t id = List.find t.prompts ~f:(fun value -> String.equal value.id id)

let find_permission_profile t id =
  List.find t.permission_profiles ~f:(fun value -> String.equal value.id id)
;;

let find_manifest_grant t id =
  List.find t.manifest_grants ~f:(fun value -> String.equal value.id id)
;;
