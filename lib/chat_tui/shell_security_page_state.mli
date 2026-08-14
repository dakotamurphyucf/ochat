open! Core

type tab =
  | Overview
  | Runtimes
  | Grants
  | Audit
  | Interrupted
[@@deriving sexp_of, compare, equal]

type runtime =
  { id : string
  ; profile : string option
  ; sandbox : string
  ; network : bool
  ; audit : string
  }

type snapshot =
  { manifest_sha256 : string option
  ; live_manifest_sha256 : string option
  ; runtimes : runtime list
  ; manifest_grants : Session.Shell_state.Manifest_grant.persisted list
  ; grants : Session.Shell_state.Approval_grant.persisted list
  ; administrative_policy : string
  ; signature_status : string
  ; audit_status : string
  ; interrupted_requests : Session.Shell_state.Interrupted_request.t list
  }

type approval_choice =
  | Once
  | Exact_session
  | Prefix_session
  | Durable_exact
[@@deriving sexp_of, compare, equal]

type deny_editor =
  { mutable reason : string
  ; mutable cursor : int
  }

type approval_stage =
  | Choose
  | Confirm_prefix of string list
  | Confirm_durable
  | Deny_reason of deny_editor

type approval_modal =
  { request : Shell_runtime.Approval_broker.ui_request
  ; queue_count : int
  ; mutable selected : approval_choice
  ; mutable more_options : bool
  ; mutable details_expanded : bool
  ; mutable stage : approval_stage
  }

type grant_revoke_stage =
  | Confirm_revoke
  | Revoking
  | Revoke_failed of string

type grant_revoke_modal =
  { generation : int
  ; grant_id : string
  ; runtime_id : string
  ; command_sha256 : string
  ; mutable stage : grant_revoke_stage
  }

type moderator_modal =
  { request : Chat_response.In_memory_stream.pending_ui_request
  ; mutable response : string
  ; mutable cursor : int
  ; mutable selected_choice : int
  ; mutable validation_error : string option
  }

type audit_event =
  { sequence : int64
  ; timestamp : float
  ; name : string
  }

type audit_request =
  { request_id : string
  ; runtime_id : string
  ; request_kind : string
  ; command_sha256 : string
  ; effects : string list
  ; policy_action : string option
  ; approval_answer : string option
  ; backend : string option
  ; stdout_bytes : int
  ; stderr_bytes : int
  ; result : string
  ; events : audit_event list
  }

type audit_page =
  { path : string
  ; integrity : string
  ; total_requests : int
  ; requests : audit_request list
  ; last_sequence : int64 option
  }

type audit_load_state =
  | Audit_not_loaded
  | Audit_loading of int
  | Audit_loaded of audit_page
  | Audit_failed of string

type t =
  { scroll_box : Notty_scroll_box.t
  ; mutable tab : tab
  ; mutable snapshot : snapshot
  ; mutable selected_grant_id : string option
  ; mutable approval_modal : approval_modal option
  ; mutable grant_revoke_modal : grant_revoke_modal option
  ; mutable moderator_modal : moderator_modal option
  ; mutable audit_load_state : audit_load_state
  ; mutable selected_audit_request_id : string option
  ; mutable next_management_generation : int
  }

val empty_snapshot : snapshot
val empty : unit -> t
