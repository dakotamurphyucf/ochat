open! Core

(** Compiled interactive and unattended tool authorization policy. *)

type tool_default =
  | Ask
  | Policy
  | Allow
  | Deny
[@@deriving compare, equal, sexp]

type fallback =
  | Fallback_allow
  | Fallback_deny
  | Fallback_allow_if_policy
  | Fallback_reviewer of string
[@@deriving compare, equal, sexp]

type manifest_authorization =
  | Require_grant
  | Assume_authorized
  | Deny_manifest
[@@deriving compare, equal, sexp]

type invocation = Permission_reviewer.Request.t =
  { tool_name : string
  ; identity_digest : string
  ; invocation_display : string
  ; effects : string list
  }
[@@deriving sexp]

type decision =
  | Allow_now
  | Deny_now of string
  | Request_permission
  | Request_review
[@@deriving compare, equal, sexp]

type evaluator = invocation -> (bool, string) result

type t =
  { id : string
  ; revision_digest : string
  ; tool_default : tool_default
  ; approval_timeout_ms : int option
  ; fallback : fallback
  ; manifest_authorization : manifest_authorization
  ; evaluator : evaluator option
  ; evaluator_revision : string option
  ; reviewer : Permission_reviewer.t option
  }

val create
  :  id:string
  -> tool_default:tool_default
  -> approval_timeout_ms:int option
  -> fallback:fallback
  -> manifest_authorization:manifest_authorization
  -> evaluator:evaluator option
  -> evaluator_revision:string option
  -> reviewer:Permission_reviewer.t option
  -> (t, Agent_protocol.Error.t) result

(** [decide] never silently waits when no responder is available. *)
val decide : t -> responder_available:bool -> invocation -> decision

(** [review t invocation] invokes the pinned model or external reviewer.
    Missing, malformed, and raised reviewer outcomes remain typed failures. *)
val review
  :  t
  -> invocation
  -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result

(** [prefix_identity_digest ~tool_name] returns the stable identity stored by
    a session-prefix grant for [tool_name]. *)
val prefix_identity_digest : tool_name:string -> string

val manifest_authorizer
  :  t
  -> request_grant:Shell_runtime.Manifest_authorizer.t
  -> Shell_runtime.Manifest_authorizer.t
