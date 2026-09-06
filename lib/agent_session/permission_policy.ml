open! Core

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

let reviewer_identity = function
  | None -> None
  | Some reviewer ->
    Some
      ( Permission_reviewer.id reviewer
      , Permission_reviewer.kind reviewer
      , Permission_reviewer.revision reviewer )
;;

let digest
      ~id
      ~tool_default
      ~approval_timeout_ms
      ~fallback
      ~manifest_authorization
      ~evaluator_revision
      ~reviewer
  =
  [%sexp
    { id : string
    ; tool_default : tool_default
    ; approval_timeout_ms : int option
    ; fallback : fallback
    ; manifest_authorization : manifest_authorization
    ; evaluator_revision : string option
    ; reviewer : (string * Permission_reviewer.kind * string) option
    }]
  |> Sexp.to_string_mach
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let create
      ~id
      ~tool_default
      ~approval_timeout_ms
      ~fallback
      ~manifest_authorization
      ~evaluator
      ~evaluator_revision
      ~reviewer
  =
  if String.is_empty id
  then
    Error (Agent_protocol.Error.invalid_request "permission policy ID must be nonempty")
  else if Option.exists approval_timeout_ms ~f:(fun value -> value < 0)
  then Error (Agent_protocol.Error.invalid_request "approval timeout must be nonnegative")
  else if equal_tool_default tool_default Policy && Option.is_none evaluator
  then
    Error
      (Agent_protocol.Error.invalid_request "policy tool default requires an evaluator")
  else if not (Bool.equal (Option.is_some evaluator) (Option.is_some evaluator_revision))
  then
    Error
      (Agent_protocol.Error.invalid_request
         "permission evaluator and evaluator revision must be configured together")
  else if Option.value_map evaluator_revision ~default:false ~f:String.is_empty
  then
    Error
      (Agent_protocol.Error.invalid_request
         "permission evaluator revision must be nonempty")
  else if
    match fallback, reviewer with
    | Fallback_reviewer id, Some reviewer ->
      not (String.equal id (Permission_reviewer.id reviewer))
    | Fallback_reviewer _, None -> true
    | (Fallback_allow | Fallback_deny | Fallback_allow_if_policy), _ -> false
  then Error (Agent_protocol.Error.invalid_request "permission reviewer is unavailable")
  else
    Ok
      { id
      ; revision_digest =
          digest
            ~id
            ~tool_default
            ~approval_timeout_ms
            ~fallback
            ~manifest_authorization
            ~evaluator_revision
            ~reviewer:(reviewer_identity reviewer)
      ; tool_default
      ; approval_timeout_ms
      ; fallback
      ; manifest_authorization
      ; evaluator
      ; evaluator_revision
      ; reviewer
      }
;;

let review t invocation =
  match t.reviewer with
  | Some reviewer -> Permission_reviewer.review reviewer invocation
  | None ->
    Error
      Permission_reviewer.Error.
        { code = "reviewer.unavailable"; message = "permission reviewer is unavailable" }
;;

let policy_allows t invocation =
  match t.evaluator with
  | None -> false
  | Some evaluator ->
    (match evaluator invocation with
     | Ok true -> true
     | Ok false | Error _ -> false)
;;

let fallback t invocation reason =
  match t.fallback with
  | Fallback_allow -> Allow_now
  | Fallback_deny -> Deny_now reason
  | Fallback_allow_if_policy ->
    if policy_allows t invocation then Allow_now else Deny_now reason
  | Fallback_reviewer _ -> Request_review
;;

let decide t ~responder_available invocation =
  match t.tool_default with
  | Allow -> Allow_now
  | Deny -> Deny_now "tool invocation denied by permission profile"
  | Ask when responder_available -> Request_permission
  | Ask -> fallback t invocation "no permission responder is available"
  | Policy ->
    (match Option.value_exn t.evaluator invocation with
     | Ok true -> Allow_now
     | Ok false -> Deny_now "tool invocation denied by policy evaluator"
     | Error message ->
       fallback t invocation ("permission policy evaluator failed: " ^ message))
;;

let prefix_identity_digest ~tool_name =
  Digestif.SHA256.(digest_string ("tool-prefix:" ^ tool_name) |> to_hex)
;;

let manifest_authorizer t ~request_grant =
  match t.manifest_authorization with
  | Require_grant -> request_grant
  | Assume_authorized -> Shell_runtime.Manifest_authorizer.assume_authorized
  | Deny_manifest -> Shell_runtime.Manifest_authorizer.deny
;;
