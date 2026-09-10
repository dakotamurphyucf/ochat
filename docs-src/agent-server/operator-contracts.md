# Operator contract source appendix

Generated from the current tree; do not hand-edit.

Read [configuration](configuration.md), [protocol](protocol.md),
[HTTP](transports/http.md) and [environment](environment.md) first.
These complete excerpts pin the config record, scope codec and HTTP header/body
validation contract so documentation checks detect contract drift. They are
reference source, not standalone compilable examples.

Flag inventories collect literal option strings and named Core Command flag
declarations (displayed with a leading dash). Generated help/version options
and parser-added aliases are not enumerated; consult each executable's help
and [command reference](../bin/README.md) for accepted combinations.

## chat_tui.ml flag inventory

[Parser/normalizer](../../bin/chat_tui.ml).

`--archive`, `--authorize-shell-manifest`, `--auto-persist`, `--bearer-token-file`, `--build-info`, `--cancel`, `--config`, `--connect`, `--delete-session`, `--detached`, `--disconnect-grace-ms`, `--dry-run`, `--export-file`, `--export-session`, `--format`, `--help`, `--help-short`, `--json`, `--keep-history`, `--list-sessions`, `--local`, `--new-daemon-session`, `--new-session`, `--no-config`, `--no-parallel-tool-calls`, `--no-persist`, `--out`, `--owner-bound`, `--parallel-tool-calls`, `--print-effective-args`, `--prompt`, `--prompt-file`, `--read-only`, `--rebuild-from-prompt`, `--reset-session`, `--session`, `--session-info`, `--start-session`, `--stop-session`, `--textmate-grammar`, `--typeahead`, `--typeahead-debounce-ms`, `--typeahead-history-messages`, `--typeahead-max-output-tokens`, `--typeahead-model`, `--version`, `--workspace`, `-build-info`, `-file`, `-h`, `-help`, `-prompt-preview-max`, `-query`, `-version`

## ochat_agent_server.ml flag inventory

[Parser/normalizer](../../bin/ochat_agent_server.ml).

`-config`, `-dry-run`, `-import-legacy`, `-inspect-store`, `-migrate-store`, `-print-config`, `-prompt`, `-validate-only`, `-workspace`

## ochat_agent_stdio.ml flag inventory

[Parser/normalizer](../../bin/ochat_agent_stdio.ml).

`--bearer-token-file`, `--connect`, `--data-root`, `--local`, `--prompt`, `--workspace`

## HTTP route inventory

[Dispatcher](../../lib/agent_transport_http/server.ml).

```ocaml
  | `POST, [ "v1"; "rpc" ] -> handle_rpc t request principal
  | `POST, [ "v1"; "blobs" ] -> handle_blob_upload t request principal
  | `GET, [ "v1"; "blobs"; id ] -> handle_blob_download t principal id
  | `GET, [ "v1"; "sessions"; id; "events" ] ->
  | `GET, [ "v1"; "sessions"; id; "snapshot" ] -> handle_snapshot t request principal id
  | `GET, [ "v1"; "health" ] -> handle_health t principal
  | `GET, [ "v1"; "events" ] -> handle_events t ~request_sw request principal
  | `DELETE, [ "v1"; "connection" ] -> handle_close t request principal

```
## config.mli

[Source](../../lib/agent_server/config.mli)

```ocaml
(** Validated immutable daemon configuration. *)

module Diagnostic : sig
  type t =
    { code : string
    ; config_path : string
    ; source_file : string
    ; message : string
    ; remediation : string
    }
  [@@deriving compare, equal, sexp]
end

module Server : sig
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

module Workspace : sig
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

module Prompt : sig
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

module Permission_profile : sig
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

module Manifest_grant : sig
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

val current_version : int
val find_workspace : t -> string -> Workspace.t option
val find_prompt : t -> string -> Prompt.t option
val find_permission_profile : t -> string -> Permission_profile.t option
val find_manifest_grant : t -> string -> Manifest_grant.t option
```

## scope.ml

[Source](../../lib/agent_protocol/scope.ml)

```ocaml
open Core

module T = struct
  type t =
    | List_prompts
    | List_workspaces
    | Create_sessions
    | View_session_transcript
    | Send_messages
    | Own_sessions
    | Answer_approvals
    | View_security_state
    | Manage_grants
    | Read_audit
    | Stop_sessions
    | Delete_sessions
    | Administer_configuration
    | Diagnostics
    | Submit_ingress
  [@@deriving compare, equal, sexp]
end

include T
include Comparable.Make (T)

let to_string = function
  | List_prompts -> "prompt.list"
  | List_workspaces -> "workspace.list"
  | Create_sessions -> "session.create"
  | View_session_transcript -> "session.transcript.read"
  | Send_messages -> "session.message.send"
  | Own_sessions -> "session.own"
  | Answer_approvals -> "permission.respond"
  | View_security_state -> "security.read"
  | Manage_grants -> "grant.manage"
  | Read_audit -> "audit.read"
  | Stop_sessions -> "session.stop"
  | Delete_sessions -> "session.delete"
  | Administer_configuration -> "configuration.admin"
  | Diagnostics -> "diagnostics.read"
  | Submit_ingress -> "ingress.submit"
;;

let of_string = function
  | "prompt.list" -> Ok List_prompts
  | "workspace.list" -> Ok List_workspaces
  | "session.create" -> Ok Create_sessions
  | "session.transcript.read" -> Ok View_session_transcript
  | "session.message.send" -> Ok Send_messages
  | "session.own" -> Ok Own_sessions
  | "permission.respond" -> Ok Answer_approvals
  | "security.read" -> Ok View_security_state
  | "grant.manage" -> Ok Manage_grants
  | "audit.read" -> Ok Read_audit
  | "session.stop" -> Ok Stop_sessions
  | "session.delete" -> Ok Delete_sessions
  | "configuration.admin" -> Ok Administer_configuration
  | "diagnostics.read" -> Ok Diagnostics
  | "ingress.submit" -> Ok Submit_ingress
  | encoded -> Error (Protocol_error.invalid_request ("unknown scope: " ^ encoded))
;;

let to_json t = `String (to_string t)

let of_json = function
  | `String encoded -> of_string encoded
  | _ -> Error (Protocol_error.invalid_request "scope must be a JSON string")
;;

let set_to_json scopes = `Array (Core.Set.to_list scopes |> List.map ~f:to_json)

let set_of_json json =
  let open Result.Let_syntax in
  let%bind scopes = Json_codec.list of_json json in
  let set = Set.of_list scopes in
  if Int.equal (Core.Set.length set) (List.length scopes)
  then Ok set
  else Error (Protocol_error.invalid_request "scope array contains duplicates")
;;
```

## request_contract.ml

[Source](../../lib/agent_transport_http/request_contract.ml)

```ocaml
open! Core
module P = Piaf

let connection_header = "ochat-connection-id"
let protocol_version_header = "ochat-protocol-version"
let error code message = Agent_protocol.Error.create code ~message ~retryable:false ()
let content_type_parts value = String.split value ~on:';' |> List.map ~f:String.strip

let charset_parameter parameter =
  match String.lsplit2 parameter ~on:'=' with
  | Some (name, value) when String.Caseless.equal (String.strip name) "charset" ->
    Some (String.strip value |> String.strip ~drop:(Char.equal '"'))
  | _ -> None
;;

let require_json_content_type request =
  match P.Headers.get_multi (P.Request.headers request) "content-type" with
  | [] -> Error (error Invalid_request "content-type must be application/json")
  | [ value ] ->
    (match content_type_parts value with
     | media_type :: parameters when String.Caseless.equal media_type "application/json"
       ->
       (match List.filter_map parameters ~f:charset_parameter with
        | [] -> Ok ()
        | charsets when List.for_all charsets ~f:(String.Caseless.equal "utf-8") -> Ok ()
        | _ -> Error (error Invalid_request "JSON charset must be UTF-8"))
     | _ -> Error (error Invalid_request "content-type must be application/json"))
  | _ -> Error (error Invalid_request "content-type must appear exactly once")
;;

let require_protocol_version request =
  match P.Headers.get_multi (P.Request.headers request) protocol_version_header with
  | [ "1" ] | [ "1.0" ] -> Ok ()
  | [ _ ] -> Error (error Incompatible_protocol "unsupported HTTP protocol version")
  | [] -> Error (error Incompatible_protocol "HTTP protocol version is required")
  | _ -> Error (error Invalid_request "HTTP protocol version must appear exactly once")
;;

let bearer_token request =
  match P.Headers.get_multi (P.Request.headers request) "authorization" with
  | [] -> Ok None
  | [ value ] ->
    (match String.lsplit2 (String.strip value) ~on:' ' with
     | Some (scheme, token)
       when String.Caseless.equal scheme "Bearer"
            && (not (String.is_empty token))
            && String.for_all token ~f:(Fn.non Char.is_whitespace) -> Ok (Some token)
     | _ -> Error ())
  | _ -> Error ()
;;

let content_length_exceeds request max_body_bytes =
  P.Headers.get (P.Request.headers request) "content-length"
  |> Option.exists ~f:(fun encoded ->
    Option.value_map (Int.of_string_opt encoded) ~default:false ~f:(fun length ->
      length > max_body_bytes))
;;

let request_body ~max_body_bytes request =
  if max_body_bytes <= 0
  then Error (error Internal_error "HTTP body limit must be positive")
  else if content_length_exceeds request max_body_bytes
  then Error (error Resource_limit "HTTP request body exceeds the configured limit")
  else (
    match P.Body.to_string (P.Request.body request) with
    | Error failure -> Error (error Invalid_request (P.Error.to_string failure))
    | Ok body when String.length body > max_body_bytes ->
      Error (error Resource_limit "HTTP request body exceeds the configured limit")
    | Ok body -> Ok body)
;;

let event_cursor request =
  let header = P.Headers.get (P.Request.headers request) "last-event-id" in
  let query = Uri.get_query_param (P.Request.uri request) "after_sequence" in
  if Option.both header query |> Option.exists ~f:(fun (a, b) -> not (String.equal a b))
  then Error (error Invalid_request "event cursors disagree")
  else (
    match Option.first_some header query with
    | None -> Ok None
    | Some encoded ->
      (match Int64.of_string_opt encoded with
       | Some value when Int64.(value >= zero) -> Ok (Some value)
       | _ -> Error (error Invalid_request "event cursor is invalid")))
;;
```
