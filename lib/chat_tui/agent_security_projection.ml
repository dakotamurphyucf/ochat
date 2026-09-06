open! Core
module Legacy = Session.Shell_state
module Page = Shell_security_page_state

let nanoseconds timestamp =
  timestamp
  |> Agent_protocol.Timestamp.to_time_ns
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.to_int64
;;

let optional_nanoseconds = Option.map ~f:nanoseconds

let scope = function
  | Agent_protocol.Grant.Exact_session -> Legacy.Approval_scope.Exact_session
  | Prefix_session -> Prefix_session { prefix = [] }
  | Durable_exact -> Durable_exact
;;

let approval_grant (grant : Agent_protocol.Grant.t) =
  Legacy.Approval_grant.
    { grant_id = Agent_protocol.Id.Grant.to_string grant.id
    ; manifest_sha256 = grant.identity_digest
    ; runtime_id = grant.tool_name
    ; request_kind = Structured
    ; command_sha256 = grant.identity_digest
    ; executable_sha256 = grant.identity_digest
    ; argv = []
    ; argv_prefix = None
    ; cwd_sha256 = "redacted"
    ; environment_sha256 = "redacted"
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    ; scope = scope grant.scope
    ; session_id = Some (Agent_protocol.Id.Session.to_string grant.session_id)
    ; user_id = Some (Agent_protocol.Id.Principal.to_string grant.principal_id)
    ; host_id = None
    ; created_at_ns = nanoseconds grant.created_at
    ; expires_at_ns = optional_nanoseconds grant.expires_at
    ; last_used_at_ns = None
    ; reviewer = { source = "daemon"; reviewer_id = None }
    ; revoked_at_ns = optional_nanoseconds grant.revoked_at
    ; revocation_reason = grant.revocation_reason
    }
;;

let manifest_grant (grant : Agent_protocol.Grant.t) =
  Legacy.Manifest_grant.
    { grant_id = Agent_protocol.Id.Grant.to_string grant.id
    ; manifest_sha256 = grant.identity_digest
    ; canonical_source_root = "redacted"
    ; repository_identity = None
    ; source_sha256 = grant.identity_digest
    ; signer = None
    ; issuer = None
    ; audience = []
    ; schema_version = 1
    ; builtin_versions = []
    ; imported_source_sha256 = []
    ; session_id = Some (Agent_protocol.Id.Session.to_string grant.session_id)
    ; user_id = Some (Agent_protocol.Id.Principal.to_string grant.principal_id)
    ; host_id = None
    ; created_at_ns = nanoseconds grant.created_at
    ; expires_at_ns = optional_nanoseconds grant.expires_at
    ; revoked_at_ns = optional_nanoseconds grant.revoked_at
    ; revocation_reason = grant.revocation_reason
    }
;;

let is_manifest grant = String.equal grant.Agent_protocol.Grant.tool_name "shell.manifest"

let first_active_manifest grants =
  List.find grants ~f:(fun grant ->
    Agent_protocol.Grant.equal_state grant.Agent_protocol.Grant.state Active)
  |> Option.map ~f:(fun grant -> grant.Agent_protocol.Grant.identity_digest)
;;

let permission_profile session =
  Option.value session.Agent_protocol.Session.spec.permission_profile ~default:"default"
;;

let snapshot ~(current : Page.snapshot) (projection : Agent_protocol.Snapshot.t) =
  let manifest, approval =
    List.partition_tf projection.Agent_protocol.Snapshot.grants ~f:is_manifest
  in
  { current with
    manifest_sha256 = first_active_manifest manifest
  ; live_manifest_sha256 = first_active_manifest manifest
  ; manifest_grants = List.map manifest ~f:manifest_grant
  ; grants = List.map approval ~f:approval_grant
  ; administrative_policy = "daemon profile: " ^ permission_profile projection.session
  ; signature_status = "daemon-owned redacted projection"
  ; audit_status = "remote audit available"
  ; interrupted_requests = []
  }
;;

let fields = function
  | `Object fields -> fields
  | `Null | `False | `True | `String _ | `Number _ | `Array _ -> []
;;

let string_field fields name =
  List.Assoc.find fields name ~equal:String.equal
  |> Option.bind ~f:(function
    | `String value -> Some value
    | `Null | `False | `True | `Number _ | `Object _ | `Array _ -> None)
;;

let int_field fields name =
  List.Assoc.find fields name ~equal:String.equal
  |> Option.bind ~f:(function
    | `Number value -> Int.of_string_opt value
    | `Null | `False | `True | `String _ | `Object _ | `Array _ -> None)
  |> Option.value ~default:0
;;

let strings_field fields name =
  List.Assoc.find fields name ~equal:String.equal
  |> Option.bind ~f:(function
    | `Array values ->
      Some
        (List.filter_map values ~f:(function
           | `String value -> Some value
           | `Null | `False | `True | `Number _ | `Object _ | `Array _ -> None))
    | `Null | `False | `True | `String _ | `Number _ | `Object _ -> None)
  |> Option.value ~default:[]
;;

let level = function
  | Agent_protocol.Audit.Info -> "info"
  | Warning -> "warning"
  | Error -> "error"
;;

let audit_request (audit : Agent_protocol.Audit.t) =
  let payload = fields audit.payload in
  let value name ~default = Option.value (string_field payload name) ~default in
  let sequence = Int64.to_string audit.sequence in
  Page.
    { request_id = value "request_id" ~default:(audit.name ^ ":" ^ sequence)
    ; runtime_id = value "runtime_id" ~default:"daemon"
    ; request_kind = value "request_kind" ~default:audit.name
    ; command_sha256 = value "identity_digest" ~default:"redacted"
    ; effects = strings_field payload "effects"
    ; policy_action = string_field payload "policy_action"
    ; approval_answer = string_field payload "approval_answer"
    ; backend = string_field payload "backend"
    ; stdout_bytes = int_field payload "stdout_bytes"
    ; stderr_bytes = int_field payload "stderr_bytes"
    ; result = level audit.level
    ; events =
        [ { sequence = audit.sequence
          ; timestamp =
              Agent_protocol.Timestamp.to_time_ns audit.timestamp
              |> Time_ns.to_span_since_epoch
              |> Time_ns.Span.to_sec
          ; name = audit.name
          }
        ]
    }
;;

let maximum_sequence items =
  List.max_elt items ~compare:(fun left right ->
    Int64.compare left.Agent_protocol.Audit.sequence right.sequence)
  |> Option.map ~f:(fun audit -> audit.Agent_protocol.Audit.sequence)
;;

let audit_page ~session_id (page : Agent_protocol.Audit.t Agent_protocol.Page.t) =
  let redacted =
    List.exists page.Agent_protocol.Page.items ~f:(fun audit -> audit.redacted)
  in
  Page.
    { path = "daemon://audit/" ^ Agent_protocol.Id.Session.to_string session_id
    ; integrity = (if redacted then "verified; redacted" else "verified")
    ; total_requests = List.length page.items
    ; requests =
        List.sort page.items ~compare:(fun left right ->
          Int64.compare right.sequence left.sequence)
        |> List.map ~f:audit_request
    ; last_sequence = maximum_sequence page.items
    }
;;
