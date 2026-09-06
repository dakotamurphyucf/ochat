open Core

type code =
  | Invalid_request
  | Method_not_found
  | Unauthenticated
  | Permission_denied
  | Session_not_found
  | Prompt_not_found
  | Workspace_not_found
  | Invalid_state
  | Already_resolved
  | Resource_limit
  | Workspace_unavailable
  | Prompt_unavailable
  | Manifest_unauthorized
  | Approval_required
  | Idempotency_conflict
  | Snapshot_required
  | Operation_not_found
  | Persistence_error
  | Interrupted
  | Conflict
  | Internal_error
  | Incompatible_protocol
  | Cursor_expired
  | Blob_unavailable
  | Lease_stale
  | Configuration_invalid
  | Store_locked
  | Store_schema_too_new
  | Migration_required
  | Journal_corrupt
  | Server_shutting_down
  | Command_queue_full
[@@deriving compare, equal, sexp]

type t =
  { code : code
  ; message : string
  ; retryable : bool
  ; data : Jsonaf.t
  }
[@@deriving sexp]

let create code ~message ~retryable ?(data = `Object []) () =
  { code; message; retryable; data }
;;

let invalid_request ?data message =
  create Invalid_request ~message ~retryable:false ?data ()
;;

let code_to_string = function
  | Invalid_request -> "invalid_request"
  | Method_not_found -> "method_not_found"
  | Unauthenticated -> "unauthenticated"
  | Permission_denied -> "permission_denied"
  | Session_not_found -> "session_not_found"
  | Prompt_not_found -> "prompt_not_found"
  | Workspace_not_found -> "workspace_not_found"
  | Invalid_state -> "invalid_state"
  | Already_resolved -> "already_resolved"
  | Resource_limit -> "resource_limit"
  | Workspace_unavailable -> "workspace_unavailable"
  | Prompt_unavailable -> "prompt_unavailable"
  | Manifest_unauthorized -> "manifest_unauthorized"
  | Approval_required -> "approval_required"
  | Idempotency_conflict -> "idempotency_conflict"
  | Snapshot_required -> "snapshot_required"
  | Operation_not_found -> "operation_not_found"
  | Persistence_error -> "persistence_error"
  | Interrupted -> "interrupted"
  | Conflict -> "conflict"
  | Internal_error -> "internal_error"
  | Incompatible_protocol -> "incompatible_protocol"
  | Cursor_expired -> "cursor_expired"
  | Blob_unavailable -> "blob_unavailable"
  | Lease_stale -> "lease_stale"
  | Configuration_invalid -> "configuration_invalid"
  | Store_locked -> "store_locked"
  | Store_schema_too_new -> "store_schema_too_new"
  | Migration_required -> "migration_required"
  | Journal_corrupt -> "journal_corrupt"
  | Server_shutting_down -> "server_shutting_down"
  | Command_queue_full -> "command_queue_full"
;;

let code_of_string value =
  match value with
  | "invalid_request" -> Ok Invalid_request
  | "method_not_found" -> Ok Method_not_found
  | "unauthenticated" -> Ok Unauthenticated
  | "permission_denied" -> Ok Permission_denied
  | "session_not_found" -> Ok Session_not_found
  | "prompt_not_found" -> Ok Prompt_not_found
  | "workspace_not_found" -> Ok Workspace_not_found
  | "invalid_state" -> Ok Invalid_state
  | "already_resolved" -> Ok Already_resolved
  | "resource_limit" -> Ok Resource_limit
  | "workspace_unavailable" -> Ok Workspace_unavailable
  | "prompt_unavailable" -> Ok Prompt_unavailable
  | "manifest_unauthorized" -> Ok Manifest_unauthorized
  | "approval_required" -> Ok Approval_required
  | "idempotency_conflict" -> Ok Idempotency_conflict
  | "snapshot_required" -> Ok Snapshot_required
  | "operation_not_found" -> Ok Operation_not_found
  | "persistence_error" -> Ok Persistence_error
  | "interrupted" -> Ok Interrupted
  | "conflict" -> Ok Conflict
  | "internal_error" -> Ok Internal_error
  | "incompatible_protocol" -> Ok Incompatible_protocol
  | "cursor_expired" -> Ok Cursor_expired
  | "blob_unavailable" -> Ok Blob_unavailable
  | "lease_stale" -> Ok Lease_stale
  | "configuration_invalid" -> Ok Configuration_invalid
  | "store_locked" -> Ok Store_locked
  | "store_schema_too_new" -> Ok Store_schema_too_new
  | "migration_required" -> Ok Migration_required
  | "journal_corrupt" -> Ok Journal_corrupt
  | "server_shutting_down" -> Ok Server_shutting_down
  | "command_queue_full" -> Ok Command_queue_full
  | _ -> Error (invalid_request ("unknown error code: " ^ value))
;;

let to_json t =
  `Object
    [ "code", `String (code_to_string t.code)
    ; "message", `String t.message
    ; ("retryable", if t.retryable then `True else `False)
    ; "data", t.data
    ]
;;

let fields_of_json = function
  | `Object fields ->
    let names = List.map fields ~f:fst in
    (match List.find_a_dup names ~compare:String.compare with
     | None -> Ok fields
     | Some name -> Error (invalid_request ("duplicate error field: " ^ name)))
  | _ -> Error (invalid_request "protocol error must be an object")
;;

let required fields name =
  match List.Assoc.find fields name ~equal:String.equal with
  | Some value -> Ok value
  | None -> Error (invalid_request ("missing error field: " ^ name))
;;

let decode_string fields name =
  let open Result.Let_syntax in
  let%bind value = required fields name in
  match value with
  | `String value -> Ok value
  | _ -> Error (invalid_request ("error field must be a string: " ^ name))
;;

let decode_bool fields name =
  let open Result.Let_syntax in
  let%bind value = required fields name in
  match value with
  | `True -> Ok true
  | `False -> Ok false
  | _ -> Error (invalid_request ("error field must be a boolean: " ^ name))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = fields_of_json json in
  let%bind code_text = decode_string fields "code" in
  let%bind code = code_of_string code_text in
  let%bind message = decode_string fields "message" in
  let%bind retryable = decode_bool fields "retryable" in
  let data =
    List.Assoc.find fields "data" ~equal:String.equal
    |> Option.value ~default:(`Object [])
  in
  Ok { code; message; retryable; data }
;;
