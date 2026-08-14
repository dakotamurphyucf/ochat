open! Core
module C = Chatml_codec
module L = Chatml.Chatml_lang

type t =
  { phase : string
  ; sequence : int64
  ; timestamp : float
  ; session_id : string option
  ; runtime_id : string
  ; manifest_sha256 : string
  ; request_id : string
  ; plan_id : string option
  ; event : string
  ; fields : string String.Map.t
  }

type response =
  | Keep
  | Drop_field of string
  | Replace_fields of string String.Map.t

let event_name = function
  | Shell_access.Audit.Resolved _ -> "resolved"
  | Policy_decided _ -> "policy_decided"
  | Approval_requested _ -> "approval_requested"
  | Approval_answered _ -> "approval_answered"
  | Reviewer_completed _ -> "reviewer_completed"
  | Intercepted _ -> "intercepted"
  | Plan_created _ -> "plan_created"
  | Started _ -> "started"
  | Output _ -> "output"
  | Finished _ -> "finished"
  | Terminated _ -> "terminated"
  | Rejected _ -> "rejected"
;;

let status_fields = function
  | `Exited code -> [ "exit_kind", "exited"; "exit_code", Int.to_string code ]
  | `Signaled signal -> [ "exit_kind", "signaled"; "signal", Int.to_string signal ]
;;

let termination_fields = function
  | Shell_access.Audit.Timed_out seconds ->
    [ "exit_kind", "timed_out"; "timeout_seconds", Float.to_string seconds ]
  | Idle_timed_out seconds ->
    [ "exit_kind", "idle_timed_out"; "timeout_seconds", Float.to_string seconds ]
  | Output_limit_exceeded bytes ->
    [ "exit_kind", "output_limit_exceeded"; "limit_bytes", Int.to_string bytes ]
  | Cancelled -> [ "exit_kind", "cancelled" ]
;;

let event_fields = function
  | Shell_access.Audit.Policy_decided (_, decision) ->
    [ "policy_action", Shell_access.Policy.string_of_action decision.action
    ; "policy_reason", decision.reason
    ]
  | Approval_answered (_, answer) -> [ "approval_answer", answer ]
  | Reviewer_completed (_, metadata, action) ->
    [ "reviewer_id", metadata.reviewer_id
    ; "reviewer_kind", metadata.reviewer_kind
    ; "model", Option.value metadata.model ~default:""
    ; "response_action", action
    ]
  | Intercepted (name, _) -> [ "interceptor", name ]
  | Plan_created (backend, _, _) | Started (backend, _, _) -> [ "backend", backend ]
  | Output (_, _, channel, bytes) ->
    [ "channel", (match channel with `Stdout -> "stdout" | `Stderr -> "stderr")
    ; "bytes", Int.to_string bytes
    ]
  | Finished (_, _, status) -> status_fields status
  | Terminated (_, _, termination) -> termination_fields termination
  | Rejected (_, reason) -> [ "reason", reason ]
  | Resolved _ | Approval_requested _ -> []
;;

let of_envelope ~secret_filter envelope =
  let context = Shell_access.Audit.context envelope.Shell_access.Audit.event in
  let redact = Shell_access.Secret_filter.redact secret_filter in
  let fields =
    ("command", Shell_access.Secret_filter.redact_command secret_filter context.command)
    :: event_fields envelope.event
    |> List.map ~f:(fun (name, value) -> name, redact value)
    |> String.Map.of_alist_exn
  in
  { phase = "shell_audit"
  ; sequence = envelope.sequence
  ; timestamp = envelope.timestamp
  ; session_id = envelope.session_id
  ; runtime_id = envelope.runtime_id
  ; manifest_sha256 = envelope.manifest_sha256
  ; request_id = envelope.request_id
  ; plan_id = envelope.plan_id
  ; event = event_name envelope.event
  ; fields
  }
;;

let encode_fields fields =
  Map.to_alist fields
  |> List.map ~f:(fun (name, value) -> C.encode_record [ "name", L.VString name; "value", VString value ])
  |> Array.of_list
  |> fun values -> L.VArray values
;;

let encode value =
  C.encode_record
    [ "version", L.VString "shell-audit-v1"
    ; "phase", VString value.phase
    ; "sequence", VString (Int64.to_string value.sequence)
    ; "timestamp", VFloat value.timestamp
    ; "session_id", C.encode_option (fun value -> L.VString value) value.session_id
    ; "runtime_id", VString value.runtime_id
    ; "manifest_sha256", VString value.manifest_sha256
    ; "request_id", VString value.request_id
    ; "plan_id", C.encode_option (fun value -> L.VString value) value.plan_id
    ; "event", VString value.event
    ; "fields", encode_fields value.fields
    ]
;;

let decode_entry index value =
  let path = [ "audit"; "fields"; Int.to_string index ] in
  let open Result.Let_syntax in
  let%bind fields = C.record ~path ~allowed:[ "name"; "value" ] ~required:[ "name"; "value" ] value in
  let%bind name = C.field ~path fields "name" >>= C.string ~path:(path @ [ "name" ]) in
  let%map value = C.field ~path fields "value" >>= C.string ~path:(path @ [ "value" ]) in
  name, value
;;

let decode_fields = function
  | L.VArray values ->
    Array.to_list values
    |> List.mapi ~f:decode_entry
    |> Result.all
    |> Result.bind ~f:(fun values ->
      match String.Map.of_alist values with
      | `Ok values -> Ok values
      | `Duplicate_key name -> Error (Chatmd_shell_spec.Diagnostic.error ~path:[ "audit"; "fields"; name ] ~code:"shell.chatml_codec" "duplicate field"))
  | _ -> Error (Chatmd_shell_spec.Diagnostic.error ~path:[ "audit"; "fields" ] ~code:"shell.chatml_codec" "expected array")
;;

let names = [ "version"; "phase"; "sequence"; "timestamp"; "session_id"; "runtime_id"; "manifest_sha256"; "request_id"; "plan_id"; "event"; "fields" ]

let decode value =
  let path = [ "audit" ] in
  let open Result.Let_syntax in
  let%bind values = C.record ~path ~allowed:names ~required:names value in
  let get name decode = C.field ~path values name >>= decode ~path:(path @ [ name ]) in
  let%bind version = get "version" C.string in
  let%bind () =
    if String.equal version "shell-audit-v1"
    then Ok ()
    else Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "version" ]) ~code:"shell.chatml_codec" "unsupported version")
  in
  let%bind phase = get "phase" C.string in
  let%bind sequence = get "sequence" C.string >>= fun value -> Result.try_with (fun () -> Int64.of_string value) |> Result.map_error ~f:(fun _ -> Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "sequence" ]) ~code:"shell.chatml_codec" "invalid sequence") in
  let%bind timestamp = get "timestamp" (fun ~path -> function L.VFloat value -> Ok value | _ -> Error (Chatmd_shell_spec.Diagnostic.error ~path ~code:"shell.chatml_codec" "expected float")) in
  let%bind session_id = get "session_id" (fun ~path -> C.option ~path C.string) in
  let%bind runtime_id = get "runtime_id" C.string in
  let%bind manifest_sha256 = get "manifest_sha256" C.string in
  let%bind request_id = get "request_id" C.string in
  let%bind plan_id = get "plan_id" (fun ~path -> C.option ~path C.string) in
  let%bind event = get "event" C.string in
  let%map fields = C.field ~path values "fields" >>= decode_fields in
  { phase; sequence; timestamp; session_id; runtime_id; manifest_sha256; request_id; plan_id; event; fields }
;;

let encode_response = function
  | Keep -> C.encode_record [ "version", L.VString "shell-audit-response-v1"; "action", VString "keep" ]
  | Drop_field field -> C.encode_record [ "version", L.VString "shell-audit-response-v1"; "action", VString "drop_field"; "field", VString field ]
  | Replace_fields fields -> C.encode_record [ "version", L.VString "shell-audit-response-v1"; "action", VString "replace"; "fields", encode_fields fields ]
;;

let decode_response value =
  let path = [ "audit_response" ] in
  let allowed = [ "version"; "action"; "field"; "fields" ] in
  let open Result.Let_syntax in
  let%bind values = C.record ~path ~allowed ~required:[ "version"; "action" ] value in
  let%bind version = C.field ~path values "version" >>= C.string ~path:(path @ [ "version" ]) in
  let%bind action = C.field ~path values "action" >>= C.string ~path:(path @ [ "action" ]) in
  if not (String.equal version "shell-audit-response-v1")
  then Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "version" ]) ~code:"shell.chatml_codec" "unsupported version")
  else match action with
    | "keep" -> Ok Keep
    | "drop_field" -> C.field ~path values "field" >>= C.string ~path:(path @ [ "field" ]) |> Result.map ~f:(fun field -> Drop_field field)
    | "replace" -> C.field ~path values "fields" >>= decode_fields |> Result.map ~f:(fun fields -> Replace_fields fields)
    | _ -> Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "action" ]) ~code:"shell.chatml_codec" "unknown audit action")
;;
