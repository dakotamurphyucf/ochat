open! Core

let string value = `String value
let strings values = `Array (List.map values ~f:string)
let option f = Option.value_map ~default:`Null ~f

let status = function
  | `Exited code -> `Object [ "exited", `Number (Int.to_string code) ]
  | `Signaled signal -> `Object [ "signaled", `Number (Int.to_string signal) ]
;;

let executable (value : Chatml_context_value.executable) =
  `Object
    [ "requested", string value.requested
    ; "path", string value.path
    ; "canonical_path", string value.canonical_path
    ; "sha256", string value.sha256
    ; "trusted", if value.trusted then `True else `False
    ]
;;

let capabilities (value : Chatml_context_value.capabilities) =
  `Object
    [ "read_roots", strings value.read_roots
    ; "write_roots", strings value.write_roots
    ; "network", if value.network then `True else `False
    ; "child_processes", if value.child_processes then `True else `False
    ; "arbitrary_code", if value.arbitrary_code then `True else `False
    ; "privilege_change", if value.privilege_change then `True else `False
    ; "sandbox", string value.sandbox
    ]
;;

let policy decision =
  let value = Chatml_policy_value.of_decision decision in
  let matches =
    List.map value.matches ~f:(fun match_ ->
      `Object
        [ "rule_id", string match_.rule_id
        ; "action", string (Shell_access.Policy.string_of_action match_.action)
        ; "reason", option string match_.reason
        ])
  in
  `Object
    [ "action", string (Shell_access.Policy.string_of_action value.action)
    ; "matches", `Array matches
    ; "reason", string value.reason
    ]
;;

let environment_keys context =
  Array.to_list context.Shell_access.Context.environment
  |> List.map ~f:(fun entry -> String.lsplit2 entry ~on:'=' |> Option.value_map ~default:entry ~f:fst)
  |> List.dedup_and_sort ~compare:String.compare
;;

let context ?policy:decision ~event context =
  let value = Chatml_context_value.of_context context in
  `Object
    [ "event", string event
    ; "request_id", string value.request_id
    ; "runtime_id", string value.runtime_id
    ; "manifest_sha256", string value.manifest_sha256
    ; "command",
      `Object
        [ "program", string (List.hd_exn value.argv)
        ; "arguments", strings (List.tl_exn value.argv)
        ]
    ; "executable", executable value.executable
    ; "cwd", string value.cwd
    ; "environment_keys", strings (environment_keys context)
    ; "origin", string value.origin
    ; "request_kind", string value.request_kind
    ; "stdin_kind", string value.stdin_kind
    ; "stdin_sha256", option string value.stdin_sha256
    ; "stdin_bytes", `Number (Int.to_string value.stdin_bytes)
    ; "effects", strings value.effects
    ; "capabilities", capabilities value.capabilities
    ; "session_id", option string value.session_id
    ; "policy",
      (match decision with
       | Some decision -> policy decision
       | None ->
         option
           Fn.id
           (Option.map context.policy_action ~f:(fun action ->
              `Object
                [ "action", string action
                ; "matches", strings context.policy_matches
                ; "reason", string ""
                ])))
    ]
;;

let approval_request request =
  let base = context ~policy:request.Shell_access.Approval.policy ~event:"approval_request" request.context in
  match base with
  | `Object fields ->
    `Object
      (("display_command", string request.display_command)
       :: ("rationale", option string request.rationale)
       :: fields)
  | _ -> assert false
;;

let command_result result =
  let value = Chatml_result_value.of_result result in
  `Object
    [ "event", string "after_command"
    ; "command",
      `Object
        [ "program", string (List.hd_exn value.argv)
        ; "arguments", strings (List.tl_exn value.argv)
        ]
    ; "status", status result.status
    ; "stdout", string value.stdout
    ; "stderr", string value.stderr
    ; "stdout_truncated", if value.stdout_truncated then `True else `False
    ; "stderr_truncated", if value.stderr_truncated then `True else `False
    ; "intercepted_by", option string value.intercepted_by
    ; "untrusted_output", if value.untrusted_output then `True else `False
    ]
;;

let audit_event ~secret_filter envelope =
  let value = Chatml_audit_value.of_envelope ~secret_filter envelope in
  let fields = Map.to_alist value.fields |> List.map ~f:(fun (name, value) -> name, string value) in
  `Object
    [ "event", string "audit_event"
    ; "sequence", `Number (Int64.to_string value.sequence)
    ; "timestamp", `Number (Float.to_string value.timestamp)
    ; "session_id", option string value.session_id
    ; "runtime_id", string value.runtime_id
    ; "manifest_sha256", string value.manifest_sha256
    ; "request_id", string value.request_id
    ; "plan_id", option string value.plan_id
    ; "event_type", string value.event
    ; "fields", `Object fields
    ]
;;
