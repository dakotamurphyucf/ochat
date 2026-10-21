open! Core
module P = Agent_protocol
module Identity = Developer_command_original.Identity
module Reference = Developer_repl_epoch.Task_reference

type t =
  { reference : Reference.t
  ; requested_execution : Chatml_execution.limits option
  ; requested_max_output_bytes : int option
  ; fingerprint : Chatml_source.Digest.t
  }

let create ~reference ~requested_execution ~requested_max_output_bytes ~checkpoint =
  if
    (not (Option.for_all requested_execution ~f:Chatml_execution.valid_limits))
    || not (Option.for_all requested_max_output_bytes ~f:(fun bytes -> bytes >= 256))
  then Error (P.Error.invalid_request "invalid developer REPL Task limits")
  else (
    let option f = function
      | None -> `Null
      | Some value -> f value
    in
    let fingerprint =
      `Object
        [ "version", `Number "1"
        ; "action", `String "developer_repl.task_run"
        ; "reference", `String (Reference.to_string reference)
        ; "requested_execution", option Identity.execution_json requested_execution
        ; ( "requested_max_output_bytes"
          , option (fun bytes -> `Number (Int.to_string bytes)) requested_max_output_bytes
          )
        ]
      |> Identity.digest_json ~checkpoint
    in
    Ok { reference; requested_execution; requested_max_output_bytes; fingerprint })
;;

let reference t = t.reference
let requested_execution t = t.requested_execution
let requested_max_output_bytes t = t.requested_max_output_bytes
let fingerprint t = t.fingerprint
