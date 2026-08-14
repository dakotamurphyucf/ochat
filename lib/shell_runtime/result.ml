open! Core
open Jsonaf.Export
module I = Shell_access.Interceptor

type status =
  | Exited of int
  | Signaled of int
[@@deriving sexp, compare, equal, jsonaf]

type command =
  { argv : string list
  ; executable_sha256 : string
  ; status : status
  ; intercepted_by : string option
  }
[@@deriving sexp, compare, equal, jsonaf]

type t =
  { request_id : string
  ; status : status
  ; stdout : string
  ; stderr : string
  ; stdout_truncated : bool
  ; stderr_truncated : bool
  ; backend : string
  ; runtime_id : string
  ; manifest_sha256 : string
  ; commands : command list
  }
[@@deriving sexp, compare, equal, jsonaf]

type error =
  { code : string
  ; message : string
  ; permission_request : Shell_access.Approval.request option
  }

let status = function
  | `Exited code -> Exited code
  | `Signaled signal -> Signaled signal
;;

let command (result : I.command_result) =
  { argv = Shell_access.Command.to_argv result.command
  ; executable_sha256 =
      Option.value_map result.executable ~default:"" ~f:(fun executable ->
        executable.Shell_access.Executable.fingerprint.sha256)
  ; status = status result.status
  ; intercepted_by = result.intercepted_by
  }
;;

let of_executor ~runtime_id ~manifest_sha256 result =
  { request_id = result.Shell_access.Executor.request_id
  ; status = status result.Shell_access.Executor.status
  ; stdout = result.stdout
  ; stderr = result.stderr
  ; stdout_truncated =
      List.exists result.commands ~f:(fun (command : I.command_result) ->
        command.stdout_truncated)
  ; stderr_truncated =
      List.exists result.commands ~f:(fun (command : I.command_result) ->
        command.stderr_truncated)
  ; backend = result.backend
  ; runtime_id
  ; manifest_sha256
  ; commands = List.map result.commands ~f:command
  }
;;

let error code error =
  { code
  ; message = Shell_access.Executor.error_to_string error
  ; permission_request =
      (match error with
       | Shell_access.Executor.Permission_required request -> Some request
       | _ -> None)
  }
;;

let error_of_executor = function
  | Shell_access.Executor.Permission_required _ as value ->
    error "permission_required" value
  | Denied _ as value -> error "denied" value
  | Resolution_error _ as value -> error "resolution_error" value
  | Capability_violation _ as value -> error "capability_violation" value
  | Sandbox_unavailable _ as value -> error "sandbox_unavailable" value
  | Interceptor_rejected _ as value -> error "interceptor_rejected" value
  | Spawn_error _ as value -> error "spawn_error" value
  | Executable_changed _ as value -> error "executable_changed" value
  | Script_changed _ as value -> error "script_changed" value
  | Audit_unavailable _ as value -> error "audit_unavailable" value
  | Timed_out _ as value -> error "timed_out" value
  | Idle_timed_out _ as value -> error "idle_timed_out" value
  | Stdin_limit_exceeded _ as value -> error "stdin_limit_exceeded" value
  | Output_limit_exceeded _ as value -> error "output_limit_exceeded" value
;;
