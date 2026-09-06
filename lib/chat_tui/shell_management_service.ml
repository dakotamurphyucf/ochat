open! Core
module Audit = Shell_runtime.Audit_replay
module State = Shell_security_page_state

let error_text errors =
  errors
  |> List.map ~f:(fun (error : Audit.error) -> error.message)
  |> String.concat ~sep:"; "
;;

let event (event : Audit.event) =
  State.
    { sequence = event.sequence
    ; timestamp = event.timestamp
    ; name = event.name
    }
;;

let request_result (request : Audit.request) =
  Option.value request.exit_kind ~default:(if request.completed then "completed" else "interrupted")
;;

let request (request : Audit.request) =
  State.
    { request_id = request.request_id
    ; runtime_id = request.runtime_id
    ; request_kind = Option.value request.request_kind ~default:"unknown"
    ; command_sha256 = Option.value request.command_sha256 ~default:"unavailable"
    ; effects = request.effects
    ; policy_action = request.policy_action
    ; approval_answer = request.approval_answer
    ; backend = request.backend
    ; stdout_bytes = request.stdout_bytes
    ; stderr_bytes = request.stderr_bytes
    ; result = request_result request
    ; events = List.map request.events ~f:event
    }
;;

let last_sequence request =
  request.State.events
  |> List.last
  |> Option.map ~f:(fun event -> event.State.sequence)
  |> Option.value ~default:Int64.min_value
;;

let page ~path events =
  let requests = Audit.requests events |> List.map ~f:request in
  let total_requests = List.length requests in
  let requests =
    List.sort requests ~compare:(fun left right ->
      Int64.compare (last_sequence right) (last_sequence left))
    |> Fn.flip List.take 200
  in
  State.
    { path
    ; integrity = "verified"
    ; total_requests
    ; requests
    ; last_sequence = List.last events |> Option.map ~f:(fun event -> event.Audit.sequence)
    }
;;

let empty_page path =
  State.
    { path
    ; integrity = "empty"
    ; total_requests = 0
    ; requests = []
    ; last_sequence = None
    }
;;

let load_audit ~env ~session_id =
  let path =
    Filename.concat (Session_store.rel_path session_id) ".chatmd/shell-audit.jsonl"
  in
  let file = Eio.Path.(Eio.Stdenv.fs env / path) in
  if not (Eio.Path.is_file file)
  then Ok (empty_page path)
  else
    match Audit.load_rotated ~fs:(Eio.Stdenv.fs env) ~path with
    | Error errors -> Error (error_text errors)
    | Ok events ->
      (match Audit.validate events with
       | Error errors -> Error (error_text errors)
       | Ok () -> Ok (page ~path events))
;;
