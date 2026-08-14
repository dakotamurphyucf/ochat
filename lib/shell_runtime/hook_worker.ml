open! Core

type t =
  { env : Eio_unix.Stdenv.base
  ; hook_id : string
  ; kind : Hook_protocol.kind
  ; executable : Shell_access.Executable.t
  ; executor_config : Shell_access.Executor.config
  ; timeout_seconds : float option
  ; max_input_bytes : int
  ; max_output_bytes : int
  ; redact : string -> string
  }

type error =
  { code : string
  ; message : string
  ; stderr : string option
  }
[@@deriving sexp, compare, equal]

let next_id = Atomic.make 0

let fresh_request_id () =
  sprintf "shell-hook-%08d" (Atomic.fetch_and_add next_id 1)
;;

let create
      ~env
      ~hook_id
      ~kind
      ~executable
      ~executor_config
      ?timeout_seconds
      ~max_input_bytes
      ~max_output_bytes
      ~redact
      ()
  =
  { env
  ; hook_id
  ; kind
  ; executable
  ; executor_config
  ; timeout_seconds
  ; max_input_bytes
  ; max_output_bytes
  ; redact
  }
;;

let error ?stderr code message = Error { code; message; stderr }

let invocation t input =
  Shell_access.Executor.
    { request =
        Shell_access.Request.command
          (Shell_access.Command.create t.executable.canonical_path [])
    ; input = Shell_access.Input.Text input
    ; rationale = Some ("shell hook " ^ t.hook_id)
    ; origin = Shell_access.Context.Host ("hook:" ^ t.hook_id)
    }
;;

let run t input =
  let perform () = Shell_access.Executor.run t.executor_config (invocation t input) in
  match t.timeout_seconds with
  | None -> perform ()
  | Some seconds ->
    (try
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock t.env) seconds perform
     with
     | Eio.Time.Timeout -> Error (Shell_access.Executor.Timed_out seconds))
;;

let validate_result t request_id result =
  let overflow =
    List.exists result.Shell_access.Executor.commands ~f:(fun command ->
      command.stdout_truncated || command.stderr_truncated)
  in
  if overflow
  then error "shell.hook_output_limit" "hook output exceeded its configured limit"
  else if not (Poly.equal result.status (`Exited 0))
  then
    error
      ~stderr:(t.redact result.stderr)
      "shell.hook_nonzero_exit"
      "hook process did not exit successfully"
  else if String.length result.stdout > t.max_output_bytes
  then error "shell.hook_output_limit" "hook stdout exceeded its configured limit"
  else
    match Hook_protocol.decode_response ~kind:t.kind result.stdout with
    | Error message ->
      error ~stderr:(t.redact result.stderr) "shell.hook_protocol" message
    | Ok response when not (String.equal response.request_id request_id) ->
      error "shell.hook_request_id" "hook response request ID does not match"
    | Ok response -> Ok response.action
;;

let invoke (t : t) payload =
  let request_id = fresh_request_id () in
  let request =
    Hook_protocol.
      { version = V1; request_id; hook_id = t.hook_id; kind = t.kind; payload }
  in
  let input = Hook_protocol.encode_request request in
  if String.length input > t.max_input_bytes
  then error "shell.hook_input_limit" "hook input exceeded its configured limit"
  else
    match run t input with
    | Error failure ->
      error "shell.hook_execution" (Shell_access.Executor.error_to_string failure)
    | Ok result -> validate_result t request_id result
;;

let executable t = t.executable
