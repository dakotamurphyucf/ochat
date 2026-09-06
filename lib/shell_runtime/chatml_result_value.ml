open! Core
module C = Chatml_codec
module L = Chatml.Chatml_lang

type t =
  { phase : string
  ; argv : string list
  ; status_kind : string
  ; status_code : int
  ; stdout : string
  ; stderr : string
  ; stdout_truncated : bool
  ; stderr_truncated : bool
  ; intercepted_by : string option
  ; untrusted_output : bool
  }
[@@deriving sexp, compare, equal]

let status = function
  | `Exited code -> "exited", code
  | `Signaled signal -> "signaled", signal
;;

let of_result (value : Shell_access.Interceptor.command_result) =
  let status_kind, status_code = status value.status in
  { phase = "shell_after"
  ; argv = Shell_access.Command.to_argv value.command
  ; status_kind
  ; status_code
  ; stdout = value.stdout
  ; stderr = value.stderr
  ; stdout_truncated = value.stdout_truncated
  ; stderr_truncated = value.stderr_truncated
  ; intercepted_by = value.intercepted_by
  ; untrusted_output = value.untrusted_output
  }
;;

let encode value =
  C.encode_record
    [ "version", L.VString "shell-result-v1"
    ; "phase", VString value.phase
    ; "argv", C.encode_strings value.argv
    ; "status_kind", VString value.status_kind
    ; "status_code", VInt value.status_code
    ; "stdout", VString value.stdout
    ; "stderr", VString value.stderr
    ; "stdout_truncated", VBool value.stdout_truncated
    ; "stderr_truncated", VBool value.stderr_truncated
    ; "intercepted_by", C.encode_option (fun value -> L.VString value) value.intercepted_by
    ; "untrusted_output", VBool value.untrusted_output
    ]
;;

let names =
  [ "version"; "phase"; "argv"; "status_kind"; "status_code"; "stdout"; "stderr"
  ; "stdout_truncated"; "stderr_truncated"; "intercepted_by"; "untrusted_output"
  ]
;;

let get path fields name decode =
  Result.bind (C.field ~path fields name) ~f:(decode ~path:(path @ [ name ]))
;;

let decode_fields fields =
  let path = [ "result" ] in
  let open Result.Let_syntax in
  let%bind phase = get path fields "phase" C.string in
  let%bind argv = get path fields "argv" C.strings in
  let%bind status_kind = get path fields "status_kind" C.string in
  let%bind status_code = get path fields "status_code" C.int in
  let%bind stdout = get path fields "stdout" C.string in
  let%bind stderr = get path fields "stderr" C.string in
  let%bind stdout_truncated = get path fields "stdout_truncated" C.bool in
  let%bind stderr_truncated = get path fields "stderr_truncated" C.bool in
  let%bind intercepted_by = get path fields "intercepted_by" (fun ~path -> C.option ~path C.string) in
  let%map untrusted_output = get path fields "untrusted_output" C.bool in
  { phase; argv; status_kind; status_code; stdout; stderr; stdout_truncated
  ; stderr_truncated; intercepted_by; untrusted_output
  }
;;

let decode value =
  let path = [ "result" ] in
  let open Result.Let_syntax in
  let%bind fields = C.record ~path ~allowed:names ~required:names value in
  let%bind version = get path fields "version" C.string in
  if String.equal version "shell-result-v1"
  then decode_fields fields
  else Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "version" ]) ~code:"shell.chatml_codec" "unsupported version")
;;
