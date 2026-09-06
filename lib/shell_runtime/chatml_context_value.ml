open! Core
module C = Chatml_codec
module L = Chatml.Chatml_lang

type executable =
  { requested : string
  ; path : string
  ; canonical_path : string
  ; trusted : bool
  ; sha256 : string
  }

type capabilities =
  { read_roots : string list
  ; write_roots : string list
  ; network : bool
  ; child_processes : bool
  ; arbitrary_code : bool
  ; privilege_change : bool
  ; sandbox : string
  }

type t =
  { phase : string
  ; request_id : string
  ; runtime_id : string
  ; manifest_sha256 : string
  ; argv : string list
  ; executable : executable
  ; cwd : string
  ; origin : string
  ; request_kind : string
  ; stdin_kind : string
  ; stdin_sha256 : string option
  ; stdin_bytes : int
  ; script_sha256 : string option
  ; script_preview : string option
  ; effects : string list
  ; capabilities : capabilities
  ; session_id : string option
  ; policy : Chatml_policy_value.t option
  }

let origin = function
  | Shell_access.Context.Tool -> "tool"
  | Moderator -> "moderator"
  | Host name -> "host:" ^ name
;;

let request_kind = function
  | Shell_access.Context.Structured -> "structured"
  | Script_file -> "script_file"
  | Raw_shell -> "raw_shell"
;;

let stdin_kind = function
  | Shell_access.Context.Empty -> "empty"
  | Pipeline -> "pipeline"
  | Supplied -> "supplied"
;;

let sandbox = function
  | Shell_access.Capabilities.Required -> "required"
  | Preferred -> "preferred"
  | Direct_unsafe -> "direct_unsafe"
;;

let executable (value : Shell_access.Executable.t) =
  { requested = value.requested
  ; path = value.path
  ; canonical_path = value.canonical_path
  ; trusted = value.trusted
  ; sha256 = value.fingerprint.sha256
  }
;;

let capabilities (value : Shell_access.Capabilities.t) =
  { read_roots = value.read_roots
  ; write_roots = value.write_roots
  ; network = value.network
  ; child_processes = value.allow_child_processes
  ; arbitrary_code = value.allow_arbitrary_code
  ; privilege_change = value.allow_privilege_change
  ; sandbox = sandbox value.sandbox
  }
;;

let of_context (value : Shell_access.Context.t) =
  { phase = "shell"
  ; request_id = value.request_id
  ; runtime_id = value.runtime_id
  ; manifest_sha256 = value.manifest_sha256
  ; argv = Shell_access.Command.to_argv value.command
  ; executable = executable value.executable
  ; cwd = value.cwd
  ; origin = origin value.origin
  ; request_kind = request_kind value.request_kind
  ; stdin_kind = stdin_kind value.stdin_kind
  ; stdin_sha256 = value.stdin_sha256
  ; stdin_bytes = value.stdin_bytes
  ; script_sha256 = value.script_sha256
  ; script_preview = value.script_preview
  ; effects = Shell_access.Effect.to_strings value.effects
  ; capabilities = capabilities value.capabilities
  ; session_id = value.session_id
  ; policy = None
  }
;;

let with_phase value phase = { value with phase }

let with_policy value decision =
  { value with policy = Some (Chatml_policy_value.of_decision decision) }
;;

let encode_executable value =
  C.encode_record
    [ "requested", L.VString value.requested
    ; "path", VString value.path
    ; "canonical_path", VString value.canonical_path
    ; "trusted", VBool value.trusted
    ; "sha256", VString value.sha256
    ]
;;

let encode_capabilities value =
  C.encode_record
    [ "read_roots", C.encode_strings value.read_roots
    ; "write_roots", C.encode_strings value.write_roots
    ; "network", L.VBool value.network
    ; "child_processes", VBool value.child_processes
    ; "arbitrary_code", VBool value.arbitrary_code
    ; "privilege_change", VBool value.privilege_change
    ; "sandbox", VString value.sandbox
    ]
;;

let encode value =
  C.encode_record
    [ "version", L.VString "shell-context-v1"
    ; "phase", VString value.phase
    ; "request_id", VString value.request_id
    ; "runtime_id", VString value.runtime_id
    ; "manifest_sha256", VString value.manifest_sha256
    ; "argv", C.encode_strings value.argv
    ; "executable", encode_executable value.executable
    ; "cwd", VString value.cwd
    ; "origin", VString value.origin
    ; "request_kind", VString value.request_kind
    ; "stdin_kind", VString value.stdin_kind
    ; "stdin_sha256", C.encode_option (fun value -> L.VString value) value.stdin_sha256
    ; "stdin_bytes", VInt value.stdin_bytes
    ; "script_sha256", C.encode_option (fun value -> L.VString value) value.script_sha256
    ; "script_preview", C.encode_option (fun value -> L.VString value) value.script_preview
    ; "effects", C.encode_strings value.effects
    ; "capabilities", encode_capabilities value.capabilities
    ; "session_id", C.encode_option (fun value -> L.VString value) value.session_id
    ; "policy", C.encode_option Chatml_policy_value.encode value.policy
    ]
;;

let get path fields name decode =
  Result.bind (C.field ~path fields name) ~f:(decode ~path:(path @ [ name ]))
;;

let decode_executable value =
  let path = [ "context"; "executable" ] in
  let open Result.Let_syntax in
  let names = [ "requested"; "path"; "canonical_path"; "trusted"; "sha256" ] in
  let%bind fields = C.record ~path ~allowed:names ~required:names value in
  let%bind requested = get path fields "requested" C.string in
  let%bind path_value = get path fields "path" C.string in
  let%bind canonical_path = get path fields "canonical_path" C.string in
  let%bind trusted = get path fields "trusted" C.bool in
  let%map sha256 = get path fields "sha256" C.string in
  { requested; path = path_value; canonical_path; trusted; sha256 }
;;

let decode_capabilities value =
  let path = [ "context"; "capabilities" ] in
  let open Result.Let_syntax in
  let names = [ "read_roots"; "write_roots"; "network"; "child_processes"; "arbitrary_code"; "privilege_change"; "sandbox" ] in
  let%bind fields = C.record ~path ~allowed:names ~required:names value in
  let%bind read_roots = get path fields "read_roots" C.strings in
  let%bind write_roots = get path fields "write_roots" C.strings in
  let%bind network = get path fields "network" C.bool in
  let%bind child_processes = get path fields "child_processes" C.bool in
  let%bind arbitrary_code = get path fields "arbitrary_code" C.bool in
  let%bind privilege_change = get path fields "privilege_change" C.bool in
  let%map sandbox = get path fields "sandbox" C.string in
  { read_roots; write_roots; network; child_processes; arbitrary_code; privilege_change; sandbox }
;;

let field_names =
  [ "version"; "phase"; "request_id"; "runtime_id"; "manifest_sha256"; "argv"; "executable"
  ; "cwd"; "origin"; "request_kind"; "stdin_kind"; "stdin_sha256"; "stdin_bytes"
  ; "script_sha256"; "script_preview"; "effects"; "capabilities"; "session_id"; "policy"
  ]
;;

let decode_fields fields =
  let path = [ "context" ] in
  let open Result.Let_syntax in
  let%bind phase = get path fields "phase" C.string in
  let%bind request_id = get path fields "request_id" C.string in
  let%bind runtime_id = get path fields "runtime_id" C.string in
  let%bind manifest_sha256 = get path fields "manifest_sha256" C.string in
  let%bind argv = get path fields "argv" C.strings in
  let%bind executable = get path fields "executable" (fun ~path:_ -> decode_executable) in
  let%bind cwd = get path fields "cwd" C.string in
  let%bind origin = get path fields "origin" C.string in
  let%bind request_kind = get path fields "request_kind" C.string in
  let%bind stdin_kind = get path fields "stdin_kind" C.string in
  let%bind stdin_sha256 = get path fields "stdin_sha256" (fun ~path -> C.option ~path C.string) in
  let%bind stdin_bytes = get path fields "stdin_bytes" C.int in
  let%bind script_sha256 = get path fields "script_sha256" (fun ~path -> C.option ~path C.string) in
  let%bind script_preview = get path fields "script_preview" (fun ~path -> C.option ~path C.string) in
  let%bind effects = get path fields "effects" C.strings in
  let%bind capabilities = get path fields "capabilities" (fun ~path:_ -> decode_capabilities) in
  let%bind session_id = get path fields "session_id" (fun ~path -> C.option ~path C.string) in
  let%map policy = get path fields "policy" (fun ~path -> C.option ~path (fun ~path:_ -> Chatml_policy_value.decode)) in
  { phase; request_id; runtime_id; manifest_sha256; argv; executable; cwd; origin; request_kind
  ; stdin_kind; stdin_sha256; stdin_bytes; script_sha256; script_preview; effects
  ; capabilities; session_id; policy
  }
;;

let decode value =
  let path = [ "context" ] in
  let open Result.Let_syntax in
  let%bind fields = C.record ~path ~allowed:field_names ~required:field_names value in
  let%bind version = get path fields "version" C.string in
  if not (String.equal version "shell-context-v1")
  then Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "version" ]) ~code:"shell.chatml_codec" "unsupported version")
  else decode_fields fields
;;
