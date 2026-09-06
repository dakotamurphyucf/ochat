open! Core
module S = Chatmd_shell_spec.Shell_spec

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

exception Lowering_error of error

let fail code message = raise_notrace (Lowering_error { code; message })

let result f =
  try Ok (f ()) with
  | Lowering_error error -> Error error
;;

let bool_setting name = function
  | S.Set value -> value
  | Inherit | Clear -> fail "shell.unresolved_setting" (name ^ " is unresolved")
;;

let sandbox_setting = function
  | S.Set S.Required -> Shell_access.Capabilities.Required
  | Set S.Preferred -> Preferred
  | Set S.Direct_unsafe -> Direct_unsafe
  | Inherit | Clear -> fail "shell.unresolved_setting" "capability sandbox is unresolved"
;;

let process_path host source path =
  match Host.resolve_path host ~source path with
  | Ok path -> Eio.Path.native_exn path
  | Error error -> fail error.code error.message
;;

let capability_root host source (root : S.capability_root) =
  match root with
  | S.Path path -> Some (process_path host source path)
  | Path_env { name; optional } ->
    (match Environment.find_process_value host name with
     | Some path -> Some (Eio.Path.(Eio.Stdenv.fs host.env / path) |> Eio.Path.native_exn)
     | None when optional -> None
     | None ->
       fail
         "shell.capability_path_missing"
         ("required capability path source is missing: " ^ name))
;;

let capabilities host ~source specification =
  result (fun () ->
    { Shell_access.Capabilities.read_roots =
        List.filter_map specification.S.read ~f:(capability_root host source)
    ; write_roots = List.filter_map specification.write ~f:(capability_root host source)
    ; network = bool_setting "capabilities.network" specification.network
    ; allow_child_processes =
        bool_setting "capabilities.child_processes" specification.child_processes
    ; allow_arbitrary_code =
        bool_setting "capabilities.arbitrary_code" specification.arbitrary_code
    ; allow_privilege_change =
        bool_setting "capabilities.privilege_change" specification.privilege_change
    ; sandbox = sandbox_setting specification.sandbox
    })
;;

let resolver host ~source specification =
  result (fun () ->
    let search_path =
      List.map specification.S.search_path ~f:(process_path host source)
    in
    let trusted_roots =
      List.map specification.trusted_root ~f:(process_path host source)
    in
    let executables =
      List.map specification.executables ~f:(fun executable ->
        ( executable.S.id
        , Shell_access.Resolver.
            { path = process_path host source executable.path
            ; sha256 = executable.sha256
            ; trusted = executable.trusted
            } ))
    in
    Shell_access.Resolver.create
      ?search_path:(Option.some_if (not (List.is_empty search_path)) search_path)
      ~trusted_roots
      ~executables
      ())
;;

let duration_option name = function
  | S.Set value -> Option.map value ~f:Chatmd_shell_spec.Duration.to_seconds
  | Clear -> None
  | Inherit -> fail "shell.unresolved_setting" (name ^ " is unresolved")
;;

let duration_required name setting =
  match duration_option name setting with
  | Some value -> value
  | None -> fail "shell.required_limit_disabled" (name ^ " cannot be disabled")
;;

let int64_to_int name value =
  match Int64.to_int value with
  | Some value -> value
  | None -> fail "shell.limit_out_of_range" (name ^ " exceeds the host integer range")
;;

let bytes_required name = function
  | S.Set value -> int64_to_int name (Chatmd_shell_spec.Duration.bytes_to_int64 value)
  | Inherit | Clear -> fail "shell.unresolved_setting" (name ^ " is unresolved")
;;

let bytes_optional name = function
  | S.Set value ->
    Some (int64_to_int name (Chatmd_shell_spec.Duration.bytes_to_int64 value))
  | Clear -> None
  | Inherit -> fail "shell.unresolved_setting" (name ^ " is unresolved")
;;

let int_optional name = function
  | S.Set value -> Some value
  | Clear -> None
  | Inherit -> fail "shell.unresolved_setting" (name ^ " is unresolved")
;;

let cpu_seconds setting =
  duration_option "limits.cpu_time" setting |> Option.map ~f:Float.iround_up_exn
;;

let limits specification =
  result (fun () ->
    let max_stdin = bytes_required "limits.max_stdin" specification.S.max_stdin in
    ( { Shell_access.Limits.wall_time_seconds =
          duration_required "limits.wall_time" specification.wall_time
      ; idle_time_seconds = duration_option "limits.idle_time" specification.idle_time
      ; max_stdin_bytes = max_stdin
      ; max_stdout_bytes = bytes_required "limits.stdout" specification.stdout
      ; max_stderr_bytes = bytes_required "limits.stderr" specification.stderr
      ; max_total_bytes = bytes_required "limits.total_output" specification.total_output
      ; cpu_seconds = cpu_seconds specification.cpu_time
      ; memory_bytes = bytes_optional "limits.memory" specification.memory
      ; file_size_bytes = bytes_optional "limits.file_size" specification.file_size
      ; open_files = int_optional "limits.open_files" specification.open_files
      }
    , max_stdin ))
;;

let policy_action = function
  | S.Allow -> Shell_access.Policy.Allow
  | S.Ask -> Ask
  | S.Deny -> Deny
;;

let path_is_under root path =
  let prefix =
    String.chop_suffix_if_exists root ~suffix:Filename.dir_sep ^ Filename.dir_sep
  in
  String.equal root path || String.is_prefix path ~prefix
;;

let effect_matches kind under = function
  | Shell_access.Effect.Read_path path ->
    S.equal_process_effect kind S.Read_path
    && Option.for_all under ~f:(fun root -> path_is_under root path)
  | Write_path path ->
    S.equal_process_effect kind S.Write_path
    && Option.for_all under ~f:(fun root -> path_is_under root path)
  | Network -> S.equal_process_effect kind S.Network
  | Child_processes -> S.equal_process_effect kind S.Child_processes
  | Arbitrary_code -> S.equal_process_effect kind S.Arbitrary_code
  | Privilege_change -> S.equal_process_effect kind S.Privilege_change
  | Unknown _ -> S.equal_process_effect kind S.Unknown
;;

let rec matcher host source ~chatml_matcher ~action = function
  | S.Any_command -> Shell_access.Matcher.any
  | Program value -> Shell_access.Matcher.program value
  | Basename value -> Shell_access.Matcher.basename value
  | Resolved_path path ->
    Shell_access.Matcher.resolved_path (process_path host source path)
  | Trusted_executable -> Shell_access.Matcher.trusted_executable
  | Program_regex value ->
    (match Shell_access.Matcher.program_regex value with
     | Ok matcher -> matcher
     | Error message -> fail "shell.invalid_program_regex" message)
  | Argv_prefix values -> Shell_access.Matcher.argv_prefix values
  | Argument value -> Shell_access.Matcher.argument value
  | Argument_contains value -> Shell_access.Matcher.argument_contains value
  | Effect { kind; under } ->
    let under = Option.map under ~f:(process_path host source) in
    Shell_access.Matcher.has_effect (effect_matches kind under)
  | No_unknown_effects -> Shell_access.Matcher.no_unknown_effects
  | Raw_shell_request -> Shell_access.Matcher.request_kind Raw_shell
  | Chatml_match value -> chatml_matcher ~action value
  | All values ->
    Shell_access.Matcher.all
      (List.map values ~f:(matcher host source ~chatml_matcher ~action))
  | Any values ->
    Shell_access.Matcher.any_of
      (List.map values ~f:(matcher host source ~chatml_matcher ~action))
  | Not value ->
    Shell_access.Matcher.negate (matcher host source ~chatml_matcher ~action value)
;;

let policy host ~source ~chatml_matcher specification =
  result (fun () ->
    let default =
      match specification.S.default with
      | Set action -> policy_action action
      | Inherit | Clear -> fail "shell.unresolved_setting" "policy default is unresolved"
    in
    let rules =
      List.map specification.rules ~f:(fun rule ->
        Shell_access.Policy.rule
          ~id:rule.id
          ~action:(policy_action rule.action)
          (matcher host source ~chatml_matcher ~action:rule.action rule.matcher))
    in
    Shell_access.Policy.create ~default rules)
;;

let platform_matches selected declared =
  S.equal_platform declared S.Any || S.equal_platform selected declared
;;

let backend host source ~external_backend = function
  | S.Seatbelt _ -> Shell_access.Backend.macos_seatbelt
  | Bubblewrap { executable; _ } ->
    let executable = Option.map executable ~f:(process_path host source) in
    Shell_access.Backend.linux_bubblewrap ?executable ()
  | Direct _ -> Shell_access.Backend.direct
  | (External _ as specification) -> external_backend specification
;;

let backend_platform = function
  | S.Seatbelt { when_; _ }
  | Bubblewrap { when_; _ }
  | Direct { when_; _ }
  | External { when_; _ } -> when_
;;

let backends host ~source ~platform ~external_backend (specification : S.backends) =
  result (fun () ->
    List.filter specification.S.values ~f:(fun (value : S.backend) ->
      platform_matches platform (backend_platform value))
    |> List.map ~f:(backend host source ~external_backend))
;;

let audit_failure = function
  | S.Continue -> Shell_access.Audit.Ignore_failure
  | S.Deny_start -> Deny_start
  | S.Terminate -> Terminate_runtime
;;
