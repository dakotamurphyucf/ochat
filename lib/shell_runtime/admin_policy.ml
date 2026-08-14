open! Core
open Jsonaf.Export
module S = Chatmd_shell_spec.Shell_spec

module Limit_ceiling = struct
  type t =
    { wall_time_seconds : float option
    ; idle_time_seconds : float option
    ; max_stdin_bytes : int64 option
    ; max_stdout_bytes : int64 option
    ; max_stderr_bytes : int64 option
    ; max_total_bytes : int64 option
    ; cpu_seconds : float option
    ; memory_bytes : int64 option
    ; file_size_bytes : int64 option
    ; open_files : int option
    }
  [@@deriving sexp, compare, equal, jsonaf]

  let none =
    { wall_time_seconds = None
    ; idle_time_seconds = None
    ; max_stdin_bytes = None
    ; max_stdout_bytes = None
    ; max_stderr_bytes = None
    ; max_total_bytes = None
    ; cpu_seconds = None
    ; memory_bytes = None
    ; file_size_bytes = None
    ; open_files = None
    }
  ;;
end

type t =
  { source : string
  ; allowed_read_roots : string list option
  ; allowed_write_roots : string list option
  ; allow_network : bool
  ; allow_child_processes : bool
  ; allow_arbitrary_code : bool
  ; allow_privilege_change : bool
  ; require_sandbox : bool
  ; allow_direct_backend : bool
  ; allow_declared_confinement : bool
  ; allow_external_backends : bool
  ; allow_hooks : bool
  ; allowed_reviewer_kinds : string list option
  ; allowed_approval_scopes : S.approval_scope list
  ; allow_raw_prefix_grants : bool
  ; allow_yolo : bool
  ; require_executable_hashes : bool
  ; require_trusted_executables : bool
  ; require_audit : bool
  ; require_durable_audit : bool
  ; require_trusted_source : bool
  ; require_signature : bool
  ; denied_programs : string list
  ; denied_argument_substrings : string list
  ; denied_effects : S.process_effect list
  ; limits : Limit_ceiling.t
  }
[@@deriving sexp, compare, equal, jsonaf]

type violation =
  { code : string
  ; runtime_id : string option
  ; requested : string
  ; ceiling : string
  ; policy_source : string
  ; remediation : string
  }
[@@deriving sexp, compare, equal]

let permissive =
  { source = "builtin:permissive"
  ; allowed_read_roots = None
  ; allowed_write_roots = None
  ; allow_network = true
  ; allow_child_processes = true
  ; allow_arbitrary_code = true
  ; allow_privilege_change = true
  ; require_sandbox = false
  ; allow_direct_backend = true
  ; allow_declared_confinement = true
  ; allow_external_backends = true
  ; allow_hooks = true
  ; allowed_reviewer_kinds = None
  ; allowed_approval_scopes = [ S.Once; Exact_session; Prefix_session; Durable_exact ]
  ; allow_raw_prefix_grants = true
  ; allow_yolo = true
  ; require_executable_hashes = false
  ; require_trusted_executables = false
  ; require_audit = false
  ; require_durable_audit = false
  ; require_trusted_source = false
  ; require_signature = false
  ; denied_programs = []
  ; denied_argument_substrings = []
  ; denied_effects = []
  ; limits = Limit_ceiling.none
  }
;;

let violation t ?runtime_id code ~requested ~ceiling ~remediation () =
  { code
  ; runtime_id
  ; requested
  ; ceiling
  ; policy_source = t.source
  ; remediation
  }
;;

let runtime_id runtime = S.Runtime_id.to_string runtime.S.id

let reject_bool t runtime field requested allowed =
  if requested && not allowed
  then
    [ violation
        t
        ~runtime_id:(runtime_id runtime)
        ("shell.admin_" ^ field ^ "_denied")
        ~requested:"true"
        ~ceiling:"false"
        ~remediation:("disable capabilities." ^ field ^ " in ChatMD")
        ()
    ]
  else []
;;

let setting_is_true = function
  | S.Set true -> true
  | Set false | Inherit | Clear -> false
;;

let path_under root path =
  let root = String.chop_suffix_if_exists root ~suffix:Filename.dir_sep in
  String.equal root path
  || String.is_prefix path ~prefix:(root ^ Filename.dir_sep)
;;

let root_allowed allowed (root : S.capability_root) =
  match root with
  | S.Path path ->
    let value = Chatmd_shell_spec.Path_expr.to_string path in
    List.exists allowed ~f:(fun ceiling -> path_under ceiling value)
  | Path_env _ -> false
;;

let root_violations t runtime kind allowed roots =
  match allowed with
  | None -> []
  | Some allowed ->
    List.filter_map roots ~f:(fun root ->
      Option.some_if
        (not (root_allowed allowed root))
        (violation
           t
           ~runtime_id:(runtime_id runtime)
           ("shell.admin_" ^ kind ^ "_root_denied")
           ~requested:(S.sexp_of_capability_root root |> Sexp.to_string_hum)
           ~ceiling:(String.concat ~sep:", " allowed)
           ~remediation:("choose a " ^ kind ^ " root under an administrative root")
           ()))
;;

let capability_violations t runtime =
  match runtime.S.capabilities with
  | None -> []
  | Some capabilities ->
    let sandbox =
      if t.require_sandbox && S.equal_setting S.equal_sandbox capabilities.sandbox (Set Direct_unsafe)
      then
        [ violation
            t
            ~runtime_id:(runtime_id runtime)
            "shell.admin_sandbox_required"
            ~requested:"direct_unsafe"
            ~ceiling:"verified sandbox required"
            ~remediation:"request sandbox=required and configure a verified backend"
            ()
        ]
      else []
    in
    List.concat
      [ sandbox
      ; reject_bool t runtime "network" (setting_is_true capabilities.network) t.allow_network
      ; reject_bool
          t
          runtime
          "child_processes"
          (setting_is_true capabilities.child_processes)
          t.allow_child_processes
      ; reject_bool
          t
          runtime
          "arbitrary_code"
          (setting_is_true capabilities.arbitrary_code)
          t.allow_arbitrary_code
      ; reject_bool
          t
          runtime
          "privilege_change"
          (setting_is_true capabilities.privilege_change)
          t.allow_privilege_change
      ; root_violations t runtime "read" t.allowed_read_roots capabilities.read
      ; root_violations t runtime "write" t.allowed_write_roots capabilities.write
      ]
;;

let backend_violations t runtime =
  match runtime.S.backends with
  | None -> []
  | Some backends ->
    List.concat_map backends.values ~f:(function
      | S.Direct _ when not t.allow_direct_backend ->
        [ violation t ~runtime_id:(runtime_id runtime) "shell.admin_direct_backend_denied"
            ~requested:"direct" ~ceiling:"direct backends disabled"
            ~remediation:"configure a verified sandbox backend" () ]
      | External { confinement = Declared_confinement; _ }
        when not t.allow_declared_confinement ->
        [ violation t ~runtime_id:(runtime_id runtime) "shell.admin_declared_confinement_denied"
            ~requested:"declared confinement" ~ceiling:"verified confinement required"
            ~remediation:"use a verified built-in or verified external backend" () ]
      | External _ when not t.allow_external_backends ->
        [ violation t ~runtime_id:(runtime_id runtime) "shell.admin_external_backend_denied"
            ~requested:"external backend" ~ceiling:"external backends disabled"
            ~remediation:"use an administratively permitted built-in backend" () ]
      | Seatbelt _ | Bubblewrap _ | Direct _ | External _ -> [])
;;

let executable_violations t runtime =
  match runtime.S.resolver with
  | None -> []
  | Some resolver ->
    List.concat_map resolver.executables ~f:(fun executable ->
      let common code requested ceiling remediation =
        violation t ~runtime_id:(runtime_id runtime) code ~requested ~ceiling ~remediation ()
      in
      List.filter_opt
        [ Option.some_if
            (t.require_executable_hashes && Option.is_none executable.sha256)
            (common "shell.admin_executable_hash_required" executable.id
               "every executable must have sha256"
               "pin the executable with sha256")
        ; Option.some_if
            (t.require_trusted_executables && not executable.trusted)
            (common "shell.admin_trusted_executable_required" executable.id
               "trusted=true required"
               "mark and pin an administratively trusted executable")
        ])
;;

let reviewer_kind = function
  | S.Ui_reviewer _ -> "ui"
  | Chatml_reviewer _ -> "chatml"
  | Executable_reviewer _ -> "executable"
  | Model_reviewer _ -> "model"
;;

let reviewer_violations t runtime =
  match runtime.S.reviewers, t.allowed_reviewer_kinds with
  | None, _ | _, None -> []
  | Some reviewers, Some allowed ->
    List.filter_map reviewers.values ~f:(fun reviewer ->
      let kind = reviewer_kind reviewer in
      Option.some_if
        (not (List.mem allowed kind ~equal:String.equal))
        (violation t ~runtime_id:(runtime_id runtime) "shell.admin_reviewer_denied"
           ~requested:kind ~ceiling:(String.concat ~sep:", " allowed)
           ~remediation:"remove or replace the reviewer" ()))
;;

let allows_scope t scope =
  List.mem t.allowed_approval_scopes scope ~equal:S.equal_approval_scope
;;

let approval_violations t runtime =
  match runtime.S.approvals with
  | None -> []
  | Some approvals ->
    List.filter_map approvals.scopes ~f:(fun scope ->
      Option.some_if
        (not (allows_scope t scope))
        (violation t ~runtime_id:(runtime_id runtime) "shell.admin_approval_scope_denied"
           ~requested:(S.sexp_of_approval_scope scope |> Sexp.to_string_hum)
           ~ceiling:"scope is not administratively enabled"
           ~remediation:"remove the approval scope from ChatMD" ()))
;;

let audit_violations t runtime =
  match runtime.S.audit with
  | None when t.require_audit ->
    [ violation t ~runtime_id:(runtime_id runtime) "shell.admin_audit_required"
        ~requested:"no audit" ~ceiling:"audit required"
        ~remediation:"configure a durable audit sink" () ]
  | Some audit
    when t.require_audit && S.equal_audit_format audit.format S.No_audit ->
    [ violation t ~runtime_id:(runtime_id runtime) "shell.admin_audit_required"
        ~requested:"audit=none" ~ceiling:"audit required"
        ~remediation:"configure a durable audit sink" () ]
  | Some audit
    when t.require_durable_audit
         && (S.equal_audit_format audit.format S.No_audit
             || S.equal_audit_format audit.format S.Stderr) ->
    [ violation t ~runtime_id:(runtime_id runtime) "shell.admin_durable_audit_required"
        ~requested:(S.sexp_of_audit_format audit.format |> Sexp.to_string_hum)
        ~ceiling:"durable audit required"
        ~remediation:"configure jsonl or session audit" () ]
  | None | Some _ -> []
;;

let limit_violation t runtime name requested ceiling =
  Option.some_if
    Float.(requested > ceiling)
    (violation
       t
       ~runtime_id:(runtime_id runtime)
       ("shell.admin_limit_" ^ name)
       ~requested:(Float.to_string requested)
       ~ceiling:(Float.to_string ceiling)
       ~remediation:("lower limits." ^ name ^ " in ChatMD")
       ())
;;

let duration_value = function
  | S.Set (Some value) -> Some (Chatmd_shell_spec.Duration.to_seconds value)
  | Set None | Inherit | Clear -> None
;;

let bytes_value = function
  | S.Set value ->
    Some (Chatmd_shell_spec.Duration.bytes_to_int64 value |> Int64.to_float)
  | Inherit | Clear -> None
;;

let integer_value = function
  | S.Set value -> Some (Float.of_int value)
  | Inherit | Clear -> None
;;

let checked_limit t runtime name requested ceiling =
  Option.bind requested ~f:(fun requested ->
    Option.bind ceiling ~f:(limit_violation t runtime name requested))
;;

let limit_violations t runtime =
  match runtime.S.limits with
  | None -> []
  | Some limits ->
    List.filter_opt
      [ checked_limit t runtime "wall_time" (duration_value limits.wall_time)
          t.limits.wall_time_seconds
      ; checked_limit t runtime "idle_time" (duration_value limits.idle_time)
          t.limits.idle_time_seconds
      ; checked_limit t runtime "max_stdin" (bytes_value limits.max_stdin)
          (Option.map t.limits.max_stdin_bytes ~f:Int64.to_float)
      ; checked_limit t runtime "stdout" (bytes_value limits.stdout)
          (Option.map t.limits.max_stdout_bytes ~f:Int64.to_float)
      ; checked_limit t runtime "stderr" (bytes_value limits.stderr)
          (Option.map t.limits.max_stderr_bytes ~f:Int64.to_float)
      ; checked_limit t runtime "total_output" (bytes_value limits.total_output)
          (Option.map t.limits.max_total_bytes ~f:Int64.to_float)
      ; checked_limit t runtime "cpu_time" (duration_value limits.cpu_time)
          t.limits.cpu_seconds
      ; checked_limit t runtime "memory" (bytes_value limits.memory)
          (Option.map t.limits.memory_bytes ~f:Int64.to_float)
      ; checked_limit t runtime "file_size" (bytes_value limits.file_size)
          (Option.map t.limits.file_size_bytes ~f:Int64.to_float)
      ; checked_limit t runtime "open_files" (integer_value limits.open_files)
          (Option.map t.limits.open_files ~f:Float.of_int)
      ]
;;

let profile_violations t runtime =
  if
    (not t.allow_yolo)
    && Option.exists runtime.S.resolved_profile ~f:(String.equal "builtin:yolo@1")
  then
    [ violation t ~runtime_id:(runtime_id runtime) "shell.admin_yolo_denied"
        ~requested:"builtin:yolo@1" ~ceiling:"yolo profiles disabled"
        ~remediation:"select a constrained versioned profile" () ]
  else []
;;

let extension_violations t runtime =
  if t.allow_hooks
  then []
  else
    let has_values = function
      | None -> false
      | Some values -> not (List.is_empty values)
    in
    let requested =
      has_values (Option.map runtime.S.reviewers ~f:(fun x -> x.values))
      || has_values (Option.map runtime.interceptors ~f:(fun x -> x.values))
      || has_values (Option.map runtime.effect_analysis ~f:(fun x -> x.analyzers))
      || Option.exists runtime.audit ~f:(fun audit -> Option.is_some audit.filter)
    in
    if requested
    then
      [ violation t ~runtime_id:(runtime_id runtime) "shell.admin_hooks_denied"
          ~requested:"runtime extensions" ~ceiling:"hooks disabled"
          ~remediation:"remove executable and ChatML runtime extensions" () ]
    else []
;;

let raw_prefix_violations t manifest runtimes =
  if t.allow_raw_prefix_grants
  then []
  else
    let runtime_by_id =
      List.fold runtimes ~init:String.Map.empty ~f:(fun map runtime ->
        Map.set map ~key:(runtime_id runtime) ~data:runtime)
    in
    List.filter_map manifest.Chatmd_shell_spec.Manifest.payload.tools ~f:(fun tool ->
      match tool.Chatmd_shell_spec.Shell_tool_spec.mode, Map.find runtime_by_id tool.runtime with
      | Raw _, Some runtime
        when Option.exists runtime.S.approvals ~f:(fun approvals ->
          List.mem approvals.scopes Prefix_session ~equal:S.equal_approval_scope) ->
        Some
          (violation
             t
             ~runtime_id:(runtime_id runtime)
             "shell.admin_raw_prefix_grant_denied"
             ~requested:("raw tool " ^ tool.name ^ " with prefix_session approval")
             ~ceiling:"prefix grants for raw shell tools are disabled"
             ~remediation:
               "remove prefix_session from the runtime or replace the raw tool with a structured tool"
             ())
      | (Fixed _ | Structured | Chain | Script_file _ | Raw _), _ -> None)
;;

let evaluate t ~manifest ~runtimes =
  let violations =
    raw_prefix_violations t manifest runtimes
    @ List.concat_map runtimes ~f:(fun runtime ->
        List.concat
          [ profile_violations t runtime
          ; capability_violations t runtime
          ; backend_violations t runtime
          ; executable_violations t runtime
          ; reviewer_violations t runtime
          ; approval_violations t runtime
          ; audit_violations t runtime
          ; limit_violations t runtime
          ; extension_violations t runtime
          ])
  in
  if List.is_empty violations then Ok () else Error violations
;;

let effect_kind = function
  | Shell_access.Effect.Read_path _ -> S.Read_path
  | Write_path _ -> Write_path
  | Network -> Network
  | Child_processes -> Child_processes
  | Arbitrary_code -> Arbitrary_code
  | Privilege_change -> Privilege_change
  | Unknown _ -> Unknown
;;

let check_context t context =
  let command = context.Shell_access.Context.command in
  let program_denied =
    List.mem t.denied_programs (Shell_access.Command.basename command) ~equal:String.equal
  in
  let argument_denied =
    List.exists (Shell_access.Command.to_argv command) ~f:(fun argument ->
      List.exists t.denied_argument_substrings ~f:(fun substring ->
        String.is_substring argument ~substring))
  in
  let denied_effect =
    List.find context.effects ~f:(fun effect_value ->
      List.mem
        t.denied_effects
        (effect_kind effect_value)
        ~equal:S.equal_process_effect)
  in
  let make code requested remediation =
    violation t ~runtime_id:context.runtime_id code ~requested
      ~ceiling:"administrative hard deny" ~remediation ()
  in
  let violations =
    List.filter_opt
      [ Option.some_if program_denied
          (make "shell.admin_program_denied" (Shell_access.Command.basename command)
             "use an administratively permitted executable")
      ; Option.some_if argument_denied
          (make "shell.admin_argument_denied" "redacted argument pattern matched"
             "remove the prohibited argument")
      ; Option.map denied_effect ~f:(fun effect_value ->
          make "shell.admin_effect_denied"
            (S.sexp_of_process_effect (effect_kind effect_value) |> Sexp.to_string_hum)
            "remove the prohibited effect")
      ]
  in
  if List.is_empty violations then Ok () else Error violations
;;
