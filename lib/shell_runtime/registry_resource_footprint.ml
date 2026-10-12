open Core
module S = Chatmd_shell_spec.Shell_spec
module T = Chatmd_shell_spec.Shell_tool_spec
module P = Admin_policy
module B = Registry_code_budget

module Error = struct
  type t =
    { code : string
    ; runtime_id : string option
    ; field : string
    }
  [@@deriving sexp, equal]
end

open Registry_footprint_comparison

let check budget ~runtime_id ~field equal left right =
  B.work budget;
  if equal budget left right
  then Ok ()
  else Error Error.{ code = "shell.native_footprint_changed"; runtime_id; field }
;;

let check_runtime original ~candidate ~budget =
  Registry_runtime_footprint.check original ~candidate ~budget
  |> Result.map_error ~f:(fun field ->
    Error.
      { code = "shell.native_footprint_changed"
      ; runtime_id = Some (S.Runtime_id.to_string original.id)
      ; field
      })
;;

let arguments budget (left : T.arguments) (right : T.arguments) =
  B.work budget;
  let ({ T.mode = original_mode
       ; min_count = original_min_count
       ; max_count = original_max_count
       ; max_item_bytes = original_max_item_bytes
       }
        : T.arguments)
    =
    left
  in
  scalar T.equal_arguments_mode budget original_mode right.mode
  && option int budget original_min_count right.min_count
  && option int budget original_max_count right.max_count
  && option int budget original_max_item_bytes right.max_item_bytes
;;

let fixed_argument budget (left : T.fixed_argument) (right : T.fixed_argument) =
  B.work budget;
  match left, right with
  | T.Literal a, Literal b -> string budget a b
  | Secret_env { name = a; prefix = ap }, Secret_env { name = b; prefix = bp } ->
    string budget a b && string budget ap bp
  | Path a, Path b -> path budget a b
  | (Literal _ | Secret_env _ | Path _), _ -> false
;;

let fixed_command budget (left : T.fixed_command) (right : T.fixed_command) =
  B.work budget;
  match left, right with
  | T.Compact a, Compact b -> string budget a b
  | ( Argv { program = a; executable_ref = ae; arguments = aa }
    , Argv { program = b; executable_ref = be; arguments = ba } ) ->
    option string budget a b
    && option string budget ae be
    && list fixed_argument budget aa ba
  | Compact _, Argv _ | Argv _, Compact _ -> false
;;

let raw_shell budget (left : T.raw_shell) (right : T.raw_shell) =
  B.work budget;
  let ({ T.executable = original_executable
       ; arguments_before_script = original_arguments_before_script
       }
        : T.raw_shell)
    =
    left
  in
  path budget original_executable right.executable
  && list string budget original_arguments_before_script right.arguments_before_script
;;

let script_file budget (left : T.script_file) (right : T.script_file) =
  B.work budget;
  let ({ T.script = original_script
       ; interpreter = original_interpreter
       ; executable = original_executable
       ; fixed_arguments = original_fixed_arguments
       ; verification = original_verification
       ; max_source_bytes = original_max_source_bytes
       }
        : T.script_file)
    =
    left
  in
  path budget original_script right.script
  && option path budget original_interpreter right.interpreter
  && bool budget original_executable right.executable
  && list string budget original_fixed_arguments right.fixed_arguments
  && scalar T.equal_verification_mode budget original_verification right.verification
  && int budget original_max_source_bytes right.max_source_bytes
;;

let mode budget (left : T.mode) (right : T.mode) =
  B.work budget;
  match left, right with
  | ( T.Fixed { command = a; model_arguments = aa }
    , Fixed { command = b; model_arguments = ba } ) ->
    fixed_command budget a b && arguments budget aa ba
  | Structured, Structured | Chain, Chain -> true
  | Raw a, Raw b -> raw_shell budget a b
  | Script_file a, Script_file b -> script_file budget a b
  | (Fixed _ | Structured | Chain | Raw _ | Script_file _), _ -> false
;;

let tool budget (left : T.t) (right : T.t) =
  B.work budget;
  let ({ T.name = original_name
       ; description = original_description
       ; runtime = original_runtime
       ; mode = original_mode
       ; stdin = original_stdin
       ; rationale = original_rationale
       ; result = original_result
       ; stream = original_stream
       ; nonzero = original_nonzero
       ; source = _
       }
        : T.t)
    =
    left
  in
  string budget original_name right.name
  && option string budget original_description right.description
  && string budget original_runtime right.runtime
  && mode budget original_mode right.mode
  && scalar T.equal_stdin_mode budget original_stdin right.stdin
  && scalar T.equal_rationale_mode budget original_rationale right.rationale
  && scalar T.equal_result_format budget original_result right.result
  && scalar T.equal_stream_mode budget original_stream right.stream
  && scalar T.equal_nonzero budget original_nonzero right.nonzero
;;

let check_tool (left : T.t) ~candidate:right ~budget =
  check budget ~runtime_id:(Some left.runtime) ~field:"tool" tool left right
;;

let extension_limits
      budget
      (left : Chatmd_shell_spec.Chatmd_script_spec.limits)
      (right : Chatmd_shell_spec.Chatmd_script_spec.limits)
  =
  B.work budget;
  let { Chatmd_shell_spec.Chatmd_script_spec.wall_time = original_wall_time
      ; fuel = original_fuel
      ; max_tasks = original_max_tasks
      ; max_value_bytes = original_max_value_bytes
      ; max_output_bytes = original_max_output_bytes
      ; max_array_items = original_max_array_items
      ; max_depth = original_max_depth
      }
    =
    left
  in
  duration budget original_wall_time right.wall_time
  && int budget original_fuel right.fuel
  && int budget original_max_tasks right.max_tasks
  && bytes budget original_max_value_bytes right.max_value_bytes
  && bytes budget original_max_output_bytes right.max_output_bytes
  && int budget original_max_array_items right.max_array_items
  && int budget original_max_depth right.max_depth
;;

let extension_identity
      budget
      (left : Chatmd_shell_spec.Manifest.extension_script)
      (right : Chatmd_shell_spec.Manifest.extension_script)
  =
  B.work budget;
  let { Chatmd_shell_spec.Manifest.id = original_id
      ; kind = original_kind
      ; surface_version = original_surface_version
      ; entrypoint = original_entrypoint
      ; limits = original_limits
      ; source_sha256 = _
      }
    =
    left
  in
  string budget original_id right.id
  && scalar
       Chatmd_shell_spec.Chatmd_script_spec.equal_kind
       budget
       original_kind
       right.kind
  && string budget original_surface_version right.surface_version
  && string budget original_entrypoint right.entrypoint
  && extension_limits budget original_limits right.limits
;;

let check_extension left ~candidate:right ~budget =
  check budget ~runtime_id:None ~field:"extension" extension_identity left right
;;

let limit_ceiling budget (left : P.Limit_ceiling.t) (right : P.Limit_ceiling.t) =
  B.work budget;
  let ({ P.Limit_ceiling.wall_time_seconds = original_wall_time_seconds
       ; idle_time_seconds = original_idle_time_seconds
       ; max_stdin_bytes = original_max_stdin_bytes
       ; max_stdout_bytes = original_max_stdout_bytes
       ; max_stderr_bytes = original_max_stderr_bytes
       ; max_total_bytes = original_max_total_bytes
       ; cpu_seconds = original_cpu_seconds
       ; memory_bytes = original_memory_bytes
       ; file_size_bytes = original_file_size_bytes
       ; open_files = original_open_files
       }
        : P.Limit_ceiling.t)
    =
    left
  in
  option float budget original_wall_time_seconds right.wall_time_seconds
  && option float budget original_idle_time_seconds right.idle_time_seconds
  && option int64 budget original_max_stdin_bytes right.max_stdin_bytes
  && option int64 budget original_max_stdout_bytes right.max_stdout_bytes
  && option int64 budget original_max_stderr_bytes right.max_stderr_bytes
  && option int64 budget original_max_total_bytes right.max_total_bytes
  && option float budget original_cpu_seconds right.cpu_seconds
  && option int64 budget original_memory_bytes right.memory_bytes
  && option int64 budget original_file_size_bytes right.file_size_bytes
  && option int budget original_open_files right.open_files
;;

let admin_policy budget (left : P.t) (right : P.t) =
  B.work budget;
  let ({ P.source = original_source
       ; allowed_read_roots = original_allowed_read_roots
       ; allowed_write_roots = original_allowed_write_roots
       ; allow_network = original_allow_network
       ; allow_child_processes = original_allow_child_processes
       ; allow_arbitrary_code = original_allow_arbitrary_code
       ; allow_privilege_change = original_allow_privilege_change
       ; require_sandbox = original_require_sandbox
       ; allow_direct_backend = original_allow_direct_backend
       ; allow_declared_confinement = original_allow_declared_confinement
       ; allow_external_backends = original_allow_external_backends
       ; allow_hooks = original_allow_hooks
       ; allowed_reviewer_kinds = original_allowed_reviewer_kinds
       ; allowed_approval_scopes = original_allowed_approval_scopes
       ; allow_raw_prefix_grants = original_allow_raw_prefix_grants
       ; allow_yolo = original_allow_yolo
       ; require_executable_hashes = original_require_executable_hashes
       ; require_trusted_executables = original_require_trusted_executables
       ; require_audit = original_require_audit
       ; require_durable_audit = original_require_durable_audit
       ; require_trusted_source = original_require_trusted_source
       ; require_signature = original_require_signature
       ; denied_programs = original_denied_programs
       ; denied_argument_substrings = original_denied_argument_substrings
       ; denied_effects = original_denied_effects
       ; limits = original_limits
       }
        : P.t)
    =
    left
  in
  string budget original_source right.source
  && option (list string) budget original_allowed_read_roots right.allowed_read_roots
  && option (list string) budget original_allowed_write_roots right.allowed_write_roots
  && bool budget original_allow_network right.allow_network
  && bool budget original_allow_child_processes right.allow_child_processes
  && bool budget original_allow_arbitrary_code right.allow_arbitrary_code
  && bool budget original_allow_privilege_change right.allow_privilege_change
  && bool budget original_require_sandbox right.require_sandbox
  && bool budget original_allow_direct_backend right.allow_direct_backend
  && bool budget original_allow_declared_confinement right.allow_declared_confinement
  && bool budget original_allow_external_backends right.allow_external_backends
  && bool budget original_allow_hooks right.allow_hooks
  && option
       (list string)
       budget
       original_allowed_reviewer_kinds
       right.allowed_reviewer_kinds
  && list
       (scalar S.equal_approval_scope)
       budget
       original_allowed_approval_scopes
       right.allowed_approval_scopes
  && bool budget original_allow_raw_prefix_grants right.allow_raw_prefix_grants
  && bool budget original_allow_yolo right.allow_yolo
  && bool budget original_require_executable_hashes right.require_executable_hashes
  && bool budget original_require_trusted_executables right.require_trusted_executables
  && bool budget original_require_audit right.require_audit
  && bool budget original_require_durable_audit right.require_durable_audit
  && bool budget original_require_trusted_source right.require_trusted_source
  && bool budget original_require_signature right.require_signature
  && list string budget original_denied_programs right.denied_programs
  && list
       string
       budget
       original_denied_argument_substrings
       right.denied_argument_substrings
  && list
       (scalar S.equal_process_effect)
       budget
       original_denied_effects
       right.denied_effects
  && limit_ceiling budget original_limits right.limits
;;

let check_admin_policy left ~candidate:right ~budget =
  check budget ~runtime_id:None ~field:"administrative_policy" admin_policy left right
;;
