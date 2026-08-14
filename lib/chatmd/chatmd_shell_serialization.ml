open! Core
module S = Chatmd_shell_spec.Shell_spec
module T = Chatmd_shell_spec.Shell_tool_spec

let escape value =
  String.concat_map value ~f:(function
    | '&' -> "&amp;"
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '"' -> "&quot;"
    | '\'' -> "&apos;"
    | character -> String.of_char character)
;;

let attribute name value = sprintf " %s=\"%s\"" name (escape value)
let optional_attribute name = Option.value_map ~default:"" ~f:(attribute name)
let bool value = Bool.to_string value

let element name attributes children =
  let attributes = String.concat attributes in
  match children with
  | [] -> sprintf "<%s%s/>" name attributes
  | children -> sprintf "<%s%s>%s</%s>" name attributes (String.concat children) name
;;

let path value = Chatmd_shell_spec.Path_expr.to_string value
let duration value = Chatmd_shell_spec.Duration.to_string value
let bytes value = Chatmd_shell_spec.Duration.bytes_to_string value

let setting name render = function
  | S.Inherit -> ""
  | S.Clear -> attribute name "none"
  | S.Set value -> attribute name (render value)
;;

let merge = function
  | S.Append -> ""
  | S.Replace -> attribute "merge" "replace"
;;

let sandbox = function
  | S.Required -> "required"
  | S.Preferred -> "preferred"
  | S.Direct_unsafe -> "direct_unsafe"
;;

let capability_root name (root : S.capability_root) =
  match root with
  | S.Path value -> element name [ attribute "path" (path value) ] []
  | S.Path_env { name = value; optional } ->
    element
      name
      [ attribute "path_env" value
      ; (if optional then attribute "optional" "true" else "")
      ]
      []
;;

let capabilities (value : S.capabilities) =
  let attributes =
    [ setting "sandbox" sandbox value.sandbox
    ; setting "network" bool value.network
    ; setting "child_processes" bool value.child_processes
    ; setting "arbitrary_code" bool value.arbitrary_code
    ; setting "privilege_change" bool value.privilege_change
    ; merge value.merge
    ]
  in
  let children =
    List.map value.read ~f:(capability_root "read")
    @ List.map value.write ~f:(capability_root "write")
  in
  element "capabilities" attributes children
;;

let executable (value : S.executable) =
  element
    "executable"
    [ attribute "id" value.id
    ; attribute "path" (path value.path)
    ; optional_attribute "sha256" value.sha256
    ; (if value.trusted then attribute "trusted" "true" else "")
    ; (if value.override then attribute "override" "true" else "")
    ]
    []
;;

let resolver (value : S.resolver) =
  let paths name values =
    List.map values ~f:(fun value -> element name [ attribute "path" (path value) ] [])
  in
  element
    "resolver"
    [ setting "allow_relative_search_path" bool value.allow_relative_search_path
    ; merge value.merge
    ]
    (paths "search_path" value.search_path
     @ paths "trusted_root" value.trusted_root
     @ List.map value.executables ~f:executable)
;;

let environment_inherit = function
  | S.None_ -> "none"
  | S.Safe -> "safe"
  | S.Selected -> "selected"
  | S.All_sanitized -> "all_sanitized"
  | S.Raw -> "raw"
;;

let path_position = function
  | S.Prepend -> "prepend"
  | S.Append_path -> "append"
;;

let environment_operation = function
  | S.Set_env { name; value } ->
    element "set" [ attribute "name" name; attribute "value" value ] []
  | S.Pass_env { name; required; secret } ->
    element
      "pass"
      [ attribute "name" name
      ; (if required then attribute "required" "true" else "")
      ; (if secret then attribute "secret" "true" else "")
      ]
      []
  | S.Unset_env name -> element "unset" [ attribute "name" name ] []
  | S.Unset_prefix value -> element "unset_prefix" [ attribute "value" value ] []
  | S.Path { position; path = value } ->
    element "path" [ attribute (path_position position) (path value) ] []
  | S.Path_env { name; suffix; position; required } ->
    element
      "path_env"
      [ attribute "name" name
      ; optional_attribute "suffix" suffix
      ; attribute "position" (path_position position)
      ; (if required then attribute "required" "true" else "")
      ]
      []
;;

let environment (value : S.environment) =
  element
    "environment"
    [ setting "inherit" environment_inherit value.inherit_; merge value.merge ]
    (List.map value.operations ~f:environment_operation)
;;

let optional_duration = Option.value_map ~default:"none" ~f:duration

let limits (value : S.limits) =
  element
    "limits"
    [ setting "wall_time" optional_duration value.wall_time
    ; setting "idle_time" optional_duration value.idle_time
    ; setting "max_stdin" bytes value.max_stdin
    ; setting "stdout" bytes value.stdout
    ; setting "stderr" bytes value.stderr
    ; setting "total_output" bytes value.total_output
    ; setting "cpu_time" optional_duration value.cpu_time
    ; setting "memory" bytes value.memory
    ; setting "file_size" bytes value.file_size
    ; setting "open_files" Int.to_string value.open_files
    ]
    []
;;

let platform = function
  | S.Macos -> "macos"
  | S.Linux -> "linux"
  | S.Windows -> "windows"
  | S.Any -> "any"
;;

let backend = function
  | S.Seatbelt { id; when_; executable; allow_system_reads } ->
    element
      "seatbelt"
      [ optional_attribute "id" id
      ; attribute "when" (platform when_)
      ; Option.value_map executable ~default:"" ~f:(fun value ->
          attribute "executable" (path value))
      ; attribute "allow_system_reads" (bool allow_system_reads)
      ]
      []
  | S.Bubblewrap { id; when_; executable; private_tmp; proc; dev } ->
    element
      "bubblewrap"
      [ optional_attribute "id" id
      ; attribute "when" (platform when_)
      ; Option.value_map executable ~default:"" ~f:(fun value ->
          attribute "executable" (path value))
      ; attribute "private_tmp" (bool private_tmp)
      ; attribute "proc" (bool proc)
      ; attribute "dev" dev
      ]
      []
  | S.Direct { id; when_ } ->
    element "direct" [ optional_attribute "id" id; attribute "when" (platform when_) ] []
  | S.External { id; when_; executable; sha256; confinement; atoms } ->
    let confinement =
      match confinement with
      | S.Verified_confinement -> "verified"
      | Declared_confinement -> "declared"
      | No_confinement -> "none"
    in
    let atom = function
      | S.Literal_atom value -> element "arg" [ attribute "value" value ] []
      | Cwd_atom -> element "cwd_value" [] []
      | Target_executable_atom -> element "target_executable" [] []
      | Command_argv_atom -> element "command_argv" [] []
      | Read_roots_atom { flag } -> element "read_roots" [ attribute "flag" flag ] []
      | Write_roots_atom { flag } -> element "write_roots" [ attribute "flag" flag ] []
      | Network_flag_atom value -> element "network_flag" [ attribute "value" value ] []
      | Resource_limit_args_atom -> element "resource_limit_args" [] []
    in
    element
      "external_backend"
      [ optional_attribute "id" id
      ; attribute "when" (platform when_)
      ; attribute "executable" (path executable)
      ; optional_attribute "sha256" sha256
      ; attribute "confinement" confinement
      ]
      (List.map atoms ~f:atom)
;;

let backends (value : S.backends) =
  element
    "backends"
    [ (if value.accept_declared_confinement
       then attribute "accept_declared_confinement" "true"
       else "")
    ; merge value.merge
    ]
    (List.map value.values ~f:backend)
;;

let policy_action = function
  | S.Allow -> "allow"
  | S.Ask -> "ask"
  | S.Deny -> "deny"
;;

let process_effect = function
  | S.Read_path -> "read_path"
  | S.Write_path -> "write_path"
  | S.Network -> "network"
  | S.Child_processes -> "child_processes"
  | S.Arbitrary_code -> "arbitrary_code"
  | S.Privilege_change -> "privilege_change"
  | S.Unknown -> "unknown"
;;

let lifecycle = function
  | S.Invocation -> "invocation"
  | S.Session -> "session"
  | S.Runtime -> "runtime"
;;

let hook_failure = function
  | S.Conservative_failure -> "conservative"
  | S.Deny_failure -> "deny"
  | S.Error_failure -> "error"
  | S.No_match_failure -> "no_match"
  | S.Unknown_failure -> "unknown"
  | S.Keep_failure -> "keep"
;;

let executable_hook (value : S.executable_hook) =
  [ attribute "executable" (path value.executable)
  ; optional_attribute "sha256" value.sha256
  ; attribute "runtime" value.runtime
  ; attribute "protocol" "shell-hook-json-v1"
  ; Option.value_map value.timeout ~default:"" ~f:(fun value ->
      attribute "timeout" (duration value))
  ; Option.value_map value.max_input ~default:"" ~f:(fun value ->
      attribute "max_input" (bytes value))
  ; Option.value_map value.max_output ~default:"" ~f:(fun value ->
      attribute "max_output" (bytes value))
  ]
;;

let rec matcher = function
  | S.Any_command -> element "any_command" [] []
  | S.Program value -> value_matcher "program" value
  | S.Basename value -> value_matcher "basename" value
  | S.Resolved_path value -> value_matcher "resolved_path" (path value)
  | S.Trusted_executable -> element "trusted_executable" [] []
  | S.Program_regex value -> value_matcher "program_regex" value
  | S.Argv_prefix values ->
    element "argv_prefix" [ attribute "values" (String.concat ~sep:"," values) ] []
  | S.Argument value -> value_matcher "argument" value
  | S.Argument_contains value -> value_matcher "argument_contains" value
  | S.Effect { kind; under } ->
    element
      "effect"
      [ attribute "name" (process_effect kind)
      ; Option.value_map under ~default:"" ~f:(fun value ->
          attribute "under" (path value))
      ]
      []
  | S.No_unknown_effects -> element "no_unknown_effects" [] []
  | S.Raw_shell_request -> element "raw_shell" [] []
  | S.Chatml_match { script; function_; failure } ->
    element
      "chatml_match"
      [ attribute "script" script
      ; attribute "function" function_
      ; attribute "failure" (hook_failure failure)
      ]
      []
  | S.All values -> element "all" [] (List.map values ~f:matcher)
  | S.Any values -> element "any" [] (List.map values ~f:matcher)
  | S.Not value -> element "not" [] [ matcher value ]

and value_matcher name value = element name [ attribute "value" value ] []

let policy_rule (value : S.policy_rule) =
  element
    "rule"
    [ attribute "id" value.id
    ; attribute "action" (policy_action value.action)
    ; (if value.override then attribute "override" "true" else "")
    ]
    [ matcher value.matcher ]
;;

let policy (value : S.policy) =
  element
    "policy"
    [ setting "default" policy_action value.default; merge value.merge ]
    (List.map value.rules ~f:policy_rule)
;;

let approval_provider = function
  | S.Ui -> "ui"
  | S.No_provider -> "none"
;;

let approval_unavailable = function
  | S.Deny_unavailable -> "deny"
  | S.Error_unavailable -> "error"
;;

let approval_scope = function
  | S.Once -> "once"
  | S.Exact_session -> "exact_session"
  | S.Prefix_session -> "prefix_session"
  | S.Durable_exact -> "durable_exact"
;;

let approvals (value : S.approvals) =
  element
    "approvals"
    [ setting "provider" approval_provider value.provider
    ; setting "unavailable" approval_unavailable value.unavailable
    ; (if List.is_empty value.scopes
       then ""
       else
         attribute
           "scopes"
           (List.map value.scopes ~f:approval_scope |> String.concat ~sep:","))
    ; setting "durable" bool value.durable
    ]
    []
;;

let reviewer = function
  | S.Ui_reviewer { id } ->
    element "reviewer" [ attribute "id" id; attribute "kind" "ui" ] []
  | S.Chatml_reviewer { id; script; lifecycle = lifetime; failure } ->
    element
      "reviewer"
      [ attribute "id" id
      ; attribute "kind" "chatml"
      ; attribute "script" script
      ; attribute "lifecycle" (lifecycle lifetime)
      ; attribute "failure" (hook_failure failure)
      ]
      []
  | S.Model_reviewer { id; agent; model; failure } ->
    element
      "reviewer"
      [ attribute "id" id
      ; attribute "kind" "model"
      ; attribute "agent" agent
      ; optional_attribute "model" model
      ; attribute "failure" (hook_failure failure)
      ]
      []
  | S.Executable_reviewer { id; hook; failure } ->
    element
      "reviewer"
      ([ attribute "id" id
       ; attribute "kind" "executable"
       ; attribute "failure" (hook_failure failure)
       ]
       @ executable_hook hook)
      []
;;

let reviewers (value : S.reviewers) =
  element "reviewers" [ merge value.merge ] (List.map value.values ~f:reviewer)
;;

let interceptor_phase = function
  | S.Before -> "before"
  | S.After -> "after"
;;

let interceptor (value : S.interceptor) =
  let children =
    Option.to_list
      (Option.map value.matcher ~f:(fun value -> element "match" [] [ matcher value ]))
  in
  let extension =
    match value.extension with
    | S.Chatml_extension { script; lifecycle = lifetime } ->
      [ attribute "script" script; attribute "lifecycle" (lifecycle lifetime) ]
    | Executable_extension hook -> executable_hook hook
  in
  element
    "interceptor"
    ([ attribute "id" value.id
     ; attribute "phase" (interceptor_phase value.phase)
     ; attribute "failure" (hook_failure value.failure)
     ]
     @ extension)
    children
;;

let interceptors (value : S.interceptors) =
  element "interceptors" [ merge value.merge ] (List.map value.values ~f:interceptor)
;;

let effect_analyzer (value : S.effect_analyzer) =
  let kind, extension =
    match value.extension with
    | S.Chatml_extension { script; lifecycle = lifetime } ->
      ( "chatml"
      , [ attribute "script" script; attribute "lifecycle" (lifecycle lifetime) ] )
    | Executable_extension hook -> "executable", executable_hook hook
  in
  element
    "analyzer"
    ([ attribute "id" value.id
     ; attribute "kind" kind
     ; attribute "replace" (bool value.replace)
     ; attribute "failure" (hook_failure value.failure)
     ]
     @ extension)
    []
;;

let effect_analysis (value : S.effect_analysis) =
  element
    "effect_analysis"
    [ merge value.merge ]
    (List.map value.analyzers ~f:effect_analyzer)
;;

let secret_source = function
  | S.From_env { name; optional } ->
    element
      "from_env"
      [ attribute "name" name; (if optional then attribute "optional" "true" else "") ]
      []
  | S.From_file { path = value; optional; strip } ->
    element
      "from_file"
      [ attribute "path" (path value)
      ; (if optional then attribute "optional" "true" else "")
      ; attribute "strip" (bool strip)
      ]
      []
  | S.Literal value -> element "literal" [ attribute "value" value ] []
;;

let secrets (value : S.secrets) =
  element
    "secrets"
    [ setting "replacement" Fn.id value.replacement; merge value.merge ]
    (List.map value.sources ~f:secret_source)
;;

let audit_format = function
  | S.No_audit -> "none"
  | S.Stderr -> "stderr"
  | S.Jsonl -> "jsonl"
  | S.Session -> "session"
;;

let audit_content = function
  | S.Metadata -> "metadata"
  | S.Redacted -> "redacted"
  | S.Full -> "full"
;;

let audit_failure = function
  | S.Continue -> "continue"
  | S.Deny_start -> "deny_start"
  | S.Terminate -> "terminate"
;;

let audit (value : S.audit) =
  element
    "audit"
    [ attribute "format" (audit_format value.format)
    ; Option.value_map value.path ~default:"" ~f:(fun value ->
        attribute "path" (path value))
    ; attribute "content" (audit_content value.content)
    ; attribute "failure" (audit_failure value.failure)
    ]
    (Option.to_list
       (Option.map value.filter ~f:(function
          | S.Chatml_extension { script; lifecycle = lifetime } ->
            element
              "filter"
              [ attribute "script" script; attribute "lifecycle" (lifecycle lifetime) ]
              []
          | Executable_extension hook -> element "filter" (executable_hook hook) [])))
;;

let runtime (value : S.t) =
  let children =
    List.filter_opt
      [ Option.map value.capabilities ~f:capabilities
      ; Option.map value.resolver ~f:resolver
      ; Option.map value.environment ~f:environment
      ; Option.map value.limits ~f:limits
      ; Option.map value.backends ~f:backends
      ; Option.map value.policy ~f:policy
      ; Option.map value.approvals ~f:approvals
      ; Option.map value.reviewers ~f:reviewers
      ; Option.map value.interceptors ~f:interceptors
      ; Option.map value.effect_analysis ~f:effect_analysis
      ; Option.map value.secrets ~f:secrets
      ; Option.map value.audit ~f:audit
      ]
  in
  element
    "shell_access"
    [ attribute "id" (S.Runtime_id.to_string value.id)
    ; optional_attribute "extends" value.extends
    ; setting "cwd" path value.cwd
    ; setting "pipefail" bool value.pipefail
    ]
    children
;;

let stdin_mode = function
  | T.No_stdin -> "none"
  | T.Optional_stdin -> "optional"
  | T.Required_stdin -> "required"
;;

let rationale_mode = function
  | T.No_rationale -> "none"
  | T.Optional_rationale -> "optional"
  | T.Required_rationale -> "required"
;;

let result_format = function
  | T.Combined -> "combined"
  | T.Stdout -> "stdout"
  | T.Structured_result -> "structured"
;;

let stream_mode = function
  | T.Finalized -> "finalized"
  | T.Sanitized -> "sanitized"
;;

let nonzero = function
  | T.Result -> "result"
  | T.Error -> "error"
;;

let arguments_mode = function
  | T.No_arguments -> "none"
  | T.Optional_arguments -> "optional"
  | T.Required_arguments -> "required"
;;

let arguments (value : T.arguments) =
  element
    "arguments"
    [ attribute "mode" (arguments_mode value.mode)
    ; Option.value_map value.min_count ~default:"" ~f:(fun value ->
        attribute "min_count" (Int.to_string value))
    ; Option.value_map value.max_count ~default:"" ~f:(fun value ->
        attribute "max_count" (Int.to_string value))
    ; Option.value_map value.max_item_bytes ~default:"" ~f:(fun value ->
        attribute "max_item_bytes" (Int.to_string value))
    ]
    []
;;

let fixed_argument = function
  | T.Literal value -> element "arg" [ attribute "value" value ] []
  | T.Secret_env { name; prefix } ->
    element "secret_arg" [ attribute "env" name; attribute "prefix" prefix ] []
  | T.Path (Chatmd_shell_spec.Path_expr.Absolute value) ->
    element "path_arg" [ attribute "path" value ] []
  | T.Path (Chatmd_shell_spec.Path_expr.Relative { base; path = value }) ->
    element
      "path_arg"
      [ attribute "base" (Chatmd_shell_spec.Path_expr.base_to_string base)
      ; attribute "path" value
      ]
      []
;;

let fixed_command = function
  | T.Compact value -> `Attribute value
  | T.Argv { program; executable_ref; arguments } ->
    `Child
      (element
         "command"
         [ optional_attribute "program" program
         ; optional_attribute "executable_ref" executable_ref
         ]
         (List.map arguments ~f:fixed_argument))
;;

let mode = function
  | T.Fixed _ -> "fixed"
  | T.Structured -> "structured"
  | T.Chain -> "chain"
  | T.Raw _ -> "raw"
  | T.Script_file _ -> "script"
;;

let mode_attributes = function
  | T.Fixed { command; _ } ->
    (match fixed_command command with
     | `Attribute value -> [ attribute "command" value ]
     | `Child _ -> [])
  | T.Raw { executable } -> [ attribute "executable" (path executable) ]
  | T.Script_file { script; interpreter; executable } ->
    [ attribute "script" (path script)
    ; Option.value_map interpreter ~default:"" ~f:(fun value ->
        attribute "interpreter" (path value))
    ; attribute "executable" (bool executable)
    ]
  | T.Structured | T.Chain -> []
;;

let mode_children = function
  | T.Fixed { command; model_arguments } ->
    let command =
      match fixed_command command with
      | `Attribute _ -> []
      | `Child value -> [ value ]
    in
    command @ [ arguments model_arguments ]
  | T.Structured | T.Chain | T.Raw _ | T.Script_file _ -> []
;;

let tool (value : T.t) =
  element
    "tool"
    ([ attribute "name" value.name
     ; attribute "type" "shell"
     ; attribute "mode" (mode value.mode)
     ; attribute "runtime" value.runtime
     ; optional_attribute "description" value.description
     ; attribute "stdin" (stdin_mode value.stdin)
     ; attribute "rationale" (rationale_mode value.rationale)
     ; attribute "result" (result_format value.result)
     ; attribute "stream" (stream_mode value.stream)
     ; attribute "nonzero" (nonzero value.nonzero)
     ]
     @ mode_attributes value.mode)
    (mode_children value.mode)
;;
