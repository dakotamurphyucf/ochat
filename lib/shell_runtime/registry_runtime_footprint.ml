open Core
module S = Chatmd_shell_spec.Shell_spec
module B = Registry_code_budget
open Registry_footprint_comparison

let capability_root budget (left : S.capability_root) (right : S.capability_root) =
  B.work budget;
  match left, right with
  | S.Path left, Path right -> path budget left right
  | ( Path_env { name = left_name; optional = left_optional }
    , Path_env { name = right_name; optional = right_optional } ) ->
    string budget left_name right_name && bool budget left_optional right_optional
  | Path _, Path_env _ | Path_env _, Path _ -> false
;;

let capabilities budget (left : S.capabilities) (right : S.capabilities) =
  B.work budget;
  let ({ S.sandbox = original_sandbox
       ; network = original_network
       ; child_processes = original_child_processes
       ; arbitrary_code = original_arbitrary_code
       ; privilege_change = original_privilege_change
       ; read = original_read
       ; write = original_write
       ; merge = original_merge
       }
        : S.capabilities)
    =
    left
  in
  setting (scalar S.equal_sandbox) budget original_sandbox right.sandbox
  && setting bool budget original_network right.network
  && setting bool budget original_child_processes right.child_processes
  && setting bool budget original_arbitrary_code right.arbitrary_code
  && setting bool budget original_privilege_change right.privilege_change
  && list capability_root budget original_read right.read
  && list capability_root budget original_write right.write
  && scalar S.equal_merge budget original_merge right.merge
;;

let executable budget (left : S.executable) (right : S.executable) =
  B.work budget;
  let ({ S.id = original_id
       ; path = original_path
       ; sha256 = original_sha256
       ; trusted = original_trusted
       ; override = original_override
       }
        : S.executable)
    =
    left
  in
  string budget original_id right.id
  && path budget original_path right.path
  && option string budget original_sha256 right.sha256
  && bool budget original_trusted right.trusted
  && bool budget original_override right.override
;;

let resolver budget (left : S.resolver) (right : S.resolver) =
  B.work budget;
  let ({ S.search_path = original_search_path
       ; trusted_root = original_trusted_root
       ; executables = original_executables
       ; allow_relative_search_path = original_allow_relative_search_path
       ; merge = original_merge
       }
        : S.resolver)
    =
    left
  in
  list path budget original_search_path right.search_path
  && list path budget original_trusted_root right.trusted_root
  && list executable budget original_executables right.executables
  && setting
       bool
       budget
       original_allow_relative_search_path
       right.allow_relative_search_path
  && scalar S.equal_merge budget original_merge right.merge
;;

let environment_operation
      budget
      (left : S.environment_operation)
      (right : S.environment_operation)
  =
  B.work budget;
  match left, right with
  | S.Set_env { name = a; value = av }, Set_env { name = b; value = bv } ->
    string budget a b && string budget av bv
  | ( Pass_env { name = a; required = ar; secret = ase }
    , Pass_env { name = b; required = br; secret = bse } ) ->
    string budget a b && bool budget ar br && bool budget ase bse
  | Unset_env a, Unset_env b | Unset_prefix a, Unset_prefix b -> string budget a b
  | Path { position = a; path = ap }, Path { position = b; path = bp } ->
    scalar S.equal_path_position budget a b && path budget ap bp
  | ( Path_env { name = a; suffix = asu; position = ap; required = ar }
    , Path_env { name = b; suffix = bsu; position = bp; required = br } ) ->
    string budget a b
    && option string budget asu bsu
    && scalar S.equal_path_position budget ap bp
    && bool budget ar br
  | (Set_env _ | Pass_env _ | Unset_env _ | Unset_prefix _ | Path _ | Path_env _), _ ->
    false
;;

let environment budget (left : S.environment) (right : S.environment) =
  B.work budget;
  let ({ S.inherit_ = original_inherit_
       ; operations = original_operations
       ; merge = original_merge
       }
        : S.environment)
    =
    left
  in
  setting (scalar S.equal_environment_inherit) budget original_inherit_ right.inherit_
  && list environment_operation budget original_operations right.operations
  && scalar S.equal_merge budget original_merge right.merge
;;

let limits budget (left : S.limits) (right : S.limits) =
  B.work budget;
  let ({ S.wall_time = original_wall_time
       ; idle_time = original_idle_time
       ; max_stdin = original_max_stdin
       ; stdout = original_stdout
       ; stderr = original_stderr
       ; total_output = original_total_output
       ; cpu_time = original_cpu_time
       ; memory = original_memory
       ; file_size = original_file_size
       ; open_files = original_open_files
       }
        : S.limits)
    =
    left
  in
  setting (option duration) budget original_wall_time right.wall_time
  && setting (option duration) budget original_idle_time right.idle_time
  && setting bytes budget original_max_stdin right.max_stdin
  && setting bytes budget original_stdout right.stdout
  && setting bytes budget original_stderr right.stderr
  && setting bytes budget original_total_output right.total_output
  && setting (option duration) budget original_cpu_time right.cpu_time
  && setting bytes budget original_memory right.memory
  && setting bytes budget original_file_size right.file_size
  && setting int budget original_open_files right.open_files
;;

let backend_atom budget (left : S.backend_atom) (right : S.backend_atom) =
  B.work budget;
  match left, right with
  | S.Literal_atom a, Literal_atom b | Network_flag_atom a, Network_flag_atom b ->
    string budget a b
  | Cwd_atom, Cwd_atom
  | Target_executable_atom, Target_executable_atom
  | Command_argv_atom, Command_argv_atom
  | Resource_limit_args_atom, Resource_limit_args_atom -> true
  | Read_roots_atom { flag = a }, Read_roots_atom { flag = b }
  | Write_roots_atom { flag = a }, Write_roots_atom { flag = b } -> string budget a b
  | ( ( Literal_atom _
      | Cwd_atom
      | Target_executable_atom
      | Command_argv_atom
      | Read_roots_atom _
      | Write_roots_atom _
      | Network_flag_atom _
      | Resource_limit_args_atom )
    , _ ) -> false
;;

let backend budget (left : S.backend) (right : S.backend) =
  B.work budget;
  match left, right with
  | ( S.Seatbelt { id = a; when_ = aw; executable = ae; allow_system_reads = ar }
    , Seatbelt { id = b; when_ = bw; executable = be; allow_system_reads = br } ) ->
    option string budget a b
    && scalar S.equal_platform budget aw bw
    && option path budget ae be
    && bool budget ar br
  | ( Bubblewrap
        { id = a; when_ = aw; executable = ae; private_tmp = at; proc = ap; dev = ad }
    , Bubblewrap
        { id = b; when_ = bw; executable = be; private_tmp = bt; proc = bp; dev = bd } )
    ->
    option string budget a b
    && scalar S.equal_platform budget aw bw
    && option path budget ae be
    && bool budget at bt
    && bool budget ap bp
    && string budget ad bd
  | Direct { id = a; when_ = aw }, Direct { id = b; when_ = bw } ->
    option string budget a b && scalar S.equal_platform budget aw bw
  | ( External
        { id = a
        ; when_ = aw
        ; executable = ae
        ; sha256 = ash
        ; confinement = ac
        ; atoms = aa
        }
    , External
        { id = b
        ; when_ = bw
        ; executable = be
        ; sha256 = bsh
        ; confinement = bc
        ; atoms = ba
        } ) ->
    option string budget a b
    && scalar S.equal_platform budget aw bw
    && path budget ae be
    && option string budget ash bsh
    && scalar S.equal_confinement budget ac bc
    && list backend_atom budget aa ba
  | (Seatbelt _ | Bubblewrap _ | Direct _ | External _), _ -> false
;;

let backends budget (left : S.backends) (right : S.backends) =
  B.work budget;
  let ({ S.values = original_values
       ; accept_declared_confinement = original_accept_declared_confinement
       ; merge = original_merge
       }
        : S.backends)
    =
    left
  in
  list backend budget original_values right.values
  && bool budget original_accept_declared_confinement right.accept_declared_confinement
  && scalar S.equal_merge budget original_merge right.merge
;;

let executable_hook budget (left : S.executable_hook) (right : S.executable_hook) =
  B.work budget;
  let ({ S.executable = original_executable
       ; sha256 = original_sha256
       ; runtime = original_runtime
       ; protocol = original_protocol
       ; timeout = original_timeout
       ; max_input = original_max_input
       ; max_output = original_max_output
       }
        : S.executable_hook)
    =
    left
  in
  path budget original_executable right.executable
  && option string budget original_sha256 right.sha256
  && string budget original_runtime right.runtime
  && scalar S.equal_hook_protocol budget original_protocol right.protocol
  && option duration budget original_timeout right.timeout
  && option bytes budget original_max_input right.max_input
  && option bytes budget original_max_output right.max_output
;;

let extension budget (left : S.extension) (right : S.extension) =
  B.work budget;
  match left, right with
  | ( S.Chatml_extension { script = a; lifecycle = al }
    , Chatml_extension { script = b; lifecycle = bl } ) ->
    string budget a b && scalar S.equal_lifecycle budget al bl
  | Executable_extension a, Executable_extension b -> executable_hook budget a b
  | Chatml_extension _, Executable_extension _
  | Executable_extension _, Chatml_extension _ -> false
;;

let chatml_matcher budget (left : S.chatml_matcher) (right : S.chatml_matcher) =
  B.work budget;
  let ({ S.script = original_script
       ; function_ = original_function_
       ; failure = original_failure
       }
        : S.chatml_matcher)
    =
    left
  in
  string budget original_script right.script
  && string budget original_function_ right.function_
  && scalar S.equal_hook_failure budget original_failure right.failure
;;

(* Only matcher is recursive. Retain pending pairs after charging each cell;
   nested Not/All/Any and large flat lists use constant call-stack depth. *)
let matcher budget (left : S.matcher) (right : S.matcher) =
  let rec prepend left right pending =
    B.work budget;
    match left, right with
    | [], [] -> Some pending
    | left :: lefts, right :: rights -> prepend lefts rights ((left, right) :: pending)
    | [], _ :: _ | _ :: _, [] -> None
  in
  let rec walk (pending : (S.matcher * S.matcher) list) =
    match pending with
    | [] -> true
    | (left, right) :: pending ->
      B.work budget;
      (match left, right with
       | S.Any_command, Any_command
       | Trusted_executable, Trusted_executable
       | No_unknown_effects, No_unknown_effects
       | Raw_shell_request, Raw_shell_request -> walk pending
       | Program a, Program b
       | Basename a, Basename b
       | Program_regex a, Program_regex b
       | Argument a, Argument b
       | Argument_contains a, Argument_contains b -> string budget a b && walk pending
       | Resolved_path a, Resolved_path b -> path budget a b && walk pending
       | Argv_prefix a, Argv_prefix b -> list string budget a b && walk pending
       | Effect { kind = a; under = au }, Effect { kind = b; under = bu } ->
         scalar S.equal_process_effect budget a b
         && option path budget au bu
         && walk pending
       | Chatml_match a, Chatml_match b -> chatml_matcher budget a b && walk pending
       | Not a, Not b ->
         B.work budget;
         walk ((a, b) :: pending)
       | All a, All b | Any a, Any b ->
         (match prepend a b pending with
          | None -> false
          | Some pending -> walk pending)
       | ( ( Any_command
           | Program _
           | Basename _
           | Resolved_path _
           | Trusted_executable
           | Program_regex _
           | Argv_prefix _
           | Argument _
           | Argument_contains _
           | Effect _
           | No_unknown_effects
           | Raw_shell_request
           | Chatml_match _
           | All _
           | Any _
           | Not _ )
         , _ ) -> false)
  in
  B.work budget;
  walk [ left, right ]
;;

let policy_rule budget (left : S.policy_rule) (right : S.policy_rule) =
  B.work budget;
  let ({ S.id = original_id
       ; action = original_action
       ; matcher = original_matcher
       ; override = original_override
       }
        : S.policy_rule)
    =
    left
  in
  string budget original_id right.id
  && scalar S.equal_policy_action budget original_action right.action
  && matcher budget original_matcher right.matcher
  && bool budget original_override right.override
;;

let policy budget (left : S.policy) (right : S.policy) =
  B.work budget;
  let ({ S.default = original_default; rules = original_rules; merge = original_merge }
        : S.policy)
    =
    left
  in
  setting (scalar S.equal_policy_action) budget original_default right.default
  && list policy_rule budget original_rules right.rules
  && scalar S.equal_merge budget original_merge right.merge
;;

let approvals budget (left : S.approvals) (right : S.approvals) =
  B.work budget;
  let ({ S.provider = original_provider
       ; unavailable = original_unavailable
       ; scopes = original_scopes
       ; durable = original_durable
       }
        : S.approvals)
    =
    left
  in
  setting (scalar S.equal_approval_provider) budget original_provider right.provider
  && setting
       (scalar S.equal_approval_unavailable)
       budget
       original_unavailable
       right.unavailable
  && list (scalar S.equal_approval_scope) budget original_scopes right.scopes
  && setting bool budget original_durable right.durable
;;

let reviewer budget (left : S.reviewer) (right : S.reviewer) =
  B.work budget;
  match left, right with
  | S.Ui_reviewer { id = a }, Ui_reviewer { id = b } -> string budget a b
  | ( Chatml_reviewer { id = a; script = as_; lifecycle = al; failure = af }
    , Chatml_reviewer { id = b; script = bs; lifecycle = bl; failure = bf } ) ->
    string budget a b
    && string budget as_ bs
    && scalar S.equal_lifecycle budget al bl
    && scalar S.equal_hook_failure budget af bf
  | ( Executable_reviewer { id = a; hook = ah; failure = af }
    , Executable_reviewer { id = b; hook = bh; failure = bf } ) ->
    string budget a b
    && executable_hook budget ah bh
    && scalar S.equal_hook_failure budget af bf
  | ( Model_reviewer { id = a; agent = aa; model = am; failure = af }
    , Model_reviewer { id = b; agent = ba; model = bm; failure = bf } ) ->
    string budget a b
    && string budget aa ba
    && option string budget am bm
    && scalar S.equal_hook_failure budget af bf
  | (Ui_reviewer _ | Chatml_reviewer _ | Executable_reviewer _ | Model_reviewer _), _ ->
    false
;;

let reviewers budget (left : S.reviewers) (right : S.reviewers) =
  B.work budget;
  let ({ S.values = original_values; merge = original_merge } : S.reviewers) = left in
  list reviewer budget original_values right.values
  && scalar S.equal_merge budget original_merge right.merge
;;

let interceptor budget (left : S.interceptor) (right : S.interceptor) =
  B.work budget;
  let ({ S.id = original_id
       ; phase = original_phase
       ; extension = original_extension
       ; matcher = original_matcher
       ; failure = original_failure
       }
        : S.interceptor)
    =
    left
  in
  string budget original_id right.id
  && scalar S.equal_interceptor_phase budget original_phase right.phase
  && extension budget original_extension right.extension
  && option matcher budget original_matcher right.matcher
  && scalar S.equal_hook_failure budget original_failure right.failure
;;

let interceptors budget (left : S.interceptors) (right : S.interceptors) =
  B.work budget;
  let ({ S.values = original_values; merge = original_merge } : S.interceptors) = left in
  list interceptor budget original_values right.values
  && scalar S.equal_merge budget original_merge right.merge
;;

let effect_analyzer budget (left : S.effect_analyzer) (right : S.effect_analyzer) =
  B.work budget;
  let ({ S.id = original_id
       ; extension = original_extension
       ; replace = original_replace
       ; failure = original_failure
       }
        : S.effect_analyzer)
    =
    left
  in
  string budget original_id right.id
  && extension budget original_extension right.extension
  && bool budget original_replace right.replace
  && scalar S.equal_hook_failure budget original_failure right.failure
;;

let effect_analysis budget (left : S.effect_analysis) (right : S.effect_analysis) =
  B.work budget;
  let ({ S.analyzers = original_analyzers; merge = original_merge } : S.effect_analysis) =
    left
  in
  list effect_analyzer budget original_analyzers right.analyzers
  && scalar S.equal_merge budget original_merge right.merge
;;

let secret_source budget (left : S.secret_source) (right : S.secret_source) =
  B.work budget;
  match left, right with
  | S.From_env { name = a; optional = ao }, From_env { name = b; optional = bo } ->
    string budget a b && bool budget ao bo
  | ( From_file { path = a; optional = ao; strip = ast }
    , From_file { path = b; optional = bo; strip = bst } ) ->
    path budget a b && bool budget ao bo && bool budget ast bst
  | Literal a, Literal b -> string budget a b
  | (From_env _ | From_file _ | Literal _), _ -> false
;;

let secrets budget (left : S.secrets) (right : S.secrets) =
  B.work budget;
  let ({ S.replacement = original_replacement
       ; sources = original_sources
       ; merge = original_merge
       }
        : S.secrets)
    =
    left
  in
  setting string budget original_replacement right.replacement
  && list secret_source budget original_sources right.sources
  && scalar S.equal_merge budget original_merge right.merge
;;

let audit budget (left : S.audit) (right : S.audit) =
  B.work budget;
  let ({ S.format = original_format
       ; path = original_path
       ; content = original_content
       ; failure = original_failure
       ; filter = original_filter
       }
        : S.audit)
    =
    left
  in
  scalar S.equal_audit_format budget original_format right.format
  && option path budget original_path right.path
  && scalar S.equal_audit_content budget original_content right.content
  && scalar S.equal_audit_failure budget original_failure right.failure
  && option extension budget original_filter right.filter
;;

let check_field budget ~field equal left right =
  B.work budget;
  if equal budget left right then Ok () else Error field
;;

let check (left : S.t) ~candidate:(right : S.t) ~budget =
  let open Result.Let_syntax in
  let { S.id = original_id
      ; extends = original_extends
      ; requested_profile = original_requested_profile
      ; resolved_profile = original_resolved_profile
      ; cwd = original_cwd
      ; pipefail = original_pipefail
      ; capabilities = original_capabilities
      ; resolver = original_resolver
      ; environment = original_environment
      ; limits = original_limits
      ; backends = original_backends
      ; policy = original_policy
      ; approvals = original_approvals
      ; reviewers = original_reviewers
      ; interceptors = original_interceptors
      ; effect_analysis = original_effect_analysis
      ; secrets = original_secrets
      ; audit = original_audit
      ; source = _
      }
    =
    left
  in
  let check field equal left right = check_field budget ~field equal left right in
  let%bind () =
    check
      "id"
      (fun budget a b ->
         string budget (S.Runtime_id.to_string a) (S.Runtime_id.to_string b))
      original_id
      right.id
  in
  let%bind () = check "extends" (option string) original_extends right.extends in
  let%bind () =
    check
      "requested_profile"
      (option string)
      original_requested_profile
      right.requested_profile
  in
  let%bind () =
    check
      "resolved_profile"
      (option string)
      original_resolved_profile
      right.resolved_profile
  in
  let%bind () = check "cwd" (setting path) original_cwd right.cwd in
  let%bind () = check "pipefail" (setting bool) original_pipefail right.pipefail in
  let%bind () =
    check "capabilities" (option capabilities) original_capabilities right.capabilities
  in
  let%bind () = check "resolver" (option resolver) original_resolver right.resolver in
  let%bind () =
    check "environment" (option environment) original_environment right.environment
  in
  let%bind () = check "limits" (option limits) original_limits right.limits in
  let%bind () = check "backends" (option backends) original_backends right.backends in
  let%bind () = check "policy" (option policy) original_policy right.policy in
  let%bind () = check "approvals" (option approvals) original_approvals right.approvals in
  let%bind () = check "reviewers" (option reviewers) original_reviewers right.reviewers in
  let%bind () =
    check "interceptors" (option interceptors) original_interceptors right.interceptors
  in
  let%bind () =
    check
      "effect_analysis"
      (option effect_analysis)
      original_effect_analysis
      right.effect_analysis
  in
  let%bind () = check "secrets" (option secrets) original_secrets right.secrets in
  let%bind () = check "audit" (option audit) original_audit right.audit in
  Ok ()
;;
