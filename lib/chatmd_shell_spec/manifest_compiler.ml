open! Core
open Jsonaf.Export
module D = Diagnostic
module F = Feature
module M = Manifest
module S = Shell_spec
module T = Shell_tool_spec

type legacy_tool =
  { name : string
  ; description : string option
  ; command : string
  ; source : Source_ref.t
  }

type moderator_runtime =
  { runtime : string
  ; source : Source_ref.t
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type input =
  { runtimes : S.t list
  ; tools : T.t list
  ; scripts : Chatmd_script_spec.t list
  ; legacy_tools : legacy_tool list
  ; moderator_runtime : moderator_runtime option
  ; platform : S.platform
  ; supported_features : F.Set.t
  }

type material =
  { manifest_sha256 : string
  ; runtimes : S.t list
  ; scripts : Chatmd_script_spec.t list
  ; warnings : D.t list
  }

let runtime_id runtime = S.Runtime_id.to_string runtime.S.id
let diagnostic_sort diagnostics = List.dedup_and_sort diagnostics ~compare:D.compare

let qualify_runtime (runtime : S.t) =
  { runtime with
    id = S.Runtime_id.qualify ~namespace:runtime.source.namespace runtime.id
  ; extends = Option.map runtime.extends ~f:(Source_ref.qualify runtime.source)
  }
;;

let qualify_tool (tool : T.t) =
  { tool with runtime = Source_ref.qualify tool.source tool.runtime }
;;

let legacy_tool (tool : legacy_tool) =
  { T.name = tool.name
  ; description = tool.description
  ; runtime = "legacy:custom"
  ; mode =
      Fixed
        { command = Compact tool.command
        ; model_arguments =
            { mode = Optional_arguments
            ; min_count = None
            ; max_count = None
            ; max_item_bytes = None
            }
        }
  ; stdin = No_stdin
  ; rationale = Optional_rationale
  ; result = Combined
  ; stream = Finalized
  ; nonzero = Result
  ; source = tool.source
  }
;;

let add_legacy input =
  match input.legacy_tools with
  | [] -> input.runtimes, input.tools
  | first :: _ ->
    let runtime = Manifest_defaults.legacy_runtime ~source:first.source in
    runtime :: input.runtimes, input.tools @ List.map input.legacy_tools ~f:legacy_tool
;;

let duplicate_diagnostic kind id source =
  D.error
    ~source
    ~path:[ kind; id ]
    ~code:("shell.duplicate_" ^ kind)
    (sprintf "duplicate %s: %s" kind id)
;;

let index_runtimes runtimes =
  List.fold runtimes ~init:(String.Map.empty, []) ~f:(fun (map, errors) runtime ->
    let id = runtime_id runtime in
    match Map.add map ~key:id ~data:runtime with
    | `Ok map -> map, errors
    | `Duplicate -> map, duplicate_diagnostic "runtime" id runtime.source :: errors)
;;

let index_tools tools =
  List.fold tools ~init:(String.Map.empty, []) ~f:(fun (map, errors) tool ->
    match Map.add map ~key:tool.T.name ~data:tool with
    | `Ok map -> map, errors
    | `Duplicate -> map, duplicate_diagnostic "tool" tool.name tool.source :: errors)
;;

let cycle_path path id =
  let rec drop = function
    | [] -> [ id ]
    | head :: tail as path -> if String.equal head id then path @ [ id ] else drop tail
  in
  drop path
;;

let cycle_diagnostic runtime path =
  let cycle = cycle_path path (runtime_id runtime) in
  D.error
    ~source:runtime.S.source
    ~path:[ "runtime"; runtime_id runtime; "extends" ]
    ~code:"shell.inheritance_cycle"
    ("runtime inheritance cycle: " ^ String.concat ~sep:" -> " cycle)
;;

let missing_parent runtime parent =
  D.error
    ~source:runtime.S.source
    ~path:[ "runtime"; runtime_id runtime; "extends" ]
    ~code:"shell.unknown_runtime"
    ("unknown parent runtime: " ^ parent)
;;

let builtin_parent platform runtime parent =
  Builtin_profile.expand
    ~platform
    { requested = parent; runtime_id = runtime.S.id; source = runtime.source }
  |> Result.map_error ~f:List.return
;;

let resolve_runtimes platform runtimes =
  let cache = Hashtbl.create (module String) in
  let rec resolve path runtime =
    let id = runtime_id runtime in
    match Hashtbl.find cache id with
    | Some result -> result
    | None when List.mem path id ~equal:String.equal ->
      Error [ cycle_diagnostic runtime path ]
    | None ->
      let base =
        match runtime.extends with
        | None -> Ok (Manifest_defaults.runtime ~id:runtime.id ~source:runtime.source)
        | Some parent ->
          (match Map.find runtimes parent with
           | None when String.is_prefix parent ~prefix:"builtin:" ->
             builtin_parent platform runtime parent
           | None -> Error [ missing_parent runtime parent ]
           | Some parent_runtime -> resolve (path @ [ id ]) parent_runtime)
      in
      let result =
        Result.bind base ~f:(fun base -> Manifest_merge.runtime ~base runtime)
      in
      Hashtbl.set cache ~key:id ~data:result;
      result
  in
  Map.data runtimes |> List.map ~f:(fun runtime -> resolve [] runtime) |> Result.all
;;

let platform_matches selected declared =
  S.equal_platform declared Any || S.equal_platform selected declared
;;

let backend_kind = function
  | S.Seatbelt _ -> `Confining, F.seatbelt_backend
  | Bubblewrap _ -> `Confining, F.bubblewrap_backend
  | Direct _ -> `Direct, F.direct_backend
  | External { confinement = No_confinement; _ } -> `Direct, F.external_backend
  | External _ -> `Confining, F.external_backend
;;

let backend_platform = function
  | S.Seatbelt { when_; _ }
  | Bubblewrap { when_; _ }
  | Direct { when_; _ }
  | External { when_; _ } -> when_
;;

let matching_backend platform kind backends =
  List.exists backends ~f:(fun backend ->
    platform_matches platform (backend_platform backend)
    && Poly.equal (fst (backend_kind backend)) kind)
;;

let invalid_setting source path =
  D.error
    ~source
    ~path
    ~code:"shell.unresolved_setting"
    "manifest contains an inherited or cleared required setting"
;;

let required_setting source path value errors =
  match value with
  | S.Set _ -> errors
  | Inherit | Clear -> invalid_setting source path :: errors
;;

let validate_capability_backends platform runtime errors =
  let capabilities = Option.value_exn runtime.S.capabilities in
  let backends = Option.value_exn runtime.backends in
  match capabilities.sandbox with
  | Set Required when matching_backend platform `Confining backends.values -> errors
  | Set Preferred when not (List.is_empty backends.values) -> errors
  | Set Direct_unsafe when matching_backend platform `Direct backends.values -> errors
  | Set _ | Inherit | Clear ->
    D.error
      ~source:runtime.source
      ~path:[ "runtime"; runtime_id runtime; "backends" ]
      ~code:"shell.incompatible_backend"
      "runtime sandbox mode has no matching backend for the selected platform"
    :: errors
;;

let rec validate_matcher source path errors = function
  | S.All [] | Any [] ->
    D.error ~source ~path ~code:"shell.empty_matcher" "matcher requires children"
    :: errors
  | All values | Any values ->
    List.fold values ~init:errors ~f:(validate_matcher source path)
  | Not value -> validate_matcher source path errors value
  | Program_regex "" ->
    D.error ~source ~path ~code:"shell.empty_regex" "program regex cannot be empty"
    :: errors
  | Argv_prefix values when List.is_empty values || List.exists values ~f:String.is_empty
    ->
    D.error ~source ~path ~code:"shell.invalid_argv_prefix" "argv prefix cannot be empty"
    :: errors
  | Any_command
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
  | Chatml_match _ -> errors
;;

let validate_policy runtime errors =
  let policy = Option.value_exn runtime.S.policy in
  List.fold policy.rules ~init:errors ~f:(fun errors rule ->
    validate_matcher
      runtime.source
      [ "runtime"; runtime_id runtime; "policy"; rule.id ]
      errors
      rule.matcher)
;;

let validate_runtime_settings runtime errors =
  let capabilities = Option.value_exn runtime.S.capabilities in
  let resolver = Option.value_exn runtime.resolver in
  let environment = Option.value_exn runtime.environment in
  let policy = Option.value_exn runtime.policy in
  let approvals = Option.value_exn runtime.approvals in
  errors
  |> required_setting runtime.source [ "runtime"; runtime_id runtime; "cwd" ] runtime.cwd
  |> required_setting
       runtime.source
       [ "runtime"; runtime_id runtime; "pipefail" ]
       runtime.pipefail
  |> required_setting runtime.source [ "capabilities"; "sandbox" ] capabilities.sandbox
  |> required_setting runtime.source [ "capabilities"; "network" ] capabilities.network
  |> required_setting
       runtime.source
       [ "capabilities"; "child_processes" ]
       capabilities.child_processes
  |> required_setting
       runtime.source
       [ "capabilities"; "arbitrary_code" ]
       capabilities.arbitrary_code
  |> required_setting
       runtime.source
       [ "capabilities"; "privilege_change" ]
       capabilities.privilege_change
  |> required_setting
       runtime.source
       [ "resolver"; "allow_relative_search_path" ]
       resolver.allow_relative_search_path
  |> required_setting runtime.source [ "environment"; "inherit" ] environment.inherit_
  |> required_setting runtime.source [ "policy"; "default" ] policy.default
  |> required_setting runtime.source [ "approvals"; "provider" ] approvals.provider
  |> required_setting runtime.source [ "approvals"; "unavailable" ] approvals.unavailable
  |> required_setting runtime.source [ "approvals"; "durable" ] approvals.durable
;;

let validate_external_backend runtime errors backend =
  match backend with
  | S.External { atoms; confinement; _ } ->
    let count = List.count atoms ~f:(function S.Command_argv_atom -> true | _ -> false) in
    let errors =
      if Int.equal count 1
      then errors
      else
        D.error
          ~source:runtime.S.source
          ~path:[ "runtime"; runtime_id runtime; "backends" ]
          ~code:"shell.external_backend_command_argv"
          "external backend requires exactly one command_argv element"
        :: errors
    in
    if S.equal_confinement confinement S.Verified_confinement
    then
      D.error
        ~source:runtime.source
        ~path:[ "runtime"; runtime_id runtime; "backends" ]
        ~code:"shell.external_backend_unverifiable"
        "custom external backends cannot declare verified confinement"
      :: errors
    else errors
  | Seatbelt _ | Bubblewrap _ | Direct _ -> errors
;;

let validate_external_backends runtime errors =
  let backends = Option.value_exn runtime.S.backends in
  List.fold backends.values ~init:errors ~f:(validate_external_backend runtime)
;;

let validate_runtime platform runtime =
  []
  |> validate_runtime_settings runtime
  |> validate_capability_backends platform runtime
  |> validate_policy runtime
  |> validate_external_backends runtime
;;

let executable_ids runtime =
  let resolver : S.resolver = Option.value_exn runtime.S.resolver in
  String.Set.of_list
    (List.map resolver.executables ~f:(fun (value : S.executable) -> value.id))
;;

let validate_arguments source name arguments errors =
  match arguments.T.min_count, arguments.max_count with
  | Some minimum, Some maximum when minimum > maximum ->
    D.error
      ~source
      ~path:[ "tool"; name; "arguments" ]
      ~code:"shell.invalid_argument_bounds"
      "minimum argument count exceeds maximum argument count"
    :: errors
  | _ -> errors
;;

let validate_tool runtime_map tool =
  match Map.find runtime_map tool.T.runtime with
  | None ->
    [ D.error
        ~source:tool.source
        ~path:[ "tool"; tool.name; "runtime" ]
        ~code:"shell.unknown_runtime"
        ("unknown tool runtime: " ^ tool.runtime)
    ]
  | Some runtime ->
    (match tool.mode with
     | Fixed { command = Argv { executable_ref = Some id; _ }; model_arguments } ->
       let errors = validate_arguments tool.source tool.name model_arguments [] in
       if Set.mem (executable_ids runtime) id
       then errors
       else
         D.error
           ~source:tool.source
           ~path:[ "tool"; tool.name; "executable_ref" ]
           ~code:"shell.unknown_executable"
           ("unknown executable reference: " ^ id)
         :: errors
     | Fixed { model_arguments; _ } ->
       validate_arguments tool.source tool.name model_arguments []
     | Structured | Chain | Raw _ | Script_file _ -> [])
;;

let tool_feature = function
  | T.Fixed _ -> F.fixed_tool
  | Structured -> F.structured_tool
  | Chain -> F.chain_tool
  | Raw _ -> F.raw_tool
  | Script_file _ -> F.script_tool
;;

let runtime_features platform runtime =
  let backend_features =
    let backends : S.backends = Option.value_exn runtime.S.backends in
    List.filter_map backends.values ~f:(fun value ->
      Option.some_if
        (platform_matches platform (backend_platform value))
        (snd (backend_kind value)))
  in
  let limit_features =
    let limits : S.limits = Option.value_exn runtime.limits in
    let cpu_limit =
      match limits.cpu_time with
      | Set (Some _) -> true
      | Set None | Inherit | Clear -> false
    in
    let has_limit = function
      | S.Set _ -> true
      | Inherit | Clear -> false
    in
    if
      cpu_limit
      || has_limit limits.memory
      || has_limit limits.file_size
      || has_limit limits.open_files
    then [ F.resource_limits ]
    else []
  in
  let secret_features =
    let secrets : S.secrets = Option.value_exn runtime.secrets in
    if
      List.exists secrets.sources ~f:(function
        | S.Literal _ -> true
        | _ -> false)
    then [ F.literal_secrets ]
    else []
  in
  backend_features @ limit_features @ secret_features
;;

let script_feature (script : Chatmd_script_spec.t) =
  match script.kind with
  | Shell_matcher -> F.chatml_matcher
  | Shell_reviewer -> F.chatml_reviewer
  | Shell_before_interceptor -> F.chatml_before_interceptor
  | Shell_after_interceptor -> F.chatml_after_interceptor
  | Shell_effect_analyzer -> F.chatml_effect_analyzer
  | Shell_audit_filter -> F.chatml_audit_filter
  | Moderator -> ""
;;

let has_model_reviewer runtime =
  Option.exists runtime.S.reviewers ~f:(fun reviewers ->
    List.exists reviewers.values ~f:(function
      | S.Model_reviewer _ -> true
      | Ui_reviewer _ | Chatml_reviewer _ | Executable_reviewer _ -> false))
;;

let has_executable_extension runtime =
  let is_executable = function
    | S.Executable_extension _ -> true
    | Chatml_extension _ -> false
  in
  Option.exists runtime.S.reviewers ~f:(fun values ->
    List.exists values.values ~f:(function
      | S.Executable_reviewer _ -> true
      | Ui_reviewer _ | Chatml_reviewer _ | Model_reviewer _ -> false))
  || Option.exists runtime.interceptors ~f:(fun values ->
    List.exists values.values ~f:(fun value -> is_executable value.S.extension))
  || Option.exists runtime.effect_analysis ~f:(fun values ->
    List.exists values.analyzers ~f:(fun value -> is_executable value.S.extension))
  || Option.exists runtime.audit ~f:(fun value -> Option.exists value.filter ~f:is_executable)
;;

let required_features platform runtimes tools scripts =
  List.concat_map runtimes ~f:(runtime_features platform)
  @ List.map tools ~f:(fun tool -> tool_feature tool.T.mode)
  @ List.filter_map scripts ~f:(fun script ->
    let feature = script_feature script in
    Option.some_if (not (String.is_empty feature)) feature)
  @ Option.to_list
      (Option.some_if (List.exists runtimes ~f:has_model_reviewer) F.model_reviewer)
  @ Option.to_list
      (Option.some_if (List.exists runtimes ~f:has_executable_extension) F.executable_hooks)
  |> String.Set.of_list
;;

let rec matcher_references path = function
  | S.Chatml_match matcher -> [ path, matcher.script, Chatmd_script_spec.Shell_matcher ]
  | All values | Any values -> List.concat_map values ~f:(matcher_references path)
  | Not value -> matcher_references path value
  | Any_command
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
  | Raw_shell_request -> []
;;

let reviewer_references runtime = function
  | S.Chatml_reviewer { id; script; _ } ->
    [ ( [ "runtime"; runtime_id runtime; "reviewers"; id ]
      , script
      , Chatmd_script_spec.Shell_reviewer )
    ]
  | Ui_reviewer _ | Model_reviewer _ | Executable_reviewer _ -> []
;;

let interceptor_reference runtime (value : S.interceptor) =
  let kind =
    match value.phase with
    | S.Before -> Chatmd_script_spec.Shell_before_interceptor
    | After -> Shell_after_interceptor
  in
  match value.extension with
  | S.Chatml_extension { script; _ } ->
    [ [ "runtime"; runtime_id runtime; "interceptors"; value.id ], script, kind ]
  | Executable_extension _ -> []
;;

let analyzer_reference runtime (value : S.effect_analyzer) =
  match value.extension with
  | S.Chatml_extension { script; _ } ->
    [ ( [ "runtime"; runtime_id runtime; "effect_analysis"; value.id ]
      , script
      , Chatmd_script_spec.Shell_effect_analyzer )
    ]
  | Executable_extension _ -> []
;;

let runtime_references runtime =
  let policy = Option.value_exn runtime.S.policy in
  let rules =
    List.concat_map policy.rules ~f:(fun rule ->
      matcher_references [ "runtime"; runtime_id runtime; "policy"; rule.id ] rule.matcher)
  in
  let reviewers =
    Option.value_map runtime.reviewers ~default:[] ~f:(fun value ->
      List.concat_map value.values ~f:(reviewer_references runtime))
  in
  let interceptors =
    Option.value_map runtime.interceptors ~default:[] ~f:(fun value ->
      List.concat_map value.values ~f:(interceptor_reference runtime))
  in
  let analyzers =
    Option.value_map runtime.effect_analysis ~default:[] ~f:(fun value ->
      List.concat_map value.analyzers ~f:(analyzer_reference runtime))
  in
  let audit = Option.value_exn runtime.audit in
  let filters =
    Option.value_map audit.filter ~default:[] ~f:(function
      | S.Chatml_extension { script; _ } ->
        [ ( [ "runtime"; runtime_id runtime; "audit"; "filter" ]
          , script
          , Chatmd_script_spec.Shell_audit_filter )
        ]
      | Executable_extension _ -> [])
  in
  rules @ reviewers @ interceptors @ analyzers @ filters
;;

type worker_reference =
  { caller : string
  ; hook_id : string
  ; worker : string
  ; source : Source_ref.t
  ; path : string list
  }

let executable_worker runtime hook_id path hook =
  { caller = runtime_id runtime
  ; hook_id
  ; worker = hook.S.runtime
  ; source = runtime.source
  ; path
  }
;;

let extension_worker runtime hook_id path = function
  | S.Chatml_extension _ -> []
  | Executable_extension hook -> [ executable_worker runtime hook_id path hook ]
;;

let worker_references runtime =
  let prefix = [ "runtime"; runtime_id runtime ] in
  let reviewers =
    Option.value_map runtime.S.reviewers ~default:[] ~f:(fun values ->
      List.filter_map values.values ~f:(function
        | S.Executable_reviewer { id; hook; _ } ->
          Some (executable_worker runtime id (prefix @ [ "reviewers"; id ]) hook)
        | Ui_reviewer _ | Chatml_reviewer _ | Model_reviewer _ -> None))
  in
  let interceptors =
    Option.value_map runtime.interceptors ~default:[] ~f:(fun values ->
      List.concat_map values.values ~f:(fun value ->
        extension_worker runtime value.id (prefix @ [ "interceptors"; value.id ]) value.extension))
  in
  let analyzers =
    Option.value_map runtime.effect_analysis ~default:[] ~f:(fun values ->
      List.concat_map values.analyzers ~f:(fun value ->
        extension_worker runtime value.id (prefix @ [ "effect_analysis"; value.id ]) value.extension))
  in
  let filters =
    Option.value_map (Option.value_exn runtime.audit).filter ~default:[] ~f:(fun extension ->
      extension_worker runtime "audit-filter" (prefix @ [ "audit"; "filter" ]) extension)
  in
  reviewers @ interceptors @ analyzers @ filters
;;

let validate_worker_reference runtime_map reference =
  if Map.mem runtime_map reference.worker
  then []
  else
    [ D.error
        ~source:reference.source
        ~path:reference.path
        ~code:"shell.unknown_worker_runtime"
        ("unknown worker runtime: " ^ reference.worker)
    ]
;;

let worker_cycle_diagnostic references cycle =
  let first = List.hd_exn cycle in
  let source =
    List.find_map references ~f:(fun reference ->
      Option.some_if (String.equal reference.caller first) reference.source)
  in
  D.error
    ?source
    ~path:[ "dependencies" ]
    ~code:"shell.runtime_dependency_cycle"
    ("runtime dependency cycle: " ^ String.concat ~sep:" -> " cycle)
;;

let worker_cycles references =
  let edges =
    List.fold references ~init:String.Map.empty ~f:(fun map reference ->
      Map.update map reference.caller ~f:(fun values ->
        reference.worker :: Option.value values ~default:[]))
  in
  let rec visit path seen node =
    if List.mem path node ~equal:String.equal
    then Error (cycle_path path node)
    else if Set.mem seen node
    then Ok seen
    else
      List.fold_result
        (Map.find edges node |> Option.value ~default:[])
        ~init:(Set.add seen node)
        ~f:(visit (path @ [ node ]))
  in
  Map.keys edges
  |> List.fold_until ~init:String.Set.empty ~f:(fun seen node ->
    match visit [] seen node with
    | Ok seen -> Continue seen
    | Error cycle -> Stop [ worker_cycle_diagnostic references cycle ])
       ~finish:(fun _ -> [])
;;

let dependencies input runtimes tools =
  let tool_edges =
    List.map tools ~f:(fun tool ->
      M.{ from_id = "tool:" ^ tool.T.name; to_id = tool.runtime; kind = Tool_runtime })
  in
  let moderator_edges =
    Option.to_list
      (Option.map input.moderator_runtime ~f:(fun moderator ->
         M.
           { from_id = "moderator"
           ; to_id = Source_ref.qualify moderator.source moderator.runtime
           ; kind = Moderator_runtime
           }))
  in
  let worker_edges =
    List.concat_map runtimes ~f:worker_references
    |> List.map ~f:(fun reference ->
      M.{ from_id = reference.caller; to_id = reference.worker; kind = Worker_runtime })
  in
  let script_edges =
    List.concat_map runtimes ~f:(fun runtime ->
      List.map (runtime_references runtime) ~f:(fun (_, script, _) ->
        M.{ from_id = runtime_id runtime; to_id = "script:" ^ script; kind = Chatml_extension }))
  in
  List.dedup_and_sort
    (tool_edges @ moderator_edges @ worker_edges @ script_edges)
    ~compare:M.compare_dependency
;;

let index_scripts scripts =
  List.fold scripts ~init:(String.Map.empty, []) ~f:(fun (map, errors) script ->
    match Map.add map ~key:script.Chatmd_script_spec.id ~data:script with
    | `Ok map -> map, errors
    | `Duplicate ->
      ( map
      , duplicate_diagnostic "script" script.Chatmd_script_spec.id script.source_ref
        :: errors ))
;;

let validate_script_reference scripts (path, id, expected) =
  match Map.find scripts id with
  | None ->
    Error (D.error ~path ~code:"shell.unknown_script" ("unknown ChatML script: " ^ id))
  | Some script when Chatmd_script_spec.equal_kind script.Chatmd_script_spec.kind expected
    -> Ok script
  | Some script ->
    Error
      (D.error
         ~source:script.Chatmd_script_spec.source_ref
         ~path
         ~code:"shell.script_kind_mismatch"
         (sprintf
            "script %s has kind %s, expected %s"
            id
            (Chatmd_script_spec.kind_to_string script.Chatmd_script_spec.kind)
            (Chatmd_script_spec.kind_to_string expected)))
;;

let extension_entrypoint = function
  | Chatmd_script_spec.Shell_matcher -> "match_command"
  | Shell_reviewer -> "review"
  | Shell_before_interceptor -> "before"
  | Shell_after_interceptor -> "after"
  | Shell_effect_analyzer -> "analyze"
  | Shell_audit_filter -> "filter"
  | Moderator -> "on_event"
;;

let extension_script (script : Chatmd_script_spec.t) : M.extension_script =
  { id = script.Chatmd_script_spec.id
  ; kind = script.kind
  ; source_sha256 = script.source_sha256
  ; surface_version = "ochat.shell.chatml.surface.v1"
  ; entrypoint = extension_entrypoint script.kind
  ; limits = script.limits
  }
;;

let resolve_scripts runtimes scripts =
  List.concat_map runtimes ~f:runtime_references
  |> List.map ~f:(validate_script_reference scripts)
  |> Result.all
  |> Result.map ~f:(fun scripts ->
    List.dedup_and_sort scripts ~compare:(fun left right ->
      String.compare left.Chatmd_script_spec.id right.Chatmd_script_spec.id))
;;

let unsupported_features input required =
  Set.diff required input.supported_features
  |> Set.to_list
  |> List.map ~f:(fun feature ->
    D.error
      ~code:"shell.unsupported_feature"
      ~path:[ "features"; feature ]
      ("runtime does not support required feature: " ^ feature))
;;

let sanitize_secret = function
  | S.Literal _ -> S.Literal "<literal-secret>"
  | (From_env _ | From_file _) as source -> source
;;

let sanitize_runtime runtime =
  let secrets =
    Option.map runtime.S.secrets ~f:(fun secrets ->
      { secrets with sources = List.map secrets.sources ~f:sanitize_secret })
  in
  { runtime with secrets }
;;

let moderator_reference input =
  Option.map input.moderator_runtime ~f:(fun moderator ->
    Source_ref.qualify moderator.source moderator.runtime)
;;

let validate_moderator runtime_map input =
  match moderator_reference input with
  | None -> []
  | Some runtime when Map.mem runtime_map runtime -> []
  | Some runtime ->
    [ D.error
        ~code:"shell.unknown_runtime"
        ~path:[ "moderator_runtime" ]
        ("unknown moderator runtime: " ^ runtime)
    ]
;;

let manifest input runtimes tools scripts dependencies required =
  let payload : M.payload =
    { encoding_version = "ochat.shell.manifest.v1"
    ; platform = input.platform
    ; runtimes = List.map runtimes ~f:sanitize_runtime
    ; tools
    ; moderator_runtime = moderator_reference input
    ; extension_scripts = List.map scripts ~f:extension_script
    ; dependencies
    ; required_features = Set.to_list required
    }
  in
  M.create payload
;;

let unused_script_warnings script_map scripts =
  let used = String.Set.of_list (List.map scripts ~f:(fun script -> script.Chatmd_script_spec.id)) in
  Map.data script_map
  |> List.filter ~f:(fun script -> not (Set.mem used script.Chatmd_script_spec.id))
  |> List.map ~f:(fun script ->
    D.warning
      ~source:script.source_ref
      ~path:[ "script"; script.id ]
      ~code:"chatmd.unused_script"
      ("unused ChatML script is not executable: " ^ script.id))
;;

let compile_with_material input =
  let runtimes, tools = add_legacy input in
  let runtimes = List.map runtimes ~f:qualify_runtime in
  let tools = List.map tools ~f:qualify_tool in
  let runtime_map, runtime_duplicates = index_runtimes runtimes in
  let _, tool_duplicates = index_tools tools in
  let script_map, script_duplicates = index_scripts input.scripts in
  match diagnostic_sort (runtime_duplicates @ tool_duplicates @ script_duplicates) with
  | _ :: _ as diagnostics -> Error diagnostics
  | [] ->
    (match resolve_runtimes input.platform runtime_map with
     | Error diagnostics -> Error (diagnostic_sort diagnostics)
     | Ok runtimes ->
       let runtimes =
         List.sort runtimes ~compare:(fun left right ->
           String.compare (runtime_id left) (runtime_id right))
       in
       let runtime_map =
         String.Map.of_alist_exn
           (List.map runtimes ~f:(fun runtime -> runtime_id runtime, runtime))
       in
       let tools =
         List.sort tools ~compare:(fun left right ->
           String.compare left.T.name right.name)
       in
       let validation =
         List.concat_map runtimes ~f:(validate_runtime input.platform)
         @ List.concat_map tools ~f:(validate_tool runtime_map)
         @ validate_moderator runtime_map input
         @ (let references = List.concat_map runtimes ~f:worker_references in
            List.concat_map references ~f:(validate_worker_reference runtime_map)
            @ worker_cycles references)
       in
       (match resolve_scripts runtimes script_map with
        | Error diagnostic -> Error (diagnostic_sort (diagnostic :: validation))
        | Ok scripts ->
          let required = required_features input.platform runtimes tools scripts in
          let diagnostics =
            diagnostic_sort (validation @ unsupported_features input required)
          in
          if List.is_empty diagnostics
          then (
            let dependencies = dependencies input runtimes tools in
            let manifest = manifest input runtimes tools scripts dependencies required in
            let warnings = unused_script_warnings script_map scripts in
            Ok (manifest, { manifest_sha256 = manifest.sha256; runtimes; scripts; warnings }))
          else Error diagnostics))
;;

let compile input = Result.map (compile_with_material input) ~f:fst

let effective_runtimes material ~manifest =
  if String.equal material.manifest_sha256 manifest.M.sha256
  then Ok material.runtimes
  else
    Error
      [ D.error
          ~code:"shell.manifest_material_mismatch"
          "live runtime material does not match the canonical manifest"
      ]
;;

let effective_scripts material ~manifest =
  if String.equal material.manifest_sha256 manifest.M.sha256
  then Ok material.scripts
  else
    Error
      [ D.error
          ~code:"shell.manifest_material_mismatch"
          "live script material does not match the canonical manifest"
      ]
;;

let warnings material = material.warnings
