open! Core
module M = Chatmd_shell_spec.Manifest
module S = Chatmd_shell_spec.Shell_spec

type executable =
  { path : string
  ; sha256 : string option
  ; trusted : bool
  }
[@@deriving sexp, compare, equal]

type model_completion =
  agent:string -> model:string option -> Model_reviewer.complete option

type native_worker =
  { id : string
  ; kind : Hook_protocol.kind
  ; worker : Hook_worker.t
  }

type model_service =
  { agent : string
  ; model : string option
  ; complete : Model_reviewer.complete option
  }

type resources =
  { audit_sink : Shell_access.Audit.t
  ; approval_provider : Approval_broker.provider
  ; native_workers : native_worker list
  ; model_services : model_service list option
  }

type t =
  { spec : S.t
  ; executor_config : Shell_access.Executor.config
  ; secret_filter : Shell_access.Secret_filter.t
  ; max_stdin_bytes : int
  ; executables : executable String.Map.t
  ; resolver : Shell_access.Resolver.t
  ; cwd : string
  ; environment : string array
  ; host : Host.t
  ; resources : resources
  }

type file_identity =
  { path : string
  ; verification_path : string
  ; sha256 : string
  ; size : int
  }
[@@deriving sexp, compare, equal]

type error =
  { runtime_id : string
  ; code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

exception Runtime_error of error

let runtime_id specification = S.Runtime_id.to_string specification.S.id

let fail specification code message =
  raise_notrace (Runtime_error { runtime_id = runtime_id specification; code; message })
;;

let value specification name = function
  | S.Set value -> value
  | Inherit | Clear ->
    fail specification "shell.unresolved_setting" (name ^ " is unresolved")
;;

let host_result specification = function
  | Ok value -> value
  | Error (error : Host.error) -> fail specification error.code error.message
;;

let environment_result specification = function
  | Ok value -> value
  | Error (error : Environment.error) -> fail specification error.code error.message
;;

let lowering_result specification = function
  | Ok value -> value
  | Error (error : Lowering.error) -> fail specification error.code error.message
;;

let extension_result specification = function
  | Ok value -> value
  | Error diagnostic ->
    fail specification diagnostic.Chatmd_shell_spec.Diagnostic.code diagnostic.message
;;

let extension_instance_key specification id = function
  | S.Invocation -> None
  | Runtime -> Some ("runtime\000" ^ id)
  | Session -> Some ("session\000" ^ runtime_id specification ^ "\000" ^ id)
;;

module Extension_owner = struct
  type runtime = t

  type t =
    { prepare :
        lifecycle:S.lifecycle
        -> Chatml_extension.compiled
        -> (Chatml_extension.instance, Chatmd_shell_spec.Diagnostic.t) result
    ; register :
        runtime_id:string -> lifecycle:S.lifecycle -> Chatml_extension.instance -> unit
    ; worker :
        S.t
        -> Host.t
        -> runtime String.Map.t
        -> Shell_access.Secret_filter.t
        -> string
        -> Hook_protocol.kind
        -> S.executable_hook
        -> Hook_worker.t
    ; poll : unit -> unit
    ; lookup :
        Chatml_extension.compiled String.Map.t
        -> string
        -> Chatml_extension.compiled option
    }
end

let find_extension specification owner extensions id expected =
  match owner.Extension_owner.lookup extensions id with
  | None -> fail specification "shell.unknown_script" ("unknown ChatML script: " ^ id)
  | Some compiled
    when Chatmd_shell_spec.Chatmd_script_spec.equal_kind
           (Chatml_extension.kind compiled)
           expected -> compiled
  | Some _ ->
    fail specification "shell.script_kind_mismatch" ("wrong ChatML script kind: " ^ id)
;;

let extension_instance
      specification
      _host
      extensions
      runtime_instances
      register_extension_instance
      id
      kind
      lifecycle
  =
  register_extension_instance.Extension_owner.poll ();
  let create () =
    let instance =
      find_extension specification register_extension_instance extensions id kind
      |> register_extension_instance.Extension_owner.prepare ~lifecycle
      |> extension_result specification
    in
    register_extension_instance.Extension_owner.register
      ~runtime_id:(runtime_id specification)
      ~lifecycle
      instance;
    instance
  in
  (* Admit the bounded library copying/hashing passes before constructing and
     looking up this cache key. The enclosing construction ledger owns [poll]. *)
  let admit value =
    ignore
      (Registry_code_budget.equal_string_with_checkpoint
         ~checkpoint:register_extension_instance.Extension_owner.poll
         value
         value
       : bool)
  in
  admit id;
  admit (runtime_id specification);
  admit id;
  admit (runtime_id specification);
  match extension_instance_key specification id lifecycle with
  | None -> create ()
  | Some key -> Hashtbl.find_or_add runtime_instances key ~default:create
;;

let extension_call instance ~phase context =
  let value =
    Chatml_context_value.of_context context
    |> fun value ->
    Chatml_context_value.with_phase value phase |> Chatml_context_value.encode
  in
  Chatml_extension.call instance ~context:value ~event:value
;;

let matcher_failure action failure =
  match failure with
  | S.Conservative_failure -> not (S.equal_policy_action action S.Allow)
  | S.No_match_failure | Keep_failure | Unknown_failure -> false
  | Deny_failure | Error_failure -> S.equal_policy_action action S.Deny
;;

let chatml_matcher
      specification
      _host
      extensions
      owner
      ~action
      (matcher : S.chatml_matcher)
  =
  owner.Extension_owner.poll ();
  let instance =
    let compiled =
      find_extension
        specification
        owner
        extensions
        matcher.S.script
        Chatmd_shell_spec.Chatmd_script_spec.Shell_matcher
    in
    let compiled =
      if String.equal matcher.function_ "match_command"
      then compiled
      else
        Chatml_extension.with_entrypoint compiled matcher.function_
        |> extension_result specification
    in
    let instance =
      owner.Extension_owner.prepare ~lifecycle:Invocation compiled
      |> extension_result specification
    in
    owner.register ~runtime_id:(runtime_id specification) ~lifecycle:Invocation instance;
    instance
  in
  Shell_access.Matcher.custom (fun context ->
    match extension_call instance ~phase:"shell_matcher" context with
    | Ok (Chatml_extension.Match (matched, _)) -> matched
    | Ok _ | Error _ -> matcher_failure action matcher.failure)
;;

let effect_of_string = function
  | "network" -> Shell_access.Effect.Network
  | "child_processes" -> Child_processes
  | "arbitrary_code" -> Arbitrary_code
  | "privilege_change" -> Privilege_change
  | value when String.is_prefix value ~prefix:"read:" ->
    Read_path (String.drop_prefix value 5)
  | value when String.is_prefix value ~prefix:"write:" ->
    Write_path (String.drop_prefix value 6)
  | value -> Unknown value
;;

let worker_runtime specification worker_runtimes id =
  Map.find worker_runtimes id
  |> Option.value_or_thunk ~default:(fun () ->
    fail
      specification
      "shell.worker_runtime_unavailable"
      ("worker runtime is unavailable: " ^ id))
;;

let hook_limit default = function
  | None -> default
  | Some value ->
    Chatmd_shell_spec.Duration.bytes_to_int64 value
    |> Int64.min (Int64.of_int Int.max_value)
    |> Int64.to_int_exn
;;

let resolve_hook_executable specification host worker hook =
  let path =
    host_result
      specification
      (Host.resolve_path host ~source:specification.S.source hook.S.executable)
    |> Eio.Path.native_exn
  in
  let command = Shell_access.Command.create path [] in
  match
    Shell_access.Resolver.resolve
      worker.resolver
      ~fs:(Eio.Stdenv.fs worker.host.Host.env)
      ~cwd:worker.cwd
      ~environment:worker.environment
      command
  with
  | Error message -> fail specification "shell.hook_executable_unavailable" message
  | Ok executable ->
    (match hook.sha256 with
     | Some expected
       when not (String.Caseless.equal expected executable.fingerprint.sha256) ->
       fail
         specification
         "shell.hook_digest_mismatch"
         "hook executable digest does not match"
     | Some _ | None -> executable)
;;

let create_hook_worker specification host worker_runtimes secret_filter id kind hook =
  let worker = worker_runtime specification worker_runtimes hook.S.runtime in
  let executable = resolve_hook_executable specification host worker hook in
  Hook_worker.create
    ~env:host.Host.env
    ~hook_id:id
    ~kind
    ~executable
    ~executor_config:worker.executor_config
    ?timeout_seconds:(Option.map hook.timeout ~f:Chatmd_shell_spec.Duration.to_seconds)
    ~max_input_bytes:(hook_limit 1_048_576 hook.max_input)
    ~max_output_bytes:(hook_limit 262_144 hook.max_output)
    ~redact:(Shell_access.Secret_filter.redact secret_filter)
    ()
;;

let hook_worker specification host worker_runtimes secret_filter owner id kind hook =
  owner.Extension_owner.poll ();
  owner.worker specification host worker_runtimes secret_filter id kind hook
;;

let chatml_analyzer
      specification
      host
      extensions
      runtime_instances
      register
      (value : S.effect_analyzer)
      script
      lifecycle
  =
  let instance =
    extension_instance
      specification
      host
      extensions
      runtime_instances
      register
      script
      Chatmd_shell_spec.Chatmd_script_spec.Shell_effect_analyzer
      lifecycle
  in
  fun context ->
    let failed message =
      match value.failure with
      | S.Keep_failure | No_match_failure -> Ok (Shell_access.Analyzer.Add [])
      | Conservative_failure | Deny_failure | Error_failure | Unknown_failure ->
        Error message
    in
    match extension_call instance ~phase:"shell_effect" context with
    | Ok (Chatml_extension.Effect_add value) ->
      Ok (Shell_access.Analyzer.Add [ effect_of_string value ])
    | Ok (Effect_replace values) ->
      let effects = List.map values ~f:effect_of_string in
      if value.replace
      then Ok (Shell_access.Analyzer.Replace effects)
      else failed "effect analyzer returned replace without replace=true"
    | Ok _ -> failed "effect analyzer returned the wrong action"
    | Error diagnostic -> failed diagnostic.message
;;

let executable_analyzer
      specification
      host
      worker_runtimes
      secret_filter
      register
      (value : S.effect_analyzer)
      hook
  =
  let worker =
    hook_worker
      specification
      host
      worker_runtimes
      secret_filter
      register
      value.S.id
      Hook_protocol.Effect_analyzer
      hook
  in
  fun context ->
    match Executable_analyzer.analyze worker context with
    | Ok (Shell_access.Analyzer.Replace _) when not value.replace ->
      Error "effect analyzer returned replace without replace=true"
    | Ok result -> Ok result
    | Error error -> Error error.message
;;

let analyzer
      specification
      host
      extensions
      runtime_instances
      register
      worker_runtimes
      secret_filter
      (value : S.effect_analyzer)
  =
  register.Extension_owner.poll ();
  match value.S.extension with
  | Chatml_extension { script; lifecycle } ->
    chatml_analyzer
      specification
      host
      extensions
      runtime_instances
      register
      value
      script
      lifecycle
  | Executable_extension hook ->
    executable_analyzer
      specification
      host
      worker_runtimes
      secret_filter
      register
      value
      hook
;;

let analyzers
      specification
      host
      extensions
      runtime_instances
      register
      worker_runtimes
      secret_filter
  =
  Option.value_map specification.S.effect_analysis ~default:[] ~f:(fun value ->
    List.map
      value.analyzers
      ~f:
        (analyzer
           specification
           host
           extensions
           runtime_instances
           register
           worker_runtimes
           secret_filter))
;;

let digest_path path =
  Eio.Path.load path |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
;;

let executable specification host (value : S.executable) =
  let path =
    host_result
      specification
      (Host.resolve_path host ~source:specification.source value.path)
  in
  if not (Eio.Path.is_file path)
  then
    fail
      specification
      "shell.executable_unavailable"
      ("executable is unavailable: " ^ value.id);
  let actual_sha256 = digest_path path in
  (match value.sha256 with
   | Some expected when not (String.Caseless.equal expected actual_sha256) ->
     fail
       specification
       "shell.executable_digest_mismatch"
       ("executable digest mismatch: " ^ value.id)
   | Some _ | None -> ());
  ( value.id
  , { path = Eio.Path.native_exn path
    ; sha256 = Some actual_sha256
    ; trusted = value.trusted
    } )
;;

let executables specification host =
  let resolver = Option.value_exn specification.S.resolver in
  List.map resolver.executables ~f:(executable specification host)
  |> String.Map.of_alist_exn
;;

let replacement specification =
  let secrets = Option.value_exn specification.S.secrets in
  value specification "secrets.replacement" secrets.replacement
;;

let secret_filter specification environment secret_values =
  Shell_access.Secret_filter.create
    ~replacement:(replacement specification)
    (environment.Environment.secrets @ secret_values)
;;

let approval_provider specification configured =
  let approvals = Option.value_exn specification.S.approvals in
  match value specification "approvals.provider" approvals.provider with
  | S.No_provider -> Approval_broker.None_available
  | Ui -> configured
;;

let ui_reviewer specification configured manifest =
  let approvals = Option.value_exn specification.S.approvals in
  let reviewer provider =
    Approval_broker.reviewer
      (approval_provider specification provider)
      ~runtime_id:(runtime_id specification)
      ~manifest_sha256:manifest.M.sha256
      ~scopes:approvals.scopes
  in
  reviewer configured
  |> Option.map ~f:(fun original request ->
    match Call_context.current () with
    | Error message -> Shell_access.Approval.Deny message
    | Ok None -> original request
    | Ok (Some context) ->
      (match request.Shell_access.Approval.context.session_id with
       | Some session_id when String.equal session_id context.session_id ->
         (match reviewer context.approval_provider with
          | Some current -> current request
          | None -> Deny "shell approval provider is unavailable for the invoking session")
       | _ -> Deny "shell approval request does not belong to the invoking session"))
;;

let command_of_argv = function
  | [] -> Error "rewritten argv cannot be empty"
  | program :: arguments -> Ok (Shell_access.Command.create program arguments)
;;

let approval_scope request = function
  | "once" -> Ok Shell_access.Approval.Once
  | "exact_session" -> Ok (Exact_session { expires_at = None })
  | "prefix_session" ->
    Ok
      (Prefix_session
         { prefix =
             Shell_access.Command.to_argv request.Shell_access.Approval.context.command
         ; expires_at = None
         })
  | "durable_exact" -> Ok (Durable_exact { expires_at = None })
  | value -> Error ("unknown approval scope: " ^ value)
;;

type review_result =
  | Terminal of Shell_access.Approval.review
  | Defer

let review response = Shell_access.Approval.{ response; metadata = None }

let chatml_review instance request =
  let value = Chatml_approval_value.encode_request request in
  match Chatml_extension.call instance ~context:value ~event:value with
  | Ok Chatml_extension.Review_approve -> Ok (Terminal (review Approve))
  | Ok (Review_approve_for scope) ->
    Result.map (approval_scope request scope) ~f:(fun scope ->
      Terminal (review (Approve_for scope)))
  | Ok (Review_deny reason) -> Ok (Terminal (review (Deny reason)))
  | Ok (Review_rewrite argv) ->
    Result.map (command_of_argv argv) ~f:(fun command ->
      Terminal (review (Rewrite command)))
  | Ok Review_defer -> Ok Defer
  | Ok _ -> Error "reviewer returned the wrong action"
  | Error diagnostic -> Error diagnostic.message
;;

let reviewer_failure failure message =
  match failure with
  | S.Keep_failure | No_match_failure -> Ok Defer
  | Conservative_failure | Deny_failure | Error_failure | Unknown_failure -> Error message
;;

let reviewer_entry
      specification
      host
      extensions
      runtime_instances
      register
      worker_runtimes
      secret_filter
      configured
      manifest
      model_completion
  = function
  | S.Ui_reviewer _ ->
    (match ui_reviewer specification configured manifest with
     | None -> fun _ -> Error "UI approval is unavailable"
     | Some reviewer -> fun request -> Ok (Terminal (review (reviewer request))))
  | Chatml_reviewer { script; lifecycle; failure; _ } ->
    let instance =
      extension_instance
        specification
        host
        extensions
        runtime_instances
        register
        script
        Chatmd_shell_spec.Chatmd_script_spec.Shell_reviewer
        lifecycle
    in
    fun request ->
      (match chatml_review instance request with
       | Ok response -> Ok response
       | Error message -> reviewer_failure failure message)
  | Model_reviewer { id; agent; model; failure } ->
    (match Option.bind model_completion ~f:(fun complete -> complete ~agent ~model) with
     | None ->
       fun _ -> reviewer_failure failure "model reviewer completion is not configured"
     | Some complete ->
       let reviewer = Model_reviewer.create ~env:host.Host.env ~id ~complete () in
       fun request ->
         (match Model_reviewer.review_result reviewer request with
          | Ok response -> Ok (Terminal response)
          | Error message -> reviewer_failure failure message))
  | Executable_reviewer { id; hook; failure } ->
    let worker =
      hook_worker
        specification
        host
        worker_runtimes
        secret_filter
        register
        id
        Hook_protocol.Reviewer
        hook
    in
    let reviewer = Executable_reviewer.create worker in
    fun request ->
      (match Executable_reviewer.review reviewer request with
       | Ok None -> Ok Defer
       | Ok (Some response) -> Ok (Terminal (review response))
       | Error error -> reviewer_failure failure error.message)
;;

let reviewer
      specification
      host
      extensions
      runtime_instances
      register
      worker_runtimes
      secret_filter
      configured
      manifest
      model_completion
  =
  match specification.S.reviewers with
  | None ->
    ui_reviewer specification configured manifest
    |> Option.map ~f:(fun reviewer request -> review (reviewer request))
  | Some reviewers ->
    let entries =
      List.map reviewers.values ~f:(fun entry ->
        register.Extension_owner.poll ();
        reviewer_entry
          specification
          host
          extensions
          runtime_instances
          register
          worker_runtimes
          secret_filter
          configured
          manifest
          model_completion
          entry)
    in
    Some
      (fun request ->
        let rec loop = function
          | [] -> review (Shell_access.Approval.Deny "all configured reviewers deferred")
          | reviewer :: rest ->
            (match reviewer request with
             | Ok (Terminal response) -> response
             | Ok Defer -> loop rest
             | Error message -> review (Shell_access.Approval.Deny message))
        in
        loop entries)
;;

let successful_result command stdout stderr =
  Shell_access.Interceptor.
    { command
    ; executable = None
    ; status = `Exited 0
    ; stdout
    ; stderr
    ; stdout_truncated = false
    ; stderr_truncated = false
    ; intercepted_by = None
    ; untrusted_output = true
    }
;;

let chatml_before_interceptor
      specification
      host
      extensions
      runtime_instances
      register
      (value : S.interceptor)
      script
      lifecycle
  =
  let instance =
    extension_instance
      specification
      host
      extensions
      runtime_instances
      register
      script
      Chatmd_shell_spec.Chatmd_script_spec.Shell_before_interceptor
      lifecycle
  in
  let matcher =
    Option.map value.matcher ~f:(fun matcher ->
      Lowering.matcher
        ~checkpoint:register.Extension_owner.poll
        host
        specification.source
        ~chatml_matcher:(chatml_matcher specification host extensions register)
        ~action:S.Deny
        matcher)
  in
  Shell_access.Interceptor.trusted_substitute ~name:value.id ~before:(fun context ->
    let failed message =
      match value.failure with
      | S.Keep_failure | No_match_failure -> Shell_access.Interceptor.Continue
      | Conservative_failure | Deny_failure | Error_failure | Unknown_failure ->
        Reject message
    in
    if
      Option.exists matcher ~f:(fun matcher ->
        not (Shell_access.Matcher.matches matcher context))
    then Continue
    else (
      match extension_call instance ~phase:"shell_before" context with
      | Ok Chatml_extension.Intercept_continue -> Continue
      | Ok (Intercept_rewrite argv) ->
        (match command_of_argv argv with
         | Ok command -> Rewrite command
         | Error message -> Reject message)
      | Ok (Intercept_respond (stdout, stderr)) ->
        Respond (successful_result context.command stdout stderr)
      | Ok (Intercept_reject reason) -> Reject reason
      | Ok _ -> failed "before interceptor returned the wrong action"
      | Error diagnostic -> failed diagnostic.message))
;;

let executable_before_interceptor
      specification
      host
      extensions
      register
      worker_runtimes
      secret_filter
      (value : S.interceptor)
      hook
  =
  let worker =
    hook_worker
      specification
      host
      worker_runtimes
      secret_filter
      register
      value.S.id
      Hook_protocol.Before_interceptor
      hook
  in
  let matcher =
    Option.map value.matcher ~f:(fun matcher ->
      Lowering.matcher
        ~checkpoint:register.Extension_owner.poll
        host
        specification.source
        ~chatml_matcher:(chatml_matcher specification host extensions register)
        ~action:S.Deny
        matcher)
  in
  Shell_access.Interceptor.trusted_substitute ~name:value.id ~before:(fun context ->
    if
      Option.exists matcher ~f:(fun matcher ->
        not (Shell_access.Matcher.matches matcher context))
    then Continue
    else (
      match Executable_interceptor.before worker context with
      | Ok action -> action
      | Error error ->
        (match value.failure with
         | S.Keep_failure | No_match_failure -> Continue
         | Conservative_failure | Deny_failure | Error_failure | Unknown_failure ->
           Reject error.message)))
;;

let result_event result =
  Chatml_result_value.of_result result |> Chatml_result_value.encode
;;

let chatml_after_interceptor
      specification
      host
      extensions
      runtime_instances
      register
      (value : S.interceptor)
      script
      lifecycle
  =
  let instance =
    extension_instance
      specification
      host
      extensions
      runtime_instances
      register
      script
      Chatmd_shell_spec.Chatmd_script_spec.Shell_after_interceptor
      lifecycle
  in
  Shell_access.Interceptor.output_filter ~name:value.id ~after:(fun result ->
    let failed message =
      match value.failure with
      | S.Keep_failure | No_match_failure -> result
      | Conservative_failure | Deny_failure | Error_failure | Unknown_failure ->
        failwith message
    in
    let event = result_event result in
    match Chatml_extension.call instance ~context:event ~event with
    | Ok Chatml_extension.Result_keep -> result
    | Ok (Result_replace (stdout, stderr)) -> { result with stdout; stderr }
    | Ok (Result_reject_disclosure reason) -> failwith reason
    | Ok _ -> failed "after interceptor returned the wrong action"
    | Error diagnostic -> failed diagnostic.message)
;;

let executable_after_interceptor
      specification
      host
      worker_runtimes
      secret_filter
      register
      (value : S.interceptor)
      hook
  =
  let worker =
    hook_worker
      specification
      host
      worker_runtimes
      secret_filter
      register
      value.S.id
      Hook_protocol.After_interceptor
      hook
  in
  Shell_access.Interceptor.output_filter ~name:value.id ~after:(fun result ->
    match Executable_interceptor.after worker result with
    | Ok result -> result
    | Error error ->
      (match value.failure with
       | S.Keep_failure | No_match_failure -> result
       | Conservative_failure | Deny_failure | Error_failure | Unknown_failure ->
         failwith error.message))
;;

let interceptors
      specification
      host
      extensions
      runtime_instances
      register
      worker_runtimes
      secret_filter
  =
  register.Extension_owner.poll ();
  Option.value_map specification.S.interceptors ~default:[] ~f:(fun interceptors ->
    List.map interceptors.values ~f:(fun value ->
      register.Extension_owner.poll ();
      match value.S.phase, value.extension with
      | Before, Chatml_extension { script; lifecycle } ->
        chatml_before_interceptor
          specification
          host
          extensions
          runtime_instances
          register
          value
          script
          lifecycle
      | After, Chatml_extension { script; lifecycle } ->
        chatml_after_interceptor
          specification
          host
          extensions
          runtime_instances
          register
          value
          script
          lifecycle
      | Before, Executable_extension hook ->
        executable_before_interceptor
          specification
          host
          extensions
          register
          worker_runtimes
          secret_filter
          value
          hook
      | After, Executable_extension hook ->
        executable_after_interceptor
          specification
          host
          worker_runtimes
          secret_filter
          register
          value
          hook))
;;

let audit_path specification host audit =
  match audit.S.format, audit.path with
  | Jsonl, Some path ->
    host_result specification (Host.resolve_path host ~source:specification.source path)
  | Session, _ -> Eio.Path.(host.Host.session_dir / "shell-audit.jsonl")
  | Jsonl, None ->
    fail specification "shell.audit_path_required" "JSONL audit requires a path"
  | (No_audit | Stderr), _ -> assert false
;;

let protected_audit_fields =
  String.Set.of_list
    [ "sequence"; "timestamp"; "request_id"; "runtime_id"; "manifest_sha256" ]
;;

let mutable_audit_field name = not (Set.mem protected_audit_fields name)

let replacement_fields secret_filter source =
  let open Result.Let_syntax in
  let%bind json = Jsonaf.parse source |> Result.map_error ~f:Error.to_string_hum in
  match json with
  | `Object fields ->
    let%bind fields =
      List.map fields ~f:(function
        | name, `String value when mutable_audit_field name ->
          Ok (name, Shell_access.Secret_filter.redact secret_filter value)
        | name, `String _ -> Error ("audit replacement cannot modify " ^ name)
        | name, _ -> Error ("audit replacement field must be a string: " ^ name))
      |> Result.all
    in
    (match String.Map.of_alist fields with
     | `Ok fields -> Ok fields
     | `Duplicate_key name -> Error ("duplicate audit replacement field: " ^ name))
  | _ -> Error "audit replacement must be a JSON object"
;;

module For_testing = struct
  let matcher_failure = matcher_failure
  let mutable_audit_field = mutable_audit_field
  let replacement_fields = replacement_fields
end

let chatml_audit_filter
      specification
      host
      extensions
      runtime_instances
      register
      secret_filter
      extension
  =
  let script, lifecycle =
    match extension with
    | S.Chatml_extension value -> value.script, value.lifecycle
    | Executable_extension _ -> assert false
  in
  let instance =
    extension_instance
      specification
      host
      extensions
      runtime_instances
      register
      script
      Chatmd_shell_spec.Chatmd_script_spec.Shell_audit_filter
      lifecycle
  in
  fun sink ->
    Shell_access.Audit.filter sink (fun envelope ->
      let event =
        Chatml_audit_value.of_envelope ~secret_filter envelope
        |> Chatml_audit_value.encode
      in
      match Chatml_extension.call instance ~context:event ~event with
      | Ok Chatml_extension.Audit_keep -> Ok (Some envelope)
      | Ok (Audit_drop_field field) when mutable_audit_field field ->
        Ok (Some { envelope with dropped_fields = Set.add envelope.dropped_fields field })
      | Ok (Audit_drop_field field) -> Error ("audit filter cannot drop " ^ field)
      | Ok (Audit_replace source) ->
        Result.map (replacement_fields secret_filter source) ~f:(fun replacement_fields ->
          Some { envelope with replacement_fields })
      | Ok _ -> Error "audit filter returned the wrong action"
      | Error diagnostic -> Error diagnostic.message)
;;

let executable_audit_filter specification host worker_runtimes secret_filter register hook
  =
  let worker =
    hook_worker
      specification
      host
      worker_runtimes
      secret_filter
      register
      "audit-filter"
      Hook_protocol.Audit_filter
      hook
  in
  fun sink ->
    Shell_access.Audit.filter sink (fun envelope ->
      match Executable_audit_filter.filter worker ~secret_filter envelope with
      | Error error -> Error error.message
      | Ok Hook_protocol.Audit_keep -> Ok (Some envelope)
      | Ok (Audit_drop_field field) when mutable_audit_field field ->
        Ok (Some { envelope with dropped_fields = Set.add envelope.dropped_fields field })
      | Ok (Audit_drop_field field) -> Error ("audit filter cannot drop " ^ field)
      | Ok (Audit_replace_fields fields) ->
        let source =
          `Object
            (Map.to_alist fields |> List.map ~f:(fun (name, value) -> name, `String value))
          |> Jsonaf.to_string
        in
        Result.map (replacement_fields secret_filter source) ~f:(fun replacement_fields ->
          Some { envelope with replacement_fields })
      | Ok _ -> Error "audit filter returned the wrong action")
;;

let audit_filter
      specification
      host
      extensions
      runtime_instances
      register
      worker_runtimes
      secret_filter
      audit
  =
  register.Extension_owner.poll ();
  match audit.S.filter with
  | None -> Fun.id
  | Some (Chatml_extension _ as extension) ->
    chatml_audit_filter
      specification
      host
      extensions
      runtime_instances
      register
      secret_filter
      extension
  | Some (Executable_extension hook) ->
    executable_audit_filter specification host worker_runtimes secret_filter register hook
;;

let audit_sink specification ~sw (host : Host.t) secret_filter =
  let audit = Option.value_exn specification.S.audit in
  let failure_policy = Lowering.audit_failure audit.failure in
  let sink =
    match audit.format with
    | S.No_audit -> Shell_access.Audit.ignore
    | Stderr ->
      Audit_sink.create_flow
        ~flow:(Eio.Stdenv.stderr host.env)
        ~content:audit.content
        ~failure_policy
        ~secret_filter
    | Jsonl ->
      (match
         Audit_sink.create_chained_jsonl
           ~env:host.env
           ~sw
           ~path:(audit_path specification host audit)
           ~content:audit.content
           ~failure_policy
           ~secret_filter
       with
       | Ok audit -> audit
       | Error error -> fail specification error.code error.message)
    | Session ->
      (match
         Audit_sink.create_session
           ~env:host.env
           ~session_dir:host.session_dir
           ~content:audit.content
           ~failure_policy
           ~secret_filter
       with
       | Ok audit -> audit
       | Error error -> fail specification error.code error.message)
  in
  sink
;;

let available_backends specification host backends =
  let available =
    List.filter
      backends
      ~f:(Shell_access.Backend.available ~fs:(Eio.Stdenv.fs host.Host.env))
  in
  if List.is_empty available
  then fail specification "shell.backend_unavailable" "no configured backend is available";
  backends
;;

let backend_atom = function
  | S.Literal_atom value -> Shell_access.Backend.Literal value
  | Cwd_atom -> Cwd
  | Target_executable_atom -> Target_executable
  | Command_argv_atom -> Command_argv
  | Read_roots_atom { flag } -> Read_roots { flag }
  | Write_roots_atom { flag } -> Write_roots { flag }
  | Network_flag_atom value -> Network_flag value
  | Resource_limit_args_atom -> Resource_limit_args
;;

let external_backend specification host resolver cwd environment accept = function
  | S.External { id; executable; sha256; confinement; atoms; _ } ->
    let path =
      host_result
        specification
        (Host.resolve_path host ~source:specification.source executable)
      |> Eio.Path.native_exn
    in
    let wrapper =
      match
        Shell_access.Resolver.resolve
          resolver
          ~fs:(Eio.Stdenv.fs host.Host.env)
          ~cwd
          ~environment
          (Shell_access.Command.create path [])
      with
      | Ok executable -> executable
      | Error message -> fail specification "shell.external_backend_unavailable" message
    in
    Option.iter sha256 ~f:(fun expected ->
      if not (String.Caseless.equal expected wrapper.fingerprint.sha256)
      then
        fail
          specification
          "shell.external_backend_digest_mismatch"
          "external backend digest does not match");
    let confinement =
      match confinement with
      | S.Verified_confinement -> Shell_access.Backend.Verified
      | Declared_confinement -> Declared
      | No_confinement -> Unconfined
    in
    Shell_access.Backend.external_
      ~name:(Option.value id ~default:("external:" ^ wrapper.canonical_path))
      ~wrapper
      ~confinement
      ~accept_declared_confinement:accept
      (List.map atoms ~f:backend_atom)
    |> Result.ok_or_failwith
  | Seatbelt _ | Bubblewrap _ | Direct _ -> assert false
;;

let config
      specification
      ~sw
      ~host
      ~manifest
      ~platform
      ~approval_provider
      ~admin_policy
      ~approval_store
      ~audit_sequence
      ~extensions
      ~runtime_instances
      ~register_extension_instance
      ~worker_runtimes
      ~model_completion
      ~check_execution
  =
  let source = specification.S.source in
  let environment =
    environment_result
      specification
      (Environment.create host ~source (Option.value_exn specification.environment))
  in
  let secret_values =
    environment_result
      specification
      (Environment.load_secrets host ~source (Option.value_exn specification.secrets))
  in
  let secret_filter = secret_filter specification environment secret_values in
  let cwd =
    host_result
      specification
      (Host.resolve_existing_directory
         host
         ~source
         (value specification "cwd" specification.cwd))
  in
  let capabilities =
    lowering_result
      specification
      (Lowering.capabilities host ~source (Option.value_exn specification.capabilities))
  in
  let resolver =
    lowering_result
      specification
      (Lowering.resolver host ~source (Option.value_exn specification.resolver))
  in
  let limits, max_stdin_bytes =
    lowering_result
      specification
      (Lowering.limits (Option.value_exn specification.limits))
  in
  let policy =
    lowering_result
      specification
      (Lowering.policy
         host
         ~source
         ~chatml_matcher:
           (chatml_matcher specification host extensions register_extension_instance)
         (Option.value_exn specification.policy))
  in
  let backends =
    let specification_backends = Option.value_exn specification.backends in
    let external_backend =
      external_backend
        specification
        host
        resolver
        (Eio.Path.native_exn cwd)
        environment.values
        specification_backends.accept_declared_confinement
    in
    lowering_result
      specification
      (Lowering.backends host ~source ~platform ~external_backend specification_backends)
    |> available_backends specification host
  in
  let reviewer =
    reviewer
      specification
      host
      extensions
      runtime_instances
      register_extension_instance
      worker_runtimes
      secret_filter
      approval_provider
      manifest
      model_completion
  in
  let analyzers =
    analyzers
      specification
      host
      extensions
      runtime_instances
      register_extension_instance
      worker_runtimes
      secret_filter
  in
  let interceptors =
    interceptors
      specification
      host
      extensions
      runtime_instances
      register_extension_instance
      worker_runtimes
      secret_filter
  in
  let audit_sink = audit_sink specification ~sw host secret_filter in
  let audit =
    audit_filter
      specification
      host
      extensions
      runtime_instances
      register_extension_instance
      worker_runtimes
      secret_filter
      (Option.value_exn specification.S.audit)
      audit_sink
  in
  let executor_config =
    Shell_access.Executor.config
      ~env:host.env
      ~runtime_id:(runtime_id specification)
      ~manifest_sha256:manifest.sha256
      ~policy
      ~capabilities
      ~resolver
      ?reviewer_with_metadata:reviewer
      ~approval_store
      ~administrative_check:(fun context ->
        Admin_policy.check_context admin_policy context
        |> Result.map_error ~f:(fun violations ->
          violations
          |> List.map ~f:(fun violation ->
            sprintf
              "%s: requested=%s ceiling=%s source=%s remediation=%s"
              violation.Admin_policy.code
              violation.requested
              violation.ceiling
              violation.policy_source
              violation.remediation)
          |> String.concat ~sep:"; "))
      ~analyzers
      ~interceptors
      ~backends
      ~cwd
      ~process_env:environment.values
      ~limits
      ~secret_filter
      ~audit
      ~audit_sequence
      ~session_id:host.session_id
      ~pipefail:(value specification "pipefail" specification.pipefail)
      ()
  in
  let executor_config =
    match check_execution with
    | None -> executor_config
    | Some check ->
      Shell_access.Executor.with_execution_scope
        executor_config
        ~session_id:host.session_id
        ~approval_store
        ~check
  in
  ( executor_config
  , secret_filter
  , max_stdin_bytes
  , environment.values
  , resolver
  , Eio.Path.native_exn cwd
  , audit_sink )
;;

let create_exn
      ~sw
      ~host
      ~manifest
      ~platform
      ~approval_provider
      ~admin_policy
      ~approval_store
      ~audit_sequence
      ~extensions
      ~runtime_instances
      ~register_extension_instance
      ~worker_runtimes
      ~model_completion
      ~check_execution
      ~native_workers
      ~model_services
      specification
  =
  let ( executor_config
      , secret_filter
      , max_stdin_bytes
      , environment
      , resolver
      , cwd
      , audit_sink )
    =
    config
      specification
      ~sw
      ~host
      ~manifest
      ~platform
      ~approval_provider
      ~admin_policy
      ~approval_store
      ~audit_sequence
      ~extensions
      ~runtime_instances
      ~register_extension_instance
      ~worker_runtimes
      ~model_completion
      ~check_execution
  in
  { spec = specification
  ; executor_config
  ; secret_filter
  ; max_stdin_bytes
  ; executables = executables specification host
  ; resolver
  ; cwd
  ; environment
  ; host
  ; resources =
      { audit_sink
      ; approval_provider
      ; native_workers = List.rev !native_workers
      ; model_services =
          Option.map model_completion ~f:(fun _ -> List.rev !model_services)
      }
  }
;;

let create
      ~sw
      ~host
      ~manifest
      ~platform
      ~approval_provider
      ?(admin_policy = Admin_policy.permissive)
      ?(approval_store = Shell_access.Approval.create_store ())
      ?(audit_sequence = Atomic.make 0)
      ?(extensions = String.Map.empty)
      ?(runtime_instances = String.Table.create ())
      ?(register_extension_instance = fun ~runtime_id:_ ~lifecycle:_ _ -> ())
      ?prepare_extension
      ?check_execution
      ?(worker_runtimes = String.Map.empty)
      ?model_completion
      specification
  =
  let native_workers = ref [] in
  let model_services = ref [] in
  let model_completion =
    Option.map model_completion ~f:(fun select ~agent ~model ->
      let complete = select ~agent ~model in
      model_services := { agent; model; complete } :: !model_services;
      complete)
  in
  let register_extension_instance =
    Extension_owner.
      { prepare =
          Option.value prepare_extension ~default:(fun ~lifecycle compiled ->
            Chatml_extension.instantiate ~env:host.Host.env ~lifecycle compiled)
      ; register = register_extension_instance
      ; poll = ignore
      ; lookup = Map.find
      ; worker =
          (fun specification host worker_runtimes secret_filter id kind hook ->
            let worker =
              create_hook_worker
                specification
                host
                worker_runtimes
                secret_filter
                id
                kind
                hook
            in
            native_workers := { id; kind; worker } :: !native_workers;
            worker)
      }
  in
  try
    Ok
      (create_exn
         ~sw
         ~host
         ~manifest
         ~platform
         ~approval_provider
         ~admin_policy
         ~approval_store
         ~audit_sequence
         ~extensions
         ~runtime_instances
         ~register_extension_instance
         ~worker_runtimes
         ~model_completion
         ~check_execution
         ~native_workers
         ~model_services
         specification)
  with
  | Runtime_error error -> Error [ error ]
  | exn ->
    Error
      [ { runtime_id = runtime_id specification
        ; code = "shell.runtime_instantiation_failed"
        ; message = Exn.to_string exn
        }
      ]
;;

let id t = runtime_id t.spec
let spec t = t.spec
let executor_config t = t.executor_config
let executor_config_for_call t = Call_context.prepare_executor_for_call t.executor_config
let redact t value = Shell_access.Secret_filter.redact t.secret_filter value
let max_stdin_bytes t = t.max_stdin_bytes
let executable t id = Map.find t.executables id

let environment_value t name =
  let prefix = name ^ "=" in
  Array.find_map t.environment ~f:(fun entry ->
    Option.map (String.chop_prefix entry ~prefix) ~f:Fn.id)
;;

let normalized_components path =
  String.split path ~on:'/'
  |> List.fold ~init:[] ~f:(fun components -> function
    | "" | "." -> components
    | ".." ->
      (match components with
       | component :: rest when not (String.equal component "..") -> rest
       | _ -> ".." :: components)
    | component -> component :: components)
  |> List.rev
;;

let relative_path ~from ~to_ =
  if not (Bool.equal (Filename.is_relative from) (Filename.is_relative to_))
  then to_
  else (
    let rec remove_common from to_ =
      match from, to_ with
      | from_head :: from_tail, to_head :: to_tail when String.equal from_head to_head ->
        remove_common from_tail to_tail
      | _ -> from, to_
    in
    let from, to_ =
      remove_common (normalized_components from) (normalized_components to_)
    in
    let parent = List.init (List.length from) ~f:(fun _ -> "..") in
    match parent @ to_ with
    | [] -> "."
    | components -> String.concat ~sep:"/" components)
;;

let process_path t path =
  let path = Eio.Path.native_exn path in
  if Filename.is_relative path then relative_path ~from:t.cwd ~to_:path else path
;;

let resolve_path t ~source path =
  Result.map (Host.resolve_path t.host ~source path) ~f:(process_path t)
;;

let resolve_executable t program =
  Shell_access.Resolver.resolve
    t.resolver
    ~fs:(Eio.Stdenv.fs t.host.env)
    ~cwd:t.cwd
    ~environment:t.environment
    (Shell_access.Command.create program [])
;;

let load_file t ~source ~max_bytes path =
  Result.bind (Host.resolve_path t.host ~source path) ~f:(fun path ->
    if not (Eio.Path.is_file path)
    then
      Error { Host.code = "shell.script_unavailable"; message = "script is unavailable" }
    else (
      let contents = Eio.Path.load path in
      let size = String.length contents in
      if size > max_bytes
      then
        Error
          { Host.code = "shell.script_too_large"
          ; message = "script exceeds the configured source limit"
          }
      else
        Ok
          { path = process_path t path
          ; verification_path = Eio.Path.native_exn path
          ; sha256 = Digestif.SHA256.(to_hex (digest_string contents))
          ; size
          }))
;;

let with_execution_check t ~check =
  { t with
    executor_config = Shell_access.Executor.with_execution_check t.executor_config ~check
  }
;;

let rebind_code
      original
      ~manifest
      ~specification
      ~extensions
      ~runtime_instances
      ~register_extension_instance
      ~prepare_extension
      ~worker_runtimes
      ~check_execution
      ~checkpoint
  =
  Chatml.Chatml_checkpoint.protect checkpoint ~f:(fun checkpoint ->
    let equal = Registry_code_budget.equal_string_with_checkpoint ~checkpoint in
    let workers = ref original.resources.native_workers in
    let models = ref (Option.value original.resources.model_services ~default:[]) in
    let failure code message = fail specification code message in
    let lookup map key =
      Map.fold map ~init:None ~f:(fun ~key:current ~data found ->
        checkpoint ();
        match found with
        | Some _ -> found
        | None -> if equal current key then Some data else None)
    in
    let model_completion ~agent ~model =
      checkpoint ();
      match !models with
      | [] -> failure "shell.retained_model_missing" "retained model service is missing"
      | service :: remaining ->
        let same_model =
          match service.model, model with
          | None, None -> true
          | Some left, Some right -> equal left right
          | None, Some _ | Some _, None -> false
        in
        if not (equal service.agent agent && same_model)
        then failure "shell.retained_model_changed" "retained model service order changed";
        models := remaining;
        service.complete
    in
    let owner =
      Extension_owner.
        { prepare =
            (fun ~lifecycle compiled ->
              checkpoint ();
              prepare_extension ~lifecycle compiled)
        ; register = register_extension_instance
        ; poll = checkpoint
        ; lookup
        ; worker =
            (fun _specification _host runtimes _secret_filter id kind hook ->
              checkpoint ();
              match !workers with
              | [] ->
                failure
                  "shell.retained_worker_missing"
                  "retained native worker is missing"
              | occurrence :: remaining ->
                if
                  not
                    (equal occurrence.id id
                     && Hook_protocol.equal_kind occurrence.kind kind)
                then
                  failure
                    "shell.retained_worker_changed"
                    "retained native worker order changed";
                let runtime =
                  match lookup runtimes hook.S.runtime with
                  | None ->
                    failure
                      "shell.retained_worker_runtime_missing"
                      "retained worker runtime is missing"
                  | Some runtime -> runtime
                in
                let worker =
                  match
                    Hook_worker.with_executor_config
                      occurrence.worker
                      runtime.executor_config
                  with
                  | Ok worker -> worker
                  | Error message -> failure "shell.retained_worker_owner_changed" message
                in
                workers := remaining;
                worker)
        }
    in
    (* Native path expressions retain the original actual source bases. The new
     specification remains the returned code's diagnostic declaration. *)
    let native_specification = { specification with S.source = original.spec.source } in
    try
      checkpoint ();
      let policy =
        lowering_result
          specification
          (Lowering.policy
             ~checkpoint
             original.host
             ~source:original.spec.source
             ~chatml_matcher:
               (chatml_matcher native_specification original.host extensions owner)
             (Option.value_exn specification.S.policy))
      in
      checkpoint ();
      let reviewer =
        reviewer
          native_specification
          original.host
          extensions
          runtime_instances
          owner
          worker_runtimes
          original.secret_filter
          original.resources.approval_provider
          manifest
          (Option.map original.resources.model_services ~f:(fun _ -> model_completion))
      in
      let analyzers =
        analyzers
          native_specification
          original.host
          extensions
          runtime_instances
          owner
          worker_runtimes
          original.secret_filter
      in
      let interceptors =
        interceptors
          native_specification
          original.host
          extensions
          runtime_instances
          owner
          worker_runtimes
          original.secret_filter
      in
      let audit =
        audit_filter
          native_specification
          original.host
          extensions
          runtime_instances
          owner
          worker_runtimes
          original.secret_filter
          (Option.value_exn specification.S.audit)
          original.resources.audit_sink
      in
      checkpoint ();
      if not (List.is_empty !workers)
      then
        failure "shell.retained_worker_surplus" "retained native worker was not consumed";
      if not (List.is_empty !models)
      then
        failure "shell.retained_model_surplus" "retained model service was not consumed";
      let code =
        Shell_access.Executor.Code.create
          ~manifest_sha256:manifest.M.sha256
          ~policy
          ~reviewer:None
          ~reviewer_with_metadata:reviewer
          ~analyzers
          ~interceptors
          ~audit
          ~check:check_execution
      in
      let executor_config =
        Shell_access.Executor.with_code original.executor_config code
      in
      checkpoint ();
      Ok { original with spec = specification; executor_config }
    with
    | Runtime_error error -> Error [ error ])
;;
