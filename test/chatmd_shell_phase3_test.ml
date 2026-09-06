open! Core
module C = Shell_runtime.Chatml_extension
module CM = Prompt.Chat_markdown
module L = Chatml.Chatml_lang
module M = Chatmd_shell_spec.Manifest
module MC = Chatmd_shell_spec.Manifest_compiler
module S = Chatmd_shell_spec.Chatmd_script_spec
module Surface = Chatml.Chatml_builtin_surface

let source_ref source =
  let position = Chatmd_shell_spec.Source_ref.{ offset = 0; line = 1; column = 0 } in
  Chatmd_shell_spec.Source_ref.create
    ~file:"phase3.chatmd"
    ~source_dir:"."
    ~prompt_dir:"."
    ~namespace:None
    ~start_pos:position
    ~end_pos:position
    ~source
;;

let script ?(id = "matcher") ?(limits = S.default_limits) kind source =
  { S.id = id
  ; language = "chatml"
  ; kind
  ; source = Inline source
  ; source_ref = source_ref source
  ; source_sha256 = Chatmd_shell_spec.Source_ref.digest source
  ; limits
  }
;;

let matcher_source =
  {|
    type state = { count : int }
    let initial_state = { count = 0 }
    let match_command : shell_context -> state -> state task =
      fun ctx st ->
        Task.bind(Match.yes(to_string(st.count)), fun ignored ->
        Task.pure({ count = st.count + 1 }))
  |}
;;

let event () =
  Shell_runtime.Chatml_context_value.
    { phase = "shell_matcher"
    ; request_id = "request"
    ; runtime_id = "runtime"
    ; manifest_sha256 = "digest"
    ; argv = [ "echo"; "hello" ]
    ; executable =
        { requested = "echo"
        ; path = "/bin/echo"
        ; canonical_path = "/bin/echo"
        ; trusted = true
        ; sha256 = "executable-digest"
        }
    ; cwd = "/workspace"
    ; origin = "tool"
    ; request_kind = "structured"
    ; stdin_kind = "empty"
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    ; script_preview = None
    ; effects = []
    ; capabilities =
        { read_roots = [ "/workspace" ]
        ; write_roots = []
        ; network = false
        ; child_processes = false
        ; arbitrary_code = false
        ; privilege_change = false
        ; sandbox = "required"
        }
    ; session_id = Some "session"
    ; policy = None
    }
  |> Shell_runtime.Chatml_context_value.encode
;;

let ok_exn = function
  | Ok value -> value
  | Error diagnostics ->
    diagnostics
    |> List.map ~f:Chatmd_shell_spec.Diagnostic.to_string
    |> String.concat ~sep:"\n"
    |> failwith
;;

let extension_ok = function
  | Ok value -> value
  | Error diagnostic -> failwith (Chatmd_shell_spec.Diagnostic.to_string diagnostic)
;;

let codec_ok = function
  | Ok value -> value
  | Error diagnostic -> failwith (Chatmd_shell_spec.Diagnostic.to_string diagnostic)
;;

let%expect_test "stateful matcher commits one validated action per call" =
  Eio_main.run (fun env ->
    let compiled = C.compile ~script:(script Shell_matcher matcher_source) |> ok_exn in
    let instance =
      C.instantiate ~env ~lifecycle:Chatmd_shell_spec.Shell_spec.Session compiled
      |> extension_ok
    in
    let call () = C.call instance ~context:(event ()) ~event:(event ()) |> extension_ok in
    print_s [%sexp (call () : C.action)];
    print_s [%sexp (call () : C.action)]);
  [%expect
    {|
    (Match true 0)
    (Match true 1) |}]
;;

let%expect_test "invocation lifecycle starts from fresh state" =
  Eio_main.run (fun env ->
    let compiled = C.compile ~script:(script Shell_matcher matcher_source) |> ok_exn in
    let instance =
      C.instantiate ~env ~lifecycle:Chatmd_shell_spec.Shell_spec.Invocation compiled
      |> extension_ok
    in
    let call () = C.call instance ~context:(event ()) ~event:(event ()) |> extension_ok in
    print_s [%sexp (call () : C.action)];
    print_s [%sexp (call () : C.action)]);
  [%expect
    {|
    (Match true 0)
    (Match true 0) |}]
;;

let invalid_transaction_source =
  {|
    type state = { count : int }
    let initial_state = { count = 0 }
    let match_command : shell_context -> state -> state task =
      fun ctx st ->
        Task.bind(Match.yes(to_string(st.count)), fun first ->
        Task.bind(Match.no("second"), fun second ->
        Task.pure({ count = st.count + 1 })))
  |}
;;

let%expect_test "invalid multiple actions roll back state" =
  Eio_main.run (fun env ->
    let compiled =
      C.compile ~script:(script Shell_matcher invalid_transaction_source) |> ok_exn
    in
    let instance =
      C.instantiate ~env ~lifecycle:Chatmd_shell_spec.Shell_spec.Session compiled
      |> extension_ok
    in
    for _ = 1 to 2 do
      match C.call instance ~context:(event ()) ~event:(event ()) with
      | Ok action -> print_s [%sexp (action : C.action)]
      | Error diagnostic -> print_endline diagnostic.code
    done);
  [%expect
    {|
    shell.chatml_runtime
    shell.chatml_runtime |}]
;;

let module_names surface =
  List.map surface.Surface.modules ~f:(fun value -> value.Chatml.Chatml_builtin_spec.name)
;;

let%expect_test "shell surfaces expose no ambient process or UI capabilities" =
  List.iter
    [ Surface.shell_matcher_surface
    ; Surface.shell_reviewer_surface
    ; Surface.shell_before_interceptor_surface
    ; Surface.shell_after_interceptor_surface
    ; Surface.shell_effect_surface
    ; Surface.shell_audit_surface
    ]
    ~f:(fun surface -> print_s [%sexp (module_names surface : string list)]);
  [%expect
    {|
    (String Array Json Task Option Hashtbl Shell Match)
    (String Array Json Task Option Hashtbl Shell Review)
    (String Array Json Task Option Hashtbl Shell Intercept)
    (String Array Json Task Option Hashtbl Result)
    (String Array Json Task Option Hashtbl Shell Effect)
    (String Array Json Task Option Hashtbl Audit)
    |}]
;;

let value_string = Chatml.Chatml_builtin_spec.value_to_string

let%expect_test "shell surfaces snapshot globals modules aliases and exports" =
  let snapshot surface =
    let globals = List.map surface.Surface.globals ~f:(fun value -> value.name) in
    let modules =
      List.map surface.modules ~f:(fun value ->
        value.name, List.map value.exports ~f:(fun export -> export.name))
    in
    let aliases = List.map surface.type_aliases ~f:(fun value -> value.name) in
    print_s [%sexp (globals : string list), (modules : (string * string list) list), (aliases : string list)]
  in
  snapshot Surface.shell_effect_surface;
  [%expect
    {|
    ((to_string length string_length string_is_empty array_copy record_keys
      variant_tag swap_ref fail hash_md5)
     ((String
       (length is_empty concat equal contains starts_with ends_with trim slice
        find split to_upper to_lower replace_all))
      (Array
       (length copy get set make append sub reverse reverse_in_place swap fill
        init map mapi iter iteri fold filter exists for_all find find_map))
      (Json
       (parse stringify pretty parse_opt validate tag as_bool as_number as_string
        as_array as_object object_keys get_field get_path set_field remove_field))
      (Task (pure bind map fail catch))
      (Option (none some is_none is_some get_or))
      (Hashtbl (create set get mem remove))
      (Shell
       (request_id runtime_id manifest_sha256 argv executable cwd origin
        request_kind stdin_kind stdin_sha256 stdin_bytes script_sha256
        script_preview effects capabilities session_id policy))
      (Effect
       (add replace read_path write_path network child_processes arbitrary_code
        privilege_change)))
     (json shell_context shell_policy))
    |}]
;;

let%test_unit "composed shell surfaces contain no name collisions" =
  List.iter
    [ Surface.shell_matcher_surface
    ; Surface.shell_reviewer_surface
    ; Surface.shell_before_interceptor_surface
    ; Surface.shell_after_interceptor_surface
    ; Surface.shell_effect_surface
    ; Surface.shell_audit_surface
    ]
    ~f:(fun surface ->
      let unique names =
        List.length names = Set.length (String.Set.of_list names)
      in
      assert (unique (List.map surface.globals ~f:(fun value -> value.name)));
      assert (unique (List.map surface.modules ~f:(fun value -> value.name)));
      assert (unique (List.map surface.type_aliases ~f:(fun value -> value.name))));
  assert
    (not
       (List.exists Surface.shell_matcher_surface.globals ~f:(fun value ->
          String.equal value.name "print")))
;;

let hook_source kind =
  match kind with
  | S.Shell_matcher ->
    "let initial_state = ()\nlet match_command = fun ev st -> Task.bind(Match.yes(\"yes\"), fun ignored -> Task.pure(st))"
  | Shell_reviewer ->
    "let initial_state = ()\nlet review = fun ev st -> Task.bind(Review.defer(), fun ignored -> Task.pure(st))"
  | Shell_before_interceptor ->
    "let initial_state = ()\nlet before = fun ev st -> Task.bind(Intercept.continue(), fun ignored -> Task.pure(st))"
  | Shell_after_interceptor ->
    "let initial_state = ()\nlet after = fun ev st -> Task.bind(Result.keep(), fun ignored -> Task.pure(st))"
  | Shell_effect_analyzer ->
    "let initial_state = ()\nlet analyze = fun ev st -> Task.bind(Effect.network(), fun ignored -> Task.pure(st))"
  | Shell_audit_filter ->
    "let initial_state = ()\nlet filter = fun ev st -> Task.bind(Audit.keep(), fun ignored -> Task.pure(st))"
  | Moderator -> assert false
;;

let result_event () =
  Shell_runtime.Chatml_result_value.
    { phase = "shell_after"
    ; argv = [ "echo" ]
    ; status_kind = "exited"
    ; status_code = 0
    ; stdout = "ok"
    ; stderr = ""
    ; stdout_truncated = false
    ; stderr_truncated = false
    ; intercepted_by = None
    ; untrusted_output = false
    }
  |> Shell_runtime.Chatml_result_value.encode
;;

let audit_event () =
  Shell_runtime.Chatml_audit_value.
    { phase = "shell_audit"
    ; sequence = 1L
    ; timestamp = 1.
    ; session_id = Some "session"
    ; runtime_id = "runtime"
    ; manifest_sha256 = "digest"
    ; request_id = "request"
    ; plan_id = None
    ; event = "resolved"
    ; fields = String.Map.of_alist_exn [ "command", "echo" ]
    }
  |> Shell_runtime.Chatml_audit_value.encode
;;

let event_for_kind = function
  | S.Shell_after_interceptor -> result_event ()
  | Shell_audit_filter -> audit_event ()
  | Shell_matcher | Shell_reviewer | Shell_before_interceptor | Shell_effect_analyzer -> event ()
  | Moderator -> assert false
;;

let%expect_test "all six extension kinds execute through the generic host runtime" =
  Eio_main.run (fun env ->
    List.iter
      [ S.Shell_matcher; Shell_reviewer; Shell_before_interceptor; Shell_after_interceptor
      ; Shell_effect_analyzer; Shell_audit_filter
      ]
      ~f:(fun kind ->
        let compiled = C.compile ~script:(script ~id:(S.kind_to_string kind) kind (hook_source kind)) |> ok_exn in
        let instance = C.instantiate ~env ~lifecycle:Invocation compiled |> extension_ok in
        let value = event_for_kind kind in
        print_s [%sexp (C.call instance ~context:value ~event:value |> extension_ok : C.action)]));
  [%expect
    {|
    (Match true yes)
    Review_defer
    Intercept_continue
    Result_keep
    (Effect_add network)
    Audit_keep |}]
;;

let%expect_test "matcher failures use action-sensitive conservative defaults" =
  let failure = Shell_runtime.Runtime.For_testing.matcher_failure in
  List.iter [ Chatmd_shell_spec.Shell_spec.Allow; Ask; Deny ] ~f:(fun action ->
    printf "%b\n" (failure action Conservative_failure));
  [%expect {|
    false
    true
    true |}]
;;

let%expect_test "fuel exhaustion rolls back state" =
  Eio_main.run (fun env ->
    let limits = { C.default_limits with fuel = 1 } in
    let compiled = C.compile ~script:(script Shell_matcher matcher_source) |> ok_exn in
    let instance = C.instantiate ~env ~limits ~lifecycle:Session compiled |> extension_ok in
    match C.call instance ~context:(event ()) ~event:(event ()) with
    | Ok _ -> print_endline "unexpected"
    | Error diagnostic -> print_endline diagnostic.message);
  [%expect {| ChatML task fuel exhausted |}]
;;

let%expect_test "stateful extension calls are serialized" =
  Eio_main.run (fun env ->
    let compiled = C.compile ~script:(script Shell_matcher matcher_source) |> ok_exn in
    let instance = C.instantiate ~env ~lifecycle:Session compiled |> extension_ok in
    let call () = C.call instance ~context:(event ()) ~event:(event ()) |> extension_ok in
    let left = ref None in
    let right = ref None in
    Eio.Fiber.both
      (fun () -> left := Some (call ()))
      (fun () -> right := Some (call ()))
    |> ignore;
    print_s [%sexp ([ Option.value_exn !left; Option.value_exn !right ] : C.action list)]);
  [%expect {| ((Match true 0) (Match true 1)) |}]
;;

let executable () =
  Shell_access.Executable.
    { requested = "echo"
    ; path = "/bin/echo"
    ; canonical_path = "/bin/echo"
    ; trusted = true
    ; fingerprint =
        { device = 1
        ; inode = 2
        ; mode = 0o755
        ; uid = 1
        ; gid = 1
        ; size = 10L
        ; mtime = 1.
        ; sha256 = "executable-digest"
        }
    }
;;

let shell_context () =
  Shell_access.Context.
    { request_id = "request"
    ; runtime_id = "runtime"
    ; manifest_sha256 = "manifest"
    ; command = Shell_access.Command.create "echo" [ "hello" ]
    ; executable = executable ()
    ; cwd = "/workspace"
    ; environment = [||]
    ; request_kind = Structured
    ; stdin_kind = Empty
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    ; script_preview = None
    ; origin = Tool
    ; effects = []
    ; capabilities = Shell_access.Capabilities.read_only ~roots:[ "/workspace" ]
    ; policy_action = None
    ; policy_matches = []
    ; session_id = Some "session"
    }
;;

let approval_request () =
  let context = shell_context () in
  Shell_access.Approval.
    { context
    ; policy = { action = Ask; matches = []; reason = "dynamic policy" }
    ; identity =
        { manifest_sha256 = context.manifest_sha256
        ; runtime_id = context.runtime_id
        ; request_kind = context.request_kind
        ; command_hash = "command"
        ; executable_sha256 = context.executable.fingerprint.sha256
        ; argv = Shell_access.Command.to_argv context.command
        ; cwd_sha256 = "cwd"
        ; environment_sha256 = "environment"
        ; stdin_sha256 = None
        ; stdin_bytes = 0
        ; script_sha256 = None
        }
    ; display_command = "echo hello"
    ; rationale = None
    }
;;

let%expect_test "model reviewer enforces timeout strict JSON and metadata" =
  Eio_main.run (fun env ->
    let malformed =
      Shell_runtime.Model_reviewer.create
        ~env
        ~id:"malformed"
        ~complete:(fun ~prompt:_ ->
          Ok { text = "not-json"; model = "test"; input_tokens = None; output_tokens = None })
        ()
    in
    let timed_out =
      Shell_runtime.Model_reviewer.create
        ~env
        ~wall_time_seconds:0.001
        ~id:"timeout"
        ~complete:(fun ~prompt:_ ->
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
          Ok { text = "{}"; model = "test"; input_tokens = None; output_tokens = None })
        ()
    in
    let valid =
      Shell_runtime.Model_reviewer.create
        ~env
        ~id:"valid"
        ~complete:(fun ~prompt:_ ->
          Ok
            { text = {|{"decision":"allow_once"}|}
            ; model = "test-model"
            ; input_tokens = Some 10
            ; output_tokens = Some 2
            })
        ()
    in
    List.iter [ malformed; timed_out ] ~f:(fun reviewer ->
      match Shell_runtime.Model_reviewer.review_result reviewer (approval_request ()) with
      | Ok _ -> print_endline "unexpected"
      | Error message -> print_endline message);
    match Shell_runtime.Model_reviewer.review_result valid (approval_request ()) with
    | Error message -> print_endline message
    | Ok review ->
      let metadata = Option.value_exn review.metadata in
      printf "%s:%s:%d:%d\n"
        metadata.reviewer_id
        (Option.value_exn metadata.model)
        (Option.value_exn metadata.input_tokens)
        (Option.value_exn metadata.output_tokens));
  [%expect
    {|
    malformed: invalid moderator JSON: ("Jsonaf.of_string: parse error" (error "json: unexpected string: 'not'")
      (input not-json))
    timeout: model reviewer timed out
    valid:test-model:10:2
    |}]
;;

let%expect_test "typed codecs round trip and reject unknown fields" =
  let value = shell_context () |> Shell_runtime.Chatml_context_value.of_context |> Shell_runtime.Chatml_context_value.encode in
  let decoded = Shell_runtime.Chatml_context_value.decode value |> codec_ok in
  let round_trip = Shell_runtime.Chatml_context_value.encode decoded in
  printf "%b\n" (String.equal (value_string value) (value_string round_trip));
  let unknown =
    match value with
    | L.VRecord fields -> L.VRecord (Map.set fields ~key:"secret_environment" ~data:(L.VString "secret"))
    | _ -> assert false
  in
  (match Shell_runtime.Chatml_context_value.decode unknown with
   | Ok _ -> print_endline "unexpected"
   | Error diagnostic -> print_endline (String.concat ~sep:"." diagnostic.path));
  let analyzer_result =
    Shell_runtime.Chatml_effect_value.Replace [ "network"; "read:/tmp" ]
  in
  let effect_value = Shell_runtime.Chatml_effect_value.encode analyzer_result in
  let decoded_effect = Shell_runtime.Chatml_effect_value.decode effect_value |> codec_ok in
  print_s [%sexp (decoded_effect : Shell_runtime.Chatml_effect_value.t)];
  let approval = Shell_runtime.Chatml_approval_value.Deny "unsafe" in
  let approval =
    Shell_runtime.Chatml_approval_value.encode_response approval
    |> Shell_runtime.Chatml_approval_value.decode_response
    |> codec_ok
  in
  print_s [%sexp (approval : Shell_runtime.Chatml_approval_value.response)];
  let interceptor = Shell_runtime.Chatml_interceptor_value.Rewrite [ "echo"; "safe" ] in
  let interceptor =
    Shell_runtime.Chatml_interceptor_value.encode interceptor
    |> Shell_runtime.Chatml_interceptor_value.decode
    |> codec_ok
  in
  print_s [%sexp (interceptor : Shell_runtime.Chatml_interceptor_value.t)];
  let result_value = result_event () in
  let result_round_trip =
    Shell_runtime.Chatml_result_value.decode result_value
    |> codec_ok
    |> Shell_runtime.Chatml_result_value.encode
  in
  printf "%b\n" (String.equal (value_string result_value) (value_string result_round_trip));
  let audit_value = audit_event () in
  let audit_round_trip =
    Shell_runtime.Chatml_audit_value.decode audit_value
    |> codec_ok
    |> Shell_runtime.Chatml_audit_value.encode
  in
  printf "%b\n" (String.equal (value_string audit_value) (value_string audit_round_trip));
  [%expect
    {|
    true
    context.secret_environment
    (Replace (network read:/tmp))
    (Deny unsafe)
    (Rewrite (echo safe))
    true
    true |}]
;;

let manifest_of_source env source =
  let elements = CM.parse_chat_inputs ~source:"digest.chatmd" ~dir:(Eio.Stdenv.cwd env) source in
  let runtimes, scripts =
    List.fold elements ~init:([], []) ~f:(fun (runtimes, scripts) -> function
      | CM.Shell_runtime runtime -> runtime :: runtimes, scripts
      | Shell_script script -> runtimes, script :: scripts
      | _ -> runtimes, scripts)
  in
  MC.compile
    { runtimes
    ; tools = []
    ; scripts
    ; legacy_tools = []
    ; moderator_runtime = None
    ; platform = Macos
    ; supported_features = Chatmd_shell_spec.Feature.phase3
    }
  |> ok_exn
;;

let digest_source body =
  sprintf
    {|<script id="matcher" language="chatml" kind="shell_matcher">%s</script>
       <shell_access id="runtime" extends="builtin:yolo@1">
         <policy><rule id="dynamic" action="deny"><chatml_match script="matcher"/></rule></policy>
       </shell_access>|}
    body
;;

let%expect_test "script source edits change the authorized manifest" =
  Eio_main.run (fun env ->
    let left = manifest_of_source env (digest_source "let initial_state = 1") in
    let right = manifest_of_source env (digest_source "let initial_state = 2") in
    printf "%b\n" (not (String.equal left.sha256 right.sha256)));
  [%expect {| true |}]
;;

let%expect_test "unused scripts produce warnings and executable material excludes them" =
  Eio_main.run (fun env ->
    let elements =
      CM.parse_chat_inputs
        ~source:"unused.chatmd"
        ~dir:(Eio.Stdenv.cwd env)
        {|<script id="unused" language="chatml" kind="shell_matcher">let initial_state = ()</script>
          <shell_access id="runtime" extends="builtin:yolo@1"/>|}
    in
    let runtimes, scripts =
      List.fold elements ~init:([], []) ~f:(fun (runtimes, scripts) -> function
        | CM.Shell_runtime runtime -> runtime :: runtimes, scripts
        | Shell_script script -> runtimes, script :: scripts
        | _ -> runtimes, scripts)
    in
    let _, material =
      MC.compile_with_material
        { runtimes; tools = []; scripts; legacy_tools = []; moderator_runtime = None
        ; platform = Macos; supported_features = Chatmd_shell_spec.Feature.phase3
        }
      |> ok_exn
    in
    List.iter (MC.warnings material) ~f:(fun warning -> print_endline warning.code));
  [%expect {| chatmd.unused_script |}]
;;

let%expect_test "protected audit identity fields cannot be transformed" =
  let mutable_field = Shell_runtime.Runtime.For_testing.mutable_audit_field in
  List.iter
    [ "sequence"; "timestamp"; "request_id"; "runtime_id"; "manifest_sha256"; "command" ]
    ~f:(fun field -> printf "%s:%b\n" field (mutable_field field));
  let secret_filter = Shell_access.Secret_filter.create [ "PRIVATE_KEY" ] in
  let replacement =
    Shell_runtime.Runtime.For_testing.replacement_fields
      secret_filter
      {|{"command":"PRIVATE_KEY","reason":"safe"}|}
    |> Result.ok_or_failwith
  in
  print_endline (Map.find_exn replacement "command");
  (match
     Shell_runtime.Runtime.For_testing.replacement_fields
       secret_filter
       {|{"request_id":"changed"}|}
   with
   | Ok _ -> print_endline "unexpected"
   | Error message -> print_endline message);
  [%expect
    {|
    sequence:false
    timestamp:false
    request_id:false
    runtime_id:false
    manifest_sha256:false
    command:true
    [REDACTED]
    audit replacement cannot modify request_id |}]
;;

let%test_unit "generic and compatibility runtime facades preserve type equality" =
  let host_state : Chatml_host_runtime.session -> Chatml.Chatml_lang.value =
    Chatml_runtime.current_state
  in
  let moderator_state : Chatml_host_runtime.session -> Chatml.Chatml_lang.value =
    Chatml_moderator_runtime.current_state
  in
  ignore (host_state, moderator_state : _ * _)
;;

let%expect_test "all script kinds serialize limits and round-trip through ChatMD" =
  Eio_main.run (fun env ->
    List.iter
      [ S.Shell_matcher; Shell_reviewer; Shell_before_interceptor; Shell_after_interceptor
      ; Shell_effect_analyzer; Shell_audit_filter
      ]
      ~f:(fun kind ->
        let declaration =
          Chatmd_script_declaration.serialize
            (script ~id:(S.kind_to_string kind) kind (hook_source kind))
        in
        let parsed =
          CM.parse_chat_inputs ~source:"round-trip.chatmd" ~dir:(Eio.Stdenv.cwd env) declaration
        in
        match parsed with
        | [ CM.Shell_script script ] ->
          printf "%s:%d:%d\n" (S.kind_to_string script.kind) script.limits.fuel script.limits.max_tasks
        | _ -> print_endline "unexpected"));
  [%expect
    {|
    shell_matcher:100000:256
    shell_reviewer:100000:256
    shell_before_interceptor:100000:256
    shell_after_interceptor:100000:256
    shell_effect_analyzer:100000:256
    shell_audit_filter:100000:256 |}]
;;

let%expect_test "extension compilation fails during preparation before authorization" =
  Eio_main.run (fun env ->
    let elements =
      CM.parse_chat_inputs
        ~source:"prepare.chatmd"
        ~dir:(Eio.Stdenv.cwd env)
        (digest_source "let initial_state = ()")
    in
    let runtimes, scripts =
      List.fold elements ~init:([], []) ~f:(fun (runtimes, scripts) -> function
        | CM.Shell_runtime runtime -> runtime :: runtimes, scripts
        | Shell_script script -> runtimes, script :: scripts
        | _ -> runtimes, scripts)
    in
    let manifest, material =
      MC.compile_with_material
        { runtimes; tools = []; scripts; legacy_tools = []; moderator_runtime = None
        ; platform = Macos; supported_features = Chatmd_shell_spec.Feature.phase3
        }
      |> ok_exn
    in
    match Shell_runtime.Registry.prepare ~manifest ~material with
    | Ok _ -> print_endline "unexpected"
    | Error errors -> print_endline (List.hd_exn errors).code);
  [%expect {| shell.chatml_compile |}]
;;

let declaration_source =
  {|
    <script id="matcher" language="chatml" kind="shell_matcher">let initial_state = ()</script>
    <script id="reviewer" language="chatml" kind="shell_reviewer">let initial_state = ()</script>
    <script id="before" language="chatml" kind="shell_before_interceptor">let initial_state = ()</script>
    <script id="after" language="chatml" kind="shell_after_interceptor">let initial_state = ()</script>
    <script id="effects" language="chatml" kind="shell_effect_analyzer">let initial_state = ()</script>
    <script id="audit" language="chatml" kind="shell_audit_filter">let initial_state = ()</script>
    <shell_access id="runtime" extends="builtin:yolo@1">
      <policy><rule id="dynamic" action="deny"><chatml_match script="matcher"/></rule></policy>
      <reviewers><reviewer id="review" kind="chatml" script="reviewer"/></reviewers>
      <interceptors>
        <interceptor id="before" phase="before" script="before"/>
        <interceptor id="after" phase="after" script="after"/>
      </interceptors>
      <effect_analysis><analyzer id="effects" script="effects"/></effect_analysis>
      <audit format="none"><filter script="audit"/></audit>
    </shell_access>
  |}
;;

let%expect_test "ChatMD and manifest retain all six typed extension kinds" =
  Eio_main.run (fun env ->
    let elements =
      CM.parse_chat_inputs
        ~source:"phase3.chatmd"
        ~dir:(Eio.Stdenv.cwd env)
        declaration_source
    in
    let runtimes, scripts =
      List.fold elements ~init:([], []) ~f:(fun (runtimes, scripts) -> function
        | CM.Shell_runtime runtime -> runtime :: runtimes, scripts
        | Shell_script script -> runtimes, script :: scripts
        | _ -> runtimes, scripts)
    in
    let manifest =
      MC.compile
        { runtimes
        ; tools = []
        ; scripts
        ; legacy_tools = []
        ; moderator_runtime = None
        ; platform = Chatmd_shell_spec.Shell_spec.Macos
        ; supported_features = Chatmd_shell_spec.Feature.phase3
        }
      |> ok_exn
    in
    List.iter manifest.M.payload.extension_scripts ~f:(fun script ->
      print_endline
        (script.id ^ ":" ^ Chatmd_shell_spec.Chatmd_script_spec.kind_to_string script.kind)));
  [%expect
    {|
    after:shell_after_interceptor
    audit:shell_audit_filter
    before:shell_before_interceptor
    effects:shell_effect_analyzer
    matcher:shell_matcher
    reviewer:shell_reviewer |}]
;;
