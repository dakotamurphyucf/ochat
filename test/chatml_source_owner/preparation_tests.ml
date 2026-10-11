open Core
open Support
module CM = Prompt.Chat_markdown
module Plan = Chat_response.Script_contract_plan
module C = Chatml_prompt_programs
module Prepare = Agent_session.Prompt_script_preparation
module Startup = Chat_response.Startup_compilation

let policy ?(roles = 16) ?(references = 64) ?(work = 1_000_000) () =
  let sources =
    Prepare.Source_policy.create
      ~root_limits:(Project_tests.root_limits ())
      ~max_grants:8
      ~max_grant_bytes:65536
      ~max_request_bytes:65536
      ~max_path_components:32
      ~max_input_bytes:1_000_000
      ~max_work_steps:1_000_000
      ~policy:(Project_tests.policy ())
      ~limits:(Project_tests.limits ())
      ~custody_limits:(Script_tests.custody_limits ())
    |> Or_error.ok_exn
  in
  Prepare.Policy.create
    ~compilation:Chatml_compilation.default_limits
    ~planning:
      (Plan.Limits.create
         ~max_programs:16
         ~max_metadata_bytes:1_000_000
         ~max_work_steps:1_000_000
       |> Or_error.ok_exn)
    ~sources
    ~max_context_roles:roles
    ~max_reference_visits:references
    ~max_work_steps:work
  |> Or_error.ok_exn
;;

let prepare
      ?startup
      ?(root_role = Prepare.Root_role.Ui)
      ?(checkpoint = ignore)
      ?(operation = fun _ -> Chatml_project_build.Operation.Initial_prepare)
      ?(policy = policy ())
      env
      scope
      captured
  =
  Prepare.capture
    policy
    ?startup
    ~root_role
    ~host:{ additional_anchors = []; git_store = None; git_grants = [] }
    ~scope
    ~capture:captured
    ~operation
    ~provided:[]
    ~env
    ~checkpoint
    ()
;;

let authored captured =
  let value = Owner.Capture.value captured in
  let input = CM.Parsed.root_input (Builder.Capture.root value) in
  Chatmd_source_bundle.create
    ~root_file:(Chatmd_parser_input.path input)
    ~sources:
      ((Chatmd_parser_input.path input, Chatmd_parser_input.authored input)
       :: Builder.Capture.sources value)
    ()
  |> ok
;;

let%expect_test "actual imported invocations select roles and root cycles stay finite" =
  with_workspace (fun env _ root ->
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "parts");
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "children");
    let moderator project =
      sprintf
        {|<script id="owner" language="chatml" kind="moderator" api="extensibility-v1" project="%s">let initial_state = fail("initializer must stay inert")
let on_event = fun ctx state event -> Task.pure(state)</script>|}
        project
    in
    save
      root
      "root.chatmd"
      (moderator "root.project.json"
       ^ {|<script id="legacy" language="chatml" kind="moderator" project="root.project.json">let initial_state = 0
let on_event = fun event state -> Task.pure(state)</script>|}
       ^ {|<import src="parts/references.chatmd"/>
<tool name="remote" agent="https://example.invalid/agent"/>
<tool name="external" agent="/host/external.chatmd" local/>|}
      );
    save
      root
      "parts/references.chatmd"
      {|<tool name="once" agent="../children/child.chatmd" local/>
<tool name="keep" agent="../children/child.chatmd" local persistence="optional"/>
<user><agent src="../children/child.chatmd" local>content</agent></user>|};
    save
      root
      "children/child.chatmd"
      (moderator "child.project.json"
       ^ {|<tool name="root" agent="../root.chatmd" local/>
<tool name="delegated_root" agent="../root.chatmd" local persistence="persistent"/>|}
      );
    save root "root.project.json" {|{"schema_version":1,"namespace":"root"}|};
    save root "children/child.project.json" {|{"schema_version":1,"namespace":"child"}|};
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let value = Owner.Capture.value captured in
        assert (List.length (Builder.Capture.agents value) = 1);
        let calls = ref 0 in
        let operation _ =
          incr calls;
          Chatml_project_build.Operation.Initial_prepare
        in
        List.iter
          [ policy ~roles:4 (); policy ~references:6 () ]
          ~f:(fun policy ->
            assert (Result.is_error (prepare ~operation ~policy env scope captured));
            assert (!calls = 0));
        let custody =
          prepare ~operation ~policy:(policy ~roles:5 ~references:7 ()) env scope captured
          |> ok
        in
        assert (!calls = 6);
        assert (List.length (C.bindings custody) = 6);
        let root_plan = Script_tests.plan (Builder.Capture.root value) in
        let root_programs =
          Chat_response.Restored_programs.restore
            root_plan
            ~custody
            ~limits:(Script_tests.custody_limits ())
            ~checkpoint:ignore
          |> ok
          |> Option.value_exn
        in
        assert (List.length (C.Restored.programs root_programs) = 2);
        let headless_plan =
          Script_tests.plan
            ~moderator_surface:Chatml.Chatml_builtin_surface.moderator_surface
            (Builder.Capture.root value)
        in
        let headless_custody =
          prepare
            ~root_role:Prepare.Root_role.Headless
            ~policy:(policy ~roles:4 ~references:7 ())
            env
            scope
            captured
          |> ok
        in
        let headless_programs =
          Chat_response.Restored_programs.restore
            headless_plan
            ~custody:headless_custody
            ~limits:(Script_tests.custody_limits ())
            ~checkpoint:ignore
          |> ok
          |> Option.value_exn
        in
        assert (List.length (C.Restored.programs headless_programs) = 2);
        assert (List.length (C.bindings headless_custody) = 6);
        let legacy_contract programs =
          List.find_map (C.Restored.programs programs) ~f:(fun program ->
            if
              String.equal
                (C.Binding.script_id (C.Restored.Program.binding program))
                "legacy"
            then
              Some
                (Chatml_prepared_program.compiler_contract
                   (C.Restored.Program.prepared program))
            else None)
          |> Option.value_exn
        in
        assert (
          not
            (Chatml_source.Digest.equal
               (legacy_contract headless_programs)
               (legacy_contract root_programs)));
        assert (
          Result.is_error
            (Chat_response.Restored_programs.restore
               root_plan
               ~custody:headless_custody
               ~limits:(Script_tests.custody_limits ())
               ~checkpoint:ignore));
        let child = snd (List.hd_exn (Builder.Capture.agents value)) in
        List.iter
          [ Script_tests.plan
              ~moderator_surface:Chatml.Chatml_builtin_surface.moderator_surface
              child
          ; Script_tests.plan ~delegated_moderator:true child
          ]
          ~f:(fun planned ->
            let mappings =
              List.map (Plan.programs planned) ~f:(fun program ->
                let binding = Plan.Program.binding program in
                binding, Plan.Program.compiler_contract program, binding)
            in
            let child_authored =
              Chatmd_source_bundle.create
                ~root_file:(Chatmd_parser_input.path (CM.Parsed.root_input child))
                ~sources:(Chatmd_source_bundle.sources (authored captured))
                ()
              |> ok
            in
            let selected =
              C.reroot
                custody
                ~authored:child_authored
                ~parsed:child
                ~programs:mappings
                ~limits:(Script_tests.custody_limits ())
                ~checkpoint:ignore
              |> ok
            in
            let restored =
              Chat_response.Restored_programs.restore
                planned
                ~custody:selected
                ~limits:(Script_tests.custody_limits ())
                ~checkpoint:ignore
              |> ok
              |> Option.value_exn
            in
            assert (List.length (C.Restored.programs restored) = 1));
        print_s
          [%sexp
            { retained_variants = (List.length (C.bindings custody) : int)
            ; configured_requests = (!calls : int)
            ; active_root = (List.length (C.Restored.programs root_programs) : int)
            }];
        Ok ())
      |> ok));
  [%expect {| ((retained_variants 6) (configured_requests 6) (active_root 2)) |}]
;;

let%test_unit "unused markup stays unreachable and whole helper work is not reset" =
  with_workspace (fun env _ root ->
    save
      root
      "root.chatmd"
      {|<user>plain<tool name="ignored" agent="unused.chatmd" local/></user>|};
    save
      root
      "unused.chatmd"
      (sprintf
         {|<script id="unused" language="chatml" kind="tool" project="missing.json">%s</script>|}
         Script_tests.body);
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let calls = ref 0 in
        let operation _ =
          incr calls;
          assert false
        in
        let count = ref 0 in
        let custody =
          prepare
            ~operation
            ~policy:(policy ~roles:1 ~references:0 ())
            ~checkpoint:(fun () -> incr count)
            env
            scope
            captured
          |> ok
        in
        assert (!calls = 0);
        assert (List.is_empty (C.bindings custody));
        let succeeds work =
          Result.is_ok
            (prepare
               ~operation
               ~policy:(policy ~roles:1 ~references:0 ~work ())
               env
               scope
               captured)
        in
        let low = ref 0
        and high = ref !count in
        assert (succeeds !high);
        while !high - !low > 1 do
          let middle = !low + ((!high - !low) / 2) in
          if succeeds middle then high := middle else low := middle
        done;
        assert (!high > 1);
        assert (succeeds !high);
        assert (not (succeeds (!high - 1)));
        let exception Injected of int in
        for at = 1 to !count do
          let calls = ref 0 in
          let original = Injected at in
          match
            prepare ~operation env scope captured ~checkpoint:(fun () ->
              incr calls;
              if !calls = at then raise original)
          with
          | _ -> assert false
          | exception exn -> assert (phys_equal exn original)
        done;
        assert (succeeds !high);
        Ok ())
      |> ok))
;;

let%expect_test "source role preparation borrows original startup deadline" =
  with_workspace (fun env _ root ->
    Script_tests.populate root;
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        Eio.Path.unlink Eio.Path.(root / "pkg/chatml.project.json");
        let clock = Eio_mock.Clock.Mono.make () in
        let timed_env =
          object
            method fs = env#fs
            method cwd = env#cwd
            method stdin = env#stdin
            method stdout = env#stdout
            method stderr = env#stderr
            method net = env#net
            method domain_mgr = env#domain_mgr
            method process_mgr = env#process_mgr
            method clock = env#clock
            method mono_clock = (clock :> Eio.Time.Mono.ty Eio.Resource.t)
            method secure_random = env#secure_random
            method debug = env#debug
            method backend_id = env#backend_id
          end
        in
        let calls = ref 0 in
        let result =
          Startup.with_context
            ~limits:Chatml_compilation.default_limits
            ~env:timed_env
            ~checkpoint:ignore
            ~map_error:(fun error ->
              [ Builder.Diagnostic.
                  { code = error.Chatml_compilation.code
                  ; message = error.message
                  ; source = None
                  }
              ])
            ~f:(fun startup ->
              Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 4_000_000_000L);
              prepare ~startup timed_env scope captured ~operation:(fun _ ->
                incr calls;
                Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 6_000_000_000L);
                Chatml_project_build.Operation.Initial_prepare))
            ()
        in
        (match result with
         | Ok _ -> assert false
         | Error errors ->
           assert (
             List.exists errors ~f:(fun error ->
               String.equal error.Builder.Diagnostic.code "chatml.compile_timeout")));
        assert (!calls = 1);
        print_endline "original deadline rejected before missing project acquisition";
        Ok ())
      |> ok));
  [%expect
    {|
    +mock time is now 4
    +mock time is now 6
    original deadline rejected before missing project acquisition
    |}]
;;
