open Core
open Support
module CM = Prompt.Chat_markdown
module Plan = Chat_response.Script_contract_plan
module Scripts = Agent_session.Prompt_script_sources
module C = Chatml_prompt_programs
module P = Chatml_prepared_program
module S = Chatml_source
module Input = Chatmd_parser_input

let custody_limits ?(programs = 16) () =
  C.Limits.create
    ~max_programs:programs
    ~max_parser_inputs:32
    ~max_logical_sources:128
    ~max_total_input_bytes:1_000_000
    ~project_work_steps:1_000_000
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let plan
      ?(checkpoint = ignore)
      ?(shell = None)
      ?(moderator_invocation = Plan.Program.Role.Moderator_invocation.Ui)
      ?(moderator_surface = Chatml.Chatml_builtin_surface.ui_moderator_surface)
      ?(delegated_moderator = false)
      parsed
  =
  Plan.create
    parsed
    ~moderator_invocation
    ~moderator_surface
    ~delegated_moderator
    ~shell
    ~limits:
      (Plan.Limits.create
         ~max_programs:16
         ~max_metadata_bytes:1_000_000
         ~max_work_steps:1_000_000
       |> Or_error.ok_exn)
    ~checkpoint
  |> ok
;;

let prepare
      ?(operation = fun _ -> Chatml_project_build.Operation.Initial_prepare)
      ?(provided = [])
      ?(checkpoint = ignore)
      ?(additional_anchors = [])
      ?(max_work_steps = 1_000_000)
      env
      scope
      captured
      plans
  =
  let root_plan, role_plans =
    match plans with
    | root_plan :: role_plans -> root_plan, role_plans
    | [] -> failwith "fixture requires explicit Root plan"
  in
  Scripts.capture
    scope
    ~capture:captured
    ~root_plan
    ~role_plans
    ~operation
    ~provided
    ~additional_anchors
    ~git_store:None
    ~git_grants:[]
    ~root_limits:(Project_tests.root_limits ())
    ~max_grants:8
    ~max_grant_bytes:65536
    ~max_request_bytes:65536
    ~max_path_components:32
    ~max_input_bytes:1_000_000
    ~max_work_steps
    ~policy:(Project_tests.policy ())
    ~limits:(Project_tests.limits ())
    ~custody_limits:(custody_limits ())
    ~clock:env#mono_clock
    ~checkpoint
;;

let prepare_with_projections env scope captured root_plan =
  Scripts.capture_with_projections
    scope
    ~capture:captured
    ~root_plan
    ~role_plans:[]
    ~operation:(fun _ -> Chatml_project_build.Operation.Initial_prepare)
    ~provided:[]
    ~additional_anchors:[]
    ~git_store:None
    ~git_grants:[]
    ~root_limits:(Project_tests.root_limits ())
    ~max_grants:8
    ~max_grant_bytes:65536
    ~max_request_bytes:65536
    ~max_path_components:32
    ~max_input_bytes:1_000_000
    ~max_work_steps:1_000_000
    ~policy:(Project_tests.policy ())
    ~limits:(Project_tests.limits ())
    ~custody_limits:(custody_limits ())
    ~clock:env#mono_clock
    ~checkpoint:ignore
;;

let raw = {|<import src="pkg/declarations.chatmd"/>|}
let body = "let run = fun ctx input -> Task.pure(`Complete(`String(\"captured\")))"

let declarations =
  sprintf
    {|<script id="lookup" language="chatml" kind="tool" src="lookup.chatml" project="chatml.project.json"/>
<script id="spare" language="chatml" kind="tool">%s</script>|}
    body
;;

let populate root =
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "pkg");
  save root "root.chatmd" raw;
  save root "pkg/declarations.chatmd" declarations;
  save root "pkg/lookup.chatml" body;
  save
    root
    "pkg/chatml.project.json"
    {|{"schema_version":1,"namespace":"example","modules":{"Lookup":{"implementation":"lookup.chatml"}}}|}
;;

let replay env captured custody =
  let value = Owner.Capture.value captured in
  let root = Builder.Capture.root value in
  let input = CM.Parsed.root_input root in
  let sources =
    (Input.path input, Input.authored input) :: Builder.Capture.sources value
  in
  let authored =
    Chatmd_source_bundle.create ~root_file:(Input.path input) ~sources () |> ok
  in
  let custody =
    C.of_storage
      (C.to_storage custody)
      ~authored
      ~limits:(custody_limits ())
      ~checkpoint:ignore
    |> ok
  in
  let parsed =
    CM.replay_chat_inputs
      (C.root_input custody)
      ~source_loader:
        (Source_loader.captured_filesystem ~root:(Eio.Stdenv.cwd env) ~sources)
      ~dir:(Eio.Stdenv.cwd env)
      ~limits:(parser_limits ())
      ~checkpoint:ignore
  in
  let restored =
    Chat_response.Restored_programs.restore
      (plan parsed)
      ~custody
      ~limits:(custody_limits ())
      ~checkpoint:ignore
    |> ok
    |> Option.value_exn
  in
  parsed, restored
;;

let%test_unit "host receipt projects final selected source through original capture" =
  with_workspace (fun env _ root ->
    populate root;
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let parsed = Builder.Capture.root (Owner.Capture.value captured) in
        let result = prepare_with_projections env scope captured (plan parsed) |> ok in
        let custody = Scripts.Captured.programs result in
        let receipts = Scripts.Captured.projections result in
        assert (List.length receipts = 2);
        let _, restored = replay env captured custody in
        List.iter (C.Restored.programs restored) ~f:(fun program ->
          let binding = C.Restored.Program.binding program in
          let receipt =
            List.find_exn receipts ~f:(fun candidate ->
              C.Binding.equal (Scripts.Projection_receipt.binding candidate) binding)
          in
          let graph = C.Restored.Program.prepared program in
          let entry = P.find_source graph (P.entry graph) |> Option.value_exn in
          let location =
            S.locate entry ~start_byte:0 ~end_byte:3 ~checkpoint:ignore |> ok
          in
          let projection =
            Scripts.Projection_receipt.project receipt location ~checkpoint:ignore |> ok
          in
          match C.Restored.Program.input program, projection with
          | Selected _, Exact_external external_location ->
            assert (String.equal (S.Id.package_namespace (S.id entry)) "example");
            assert (
              String.equal
                (S.Id.relative_path (S.Location.source external_location))
                "pkg/lookup.chatml");
            assert (not (S.Id.equal (S.id entry) (S.Location.source external_location)));
            assert (Int.equal (S.Location.span external_location).left.offset 0);
            let forged =
              S.reidentify
                entry
                ~id:
                  (S.Id.create ~package_namespace:"forged" ~relative_path:"lookup.chatml"
                   |> Or_error.ok_exn)
            in
            let forged_location =
              S.locate forged ~start_byte:0 ~end_byte:3 ~checkpoint:ignore |> ok
            in
            assert (
              Result.is_error
                (Scripts.Projection_receipt.project
                   receipt
                   forged_location
                   ~checkpoint:ignore))
          | Provided _, Exact_inline span ->
            assert (
              String.equal
                (Input.path (CM.Declaration.Extraction.input span))
                "pkg/declarations.chatmd")
          | _ -> assert false);
        Ok ())
      |> ok))
;;

let%expect_test
    "imported project selection and mixed Provided sibling survive source removal"
  =
  with_workspace (fun env _ root ->
    populate root;
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let parsed = Builder.Capture.root (Owner.Capture.value captured) in
        let custody = prepare env scope captured [ plan parsed ] |> ok in
        assert (List.length (C.bindings custody) = 2);
        Eio.Path.rmtree Eio.Path.(root / "pkg");
        Eio.Path.unlink Eio.Path.(root / "root.chatmd");
        let _, restored = replay env captured custody in
        let programs = C.Restored.programs restored in
        assert (List.length programs = 2);
        List.iter programs ~f:(fun program ->
          let graph = C.Restored.Program.prepared program in
          let entry = P.find_source graph (P.entry graph) |> Option.value_exn in
          assert (String.equal (S.contents entry) body);
          match C.Restored.Program.input program with
          | Selected build ->
            assert (S.Id.equal (P.entry graph) (S.id entry));
            assert (List.length (P.definitions graph) = 1);
            ignore (build : Chatml_project_build.t);
            assert (
              List.for_all (P.definitions graph) ~f:(fun definition ->
                S.Id.equal (P.Definition.implementation definition) (P.entry graph)));
            printf
              "selected entry: %s/%s; catalog definitions: %d\n"
              (S.Id.package_namespace (P.entry graph))
              (S.Id.relative_path (P.entry graph))
              (List.length (P.definitions graph))
          | Provided _ ->
            assert (List.is_empty (P.definitions graph));
            assert (
              String.is_prefix (S.Id.relative_path (P.entry graph)) ~prefix:"fragments/");
            print_endline "unconfigured sibling: implicit Provided fragment");
        Ok ())
      |> ok));
  [%expect
    {|
    selected entry: example/lookup.chatml; catalog definitions: 1
    unconfigured sibling: implicit Provided fragment
    |}]
;;

let%test_unit "supplied graph identity is binding plus contract, never body alone" =
  with_workspace (fun env _ root ->
    populate root;
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let initial_plan = plan (Builder.Capture.root (Owner.Capture.value captured)) in
        let custody = prepare env scope captured [ initial_plan ] |> ok in
        let _, restored = replay env captured custody in
        let supplied =
          List.find_map (C.Restored.programs restored) ~f:(fun program ->
            match C.Restored.Program.input program with
            | Selected _ -> None
            | Provided graph -> Some (C.Restored.Program.binding program, graph))
          |> Option.value_exn
        in
        ignore
          (prepare ~provided:[ supplied ] env scope captured [ initial_plan ] |> ok : C.t);
        assert (
          Result.is_error
            (prepare ~provided:[ supplied; supplied ] env scope captured [ initial_plan ]));
        let binding, graph = supplied in
        let selected_binding =
          List.find_map (Plan.programs initial_plan) ~f:(fun program ->
            if
              Option.is_some
                (CM.Declaration.project_selection (Plan.Program.declaration program))
            then Some (Plan.Program.binding program)
            else None)
          |> Option.value_exn
        in
        let input = CM.Parsed.root_input (Plan.parsed initial_plan) in
        let authored =
          Chatmd_source_bundle.create
            ~root_file:(Input.path input)
            ~sources:
              ((Input.path input, Input.authored input)
               :: Builder.Capture.sources (Owner.Capture.value captured))
            ()
          |> ok
        in
        let partial =
          C.capture
            authored
            ~parsed_contexts:[ Plan.parsed initial_plan ]
            ~root:input
            ~programs:[ binding, C.Input.Provided graph ]
            ~active_programs:[ binding, P.compiler_contract graph ]
            ~limits:(custody_limits ())
            ~checkpoint:ignore
          |> ok
        in
        (match
           Chat_response.Restored_programs.restore
             initial_plan
             ~custody:partial
             ~limits:(custody_limits ())
             ~checkpoint:ignore
         with
         | Ok _ -> assert false
         | Error errors ->
           assert (
             List.exists errors ~f:(fun error ->
               Option.is_some error.Chatmd_shell_spec.Diagnostic.source)));
        assert (not (C.Binding.equal binding selected_binding));
        assert (
          Result.is_error
            (prepare
               ~provided:[ selected_binding, graph ]
               env
               scope
               captured
               [ initial_plan ]));
        ignore (prepare env scope captured [ initial_plan ] |> ok : C.t);
        Ok ())
      |> ok))
;;

let%test_unit "empty preparation validates actual owner without acquiring projects" =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>parser-only</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let foreign_owner = open_owner sw root definition in
      let captured =
        Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
      in
      let current_plan = plan (Builder.Capture.root (Owner.Capture.value captured)) in
      Owner.with_scope foreign_owner ~f:(fun scope ->
        assert (Result.is_error (prepare env scope captured [ current_plan ]));
        Ok ())
      |> ok;
      Owner.with_scope owner ~f:(fun scope ->
        let custody = prepare env scope captured [ current_plan ] |> ok in
        assert (List.is_empty (C.bindings custody));
        let count = ref 0 in
        ignore
          (prepare ~checkpoint:(fun () -> incr count) env scope captured [ current_plan ]
           |> ok
           : C.t);
        assert (!count > 0);
        ignore
          (prepare ~max_work_steps:!count env scope captured [ current_plan ] |> ok : C.t);
        assert (
          Result.is_error
            (prepare ~max_work_steps:(!count - 1) env scope captured [ current_plan ]));
        let exception Injected of int in
        for at = 1 to !count do
          let calls = ref 0 in
          let original = Injected at in
          let raised =
            try
              ignore
                (prepare env scope captured [ current_plan ] ~checkpoint:(fun () ->
                   incr calls;
                   if !calls = at then raise original)
                 : (C.t, Builder.Diagnostic.t list) Result.t);
              false
            with
            | exn ->
              assert (phys_equal exn original);
              true
          in
          assert raised;
          assert (
            List.is_empty (C.bindings (prepare env scope captured [ current_plan ] |> ok)))
        done;
        Ok ())
      |> ok))
;;

let%test_unit
    "one native matcher binding retains its authentic default and selected contracts"
  =
  with_workspace (fun env _ root ->
    save
      root
      "root.chatmd"
      {|<script id="matcher" language="chatml" kind="shell_matcher">let initial_state = fail("must remain inert")</script>
<shell_access id="runtime" extends="builtin:yolo@1">
  <policy><rule id="dynamic" action="deny"><chatml_match script="matcher" function="selective"/></rule></policy>
</shell_access>|};
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let parsed = Builder.Capture.root (Owner.Capture.value captured) in
        let shell =
          Chat_response.Agent_runtime.prepare_shell_manifest
            ~platform:Macos
            ~prompt_elements:(CM.Parsed.elements parsed)
          |> ok
        in
        let planned = plan ~shell:(Some shell) parsed in
        let programs = Plan.programs planned in
        assert (List.length programs = 2);
        (match programs with
         | [ left; right ] ->
           assert (
             C.Binding.equal (Plan.Program.binding left) (Plan.Program.binding right));
           assert (
             not
               (S.Digest.equal
                  (Plan.Program.compiler_contract left)
                  (Plan.Program.compiler_contract right)))
         | [] | [ _ ] | _ :: _ :: _ :: _ -> assert false);
        let required =
          List.concat_map programs ~f:(fun program ->
            Chatml.Chatml_compiler_contract.required_bindings
              (Plan.Program.contract program)
            |> List.map ~f:fst)
        in
        assert (List.mem required "match_command" ~equal:String.equal);
        assert (List.mem required "selective" ~equal:String.equal);
        Ok ())
      |> ok))
;;

let%test_unit
    "unconfigured actual script restores through lightweight path, not an empty graph \
     registry"
  =
  with_workspace (fun env _ root ->
    save
      root
      "root.chatmd"
      (sprintf {|<script id="local" language="chatml" kind="tool">%s</script>|} body);
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let parsed = Builder.Capture.root (Owner.Capture.value captured) in
        let planned = plan parsed in
        assert (List.length (Plan.programs planned) = 1);
        let custody = prepare env scope captured [ planned ] |> ok in
        assert (not (C.has_active_programs custody));
        assert (
          Option.is_none
            (Chat_response.Restored_programs.restore
               planned
               ~custody
               ~limits:(custody_limits ())
               ~checkpoint:ignore
             |> ok));
        let program = List.hd_exn (Plan.programs planned) in
        let binding = Plan.Program.binding program in
        let graph =
          P.single_file
            ~compiler_contract:(Plan.Program.compiler_contract program)
            ~source:(CM.Declaration.script_body (Plan.Program.declaration program))
            ~checkpoint:ignore
            ()
          |> ok
        in
        let provided =
          prepare ~provided:[ binding, graph ] env scope captured [ planned ] |> ok
        in
        assert (C.has_active_programs provided);
        assert (
          Option.is_some
            (Chat_response.Restored_programs.restore
               planned
               ~custody:provided
               ~limits:(custody_limits ())
               ~checkpoint:ignore
             |> ok));
        let input = CM.Parsed.root_input parsed in
        let authored =
          Chatmd_source_bundle.create
            ~root_file:(Input.path input)
            ~sources:
              ((Input.path input, Input.authored input)
               :: Builder.Capture.sources (Owner.Capture.value captured))
            ()
          |> ok
        in
        let inactive =
          C.reroot
            provided
            ~authored
            ~parsed
            ~programs:[]
            ~limits:(custody_limits ())
            ~checkpoint:ignore
          |> ok
        in
        assert (not (C.has_active_programs inactive));
        (match
           Chat_response.Restored_programs.restore
             planned
             ~custody:inactive
             ~limits:(custody_limits ())
             ~checkpoint:ignore
         with
         | Ok _ -> assert false
         | Error errors ->
           assert (
             List.exists errors ~f:(fun error ->
               String.equal
                 error.Chatmd_shell_spec.Diagnostic.code
                 "chatml.program_missing_selection")));
        Ok ())
      |> ok))
;;

let%test_unit
    "nested grant cannot turn original captured file into an equal-body foreign member"
  =
  with_workspace (fun env parent root ->
    populate root;
    let alternate = Eio.Path.(parent / "alternate") in
    Eio.Path.mkdir ~perm:0o700 alternate;
    Eio.Path.with_open_dir alternate (fun alternate_root ->
      save alternate_root "lookup.chatml" body;
      save
        alternate_root
        "chatml.project.json"
        {|{"schema_version":1,"namespace":"alternate","modules":{"Lookup":{"implementation":"lookup.chatml"}}}|};
      let definition = definition root in
      Eio.Switch.run (fun sw ->
        let owner = open_owner sw root definition in
        let anchor prefix =
          Chatml_local_roots.Anchor.create
            alternate_root
            ~host_prefix:prefix
            ~max_path_components:32
            ~limits:(Project_tests.root_limits ())
            ~checkpoint:ignore
          |> ok
        in
        let captured =
          Owner.with_scope owner ~f:(fun scope ->
            let captured = capture env scope definition |> ok in
            let parsed = Builder.Capture.root (Owner.Capture.value captured) in
            let planned = plan parsed in
            let selected additional_anchors =
              let custody =
                prepare ~additional_anchors env scope captured [ planned ] |> ok
              in
              let _, restored = replay env captured custody in
              List.find_map (C.Restored.programs restored) ~f:(fun program ->
                match C.Restored.Program.input program with
                | Provided _ -> None
                | Selected _ -> Some (C.Restored.Program.prepared program))
              |> Option.value_exn
            in
            let unrelated =
              selected [ anchor (Eio.Path.native_exn Eio.Path.(parent / "unrelated")) ]
            in
            assert (String.equal (S.Id.package_namespace (P.entry unrelated)) "example");
            let nested_prefix = Eio.Path.native_exn Eio.Path.(root / "pkg") in
            let overridden = selected [ anchor (nested_prefix ^ "/./") ] in
            assert (
              String.is_prefix
                (S.Id.package_namespace (P.entry overridden))
                ~prefix:"chatmd.");
            let definition = List.hd_exn (P.definitions overridden) in
            assert (
              String.equal
                (S.Id.package_namespace (P.Definition.implementation definition))
                "alternate");
            assert (
              not
                (S.Id.equal (P.entry overridden) (P.Definition.implementation definition)));
            Ok captured)
          |> ok
        in
        assert (
          Result.is_error
            (Owner.with_scope owner ~f:(fun scope ->
               let planned = plan (Builder.Capture.root (Owner.Capture.value captured)) in
               assert (
                 Result.is_error
                   (prepare
                      ~additional_anchors:[ anchor (Eio.Path.native_exn root) ]
                      env
                      scope
                      captured
                      [ planned ]));
               (* A failed native batch invalidates its actual Scope, even when the
               callback observes the typed failure. It never revokes the owner. *)
               Ok ())));
        Owner.with_scope owner ~f:(fun scope ->
          ignore
            (prepare
               env
               scope
               captured
               [ plan (Builder.Capture.root (Owner.Capture.value captured)) ]
             |> ok
             : C.t);
          Ok ())
        |> ok)))
;;

let%test_unit
    "one queued child retains only the two requested authentic roles and deduplicates \
     common programs"
  =
  with_workspace (fun env _ root ->
    save
      root
      "root.chatmd"
      {|<tool name="headless" agent="child.chatmd" local="true"/>
<tool name="delegated" agent="child.chatmd" local="true" persistence="persistent"/>
<import src="matcher.chatmd" namespace="unused_a"/>
<import src="matcher.chatmd" namespace="unused_b"/>
<import src="matcher.chatmd" namespace="unused_c"/>
<import src="matcher.chatmd" namespace="unused_d"/>
<import src="matcher.chatmd" namespace="unused_e"/>|};
    save
      root
      "child.chatmd"
      (sprintf
         {|<script id="owner" language="chatml" kind="moderator" api="extensibility-v1" project="child.project.json">let initial_state = 0
let on_event = fun ctx state event -> Task.pure(state)</script>
<script id="common" language="chatml" kind="tool">%s</script>
<import src="matcher.chatmd" namespace="shared"/>
<shell_access id="checks" extends="builtin:yolo@1">
<policy><rule id="dynamic" action="deny"><chatml_match script="shared:matcher"/></rule></policy>
</shell_access>|}
         body);
    save
      root
      "matcher.chatmd"
      {|<script id="matcher" language="chatml" kind="shell_matcher">let initial_state = ()</script>|};
    save root "child.project.json" {|{"schema_version":1,"namespace":"child"}|};
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let value = Owner.Capture.value captured in
        let root_plan = plan (Builder.Capture.root value) in
        assert (List.is_empty (Plan.programs root_plan));
        assert (List.length (CM.Parsed.declarations (Plan.parsed root_plan)) = 5);
        let children = Builder.Capture.agents value in
        assert (List.length children = 1);
        let _, child = List.hd_exn children in
        let shell =
          Chat_response.Agent_runtime.prepare_shell_manifest
            ~platform:Macos
            ~prompt_elements:(CM.Parsed.elements child)
          |> ok
        in
        let headless =
          plan
            ~shell:(Some shell)
            ~moderator_invocation:Headless
            ~moderator_surface:Chatml.Chatml_builtin_surface.moderator_surface
            child
        in
        let delegated = plan ~shell:(Some shell) ~delegated_moderator:true child in
        assert (List.length (Plan.programs headless) = 3);
        assert (List.length (Plan.programs delegated) = 3);
        let custody =
          prepare env scope captured [ root_plan; headless; delegated ] |> ok
        in
        assert (List.length (C.bindings custody) = 4);
        assert (not (C.has_active_programs custody));
        assert (
          Option.is_none
            (Chat_response.Restored_programs.restore
               root_plan
               ~custody
               ~limits:(custody_limits ~programs:4 ())
               ~checkpoint:ignore
             |> ok));
        let input = CM.Parsed.root_input (Builder.Capture.root value) in
        let authored =
          Chatmd_source_bundle.create
            ~root_file:(Input.path (CM.Parsed.root_input child))
            ~sources:
              ((Input.path input, Input.authored input) :: Builder.Capture.sources value)
            ()
          |> ok
        in
        (* Empty child selection cannot silently discard retained Provided graphs;
           the parent's unused import is not an executable plan row. *)
        let unselected_child =
          C.reroot
            custody
            ~authored
            ~parsed:child
            ~programs:[]
            ~limits:(custody_limits ())
            ~checkpoint:ignore
          |> ok
        in
        (match
           Chat_response.Restored_programs.restore
             headless
             ~custody:unselected_child
             ~limits:(custody_limits ())
             ~checkpoint:ignore
         with
         | Ok _ -> assert false
         | Error errors ->
           assert (
             List.exists errors ~f:(fun error ->
               String.equal
                 error.Chatmd_shell_spec.Diagnostic.code
                 "chatml.program_missing_selection")));
        List.iter [ headless; delegated ] ~f:(fun selected ->
          let active =
            List.map (Plan.programs selected) ~f:(fun program ->
              let binding = Plan.Program.binding program in
              binding, Plan.Program.compiler_contract program, binding)
          in
          let selected_custody =
            C.reroot
              custody
              ~authored
              ~parsed:child
              ~programs:active
              ~limits:(custody_limits ())
              ~checkpoint:ignore
            |> ok
          in
          let restored =
            Chat_response.Restored_programs.restore
              selected
              ~custody:selected_custody
              ~limits:(custody_limits ())
              ~checkpoint:ignore
            |> ok
            |> Option.value_exn
          in
          assert (List.length (C.Restored.programs restored) = 3));
        let repeated =
          prepare env scope captured [ root_plan; root_plan; headless; headless ] |> ok
        in
        assert (List.length (C.bindings repeated) = 3);
        assert (not (C.has_active_programs repeated));
        assert (Result.is_error (prepare env scope captured [ headless; root_plan ]));
        Ok ())
      |> ok))
;;

let%test_unit "source-slot matching streams qualified-import pairs beyond program count" =
  with_workspace (fun env _ root ->
    let aliases = [ "a"; "b"; "c"; "d"; "e" ] in
    save
      root
      "root.chatmd"
      (List.map aliases ~f:(fun alias ->
         sprintf {|<import src="library.chatmd" namespace="%s"/>|} alias)
       |> String.concat ~sep:"\n");
    save
      root
      "library.chatmd"
      (sprintf
         {|<script id="local" language="chatml" kind="tool" project="library.project.json">%s</script>|}
         body);
    save root "library.project.json" {|{"schema_version":1,"namespace":"library"}|};
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let parsed = Builder.Capture.root (Owner.Capture.value captured) in
        let planned = plan parsed in
        let custody = prepare env scope captured [ planned ] |> ok in
        let count = List.length aliases in
        assert (List.length (Plan.programs planned) = count);
        let limits =
          C.Limits.create
            ~max_programs:count
            ~max_parser_inputs:32
            ~max_logical_sources:128
            ~max_total_input_bytes:1_000_000
            ~project_work_steps:1_000_000
            ~document:Document_schema.Limits.default
          |> Or_error.ok_exn
        in
        let pairs, exact =
          C.fold_declaration_matches
            custody
            ~parsed
            ~limits
            ~checkpoint:ignore
            ~init:(0, 0)
            ~f:(fun (pairs, exact) matched ->
              ( pairs + 1
              , exact
                + Bool.to_int
                    (C.Binding.equal
                       (C.Declaration_match.original matched)
                       (C.Declaration_match.current matched)) ))
          |> ok
        in
        (* Five differently qualified bindings share one physical source slot.
           All pairs match that slot; only the diagonal matches full identity. *)
        assert (pairs = count * count);
        assert (exact = count);
        let exception Injected in
        (match
           C.fold_declaration_matches
             custody
             ~parsed
             ~limits
             ~checkpoint:ignore
             ~init:()
             ~f:(fun () _ -> raise Injected)
         with
         | _ -> assert false
         | exception Injected -> ());
        let reused =
          C.fold_declaration_matches
            custody
            ~parsed
            ~limits
            ~checkpoint:ignore
            ~init:0
            ~f:(fun count _ -> count + 1)
          |> ok
        in
        assert (reused = pairs);
        Ok ())
      |> ok))
;;

let%test_unit "self-root requested variants retain only the actual current role active" =
  with_workspace (fun env _ root ->
    save
      root
      "root.chatmd"
      {|<tool name="headless" agent="root.chatmd" local="true"/>
<tool name="delegated" agent="root.chatmd" local="true" persistence="persistent"/>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1" project="chatml.project.json">let initial_state = 0
let on_event = fun ctx state event -> Task.pure(state)</script>|};
    save root "chatml.project.json" {|{"schema_version":1,"namespace":"self"}|};
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let value = Owner.Capture.value captured in
        assert (List.is_empty (Builder.Capture.agents value));
        let parsed = Builder.Capture.root value in
        let headless =
          plan
            ~moderator_invocation:Headless
            ~moderator_surface:Chatml.Chatml_builtin_surface.moderator_surface
            parsed
        in
        let delegated = plan ~delegated_moderator:true parsed in
        let calls = ref 0 in
        let custody =
          prepare
            env
            scope
            captured
            [ headless; delegated; headless; delegated ]
            ~operation:(fun _ ->
              incr calls;
              Chatml_project_build.Operation.Initial_prepare)
          |> ok
        in
        assert (!calls = 2);
        assert (List.length (C.bindings custody) = 2);
        let restored =
          Chat_response.Restored_programs.restore
            headless
            ~custody
            ~limits:(custody_limits ())
            ~checkpoint:ignore
          |> ok
          |> Option.value_exn
        in
        let active = C.Restored.programs restored in
        assert (List.length active = 1);
        assert (
          S.Digest.equal
            (Plan.Program.compiler_contract (List.hd_exn (Plan.programs headless)))
            (P.compiler_contract (C.Restored.Program.prepared (List.hd_exn active))));
        assert (
          Result.is_error
            (Chat_response.Restored_programs.restore
               delegated
               ~custody
               ~limits:(custody_limits ())
               ~checkpoint:ignore));
        Ok ())
      |> ok))
;;

let%test_unit "configured operation callback is forwarded once and preserves exceptions" =
  with_workspace (fun env _ root ->
    populate root;
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let captured =
        Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
      in
      let planned = plan (Builder.Capture.root (Owner.Capture.value captured)) in
      let previous =
        Owner.with_scope owner ~f:(fun scope ->
          let custody = prepare env scope captured [ planned; planned ] |> ok in
          let _, restored = replay env captured custody in
          let previous =
            List.find_map (C.Restored.programs restored) ~f:(fun program ->
              match C.Restored.Program.input program with
              | Provided _ -> None
              | Selected build -> Some (Chatml_project_build.lock_source build))
            |> Option.value_exn
          in
          Ok previous)
        |> ok
      in
      Owner.with_scope owner ~f:(fun scope ->
        let calls = ref 0 in
        let custody =
          prepare env scope captured [ planned; planned ] ~operation:(fun program ->
            incr calls;
            assert (
              Option.is_some
                (CM.Declaration.project_selection (Plan.Program.declaration program)));
            Chatml_project_build.Operation.Build { previous_lock = previous })
          |> ok
        in
        assert (!calls = 1);
        let _, restored = replay env captured custody in
        assert (List.length (C.Restored.programs restored) = 2);
        let exception Injected in
        let original = Injected in
        (match
           prepare env scope captured [ planned ] ~operation:(fun _ -> raise original)
         with
         | _ -> assert false
         | exception exn -> assert (phys_equal exn original));
        Ok ())
      |> ok;
      let invalid =
        S.create
          (S.id previous)
          ~contents:"{}"
          ~expected_digest:None
          ~max_bytes:None
          ~checkpoint:ignore
        |> ok
      in
      assert (
        Result.is_error
          (Owner.with_scope owner ~f:(fun scope ->
             match
               prepare env scope captured [ planned ] ~operation:(fun _ ->
                 Chatml_project_build.Operation.Build { previous_lock = invalid })
             with
             | Ok _ -> assert false
             | Error _ -> Ok ())));
      Owner.with_scope owner ~f:(fun scope ->
        ignore (prepare env scope captured [ planned ] |> ok : C.t);
        Ok ())
      |> ok))
;;
