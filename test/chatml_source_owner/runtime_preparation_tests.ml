open Core
open Support
module Preparation = Agent_session.Runtime_script_preparation
module Revision = Agent_session.Prompt_revision

let policy ?(max_custody_work_steps = 1_000_000) () =
  let planning =
    Chat_response.Script_contract_plan.Limits.create
      ~max_programs:16
      ~max_metadata_bytes:(8 * 1024 * 1024)
      ~max_work_steps:1_000_000
    |> Or_error.ok_exn
  in
  let restoration =
    Chatml_prompt_programs.Limits.create
      ~max_programs:16
      ~max_parser_inputs:32
      ~max_logical_sources:64
      ~max_total_input_bytes:(8 * 1024 * 1024)
      ~project_work_steps:1_000_000
      ~document:Document_schema.Limits.default
    |> Or_error.ok_exn
  in
  Preparation.create
    ~compilation:Chatml_compilation.default_limits
    ~planning
    ~restoration
    ~max_custody_work_steps
  |> Or_error.ok_exn
;;

let%expect_test "retained child roles are selected before source identity is reserved" =
  with_workspace (fun env parent root ->
    List.iter [ false; true ] ~f:(fun delegated ->
      save
        root
        "root.chatmd"
        {|<tool name="child" agent="child.chatmd" local persistence="persistent"/>|};
      save
        root
        "child.chatmd"
        {|<script id="owner" language="chatml" kind="moderator" api="extensibility-v1" project="chatml.project.json">let initial_state = 0
let on_event = fun ctx state event -> Task.pure(state)</script>|};
      save root "chatml.project.json" {|{"schema_version":1,"namespace":"child"}|};
      Eio.Switch.run (fun sw ->
        let module A = Agent_session.Authored_agent_source in
        let module C = Chatml_prompt_programs in
        let definition = definition root in
        let catalog = catalog env parent in
        let candidate =
          prepare catalog sw [ definition ] ~prepare_programs:(fun scope captured ->
            let value = Owner.Capture.value captured in
            let child = snd (List.hd_exn (Builder.Capture.agents value)) in
            Script_tests.prepare
              env
              scope
              captured
              [ Script_tests.plan (Builder.Capture.root value)
              ; Script_tests.plan ~delegated_moderator:delegated child
              ])
          |> ok
        in
        Catalog.install_owned catalog candidate |> ok;
        let parent = revision catalog definition in
        let before = A.capture ~parent ~tool_name:"child" () |> ok in
        List.iter [ "root.chatmd"; "child.chatmd"; "chatml.project.json" ] ~f:(fun file ->
          Eio.Path.unlink Eio.Path.(root / file));
        let borrowed = ref None in
        let result =
          Preparation.with_authored_child
            (Some (policy ()))
            ~parent
            ~tool_name:"child"
            ~content_limits:None
            ~env
            ~checkpoint:ignore
            ~f:(fun startup ~source ~revision ->
              assert delegated;
              borrowed := Some startup;
              assert (not (String.equal (A.fingerprint before) (A.fingerprint source)));
              assert (
                C.has_active_programs (Revision.programs revision |> Option.value_exn));
              let projected = A.resource_revision ~parent source |> ok in
              assert (
                Agent_protocol.Id.Prompt_revision.equal
                  (Revision.id projected)
                  (Revision.id revision));
              Preparation.with_revision
                (Some (policy ()))
                ~startup
                revision
                ~env
                ~invocation:Delegated_ui
                ~checkpoint:ignore
                ~f:(fun same ~compilation_limits:_ ~programs ->
                  assert (phys_equal same startup);
                  assert (
                    List.length (C.Restored.programs (Option.value_exn programs)) = 1);
                  Ok ()))
        in
        match result with
        | Error error ->
          assert (not delegated);
          assert (Option.is_none !borrowed);
          assert (
            String.is_substring error.message ~substring:"chatml.program_missing_role");
          print_endline "wrong retained role: rejected before reservation callback"
        | Ok () ->
          assert delegated;
          assert (
            Result.is_error
              (Chat_response.Startup_compilation.remaining_limits
                 (Option.value_exn !borrowed)));
          print_endline
            "delegated role: selected identity, original startup, no source reads")));
  [%expect
    {|
    wrong retained role: rejected before reservation callback
    delegated role: selected identity, original startup, no source reads
    |}]
;;

let%test_unit "owned revisions require explicit policy and actual parser attachments" =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>owned revision</user>";
    Eio.Switch.run (fun sw ->
      let definition = definition root in
      let catalog = catalog env parent in
      let candidate = prepare catalog sw [ definition ] |> Or_error.ok_exn in
      Catalog.install_owned catalog candidate |> Or_error.ok_exn;
      let original = revision catalog definition in
      let detached =
        Revision.create
          ~definition:(Revision.definition original)
          ~artifact:(Revision.artifact original)
          ~materialized_tree:(Revision.materialized_tree original)
          ~elements:(Revision.elements original)
      in
      let generous = policy () in
      let exhausted = policy ~max_custody_work_steps:1 () in
      List.iter
        [ None, false; Some generous, true; Some exhausted, false ]
        ~f:(fun (policy, sufficient) ->
          List.iter [ false; true ] ~f:(fun attached ->
            let invoked = ref false in
            let revision = if attached then original else detached in
            let result =
              Preparation.with_revision
                policy
                revision
                ~env
                ~invocation:Ui
                ~checkpoint:ignore
                ~f:(fun _ ~compilation_limits:_ ~programs ->
                  assert (Option.is_none programs);
                  invoked := true;
                  Ok ())
            in
            let admitted = sufficient && attached in
            [%test_eq: bool] (Result.is_ok result) admitted;
            [%test_eq: bool] !invoked admitted));
      let identity = Failure "original runtime callback" in
      let observed =
        try
          ignore
            (Preparation.with_revision
               (Some generous)
               original
               ~env
               ~invocation:Ui
               ~checkpoint:ignore
               ~f:(fun _ ~compilation_limits:_ ~programs:_ -> raise identity));
          failwith "callback failure disappeared"
        with
        | exn -> exn
      in
      assert (phys_equal observed identity)))
;;

let%expect_test
    "resource preparation compiles captured modules and checks the handler's tool subset"
  =
  with_workspace (fun env parent root ->
    List.iter [ true; false ] ~f:(fun admitted ->
      let body =
        "let poison = fail(\"entry initializer must remain inert\")\n\
         let run = fun ctx input -> Task.pure(`Complete(`String(Graph.value)))"
      in
      save
        root
        "root.chatmd"
        (sprintf
           {|<tool name="read_file"/>
<script id="lookup" language="chatml" kind="tool" src="main.chatml" project="chatml.project.json"/>
<tool name="lookup" type="chatml" script="lookup" entrypoint="run" input_schema="schema.json" output_schema="schema.json">%s</tool>|}
           (if admitted then {|<uses tool="read_file"/>|} else ""));
      save root "main.chatml" body;
      save
        root
        "graph.chatml"
        "let poison = fail(\"module initializer must remain inert\")\n\
         let value = \"captured module\"\n\
         let hidden = 42";
      save root "graph.chatmli" "val value : string";
      save root "schema.json" "true";
      save
        root
        "chatml.project.json"
        {|{"schema_version":1,"namespace":"resource_example","capabilities":["read_file"],"modules":{"Main":{"implementation":"main.chatml"},"Graph":{"implementation":"graph.chatml","interface":"graph.chatmli"}}}|};
      Eio.Switch.run (fun sw ->
        let module B = Agent_session.Runtime_builder in
        let module CM = Prompt.Chat_markdown in
        let module EC = Chat_response.Extension_compiler in
        let definition = definition root in
        let catalog = catalog env parent in
        let candidate =
          prepare
            ~prepare_programs:(fun scope captured ->
              let parsed = Builder.Capture.root (Owner.Capture.value captured) in
              Script_tests.prepare env scope captured [ Script_tests.plan parsed ])
            catalog
            sw
            [ definition ]
          |> ok
        in
        Catalog.install_owned catalog candidate |> ok;
        let revision = revision catalog definition in
        let elements = Revision.elements revision in
        let parsed = Revision.parser_context revision |> Option.value_exn in
        assert (phys_equal elements (CM.Parsed.elements parsed));
        List.iter
          [ "root.chatmd"
          ; "main.chatml"
          ; "graph.chatml"
          ; "graph.chatmli"
          ; "schema.json"
          ; "chatml.project.json"
          ]
          ~f:(fun name -> Eio.Path.unlink Eio.Path.(root / name));
        let fixture =
          Inference_fixture.create
            ~namespace:(if admitted then "resource-admitted" else "resource-denied")
            ~default_model:"o3"
            ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "resource preparation inferred")
        in
        let target =
          Inference_fixture.capture_config
            fixture
            (Chat_response.Config.of_elements elements)
          |> ok
        in
        let paths : Agent_session.Runtime_paths.t =
          let borrowed_root = (root :> Eio.Fs.dir_ty Eio.Path.t) in
          { workspace = borrowed_root
          ; tool_dir = borrowed_root
          ; prompt_dir = Revision.materialized_tree revision
          ; session_dir = Eio.Path.(borrowed_root / "session")
          ; cache_dir = Eio.Path.(borrowed_root / "cache")
          ; home = borrowed_root
          }
        in
        Eio.Path.mkdirs ~perm:0o700 ~exists_ok:true paths.cache_dir;
        (* This example exercises managed project preparation. It does not
           request any readonly authoring-validation target. *)
        let authoring_validation_host =
          Chat_response.Authoring_validation.create_host
            ~runtime_identity:"resource-preparation-example"
            ~targets:[]
            ~moderator_surface:Ordinary
            ~compilation:Chatml_compilation.default_limits
          |> Result.ok_or_failwith
        in
        let resources =
          B.prepare_resources
            ~agent_source:None
            ~startup:None
            ~root_invocation:Ui
            ~script_preparation:(Some (policy ()))
            ~native_registrations:[]
            ~native_service_revision:None
            ~env
            ~inference_context:((Inference_fixture.resolve fixture) target |> ok)
            ~inference_identity:(Inference_fixture.identity fixture)
            ~on_inference_attempt:ignore
            ~on_inference_completion:ignore
            ~on_inference_observation:ignore
            ~sw
            ~paths
            ~storage_paths:paths
            ~revision
            ~session_id:(Agent_protocol.Id.Session.of_string "ses_resource" |> ok)
            ~one_off_policy:Chat_response.One_off_request.default_policy
            ~authoring_validation_host:(Some authoring_validation_host)
            ~manifest_authorizer:(fun _ -> failwith "resource fixture has no shell hooks")
            ~approval_provider:Shell_runtime.Approval_broker.None_available
            ~approval_store:(Shell_access.Approval.create_store ())
        in
        match resources with
        | Error error ->
          if admitted then failwith error.message;
          assert (String.is_substring error.message ~substring:"capability.not_selected");
          print_endline "required project tool absent from handler subset: rejected"
        | Ok resources ->
          assert admitted;
          assert (Option.is_some resources.managed);
          let definition = resources.definition |> Option.value_exn in
          (match EC.compiled_scripts definition with
           | [ (_, Prepared linked) ] ->
             let prepared = Chatml_linked_program.prepared linked in
             assert (List.length (Chatml_linked_program.units linked) = 2);
             assert (
               String.equal
                 (Chatml_source.Id.package_namespace
                    (Chatml_prepared_program.entry prepared))
                 "resource_example");
             let graph =
               List.find (Chatml_linked_program.units linked) ~f:(fun unit ->
                 String.equal
                   (Chatml_source.Id.relative_path
                      (Chatml_source.id (Chatml_linked_program.Unit.implementation unit)))
                   "graph.chatml")
               |> Option.value_exn
             in
             assert (Option.is_some (Chatml_linked_program.Unit.interface graph));
             print_endline "captured module and companion compiled after source removal"
           | [ (_, Single _) ] | [] | _ :: _ :: _ ->
             failwith "resources lost their actual configured graph"))));
  [%expect
    {|
    captured module and companion compiled after source removal
    required project tool absent from handler subset: rejected
    |}]
;;
