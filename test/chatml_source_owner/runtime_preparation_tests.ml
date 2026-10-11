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
                ~delegated_moderator:false
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
               ~delegated_moderator:false
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
