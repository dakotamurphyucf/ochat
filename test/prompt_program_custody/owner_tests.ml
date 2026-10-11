open Core

let%test_module "actual prompt and retained-child custody owners" =
  (module struct
    module C = Chatml_prompt_programs
    module S = Chatml_source
    module P = Chatml_prepared_program
    module CM = Prompt.Chat_markdown
    module B = Agent_session.Prompt_revision_builder
    module R = Agent_session.Prompt_revision
    module A = Agent_session.Authored_agent_source
    module Store = Agent_store.Prompt_artifact_store
    module Protocol = Agent_protocol
    module Contract = Chatml.Chatml_compiler_contract

    let ok = function
      | Ok value -> value
      | Error _ -> failwith "valid owner operation rejected"
    ;;

    let built = function
      | Ok value -> value
      | Error errors -> raise_s [%sexp (errors : B.Diagnostic.t list)]
    ;;

    let standard = Contract.for_target One_off_v1 ~checkpoint:ignore

    let native =
      Contract.for_static_surface
        (Chatml.Chatml_static_surface.of_runtime_surface
           Chatml.Chatml_builtin_surface.empty
           ~checkpoint:ignore)
        ~required_bindings:[]
    ;;

    let timestamp = Protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok

    let with_directory f =
      Eio_main.run (fun env ->
        let random = Cstruct.create 16 in
        Eio.Flow.read_exact (Eio.Stdenv.secure_random env) random;
        let suffix =
          Cstruct.to_string random
          |> String.to_list
          |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
          |> String.concat
        in
        let root = Eio.Path.(Eio.Stdenv.fs env / "/tmp" / ("ochat-custody." ^ suffix)) in
        Eio.Path.mkdir ~perm:0o700 root;
        Exn.protect
          ~f:(fun () -> f env root)
          ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root)))
    ;;

    let definition root =
      Agent_session.Prompt_definition.create
        ~id:(Protocol.Id.Prompt_definition.of_string "prd_program_custody" |> ok)
        ~config_name:"program-custody"
        ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
        ~allowed_workspaces:[]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> ok
    ;;

    let save root name body =
      Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / name) body
    ;;

    let graph contract body =
      let contract = Contract.fingerprint contract ~checkpoint:ignore in
      let id =
        S.Id.create ~package_namespace:"app" ~relative_path:"inline.chatml"
        |> Or_error.ok_exn
      in
      let source =
        S.create
          id
          ~contents:body
          ~expected_digest:None
          ~max_bytes:None
          ~checkpoint:ignore
        |> Or_error.ok_exn
      in
      let scope =
        P.Scope.create id ~bindings:[] ~selected_at:None ~checkpoint:ignore
        |> Or_error.ok_exn
      in
      P.create
        ~max_input_bytes:None
        ~compiler_contract:contract
        ~environment:
          (P.Environment.create ~name:"role" ~required_contract:contract ~declared_at:None
           |> Or_error.ok_exn)
        ~provenance:Implicit
        ~entry:id
        ~sources:[ source ]
        ~definitions:[]
        ~scopes:[ scope ]
        ~checkpoint:ignore
        ()
      |> ok
    ;;

    let binding parsed =
      List.hd_exn (CM.Parsed.declarations parsed)
      |> C.Binding.of_declaration parsed ~checkpoint:ignore
      |> Or_error.ok_exn
    ;;

    let contexts capture =
      B.Capture.root capture :: List.map (B.Capture.agents capture) ~f:snd
    ;;

    let authored capture =
      let root = CM.Parsed.root_input (B.Capture.root capture) in
      Chatmd_source_bundle.create
        ~root_file:(Chatmd_parser_input.path root)
        ~sources:
          ((Chatmd_parser_input.path root, Chatmd_parser_input.authored root)
           :: B.Capture.sources capture)
        ()
      |> ok
    ;;

    let prepare contract capture =
      let child =
        List.Assoc.find_exn (B.Capture.agents capture) "child.chatmd" ~equal:String.equal
      in
      let declaration = List.hd_exn (CM.Parsed.declarations child) in
      C.capture
        (authored capture)
        ~parsed_contexts:(contexts capture)
        ~root:(CM.Parsed.root_input (B.Capture.root capture))
        ~programs:
          [ ( binding child
            , C.Input.Provided (graph contract (CM.Declaration.script_body declaration)) )
          ]
        ~active_programs:[]
        ~limits:(Prompt_program_custody.limits ())
        ~checkpoint:Eio.Fiber.check
      |> Result.map_error ~f:(fun _ ->
        [ B.Diagnostic.
            { code = "test.prepare"; message = "Invalid fixture"; source = None }
        ])
    ;;

    let configure root =
      save
        root
        "root.chatmd"
        "<developer>Parent</developer><tool name=\"child\" agent=\"child.chatmd\" local \
         persistence=\"optional\"/>";
      save
        root
        "child.chatmd"
        "<developer>Child</developer><script id=\"m\" kind=\"moderator\" \
         language=\"chatml\">let main = 0</script>"
    ;;

    let%expect_test "fresh and retained-child graph identity survives artifact restart" =
      with_directory (fun env root ->
        configure root;
        let definition = definition root in
        let store =
          Store.create ~env ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
          |> ok
        in
        let serial = ref 0 in
        let transaction () =
          incr serial;
          Protocol.Id.Transaction.of_string (sprintf "txn_custody_%d" !serial) |> ok
        in
        let build contract =
          B.build
            ~prepare_programs:(prepare contract)
            ~env
            ~artifact_store:store
            ~transaction_id:(transaction ())
            ~created_at:timestamp
            definition
          |> built
        in
        let parent = build standard
        and another = build native in
        assert (
          String.equal (R.artifact parent).root_chatmd (R.artifact another).root_chatmd);
        assert (not (Protocol.Id.Prompt_revision.equal (R.id parent) (R.id another)));
        let source = A.capture ~parent ~tool_name:"child" |> ok in
        let base = A.resource_revision ~parent source |> ok in
        let custody = R.programs base |> Option.value_exn in
        let parsed = R.parser_context base |> Option.value_exn in
        let selected_binding = binding parsed in
        let prepared = graph native "let main = 0" in
        let extended =
          C.extend
            custody
            ~authored:(A.authored_sources source)
            ~parsed_contexts:[ parsed ]
            ~programs:[ selected_binding, C.Input.Provided prepared ]
            ~limits:(Prompt_program_custody.limits ())
            ~checkpoint:Eio.Fiber.check
          |> ok
        in
        let selected =
          C.reroot
            extended
            ~authored:(A.authored_sources source)
            ~parsed
            ~programs:
              [ ( selected_binding
                , Contract.fingerprint native ~checkpoint:ignore
                , selected_binding )
              ]
            ~limits:(Prompt_program_custody.limits ())
            ~checkpoint:Eio.Fiber.check
          |> ok
        in
        let storage = C.to_storage selected in
        let metadata =
          Agent_store.Prompt_program_bundle.Manifest.metadata
            (Agent_store.Prompt_program_bundle.manifest storage)
        in
        let padded length =
          match Document_schema.Document.json metadata with
          | `Object fields ->
            `Object (("padding", `String (String.make length 'x')) :: fields)
          | _ -> assert false
        in
        let fixed =
          Document_schema.Json.validate_and_measure
            ~limits:Document_schema.Limits.default
            (padded 0)
          |> ok
        in
        let oversized_metadata =
          Document_schema.Document.inspect
            ~limits:Document_schema.Limits.default
            (padded (262144 - fixed))
          |> ok
        in
        [%test_eq: int]
          (Document_schema.Json.validate_and_measure
             ~limits:Document_schema.Limits.default
             (Document_schema.Document.json oversized_metadata)
           |> ok)
          262144;
        let oversized_storage =
          Agent_store.Prompt_program_bundle.create
            ~metadata:oversized_metadata
            ~payloads:(Agent_store.Prompt_program_bundle.payloads storage)
          |> ok
        in
        let oversized =
          C.of_storage
            oversized_storage
            ~authored:(A.authored_sources source)
            ~limits:(Prompt_program_custody.limits ())
            ~checkpoint:Eio.Fiber.check
          |> ok
        in
        (match
           Prompt_program_custody.fingerprint oversized ~checkpoint:Eio.Fiber.check
         with
         | Error (C.Error.Document _) -> ()
         | _ -> failwith "Complete manifest admission was not typed");
        (match A.with_programs source oversized with
         | Error _ -> ()
         | Ok _ -> failwith "Oversized child manifest admitted");
        let changed = A.with_programs source selected |> ok in
        assert (A.Identity.equal (A.identity source) (A.identity changed));
        assert (not (String.equal (A.fingerprint source) (A.fingerprint changed)));
        let resource = A.resource_revision ~parent changed |> ok in
        assert (not (Protocol.Id.Prompt_revision.equal (R.id base) (R.id resource)));
        let artifact =
          A.artifact changed ~revision_id:(R.id resource) ~created_at:timestamp |> ok
        in
        Store.install store ~transaction_id:(transaction ()) artifact |> ok;
        Eio.Path.unlink Eio.Path.(root / "root.chatmd");
        Eio.Path.unlink Eio.Path.(root / "child.chatmd");
        let restarted =
          B.restore ~artifact_store:store definition artifact.revision_id |> built
        in
        let parsed = R.parser_context restarted |> Option.value_exn in
        let retained = R.programs restarted |> Option.value_exn in
        let restored =
          C.restore
            retained
            ~parsed
            ~contracts:[ binding parsed, native ]
            ~limits:
              (C.Limits.create
                 ~max_programs:8
                 ~max_parser_inputs:16
                 ~max_logical_sources:32
                 ~max_total_input_bytes:(8 * 1024 * 1024)
                 ~project_work_steps:100_000
                 ~document:Document_schema.Limits.default
               |> Or_error.ok_exn)
            ~checkpoint:Eio.Fiber.check
          |> ok
        in
        [%test_eq: int] (List.length (C.bindings retained)) 2;
        [%test_eq: int] (List.length (C.Restored.programs restored)) 1;
        assert (
          S.Digest.equal
            (P.fingerprint prepared)
            (P.fingerprint
               (C.Restored.Program.prepared (List.hd_exn (C.Restored.programs restored)))));
        printf "fresh-raw-equal=true graph-revisions-distinct=true\n";
        printf "child-origin-equal=true role-fingerprints-distinct=true\n";
        printf "restart-originals-removed=true retained-variants=2 active-programs=1\n");
      [%expect
        {|
      fresh-raw-equal=true graph-revisions-distinct=true
      child-origin-equal=true role-fingerprints-distinct=true
      restart-originals-removed=true retained-variants=2 active-programs=1
      |}]
    ;;

    let%test_unit
        "parser-only restart and complete effective-input fingerprint use actual owners"
      =
      with_directory (fun env root ->
        configure root;
        let definition = definition root in
        let store =
          Store.create ~env ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
          |> ok
        in
        let revision =
          B.build
            ~env
            ~artifact_store:store
            ~transaction_id:(Protocol.Id.Transaction.of_string "txn_parser_only" |> ok)
            ~created_at:timestamp
            definition
          |> built
        in
        let restarted =
          B.restore ~artifact_store:store definition (R.id revision) |> built
        in
        [%test_eq: int]
          (List.length (C.bindings (R.programs restarted |> Option.value_exn)))
          0;
        let source = A.capture ~parent:restarted ~tool_name:"child" |> ok in
        let child = A.resource_revision ~parent:restarted source |> ok in
        assert (
          String.equal
            (Chatmd_parser_input.path
               (C.root_input (R.programs child |> Option.value_exn)))
            "child.chatmd");
        let capture prefix =
          B.capture
            ~env
            ~preprocess:(fun raw -> prefix ^ raw)
            ~limits:
              (CM.Limits.create
                 ~max_input_bytes:(8 * 1024 * 1024)
                 ~max_parse_calls:64
                 ~max_tokens:10_000
                 ~max_depth:32
               |> Or_error.ok_exn)
            ~checkpoint:Eio.Fiber.check
            definition
          |> built
        in
        let first = capture "<!-- first -->"
        and second = capture "<!-- second -->" in
        let custody capture =
          C.capture
            (authored capture)
            ~parsed_contexts:(contexts capture)
            ~root:(CM.Parsed.root_input (B.Capture.root capture))
            ~programs:[]
            ~active_programs:[]
            ~limits:(Prompt_program_custody.limits ())
            ~checkpoint:Eio.Fiber.check
          |> ok
        in
        assert (
          String.equal
            (Chatmd_parser_input.authored (CM.Parsed.root_input (B.Capture.root first)))
            (Chatmd_parser_input.authored (CM.Parsed.root_input (B.Capture.root second))));
        assert (
          not
            (String.equal
               (Prompt_program_custody.fingerprint
                  (custody first)
                  ~checkpoint:Eio.Fiber.check
                |> ok)
               (Prompt_program_custody.fingerprint
                  (custody second)
                  ~checkpoint:Eio.Fiber.check
                |> ok)));
        let exception Sentinel in
        match
          B.build
            ~prepare_programs:(fun _ -> raise Sentinel)
            ~env
            ~artifact_store:store
            ~transaction_id:
              (Protocol.Id.Transaction.of_string "txn_callback_failure" |> ok)
            ~created_at:timestamp
            definition
        with
        | exception Sentinel -> ()
        | _ -> failwith "Preparation exception identity lost")
    ;;
  end)
;;
