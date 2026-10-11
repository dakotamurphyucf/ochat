open! Core

let%test_module "actual prompt content custody owners" =
  (module struct
    module P = Agent_protocol
    module Owner = Agent_session.Prompt_source_owner
    module Builder = Agent_session.Prompt_revision_builder
    module Revision = Agent_session.Prompt_revision
    module Authored = Agent_session.Authored_agent_source
    module Store = Agent_store.Prompt_artifact_store
    module Content = Chatmd_prompt_content
    module Native = Chatmd_content_capture
    module CM = Prompt.Chat_markdown

    let () = Mirage_crypto_rng_unix.use_default ()

    let ok = function
      | Ok value -> value
      | Error _ -> failwith "valid fixture rejected"
    ;;

    let built = function
      | Ok value -> value
      | Error errors -> raise_s [%sexp (errors : Builder.Diagnostic.t list)]
    ;;

    let timestamp = P.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok

    let limits ?(work = 1000000) () =
      Content.Limits.create
        ~max_occurrences:32
        ~max_parser_inputs:32
        ~max_input_bytes:100000
        ~max_output_bytes:100000
        ~max_work_steps:work
        ~max_nodes:100000
        ~max_depth:64
        ~max_payloads:32
        ~max_payload_bytes:100000
        ~document:Document_schema.Limits.default
      |> Or_error.ok_exn
    ;;

    let with_fixture f =
      Eio_main.run (fun env ->
        let random = Cstruct.create 16 in
        Eio.Flow.read_exact env#secure_random random;
        let suffix =
          Cstruct.to_string random
          |> String.to_list
          |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
          |> String.concat
        in
        let directory = Eio.Path.(env#fs / "/tmp" / ("och221-owner-" ^ suffix)) in
        Eio.Path.mkdir ~perm:0o700 directory;
        Fun.protect
          ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory))
          (fun () ->
             Eio.Path.with_open_dir directory (fun root ->
               let save name body =
                 Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / name) body
               in
               save
                 "root.chatmd"
                 {|<developer><doc src="guide.md" local capture/></developer><tool name="child" agent="child.chatmd" local persistence="optional"/>|};
               save
                 "child.chatmd"
                 {|<developer><doc src="guide.md" local capture/></developer>|};
               save "guide.md" "original";
               let definition =
                 Agent_session.Prompt_definition.create
                   ~id:(P.Id.Prompt_definition.of_string "prd_content_fixture" |> ok)
                   ~config_name:"content-fixture"
                   ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
                   ~allowed_workspaces:[]
                   ~permission_profile:"interactive"
                   ~runtime_policy:None
                   ~enabled:true
                   ~description:None
                 |> ok
               in
               let receipt =
                 Agent_store.Prompt_source_receipt.create
                   ~definition_id:definition.id
                   ~root_file:definition.root_file
                   ~max_path_bytes:4096
                   ~checkpoint:ignore
                 |> Or_error.ok_exn
               in
               let policy =
                 Owner.Policy.create
                   receipt
                   ~limits:
                     (Owner.Limits.create ~max_path_components:32 ~max_runtime_leases:4
                      |> Or_error.ok_exn)
               in
               let store =
                 Store.create
                   ~env
                   ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
                 |> ok
               in
               Eio.Switch.run (fun sw ->
                 let owner = Owner.open_owned root ~sw ~policy ~checkpoint:ignore |> ok in
                 f env root save definition receipt store owner))))
    ;;

    let acquire ?(work = 1000000) env owner definition =
      Owner.with_scope owner ~f:(fun scope ->
        let open Result.Let_syntax in
        let%bind captured =
          Owner.Scope.capture
            scope
            ~env
            ~definition
            ~preprocess:Fn.id
            ~limits:
              (CM.Limits.create
                 ~max_input_bytes:100000
                 ~max_parse_calls:32
                 ~max_tokens:10000
                 ~max_depth:32
               |> Or_error.ok_exn)
            ~checkpoint:ignore
        in
        let%map content =
          Owner.Scope.capture_documents
            scope
            ~capture:captured
            ~limits:(limits ~work ())
            ~checkpoint:ignore
        in
        captured, content)
      |> ok
    ;;

    let publish ?prepare_content captured definition receipt store =
      Builder.build_captured
        ?prepare_content
        ~capture:(Owner.Capture.value captured)
        ~source_receipt:receipt
        ~artifact_store:store
        ~transaction_id:(P.Id.Transaction.create ())
        ~created_at:timestamp
        definition
    ;;

    let without_content env root revision =
      let original = Revision.artifact revision in
      let stripped =
        Store.Artifact.create
          ~revision_id:original.revision_id
          ?prompt_definition_id:original.prompt_definition_id
          ?source_receipt:original.source_receipt
          ?canonical_source:original.canonical_source
          ~root_relative_path:original.root_relative_path
          ~root_chatmd:original.root_chatmd
          ~sources:original.sources
          ~parser_schema_version:original.parser_schema_version
          ~runtime_schema_version:original.runtime_schema_version
          ?shell_manifest_sha256:original.shell_manifest_sha256
          ?program_bundle:original.program_bundle
          ~created_at:original.created_at
          ()
        |> ok
      in
      let store =
        Store.create ~env ~root:(Eio.Path.native_exn Eio.Path.(root / "missing-content"))
        |> ok
      in
      Store.install store ~transaction_id:(P.Id.Transaction.create ()) stripped |> ok;
      (* The low storage domain admits this complete inventory; authentic parser
         semantics must independently reject the missing captured slot payload. *)
      assert (Result.is_ok (Store.load store original.revision_id));
      store
    ;;

    let reject_missing_content env root definition revision =
      let store = without_content env root revision in
      assert (
        Result.is_error
          (Builder.restore
             ~content_limits:(limits ())
             ~artifact_store:store
             definition
             (Revision.id revision)))
    ;;

    let literals revision =
      List.concat_map (Revision.elements revision) ~f:(function
        | CM.User msg | System msg | Developer msg | Msg msg | Assistant msg ->
          (match msg.content with
           | Some (Items items) ->
             List.filter_map items ~f:(function
               | Basic value ->
                 assert (Option.is_none value.document_url);
                 value.text
               | Agent _ -> failwith "unexpected fixture agent item")
           | Some (Text text) -> [ text ]
           | None -> [])
        | _ -> [])
    ;;

    let%expect_test
        "captured literal identity and retained child replay survive source removal"
      =
      with_fixture (fun env root save definition receipt store owner ->
        List.iter [ "definitions"; "decoys" ] ~f:(fun name ->
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / name));
        save
          "root.chatmd"
          {|<developer><doc src="guide.md" local capture/><tool name="child" agent="decoys/child.chatmd" local persistence="optional"/></developer><import src="definitions/tools.chatmd"/>|};
        save
          "definitions/tools.chatmd"
          {|<tool name="child" agent="../child.chatmd" local persistence="optional"/>|};
        save "decoys/child.chatmd" {|<developer>Unused same-name child</developer>|};
        let parent =
          Eio.Switch.run (fun sw ->
            let module Catalog = Agent_session.Prompt_catalog in
            let catalog =
              Catalog.create ~env ~artifact_store:store ~content_limits:(Some (limits ()))
            in
            let candidate =
              Catalog.prepare_owned
                catalog
                ~prepare_content:(fun scope capture ->
                  Ok
                    (Owner.Scope.capture_documents
                       scope
                       ~capture
                       ~limits:(limits ())
                       ~checkpoint:ignore
                     |> ok))
                ~sw
                ~source_limits:
                  (Owner.Limits.create ~max_path_components:64 ~max_runtime_leases:8
                   |> Or_error.ok_exn)
                ~max_roots:1
                ~checkpoint:ignore
                ~transaction_id:(fun _ -> P.Id.Transaction.create ())
                ~created_at:timestamp
                [ definition ]
              |> ok
            in
            Catalog.install_owned catalog candidate |> ok;
            match Catalog.find catalog definition.id with
            | Some { availability = Ready revision; _ } -> revision
            | _ -> failwith "captured document catalog entry unavailable")
        in
        assert (List.equal String.equal (literals parent) [ "original" ]);
        reject_missing_content env root definition parent;
        let projection = Option.value_exn (Revision.document_projection parent) in
        assert (
          phys_equal
            (CM.Document_projection.parsed projection)
            (Option.value_exn (Revision.parser_context parent)));
        assert (
          phys_equal
            (CM.Document_projection.elements projection)
            (Revision.elements parent));
        assert (
          List.is_empty
            (Chatml_prompt_programs.bindings
               (Option.value_exn (Revision.programs parent))));
        let history =
          Chat_response.Initial_prompt_history.create
            ~session_id:(P.Id.Session.of_string "ses_content" |> ok)
            (Revision.elements parent)
        in
        assert (Result.is_ok history);
        save "guide.md" "edited";
        let newer_capture, newer_content = acquire env owner definition in
        let newer =
          publish newer_capture definition receipt store ~prepare_content:(fun _ ->
            Ok newer_content)
          |> built
        in
        assert (
          String.equal
            (Revision.artifact parent).root_chatmd
            (Revision.artifact newer).root_chatmd);
        assert (not (P.Id.Prompt_revision.equal (Revision.id parent) (Revision.id newer)));
        assert (List.equal String.equal (literals parent) [ "original" ]);
        assert (List.equal String.equal (literals newer) [ "edited" ]);
        Owner.close owner;
        List.iter
          [ "root.chatmd"
          ; "child.chatmd"
          ; "guide.md"
          ; "definitions/tools.chatmd"
          ; "decoys/child.chatmd"
          ]
          ~f:(fun name -> Eio.Path.unlink Eio.Path.(root / name));
        assert (
          Result.is_error
            (Builder.restore ~artifact_store:store definition (Revision.id parent)));
        let restarted =
          let module Catalog = Agent_session.Prompt_catalog in
          let restore content_limits =
            let catalog = Catalog.create ~env ~artifact_store:store ~content_limits in
            let prepared =
              Catalog.prepare
                catalog
                ~transaction_id:(fun _ -> P.Id.Transaction.create ())
                ~created_at:timestamp
                [ definition ]
            in
            Catalog.install catalog prepared;
            assert (Option.is_none (Catalog.find_revision catalog (Revision.id parent)));
            let result =
              Catalog.restore_revision
                catalog
                ~definition_id:definition.id
                ~revision_id:(Revision.id parent)
            in
            (match result with
             | Error _ ->
               assert (Option.is_none (Catalog.find_revision catalog (Revision.id parent)))
             | Ok revision ->
               assert (
                 phys_equal
                   revision
                   (Catalog.restore_revision
                      catalog
                      ~definition_id:definition.id
                      ~revision_id:(Revision.id parent)
                    |> built)));
            result
          in
          assert (Result.is_error (restore None));
          assert (Result.is_error (restore (Some (limits ~work:1 ()))));
          restore (Some (limits ())) |> built
        in
        assert (List.equal String.equal (literals restarted) [ "original" ]);
        assert (Result.is_error (Authored.capture ~parent:restarted ~tool_name:"child" ()));
        let child_source =
          Authored.capture
            ~content_limits:(limits ())
            ~parent:restarted
            ~tool_name:"child"
            ()
          |> ok
        in
        assert (
          String.equal (Authored.identity child_source).root_relative_path "child.chatmd");
        (* Imported declaration metadata follows the installed parser owner;
           neither a relocated native prefix nor the ignored same-name tool is
           a captured source key. The originals have already been removed. *)
        let moved_store =
          Store.create
            ~env
            ~root:(Eio.Path.native_exn Eio.Path.(root / "moved-artifacts"))
          |> ok
        in
        Store.install
          moved_store
          ~transaction_id:(P.Id.Transaction.create ())
          (Revision.artifact restarted)
        |> ok;
        let moved =
          Builder.restore
            ~content_limits:(limits ())
            ~artifact_store:moved_store
            definition
            (Revision.id restarted)
          |> built
        in
        let moved_source =
          Authored.capture ~content_limits:(limits ()) ~parent:moved ~tool_name:"child" ()
          |> ok
        in
        assert (
          Authored.Identity.equal
            (Authored.identity child_source)
            (Authored.identity moved_source));
        assert (
          String.equal
            (Authored.fingerprint child_source)
            (Authored.fingerprint moved_source));
        let child = Authored.resource_revision ~parent:restarted child_source |> ok in
        assert (List.equal String.equal (literals child) [ "original" ]);
        let child_again =
          Authored.resource_revision ~parent:restarted child_source |> ok
        in
        assert (
          phys_equal
            (Option.value_exn (Revision.parser_context child))
            (Option.value_exn (Revision.parser_context child_again)));
        assert (
          phys_equal
            (Option.value_exn (Revision.document_projection child))
            (Option.value_exn (Revision.document_projection child_again)));
        assert (
          Result.is_error
            (Authored.with_programs
               child_source
               (Option.value_exn (Revision.programs restarted))));
        let adopted =
          Authored.with_programs child_source (Option.value_exn (Revision.programs child))
          |> ok
        in
        let adopted_child = Authored.resource_revision ~parent:restarted adopted |> ok in
        assert (
          phys_equal
            (Option.value_exn (Revision.document_projection child))
            (Option.value_exn (Revision.document_projection adopted_child)));
        let artifact =
          Authored.artifact
            child_source
            ~revision_id:(Revision.id child)
            ~created_at:timestamp
          |> ok
        in
        Store.install store ~transaction_id:(P.Id.Transaction.create ()) artifact |> ok;
        let child_restart =
          Builder.restore
            ~content_limits:(limits ())
            ~artifact_store:store
            definition
            (Revision.id child)
          |> built
        in
        assert (List.equal String.equal (literals child_restart) [ "original" ]);
        print_s
          [%sexp
            { retained_parent = (literals restarted : string list)
            ; fresh_parent = (literals newer : string list)
            ; retained_child = (literals child_restart : string list)
            ; active_programs = 0
            ; history_literal = true
            }]);
      [%expect
        {|
        ((retained_parent (original)) (fresh_parent (edited))
         (retained_child (original)) (active_programs 0) (history_literal true))
        |}]
    ;;

    let%test_unit "inactive captured child cannot inherit missing parent content" =
      with_fixture (fun env root save definition receipt store owner ->
        save
          "root.chatmd"
          {|<developer>parent only</developer><tool name="child" agent="child.chatmd" local persistence="optional"/>|};
        let captured, content = acquire env owner definition in
        let parent =
          publish captured definition receipt store ~prepare_content:(fun _ -> Ok content)
          |> built
        in
        let missing = without_content env root parent in
        let restarted =
          Builder.restore ~artifact_store:missing definition (Revision.id parent) |> built
        in
        assert (
          List.is_empty
            (CM.Parsed.document_occurrences
               (Option.value_exn (Revision.parser_context restarted))));
        assert (
          Result.is_error
            (Authored.capture
               ~content_limits:(limits ())
               ~parent:restarted
               ~tool_name:"child"
               ())))
    ;;

    let%test_unit "actual bundled instruction capture and revoked native scope" =
      with_fixture (fun env root save definition receipt store owner ->
        save "root.chatmd" {|<developer><doc src="builtin:chatml"/></developer>|};
        let captured, content = acquire env owner definition in
        let asset =
          Builtin_instructions.find
            (Builtin_instructions.Id.of_string "chatml" |> Or_error.ok_exn)
          |> ok
        in
        let parent =
          publish captured definition receipt store ~prepare_content:(fun _ -> Ok content)
          |> built
        in
        assert (
          List.equal
            String.equal
            (literals parent)
            [ Builtin_instructions.Asset.contents asset ]);
        assert (Int.equal (Native.Accounting.occurrences (Native.accounting content)) 1);
        assert (
          Int.equal
            (Native.Accounting.output_bytes (Native.accounting content))
            (Builtin_instructions.Asset.byte_length asset));
        reject_missing_content env root definition parent;
        Eio.Switch.run (fun sw ->
          let other =
            Owner.open_owned root ~sw ~policy:(Owner.policy owner) ~checkpoint:ignore
            |> ok
          in
          assert (
            Result.is_error
              (Owner.with_scope other ~f:(fun scope ->
                 Owner.Scope.capture_documents
                   scope
                   ~capture:captured
                   ~limits:(limits ())
                   ~checkpoint:ignore))));
        Owner.Policy.revoke (Owner.policy owner);
        assert (
          Result.is_error
            (Owner.with_scope owner ~f:(fun scope ->
               Owner.Scope.capture_documents
                 scope
                 ~capture:captured
                 ~limits:(limits ())
                 ~checkpoint:ignore)));
        Eio.Path.unlink Eio.Path.(root / "root.chatmd");
        let restarted =
          Builder.restore
            ~content_limits:(limits ())
            ~artifact_store:store
            definition
            (Revision.id parent)
          |> built
        in
        assert (
          List.equal
            String.equal
            (literals restarted)
            [ Builtin_instructions.Asset.contents asset ]))
    ;;

    let%test_unit
        "missing producer callback identity and complete helper work fail before install"
      =
      with_fixture (fun env root _ definition receipt store owner ->
        let captured, content = acquire env owner definition in
        assert (Result.is_error (publish captured definition receipt store));
        let marker = Failure "host preparation" in
        (match
           publish captured definition receipt store ~prepare_content:(fun _ ->
             raise marker)
         with
         | exception exn -> assert (phys_equal marker exn)
         | _ -> failwith "producer exception was mapped");
        let parent =
          publish captured definition receipt store ~prepare_content:(fun _ -> Ok content)
          |> built
        in
        let capture = Owner.Capture.value captured in
        let contexts =
          Builder.Capture.root capture :: List.map (Builder.Capture.agents capture) ~f:snd
        in
        let input = CM.Parsed.root_input (Builder.Capture.root capture) in
        let authored =
          Agent_session.Prompt_program_custody.authored
            ~root_file:(Chatmd_parser_input.path input)
            ~sources:
              ((Chatmd_parser_input.path input, Chatmd_parser_input.authored input)
               :: Builder.Capture.sources capture)
          |> Or_error.ok_exn
        in
        let programs =
          Agent_session.Prompt_program_custody.prepare
            authored
            ~parsed_contexts:contexts
            ~root:input
            ~supplied:None
            ~checkpoint:ignore
          |> ok
        in
        let prepare bounds =
          Agent_session.Prompt_content_custody.prepare
            ~parsed_contexts:contexts
            ~programs
            ~supplied:(Some content)
            ~limits:bounds
            ~checkpoint:ignore
        in
        let completed = prepare (limits ()) |> ok in
        let work = Agent_session.Prompt_content_custody.Prepared.work_steps completed in
        assert (work > Native.Accounting.work_steps (Native.accounting content));
        assert (Result.is_ok (prepare (limits ~work ())));
        assert (Result.is_error (prepare (limits ~work:(work - 1) ())));
        let full =
          Agent_session.Prompt_content_custody.replay
            completed
            ~parsed:(Option.value_exn (Revision.parser_context parent))
            ~checkpoint:ignore
          |> ok
        in
        let full_work = Agent_session.Prompt_content_custody.Restored.work_steps full in
        assert (full_work > work);
        let exact_capture, exact_content = acquire ~work:full_work env owner definition in
        let make_store name =
          Store.create ~env ~root:(Eio.Path.native_exn Eio.Path.(root / name)) |> ok
        in
        let exact_store = make_store "exact" in
        assert (not (Store.exists exact_store (Revision.id parent)));
        let exact =
          publish exact_capture definition receipt exact_store ~prepare_content:(fun _ ->
            Ok exact_content)
          |> built
        in
        assert (P.Id.Prompt_revision.equal (Revision.id parent) (Revision.id exact));
        assert (Store.exists exact_store (Revision.id parent));
        let below_capture, below_content =
          acquire ~work:(full_work - 1) env owner definition
        in
        let below_store = make_store "below" in
        assert (
          Result.is_error
            (publish
               below_capture
               definition
               receipt
               below_store
               ~prepare_content:(fun _ -> Ok below_content)));
        assert (not (Store.exists below_store (Revision.id parent)));
        let original = Option.value_exn (Revision.content parent) in
        let cloned =
          Content.of_storage
            (Content.to_storage original)
            ~inputs:(Chatml_prompt_programs.parser_inputs programs)
            ~limits:(limits ())
            ~checkpoint:ignore
          |> ok
        in
        let foreign =
          Content.materialize
            cloned
            ~parsed:(Option.value_exn (Revision.parser_context parent))
            ~limits:(limits ())
            ~checkpoint:ignore
          |> ok
        in
        assert (
          Result.is_error
            (Revision.with_content
               parent
               ~content:original
               ~materialized:foreign
               ~checkpoint:ignore)))
    ;;
  end)
;;
