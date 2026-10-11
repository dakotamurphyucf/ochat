open Core
open Support
module CM = Prompt.Chat_markdown
module Revision = Agent_session.Prompt_revision
module Child = Agent_session.Captured_child_source
module Preparation = Agent_session.Runtime_script_preparation
module C = Chatml_prompt_programs

let content_limits =
  Chatmd_prompt_content.Limits.create
    ~max_occurrences:32
    ~max_parser_inputs:32
    ~max_input_bytes:1_000_000
    ~max_output_bytes:1_000_000
    ~max_work_steps:1_000_000
    ~max_nodes:10_000
    ~max_depth:32
    ~max_payloads:32
    ~max_payload_bytes:1_000_000
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let with_parent f =
  with_workspace (fun env directory root ->
    save
      root
      "root.chatmd"
      {|<tool name="ordinary" agent="child.chatmd" local/>
<tool name="persistent" agent="child.chatmd" local persistence="optional"/>
<user><agent src="child.chatmd" local><doc src="notes.md" local capture/></agent></user>|};
    save
      root
      "child.chatmd"
      {|<user><doc src="notes.md" local capture/></user>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1" project="chatml.project.json">let captured_answer = Numbers.answer
let initial_state = fail("initializer must remain inert")
let on_event = fun ctx state event -> Task.pure(state)</script>|};
    save root "notes.md" "retained document";
    save root "numbers.chatml" "let answer = 42";
    save root "numbers.chatmli" "val answer : int";
    save
      root
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"child","modules":{"Numbers":{"implementation":"numbers.chatml","interface":"numbers.chatmli"}}}|};
    Eio.Switch.run (fun sw ->
      let definition = definition root in
      let catalog = catalog env directory in
      let candidate =
        Catalog.prepare_owned
          catalog
          ~prepare_programs:(fun scope captured ->
            let value = Owner.Capture.value captured in
            let child = snd (List.hd_exn (Builder.Capture.agents value)) in
            Script_tests.prepare
              env
              scope
              captured
              [ Script_tests.plan (Builder.Capture.root value)
              ; Script_tests.plan
                  ~moderator_invocation:Headless
                  ~moderator_surface:Chatml.Chatml_builtin_surface.moderator_surface
                  child
              ])
          ~prepare_content:(fun scope captured ->
            Owner.Scope.capture_documents
              scope
              ~capture:captured
              ~limits:content_limits
              ~checkpoint:ignore
            |> Result.map_error ~f:(fun error ->
              [ Builder.Diagnostic.
                  { code = "fixture.capture_content"
                  ; message = Sexp.to_string_hum (Owner.Error.sexp_of_t error)
                  ; source = None
                  }
              ]))
          ~sw
          ~source_limits:(limits ())
          ~max_roots:8
          ~checkpoint:ignore
          ~transaction_id:(fun _ -> Agent_protocol.Id.Transaction.create ())
          ~created_at:(Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok)
          [ definition ]
        |> ok
      in
      Catalog.install_owned catalog candidate |> ok;
      f env root (revision catalog definition)))
;;

let references parent =
  CM.Parsed.agent_references (Revision.parser_context parent |> Option.value_exn)
;;

let tool_reference parent =
  references parent
  |> List.find_exn ~f:(fun reference ->
    match CM.Agent_reference.invocation reference with
    | Tool _ -> true
    | Persistent_tool _ | Content _ -> false)
;;

let projected_content_reference parent =
  let parsed = Revision.parser_context parent |> Option.value_exn in
  let projection = Revision.document_projection parent |> Option.value_exn in
  let projected =
    List.find_map (CM.Document_projection.elements projection) ~f:(function
      | CM.User { content = Some (Items items); _ } ->
        List.find_map items ~f:(function
          | Agent descriptor -> Some descriptor
          | Basic _ -> None)
      | _ -> None)
    |> Option.value_exn
  in
  CM.content_agent_reference
    parsed
    ~projection
    ~max_work_steps:100_000
    ~checkpoint:ignore
    projected
  |> Or_error.ok_exn
;;

let run_child env parent reference ~f =
  Preparation.with_captured_child
    (Runtime_preparation_tests.policy ())
    parent
    ~reference
    ~invocation:Headless
    ~content_limits:(Some content_limits)
    ~env
    ~checkpoint:ignore
    ~f
;;

let%expect_test "ordinary and projected content use their captured child graph" =
  with_parent (fun env root parent ->
    let tool = tool_reference parent in
    let content = projected_content_reference parent in
    assert (not (phys_equal tool content));
    List.iter
      [ "root.chatmd"
      ; "child.chatmd"
      ; "notes.md"
      ; "numbers.chatml"
      ; "numbers.chatmli"
      ; "chatml.project.json"
      ]
      ~f:(fun path -> Eio.Path.unlink Eio.Path.(root / path));
    List.iter [ tool; content ] ~f:(fun reference ->
      let borrowed = ref None in
      run_child
        env
        parent
        reference
        ~f:(fun startup ~revision ~compilation_limits:_ ~programs ->
          borrowed := Some startup;
          let parsed = Revision.parser_context revision |> Option.value_exn in
          assert (
            not (phys_equal parsed (Option.value_exn (Revision.parser_context parent))));
          [%test_eq: string] "child.chatmd" (Revision.root_relative_path revision);
          assert (Option.is_some (Revision.document_projection revision));
          let programs = C.Restored.programs (Option.value_exn programs) in
          [%test_eq: int] 1 (List.length programs);
          let prepared = C.Restored.Program.prepared (List.hd_exn programs) in
          let definitions = Chatml_prepared_program.definitions prepared in
          [%test_eq: int] 1 (List.length definitions);
          assert (
            Option.is_some
              (Chatml_prepared_program.Definition.interface (List.hd_exn definitions)));
          Ok ())
      |> ok;
      assert (
        Result.is_error
          (Chat_response.Startup_compilation.remaining_limits
             (Option.value_exn !borrowed))));
    print_endline
      "tool and projected content: child graph + retained document, originals absent");
  [%expect
    {| tool and projected content: child graph + retained document, originals absent |}]
;;

let%test_unit "captured child rejects foreign parser and missing content policy" =
  with_parent (fun _ _ parent ->
    let original = Revision.parser_context parent |> Option.value_exn in
    let artifact = Revision.artifact parent in
    let sources =
      (artifact.root_relative_path, artifact.root_chatmd)
      :: List.map artifact.sources ~f:(fun source ->
        source.Agent_store.Prompt_artifact_store.Source.relative_path, source.contents)
    in
    let clone =
      CM.replay_chat_inputs
        (CM.Parsed.root_input original)
        ~source_loader:
          (Source_loader.captured_filesystem
             ~root:(Revision.materialized_tree parent)
             ~sources)
        ~dir:(Revision.materialized_tree parent)
        ~limits:(parser_limits ())
        ~checkpoint:ignore
    in
    let foreign = List.hd_exn (CM.Parsed.agent_references clone) in
    assert (
      Result.is_error
        (Child.capture
           ~parent
           ~reference:foreign
           ~content_limits:(Some content_limits)
           ~checkpoint:ignore));
    let reference = tool_reference parent in
    assert (
      Result.is_error
        (Child.capture ~parent ~reference ~content_limits:None ~checkpoint:ignore));
    match CM.Agent_reference.invocation reference with
    | Tool declaration ->
      assert (
        Result.is_error
          (Child.capture_legacy_tool
             ~parent
             ~declaration
             ~content_limits:(Some content_limits)
             ~checkpoint:ignore))
    | Persistent_tool _ | Content _ -> failwith "expected actual tool")
;;

let interruption_markers () =
  let cancelled =
    try
      Eio.Cancel.sub (fun context ->
        Eio.Cancel.cancel context Exit;
        Eio.Fiber.check ());
      failwith "cancelled context continued"
    with
    | Eio.Cancel.Cancelled _ as exn -> exn
  in
  [ Failure "original parser checkpoint"; cancelled ]
;;

let sweep operation =
  let polls = ref 0 in
  operation (fun () -> incr polls) |> ok |> ignore;
  assert (!polls > 3);
  let markers = interruption_markers () in
  List.iter markers ~f:(fun marker ->
    for stop = 1 to !polls do
      let seen = ref 0 in
      try
        operation (fun () ->
          incr seen;
          if !seen = stop then raise marker)
        |> ignore;
        failwith "checkpoint failure was interpreted as parser diagnostics"
      with
      | exn -> assert (phys_equal exn marker)
    done)
;;

let%test_unit "borrowed callbacks survive actual captured replay and reparse" =
  with_parent (fun env _ parent ->
    let reference = tool_reference parent in
    sweep (fun checkpoint ->
      Child.capture ~parent ~reference ~content_limits:(Some content_limits) ~checkpoint);
    let child =
      run_child
        env
        parent
        reference
        ~f:(fun _ ~revision ~compilation_limits:_ ~programs:_ -> Ok revision)
      |> ok
    in
    sweep (fun checkpoint ->
      Builder.reparse
        ~checkpoint
        ~content_limits
        ~definition:(Revision.definition parent)
        ~artifact:(Revision.artifact child)
        ~materialized_tree:(Revision.materialized_tree parent)
        ()))
;;

let%test_unit "persistent source fingerprint retains the original v3 formula" =
  with_parent (fun _ _ parent ->
    let module A = Agent_session.Authored_agent_source in
    let module Store = Agent_store.Prompt_artifact_store in
    let module Manifest = Agent_store.Prompt_program_bundle.Manifest in
    let source = A.capture ~content_limits ~parent ~tool_name:"persistent" () |> ok in
    let artifact =
      A.artifact
        source
        ~revision_id:(Agent_protocol.Id.Prompt_revision.create ())
        ~created_at:(Revision.artifact parent).created_at
      |> ok
    in
    let limits =
      Document_schema.Limits.create
        ~max_bytes:262144
        ~max_depth:128
        ~max_fields:100_000
        ~max_nodes:1_000_000
      |> ok
    in
    let manifest_digest storage =
      Manifest.fingerprint
        (Agent_store.Prompt_program_bundle.manifest storage)
        ~limits
        ~checkpoint:ignore
      |> ok
    in
    let program_digest = manifest_digest (Option.value_exn artifact.program_bundle) in
    let content_digest = manifest_digest (Option.value_exn artifact.content_bundle) in
    let parent_artifact = Revision.artifact parent in
    let expected_identity : A.Identity.t = A.identity source in
    [%test_eq: Agent_protocol.Id.Prompt_revision.t]
      parent_artifact.revision_id
      expected_identity.parent_revision_id;
    [%test_eq: string]
      parent_artifact.manifest_sha256
      expected_identity.parent_manifest_sha256;
    [%test_eq: string] "persistent" expected_identity.tool_name;
    [%test_eq: string] "child.chatmd" expected_identity.root_relative_path;
    assert (CM.equal_agent_persistence Optional expected_identity.policy);
    let program_fingerprint =
      [%sexp
        ("ochat.authored-agent-source.v2" : string)
      , (expected_identity : A.Identity.t)
      , (program_digest : string)]
      |> Sexp.to_string_mach
      |> Chatmd_shell_spec.Source_ref.digest
    in
    let expected =
      [%sexp
        ("ochat.authored-agent-source.v3" : string)
      , (program_fingerprint : string)
      , (content_digest : string)]
      |> Sexp.to_string_mach
      |> Chatmd_shell_spec.Source_ref.digest
    in
    [%test_eq: string] expected (A.fingerprint source);
    [%test_eq: string]
      parent_artifact.root_chatmd
      (List.find_exn artifact.sources ~f:(fun item ->
         String.equal item.Store.Source.relative_path parent_artifact.root_relative_path))
        .contents;
    [%test_eq: string]
      ("prv_authored_" ^ expected)
      (Agent_protocol.Id.Prompt_revision.to_string
         (Revision.id (A.resource_revision ~parent source |> ok))))
;;
