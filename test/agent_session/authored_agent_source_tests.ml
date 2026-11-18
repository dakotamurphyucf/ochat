open Core
open Fixtures
module P = Agent_protocol
module CM = Prompt.Chat_markdown
module S = Agent_session.Authored_agent_source
module R = Agent_session.Prompt_revision
module B = Agent_session.Prompt_revision_builder
module Store = Agent_store.Prompt_artifact_store

let built = function
  | Ok value -> value
  | Error errors -> raise_s [%sexp (errors : B.Diagnostic.t list)]
;;

let check_delayed_source_binding ~env ~temporary ~definition ~artifact_store ~root ~parent
  =
  (* Capture the real delegated plan before any future child reservation exists.
     The poisoned moderator above must remain uninitialized throughout. *)
  let module Preparation = Agent_session.Runtime_script_preparation in
  let module Selection = Agent_session.Runtime_source_preparation in
  let policy = script_preparation () in
  let selected_source, selected_revision, prepared =
    Preparation.with_authored_child
      (Some policy)
      ~parent
      ~tool_name:"researcher"
      ~content_limits:None
      ~env
      ~checkpoint:Eio.Fiber.check
      ~f:(fun startup ~source ~revision ->
        let open Result.Let_syntax in
        Preparation.with_revision_plan
          (Some policy)
          ~startup
          revision
          ~env
          ~invocation:Delegated_ui
          ~checkpoint:Eio.Fiber.check
          ~f:(fun startup ~compilation_limits ~plan ~programs ->
            let%map prepared =
              Selection.Prepared_revision.capture
                revision
                ~invocation:Delegated_ui
                ~startup
                ~compilation_limits
                ~plan
                ~programs
                ~checkpoint:(fun () ->
                  Chat_response.Startup_compilation.checkpoint startup)
            in
            source, revision, prepared))
    |> protocol_ok
  in
  let original_selection =
    Selection.Prepared_revision.bind
      prepared
      ~input:
        (Selection.Revision_input.authored selected_revision ~invocation:Delegated_ui)
    |> protocol_ok
  in
  assert (
    Selection.Moderator.equal
      (Selection.Prepared_revision.moderator prepared)
      (Selection.moderator original_selection));
  (match Selection.moderator original_selection with
   | Extension _ -> ()
   | No_moderator | Legacy _ -> failwith "original delegated moderator was not selected");
  let denied result =
    match result with
    | Error { P.Error.code = Invalid_state; _ } -> ()
    | Error error -> raise_s [%sexp (error : P.Error.t)]
    | Ok _ -> failwith "delayed source binding accepted a different role or artifact"
  in
  Selection.Prepared_revision.bind
    prepared
    ~input:(Selection.Revision_input.authored selected_revision ~invocation:Ui)
  |> denied;
  Selection.Prepared_revision.bind
    prepared
    ~input:(Selection.Revision_input.authored parent ~invocation:Delegated_ui)
  |> denied;
  Eio.Switch.run (fun sw ->
    let module D = Agent_store.Delegation_store in
    let module Ledger = Agent_store.Session_store in
    let ledger =
      Ledger.create
        ~env
        ~sw
        ~root:(Filename.concat temporary "delayed-source-ledger")
        ~server_id:(P.Id.Server.create ())
        ~process_start_identity:None
        ~lock_nonce:"delayed-source"
      |> store_ok
    in
    Exn.protect
      ~finally:(fun () -> Ledger.close ledger |> store_ok)
      ~f:(fun () ->
        let artifact =
          S.artifact
            selected_source
            ~revision_id:(P.Id.Prompt_revision.create ())
            ~created_at:timestamp
          |> store_ok
        in
        let admission : D.Admission.t =
          { child_session_id = P.Id.Session.create ()
          ; revision_id = artifact.revision_id
          ; transaction_id = P.Id.Transaction.create ()
          ; manifest_sha256 = artifact.manifest_sha256
          ; parent_revision_id = R.id parent
          ; parent_stop_epoch = Some 0L
          ; authority_sha256 = Chatmd_shell_spec.Source_ref.digest "source binding DATA"
          ; authored_tool =
              Some { name = "researcher"; source_sha256 = S.fingerprint selected_source }
          ; goal_origin = None
          ; captured_recurrence = None
          ; capability_pins = []
          ; lifetime = Owned
          ; created_at = artifact.created_at
          ; inference_target = Some (delegation_inference_target ())
          }
        in
        let record =
          D.reserve
            (Ledger.delegations ledger)
            ~key:
              { parent_session_id = session_id
              ; parent_generation = 0
              ; principal_id
              ; idempotency_key =
                  P.Idempotency_key.of_string "delayed-source" |> protocol_ok
              }
            ~request_sha256:(Chatmd_shell_spec.Source_ref.digest "delayed-source")
            ~admission
            ~max_records:4
            ~max_bytes:1_000_000
          |> store_ok
          |> function
          | D.New record -> record
          | Replay _ | Conflict _ -> failwith "delayed source reservation reused"
        in
        let record =
          S.install_reserved
            ~delegations:(Ledger.delegations ledger)
            ~reservation:record
            ~artifact_store
            ~capability_pins:[]
            selected_source
          |> protocol_ok
        in
        let installed =
          B.restore ~artifact_store definition artifact.revision_id |> built
        in
        let child =
          Agent_session.Prompt_child_source.restore
            ~artifact_store
            ~definition
            ~content_limits:None
            (Agent_session.Prompt_child_source.reference_of_revision
               installed
               ~checkpoint:Eio.Fiber.check
             |> protocol_ok)
            ~checkpoint:Eio.Fiber.check
          |> protocol_ok
        in
        List.iter
          Preparation.Invocation.[ Ui; Headless ]
          ~f:(fun invocation ->
            let input =
              Selection.Revision_input.authored_child child ~record ~invocation
              |> protocol_ok
            in
            assert (
              Preparation.Invocation.equal
                invocation
                (Selection.Revision_input.binding_invocation input));
            assert (
              Preparation.Invocation.equal
                Delegated_ui
                (Selection.Revision_input.preparation_invocation input));
            let bound = Selection.Prepared_revision.bind prepared ~input |> protocol_ok in
            assert (Preparation.Invocation.equal invocation (Selection.invocation bound));
            assert (
              P.Id.Prompt_revision.equal
                artifact.revision_id
                (Selection.artifact bound).revision_id);
            assert (
              Selection.Moderator.equal
                (Selection.moderator original_selection)
                (Selection.moderator bound));
            match Selection.programs original_selection, Selection.programs bound with
            | None, None -> ()
            | Some original, Some bound -> assert (phys_equal original bound)
            | None, Some _ | Some _, None ->
              failwith "delayed binding changed the original restored program custody")));
  (* Genuine legacy custody absence still classifies the actual declaration;
     absent plan/programs is never itself a No_moderator proof. *)
  List.iter
    [ "none", "<developer>No policy.</developer>"
    ; ( "legacy"
      , {|<script id="legacy" language="chatml" kind="moderator">let initial_state = fail("legacy classification initialized")</script>|}
      )
    ; ( "extension"
      , {|<script id="extension" language="chatml" kind="moderator" api="extensibility-v1">let initial_state = fail("extension classification initialized")</script>|}
      )
    ]
    ~f:(fun (kind, text) ->
      let artifact =
        Store.Artifact.create
          ~revision_id:(P.Id.Prompt_revision.create ())
          ~root_chatmd:text
          ~sources:[]
          ~parser_schema_version:5
          ~runtime_schema_version:1
          ~created_at:timestamp
          ()
        |> store_ok
      in
      let revision =
        R.create
          ~definition
          ~artifact
          ~materialized_tree:root
          ~elements:
            (CM.parse_chat_inputs
               ~dir:root
               ~source_loader:(Source_loader.captured_filesystem ~root ~sources:[])
               text)
      in
      let bound =
        Preparation.with_revision_plan
          None
          revision
          ~env
          ~invocation:Ui
          ~checkpoint:Eio.Fiber.check
          ~f:(fun startup ~compilation_limits ~plan ~programs ->
            assert (Option.is_none plan && Option.is_none programs);
            let open Result.Let_syntax in
            let%bind prepared =
              Selection.Prepared_revision.capture
                revision
                ~invocation:Ui
                ~startup
                ~compilation_limits
                ~plan
                ~programs
                ~checkpoint:(fun () ->
                  Chat_response.Startup_compilation.checkpoint startup)
            in
            let%map bound =
              Selection.Prepared_revision.bind
                prepared
                ~input:(Selection.Revision_input.authored revision ~invocation:Ui)
            in
            assert (
              Selection.Moderator.equal
                (Selection.Prepared_revision.moderator prepared)
                (Selection.moderator bound));
            bound)
        |> protocol_ok
      in
      let actual =
        match Selection.moderator bound with
        | No_moderator -> "none"
        | Legacy _ -> "legacy"
        | Extension _ -> "extension"
      in
      [%test_eq: string] kind actual)
;;

let%expect_test
    "authored specialists retain their source closure across edits and relocation"
  =
  with_temp_directory (fun env temporary ->
    let root = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    let save name contents =
      Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / name) contents
    in
    List.iter [ "definitions"; "agents"; "shared" ] ~f:(fun name ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(root / name));
    save
      "root.chatmd"
      {|<developer>Parent-only instructions</developer><import src="definitions/tools.chatmd"/>|};
    save
      "definitions/tools.chatmd"
      {|<tool name="researcher" agent="../agents/researcher.chatmd" local persistence="optional" description="Research carefully"/>
<tool name="reviewer" agent="../agents/researcher.chatmd" local persistence="persistent"/>
<tool name="legacy" agent="../agents/researcher.chatmd" local/>|};
    let original =
      {|<config model="specialist-model" reasoning_effort="high"/>
<developer>Original specialist instructions</developer>
<import src="../shared/tools.chatmd"/>
<tool name="peer" agent="peer.chatmd" local/>
<script id="coordinate" kind="moderator" language="chatml" api="extensibility-v1" src="coordinate.chatml"/>|}
    in
    save "agents/researcher.chatmd" original;
    save "agents/peer.chatmd" {|<developer>Pinned peer</developer>|};
    save "shared/tools.chatmd" {|<tool name="read_file"/>|};
    save
      "agents/coordinate.chatml"
      "let initial_state = fail(\"source capture must not initialize scripts\")\n\
       let on_event ctx state event = Task.pure(state)";
    let definition =
      Agent_session.Prompt_definition.create
        ~root_invocation:Ui
        ~id:prompt_id
        ~config_name:"authored-source"
        ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
        ~allowed_workspaces:[ workspace_id ]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    let artifact_store =
      Store.create ~env ~root:(Filename.concat temporary "artifacts") |> store_ok
    in
    let build () =
      B.build
        ~env
        ~artifact_store
        ~transaction_id:(P.Id.Transaction.create ())
        ~created_at:timestamp
        definition
      |> built
    in
    let parent = build () in
    let captured = S.capture ~parent ~tool_name:"researcher" () |> protocol_ok in
    let reviewer = S.capture ~parent ~tool_name:"reviewer" () |> protocol_ok in
    check_delayed_source_binding ~env ~temporary ~definition ~artifact_store ~root ~parent;
    assert (phys_equal parent (S.defining_revision captured));
    let parsed = R.parser_context parent |> Option.value_exn in
    let reference = S.defining_reference captured |> Option.value_exn in
    let reviewer_reference = S.defining_reference reviewer |> Option.value_exn in
    assert (not (phys_equal reference reviewer_reference));
    List.iter [ reference; reviewer_reference ] ~f:(fun reference ->
      assert (List.exists (CM.Parsed.agent_references parsed) ~f:(phys_equal reference)));
    (match CM.Agent_reference.invocation reference with
     | Persistent_tool (declaration, Optional) ->
       [%test_eq: string] "researcher" declaration.name;
       assert (
         List.exists (R.elements parent) ~f:(function
           | CM.Tool (Persistent_agent (actual, Optional)) ->
             phys_equal declaration actual
           | _ -> false))
     | Persistent_tool (_, Persistent) | Tool _ | Content _ ->
       failwith "defining reference lost the actual declaration/policy");
    [%test_eq: string option]
      (Some "agents/researcher.chatmd")
      (CM.Agent_reference.queued_key reference);
    let range = CM.Agent_reference.range reference in
    let input = Chatmd_parser_input.Range.input range in
    [%test_eq: string] "definitions/tools.chatmd" (Chatmd_parser_input.path input);
    [%test_eq: string]
      {|<tool name="researcher" agent="../agents/researcher.chatmd" local persistence="optional" description="Research carefully"/>|}
      (String.sub
         (Chatmd_parser_input.effective input)
         ~pos:(Chatmd_parser_input.Range.start_byte range)
         ~len:
           (Chatmd_parser_input.Range.end_byte range
            - Chatmd_parser_input.Range.start_byte range));
    assert (not (String.equal (S.fingerprint captured) (S.fingerprint reviewer)));
    [%test_eq: string] "agents/researcher.chatmd" (S.declaration captured).agent;
    assert (CM.equal_agent_persistence (S.identity captured).policy Optional);
    List.iter [ "missing"; "legacy" ] ~f:(fun tool_name ->
      match S.capture ~parent ~tool_name () with
      | Error { code = Permission_denied; _ } -> ()
      | _ -> failwith "non-persistent declaration was captured");
    (* Edits occur before creating the child artifact, not only after installing it. *)
    save
      "agents/researcher.chatmd"
      {|<developer>Changed specialist instructions</developer>|};
    save "agents/peer.chatmd" {|<developer>Changed peer</developer>|};
    save "shared/tools.chatmd" {|<tool name="apply_patch"/>|};
    let changed_parent = build () in
    let changed =
      S.capture ~parent:changed_parent ~tool_name:"researcher" () |> protocol_ok
    in
    assert (not (String.equal (S.fingerprint captured) (S.fingerprint changed)));
    let child_artifact =
      S.artifact
        captured
        ~revision_id:(P.Id.Prompt_revision.create ())
        ~created_at:timestamp
      |> store_ok
    in
    [%test_eq: string] original child_artifact.root_chatmd;
    [%test_eq: int] 1 child_artifact.runtime_schema_version;
    [%test_eq: int] 5 child_artifact.parser_schema_version;
    let child_sources =
      List.map child_artifact.sources ~f:(fun source ->
        source.Store.Source.relative_path, source.contents)
    in
    [%test_eq: string]
      {|<tool name="read_file"/>|}
      (List.Assoc.find_exn child_sources ~equal:String.equal "shared/tools.chatmd");
    [%test_eq: string]
      {|<developer>Pinned peer</developer>|}
      (List.Assoc.find_exn child_sources ~equal:String.equal "agents/peer.chatmd");
    Store.install
      artifact_store
      ~transaction_id:(P.Id.Transaction.create ())
      child_artifact
    |> store_ok;
    let restored =
      B.restore ~artifact_store definition child_artifact.revision_id |> built
    in
    let elements = R.elements restored in
    assert (
      List.exists elements ~f:(function
        | CM.Tool (Read_file _) -> true
        | _ -> false));
    assert (
      not
        (List.exists elements ~f:(function
           | CM.Tool (Builtin "apply_patch") -> true
           | _ -> false)));
    let text =
      List.filter_map elements ~f:(function
        | CM.Developer { content = Some (Text text); _ } -> Some text
        | _ -> None)
    in
    [%test_eq: string list] [ "Original specialist instructions" ] text;
    let peer =
      List.find_map elements ~f:(function
        | CM.Tool (Agent agent) -> Some agent.agent
        | _ -> None)
      |> Option.value_exn
    in
    [%test_eq: string]
      (Eio.Path.native_exn Eio.Path.(R.materialized_tree restored / "agents/peer.chatmd"))
      peer;
    assert (
      List.exists elements ~f:(function
        | CM.Extension_script _ -> true
        | _ -> false));
    let parent_again = B.restore ~artifact_store definition (R.id parent) |> built in
    let again =
      S.capture ~parent:parent_again ~tool_name:"researcher" () |> protocol_ok
    in
    assert (phys_equal parent_again (S.defining_revision again));
    assert (not (phys_equal reference (S.defining_reference again |> Option.value_exn)));
    assert (phys_equal parent (S.defining_revision captured));
    assert (phys_equal reference (S.defining_reference captured |> Option.value_exn));
    assert (S.Identity.equal (S.identity captured) (S.identity again));
    [%test_eq: string] (S.fingerprint captured) (S.fingerprint again);
    (* A copied store has different materialized paths but the same source identity. *)
    let moved_store =
      Store.create ~env ~root:(Filename.concat temporary "moved") |> store_ok
    in
    Store.install
      moved_store
      ~transaction_id:(P.Id.Transaction.create ())
      (R.artifact parent)
    |> store_ok;
    let relocated =
      B.restore ~artifact_store:moved_store definition (R.id parent) |> built
    in
    let relocated =
      S.capture ~parent:relocated ~tool_name:"researcher" () |> protocol_ok
    in
    [%test_eq: string] (S.fingerprint captured) (S.fingerprint relocated);
    let regenerated =
      S.artifact
        relocated
        ~revision_id:(P.Id.Prompt_revision.create ())
        ~created_at:timestamp
      |> store_ok
    in
    [%test_eq: string] child_artifact.root_chatmd regenerated.root_chatmd;
    [%test_eq: string list]
      (List.map child_artifact.sources ~f:(fun source -> source.Store.Source.sha256))
      (List.map regenerated.sources ~f:(fun source -> source.Store.Source.sha256)));
  print_endline
    "captured imports, native declarations, moderator and nested agent survive live edits";
  print_endline
    "child uses specialist instructions and relocated paths; no script initializer or \
     tool ran";
  print_endline
    "same-name source edits and different tool identities differ; parent \
     restore/relocation retain identity";
  [%expect
    {|
    captured imports, native declarations, moderator and nested agent survive live edits
    child uses specialist instructions and relocated paths; no script initializer or tool ran
    same-name source edits and different tool identities differ; parent restore/relocation retain identity
    |}]
;;

let%expect_test "uncaptured external and ambiguous authored definitions cannot be pinned" =
  with_temp_directory (fun env temporary ->
    let root = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    let store =
      Store.create ~env ~root:(Filename.concat temporary "artifacts") |> store_ok
    in
    let definition =
      Agent_session.Prompt_definition.create
        ~root_invocation:Ui
        ~id:prompt_id
        ~config_name:"external-source"
        ~root_file:(Filename.concat temporary "root.chatmd")
        ~allowed_workspaces:[ workspace_id ]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(root / "agent.chatmd")
      "<developer>Local source</developer>";
    List.iter
      [ sprintf
          {|<tool name="researcher" agent="%s" local persistence="persistent"/>|}
          (Filename.concat temporary "agent.chatmd")
      ; {|<tool name="researcher" agent="https://invalid.example/agent.chatmd" persistence="persistent"/>|}
      ; {|<tool name="researcher" agent="agent.chatmd" local persistence="optional"/><tool name="researcher" agent="agent.chatmd" local persistence="persistent"/>|}
      ]
      ~f:(fun source ->
        Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / "root.chatmd") source;
        let parent =
          B.build
            ~env
            ~artifact_store:store
            ~transaction_id:(P.Id.Transaction.create ())
            ~created_at:timestamp
            definition
          |> built
        in
        match S.capture ~parent ~tool_name:"researcher" () with
        | Error { code = Permission_denied; _ } -> ()
        | _ -> failwith "unsafe source captured"));
  print_endline "absolute live source, remote source and duplicate declaration rejected";
  [%expect {| absolute live source, remote source and duplicate declaration rejected |}]
;;

let%expect_test "saved child preflight uses the owned parser and denies unowned effects" =
  with_temp_directory (fun env temporary ->
    let root = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    let store =
      Store.create ~env ~root:(Filename.concat temporary "preflight-artifacts")
      |> store_ok
    in
    let definition =
      Agent_session.Prompt_definition.create
        ~root_invocation:Ui
        ~id:prompt_id
        ~config_name:"child-preflight"
        ~root_file:(Filename.concat temporary "root.chatmd")
        ~allowed_workspaces:[ workspace_id ]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    let build source =
      Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / "root.chatmd") source;
      let revision =
        B.build
          ~env
          ~artifact_store:store
          ~transaction_id:(P.Id.Transaction.create ())
          ~created_at:timestamp
          definition
        |> built
      in
      revision, R.parser_context revision |> Option.value_exn
    in
    let module Preflight = Agent_session.Developer_child_preflight in
    let outcome = function
      | Ok () -> "admitted"
      | Error (error : P.Error.t) -> P.Error.code_to_string error.code
    in
    let text_input =
      CM.Basic
        { type_ = "text"
        ; text = Some "selected input"
        ; image_url = None
        ; document_url = None
        ; is_local = false
        ; cleanup_html = false
        ; markdown = false
        }
    in
    let document_input =
      CM.Basic
        { type_ = "text"
        ; text = None
        ; image_url = None
        ; document_url = Some "file:///unselected"
        ; is_local = false
        ; cleanup_html = false
        ; markdown = false
        }
    in
    let good, good_parsed =
      build {|<developer>Selected text</developer><tool name="read_file"/>|}
    in
    let selected_text =
      Preflight.admit_selected
        ~revision:good
        ~parsed:good_parsed
        ~input:[ text_input ]
        ~checkpoint:ignore
    in
    let selected_document =
      Preflight.admit_selected
        ~revision:good
        ~parsed:good_parsed
        ~input:[ document_input ]
        ~checkpoint:ignore
    in
    let literal =
      Preflight.admit_literal ~revision:good ~parsed:good_parsed ~checkpoint:ignore
    in
    print_s
      [%sexp
        ("read_file / text input" : string)
      , (outcome selected_text : string)
      , (outcome selected_document : string)
      , (outcome literal : string)];
    let assistant, assistant_parsed =
      build {|<assistant><img src="inline.png" local/></assistant>|}
    in
    let assistant_inline =
      Preflight.classify_selected
        ~ordinary_agent_tools:Denied
        ~revision:assistant
        ~parsed:assistant_parsed
        ~input:[]
        ~checkpoint:ignore
    in
    let caller_document =
      Preflight.classify_selected
        ~ordinary_agent_tools:Denied
        ~revision:good
        ~parsed:good_parsed
        ~input:[ document_input ]
        ~checkpoint:ignore
    in
    print_s
      [%sexp
        (assistant_inline : (bool, P.Error.t) result)
      , (caller_document : (bool, P.Error.t) result)];
    let oversized_text =
      match text_input with
      | CM.Basic basic ->
        CM.Basic { basic with text = Some (String.make ((8 * 1024 * 1024) + 1) 'x') }
      | CM.Agent _ -> assert false
    in
    let oversized_items = List.init 16_384 ~f:(fun _ -> text_input) in
    let limited_text =
      Preflight.admit_selected
        ~revision:good
        ~parsed:good_parsed
        ~input:[ oversized_text ]
        ~checkpoint:ignore
    in
    let limited_nodes =
      Preflight.admit_selected
        ~revision:good
        ~parsed:good_parsed
        ~input:oversized_items
        ~checkpoint:ignore
    in
    let exception Stop in
    let stopped =
      try
        ignore
          (Preflight.admit_selected
             ~revision:good
             ~parsed:good_parsed
             ~input:[ text_input ]
             ~checkpoint:(fun () -> raise Stop));
        false
      with
      | Stop -> true
    in
    print_s
      [%sexp
        ("bounds / checkpoint" : string)
      , (outcome limited_text : string)
      , (outcome limited_nodes : string)
      , (stopped : bool)];
    let fork, fork_parsed = build {|<tool name="fork"/>|} in
    let fork_result =
      Preflight.admit_selected
        ~revision:fork
        ~parsed:fork_parsed
        ~input:[]
        ~checkpoint:ignore
    in
    let foreign_parser =
      Preflight.admit_selected
        ~revision:fork
        ~parsed:good_parsed
        ~input:[]
        ~checkpoint:ignore
    in
    print_s
      [%sexp
        ("fork / foreign parser" : string)
      , (outcome fork_result : string)
      , (outcome foreign_parser : string)];
    let moderator, moderator_parsed =
      build
        {|<script id="state" kind="moderator" language="chatml">let initial_state = 0
let on_event ctx state event = Task.pure(state)</script>|}
    in
    let moderator_result =
      Preflight.admit_selected
        ~revision:moderator
        ~parsed:moderator_parsed
        ~input:[]
        ~checkpoint:ignore
    in
    let literal_moderator =
      Preflight.admit_literal
        ~revision:moderator
        ~parsed:moderator_parsed
        ~checkpoint:ignore
    in
    print_s
      [%sexp
        ("moderator selected / literal" : string)
      , (outcome moderator_result : string)
      , (outcome literal_moderator : string)];
    let metadata, metadata_parsed =
      build
        {|<shell_access id="direct" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false">
    <read path="${workspace}"/>
  </capabilities>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <policy default="allow"/>
  <approvals provider="none" unavailable="deny" scopes="once"/>
  <audit format="none"/>
</shell_access>
<moderator_runtime shell_runtime="direct"/>|}
    in
    print_s
      [%sexp
        ("moderator runtime selected / literal" : string)
      , (outcome
           (Preflight.admit_selected
              ~revision:metadata
              ~parsed:metadata_parsed
              ~input:[]
              ~checkpoint:ignore)
         : string)
      , (outcome
           (Preflight.admit_literal
              ~revision:metadata
              ~parsed:metadata_parsed
              ~checkpoint:ignore)
         : string)]);
  [%expect
    {|
    ("read_file / text input" admitted admitted invalid_state)
    ((Ok false) (Ok true))
    ("bounds / checkpoint" resource_limit resource_limit true)
    ("fork / foreign parser" invalid_request invalid_state)
    ("moderator selected / literal" admitted invalid_state)
    ("moderator runtime selected / literal" admitted invalid_state)
    |}]
;;
