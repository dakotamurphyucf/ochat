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
      , (outcome literal_moderator : string)]);
  [%expect
    {|
    ("read_file / text input" admitted admitted invalid_state)
    ((Ok false) (Ok true))
    ("bounds / checkpoint" resource_limit resource_limit true)
    ("fork / foreign parser" invalid_request invalid_state)
    ("moderator selected / literal" admitted invalid_state)
    |}]
;;
