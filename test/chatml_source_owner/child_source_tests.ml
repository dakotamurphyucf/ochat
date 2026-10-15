open Core
open Support
module CM = Prompt.Chat_markdown
module Child = Agent_session.Prompt_child_source
module Lease = Agent_session.Prompt_source_lease
module Revision = Agent_session.Prompt_revision
module Store = Agent_store.Prompt_artifact_store
module Sources = Agent_session.Prompt_script_preparation
module Runtime = Agent_session.Runtime_script_preparation
module P = Agent_protocol

let timestamp = Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok
let content_limits = Captured_child_tests.content_limits

let texts revision =
  List.concat_map (Revision.elements revision) ~f:(function
    | CM.Developer { content = Some (Items items); _ } ->
      List.filter_map items ~f:(function
        | Basic { text = Some text; _ } -> Some text
        | Basic _ | Agent _ -> None)
    | _ -> [])
;;

let reference parent =
  Revision.parser_context parent
  |> Option.value_exn
  |> CM.Parsed.agent_references
  |> List.find_exn ~f:(fun reference ->
    Option.is_some (CM.Agent_reference.queued_key reference))
;;

let with_fixture ?(headless = false) f =
  with_workspace (fun env directory root ->
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "nested");
    save
      root
      "root.chatmd"
      (if headless
       then {|<tool name="child" agent="nested/child.chatmd" local/>|}
       else
         {|<tool name="child" agent="nested/child.chatmd" local persistence="persistent"/>|});
    save
      root
      "nested/child.chatmd"
      (sprintf
         {|<developer><doc src="../guide.md" local capture/></developer>
<script id="owner" language="chatml" kind="moderator" %s project="chatml.project.json">let initial_state = Graph.seed
let on_event = fun ctx state event -> Task.pure(state)</script>|}
         (if headless then "" else {|api="extensibility-v1"|}));
    save root "guide.md" "guide A";
    save
      root
      "nested/chatml.project.json"
      {|{"schema_version":1,"namespace":"child","modules":{"Graph":{"implementation":"graph.chatml","interface":"graph.chatmli"}}}|};
    save root "nested/graph.chatml" "let seed = 1";
    save root "nested/graph.chatmli" "val seed : int";
    Eio.Switch.run (fun sw ->
      let definition = definition root in
      let store =
        Store.create ~env ~root:(Eio.Path.native_exn Eio.Path.(directory / "artifacts"))
        |> ok
      in
      let catalog =
        Catalog.create ~env ~artifact_store:store ~content_limits:(Some content_limits)
      in
      let candidate =
        Catalog.prepare_owned
          catalog
          ~prepare_programs:(fun scope capture ->
            Preparation_tests.prepare env scope capture)
          ~prepare_content:(fun scope capture ->
            Owner.Scope.capture_documents
              scope
              ~capture
              ~limits:content_limits
              ~checkpoint:ignore
            |> Result.map_error ~f:(fun error ->
              [ Builder.Diagnostic.
                  { code = "fixture.child_content"
                  ; message = Sexp.to_string_hum (Owner.Error.sexp_of_t error)
                  ; source = None
                  }
              ]))
          ~sw
          ~source_limits:(limits ())
          ~max_roots:8
          ~checkpoint:ignore
          ~transaction_id:(fun _ -> P.Id.Transaction.create ())
          ~created_at:timestamp
          [ definition ]
        |> ok
      in
      Catalog.install_owned catalog candidate |> ok;
      let parent = revision catalog definition in
      let lease =
        Lease.create
          (Revision.artifact parent).source_receipt
          ~selection:(Some (selection catalog definition))
          ~sw
          ~checkpoint:ignore
      in
      f env root sw store parent lease))
;;

let capture_file env store parent lease ?(work = 1_000_000) ?(checkpoint = ignore) () =
  let policy = Preparation_tests.policy () in
  Child.file
    ~parent
    ~reference:(reference parent)
    ~source_lease:lease
    ~artifact_store:store
    ~compilation:(Sources.Policy.compilation policy)
    ~parser_limits:(parser_limits ())
    ~content_limits:(Some content_limits)
    ~max_work_steps:work
    ~preprocess:Fn.id
    ~prepare_programs:(fun startup scope capture ->
      Sources.capture_child
        policy
        ~startup
        ~invocation:Delegated_ui
        ~host:{ additional_anchors = []; git_store = None; git_grants = [] }
        ~scope
        ~capture
        ~operation:(fun _ -> Chatml_project_build.Operation.Build_without_previous)
        ~provided:[]
        ~env
        ~checkpoint:(fun () -> Chat_response.Startup_compilation.checkpoint startup)
        ()
      |> Result.map ~f:Sources.Prepared.programs)
    ~env
    ~created_at:timestamp
    ~checkpoint
;;

let restore store parent reference =
  Child.restore
    ~artifact_store:store
    ~definition:(Revision.definition parent)
    ~content_limits:(Some content_limits)
    reference
    ~checkpoint:ignore
  |> ok
;;

let%expect_test
    "fresh selected child installs once and recorded restoration never reads later files"
  =
  with_fixture (fun env root _ store parent lease ->
    save root "guide.md" "guide B";
    save root "nested/graph.chatml" "let seed = 2";
    let polls = ref 0 in
    let selected =
      capture_file env store parent lease ~checkpoint:(fun () -> incr polls) () |> ok
    in
    let revision = Child.revision selected in
    [%test_eq: string list] [ "guide B" ] (texts revision);
    let saved = Child.reference selected in
    assert (
      Store.exists
        store
        (Chat_response.Background_request.Agent_artifact.revision_id saved));
    [%test_eq: string]
      "nested/child.chatmd"
      (Source.Portable_path.to_string
         (Chat_response.Background_request.Agent_artifact.root_relative_path saved));
    assert (
      not
        (String.equal
           (Eio.Path.native_exn (Revision.materialized_tree revision))
           (Eio.Path.native_exn root)));
    (* Exercise authentic delegated restoration; no native/initializer here. *)
    Runtime.with_revision
      (Some (Runtime_preparation_tests.policy ()))
      revision
      ~env
      ~invocation:Delegated_ui
      ~checkpoint:ignore
      ~f:(fun _ ~compilation_limits:_ ~programs ->
        assert (Option.is_some programs);
        Ok ())
    |> ok;
    ignore (capture_file env store parent lease ~work:!polls () |> ok : Child.t);
    assert (Result.is_error (capture_file env store parent lease ~work:(!polls - 1) ()));
    save root "guide.md" "guide C";
    save root "nested/graph.chatml" "let seed = 3";
    [%test_eq: string list]
      [ "guide B" ]
      (texts (Child.revision (restore store parent saved)));
    let newer = capture_file env store parent lease () |> ok in
    assert (
      not
        (P.Id.Prompt_revision.equal
           (Revision.id revision)
           (Revision.id (Child.revision newer))));
    [%test_eq: string list] [ "guide C" ] (texts (Child.revision newer));
    List.iter (Eio.Path.read_dir root) ~f:(fun name ->
      Eio.Path.rmtree Eio.Path.(root / name));
    Lease.close lease;
    [%test_eq: string list]
      [ "guide B" ]
      (texts (Child.revision (restore store parent saved)));
    assert (Result.is_error (capture_file env store parent lease ()));
    let captured =
      Child.captured
        ~parent
        ~reference:(reference parent)
        ~artifact_store:store
        ~policy:(Runtime_preparation_tests.policy ())
        ~invocation:Delegated_ui
        ~content_limits:(Some content_limits)
        ~env
        ~checkpoint:ignore
      |> ok
    in
    [%test_eq: string list] [ "guide A" ] (texts (Child.revision captured));
    print_endline
      "installed B survives C, deletion and lease close; new File selects C; Captured \
       stays A");
  [%expect
    {| installed B survives C, deletion and lease close; new File selects C; Captured stays A |}]
;;

let%test_unit
    "child callback and final-replay checkpoint exceptions preserve original identity"
  =
  with_fixture (fun env _root _ store parent lease ->
    let sentinel = Failure "original selected child checkpoint" in
    let observed = ref 0 in
    ignore
      (capture_file env store parent lease ~checkpoint:(fun () -> incr observed) () |> ok
       : Child.t);
    assert (!observed > 20);
    List.iter
      [ 1; !observed - 2; !observed ]
      ~f:(fun interrupt ->
        let calls = ref 0 in
        match
          capture_file
            env
            store
            parent
            lease
            ~checkpoint:(fun () ->
              incr calls;
              if !calls = interrupt then raise sentinel)
            ()
        with
        | _ -> assert false
        | exception exn -> assert (phys_equal sentinel exn));
    ignore (capture_file env store parent lease () |> ok : Child.t))
;;

let%expect_test
    "installed headless factory selects current files without losing explicit captured \
     binding"
  =
  with_fixture ~headless:true (fun env root sw store parent lease ->
    let module Runner = Agent_session.Captured_agent_runner in
    let policy = Preparation_tests.policy () in
    let source =
      Runner.Source.create
        ~policy_revision:
          (Chatml_source.Digest.of_contents "fixture-source-v1" ~checkpoint:ignore)
        ~compilation:(Sources.Policy.compilation policy)
        ~parser_limits:(parser_limits ())
        ~max_work_steps:1_000_000
        ~preprocess:Fn.id
        ~prepare_programs:(fun startup ~invocation scope capture ->
          Sources.capture_child
            policy
            ~startup
            ~invocation
            ~host:{ additional_anchors = []; git_store = None; git_grants = [] }
            ~scope
            ~capture
            ~operation:(fun _ -> Chatml_project_build.Operation.Build_without_previous)
            ~provided:[]
            ~env
            ~checkpoint:(fun () -> Chat_response.Startup_compilation.checkpoint startup)
            ()
          |> Result.map ~f:Sources.Prepared.programs)
      |> Or_error.ok_exn
    in
    let responses =
      Eio.Path.(env#fs / Filename.dirname (Eio.Path.native_exn root) / "responses")
    in
    Eio.Path.mkdir ~perm:0o700 responses;
    let factory =
      Runner.create
        ~script_preparation:(Runtime_preparation_tests.policy ())
        ~content_limits:(Some content_limits)
        ~policy_revision:
          (Chatml_source.Digest.of_contents "fixture-runtime-v1" ~checkpoint:ignore)
        ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
        ~approval_provider:Shell_runtime.Approval_broker.None_available
        ~response_dir:responses
    in
    let installed =
      Runner.bind_installed
        factory
        ~source
        ~artifact_store:store
        ~source_lease:lease
        ~env
        ~revision:parent
        ~sw
      |> ok
    in
    let captured = Runner.bind factory ~revision:parent ~sw |> ok in
    assert (not (Chatml_source.Digest.equal installed.revision captured.revision));
    let declaration =
      match CM.Agent_reference.invocation (reference parent) with
      | Tool declaration -> declaration
      | Persistent_tool _ | Content _ -> assert false
    in
    save root "guide.md" "guide B";
    save root "nested/graph.chatml" "let seed = 2";
    let saved =
      (Option.value_exn installed.prepare)
        ~declaration
        ~launch:
          (Chat_response.Source_launch.conversation
             (Invocation (P.Id.Invocation.create ())))
        ~checkpoint:ignore
      |> ok
    in
    [%test_eq: string list]
      [ "guide B" ]
      (texts (Child.revision (restore store parent saved)));
    let observed = Queue.create () in
    let ctx =
      Captured_runner_tests.context env responses ~post_stream:(fun ~sw:_ ~inputs ->
        Queue.enqueue observed (Captured_runner_tests.texts inputs);
        Stdlib.List.to_seq [ Captured_runner_tests.answer "selected child" ])
    in
    let module Native = Chat_response.Native_tool_pool in
    let module Capabilities = Chat_response.Tool_capability in
    let elements = Revision.elements parent in
    let workspace = (root :> Eio.Fs.dir_ty Eio.Path.t) in
    let host =
      Chat_response.Agent_runtime.host
        ~env
        ~workspace
        ~tool_dir:workspace
        ~prompt_dir:(Revision.materialized_tree parent)
        ~session_dir:responses
        ~cache_dir:responses
        ~home:workspace
        ~session_id:"installed-source-delegation"
        ~prompt_elements:elements
      |> ok
    in
    let check_delegation agent_source_runner ~admitted =
      let pool =
        Native.create
          ~sw
          ~ctx
          ~host
          ~shell:Absent
          ~admin_policy:Shell_runtime.Admin_policy.permissive
          ?agent_source_runner
          ~registrations:[]
          ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
            failwith "delegation classification invoked legacy source")
          elements
        |> ok
      in
      let binding =
        Lazy.force (Native.capabilities pool)
        |> ok
        |> fun capabilities -> Capabilities.find capabilities ~name:declaration.name |> ok
      in
      assert (
        phys_equal
          (Capabilities.native_implementation binding |> Option.value_exn)
          (List.hd_exn (Native.functions pool)));
      assert (Bool.equal admitted (Result.is_ok (Capabilities.check_delegation binding)))
    in
    check_delegation (Some installed) ~admitted:true;
    check_delegation (Some captured) ~admitted:false;
    check_delegation None ~admitted:false;
    assert (Queue.is_empty observed);
    let items =
      [ CM.Basic
          { type_ = "text"
          ; text = Some "go"
          ; image_url = None
          ; document_url = None
          ; is_local = false
          ; cleanup_html = false
          ; markdown = false
          }
      ]
    in
    save root "guide.md" "guide C";
    save root "nested/graph.chatml" "let seed = 3";
    [%test_eq: string]
      "selected child"
      (installed.run
         ~implementation:None
         ~declaration
         ~ctx
         ~source:"direct fresh call"
         ~observer:None
         items);
    [%test_eq: string list list] [ [ "guide C"; "go" ] ] (Queue.to_list observed);
    List.iter (Eio.Path.read_dir root) ~f:(fun name ->
      Eio.Path.rmtree Eio.Path.(root / name));
    Lease.close lease;
    assert (
      Result.is_error
        ((Option.value_exn installed.prepare)
           ~declaration
           ~launch:
             (Chat_response.Source_launch.conversation
                (Invocation (P.Id.Invocation.create ())))
           ~checkpoint:ignore));
    [%test_eq: string]
      "selected child"
      (captured.run
         ~implementation:None
         ~declaration
         ~ctx
         ~source:"explicit captured call"
         ~observer:None
         items);
    [%test_eq: string list list]
      [ [ "guide C"; "go" ]; [ "guide A"; "go" ] ]
      (Queue.to_list observed);
    [%test_eq: string list]
      [ "guide B" ]
      (texts (Child.revision (restore store parent saved)));
    print_endline
      "real headless calls select C or captured A; installed queued reference retains B");
  [%expect
    {| real headless calls select C or captured A; installed queued reference retains B |}]
;;
