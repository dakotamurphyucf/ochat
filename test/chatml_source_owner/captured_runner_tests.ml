open Core
open Support
module CM = Prompt.Chat_markdown
module Runner = Agent_session.Captured_agent_runner
module Revision = Agent_session.Prompt_revision
module Port = Chat_response.Tool.Agent_source_runner
module Res = Openai.Responses

let answer text : Res.Response_stream.t =
  Output_item_done
    { item =
        Output_message
          { role = Assistant
          ; id = "captured-answer"
          ; content = [ { annotations = []; text; _type = "output_text" } ]
          ; status = "completed"
          ; phase = None
          ; _type = "message"
          }
    ; output_index = 0
    ; type_ = "response.output_item.done"
    }
;;

let texts inputs =
  List.concat_map inputs ~f:(function
    | Res.Item.Input_message message ->
      List.filter_map message.content ~f:(function
        | Text { text; _ } -> Some (String.strip text)
        | Image _ -> None)
    | _ -> [])
;;

let with_parent f =
  with_workspace (fun env directory root ->
    save
      root
      "root.chatmd"
      {|<tool name="ordinary" agent="child.chatmd" local/>
<user><agent src="child.chatmd" local><agent src="caller-only.chatmd" local>caller input</agent></agent></user>|};
    save
      root
      "child.chatmd"
      {|<user><agent src="child-only.chatmd" local>child input</agent></user>
<script id="owner" language="chatml" kind="moderator" project="chatml.project.json">let initial_state = Graph.seed
let on_event = fun ctx state event -> Task.pure(state)</script>|};
    save root "caller-only.chatmd" "<user>caller source</user>";
    save root "child-only.chatmd" "<user>child source</user>";
    save root "graph.chatml" "let seed = print(\"initialized\"); 7";
    save root "graph.chatmli" "val seed : int";
    save
      root
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"child","modules":{"Graph":{"implementation":"graph.chatml","interface":"graph.chatmli"}}}|};
    Eio.Switch.run (fun sw ->
      let definition = definition root in
      let catalog = catalog env directory in
      let candidate =
        prepare catalog sw [ definition ] ~prepare_programs:(fun scope captured ->
          Preparation_tests.prepare ~root_role:Headless env scope captured)
        |> ok
      in
      Catalog.install_owned catalog candidate |> ok;
      let parent = revision catalog definition in
      List.iter (Eio.Path.read_dir root) ~f:(fun name ->
        Eio.Path.unlink Eio.Path.(root / name));
      [%test_eq: string list] [] (Eio.Path.read_dir root);
      let factory =
        Runner.create
          ~script_preparation:(Runtime_preparation_tests.policy ())
          ~content_limits:None
          ~policy_revision:
            (Chatml_source.Digest.of_contents "fixture-policy-v1" ~checkpoint:ignore)
          ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
          ~approval_provider:Shell_runtime.Approval_broker.None_available
          ~response_dir:directory
      in
      f env directory parent factory))
;;

let descriptor parent =
  let parsed = Revision.parser_context parent |> Option.value_exn in
  CM.Parsed.agent_references parsed
  |> List.find_map ~f:(fun reference ->
    match CM.Agent_reference.invocation reference with
    | Content declaration ->
      if
        Option.equal
          String.equal
          (CM.Agent_reference.queued_key reference)
          (Some "child.chatmd")
      then Some declaration
      else None
    | Tool _ | Persistent_tool _ -> None)
  |> Option.value_exn
;;

let context env directory ~post_stream =
  let inference =
    Inference_fixture.create
      ~namespace:"captured-runner"
      ~default_model:"fixture-model"
      ~post_stream
  in
  Inference_fixture.ctx
    inference
    ~env
    ~dir:directory
    ~tool_dir:directory
    ~cache:(Chat_response.Cache.create ~max_size:1 ())
    ()
;;

let%expect_test
    "captured children retain caller/child ownership and initialize independently"
  =
  with_parent (fun env directory parent factory ->
    let observed = Queue.create () in
    let initialized = ref 0 in
    Chatml.Chatml_builtin_spec.set_print_sink (fun text ->
      [%test_eq: string] "initialized" text;
      incr initialized);
    Fun.protect ~finally:Chatml.Chatml_builtin_spec.clear_print_sink (fun () ->
      let ctx =
        context env directory ~post_stream:(fun ~sw:_ ~inputs ->
          let inputs = texts inputs in
          Queue.enqueue observed inputs;
          let result =
            if List.mem inputs "caller source" ~equal:String.equal
            then "CALLER"
            else if List.mem inputs "child source" ~equal:String.equal
            then "CHILD"
            else (
              [%test_eq: string list] [ "CHILD"; "CALLER" ] inputs;
              "ANSWER")
          in
          Stdlib.List.to_seq [ answer result ])
      in
      let saved = ref None in
      Eio.Switch.run (fun sw ->
        let port = Runner.bind factory ~revision:parent ~sw |> ok in
        saved := Some port;
        [%test_eq: int] 0 !initialized;
        let content = descriptor parent in
        [%test_eq: string] "ANSWER" (port.run_content ~declaration:content ~ctx);
        let declaration =
          CM.Parsed.agent_references (Revision.parser_context parent |> Option.value_exn)
          |> List.find_map ~f:(fun reference ->
            match CM.Agent_reference.invocation reference with
            | Tool actual -> Some actual
            | Persistent_tool _ | Content _ -> None)
          |> Option.value_exn
        in
        [%test_eq: string]
          "ANSWER"
          (port.run
             ~implementation:None
             ~declaration
             ~ctx
             ~source:"ordinary-call"
             ~observer:None
             content.items);
        [%test_eq: int] 2 !initialized;
        [%test_eq: int] 6 (Queue.length observed));
      let before = Queue.length observed in
      (match
         (Option.value_exn !saved).run_content ~declaration:(descriptor parent) ~ctx
       with
       | _ -> assert false
       | exception Agent_session.Runtime_activity.Closed -> ());
      [%test_eq: int] before (Queue.length observed);
      print_s [%sexp (Queue.to_list observed : string list list), (!initialized : int)]));
  [%expect
    {|
    ((("child source" "child input") ("caller source" "caller input")
      (CHILD CALLER) ("child source" "child input")
      ("caller source" "caller input") (CHILD CALLER))
     2)
    |}]
;;

let%test_unit "caller cancellation joins provider cleanup and leaves sibling calls usable"
  =
  with_parent (fun env directory parent factory ->
    Eio.Switch.run (fun sw ->
      let port = Runner.bind factory ~revision:parent ~sw |> ok in
      let entered, enter = Eio.Promise.create () in
      let cleaned = ref false in
      let block = ref true in
      let ctx =
        context env directory ~post_stream:(fun ~sw:_ ~inputs:_ ->
          if !block
          then
            Fun.protect
              ~finally:(fun () -> cleaned := true)
              (fun () ->
                 Eio.Promise.resolve enter ();
                 Eio.Fiber.await_cancel ());
          Stdlib.List.to_seq [ answer "ANSWER" ])
      in
      let cancellation, cancel = Eio.Promise.create () in
      let caller =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Eio.Cancel.sub (fun context ->
            Eio.Promise.resolve cancel context;
            port.run_content ~declaration:(descriptor parent) ~ctx))
      in
      Eio.Promise.await entered;
      Eio.Cancel.cancel (Eio.Promise.await cancellation) Exit;
      (match Eio.Promise.await caller with
       | Error (Eio.Cancel.Cancelled _) -> ()
       | _ -> assert false);
      assert !cleaned;
      block := false;
      [%test_eq: string] "ANSWER" (port.run_content ~declaration:(descriptor parent) ~ctx)))
;;

let%expect_test "actual source and runtime policy limits participate in runner identity" =
  with_workspace (fun _env directory _root ->
    let tag = Chatml_source.Digest.of_contents "identity-host-v1" ~checkpoint:ignore in
    let source max_work_steps =
      Runner.Source.create
        ~policy_revision:tag
        ~compilation:Chatml_compilation.default_limits
        ~parser_limits:(Agent_session.Prompt_revision_builder.capture_limits ())
        ~max_work_steps
        ~preprocess:Fn.id
        ~prepare_programs:(fun _ ~invocation:_ _ _ ->
          failwith "identity construction must not prepare programs")
      |> Or_error.ok_exn
      |> Runner.Source.policy_identity
    in
    let runtime max_custody_work_steps content_limits =
      Runner.create
        ~script_preparation:(Runtime_preparation_tests.policy ~max_custody_work_steps ())
        ~content_limits
        ~policy_revision:tag
        ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
        ~approval_provider:Shell_runtime.Approval_broker.None_available
        ~response_dir:directory
      |> Runner.policy_identity
    in
    let equal = Chatml_source.Digest.equal in
    assert (equal (source 100) (source 100));
    assert (not (equal (source 100) (source 101)));
    assert (equal (runtime 100 None) (runtime 100 None));
    assert (not (equal (runtime 100 None) (runtime 101 None)));
    assert (
      not
        (equal
           (runtime 100 None)
           (runtime 100 Agent_server.Daemon.default_options.content_limits)));
    print_endline
      "identical numeric policies are stable; changed limits change identities");
  [%expect {| identical numeric policies are stable; changed limits change identities |}]
;;

let%test_unit
    "selected native source view preserves exact revision and function ownership"
  =
  with_parent (fun env directory parent factory ->
    let module View = Agent_session.Agent_source_code_view in
    Eio.Switch.run (fun resource_sw ->
      let ctx =
        context env directory ~post_stream:(fun ~sw:_ ~inputs:_ ->
          failwith "source view must not execute inference")
      in
      let port = Runner.bind factory ~revision:parent ~sw:resource_sw |> ok in
      let elements = Revision.elements parent in
      let host =
        Chat_response.Agent_runtime.host
          ~env
          ~workspace:directory
          ~tool_dir:directory
          ~prompt_dir:(Revision.materialized_tree parent)
          ~session_dir:directory
          ~cache_dir:directory
          ~home:directory
          ~session_id:"source-view"
          ~prompt_elements:elements
        |> ok
      in
      let create_native () =
        Chat_response.Agent_runtime.create
          ~agent_source_runner:port
          ~sw:resource_sw
          ~ctx
          ~host
          ~platform:(Chat_response.Agent_runtime.platform ())
          ~prompt_elements:elements
          ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
          ~approval_provider:Shell_runtime.Approval_broker.None_available
          ~approval_store:(Shell_access.Approval.create_store ())
          ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
            failwith "source view must not invoke legacy agent")
          ()
        |> ok
      in
      let native = create_native () in
      let implementation = List.hd_exn native.functions in
      let unrelated = List.hd_exn (create_native ()).functions in
      assert (not (phys_equal implementation unrelated));
      let reparse () =
        Builder.reparse
          ~definition:(Revision.definition parent)
          ~artifact:(Revision.artifact parent)
          ~materialized_tree:(Revision.materialized_tree parent)
          ()
        |> ok
      in
      let selected = reparse ()
      and foreign = reparse () in
      let retained = ref None in
      Eio.Switch.run (fun sw ->
        let view =
          View.create
            ~original:parent
            ~selected
            ~native
            ~sw
            ~max_work_steps:10_000
            ~checkpoint:Eio.Fiber.check
          |> ok
        in
        retained := Some view;
        let context =
          View.find view ~defining_revision:parent ~implementation
          |> ok
          |> Option.value_exn
        in
        assert (phys_equal (View.Context.revision context) selected);
        assert (
          phys_equal
            (View.Context.reference context)
            (List.find_exn
               (CM.Parsed.agent_references
                  (Revision.parser_context selected |> Option.value_exn))
               ~f:(fun reference ->
                 match CM.Agent_reference.invocation reference with
                 | Tool _ -> true
                 | Persistent_tool _ | Content _ -> false)));
        assert (
          Option.is_none (View.find view ~defining_revision:selected ~implementation |> ok));
        assert (
          Option.is_none
            (View.find view ~defining_revision:foreign ~implementation:unrelated |> ok));
        assert (
          Result.is_error (View.find view ~defining_revision:foreign ~implementation));
        assert (
          Result.is_error
            (View.create
               ~original:parent
               ~selected
               ~native
               ~sw
               ~max_work_steps:1
               ~checkpoint:Eio.Fiber.check));
        let exception Checkpoint in
        match
          View.create
            ~original:parent
            ~selected
            ~native
            ~sw
            ~max_work_steps:10_000
            ~checkpoint:(fun () -> raise Checkpoint)
        with
        | exception Checkpoint -> ()
        | _ -> failwith "source view swallowed original checkpoint exception");
      let closed = Option.value_exn !retained in
      assert (
        Result.is_error
          (View.find closed ~defining_revision:parent ~implementation:unrelated));
      assert (Result.is_error (View.find_for_preparation closed ~implementation))))
;;
