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
