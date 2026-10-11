open Core
module CM = Prompt.Chat_markdown
module R = Chat_response.Agent_runtime
module Tool = Chat_response.Tool
module Driver = Chat_response.Driver
module Capability = Chat_response.Tool_capability
module Custody = Chatml_prompt_programs
module Contract = Chatml.Chatml_compiler_contract
module Res = Openai.Responses

let ok = Routing_tests.ok
let checkpoint = ignore
let digest text = Chatml_source.Digest.of_contents text ~checkpoint

exception Source_callback_failed of int ref

let%test_unit "agent source interception precedes Fetch and preserves callback failures" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let elements =
      CM.parse_chat_inputs
        ~dir:env#cwd
        {|<tool name="child" agent="__och207_no_fetch__/missing.chatmd" local/>|}
    in
    Startup_tests.with_host env ~elements (fun ~sw ~ctx ~host ->
      let declaration =
        match elements with
        | [ CM.Tool (CM.Agent declaration) ] -> declaration
        | _ -> failwith "expected actual agent declaration"
      in
      let sources = Queue.create () in
      let observed = Queue.create () in
      let failure = ref None in
      let runner =
        Tool.Agent_source_runner.
          { revision = digest "owned-source-policy"
          ; run_content =
              (fun ~declaration:_ ~ctx:_ -> failwith "unexpected content agent")
          ; run =
              (fun ~declaration:actual ~ctx:actual_ctx ~source ~observer items ->
                assert (phys_equal actual_ctx ctx);
                [%test_eq: string] actual.name declaration.name;
                [%test_eq: string] actual.agent declaration.agent;
                [%test_eq: bool] actual.is_local declaration.is_local;
                Queue.enqueue sources source;
                Queue.enqueue observed (Option.is_some observer);
                (match items with
                 | [ CM.Basic { text = Some "input"; _ } ] -> ()
                 | _ -> failwith "source runner received different child items");
                match !failure with
                | Some identity -> raise (Source_callback_failed identity)
                | None -> "CAPTURED")
          }
      in
      let tool =
        Tool.of_declaration
          ~agent_source_runner:runner
          ~host
          ~sw
          ~ctx
          ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
            failwith "source runner fell back to legacy Fetch")
          (CM.Agent declaration)
        |> List.hd_exn
      in
      let check_output = function
        | Res.Tool_output.Output.Text text -> [%test_eq: string] text "CAPTURED"
        | Content _ -> failwith "different source runner output"
      in
      tool.run {|{"input":"input"}|} |> check_output;
      tool.run_with_progress
        ~invocation:(Ochat_function.Invocation.create ignore)
        {|{"input":"input"}|}
      |> check_output;
      let identity = ref 42 in
      failure := Some identity;
      (match tool.run {|{"input":"input"}|} with
       | _ -> failwith "callback failure swallowed"
       | exception Source_callback_failed actual -> assert (phys_equal identity actual));
      [%test_eq: bool list] (Queue.to_list observed) [ false; true; false ];
      let sources = Queue.to_list sources in
      [%test_eq: int] (List.length sources) 3;
      [%test_eq: int]
        (List.length (List.dedup_and_sort sources ~compare:String.compare))
        3;
      assert (
        List.for_all sources ~f:(fun source ->
          not (String.equal source declaration.agent)))))
;;

let%test_unit "source policy revision changes agent permission identity only" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let elements =
      CM.parse_chat_inputs
        ~dir:env#cwd
        {|<tool name="child" agent="__och207_no_fetch__/missing.chatmd" local/><tool name="read_dir"/>|}
    in
    Startup_tests.with_host env ~elements (fun ~sw ~ctx ~host ->
      let prepare revision =
        let runner =
          Tool.Agent_source_runner.
            { revision = digest revision
            ; run_content =
                (fun ~declaration:_ ~ctx:_ -> failwith "unexpected content agent")
            ; run =
                (fun ~declaration:_ ~ctx:_ ~source:_ ~observer:_ _ ->
                  failwith "uninvoked source runner")
            }
        in
        R.create
          ~agent_source_runner:runner
          ~sw
          ~ctx
          ~host
          ~platform:Macos
          ~prompt_elements:elements
          ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
          ~approval_provider:Shell_runtime.Approval_broker.None_available
          ~approval_store:(Shell_access.Approval.create_store ())
          ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
            failwith "uninvoked legacy runner")
          ()
        |> Startup_tests.startup_ok
        |> fun (native : R.t) -> Lazy.force native.capabilities |> ok
      in
      let first = prepare "source-policy-a" in
      let second = prepare "source-policy-b" in
      let binding registry name = Capability.find registry ~name |> ok in
      let changed name =
        let first = binding first name in
        let second = binding second name in
        let revision binding = (Capability.reference binding).implementation_revision in
        ( not (String.equal (revision first) (revision second))
        , not
            (String.equal
               (Capability.permission_fingerprint first)
               (Capability.permission_fingerprint second)) )
      in
      [%test_eq: bool * bool] (changed "child") (true, true);
      (* Live capability IDs are newly allocated for both tools. Permission identity
         is the stable projection that isolates the source-policy change. *)
      [%test_eq: bool * bool] (changed "read_directory") (false, false)))
;;

let final_answer : Res.Response_stream.t =
  Output_item_done
    { item =
        Output_message
          { role = Assistant
          ; id = "prepared-answer"
          ; content = [ { annotations = []; text = "ANSWER"; _type = "output_text" } ]
          ; status = "completed"
          ; phase = None
          ; _type = "message"
          }
    ; output_index = 0
    ; type_ = "response.output_item.done"
    }
;;

let%expect_test "prepared Driver consumes captured graph and original literal projection" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let dir = Eio.Path.(env#cwd / "och207_prepared_driver") in
    Eio.Path.mkdirs ~perm:0o755 dir;
    let script_body =
      "let initial_state = Graph.value\n\
       let on_event = fun ctx state event -> Task.pure(state)"
    in
    let raw =
      {|<script id="handler" language="chatml" kind="moderator" src="moderator.chatml"/><user><doc src="note.txt" local capture/></user>|}
    in
    let sources =
      [ "root.chatmd", raw; "moderator.chatml", script_body; "note.txt", "frozen-note" ]
    in
    List.iter sources ~f:(fun (name, body) ->
      Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(dir / name) body);
    let parser_limits =
      CM.Limits.create
        ~max_input_bytes:65536
        ~max_parse_calls:16
        ~max_tokens:10000
        ~max_depth:32
      |> Or_error.ok_exn
    in
    let parse source_loader =
      CM.prepare_chat_inputs
        ~source:"root.chatmd"
        ~source_loader
        ~dir
        ~preprocess:Fn.id
        ~limits:parser_limits
        ~checkpoint
        raw
    in
    let parsed = parse (Source_loader.filesystem ~root:dir) in
    let projection =
      CM.materialize_documents
        parsed
        ~rendered:
          [ ( List.hd_exn (CM.Parsed.document_occurrences parsed)
            , Eio.Path.load Eio.Path.(dir / "note.txt") )
          ]
        ~max_occurrences:1
        ~max_output_bytes:65536
        ~max_work_steps:100000
        ~checkpoint
      |> Or_error.ok_exn
    in
    let contract =
      Chat_response.Moderator_program.contract
        Chatml.Chatml_builtin_surface.moderator_surface
        ~checkpoint
    in
    let binding =
      Custody.Binding.of_declaration
        parsed
        (List.hd_exn (CM.Parsed.declarations parsed))
        ~checkpoint
      |> Or_error.ok_exn
    in
    let authored =
      Chatmd_source_bundle.create ~root_file:"root.chatmd" ~sources ()
      |> Result.ok_or_failwith
    in
    let custody =
      Custody.capture
        authored
        ~parsed_contexts:[ parsed ]
        ~root:(CM.Parsed.root_input parsed)
        ~programs:
          [ ( binding
            , Custody.Input.Provided
                (Routing_tests.graph ~moderator:true ~contract "let value = 7") )
          ]
        ~active_programs:[ binding, Contract.fingerprint contract ~checkpoint ]
        ~limits:Routing_tests.limits
        ~checkpoint
      |> ok
    in
    let programs =
      Custody.restore
        custody
        ~parsed
        ~contracts:[ binding, contract ]
        ~limits:Routing_tests.limits
        ~checkpoint
      |> ok
    in
    List.iter sources ~f:(fun (name, _) -> Eio.Path.unlink Eio.Path.(dir / name));
    [%test_eq: string list] (Eio.Path.read_dir dir) [];
    let dispatches = ref 0 in
    let inference =
      Inference_fixture.create
        ~namespace:"prepared-driver"
        ~default_model:"fixture-model"
        ~post_stream:(fun ~sw:_ ~inputs ->
          Int.incr dispatches;
          assert (
            List.exists inputs ~f:(function
              | Res.Item.Input_message message ->
                List.exists message.content ~f:(function
                  | Res.Input_message.Text { text; _ } ->
                    String.is_substring text ~substring:"frozen-note"
                  | _ -> false)
              | _ -> false));
          Stdlib.List.to_seq [ final_answer ])
    in
    let ctx =
      Inference_fixture.ctx
        inference
        ~env
        ~dir
        ~tool_dir:dir
        ~cache:(Chat_response.Cache.create ~max_size:1 ())
        ()
    in
    let answer =
      Driver.run_prepared_agent parsed ~programs ~document_projection:projection ~ctx []
    in
    [%test_eq: string] answer "ANSWER";
    [%test_eq: int] !dispatches 1;
    let foreign = parse (Source_loader.captured_filesystem ~root:dir ~sources) in
    (match
       Driver.run_prepared_agent foreign ~programs ~document_projection:projection ~ctx []
     with
     | _ -> failwith "foreign parser projection admitted"
     | exception Invalid_argument message ->
       [%test_eq: string]
         message
         "prepared agent document projection belongs to another parser");
    [%test_eq: int] !dispatches 1;
    print_endline
      "captured graph and literal content survived removal; foreign parser rejected \
       before dispatch");
  [%expect
    {| captured graph and literal content survived removal; foreign parser rejected before dispatch |}]
;;

let%expect_test
    "prepared Driver dispatches child prompt and caller-owned nested input separately"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let prepare body =
      CM.prepare_chat_inputs
        ~source:"root.chatmd"
        ~source_loader:
          (Source_loader.captured_filesystem
             ~root:env#cwd
             ~sources:[ "root.chatmd", body ])
        ~dir:env#cwd
        ~preprocess:Fn.id
        ~limits:
          (CM.Limits.create
             ~max_input_bytes:65536
             ~max_parse_calls:16
             ~max_tokens:10000
             ~max_depth:32
           |> Or_error.ok_exn)
        ~checkpoint
        body
    in
    let child = prepare {|<user><agent src="child.chatmd" local>child</agent></user>|} in
    let caller =
      prepare
        {|<user><agent src="caller.chatmd" local><agent src="grandchild.chatmd" local>grandchild</agent></agent></user>|}
    in
    let input =
      match CM.Parsed.elements caller with
      | [ CM.User { content = Some (Items items); _ } ] -> items
      | _ -> assert false
    in
    let calls = Queue.create () in
    let reference parsed descriptor =
      CM.content_agent_reference parsed ~max_work_steps:100 ~checkpoint descriptor
      |> Or_error.ok_exn
    in
    let child_runner =
      Tool.Agent_source_runner.
        { revision = digest "child-revision"
        ; run = (fun ~declaration:_ ~ctx:_ ~source:_ ~observer:_ _ -> assert false)
        ; run_content =
            (fun ~declaration ~ctx:_ ->
              let key = CM.Agent_reference.queued_key (reference child declaration) in
              [%test_eq: string option] key (Some "child.chatmd");
              Queue.enqueue calls "child";
              "CHILD")
        }
    in
    let dynamic ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ =
      failwith "owned nested input fetched dynamically"
    in
    let rec caller_runner : Tool.Agent_source_runner.t =
      { revision = digest "caller-revision"
      ; run = (fun ~declaration:_ ~ctx:_ ~source:_ ~observer:_ _ -> assert false)
      ; run_content =
          (fun ~declaration ~ctx ->
            match CM.Agent_reference.queued_key (reference caller declaration) with
            | Some "caller.chatmd" ->
              Queue.enqueue calls "caller";
              "CALLER("
              ^ Chat_response.Converter.string_of_items
                  ~agent_source_runner:caller_runner
                  ~ctx
                  ~run_agent:dynamic
                  declaration.items
              ^ ")"
            | Some "grandchild.chatmd" ->
              Queue.enqueue calls "grandchild";
              "GRANDCHILD"
            | None | Some _ -> assert false)
      }
    in
    let dispatches = ref 0 in
    let inference =
      Inference_fixture.create
        ~namespace:"separate-agent-inputs"
        ~default_model:"fixture-model"
        ~post_stream:(fun ~sw:_ ~inputs ->
          Int.incr dispatches;
          let texts =
            List.concat_map inputs ~f:(function
              | Res.Item.Input_message message ->
                List.filter_map message.content ~f:(function
                  | Text { text; _ } -> Some text
                  | Image _ -> None)
              | _ -> [])
          in
          [%test_eq: string list] texts [ "CHILD"; "CALLER(GRANDCHILD)" ];
          Stdlib.List.to_seq [ final_answer ])
    in
    let ctx =
      Inference_fixture.ctx
        inference
        ~env
        ~dir:env#cwd
        ~tool_dir:env#cwd
        ~cache:(Chat_response.Cache.create ~max_size:1 ())
        ()
    in
    let expired = ref None in
    let polls = ref 0 in
    let limits = Chatml_compilation.default_limits in
    Chat_response.Startup_compilation.with_context
      ~limits
      ~env
      ~checkpoint:(fun () -> Int.incr polls)
      ~map_error:Fn.id
      ~f:(fun startup ->
        expired := Some startup;
        let answer =
          Driver.run_prepared_agent
            child
            ~agent_source_runner:child_runner
            ~input_agent_source_runner:caller_runner
            ~startup
            ~compilation_limits:limits
            ~ctx
            input
        in
        [%test_eq: string] answer "ANSWER";
        Ok ())
      ()
    |> ok;
    assert (!polls > 0);
    [%test_eq: string list] (Queue.to_list calls) [ "child"; "caller"; "grandchild" ];
    [%test_eq: int] !dispatches 1;
    let reject ?checkpoint ?startup () =
      match
        Driver.run_prepared_agent
          child
          ~agent_source_runner:child_runner
          ~input_agent_source_runner:caller_runner
          ?startup
          ~compilation_limits:limits
          ?checkpoint
          ~ctx
          input
      with
      | _ -> assert false
      | exception Failure message ->
        assert (String.is_substring message ~substring:"chatml.invalid_context")
    in
    reject ~startup:(Option.value_exn !expired) ();
    [%test_eq: int] !dispatches 1;
    let sentinel = Failure "original prepared startup checkpoint" in
    (match
       Driver.run_prepared_agent
         child
         ~agent_source_runner:child_runner
         ~input_agent_source_runner:caller_runner
         ~compilation_limits:limits
         ~checkpoint:(fun () -> raise sentinel)
         ~ctx
         input
     with
     | _ -> assert false
     | exception actual -> assert (phys_equal actual sentinel));
    [%test_eq: int] !dispatches 1;
    print_s [%sexp (Queue.to_list calls : string list)]);
  [%expect {| (child caller grandchild) |}]
;;
