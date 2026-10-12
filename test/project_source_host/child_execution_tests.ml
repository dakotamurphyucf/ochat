open Core
module Fixture = Daemon_dependency_tests
module P = Agent_protocol
module C = Agent_client
module Daemon = Agent_server.Daemon
module Response = Openai.Responses.Response_stream

let ok = Agent_server_test_support.protocol_ok
let save = Native_tests.save

let call name id =
  let item arguments status =
    Response.Item.Function_call
      { name; arguments; call_id = id; _type = "function_call"; id = Some id; status }
  in
  let arguments = {|{"input":"Run the selected code."}|} in
  [ Response.Output_item_added
      { item = item "" None; output_index = 0; type_ = "response.output_item.added" }
  ; Function_call_arguments_done
      { arguments
      ; item_id = id
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item = item arguments (Some "completed")
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let answer text =
  [ Response.Output_item_done
      { item =
          Output_message
            { role = Assistant
            ; id = "configured-child-answer"
            ; content = [ { annotations = []; text; _type = "output_text" } ]
            ; status = "completed"
            ; phase = None
            ; _type = "message"
            }
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let texts inputs =
  List.concat_map inputs ~f:(function
    | Openai.Responses.Item.Input_message message ->
      List.filter_map message.content ~f:(function
        | Text { text; _ } -> Some (String.strip text)
        | Image _ -> None)
    | _ -> [])
;;

let spec () =
  P.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog Fixture.definition)
    ~workspace:(Configured (Agent_server.Catalog_identity.workspace_definition "project"))
    ~liveness:Detached
    ~persistence:Durable
    ~start_immediately:true
    ~labels:[]
    ()
  |> ok
;;

let%expect_test
    "configured imported children select fresh files and retain locked Git code"
  =
  Fixture.with_workspace (fun env root ->
    Eio.Switch.run (fun sw ->
      let helpers = Native_tests.helpers root in
      let inner_anchor = Eio.Path.(root / "inner-origin") in
      let outer_anchor = Eio.Path.(root / "outer-origin") in
      List.iter
        [ inner_anchor; outer_anchor; Eio.Path.(root / "nested") ]
        ~f:(Eio.Path.mkdir ~perm:0o700);
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "nested" / "code");
      let inner, _, _, _ = Git_fixture_support.fixture env inner_anchor Sha1 in
      let outer, _, _, _ = Git_fixture_support.fixture env outer_anchor Sha1 in
      let network = ref 0 in
      let serve repository =
        Https_fixture.start env#net ~sw ~handle:(fun request ->
          incr network;
          Git_fixture_support.smart_git_response env repository request)
        ^ "/repo"
      in
      let inner_url = serve inner in
      let outer_url = serve outer in
      let inner_pin =
        Fixture.commit_project env inner Fixture.inner_manifest "let value = 41"
      in
      let outer_pin =
        Fixture.commit_project
          env
          outer
          (Fixture.outer_manifest inner_url inner_pin)
          "let value = Nested.value"
      in
      save root "ca.pem" Tls_fixture.certificate;
      save
        root
        "root.chatmd"
        {|<developer>configured parent</developer><import src="nested/tools.chatmd"/>|};
      save
        root
        "nested/tools.chatmd"
        {|<tool name="fresh" agent="child.chatmd" local/>
<tool name="captured" agent="child.chatmd" local source_mode="captured"/>|};
      save
        root
        "nested/child.chatmd"
        {|<developer><doc src="guide.md" local capture/></developer>
<import src="code/owner.chatmd"/>|};
      save
        root
        "nested/code/owner.chatmd"
        {|<script id="owner" language="chatml" kind="moderator" src="entry.chatml" project="../../chatml.project.json"/>|};
      save
        root
        "nested/code/entry.chatml"
        "let initial_state = print(Own.value + External.value); ()\n\
         let on_event ctx state event = Task.pure(state)";
      save root "chatml.project.json" (Fixture.root_manifest outer_url outer_pin);
      let edit version value =
        save root "nested/guide.md" ("child " ^ version);
        save root "lookup.chatml" (sprintf "let value = %d" value)
      in
      edit "A" 1;
      let configuration = Fixture.config_text root helpers outer_url inner_url in
      save root "server.sexp" configuration;
      let config =
        Agent_server.Config_parser.parse_string
          ~source_file:(Eio.Path.native_exn Eio.Path.(root / "server.sexp"))
          configuration
        |> Support.ok
        |> Agent_server.Config_validator.validate ~env
        |> Support.ok
      in
      let phase = ref 0 in
      let requests = ref 0 in
      let initialized = Queue.create () in
      let selected = ref ("fresh", "B") in
      let post_stream ~sw:_ ~inputs =
        incr requests;
        let tool, version = !selected in
        match !phase with
        | 0 ->
          assert (List.mem (texts inputs) "configured parent" ~equal:String.equal);
          phase := 1;
          call tool (sprintf "child-%d" !requests)
        | 1 ->
          [%test_eq: string list]
            [ "child " ^ version; "Run the selected code." ]
            (texts inputs);
          phase := 2;
          answer ("completed-child-" ^ version)
        | 2 ->
          assert (
            List.exists inputs ~f:(fun item ->
              String.is_substring
                (Openai.Responses.Item.jsonaf_of_t item |> Jsonaf.to_string)
                ~substring:("completed-child-" ^ version)));
          phase := 3;
          Stdlib.Seq.empty
        | _ -> failwith "configured child requested an extra inference"
      in
      let daemon =
        Daemon.start
          ~sw
          ~env
          ~config
          ~tool_dir:(Eio.Path.native_exn root)
          ~home:(Eio.Path.native_exn root)
          ~process_start_identity:None
          ~options:
            { Daemon.default_options with
              startup_mode = On_demand
            ; inference_policy =
                Agent_server_test_support.inference_policy
                  ~default_model:"fixture-model"
                  ~post_stream
            }
          ()
        |> ok
      in
      Exn.protect
        ~finally:(fun () -> Daemon.shutdown daemon |> ok)
        ~f:(fun () ->
          let original_revision =
            Agent_session.Prompt_revision.id (Fixture.ready daemon)
          in
          let connection =
            Fixture.connect daemon (Agent_server_test_support.principal ()) (ref true)
          in
          Exn.protect
            ~finally:(fun () -> C.Connection.close connection)
            ~f:(fun () ->
              let changed = Eio.Condition.create () in
              let handle =
                C.Session_handle.create
                  ~sw
                  ~clock:(Eio.Stdenv.clock env)
                  ~connection
                  ~spec:(spec ())
                  ~mode:Read_write
                  ~on_update:(fun _ -> Eio.Condition.broadcast changed)
                  ()
                |> ok
              in
              Exn.protect
                ~finally:(fun () -> C.Session_handle.close handle)
                ~f:(fun () ->
                  assert (!network > 0);
                  let acquired = !network in
                  ignore
                    (Fixture.commit_project
                       env
                       inner
                       Fixture.inner_manifest
                       "let value = 99"
                     : string);
                  Chatml.Chatml_builtin_spec.set_print_sink (Queue.enqueue initialized);
                  Exn.protect
                    ~finally:Chatml.Chatml_builtin_spec.clear_print_sink
                    ~f:(fun () ->
                      let run tool version =
                        phase := 0;
                        selected := tool, version;
                        ignore
                          (C.Session_handle.send_message
                             handle
                             { kind = Plain_text
                             ; text = "Call the selected child."
                             ; attachments = []
                             }
                           |> ok
                           : P.Method_result.Send_message.t);
                        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                          Eio.Fiber.first
                            (fun () ->
                               Eio.Condition.loop_no_mutex changed (fun () ->
                                 let fields =
                                   C.Session_handle.projection handle
                                   |> C.Projection.snapshot
                                   |> P.Public.Snapshot.fields
                                 in
                                 Option.iter fields.failure ~f:(fun error ->
                                   raise_s [%sexp (error : P.Error.t)]);
                                 if
                                   !phase = 3
                                   && Option.is_none fields.session.active_operation
                                 then Some ()
                                 else None))
                            (fun () ->
                               C.Session_handle.await_closed handle;
                               failwith "child fixture reader closed"));
                        [%test_eq: int] acquired !network;
                        assert (
                          P.Id.Prompt_revision.equal
                            original_revision
                            (Fixture.session_revision
                               daemon
                               (C.Session_handle.session_id handle)))
                      in
                      edit "B" 2;
                      run "fresh" "B";
                      [%test_eq: string list] [ "43" ] (Queue.to_list initialized);
                      edit "C" 3;
                      run "fresh" "C";
                      [%test_eq: string list] [ "43"; "44" ] (Queue.to_list initialized);
                      Eio.Path.rmtree Eio.Path.(root / "nested");
                      Eio.Path.unlink Eio.Path.(root / "lookup.chatml");
                      Eio.Path.unlink Eio.Path.(root / "chatml.project.json");
                      run "captured" "A";
                      [%test_eq: string list]
                        [ "43"; "44"; "42" ]
                        (Queue.to_list initialized);
                      [%test_eq: int] 9 !requests;
                      print_endline
                        "default File executes B then C; Captured executes A after \
                         original files are removed";
                      print_endline
                        "imported project initializers run independently; both Git \
                         dependencies remain pinned without network"))))));
  [%expect
    {|
    default File executes B then C; Captured executes A after original files are removed
    imported project initializers run independently; both Git dependencies remain pinned without network
    |}]
;;
