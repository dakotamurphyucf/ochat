open Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle
module Res = Openai.Responses

let field = Jsonaf.member_exn
let text json name = field name json |> Jsonaf.string_exn

let state daemon id =
  let entry = R.load (D.registry daemon) id |> protocol_ok in
  A.state entry.actor |> protocol_ok
;;

let function_call name arguments =
  let open Res.Response_stream in
  [ Output_item_added
      { item =
          Function_call
            { name
            ; arguments = ""
            ; call_id = "helper-call"
            ; _type = "function_call"
            ; id = Some "helper-item"
            ; status = None
            }
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { arguments = Jsonaf.to_string arguments
      ; item_id = "helper-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let run env helper runner =
  Mirage_crypto_rng_unix.use_default ();
  let root = temporary_root env |> Caml_unix.realpath in
  let path file = Eio.Path.(Eio.Stdenv.fs env / file) in
  Exn.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (path root))
    ~f:(fun () ->
      let public = Filename.concat root "public" in
      Eio.Path.mkdir ~perm:0o700 (path public);
      let helper_path = Filename.concat public "helper" in
      let runner_path = Filename.concat public "runner" in
      List.iter
        [ helper, helper_path; runner, runner_path ]
        ~f:(fun (source, target) ->
          Eio.Path.save
            ~create:(`Exclusive 0o700)
            (path target)
            (Eio.Path.load (path source)));
      Caml_unix.putenv "OCHAT_SHELL_RESOURCE_RUNNER" runner_path;
      let prompt = Filename.concat root "parent.chatmd" in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        (path prompt)
        {|<developer>HELPER_PARENT</developer>
<tool name="run_chatml"/>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<shell_access id="helper" cwd="${workspace}">
  <capabilities sandbox="required" network="false" child_processes="true" arbitrary_code="true" privilege_change="false"><read path="${workspace}"/></capabilities>
  <environment inherit="selected"><set name="PATH" value="/usr/bin:/bin"/></environment>
  <policy default="allow"/>
  <limits wall_time="30s" idle_time="none"/>
  <audit format="none"/>
</shell_access>
<tool name="session_bridge" type="shell" mode="fixed" runtime="helper" command="./helper" stdin="required" result="stdout"/>
<tool name="session_view" type="shell" mode="fixed" runtime="helper" command="./helper" stdin="required" result="stdout"/>|};
      let configuration = config root public prompt in
      let configuration =
        { configuration with
          workspaces =
            List.map configuration.workspaces ~f:(fun workspace ->
              { workspace with
                prompt_limits =
                  List.map workspace.prompt_limits ~f:(fun limit ->
                    { limit with max_root_agents = 2 })
              })
        }
      in
      let grant name allowed =
        Agent_session.Session_management_channel.grant
          ~policy_revision:"helper-fixture-v1"
          ~tool_name:name
          ~allowed
          ~limits:Shell_access.Request_channel.default_limits
          ~authorize:(fun context ->
            let caps = context.Shell_access.Context.capabilities in
            match
              String.equal context.executable.canonical_path helper_path
              && List.equal
                   String.equal
                   (List.map caps.read_roots ~f:(String.rstrip ~drop:(Char.equal '/')))
                   [ public ]
              && List.is_empty caps.write_roots
              && Array.for_all context.environment ~f:(fun entry ->
                List.mem
                  [ "PATH=/bin:/usr/bin"
                  ; "PAGER=cat"
                  ; "GIT_PAGER=cat"
                  ; "TERM=dumb"
                  ; "NO_COLOR=1"
                  ]
                  entry
                  ~equal:String.equal)
            with
            | true -> Ok ()
            | false -> Error "helper must retain the exact public-only fixture boundary")
        |> Result.ok_or_failwith
      in
      let grants =
        [ grant "session_bridge" [ Create; Send; Read; Status; Wait; Stop ]
        ; grant "session_view" [ Read; Status; Wait ]
        ]
      in
      let queued = ref None in
      let child_tool = ref None in
      let child_calls = ref 0 in
      let private_file = Filename.concat root "private-fixture.txt" in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        (path private_file)
        "HELPER_PRIVATE_FIXTURE_CONTENT";
      let provider ~sw:_ ~inputs =
        let child =
          List.exists inputs ~f:(function
            | Res.Item.Input_message message ->
              let json = Res.Item.jsonaf_of_t (Input_message message) in
              String.equal (text json "role") "developer"
              && String.is_substring (Jsonaf.to_string json) ~substring:"HELPER_CHILD"
            | _ -> false)
        in
        (match child with
         | true -> Int.incr child_calls
         | false -> ());
        match child with
        | false ->
          (match !queued with
           | None -> Stdlib.Seq.empty
           | Some (name, args) ->
             queued := None;
             function_call name args)
        | true when Option.is_some !child_tool ->
          let arguments = Option.value_exn !child_tool in
          child_tool := None;
          function_call "read_file" arguments
        | true ->
          let answer = "persisted helper answer" in
          let message : Res.Output_message.t =
            { role = Assistant
            ; id = "helper-answer"
            ; status = "completed"
            ; content = [ { annotations = []; text = answer; _type = "output_text" } ]
            ; phase = None
            ; _type = "message"
            }
          in
          let item = Res.Response_stream.Item.Output_message message in
          [ Res.Response_stream.Output_item_added
              { item; output_index = 0; type_ = "response.output_item.added" }
          ; Output_text_delta
              { item_id = message.id
              ; output_index = 0
              ; content_index = 0
              ; delta = answer
              ; type_ = "response.output_text.delta"
              }
          ; Output_item_done
              { item; output_index = 0; type_ = "response.output_item.done" }
          ]
          |> Stdlib.List.to_seq
      in
      let await predicate =
        let rec loop () =
          match predicate () with
          | true -> ()
          | false ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            loop ()
        in
        loop ()
      in
      let with_daemon ?(grants = grants) f =
        Eio.Switch.run (fun sw ->
          let daemon =
            D.start
              ~sw
              ~env
              ~config:configuration
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:
                { D.default_options with
                  qualify_chatml_extensions = true
                ; session_helpers = grants
                ; model_post_stream = Some provider
                }
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    initialize client;
                    f sw daemon client))))
      in
      let invoke sw daemon client parent name arguments =
        let before = state daemon parent in
        let handle =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection:client
            ~session_id:parent
            ~mode:Read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        queued := Some (name, arguments);
        H.send_message
          handle
          { kind = Plain_text; text = "Run the helper workflow."; attachments = [] }
        |> protocol_ok
        |> ignore;
        await (fun () ->
          Option.is_none (state daemon parent).active_operation && Option.is_none !queued);
        H.close handle;
        let after = state daemon parent in
        let invocation =
          List.find_exn after.invocations ~f:(fun invocation ->
            P.Invocation.equal_origin invocation.context.origin Model
            && not
                 (List.exists before.invocations ~f:(fun old ->
                    P.Id.Invocation.equal old.context.id invocation.context.id)))
        in
        match invocation.status with
        | Published (Complete (`String source)) -> source
        | status ->
          raise_s [%sexp "helper invocation failed", (status : P.Invocation.status)]
      in
      let bridge ?(name = "session_bridge") sw daemon client parent operation arguments =
        let envelope =
          `Object
            [ "version", `Number "1"
            ; "operation", `String operation
            ; "arguments", arguments
            ]
        in
        let source =
          invoke
            sw
            daemon
            client
            parent
            name
            (`Object [ "stdin", `String (Jsonaf.to_string envelope) ])
        in
        match Jsonaf.of_string source |> P.Invocation.outcome_of_json with
        | Ok outcome -> outcome
        | Error error -> failwith (error.message ^ ": " ^ source)
      in
      let complete = function
        | P.Invocation.Complete value -> value
        | outcome ->
          raise_s [%sexp "bridge application failed", (outcome : P.Invocation.outcome)]
      in
      let id value = text value "session_id" |> P.Id.Session.of_string |> protocol_ok in
      let child_request =
        `Object
          [ "version", `Number "1"
          ; "root_file", `String "child.chatmd"
          ; ( "sources"
            , `Array
                [ `Object
                    [ "path", `String "child.chatmd"
                    ; ( "text"
                      , `String
                          {|<authoring_context policy="manual"/><developer>HELPER_CHILD</developer><tool type="inherited" name="read_file"/>|}
                      )
                    ]
                ] )
          ; "tools", `Array [ `String "read_file" ]
          ; "start_immediately", `True
          ; "idempotency_key", `String "helper-child"
          ]
      in
      let parent_id, child_id, receipt =
        with_daemon (fun sw daemon client ->
          let parent, _ = create_session ~start_immediately:true client in
          await (fun () -> Option.is_none (state daemon parent.id).active_operation);
          let entry = R.load (D.registry daemon) parent.id |> protocol_ok in
          Agent_server.Runtime_owner.with_background_runtime entry.runtime (fun runtime ->
            let native = Option.value_exn runtime.native_runtime in
            let registry =
              Lazy.force native.capabilities
              |> Result.map_error ~f:(fun error ->
                error.Chat_response.Tool_capability.message)
              |> Result.ok_or_failwith
            in
            List.iter
              [ "agent_create"
              ; "agent_send"
              ; "agent_read"
              ; "agent_status"
              ; "agent_wait"
              ; "agent_stop"
              ]
              ~f:(fun name ->
                assert (
                  Result.is_error (Chat_response.Tool_capability.find registry ~name)));
            Ok ())
          |> protocol_ok;
          let create_envelope =
            `Object
              [ "version", `Number "1"
              ; "operation", `String "create"
              ; "arguments", child_request
              ]
          in
          let created =
            invoke
              sw
              daemon
              client
              parent.id
              "run_chatml"
              (`Object
                  [ ( "source"
                    , `String
                        {|let main input =
  let* result = Tool.call("session_bridge", input) in
  match result with
  | `Ok(value) -> Task.pure(value)
  | `Error(code) -> Task.fail(code)|}
                    )
                  ; ( "input"
                    , `Object [ "stdin", `String (Jsonaf.to_string create_envelope) ] )
                  ; "tools", `Array [ `String "session_bridge"; `String "read_file" ]
                  ])
            |> Jsonaf.of_string
            |> P.Invocation.outcome_of_json
            |> protocol_ok
            |> complete
          in
          let child = id created in
          [%test_eq: string]
            (text created "session_id")
            (bridge sw daemon client parent.id "create" child_request
             |> complete
             |> fun value -> text value "session_id");
          let target = [ "session_id", P.Id.Session.to_json child ] in
          ignore
            (bridge
               ~name:"session_view"
               sw
               daemon
               client
               parent.id
               "status"
               (`Object target)
             |> complete);
          (match
             bridge
               ~name:"session_view"
               sw
               daemon
               client
               parent.id
               "send"
               (`Object
                   (target
                    @ [ "message", `String "denied"
                      ; "idempotency_key", `String "view-denied"
                      ]))
           with
           | Fail error -> [%test_eq: string] "agent.management.denied" error.code
           | _ -> failwith "readonly helper gained send authority");
          await (fun () -> Option.is_none (state daemon child).active_operation);
          child_tool := Some (`Object [ "file", `String private_file ]);
          let sent =
            bridge
              sw
              daemon
              client
              parent.id
              "send"
              (`Object
                  (target
                   @ [ "message", `String "answer now"
                     ; "idempotency_key", `String "helper-message"
                     ]))
            |> complete
          in
          let receipt = text sent "receipt_id" in
          let query = target @ [ "receipt_id", `String receipt ] in
          let waited =
            bridge
              sw
              daemon
              client
              parent.id
              "wait"
              (`Object (query @ [ "timeout_ms", `Number "10000" ]))
            |> complete
          in
          [%test_eq: string] "receipt_terminal" (text waited "reason");
          assert (Option.is_none !child_tool);
          let child_results =
            (state daemon child).invocations
            |> List.filter_map ~f:(fun invocation ->
              match invocation.status with
              | Published (Complete value)
                when String.equal invocation.context.tool_name "read_file" ->
                Some (Jsonaf.to_string value)
              | _ -> None)
          in
          assert (
            List.exists child_results ~f:(fun value ->
              String.is_substring value ~substring:"outside the configured read roots"));
          assert (
            List.for_all child_results ~f:(fun value ->
              not (String.is_substring value ~substring:"HELPER_PRIVATE_FIXTURE_CONTENT")));
          let output =
            bridge sw daemon client parent.id "read" (`Object query)
            |> complete
            |> Jsonaf.to_string
          in
          assert (String.is_substring output ~substring:"persisted helper answer");
          let foreign, _ =
            create_session ~start_immediately:true ~key:"foreign-parent" client
          in
          assert (not (P.Id.Session.equal parent.id foreign.id));
          (match bridge sw daemon client foreign.id "status" (`Object target) with
           | Fail _ -> ()
           | _ -> failwith "foreign parent accessed helper-created child");
          ignore
            (bridge
               sw
               daemon
               client
               parent.id
               "stop"
               (`Object
                   (target
                    @ [ "mode", `String "graceful"
                      ; "idempotency_key", `String "helper-stop"
                      ]))
             |> complete);
          parent.id, child, receipt)
      in
      with_daemon (fun sw daemon client ->
        let handle =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection:client
            ~session_id:parent_id
            ~mode:Read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
        H.close handle;
        let target = [ "session_id", P.Id.Session.to_json child_id ] in
        let replay =
          bridge sw daemon client parent_id "create" child_request |> complete
        in
        assert (P.Id.Session.equal child_id (id replay));
        let output =
          bridge
            sw
            daemon
            client
            parent_id
            "read"
            (`Object (target @ [ "receipt_id", `String receipt ]))
          |> complete
          |> Jsonaf.to_string
        in
        assert (String.is_substring output ~substring:"persisted helper answer");
        let child_handle =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection:client
            ~session_id:child_id
            ~mode:Read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        H.start child_handle ~queue_if_limited:false |> protocol_ok |> ignore;
        await (fun () -> Option.is_none (state daemon child_id).active_operation);
        H.stop child_handle ~mode:Graceful |> protocol_ok |> ignore;
        H.close child_handle);
      with_daemon
        ~grants:
          [ grant "session_bridge" [ Create; Send; Read; Status; Wait; Stop ]
          ; grant "session_view" [ Read; Status; Wait; Send ]
          ]
        (fun sw daemon client ->
           let handle =
             H.attach
               ~sw
               ~clock:(Eio.Stdenv.clock env)
               ~connection:client
               ~session_id:parent_id
               ~mode:Read_write
               ~subscribe:false
               ()
             |> protocol_ok
           in
           H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
           H.close handle;
           (* Stored status/history remain readable; reactivating the old resource
             binding must fail before any child provider/tool execution. *)
           ignore
             (bridge
                sw
                daemon
                client
                parent_id
                "status"
                (`Object [ "session_id", P.Id.Session.to_json child_id ])
              |> complete);
           let before = !child_calls in
           let child_handle =
             H.attach
               ~sw
               ~clock:(Eio.Stdenv.clock env)
               ~connection:client
               ~session_id:child_id
               ~mode:Read_write
               ~subscribe:false
               ()
             |> protocol_ok
           in
           (match H.start child_handle ~queue_if_limited:false with
            | Error _ -> ()
            | Ok _ -> await (fun () -> Option.is_some (state daemon child_id).failure));
           H.close child_handle;
           [%test_eq: int] before !child_calls);
      print_endline
        "real shell helper lifecycle without native registrations, scoped grants, \
         foreign denial and restart PASS")
;;

let () =
  let args = Sys.get_argv () in
  Eio_main.run (fun env ->
    run env (Caml_unix.realpath args.(1)) (Caml_unix.realpath args.(2)))
;;
