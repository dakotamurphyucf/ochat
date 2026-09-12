open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol

let guidance state =
  List.filter
    state.Agent_session.Session_state.conversation.canonical_history
    ~f:(fun entry ->
      match entry.P.History.provenance with
      | Runtime_authoring _ -> true
      | _ -> false)
;;

let%expect_test
    "generated children inherit authoring helpers within the requested ceiling and \
     reload guidance"
  =
  let child_inputs = ref [] in
  with_daemon
    ~sources:
      [ ( "agent.chatmd"
        , {|<developer>PARENT-AUTHORING-MARKER</developer><tool name="run_chatml"/>|} )
      ]
    ~calls:[]
    ~initial_requests:1
    ~expected_requests:3
    ~inspect_request:(fun number inputs ->
      if number > 1 then child_inputs := inputs :: !child_inputs)
    ~after_turn_with_daemon:(fun env daemon parent ->
      Eio.Switch.run (fun sw ->
        let module C = Chat_response.Tool_capability in
        let host =
          Agent_session.Authoring_runtime.configure_host
            ~policy:Chat_response.One_off_request.default_policy
            ()
          |> Result.ok_or_failwith
        in
        let caps =
          Agent_server.Runtime_owner.with_background_runtime
            parent.runtime
            (fun runtime ->
               Lazy.force
                 (Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime)
                   .capabilities
               |> Result.map_error ~f:(fun error ->
                 P.Error.invalid_request error.C.message))
          |> protocol_ok
        in
        let bundle =
          Chatmd_source_bundle.create
            ~root_file:"child.chatmd"
            ~sources:
              [ ( "child.chatmd"
                , {|<developer>CHILD-AUTHORING-MARKER</developer><tool type="inherited" name="run_chatml"/>|}
                )
              ]
            ()
          |> Result.ok_or_failwith
        in
        let prepare references =
          Agent_session.Generated_definition.prepare
            ?catalog:(Chat_response.Authoring_validation.catalog host)
            ~env
            ~dir:(Eio.Stdenv.fs env)
            ~revision_id:(P.Id.Prompt_revision.create ())
            ~created_at:(P.Timestamp.now ())
            ~current_capabilities:(fun () -> caps)
            ~references
            bundle
        in
        let refs = C.references caps in
        (match
           prepare
             (List.filter refs ~f:(fun reference ->
                String.equal reference.C.name "run_chatml"))
         with
         | Error diagnostics ->
           assert (
             List.exists diagnostics ~f:(fun diagnostic ->
               String.equal
                 diagnostic.Chatmd_shell_spec.Diagnostic.code
                 "authoring.helper_unavailable"))
         | Ok _ -> failwith "generated auto policy widened the requested ceiling");
        let definition =
          prepare refs
          |> Result.map_error ~f:(fun errors ->
            String.concat
              ~sep:"\n"
              (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
          |> Result.ok_or_failwith
        in
        let parent_state = A.state parent.actor |> protocol_ok in
        let child =
          Agent_server.Session_factory.create_generated_session
            ~start_immediately:true
            (Agent_server.Daemon.factory daemon)
            ~parent_session_id:parent_state.identity.session_id
            ~idempotency_key:(P.Idempotency_key.of_string "authoring-child" |> protocol_ok)
            ~display_name:None
            definition
          |> protocol_ok
        in
        let connection = connection daemon (principal ()) in
        initialize connection;
        Exn.protect
          ~finally:(fun () -> Agent_client.Connection.close connection)
          ~f:(fun () ->
            let child_state () = A.state child.actor |> protocol_ok in
            let handle =
              H.attach
                ~sw
                ~clock:(Eio.Stdenv.clock env)
                ~connection
                ~session_id:(child_state ()).identity.session_id
                ~mode:Read_write
                ~subscribe:false
                ()
              |> protocol_ok
            in
            let send () =
              H.send_message
                handle
                { kind = Plain_text; text = "Inspect your tools."; attachments = [] }
              |> protocol_ok
              |> ignore;
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                let rec idle () =
                  match (child_state ()).active_operation with
                  | None -> ()
                  | Some _ ->
                    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                    idle ()
                in
                idle ());
              assert (List.length (guidance (child_state ())) = 1)
            in
            send ();
            H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
            unload_idle_runtime env child.runtime;
            H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
            send ();
            H.close handle)))
    (fun _ ->
       assert (List.length !child_inputs = 2);
       List.iter !child_inputs ~f:(fun inputs ->
         let text =
           Jsonaf.to_string
             (`Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t))
         in
         assert (String.is_substring text ~substring:"CHILD-AUTHORING-MARKER");
         assert (not (String.is_substring text ~substring:"PARENT-AUTHORING-MARKER"));
         assert (String.is_substring text ~substring:"authoring.primer"));
       print_endline
         "bounded inherited helpers; child-owned guidance; reload deduplicated");
  [%expect {| bounded inherited helpers; child-owned guidance; reload deduplicated |}]
;;

let%expect_test
    "qualified root registration applies automatic, manual and preload guidance"
  =
  List.iter
    [ "auto", "", true, []
    ; "manual", {|<authoring_context policy="manual"/>|}, false, []
    ; ( "manual-reference"
      , {|<authoring_context policy="manual"/>|}
      , false
      , [ "ochat_authoring_context" ] )
    ; ( "manual-validation"
      , {|<authoring_context policy="manual"/>|}
      , false
      , [ "ochat_validate" ] )
    ; "preload", {|<authoring_context policy="preload" topics="chatml.tasks"/>|}, true, []
    ]
    ~f:(fun (label, policy, automatic, explicit_helpers) ->
      let initial = ref [] in
      with_daemon
        ~sources:
          [ ( "agent.chatmd"
            , policy
              ^ {|<developer>Return the input.</developer><tool name="run_chatml"/><tool name="agent_create"/>|}
              ^ String.concat
                  (List.map explicit_helpers ~f:(fun name ->
                     sprintf "<tool name=%S/>" name)) )
          ]
        ~calls:
          [ ( "script"
            , "run_chatml"
            , `Object
                [ "source", `String "let main input = Task.pure(input)"
                ; "input", `String "ok"
                ; "tools", `Array []
                ] )
          ]
        ~inspect_request:(fun number inputs -> if number = 1 then initial := inputs)
        ~after_turn:(fun _ _ entry ->
          let tools, capabilities =
            Agent_server.Runtime_owner.with_background_runtime
              entry.runtime
              (fun runtime ->
                 let tools =
                   List.filter_map
                     runtime.Agent_session.Runtime_builder.moderator_tools
                     ~f:(function
                     | Openai.Responses.Request.Tool.Function tool -> Some tool
                     | _ -> None)
                 in
                 let capabilities =
                   Lazy.force (Option.value_exn runtime.native_runtime).capabilities
                   |> Result.map_error ~f:(fun error ->
                     P.Error.invalid_request error.Chat_response.Tool_capability.message)
                 in
                 Result.map capabilities ~f:(fun capabilities -> tools, capabilities))
            |> protocol_ok
          in
          let names =
            List.map tools ~f:(fun tool -> tool.name) |> List.sort ~compare:String.compare
          in
          let expected =
            ([ "agent_create"; "run_chatml" ]
             @
             if automatic
             then [ "ochat_authoring_context"; "ochat_validate" ]
             else explicit_helpers)
            |> List.sort ~compare:String.compare
          in
          assert (List.equal String.equal names expected);
          let module C = Chat_response.Tool_capability in
          let module Q = Chat_response.Authoring_context in
          let service =
            Q.create ~secret:"description-integration" () |> Result.ok_or_failwith
          in
          let host =
            Agent_session.Authoring_runtime.configure_host
              ~policy:Chat_response.One_off_request.default_policy
              ()
            |> Result.ok_or_failwith
          in
          let references capabilities =
            Q.query
              service
              ~host
              ~capabilities
              ~scope:label
              (Authoring_context_tests.request
                 ~task:"one_off_script"
                 ~topic_id:"reference.tools"
                 "topic")
            |> Authoring_context_tests.items
          in
          let described = references capabilities in
          let narrowed =
            C.select capabilities ~names:[ "agent_create"; "run_chatml" ]
            |> Result.map_error ~f:(fun error -> error.C.message)
            |> Result.ok_or_failwith
          in
          let narrowed_descriptions = references narrowed in
          List.iter [ "agent_create"; "run_chatml" ] ~f:(fun name ->
            let tool = List.find_exn tools ~f:(fun tool -> String.equal name tool.name) in
            let description = Option.value_exn tool.description in
            assert (
              String.is_substring description ~substring:"Authoring reference package:");
            List.iter [ "ochat_authoring_context"; "ochat_validate" ] ~f:(fun helper ->
              assert (
                Bool.equal
                  (String.is_substring description ~substring:helper)
                  (List.mem names helper ~equal:String.equal)));
            let item items =
              List.find_exn items ~f:(fun item ->
                String.equal name (Jsonaf.member_exn "name" item |> Jsonaf.string_exn))
            in
            let reference = item described in
            assert (
              String.equal
                description
                (Jsonaf.member_exn "description" reference |> Jsonaf.string_exn));
            assert (
              Jsonaf.exactly_equal
                tool.parameters
                (Jsonaf.member_exn "input_schema" reference));
            let restricted = item narrowed_descriptions in
            assert (
              Jsonaf.exactly_equal
                (Jsonaf.member_exn "binding_fingerprint" restricted)
                (Jsonaf.member_exn "binding_fingerprint" reference));
            let text = Jsonaf.member_exn "description" restricted |> Jsonaf.string_exn in
            assert (String.is_substring text ~substring:"Authoring reference package:");
            List.iter [ "ochat_authoring_context"; "ochat_validate" ] ~f:(fun helper ->
              assert (not (String.is_substring text ~substring:helper)))))
        (fun state ->
           (match result state "script" with
            | Complete (`String "ok") -> ()
            | other -> raise_s [%sexp (other : I.outcome)]);
           let references = guidance state in
           List.iter references ~f:(fun reference ->
             assert (
               List.exists !initial ~f:(fun item ->
                 Jsonaf.exactly_equal
                   reference.payload
                   (Openai.Responses.Item.jsonaf_of_t item))));
           let topics =
             List.concat_map references ~f:(fun entry ->
               match entry.P.History.provenance with
               | Runtime_authoring guidance ->
                 List.map guidance.topics ~f:(fun topic -> topic.P.Authoring_guidance.id)
               | _ -> assert false)
           in
           assert (Option.is_none (List.find_a_dup topics ~compare:String.compare));
           print_s [%sexp (label : string), (topics : string list)]));
  [%expect
    {|
    (auto (authoring.primer))
    (manual ())
    (manual-reference ())
    (manual-validation ())
    (preload
     (authoring.primer chatml.introduction chatml.syntax.calls
      chatml.syntax.containers chatml.types chatml.operators chatml.tasks))
    |}]
;;

let%expect_test "ordinary qualified agents gain neither helper tools nor authoring prose" =
  with_daemon
    ~sources:[ "agent.chatmd", "<developer>Respond briefly.</developer>" ]
    ~calls:[]
    ~initial_requests:1
    ~expected_requests:1
    ~after_turn:(fun _ _ entry ->
      Agent_server.Runtime_owner.with_background_runtime entry.runtime (fun runtime ->
        assert (List.is_empty runtime.Agent_session.Runtime_builder.moderator_tools);
        Ok ())
      |> protocol_ok)
    (fun state ->
       assert (List.is_empty (guidance state));
       print_endline "ordinary input preserved");
  [%expect {| ordinary input preserved |}]
;;
