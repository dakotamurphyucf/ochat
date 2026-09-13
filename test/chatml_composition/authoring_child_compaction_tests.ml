open Core
open Agent_server_test_support
open Fixtures
module Flow = Authoring_compaction_tests
module P = Agent_protocol
module C = Chat_response.Tool_capability
module V = Chat_response.Authoring_validation
module State = Agent_session.Session_state

let with_child
      ~sources
      ~calls
      ~request_counts
      ~inspect_request
      ~followup_calls
      ~after_turn
      ~settle
      f
  =
  let host = Authoring_context_tests.host () in
  let child_requests = ref 0 in
  let checked = ref false in
  let sources =
    List.map sources ~f:(fun (name, source) ->
      match name with
      | "agent.chatmd" ->
        ( name
        , source
          ^ {|
<developer>PARENT-ONLY-AUTHORING-INSTRUCTION</developer>
<tool name="parent_only_probe" type="chatml" script="work" entrypoint="run" input_schema="any.json" output_schema="any.json"/>|}
        )
      | _ -> name, source)
  in
  with_daemon
    ~validation_host:host
    ~sources
    ~calls:[]
    ~request_counts:(fun () ->
      let count = snd (request_counts ()) in
      1, if count = Int.max_value then count else count + 1)
    ~inspect_request:(fun number inputs ->
      match number with
      | 1 -> ()
      | _ ->
        child_requests := number - 1;
        let encoded =
          `Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t)
          |> Jsonaf.to_string
        in
        List.iter
          [ "PARENT-ONLY-AUTHORING-INSTRUCTION"; "parent_only_probe" ]
          ~f:(fun private_ ->
            assert (not (String.is_substring encoded ~substring:private_)));
        inspect_request (number - 1) inputs)
    ~followup_calls:(function
      | 2 -> calls
      | number -> followup_calls (number - 1))
    ~after_turn_with_daemon:(fun env daemon parent ->
      Eio.Switch.run (fun sw ->
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
        assert (Result.is_ok (C.find caps ~name:"parent_only_probe"));
        let names =
          [ "ochat_authoring_context"; "ochat_validate"; "run_chatml"; "fixture_work" ]
        in
        let references =
          C.references caps
          |> List.filter ~f:(fun reference ->
            List.mem names reference.C.name ~equal:String.equal)
        in
        assert (List.length references = 4);
        let bundle =
          Chatmd_source_bundle.create
            ~root_file:"child.chatmd"
            ~sources:
              [ ( "child.chatmd"
                , {|<developer>CHILD-AUTHORING-INSTRUCTION</developer>
<authoring_context policy="manual"/>
<tool type="inherited" name="ochat_authoring_context"/>
<tool type="inherited" name="ochat_validate"/>
<tool type="inherited" name="run_chatml"/>
<tool type="inherited" name="fixture_work"/>|}
                )
              ]
            ()
          |> Result.ok_or_failwith
        in
        let definition =
          Agent_session.Generated_definition.prepare
            ?catalog:(V.delegated_catalog host)
            ~env
            ~dir:(Eio.Stdenv.fs env)
            ~revision_id:(P.Id.Prompt_revision.create ())
            ~created_at:(P.Timestamp.now ())
            ~current_capabilities:(fun () -> caps)
            ~references
            bundle
          |> Result.map_error ~f:(fun diagnostics ->
            String.concat
              ~sep:"\n"
              (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string))
          |> Result.ok_or_failwith
        in
        let admitted = Agent_session.Generated_definition.admission definition in
        let child_caps = Chat_response.Generated_admission.capabilities admitted in
        assert (Result.is_error (C.find child_caps ~name:"parent_only_probe"));
        assert (
          List.equal
            String.equal
            (List.sort names ~compare:String.compare)
            (C.references child_caps
             |> List.map ~f:(fun reference -> reference.C.name)
             |> List.sort ~compare:String.compare));
        let parent_before = A.state parent.actor |> protocol_ok in
        let child =
          Agent_server.Session_factory.create_generated_session
            ~start_immediately:true
            (Agent_server.Daemon.factory daemon)
            ~parent_session_id:parent_before.identity.session_id
            ~idempotency_key:
              (P.Idempotency_key.of_string "authoring-compaction-child" |> protocol_ok)
            ~display_name:None
            definition
          |> protocol_ok
        in
        let connection = connection daemon (principal ()) in
        initialize connection;
        Exn.protect
          ~finally:(fun () -> Agent_client.Connection.close connection)
          ~f:(fun () ->
            let state () = A.state child.actor |> protocol_ok in
            let handle =
              H.attach
                ~sw
                ~clock:(Eio.Stdenv.clock env)
                ~connection
                ~session_id:(state ()).identity.session_id
                ~mode:Read_write
                ~subscribe:false
                ()
              |> protocol_ok
            in
            Exn.protect
              ~finally:(fun () -> H.close handle)
              ~f:(fun () ->
                H.send_message
                  handle
                  { kind = Plain_text
                  ; text = "Learn the background coordinator contract."
                  ; attachments = []
                  }
                |> protocol_ok
                |> ignore;
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                  let rec ready () =
                    let current = state () in
                    Option.iter current.failure ~f:(fun error ->
                      raise_s [%sexp (error : P.Error.t)]);
                    match
                      Option.is_none current.active_operation
                      && !child_requests >= fst (request_counts ())
                    with
                    | true -> ()
                    | false ->
                      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                      ready ()
                  in
                  ready ());
                after_turn env handle child;
                settle env child;
                let final = state () in
                List.iter final.invocations ~f:(fun invocation ->
                  Option.iter
                    invocation.P.Invocation.authoring_reference
                    ~f:(fun reference ->
                      assert (String.equal reference.surface_id "delegated_moderator_v1");
                      assert (
                        String.equal
                          reference.scope
                          (P.Authoring_reference.scope_for
                             ~session_id:final.identity.session_id
                             ~generation:final.identity.generation))));
                let parent_after = A.state parent.actor |> protocol_ok in
                assert (
                  List.equal
                    P.History.equal_entry
                    parent_before.conversation.canonical_history
                    parent_after.conversation.canonical_history);
                assert (List.is_empty parent_after.invocations);
                f final;
                checked := true))))
    (fun _ -> assert !checked)
;;

let%expect_test
    "X10 narrowed generated child compacts and authors from its own delegated references"
  =
  Flow.exercise with_child;
  [%expect
    {| paginated background package -> persisted compaction -> manual pointer -> fresh retrieval -> moderator validation -> executed completion parser |}]
;;
