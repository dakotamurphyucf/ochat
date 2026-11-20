open! Core
open Fixtures
module A = Agent_session
module Actor = A.Session_actor
module P = Agent_protocol
module Query = A.Recovery_history_query
module Access = Actor.Recovery_history_access
module Identity = Recovery_note_actor_tests
module Storage = Recovery_note_share_tests
module Native = A.Native_tool_invocation
module Capability = Chat_response.Tool_capability

let%expect_test
    "model history pagination renews from actual invocations in one foreground"
  =
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      let credential = Identity.proof [ Send_messages; View_session_transcript ] in
      let query =
        Query.create
          ~source:Identity.owner
          ~scope:Current_generation
          ~family:Canonical
          ~view:Ordinary
          (Search (P.Search_term.create "fixture" |> protocol_ok))
        |> protocol_ok
      in
      let digest = Chatml_source.Digest.to_hex (Query.digest query) in
      let payload =
        Query.Continuation.of_json
          (`Object
              [ "query", `String digest
              ; "basis", `String digest
              ; "windows", `Number "1"
              ; "last_window_members", `Number "2"
              ; "last_preparation_revision", `Null
              ; "next_position", `Number "1"
              ])
        |> protocol_ok
      in
      let module Definition = struct
        type input = string

        let name = "context_search"
        let description = None
        let type_ = "function"
        let parameters = `Object [ "type", `String "object" ]
        let input_of_string input = input
      end
      in
      let actor_ref = ref None in
      let original_proof = ref None in
      let original_request = ref None in
      let cursor = ref None in
      let calls = ref 0 in
      let implementation =
        Ochat_function.create_function
          (module Definition)
          (fun _ ->
             let actor = Option.value_exn !actor_ref in
             let borrowed = Native.borrow () |> protocol_ok in
             let selected = Native.borrowed_capabilities borrowed |> protocol_ok in
             let binding =
               Capability.find selected ~name:Definition.name
               |> Result.ok
               |> Option.value_exn
             in
             let admission = Native.Admission.capture borrowed ~binding |> protocol_ok in
             let request = Access.model admission ~binding in
             let observation = Actor.observe_resource_state actor |> protocol_ok in
             let proof = Access.capture observation request ~query |> protocol_ok in
             Int.incr calls;
             (match !calls with
              | 1 ->
                original_proof := Some proof;
                original_request := Some request;
                let encoded = Access.seal_cursor proof payload |> protocol_ok in
                cursor := Some (P.Page.Cursor.of_string encoded |> protocol_ok)
              | 2 ->
                let resumed =
                  Access.read_cursor proof (Option.value_exn !cursor) |> protocol_ok
                in
                [%test_eq: int] resumed.next_position payload.next_position
              | 3 ->
                Access.read_cursor proof (Option.value_exn !cursor)
                |> Identity.expect_error Cursor_expired
              | _ -> failwith "unexpected history tool invocation");
             Openai.Responses.Tool_output.Output.Text "ack")
      in
      let declared =
        Capability.create
          ~owner:"context-history-access-fixture"
          ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "history-access")
          [ Chatmd_shell_spec.Source_ref.digest "implementation", implementation ]
        |> Result.ok
        |> Option.value_exn
      in
      let tools, tool_tbl = Ochat_function.functions [ implementation ] in
      let attempts = ref 0 in
      let inference =
        Inference_fixture.create
          ~namespace:"history-access-fixture"
          ~default_model:"history-access-fixture"
          ~post_stream:(fun ~sw:_ ~inputs:_ ->
            Int.incr attempts;
            (match !attempts with
             | 2 | 3 ->
               assert (
                 Result.is_error
                   (Access.principal (Option.value_exn !original_request) ~query));
               Access.check_current (Option.value_exn !original_proof)
               |> protocol_ok
               |> ignore
             | _ -> ());
            match !attempts with
            | 1 | 2 | 4 ->
              Recovery_rollover_tests.call_event
                ~name:Definition.name
                ~arguments:"{}"
                !attempts
            | 3 | 5 -> Seq.empty
            | _ -> failwith "unexpected history provider attempt")
      in
      let target =
        Inference_fixture.capture_config inference Chat_response.Config.default
        |> Configuration_tests.runtime_ok
      in
      let initial = Storage.source_state workspace Identity.owner in
      let inference_target =
        Inference.Selection.captured target ~limits:document_limits
        |> Configuration_tests.inference_ok
      in
      let initial = { initial with spec = { initial.spec with inference_target } } in
      let journal =
        Identity.journal
          Agent_store.Journal.create
          env
          (Filename.concat workspace.canonical_root.native_path "history-access")
      in
      Storage.with_writer
        env
        sw
        journal
        (A.Session_persistence.Restored.authored initial)
        ~owner:Identity.owner
        ~before_commit:None
        ~now:P.Timestamp.now
        (fun actor _ ->
           actor_ref := Some actor;
           let attachment_id = Identity.attach actor Owner_read_write in
           let services =
             A.Script_tool_calls.create
               ~registry:(fun () -> declared)
               ~moderator_names:String.Set.empty
               ~now:P.Timestamp.now
               ~is_halted:(fun () -> false)
               ~requires_active_moderator:(fun _ -> false)
               ~authorize:(fun _ _ -> Ok ())
               ~prepare_output:(function
                 | Openai.Responses.Tool_output.Output.Text text -> Ok (`String text)
                 | output -> Ok (Openai.Responses.Tool_output.Output.jsonaf_of_t output))
               ~defer_observation:(fun _ -> Ok ())
           in
           let config =
             Authoring_input_tests.worker_config env (fun ~sw:_ ~inputs:_ -> Seq.empty)
           in
           let worker =
             A.Turn_worker.create
               ~root_binding:
                 (Chat_response.Root_binding.create
                    (Inference_runtime.Session.create ~sw))
               ~dispatch_tool:(fun ~input ~capabilities ->
                 A.Script_tool_calls.native_dispatch
                   services
                   ~declared
                   ~input
                   ~capabilities)
               { config with
                 inference_context =
                   Inference_fixture.resolve inference target
                   |> Configuration_tests.runtime_ok
               ; inference_identity = (fun _ -> Inference_fixture.identity inference)
               ; tools = Chat_response.Tool.convert_tools tools
               ; tool_tbl
               ; permission_profile =
                   permission_policy
                     ~tool_default:Allow
                     ~fallback:Fallback_deny
                     ~evaluator:None
                     ~reviewer:None
               }
           in
           Actor.set_operation_worker actor (Some worker) |> protocol_ok;
           Actor.start actor ~attachment_id |> protocol_ok |> ignore;
           let submit () =
             let reserved = Actor.reserve_history_block actor ~count:1 |> protocol_ok in
             let entry =
               Pending_input_tests.occurrence
                 (Int64.to_int_exn reserved.first_sequence)
                 "fixture"
             in
             Actor.submit_message_authenticated
               actor
               ~actor:credential
               ~attachment_id
               entry
             |> protocol_ok
             |> ignore;
             Authoring_input_tests.finished actor |> ignore
           in
           submit ();
           assert (
             Result.is_error (Access.check_current (Option.value_exn !original_proof)));
           submit ();
           [%test_eq: int] !calls 3;
           [%test_eq: int] !attempts 5;
           print_endline
             "same foreground resumes across provider attempts; the next foreground \
              starts fresh")));
  [%expect
    {| same foreground resumes across provider attempts; the next foreground starts fresh |}]
;;
