open Core
open Fixtures
module A = Agent_session.Session_actor
module I = Agent_protocol.Invocation
module M = Chat_response.Moderator_manager

let%expect_test "event and idle descendants require a live parent in the exact owner" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let manager, _ = handoff_manager env in
      let snapshot () = M.identity_snapshot manager |> Result.ok_or_failwith in
      let observer = M.invocation_observer manager |> Option.value_exn in
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let initial =
        { initial with
          moderator =
            Some (Agent_session.Runtime_builder.encode_moderator_snapshot (snapshot ()))
        }
      in
      let backend =
        Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
      in
      let actor =
        A.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:initial
          ~operation_worker:None
          ~persistence:(Agent_session.Memory_backend.persistence backend)
          ~services:
            { now = Agent_protocol.Timestamp.now
            ; create_attachment_id = Agent_protocol.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "descendants")
            ; job_results = None
            ; state_committed = (fun _ _ -> ())
            }
      in
      Exn.protect
        ~finally:(fun () -> A.shutdown actor)
        ~f:(fun () ->
          let writer, _ =
            A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
          in
          A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
          let deadline =
            Agent_protocol.Timestamp.of_string "2100-01-01T00:00:00Z" |> protocol_ok
          in
          let context =
            { (invocation_fixture ()).context with
              origin = Moderator
            ; provider_call_id = None
            ; call_entry_id = None
            ; parent_invocation = None
            ; parent_job = None
            ; deadline = Some deadline
            }
          in
          let rejected = ref 0
          and bodies = ref 0
          and escaped = ref [] in
          let reject result =
            match result with
            | Error _ -> incr rejected
            | Ok _ -> failwith "invalid descendant was admitted"
          in
          let exercise (execute : Agent_session.Native_tool_invocation.executor) root =
            let child ?observer parent =
              I.create
                ?observer
                { parent.I.context with
                  id = Agent_protocol.Id.Invocation.create ()
                ; origin = Script
                ; parent_invocation = Some parent.context.id
                }
              |> protocol_ok
            in
            let callback ~dispatched:_ =
              incr bodies;
              Ok (I.Complete `Null)
            in
            let resolved =
              execute ~invocation:root (fun ~dispatched ->
                let candidate = child dispatched in
                let before = A.state actor |> protocol_ok in
                List.iter
                  [ { candidate.context with
                      parent_invocation = Some (Agent_protocol.Id.Invocation.create ())
                    }
                  ; { candidate.context with deadline = None }
                  ; { candidate.context with session_id = second_session_id }
                  ]
                  ~f:(fun context ->
                    reject
                      (execute ~invocation:(I.create context |> protocol_ok) callback));
                let foreign = { observer with script_id = "different-owner" } in
                reject (execute ~invocation:(child ~observer:foreign dispatched) callback);
                assert_same_session_snapshot before (A.state actor |> protocol_ok);
                execute ~invocation:candidate callback |> protocol_ok |> ignore;
                Ok (I.Complete `Null))
              |> protocol_ok
            in
            reject (execute ~invocation:(child resolved) callback);
            escaped
            := (fun () -> execute ~invocation:(child resolved) callback) :: !escaped;
            resolved
          in
          let root = ref None in
          let claimed =
            A.with_current_moderator_event
              actor
              ~operation_id:None
              ~event:Session_start
              ~snapshot:(fun () -> Ok (snapshot ()))
              (fun ~executing ~event:_ ~execute ~commit ->
                 let invocation =
                   I.create ~observer ~parent_event:executing.context.id context
                   |> protocol_ok
                 in
                 root := Some (exercise execute invocation);
                 commit
                   ~snapshot:(snapshot ())
                   ~requests:
                     { request_turn = false
                     ; request_compaction = false
                     ; end_session = None
                     })
            |> protocol_ok
          in
          assert claimed;
          let observed =
            A.with_idle_moderator_observation_tools
              actor
              ~observer
              (fun ~observing ~execute ~commit ->
                 assert (
                   Agent_protocol.Id.Invocation.equal
                     observing.context.id
                     (Option.value_exn !root).context.id);
                 let invocation =
                   I.create
                     ~observer
                     { context with
                       id = Agent_protocol.Id.Invocation.create ()
                     ; parent_invocation = Some observing.context.id
                     }
                   |> protocol_ok
                 in
                 ignore (exercise execute invocation : I.t);
                 let resolved = I.complete_observation observing |> protocol_ok in
                 commit ~resolved ~snapshot:(snapshot ()))
            |> protocol_ok
          in
          assert observed;
          List.iter !escaped ~f:(fun call -> reject (call ()));
          let state = A.state actor |> protocol_ok in
          assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
          assert (List.is_empty state.conversation.canonical_history);
          print_s
            [%sexp
              (!rejected : int), (!bodies : int), (List.length state.invocations : int)])));
  [%expect {| (12 2 4) |}]
;;
