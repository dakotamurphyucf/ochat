open Core
open Fixtures
module A = Agent_session.Session_actor
module I = Agent_protocol.Invocation
module P = Agent_protocol.Permission

let%expect_test "nested moderator handoff owns its descendants and permission requests" =
  List.iter [ `Commit; `Abandon ] ~f:(fun mode ->
    let done_, done_u = Eio.Promise.create () in
    let rejected = ref 0 in
    let reject result =
      match result with
      | Error _ -> incr rejected
      | Ok _ -> failwith "invalid handoff accepted"
    in
    with_handoff_actor
      ~make_worker:(fun _env actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let fresh ?parent ~origin ~name () =
            I.create
              { (invocation_fixture ()).context with
                id = Agent_protocol.Id.Invocation.create ()
              ; tool_name = name
              ; origin
              ; parent_invocation = parent
              ; deadline =
                  Some
                    (Agent_protocol.Timestamp.of_string "2100-01-01T00:00:00Z"
                     |> protocol_ok)
              }
            |> protocol_ok
          in
          let expired = ref None in
          caps.with_invocation
            ~invocation:(fresh ~origin:Script ~name:"caller" ())
            (fun ~dispatched:caller ->
               let invocation =
                 fresh ~parent:caller.context.id ~origin:Script ~name:"moderated" ()
               in
               expired := Some invocation.context;
               let before = A.state actor |> protocol_ok in
               List.iter
                 [ { invocation.context with
                     parent_invocation = Some (Agent_protocol.Id.Invocation.create ())
                   }
                 ; { invocation.context with deadline = None }
                 ]
                 ~f:(fun context ->
                   reject
                     (caps.with_moderator_invocation
                        ~invocation:(I.create context |> protocol_ok)
                        (fun ~dispatched:_ ~commit:_ -> failwith "invalid callback ran")));
               assert_same_session_snapshot before (A.state actor |> protocol_ok);
               let entered, entered_u = Eio.Promise.create () in
               let release, release_u = Eio.Promise.create () in
               let children_done, children_done_u = Eio.Promise.create () in
               let handoff =
                 caps.with_moderator_invocation ~invocation (fun ~dispatched ~commit ->
                   let permission : P.t =
                     { id = Agent_protocol.Id.Permission.create ()
                     ; session_id = input.session_id
                     ; generation = input.session_generation
                     ; owner = Invocation dispatched.context.id
                     ; call_id =
                         Agent_protocol.Id.Invocation.to_string dispatched.context.id
                     ; tool_name = "moderated"
                     ; runtime_identity = None
                     ; invocation_display = "moderated(<redacted>)"
                     ; rationale = None
                     ; effects = [ "tool_invocation" ]
                     ; choices = [ Approve_once; Deny ]
                     ; created_at = timestamp
                     ; expires_at = None
                     ; state = Pending
                     ; resolution = None
                     }
                   in
                   let decision =
                     caps.request_review ~permission ~review:(fun () -> Ok Allow)
                     |> protocol_ok
                   in
                   assert (P.equal_choice decision.choice Approve_once);
                   Eio.Fiber.fork ~sw (fun () ->
                     let child_result =
                       caps.with_invocation
                         ~invocation:
                           (fresh
                              ~parent:dispatched.context.id
                              ~origin:Moderator
                              ~name:"native-child"
                              ())
                         (fun ~dispatched:child ->
                            let grandchild_result =
                              caps.with_invocation
                                ~invocation:
                                  (fresh
                                     ~parent:child.context.id
                                     ~origin:Script
                                     ~name:"grandchild"
                                     ())
                                (fun ~dispatched:_ ->
                                   Eio.Promise.resolve entered_u ();
                                   Eio.Promise.await release;
                                   Ok (I.Complete `Null))
                            in
                            (match mode with
                             | `Commit -> ignore (protocol_ok grandchild_result : I.t)
                             | `Abandon -> reject grandchild_result);
                            Ok (I.Complete `Null))
                     in
                     (match mode with
                      | `Commit -> ignore (protocol_ok child_result : I.t)
                      | `Abandon -> reject child_result);
                     Eio.Promise.resolve children_done_u ());
                   Eio.Promise.await entered;
                   let resolved =
                     I.resolve
                       dispatched
                       ~session_id:input.session_id
                       ~generation:input.session_generation
                       (Complete `Null)
                     |> protocol_ok
                   in
                   reject (commit ~resolved ~snapshot:(handoff_snapshot 1));
                   assert (Option.is_none (A.state actor |> protocol_ok).moderator);
                   match mode with
                   | `Abandon ->
                     Error (Agent_protocol.Error.invalid_request "handler abandoned")
                   | `Commit ->
                     Eio.Promise.resolve release_u ();
                     Eio.Promise.await children_done;
                     commit ~resolved ~snapshot:(handoff_snapshot 1))
               in
               (match mode with
                | `Commit -> protocol_ok handoff
                | `Abandon ->
                  reject handoff;
                  Eio.Promise.resolve release_u ();
                  Eio.Promise.await children_done);
               Ok (I.Complete `Null))
          |> protocol_ok
          |> ignore;
          let context = Option.value_exn !expired in
          reject
            (caps.with_moderator_invocation
               ~invocation:
                 (I.create { context with id = Agent_protocol.Id.Invocation.create () }
                  |> protocol_ok)
               (fun ~dispatched:_ ~commit:_ -> failwith "expired caller admitted"));
          let state = A.state actor |> protocol_ok in
          Eio.Promise.resolve done_u ();
          Completed
            { final_history = input.history
            ; moderator_snapshot = state.moderator
            ; runtime_requests = []
            }))
      (fun _env actor _writer backend ->
         Eio.Promise.await done_;
         let state = await_idle actor in
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
         assert (Int.equal (List.length state.conversation.canonical_history) 1);
         let outcomes =
           List.map state.invocations ~f:(fun invocation ->
             ( invocation.I.context.tool_name
             , match invocation.status with
               | Resolved (Complete _) -> "complete"
               | Resolved (Cancelled _) -> "cancelled"
               | Resolved (Fail _) -> "failed"
               | _ -> failwith "unfinished invocation" ))
           |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
         in
         print_s
           [%sexp
             (mode : [ `Commit | `Abandon ])
           , (!rejected : int)
           , (outcomes : (string * string) list)
           , (Option.is_some state.moderator : bool)
           , (List.map state.permissions ~f:(fun permission -> permission.P.state)
              : P.state list)]));
  [%expect
    {|
    (Commit 4
     ((caller complete) (grandchild complete) (moderated complete)
      (native-child complete))
     true (Approved))
    (Abandon 7
     ((caller complete) (grandchild cancelled) (moderated failed)
      (native-child cancelled))
     false (Approved))
    |}]
;;

let%expect_test "a queued moderator loan cannot outlive its native caller" =
  let module N = Agent_session.Native_tool_invocation in
  let module C = Chat_response.Tool_capability in
  let done_, done_u = Eio.Promise.create () in
  let rejected = ref false in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      let on_native = ref (fun () -> ()) in
      let registry =
        native_registry (ref 0) ~raises:false ~on_call:(fun () -> !on_native ())
      in
      Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let held, held_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let holder_done, holder_done_u = Eio.Promise.create () in
        let queued_done, queued_done_u = Eio.Promise.create () in
        let attempted, attempted_u = Eio.Promise.create () in
        let fresh =
          I.create
            { (invocation_fixture ()).context with
              id = Agent_protocol.Id.Invocation.create ()
            }
          |> protocol_ok
        in
        Eio.Fiber.fork ~sw (fun () ->
          caps.with_moderator_invocation ~invocation:fresh (fun ~dispatched ~commit ->
            Eio.Promise.resolve held_u ();
            Eio.Promise.await release;
            let resolved =
              I.resolve
                dispatched
                ~session_id:input.session_id
                ~generation:input.session_generation
                (Complete `Null)
              |> protocol_ok
            in
            commit ~resolved ~snapshot:(handoff_snapshot 0))
          |> protocol_ok;
          Eio.Promise.resolve holder_done_u ());
        Eio.Promise.await held;
        (on_native
         := fun () ->
              let borrowed = N.borrow () |> protocol_ok in
              let parent = N.borrowed_invocation borrowed in
              let execute = N.moderator_executor borrowed |> Option.value_exn in
              let child =
                I.create
                  { parent.context with
                    id = Agent_protocol.Id.Invocation.create ()
                  ; origin = Script
                  ; parent_invocation = Some parent.context.id
                  }
                |> protocol_ok
              in
              Eio.Fiber.fork ~sw (fun () ->
                Eio.Promise.resolve attempted_u ();
                let result =
                  execute ~invocation:child (fun ~dispatched:_ ~commit:_ ->
                    failwith "expired queued caller entered moderator")
                in
                rejected := Result.is_error result;
                Eio.Promise.resolve queued_done_u ());
              Eio.Promise.await attempted;
              Eio.Fiber.yield ());
        let reference = List.hd_exn (C.references registry) in
        let invocation =
          I.create
            { (invocation_fixture ()).context with
              id = Agent_protocol.Id.Invocation.create ()
            ; tool_name = reference.name
            ; input = `Object []
            ; implementation_revision = reference.implementation_revision
            ; capability_fingerprint = C.fingerprint registry
            }
          |> protocol_ok
        in
        N.run
          ~capabilities:caps
          ~registry:(fun () -> registry)
          ~reference
          ~invocation
          ~is_halted:(fun () -> false)
          ~authorize:(fun _ _ -> Ok ())
          ~prepare_output:(fun _ -> Ok `Null)
        |> protocol_ok
        |> ignore;
        Eio.Promise.resolve release_u ();
        Eio.Promise.await holder_done;
        Eio.Promise.await queued_done;
        let state = A.state actor |> protocol_ok in
        assert (Int.equal (List.length state.invocations) 2);
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history
          ; moderator_snapshot = state.moderator
          ; runtime_requests = []
          }))
    (fun _env actor _writer backend ->
       Eio.Promise.await done_;
       let state = await_idle actor in
       assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
       print_s [%sexp (!rejected : bool), (List.length state.invocations : int)]);
  [%expect {| (true 2) |}]
;;
