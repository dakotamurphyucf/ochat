open Core
open Fixtures
module S = Chat_response.Runtime_request_scope
module R = Chat_response.Moderation.Runtime_request

let%expect_test "owned runtime requests cross domains but expire and remain isolated" =
  Eio_main.run (fun env ->
    let leaked = ref None in
    let (), requests =
      S.collect (fun () ->
        S.emit [ R.Request_turn; End_session "first" ] |> Result.ok_or_failwith;
        let captured = S.capture () in
        leaked := captured;
        Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
          S.with_context captured (fun () ->
            S.emit [ R.Request_compaction; Request_turn; End_session "second" ]
            |> Result.ok_or_failwith));
        let (), inner =
          S.collect (fun () ->
            S.emit [ R.End_session "separate" ] |> Result.ok_or_failwith)
        in
        print_s [%sexp (inner : R.t list)];
        assert (Result.is_error (S.with_context None (fun () -> S.emit []))))
    in
    print_s [%sexp (requests : R.t list)];
    let rejected = S.with_context !leaked (fun () -> S.emit [ R.Request_turn ]) in
    print_s [%sexp (rejected : (unit, string) result)];
    let failed = ref None in
    (try
       ignore
         (S.collect (fun () ->
            failed := S.capture ();
            failwith "cancelled")
          : unit * R.t list)
     with
     | _ -> ());
    let (), next =
      S.collect (fun () ->
        assert (Result.is_error (S.with_context !failed (fun () -> S.emit [])));
        S.emit [ R.Request_compaction ] |> Result.ok_or_failwith)
    in
    print_s [%sexp (next : R.t list)]);
  [%expect
    {|
    ((End_session separate))
    (Request_turn (End_session first) Request_compaction)
    (Error "runtime request scope has ended")
    (Request_compaction)
    |}]
;;

let%expect_test
    "borrowed actor execution restores its request owner across a domain handoff"
  =
  let module N = Agent_session.Native_tool_invocation in
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let requests = ref [] in
  let finished = ref false in
  with_handoff_actor
    ~make_worker:(fun env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
        let actor = Eio.Promise.await actor_ready in
        let registry =
          native_registry (ref 0) ~raises:false ~on_call:(fun () ->
            let borrow = N.borrow () |> protocol_ok in
            let parent = N.borrowed_invocation borrow in
            let child =
              I.create
                { parent.context with
                  id = Agent_protocol.Id.Invocation.create ()
                ; origin = Script
                ; provider_call_id = None
                ; call_entry_id = None
                ; parent_invocation = Some parent.context.id
                }
              |> protocol_ok
            in
            S.emit [ R.Request_turn ] |> Result.ok_or_failwith;
            Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
              (* Domain workers have no ambient collector. Only the actual expiring
               actor borrow may restore the caller's context here. *)
              assert (Option.is_none (S.capture ()));
              N.execute_borrowed borrow ~invocation:child (fun ~dispatched:_ ->
                S.emit [ R.Request_compaction ] |> Result.ok_or_failwith;
                Ok (I.Complete `Null))
              |> protocol_ok
              |> fun (_ : I.t) -> ()))
        in
        let call, invocation = publication_call caps () in
        let reference, invocation = native_context registry invocation in
        caps.commit_invocation_call ~invocation call |> protocol_ok;
        let resolved, collected =
          S.collect (fun () ->
            N.run
              ~capabilities:caps
              ~registry:(fun () -> registry)
              ~reference
              ~invocation
              ~is_halted:(fun () -> false)
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "disclosed"))
            |> protocol_ok)
        in
        requests := collected;
        let outcome =
          match resolved.status with
          | Resolved value -> value
          | _ -> assert false
        in
        let output =
          publication_output caps ~text:(Jsonaf.to_string (I.outcome_to_json outcome)) ()
        in
        caps.publish_invocation_output ~invocation_id:resolved.context.id output
        |> protocol_ok;
        let state = A.state actor |> protocol_ok in
        assert (Int.equal (List.length state.invocations) 2);
        finished := true;
        Completed
          { final_history =
              Agent_session.History_codec.all_of_protocol
                state.conversation.canonical_history
              |> protocol_ok
          ; moderator_snapshot = None
          ; runtime_requests = []
          }))
    (fun _env actor _writer backend ->
       let rec await () =
         let state = A.state actor |> protocol_ok in
         match state.active_operation, !finished with
         | None, true -> state
         | _ ->
           Eio.Fiber.yield ();
           await ()
       in
       let state = await () in
       assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
       print_s [%sexp (!requests : R.t list)]);
  [%expect {| (Request_turn Request_compaction) |}]
;;
