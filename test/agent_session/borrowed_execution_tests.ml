open Core
open Fixtures

let%expect_test "borrowed native execution retains actor ownership and expires" =
  let module N = Agent_session.Native_tool_invocation in
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module C = Chat_response.Tool_capability in
  List.iter [ `Success; `Script; `Denied; `Revoked; `Cancel ] ~f:(fun mode ->
    let calls = ref 0 in
    let on_native = ref (fun () -> ()) in
    let initial =
      native_registry calls ~raises:false ~on_call:(fun () -> !on_native ())
    in
    let registry = ref initial in
    let saved = ref [] in
    let rejected = ref 0 in
    let worker_finished = ref false in
    let waiting, waiting_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let deadline =
      Agent_protocol.Timestamp.of_string "2099-01-01T00:00:00Z" |> protocol_ok
    in
    let child scope =
      let parent = N.borrowed_invocation scope in
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
    let reject result =
      match result with
      | Error _ -> Int.incr rejected
      | Ok _ -> failwith "borrowed executor accepted invalid ownership"
    in
    with_handoff_actor
      ~make_worker:(fun _env actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let reference = List.hd_exn (C.references initial) in
          let authorize invocation _ =
            match invocation.I.context.origin, mode with
            | Script, `Denied -> Error (handoff_error "denied by native policy")
            | Script, `Revoked ->
              registry := native_registry (ref 0) ~raises:false;
              Ok ()
            | _ -> Ok ()
          in
          let native execute invocation =
            N.run_scoped
              ~execute
              ~registry:(fun () -> !registry)
              ~reference
              ~invocation
              ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
              ~authorize
              ~prepare_output:(fun _ -> Ok (`String "disclosed"))
          in
          let release, release_u = Eio.Promise.create () in
          let probed, probed_u = Eio.Promise.create () in
          let nesting = ref 0 in
          (on_native
           := fun () ->
                let scope = N.borrow () |> protocol_ok in
                saved := scope :: !saved;
                let parent = N.borrowed_invocation scope in
                let state = A.state actor |> protocol_ok in
                assert (List.mem state.invocations parent ~equal:I.equal);
                let depth = !nesting in
                Int.incr nesting;
                Exn.protect
                  ~finally:(fun () -> Int.decr nesting)
                  ~f:(fun () ->
                    match depth with
                    | 0 ->
                      Eio.Fiber.fork ~sw (fun () ->
                        Eio.Promise.await release;
                        (match N.current_scope () with
                         | Expired -> ()
                         | Unbound | Active _ -> failwith "scope escaped in forked fiber");
                        reject (N.borrow ());
                        reject
                          (N.execute_borrowed
                             scope
                             ~invocation:(child scope)
                             (fun ~dispatched:_ ->
                                failwith "expired executor reached callback"));
                        Eio.Promise.resolve probed_u ());
                      let candidate = child scope in
                      let before = A.state actor |> protocol_ok in
                      List.iter
                        [ { candidate.context with
                            parent_invocation =
                              Some (Agent_protocol.Id.Invocation.create ())
                          }
                        ; { candidate.context with session_id = second_session_id }
                        ; { candidate.context with
                            generation = candidate.context.generation + 1
                          }
                        ; { candidate.context with deadline = None }
                        ; { candidate.context with
                            deadline =
                              Some
                                (Agent_protocol.Timestamp.of_string "2100-01-01T00:00:00Z"
                                 |> protocol_ok)
                          }
                        ; { candidate.context with origin = Moderator }
                        ]
                        ~f:(fun context ->
                          reject
                            (N.execute_borrowed
                               scope
                               ~invocation:(I.create context |> protocol_ok)
                               (fun ~dispatched:_ ->
                                  failwith "forged child reached callback")));
                      assert_same_session_snapshot before (A.state actor |> protocol_ok);
                      let resolved =
                        (match mode with
                         | `Script ->
                           N.execute_borrowed
                             scope
                             ~invocation:candidate
                             (fun ~dispatched ->
                                let script_scope = N.borrow () |> protocol_ok in
                                assert (
                                  I.equal (N.borrowed_invocation script_scope) dispatched);
                                saved := script_scope :: !saved;
                                let result =
                                  native
                                    (N.execute_borrowed script_scope)
                                    (child script_scope)
                                  |> protocol_ok
                                in
                                (match N.current_scope () with
                                 | Active current -> assert (I.equal current dispatched)
                                 | Unbound | Expired ->
                                   failwith "script scope was not restored");
                                match result.status with
                                | Resolved outcome -> Ok outcome
                                | _ -> failwith "script child has no outcome")
                         | _ -> native (N.execute_borrowed scope) candidate)
                        |> protocol_ok
                      in
                      (match mode, resolved.status with
                       | (`Success | `Script), Resolved (Complete _)
                       | ( `Denied
                         , Resolved (Fail { code = "invocation.permission_denied"; _ }) )
                       | ( `Revoked
                         , Resolved (Fail { code = "invocation.stale_binding"; _ }) ) ->
                         ()
                       | _ -> failwith "unexpected nested outcome");
                      (match N.current_scope () with
                       | Active current -> assert (I.equal current parent)
                       | Expired | Unbound ->
                         failwith "outer native scope was not restored");
                      (* The root is still live: expired child borrows cannot
                         silently use this different active invocation. *)
                      List.iter !saved ~f:(fun nested ->
                        match
                          Agent_protocol.Id.Invocation.equal
                            (N.borrowed_invocation nested).context.id
                            parent.context.id
                        with
                        | true -> ()
                        | false ->
                          reject
                            (N.execute_borrowed
                               nested
                               ~invocation:(child nested)
                               (fun ~dispatched:_ ->
                                  failwith "child borrow outlived its native call")))
                    | 1 ->
                      (match mode with
                       | `Cancel ->
                         Eio.Promise.resolve waiting_u ();
                         Eio.Promise.await never
                       | _ ->
                         native (N.execute_borrowed scope) (child scope)
                         |> protocol_ok
                         |> fun (resolved : I.t) ->
                         assert (
                           I.equal_status
                             resolved.status
                             (Resolved (Complete (`String "disclosed")))))
                    | 2 -> ()
                    | _ -> failwith "unbounded test recursion"));
          let call, invocation = publication_call caps () in
          let _, invocation = native_context initial invocation in
          let invocation =
            I.create { invocation.context with deadline = Some deadline } |> protocol_ok
          in
          caps.commit_invocation_call ~invocation call |> protocol_ok;
          let resolved = native caps.with_invocation invocation |> protocol_ok in
          caps.publish_invocation_output
            ~invocation_id:resolved.context.id
            (publication_output caps ~text:{|{"type":"complete","value":"disclosed"}|} ())
          |> protocol_ok;
          Eio.Promise.resolve release_u ();
          Eio.Promise.await probed;
          (match N.current_scope () with
           | Unbound -> ()
           | Expired | Active _ -> failwith "native scope leaked into worker");
          let state = A.state actor |> protocol_ok in
          worker_finished := true;
          Completed
            { final_history =
                Agent_session.History_codec.all_of_protocol
                  state.conversation.canonical_history
                |> protocol_ok
            ; moderator_snapshot = None
            ; runtime_requests = []
            }))
      (fun _env actor writer backend ->
         (match mode with
          | `Cancel ->
            Eio.Promise.await waiting;
            A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore
          | _ -> ());
         let rec finished () =
           let state = A.state actor |> protocol_ok in
           match state.active_operation with
           | None -> state
           | Some _ ->
             Eio.Fiber.yield ();
             finished ()
         in
         let state = finished () in
         assert (Option.is_none state.failure);
         (match mode with
          | `Cancel -> assert (not !worker_finished)
          | _ -> assert !worker_finished);
         List.iter state.invocations ~f:(fun invocation ->
           match invocation.context.parent_invocation, invocation.status, mode with
           | None, Published (Complete _), (`Success | `Script | `Denied | `Revoked)
           | None, (Resolved (Cancelled _) | Published (Cancelled _)), `Cancel
           | Some _, _, _ -> ()
           | _ -> raise_s [%sexp "unexpected root result", (invocation : I.t)]);
         List.iter !saved ~f:(fun scope ->
           reject
             (N.execute_borrowed scope ~invocation:(child scope) (fun ~dispatched:_ ->
                failwith "finished operation retained borrowed authority")));
         assert_same_session_snapshot state (A.state actor |> protocol_ok);
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
         let children =
           List.filter state.invocations ~f:(fun invocation ->
             Option.is_some invocation.context.parent_invocation)
         in
         List.iter children ~f:(fun invocation ->
           assert (I.equal_origin invocation.context.origin Script);
           assert (Option.is_none invocation.context.provider_call_id);
           assert (Option.is_none invocation.context.call_entry_id);
           assert (Option.is_none invocation.output_entry_id);
           assert (
             Option.equal
               Agent_protocol.Timestamp.equal
               invocation.context.deadline
               (Some deadline)));
         let outcomes =
           List.map children ~f:(fun invocation ->
             match invocation.status with
             | Resolved (Complete _) -> "complete"
             | Resolved (Fail error) -> error.code
             | Resolved (Cancelled _) -> "cancelled"
             | _ -> failwith "child did not retain its terminal outcome")
           |> List.sort ~compare:String.compare
         in
         print_s
           [%sexp
             (mode : [ `Success | `Script | `Denied | `Revoked | `Cancel ])
           , (!calls : int)
           , (outcomes : string list)
           , (!rejected : int)]));
  [%expect
    {|
    (Success 3 (complete complete) 13)
    (Script 3 (complete complete complete) 15)
    (Denied 1 (invocation.permission_denied) 9)
    (Revoked 1 (invocation.stale_binding) 9)
    (Cancel 2 (cancelled) 8)
    |}]
;;
