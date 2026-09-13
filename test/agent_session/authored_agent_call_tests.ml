open Core
open Fixtures
module P = Agent_protocol
module N = Agent_session.Native_tool_invocation
module W = Agent_session.Authored_agent_call
module A = Agent_session.Session_actor

let same_json expected actual = assert (Jsonaf.exactly_equal expected actual)

let%expect_test "authored call composition preserves retry identity and scoped results" =
  let callback = ref (fun () -> ()) in
  let registry =
    native_registry (ref 0) ~raises:false ~on_call:(fun () -> !callback ())
  in
  let creations = String.Table.create () in
  let sends = String.Table.create () in
  let calls = Queue.create () in
  let saved = ref None in
  let finished = ref false in
  let deny () =
    P.Invocation.
      { code = "fixture.denied"; message = "Denied"; retryable = false; details = `Null }
  in
  let ok = function
    | Ok value -> value
    | Error error -> failwith error.P.Invocation.code
  in
  let rejected code = function
    | Error error -> [%test_eq: string] code error.P.Invocation.code
    | Ok _ -> failwith "unexpected successful wrapper result"
  in
  let input = `Object [ "input", `String "question" ] in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
        let actor = Eio.Promise.await actor_ready in
        let invoke scenario =
          (callback := fun () -> scenario (N.borrow () |> protocol_ok));
          let call, invocation = publication_call caps () in
          let reference, invocation = native_context registry invocation in
          caps.commit_invocation_call ~invocation call |> protocol_ok;
          let resolved =
            N.run_scoped
              ~execute:caps.with_invocation
              ~registry:(fun () -> registry)
              ~reference
              ~invocation
              ~is_halted:(fun () -> false)
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "disclosed"))
            |> protocol_ok
          in
          caps.publish_invocation_output
            ~invocation_id:resolved.context.id
            (publication_output caps ~text:{|{"type":"complete","value":"disclosed"}|} ())
          |> protocol_ok
        in
        let previous_child = ref None in
        List.iter [ true; false ] ~f:(fun first ->
          invoke (fun borrowed ->
            let owner = N.borrowed_invocation borrowed in
            let revoked = ref false in
            let revoke_after_wait = ref false in
            let revoke_after_read = ref false in
            let corrupt_receipt = ref false in
            let status = ref "assigned" in
            let check actual =
              assert (P.Invocation.equal owner (N.borrowed_invocation actual));
              N.borrowed_capabilities actual |> protocol_ok |> ignore
            in
            let host : W.host =
              { create =
                  (fun actual ~key ->
                    check actual;
                    Queue.enqueue calls "create";
                    let id =
                      Hashtbl.find_or_add
                        creations
                        (P.Idempotency_key.to_string key)
                        ~default:P.Id.Session.create
                    in
                    Ok id)
              ; validate =
                  (fun actual id ->
                    check actual;
                    match
                      ( !revoked
                      , List.mem (Hashtbl.data creations) id ~equal:P.Id.Session.equal )
                    with
                    | false, true -> Ok ()
                    | _ -> Error (deny ()))
              ; one_off =
                  (fun actual ~input ->
                    check actual;
                    Queue.enqueue calls "one_off";
                    Ok (`String ("one-off: " ^ input)))
              }
            in
            let receipt id =
              `Object
                [ "session_id", P.Id.Session.to_json id
                ; "receipt_id", P.History.Id.to_json history_id
                ; "status", `String !status
                ]
            in
            let sessions : Agent_session.Managed_session_service.t =
              { status = (fun _ _ -> failwith "wrapper must not poll status")
              ; stop = (fun _ _ ~key:_ ~mode:_ -> failwith "wrapper must not stop child")
              ; send =
                  (fun actual id ~key ~message ->
                    check actual;
                    Queue.enqueue calls "send";
                    let old =
                      Hashtbl.find_or_add
                        sends
                        (P.Idempotency_key.to_string key)
                        ~default:(fun () -> id, message)
                    in
                    match old with
                    | old_id, old_message
                      when P.Id.Session.equal old_id id
                           && String.equal old_message message ->
                      Ok (receipt (if !corrupt_receipt then session_id else id))
                    | _ -> Error { (deny ()) with code = "fixture.conflict" })
              ; wait =
                  (fun actual id ~target ~timeout_ms ->
                    check actual;
                    [%test_eq: int] 0 timeout_ms;
                    (match target with
                     | Receipt id -> assert (P.History.Id.equal id history_id)
                     | Output _ -> failwith "must wait for submission termination");
                    Queue.enqueue calls "wait";
                    revoked := !revoke_after_wait;
                    Ok (`Object [ "reason", `String "timeout"; "receipt", receipt id ]))
              ; read =
                  (fun actual id ~receipt_id ~cursor ~limit ->
                    check actual;
                    assert (Option.equal P.History.Id.equal receipt_id (Some history_id));
                    assert (Option.is_none cursor);
                    [%test_eq: int] 16 limit;
                    Queue.enqueue calls "read";
                    revoked := !revoke_after_read;
                    Ok
                      (`Object
                          [ "session_id", P.Id.Session.to_json id
                          ; "receipt", receipt id
                          ; "items", `Array [ `String "disclosed answer fragment" ]
                          ; "next_cursor", `String "opaque-continuation"
                          ; "caught_up", `False
                          ]))
              }
            in
            let run ?(policy = Prompt.Chat_markdown.Persistent) json =
              W.run ~wait_timeout_ms:0 ~host ~sessions ~borrowed ~policy json
            in
            saved := Some (fun () -> run input);
            let pending = run input |> ok in
            same_json (`String "pending") (Jsonaf.member_exn "status" pending);
            let child =
              Jsonaf.member_exn "session_id" pending
              |> P.Id.Session.of_json
              |> protocol_ok
            in
            (match !previous_child with
             | None -> previous_child := Some child
             | Some previous -> assert (not (P.Id.Session.equal previous child)));
            let retry = run input |> ok in
            same_json pending retry;
            [%test_eq: int] (if first then 1 else 2) (Hashtbl.length creations);
            [%test_eq: int] (if first then 1 else 2) (Hashtbl.length sends);
            let continued =
              `Object
                [ "input", `String "question"; "session_id", P.Id.Session.to_json child ]
            in
            Queue.clear calls;
            status := "completed";
            let result = run continued |> ok in
            [%test_eq: string list] [ "send"; "wait"; "read" ] (Queue.to_list calls);
            same_json (`String "completed") (Jsonaf.member_exn "status" result);
            same_json
              (`String "opaque-continuation")
              (Jsonaf.member_exn "output" result |> Jsonaf.member_exn "next_cursor");
            List.iter
              [ "failed"; "cancelled"; "interrupted"; "invalidated" ]
              ~f:(fun terminal ->
                status := terminal;
                same_json
                  (`String terminal)
                  (run continued |> ok |> Jsonaf.member_exn "status"));
            Queue.clear calls;
            run (`Object [ "input", `String "changed" ]) |> rejected "fixture.conflict";
            [%test_eq: string list] [ "create"; "send" ] (Queue.to_list calls);
            [%test_eq: int] (if first then 1 else 2) (Hashtbl.length creations);
            Queue.clear calls;
            run
              (`Object
                  [ "input", `String "question"
                  ; "session_id", P.Id.Session.to_json session_id
                  ])
            |> rejected "fixture.denied";
            run ~policy:Optional continued |> rejected "agent.authored.invalid_request";
            assert (Queue.is_empty calls);
            same_json (`String "one-off: question") (run ~policy:Optional input |> ok);
            [%test_eq: string list] [ "one_off" ] (Queue.to_list calls);
            Queue.clear calls;
            corrupt_receipt := true;
            run continued |> rejected "agent.authored.service_contract";
            [%test_eq: string list] [ "send" ] (Queue.to_list calls);
            corrupt_receipt := false;
            Queue.clear calls;
            revoke_after_wait := true;
            run continued |> rejected "fixture.denied";
            [%test_eq: string list] [ "send"; "wait" ] (Queue.to_list calls);
            revoked := false;
            revoke_after_wait := false;
            revoke_after_read := true;
            Queue.clear calls;
            run continued |> rejected "fixture.denied";
            [%test_eq: string list] [ "send"; "wait"; "read" ] (Queue.to_list calls)));
        Queue.clear calls;
        (Option.value_exn !saved) () |> rejected "agent.authored.denied";
        assert (Queue.is_empty calls);
        finished := true;
        Completed
          { final_history =
              (A.state actor |> protocol_ok).conversation.canonical_history
              |> Agent_session.History_codec.all_of_protocol
              |> protocol_ok
          ; moderator_snapshot = None
          ; runtime_requests = []
          }))
    (fun _env actor _writer _backend ->
       let rec await () =
         match (A.state actor |> protocol_ok).active_operation with
         | None -> ()
         | Some _ ->
           Eio.Fiber.yield ();
           await ()
       in
       await ();
       assert !finished);
  print_endline
    "same invocation reuses creation/send keys; new invocation creates separately";
  print_endline
    "pending and terminal receipts retain bounded output and cursor; continuation skips \
     create";
  print_endline
    "foreign targets, changed retries and mismatched receipts reject before further \
     effects";
  print_endline
    "optional one-off is isolated; revocation after wait/read and expired scope prevent \
     disclosure";
  [%expect
    {|
    same invocation reuses creation/send keys; new invocation creates separately
    pending and terminal receipts retain bounded output and cursor; continuation skips create
    foreign targets, changed retries and mismatched receipts reject before further effects
    optional one-off is isolated; revocation after wait/read and expired scope prevent disclosure
    |}]
;;
