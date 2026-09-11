open Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module N = Agent_session.Native_tool_invocation
module C = Chat_response.Tool_capability
module B = Agent_session.Session_management

let%expect_test
    "management adapter retains the admitted caller, limits operations and expires"
  =
  let callback = ref (fun () -> ()) in
  let registry =
    native_registry (ref 0) ~raises:false ~on_call:(fun () -> !callback ())
  in
  let saved = ref None in
  let status_calls = ref 0 in
  let read_calls = ref 0 in
  let finished = ref false in
  let arguments = `Object [ "session_id", P.Id.Session.to_json second_session_id ] in
  let envelope operation arguments =
    `Object
      [ "version", `Number "1"; "operation", `String operation; "arguments", arguments ]
  in
  let reject code = function
    | P.Invocation.Fail error -> [%test_eq: string] code error.code
    | _ -> failwith "invalid adapter request reached a service"
  in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
        let actor = Eio.Promise.await actor_ready in
        (callback
         := fun () ->
              let borrowed = N.borrow () |> protocol_ok in
              let owner = N.borrowed_invocation borrowed in
              let check actual target =
                assert (P.Invocation.equal owner (N.borrowed_invocation actual));
                [%test_eq: string]
                  (C.fingerprint registry)
                  (N.borrowed_capabilities actual |> protocol_ok |> C.fingerprint);
                assert (P.Id.Session.equal target second_session_id)
              in
              let services : Agent_session.Managed_session_service.t =
                { status =
                    (fun actual target ->
                      check actual target;
                      Int.incr status_calls;
                      Ok (`String "status"))
                ; read =
                    (fun actual target ~receipt_id ~cursor ~limit ->
                      check actual target;
                      assert (Option.is_none receipt_id && Option.is_none cursor);
                      [%test_eq: int] 16 limit;
                      Int.incr read_calls;
                      Ok (`String "read"))
                ; send =
                    (fun _ _ ~key:_ ~message:_ -> failwith "readonly grant reached send")
                ; wait =
                    (fun _ _ ~target:_ ~timeout_ms:_ -> failwith "unselected wait ran")
                ; stop =
                    (fun _ _ ~key:_ ~mode:_ -> failwith "readonly grant reached stop")
                }
              in
              let adapter =
                B.create
                  ~borrowed
                  ~allowed:[ Status; Read ]
                  ~creation:None
                  ~sessions:(Some services)
              in
              saved := Some adapter;
              assert (
                P.Invocation.equal_outcome
                  (B.dispatch adapter (envelope "status" arguments))
                  (Complete (`String "status")));
              assert (
                P.Invocation.equal_outcome
                  (B.dispatch adapter (envelope "read" arguments))
                  (Complete (`String "read")));
              List.iter [ "create"; "send"; "stop"; "wait" ] ~f:(fun operation ->
                B.dispatch adapter (envelope operation arguments)
                |> reject "agent.management.denied");
              List.iter
                [ `Object
                    [ "version", `Number "2"
                    ; "operation", `String "status"
                    ; "arguments", arguments
                    ]
                ; `Object
                    [ "version", `Number "1"
                    ; "operation", `String "status"
                    ; "operation", `String "stop"
                    ; "arguments", arguments
                    ]
                ; `Object
                    [ "version", `Number "1"
                    ; "operation", `String "status"
                    ; "arguments", arguments
                    ; "caller_session_id", P.Id.Session.to_json session_id
                    ]
                ; envelope "delete" arguments
                ; `Object [ "version", `Number "1"; "operation", `String "status" ]
                ]
                ~f:(fun request ->
                  B.dispatch adapter request |> reject "agent.bridge.invalid_request");
              B.dispatch
                adapter
                (envelope
                   "read"
                   (`Object
                       [ "session_id", P.Id.Session.to_json second_session_id
                       ; "limit", `Number "0"
                       ]))
              |> reject "agent.read.invalid_request");
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
        |> protocol_ok;
        B.dispatch (Option.value_exn !saved) (envelope "status" arguments)
        |> reject "agent.management.denied";
        [%test_eq: int] 1 !status_calls;
        [%test_eq: int] 1 !read_calls;
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
    "actual actor borrow retained; readonly grant enforced; forged envelope rejected; \
     expired adapter cannot call services";
  [%expect
    {| actual actor borrow retained; readonly grant enforced; forged envelope rejected; expired adapter cannot call services |}]
;;
