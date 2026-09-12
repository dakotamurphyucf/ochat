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
  Mirage_crypto_rng_unix.use_default ();
  let callback = ref (fun () -> ()) in
  let registry =
    native_registry (ref 0) ~raises:false ~on_call:(fun () -> !callback ())
  in
  let saved = ref None in
  let saved_authoring = ref None in
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
    ~make_worker:(fun env actor_ready ->
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
                  ~authoring:None
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
              List.iter
                [ "create"; "send"; "stop"; "wait"; "reference"; "validate" ]
                ~f:(fun operation ->
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
              |> reject "agent.read.invalid_request";
              let module V = Chat_response.Authoring_validation in
              let host =
                V.create_host
                  ~runtime_identity:"adapter-authoring-fixture"
                  ~targets:[ One_off_script ]
                  ~moderator_surface:Ordinary
                  ~compilation:Chatml_compilation.default_limits
                |> Result.ok_or_failwith
              in
              let authoring = Agent_session.Authoring_services.create ~env ~host in
              let readonly =
                B.create
                  ~borrowed
                  ~allowed:[ Reference; Validate ]
                  ~creation:None
                  ~sessions:None
                  ~authoring:(Some authoring)
              in
              saved_authoring := Some readonly;
              let complete = function
                | P.Invocation.Complete value -> value
                | value -> raise_s [%sexp (value : P.Invocation.outcome)]
              in
              let query =
                `Object
                  [ "version", `Number "1"
                  ; "operation", `String "topic"
                  ; "task", `String "one_off_script"
                  ; "topic_id", `String "reference.tools"
                  ; "query", `Null
                  ; "features", `Null
                  ; "cursor", `Null
                  ; "max_tokens", `Null
                  ]
              in
              let reference =
                B.dispatch readonly (envelope "reference" query) |> complete
              in
              [%test_eq: string]
                (C.fingerprint registry)
                (Jsonaf.member_exn "capability_fingerprint" reference |> Jsonaf.string_exn);
              let names =
                Jsonaf.member_exn "items" reference
                |> Jsonaf.list_exn
                |> List.map ~f:(fun item ->
                  Jsonaf.member_exn "name" item |> Jsonaf.string_exn)
              in
              [%test_eq: string list]
                (C.references registry |> List.map ~f:(fun reference -> reference.name))
                names;
              let validation =
                `Object
                  [ "version", `Number "1"
                  ; "target", `String "one_off_script"
                  ; ( "source"
                    , `String
                        "let never = fail(\"validation must not execute\")\n\
                         let main input = Task.pure(input)" )
                  ; "tools", `Array []
                  ]
              in
              let report =
                B.dispatch readonly (envelope "validate" validation) |> complete
              in
              assert (Jsonaf.exactly_equal (Jsonaf.member_exn "valid" report) `True);
              List.iter
                [ "create"; "send"; "stop"; "read"; "status"; "wait" ]
                ~f:(fun operation ->
                  B.dispatch readonly (envelope operation arguments)
                  |> reject "agent.management.denied");
              let unavailable =
                B.create
                  ~borrowed
                  ~allowed:[ Reference; Validate ]
                  ~creation:None
                  ~sessions:None
                  ~authoring:None
              in
              B.dispatch unavailable (envelope "reference" query)
              |> reject "authoring.unavailable");
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
        B.dispatch (Option.value_exn !saved_authoring) (envelope "reference" `Null)
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
       Option.iter (A.state actor |> protocol_ok).failure ~f:(fun error ->
         raise_s [%sexp (error : P.Error.t)]);
       assert !finished);
  print_endline
    "actual actor borrow retained; readonly grant enforced; forged envelope rejected; \
     expired adapter cannot call services; readonly authoring uses actual scoped tools \
     and never evaluates source";
  [%expect
    {| actual actor borrow retained; readonly grant enforced; forged envelope rejected; expired adapter cannot call services; readonly authoring uses actual scoped tools and never evaluates source |}]
;;
