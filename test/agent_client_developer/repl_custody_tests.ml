open! Core
module P = Agent_protocol
module C = P.Developer_common
module R = P.Developer_repl_wire
module T = Developer_connection_tests
module RT = Repl_transport_tests
module Client = Agent_client.Developer_repl_connection

let limits = RT.limits

let capabilities =
  R.Capabilities.create ~methods:R.Request.supported_methods ~limits |> T.must
;;

let instance = R.Wire_id.Instance.of_string "ri_custody" |> T.must
let epoch = R.Wire_id.Epoch.of_string "re_custody" |> T.must
let revision = R.Wire_id.Revision.of_string "rv_custody" |> T.must
let run_id = P.Id.Developer_run.create_with T.generator
let key name = P.Idempotency_key.of_string name |> T.must

let view =
  R.View.create
    ~limits
    ~instance
    ~epoch
    ~revision
    ~run_id
    ~capture_fingerprint:(Chatml_source.Digest.of_contents "captured" ~checkpoint:Fn.id)
  |> T.must
;;

let requests () =
  let route =
    C.Route.local_source
      ~limits:(R.Limits.request limits)
      ~root_index:0
      ~member:(Source.Portable_path.create "source.chatmd" |> Or_error.ok_exn)
    |> T.must
  in
  let create =
    R.Create.create
      ~limits
      ~run_id
      ~key:(key "create-custody")
      ~route
      ~target:One_off_v1
      ~prompt:(P.Id.Prompt_definition.create_with T.generator)
      ~workspace:(P.Id.Workspace_definition.create_with T.generator)
      ~requested_execution:None
      ~requested_max_output_bytes:None
    |> T.must
  in
  let submit =
    R.Submit.create
      ~limits
      ~instance
      ~epoch
      ~expected_revision:revision
      ~key:(key "submit-custody")
      ~source:"Task.pure(8)"
    |> T.must
  in
  let task =
    R.Task_submit.create
      ~limits
      ~key:(key "task-custody")
      ~reference:"opaque-reference"
      ~requested_execution:None
      ~requested_max_output_bytes:None
      ~fresh_epoch:(Some (instance, epoch))
    |> T.must
  in
  ( create
  , submit
  , task
  , R.Current_epoch.create ~instance ~epoch ~key:(key "reset-custody")
  , R.Lookup.create ~instance ~key:(key "close-custody") )
;;

let connection ?(authentication_kind = "embedded") ~sends () =
  let initialized = T.initialization ~events:true ~repl:true ~authentication_kind () in
  let connection =
    Agent_client.In_memory.create_with_developer_repl
      ~request:(function
        | P.Command.Protocol_initialize _ -> Ok (RT.public_initialized initialized)
        | _ -> Error (P.Error.invalid_request "unexpected command"))
      ~request_developer:(fun ~bound:_ _ ->
        Error (P.Error.invalid_request "unexpected Developer request"))
      ~request_repl:(fun ~max_request_bytes:_ ~bound:_ request ->
        match request with
        | R.Request.Capabilities -> Ok (R.Capabilities.to_json capabilities)
        | Observe _ | Task_observe _ -> Ok (`Object [ "found", `False ])
        | _ ->
          Int.incr sends;
          Error (P.Error.invalid_request "reply deliberately lost"))
      ~notifications:(Eio.Stream.create 8)
      ~close:Fn.id
  in
  T.initialize ~events:true ~repl:true connection initialized;
  connection
;;

let%expect_test
    "five separate mutation custodians retain uncertainty and adopt atomically"
  =
  Eio_main.run (fun _ ->
    let sends = ref 0 in
    let physical = connection ~sends () in
    let client = Client.create physical |> T.must in
    let create, submit, task, reset, close = requests () in
    let wrong_instance = R.Wire_id.Instance.of_string "ri_other" |> T.must in
    let wrong_epoch = R.Wire_id.Epoch.of_string "re_other" |> T.must in
    List.iter
      [ wrong_instance, epoch; instance, wrong_epoch ]
      ~f:(fun fresh_epoch ->
        let mismatched =
          R.Task_submit.create
            ~limits
            ~key:(key "task-custody")
            ~reference:"opaque-reference"
            ~requested_execution:None
            ~requested_max_output_bytes:None
            ~fresh_epoch:(Some fresh_epoch)
          |> T.must
        in
        assert (Result.is_error (Client.task_submit client ~view mismatched)));
    assert (Int.equal !sends 0);
    assert (List.is_empty (Client.pending client).tasks);
    assert (Result.is_error (Client.create_instance client create));
    assert (Result.is_error (Client.submit client submit));
    assert (Result.is_error (Client.task_submit client ~view task));
    assert (Result.is_error (Client.reset_and_wait client reset));
    assert (Result.is_error (Client.close_and_wait client close));
    let pending = Client.pending client in
    assert (List.length pending.creates = 1);
    assert (List.length pending.submits = 1);
    assert (List.length pending.tasks = 1);
    assert (List.length pending.resets = 1);
    assert (List.length pending.closes = 1);
    assert (Int.equal !sends 5);
    let lease = Agent_client.Connection.claim_notifications physical |> T.must in
    assert (Result.is_error (Agent_client.Connection.claim_notifications physical));
    Agent_client.Connection.release_notifications lease;
    let no_fresh_epoch =
      R.Task_submit.create
        ~limits
        ~key:(key "task-without-epoch")
        ~reference:"opaque-reference"
        ~requested_execution:None
        ~requested_max_output_bytes:None
        ~fresh_epoch:None
      |> T.must
    in
    assert (Result.is_error (Client.task_submit client ~view no_fresh_epoch));
    assert (Int.equal !sends 5);
    assert (Result.is_error (Client.create_instance client create));
    assert (Int.equal !sends 5);
    (match
       Client.observe_pending_submit client (List.hd_exn pending.submits) |> T.must
     with
     | None -> ()
     | Some _ -> failwith "missing Submit was treated as present");
    (match Client.observe_pending_task client (List.hd_exn pending.tasks) |> T.must with
     | None -> ()
     | Some _ -> failwith "missing Task was treated as present");
    assert (List.length (Client.pending client).tasks = 1);
    Agent_client.Connection.close physical;
    let different_auth = connection ~authentication_kind:"token" ~sends () in
    let rejected = Client.create different_auth |> T.must in
    assert (Result.is_error (Client.adopt_pending rejected pending));
    Agent_client.Connection.close different_auth;
    let reconnected_physical = connection ~sends () in
    let reconnected = Client.create reconnected_physical |> T.must in
    let excessive =
      { pending with tasks = List.init 65 ~f:(fun _ -> List.hd_exn pending.tasks) }
    in
    assert (Result.is_error (Client.adopt_pending reconnected excessive));
    assert (List.is_empty (Client.pending reconnected).tasks);
    Client.adopt_pending reconnected pending |> T.must;
    Client.adopt_pending reconnected pending |> T.must;
    assert (List.length (Client.pending reconnected).tasks = 1);
    assert (Result.is_error (Client.retry_task reconnected (List.hd_exn pending.tasks)));
    assert (Int.equal !sends 6);
    assert (List.length (Client.pending reconnected).tasks = 1);
    Client.abandon_task reconnected (List.hd_exn pending.tasks) |> T.must;
    assert (List.is_empty (Client.pending reconnected).tasks);
    assert (Result.is_error (Client.abandon_task reconnected (List.hd_exn pending.tasks)));
    Agent_client.Connection.close reconnected_physical;
    print_endline
      "five exact pending intents retained; Observe None did not retire; auth mismatch \
       denied; identical retry explicit");
  [%expect
    {| five exact pending intents retained; Observe None did not retire; auth mismatch denied; identical retry explicit |}]
;;

let%expect_test "post-handoff malformed result and cancellation retain custody" =
  Eio_main.run (fun _ ->
    let initialized = T.initialization ~events:true ~repl:true () in
    let cancel = ref None in
    let malformed = ref true in
    let marker = Failure "cancel after REPL send" in
    let physical =
      Agent_client.In_memory.create_with_developer_repl
        ~request:(function
          | P.Command.Protocol_initialize _ -> Ok (RT.public_initialized initialized)
          | _ -> Error (P.Error.invalid_request "unexpected command"))
        ~request_developer:(fun ~bound:_ _ ->
          Error (P.Error.invalid_request "unexpected Developer request"))
        ~request_repl:(fun ~max_request_bytes:_ ~bound:_ request ->
          match request with
          | R.Request.Capabilities -> Ok (R.Capabilities.to_json capabilities)
          | Create _ when !malformed ->
            malformed := false;
            Ok (`Object [])
          | Submit _ ->
            Option.iter !cancel ~f:(fun context -> Eio.Cancel.cancel context marker);
            Eio.Fiber.yield ();
            assert false
          | _ -> Error (P.Error.invalid_request "unexpected REPL request"))
        ~notifications:(Eio.Stream.create 8)
        ~close:Fn.id
    in
    T.initialize ~events:true ~repl:true physical initialized;
    let client = Client.create physical |> T.must in
    let create, submit, _, _, _ = requests () in
    assert (Result.is_error (Client.create_instance client create));
    assert (List.length (Client.pending client).creates = 1);
    (match
       Eio.Cancel.sub (fun context ->
         cancel := Some context;
         Client.submit client submit)
     with
     | exception Eio.Cancel.Cancelled reason -> assert (phys_equal reason marker)
     | Ok _ | Error _ -> failwith "expected post-handoff cancellation");
    assert (List.length (Client.pending client).submits = 1);
    assert (List.length (Client.pending client).creates = 1);
    Agent_client.Connection.close physical;
    print_endline "malformed and cancelled post-handoff results retained separate intents");
  [%expect {| malformed and cancelled post-handoff results retained separate intents |}]
;;

let%expect_test "fixed local custody count rejects before transport handoff" =
  Eio_main.run (fun _ ->
    let sends = ref 0 in
    let physical = connection ~sends () in
    let client = Client.create physical |> T.must in
    for index = 1 to 64 do
      let lookup =
        R.Lookup.create ~instance ~key:(key (sprintf "close-capacity-%d" index))
      in
      assert (Result.is_error (Client.close_and_wait client lookup))
    done;
    assert (Int.equal !sends 64);
    let extra = R.Lookup.create ~instance ~key:(key "close-capacity-extra") in
    assert (Result.is_error (Client.close_and_wait client extra));
    assert (Int.equal !sends 64);
    assert (List.length (Client.pending client).closes = 64);
    Agent_client.Connection.close physical;
    print_endline "64 retained requests; 65th rejected before send");
  [%expect {| 64 retained requests; 65th rejected before send |}]
;;
