open Core
open Fixtures
open Job_fixtures
module Owner = Agent_server.Runtime_owner
module Builder = Agent_session.Runtime_builder

let runtime ?script_tools ~close () : Builder.t =
  { worker =
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ _ ->
        failwith "unexpected model operation")
  ; parse_user_content = (fun ~id:_ _ -> failwith "unexpected input")
  ; initial_history = []
  ; initial_prompt_entry_count = 0
  ; reserved_history_through = 0
  ; moderator_snapshot = None
  ; moderator_manager = None
  ; moderator_tools = []
  ; moderator_script_tools = script_tools
  ; background_executor = None
  ; moderator_activation = None
  ; start_moderator = (fun () -> Ok None)
  ; enqueue_internal_event = (fun ?prepare:_ _ -> failwith "unexpected event")
  ; drain_internal_events = (fun _ -> failwith "unexpected event drain")
  ; execute_model_job = (fun ~recipe:_ ~payload:_ -> failwith "unexpected model job")
  ; enqueue_model_job_completion = (fun ?prepare:_ _ -> failwith "unexpected completion")
  ; close
  }
;;

let%expect_test
    "independent native jobs retain one runtime without serializing each other"
  =
  with_actor (fun env sw actor _writer backend ->
    let first_entered, first_entered_u = Eio.Promise.create () in
    let second_entered, second_entered_u = Eio.Promise.create () in
    let first_done, first_done_u = Eio.Promise.create () in
    let calls = ref 0
    and running = ref 0
    and maximum = ref 0 in
    let registry =
      native_registry calls ~raises:false ~on_call:(fun () ->
        incr running;
        maximum := Int.max !maximum !running;
        Exn.protect
          ~finally:(fun () -> decr running)
          ~f:(fun () ->
            match !calls with
            | 1 ->
              Eio.Promise.resolve first_entered_u ();
              Eio.Promise.await second_entered
            | _ -> Eio.Promise.resolve second_entered_u ()))
    in
    let script_tools =
      Agent_session.Script_tool_calls.create
        ~registry:(fun () -> registry)
        ~moderator_names:String.Set.empty
        ~now:(fun () -> timestamp)
        ~is_halted:(fun () -> false)
        ~requires_active_moderator:(fun _ -> false)
        ~authorize:(fun _ _ -> Ok ())
        ~prepare_output:(fun _ -> Ok (`String "disclosed"))
        ~defer_observation:(fun _ -> Ok ())
    in
    let builds = ref 0
    and closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        incr builds;
        Ok
          (runtime
             ~script_tools
             ~close:(fun () ->
               [%test_eq: int] 0 !running;
               incr closes)
             ()))
    in
    let request =
      Chat_response.Background_request.tool
        ~capabilities:registry
        ~reference:(List.hd_exn (C.references registry))
        ~input:(`Object [])
        ~policy:Chat_response.One_off_request.default_policy
      |> protocol_ok
    in
    let run_job () =
      let job =
        add_claimed_job ~payload:(Chat_response.Background_request.to_json request) actor
      in
      Owner.with_background_runtime owner (fun runtime ->
        with_job actor job (fun ~job ~execute ->
          Agent_session.Background_execution.run
            ~env
            ~job
            ~deadline
            ~execute
            ~request
            ~policy:Chat_response.One_off_request.default_policy
            ~script_tools:(Option.value_exn runtime.moderator_script_tools)
            ~now:(fun () -> timestamp)
            ~moderate_tool:(fun _ _ -> Ok None)
            ~prepare_outcome:(fun _ -> Ok ())
            ()))
      |> protocol_ok
      |> ignore
    in
    Eio.Fiber.fork ~sw (fun () ->
      run_job ();
      Eio.Promise.resolve first_done_u ());
    Eio.Promise.await first_entered;
    let administrative_calls = ref 0 in
    reject "unload while owned" (Owner.unload owner);
    reject
      "administration while owned"
      (Owner.with_administration owner (fun () ->
         incr administrative_calls;
         Ok ()));
    [%test_eq: int] 0 !administrative_calls;
    [%test_eq: int] 0 !closes;
    run_job ();
    Eio.Promise.await first_done;
    Owner.unload owner |> protocol_ok;
    Owner.with_background_runtime owner (fun _ -> Ok ()) |> protocol_ok;
    Owner.close owner;
    let state = Agent_session.Memory_backend.state backend in
    print_s
      [%sexp
        { builds = (!builds : int)
        ; closes = (!closes : int)
        ; concurrent = (!maximum : int)
        ; invocations = (List.length state.invocations : int)
        ; history = (List.length state.conversation.canonical_history : int)
        }]);
  [%expect
    {|
    ("unload while owned" Conflict)
    ("administration while owned" Conflict)
    ((builds 2) (closes 2) (concurrent 2) (invocations 4) (history 0))
    |}]
;;

let%expect_test "close cancels callbacks and retires only after their cleanup finishes" =
  with_actor (fun _env sw actor _writer _backend ->
    let entered, entered_u = Eio.Promise.create () in
    let cleaning, cleaning_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let done_, done_u = Eio.Promise.create () in
    let joined, joined_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Ok (runtime ~close:(fun () -> incr closes) ()))
    in
    Eio.Fiber.fork ~sw (fun () ->
      let cancelled =
        try
          Owner.with_background_runtime owner (fun _ ->
            Exn.protect
              ~f:(fun () ->
                Eio.Promise.resolve entered_u ();
                Eio.Promise.await never;
                Ok ())
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                  Eio.Promise.resolve cleaning_u ();
                  Eio.Promise.await release)))
          |> protocol_ok;
          false
        with
        | Eio.Cancel.Cancelled _ -> true
      in
      Eio.Promise.resolve done_u cancelled);
    Eio.Promise.await entered;
    Eio.Fiber.fork ~sw (fun () ->
      Owner.close_and_wait owner;
      Eio.Promise.resolve joined_u ());
    Eio.Promise.await cleaning;
    assert (Option.is_none (Eio.Promise.peek joined));
    [%test_eq: int] 0 !closes;
    [%test_eq: bool] true (Owner.is_loaded owner);
    reject "reload after close" (Owner.ensure_loaded owner);
    reject
      "new callback after close"
      (Owner.with_background_runtime owner (fun _ ->
         failwith "closed owner admitted work"));
    Eio.Promise.resolve release_u ();
    [%test_eq: bool] true (Eio.Promise.await done_);
    Eio.Promise.await joined;
    Owner.close owner;
    print_s [%sexp (!closes : int), (Owner.is_loaded owner : bool)]);
  [%expect
    {|
    ("reload after close" Server_shutting_down)
    ("new callback after close" Server_shutting_down)
    (1 false)
    |}]
;;

let%expect_test
    "callback errors release ownership and self-close cleanup cannot poison the mutex"
  =
  with_actor (fun _env _sw actor _writer _backend ->
    let closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Ok
          (runtime
             ~close:(fun () ->
               incr closes;
               failwith "fixture close failure")
             ()))
    in
    let callback_failed =
      try
        Owner.with_background_runtime owner (fun _ -> failwith "fixture callback failure")
        |> ignore;
        false
      with
      | Failure message -> String.equal message "fixture callback failure"
    in
    [%test_eq: bool] true callback_failed;
    let reused = ref false in
    Owner.with_background_runtime owner (fun _ ->
      reused := true;
      Ok ())
    |> protocol_ok;
    let close_failed =
      Result.try_with (fun () ->
        Owner.with_background_runtime owner (fun _ ->
          Owner.close owner;
          Ok ()))
      |> Result.is_error
    in
    [%test_eq: bool] true close_failed;
    reject "closed after cleanup error" (Owner.ensure_loaded owner);
    Owner.close owner;
    print_s [%sexp (!reused : bool), (!closes : int), (Owner.is_loaded owner : bool)]);
  [%expect
    {|
    ("closed after cleanup error" Server_shutting_down)
    (true 1 false)
    |}]
;;

let%expect_test
    "cancelled callback releases its lease even when cleanup waits for the owner mutex"
  =
  with_actor (fun _env sw actor _writer _backend ->
    let entered, entered_u = Eio.Promise.create () in
    let leaving, leaving_u = Eio.Promise.create () in
    let finished, finished_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Ok (runtime ~close:(fun () -> incr closes) ()))
    in
    Eio.Fiber.fork ~sw (fun () ->
      let was_cancelled =
        try
          Eio.Cancel.sub (fun context ->
            Owner.with_background_runtime owner (fun _ ->
              Eio.Promise.resolve entered_u (fun () -> Eio.Cancel.cancel context Exit);
              Exn.protect
                ~finally:(fun () -> Eio.Promise.resolve leaving_u ())
                ~f:(fun () ->
                  Eio.Promise.await never;
                  Ok ())))
          |> protocol_ok;
          false
        with
        | Eio.Cancel.Cancelled _ -> true
      in
      Eio.Promise.resolve finished_u was_cancelled);
    let cancel = Eio.Promise.await entered in
    Owner.For_testing.with_loaded_runtime owner (fun () ->
      cancel ();
      Eio.Promise.await leaving;
      Eio.Fiber.yield ();
      assert (Option.is_none (Eio.Promise.peek finished));
      Ok ())
    |> protocol_ok;
    [%test_eq: bool] true (Eio.Promise.await finished);
    Owner.close_and_wait owner;
    print_s [%sexp (!closes : int), (Owner.is_loaded owner : bool)]);
  [%expect {| (1 false) |}]
;;
