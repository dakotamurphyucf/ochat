open! Core
open Agent_server_test_support
module P = Agent_protocol
module Registry = Agent_server.Session_registry

let indexed name =
  let timestamp = P.Timestamp.of_time_ns Time_ns.epoch in
  let session : P.Session.t =
    { id = P.Id.Session.of_string name |> protocol_ok
    ; creator = Some (principal ()).id
    ; created_at = timestamp
    ; updated_at = timestamp
    ; generation = 0
    ; spec = session_spec ()
    ; desired_state = Stopped
    ; observed_state = Stopped
    ; prompt_revision = None
    ; workspace_instance = None
    ; active_operation = None
    ; revision = 0L
    ; metadata_revision = 0L
    ; organization = P.Session_organization.Values.empty
    ; latest_event_sequence = 0L
    ; inference_summary = History_entry.Payload.Presence.Absent
    }
  in
  Agent_store.Session_index.Entry.
    { session
    ; runnable_job_count = 0
    ; deliverable_job_count = 0
    ; earliest_schedule_due = None
    ; owner_grace_deadline = None
    ; pending_initial_start = false
    ; archived = false
    ; lifecycle_revision = P.Session_lifecycle.Revision.zero
    ; admission = Automatic
    }
;;

let%expect_test
    "session reservation releases global lock and shutdown joins it before closing graph"
  =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let registry = Registry.create () in
      let first = indexed "ses_lifecycle_first" in
      let second = indexed "ses_lifecycle_second" in
      Registry.index_all registry [ first; second ];
      let entered, entered_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let finished, finished_u = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        let result =
          Registry.with_lifecycle registry first.session.id (fun _ ->
            Eio.Promise.resolve entered_u ();
            Eio.Promise.await release;
            Ok ())
        in
        Eio.Promise.resolve finished_u result);
      Eio.Promise.await entered;
      let same_id_conflict =
        match Registry.load registry first.session.id with
        | Error { P.Error.code = Conflict; _ } -> true
        | Error failure -> raise_s [%sexp (failure : P.Error.t)]
        | Ok _ -> false
      in
      let different_id =
        Registry.with_lifecycle registry second.session.id (fun reservation ->
          match Registry.Lifecycle_reservation.target reservation with
          | Indexed actual -> Ok (P.Id.Session.equal actual.session.id second.session.id)
          | Loaded _ | Absent -> Ok false)
        |> protocol_ok
      in
      let shutdown_done, shutdown_done_u = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Registry.shutdown registry;
        Eio.Promise.resolve shutdown_done_u ());
      let rec await_closing () =
        if Registry.is_closing registry
        then ()
        else (
          Eio.Fiber.yield ();
          await_closing ())
      in
      await_closing ();
      let still_waiting = Option.is_none (Eio.Promise.peek shutdown_done) in
      let admission_rejected =
        match Registry.with_lifecycle registry second.session.id (fun _ -> Ok ()) with
        | Error { P.Error.code = Server_shutting_down; _ } -> true
        | Error failure -> raise_s [%sexp (failure : P.Error.t)]
        | Ok () -> false
      in
      Eio.Promise.resolve release_u ();
      Eio.Promise.await finished |> protocol_ok;
      Eio.Promise.await shutdown_done;
      print_s
        [%sexp
          (( same_id_conflict
           , different_id
           , still_waiting
           , admission_rejected
           , not (Registry.lifecycle_reserved registry first.session.id) )
           : bool * bool * bool * bool * bool)]));
  [%expect {| (true true true true true) |}]
;;

let%expect_test "cancellation releases only its reservation and preserves indexed target" =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let registry = Registry.create () in
      let entry = indexed "ses_lifecycle_cancel" in
      Registry.index registry entry;
      let entered, entered_u = Eio.Promise.create () in
      let cancelled, cancelled_u = Eio.Promise.create () in
      let finished, finished_u = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        (try
           Eio.Switch.run (fun child ->
             Eio.Promise.resolve cancelled_u (fun () -> Eio.Switch.fail child Exit);
             try
               ignore
                 (Registry.with_lifecycle registry entry.session.id (fun _ ->
                    Eio.Promise.resolve entered_u ();
                    Eio.Fiber.await_cancel ())
                  : (unit, P.Error.t) Result.t)
             with
             | Eio.Cancel.Cancelled _ -> ())
         with
         | Exit -> ());
        Eio.Promise.resolve finished_u ());
      Eio.Promise.await entered;
      let cancel = Eio.Promise.await cancelled in
      cancel ();
      Eio.Promise.await finished;
      let retained =
        Registry.with_lifecycle registry entry.session.id (fun reservation ->
          match Registry.Lifecycle_reservation.target reservation with
          | Indexed original -> Ok (Agent_store.Session_index.Entry.equal original entry)
          | Loaded _ | Absent -> Ok false)
        |> protocol_ok
      in
      print_s
        [%sexp
          ((retained, not (Registry.lifecycle_reserved registry entry.session.id))
           : bool * bool)]));
  [%expect {| (true true) |}]
;;

let%expect_test
    "nested ancestor loader and unrelated reservation do not hold global mutex"
  =
  Eio_main.run (fun _env ->
    let registry = Registry.create () in
    let child = indexed "ses_nested_child" in
    let parent = indexed "ses_nested_parent" in
    let unrelated = indexed "ses_nested_unrelated" in
    Registry.index_all registry [ child; parent; unrelated ];
    let ancestor_reached = ref false in
    let unrelated_progress = ref false in
    let expected =
      P.Error.create
        Invalid_state
        ~message:"injected loader rejection"
        ~retryable:false
        ()
    in
    Registry.install_loader registry (fun entry ->
      if P.Id.Session.equal entry.session.id parent.session.id
      then (
        ancestor_reached := true;
        unrelated_progress
        := Result.is_ok
             (Registry.with_lifecycle registry unrelated.session.id (fun _ -> Ok ()));
        Error expected)
      else Registry.load registry parent.session.id);
    let original_error =
      match Registry.load registry child.session.id with
      | Error failure -> String.equal failure.message expected.message
      | Ok _ -> false
    in
    print_s
      [%sexp
        (( !ancestor_reached
         , !unrelated_progress
         , original_error
         , not (Registry.lifecycle_reserved registry child.session.id)
         , not (Registry.lifecycle_reserved registry parent.session.id) )
         : bool * bool * bool * bool * bool)]);
  [%expect {| (true true true true true) |}]
;;

let%expect_test "shutdown drains an admitted blocked reader while unrelated IDs progress" =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let registry = Registry.create () in
      let first = indexed "ses_blocked_read" in
      let other = indexed "ses_read_other" in
      Registry.index_all registry [ first; other ];
      let entered, enter = Eio.Promise.create () in
      let release, resume = Eio.Promise.create () in
      let finished, finish = Eio.Promise.create () in
      let expected =
        P.Error.create
          Persistence_error
          ~message:"original reader failure"
          ~retryable:false
          ()
      in
      Registry.install_reader registry (fun _ ->
        Eio.Promise.resolve enter ();
        Eio.Promise.await release;
        Error expected);
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.resolve
          finish
          (Registry.read_state registry ~authorize:(fun _ -> Ok ()) first.session.id));
      Eio.Promise.await entered;
      let progress =
        Result.is_ok (Registry.with_lifecycle registry other.session.id (fun _ -> Ok ()))
      in
      let closed, close = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Registry.shutdown registry;
        Eio.Promise.resolve close ());
      let rec await_closing () =
        if Registry.is_closing registry
        then ()
        else (
          Eio.Fiber.yield ();
          await_closing ())
      in
      await_closing ();
      let draining = Option.is_none (Eio.Promise.peek closed) in
      Eio.Promise.resolve resume ();
      let primary_preserved =
        match Eio.Promise.await finished with
        | Error failure -> String.equal failure.message expected.message
        | Ok _ -> false
      in
      Eio.Promise.await closed;
      print_s [%sexp ((progress, draining, primary_preserved) : bool * bool * bool)]));
  [%expect {| (true true true) |}]
;;

let%expect_test "cancelled retained read unregisters its owned lifetime before shutdown" =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let registry = Registry.create () in
      let entry = indexed "ses_cancelled_read" in
      Registry.index registry entry;
      let entered, enter = Eio.Promise.create () in
      let cancel, cancel_u = Eio.Promise.create () in
      let done_read, done_u = Eio.Promise.create () in
      Registry.install_reader registry (fun _ ->
        Eio.Promise.resolve enter ();
        Eio.Fiber.await_cancel ());
      Eio.Fiber.fork ~sw (fun () ->
        (try
           Eio.Switch.run (fun child ->
             Eio.Promise.resolve cancel_u (fun () -> Eio.Switch.fail child Exit);
             try
               ignore
                 (Registry.read_state
                    registry
                    ~authorize:(fun _ -> Ok ())
                    entry.session.id
                  : (Agent_session.Session_state.t, P.Error.t) Result.t)
             with
             | Eio.Cancel.Cancelled _ -> ())
         with
         | Exit -> ());
        Eio.Promise.resolve done_u ());
      Eio.Promise.await entered;
      (Eio.Promise.await cancel) ();
      Eio.Promise.await done_read;
      let original =
        Registry.with_lifecycle registry entry.session.id (fun reservation ->
          match Registry.Lifecycle_reservation.target reservation with
          | Indexed actual -> Ok (Agent_store.Session_index.Entry.equal actual entry)
          | Loaded _ | Absent -> Ok false)
        |> protocol_ok
      in
      Registry.shutdown registry;
      print_s [%sexp ((original, Registry.is_closing registry) : bool * bool)]));
  [%expect {| (true true) |}]
;;

let%expect_test "shutdown waits for blocked loader reservation without blocking other IDs"
  =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let registry = Registry.create () in
      let first = indexed "ses_blocked_loader" in
      let other = indexed "ses_loader_other" in
      Registry.index_all registry [ first; other ];
      let entered, enter = Eio.Promise.create () in
      let release, resume = Eio.Promise.create () in
      let finished, finish = Eio.Promise.create () in
      let expected =
        P.Error.create
          Persistence_error
          ~message:"original loader failure"
          ~retryable:false
          ()
      in
      Registry.install_loader registry (fun _ ->
        Eio.Promise.resolve enter ();
        Eio.Promise.await release;
        Error expected);
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.resolve finish (Registry.load registry first.session.id));
      Eio.Promise.await entered;
      let progress =
        Result.is_ok (Registry.with_lifecycle registry other.session.id (fun _ -> Ok ()))
      in
      let closed, close = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Registry.shutdown registry;
        Eio.Promise.resolve close ());
      let rec await_closing () =
        if Registry.is_closing registry
        then ()
        else (
          Eio.Fiber.yield ();
          await_closing ())
      in
      await_closing ();
      let draining = Option.is_none (Eio.Promise.peek closed) in
      Eio.Promise.resolve resume ();
      let original =
        match Eio.Promise.await finished with
        | Error failure -> String.equal failure.message expected.message
        | Ok _ -> false
      in
      Eio.Promise.await closed;
      print_s [%sexp ((progress, draining, original) : bool * bool * bool)]));
  [%expect {| (true true true) |}]
;;
