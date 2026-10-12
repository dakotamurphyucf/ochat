open! Core
module P = Agent_protocol
module C = Agent_client
module V = C.Completion_view
module S = Support

let fields projection = P.Public.Snapshot.fields (C.Projection.snapshot projection)

let await view fixture ~on_view =
  V.await_terminal
    view
    ~handle:(S.handle fixture)
    ~operation:S.operation_id
    ~after:S.after
    ~on_view
;;

let assert_error (result : (_, P.Error.t) Result.t) code =
  match result with
  | Error failure -> assert (P.Error.equal_code failure.code code)
  | Ok _ -> failwith "expected a completion failure"
;;

let complete fixture =
  S.emit fixture ~sequence:1L (Operation_started (S.operation Running));
  S.emit fixture ~sequence:2L (Operation_completed (S.operation Completed))
;;

let%expect_test "actual reader presents an admitted running turn before its terminal" =
  let view = V.create S.session_id in
  S.with_handle ~on_update:(V.observe view) (fun fixture ->
    let running, saw_running = Eio.Promise.create () in
    let phases = ref [] in
    let delivered = ref false in
    let on_view projection =
      match (fields projection).session.active_operation with
      | Some _ ->
        phases := !phases @ [ "running" ];
        Eio.Promise.resolve saw_running ()
      | None ->
        phases
        := !phases
           @ [ (if Option.is_some (C.Projection.terminal_operation projection)
                then "completed"
                else "idle")
             ]
    in
    Eio.Fiber.both
      (fun () ->
         let terminal = await view fixture ~on_view |> S.ok in
         assert (Int64.equal (fields terminal).latest_event_sequence 2L);
         delivered := true)
      (fun () ->
         S.emit fixture ~sequence:1L (Operation_started (S.operation Running));
         Eio.Promise.await running;
         assert (not !delivered);
         S.emit fixture ~sequence:2L (Operation_completed (S.operation Completed)));
    assert (not (C.Session_handle.is_closed (S.handle fixture)));
    assert (Int.equal (S.detach_count fixture) 0);
    print_endline (String.concat !phases ~sep:" -> "));
  print_endline "completion borrows the reader; its owner still closes/detaches";
  [%expect
    {|
    idle -> running -> completed
    completion borrows the reader; its owner still closes/detaches
    |}]
;;

let%test_unit "reader failure and transport closure wake without on_update" =
  List.iter [ `Stream_error; `Transport_end; `Durable_gap ] ~f:(fun cause ->
    let view = V.create S.session_id in
    let updates = ref 0 in
    S.with_handle
      ~on_update:(fun projection ->
        incr updates;
        V.observe view projection)
      (fun fixture ->
         let entered, signal_entered = Eio.Promise.create () in
         let on_view _ = Eio.Promise.resolve signal_entered () in
         Eio.Fiber.both
           (fun () ->
              let result = await view fixture ~on_view in
              assert_error
                result
                (match cause with
                 | `Stream_error | `Durable_gap -> Snapshot_required
                 | `Transport_end -> Interrupted))
           (fun () ->
              Eio.Promise.await entered;
              let before = !updates in
              (match cause with
               | `Stream_error ->
                 S.fail
                   fixture
                   (P.Error.create
                      Snapshot_required
                      ~message:"reader failed before any further view"
                      ~retryable:true
                      ())
               | `Transport_end -> S.end_transport fixture
               | `Durable_gap ->
                 S.emit fixture ~sequence:2L (Operation_started (S.operation Running)));
              C.Session_handle.await_closed (S.handle fixture);
              assert (Int.equal !updates before))))
;;

let%test_unit "replacement idle state cannot invent a terminal" =
  let view = V.create S.session_id in
  S.with_handle ~on_update:(V.observe view) (fun fixture ->
    let running, saw_running = Eio.Promise.create () in
    Eio.Fiber.both
      (fun () ->
         await view fixture ~on_view:(fun projection ->
           if Option.is_some (fields projection).session.active_operation
           then Eio.Promise.resolve saw_running ())
         |> fun result -> assert_error result Snapshot_required)
      (fun () ->
         S.emit fixture ~sequence:1L (Operation_started (S.operation Running));
         Eio.Promise.await running;
         S.replace_idle fixture ~sequence:2L))
;;

let%test_unit "yielding terminal presentation rechecks failure or supersession" =
  List.iter [ `Reader_failure; `Newer_operation ] ~f:(fun change ->
    let view = V.create S.session_id in
    let changed, saw_changed = Eio.Promise.create () in
    S.with_handle
      ~on_update:(fun projection ->
        V.observe view projection;
        if Int64.equal (fields projection).latest_event_sequence 3L
        then Eio.Promise.resolve saw_changed ())
      (fun fixture ->
         let terminal, saw_terminal = Eio.Promise.create () in
         let released, release = Eio.Promise.create () in
         Eio.Fiber.both
           (fun () ->
              await view fixture ~on_view:(fun projection ->
                if Option.is_some (C.Projection.terminal_operation projection)
                then (
                  Eio.Promise.resolve saw_terminal ();
                  Eio.Promise.await released))
              |> fun result ->
              assert_error
                result
                (match change with
                 | `Reader_failure -> Snapshot_required
                 | `Newer_operation -> Conflict))
           (fun () ->
              complete fixture;
              Eio.Promise.await terminal;
              (match change with
               | `Reader_failure ->
                 S.fail
                   fixture
                   (P.Error.create
                      Snapshot_required
                      ~message:"failure during output"
                      ~retryable:true
                      ());
                 C.Session_handle.await_closed (S.handle fixture)
               | `Newer_operation ->
                 let id = P.Id.Operation.of_string "op_newer_completion" |> S.ok in
                 let newer = { (S.operation Running) with id } in
                 S.emit fixture ~sequence:3L (Operation_started newer);
                 Eio.Promise.await changed);
              Eio.Promise.resolve release ())))
;;

let%test_unit "terminal callback exception propagates exactly and releases the waiter" =
  let view = V.create S.session_id in
  S.with_handle ~on_update:(V.observe view) (fun fixture ->
    complete fixture;
    let sentinel = Failure "caller-owned terminal presentation failure" in
    let observed = ref false in
    (match
       await view fixture ~on_view:(fun projection ->
         if Option.is_some (C.Projection.terminal_operation projection)
         then (
           observed := true;
           raise sentinel))
     with
     | _ -> failwith "callback failure was swallowed"
     | exception failure -> assert (phys_equal failure sentinel));
    assert !observed;
    ignore (await view fixture ~on_view:Fn.ignore |> S.ok : C.Projection.t))
;;

let%test_unit "caller cancellation at terminal delivery propagates and joins the race" =
  let view = V.create S.session_id in
  S.with_handle ~on_update:(V.observe view) (fun fixture ->
    complete fixture;
    let sentinel = Failure "cancel before terminal result delivery" in
    let observed = ref false in
    (match
       Eio.Cancel.sub (fun cancellation ->
         await view fixture ~on_view:(fun projection ->
           if Option.is_some (C.Projection.terminal_operation projection)
           then (
             observed := true;
             Eio.Cancel.cancel cancellation sentinel)))
     with
     | _ -> failwith "cancelled terminal was delivered"
     | exception Eio.Cancel.Cancelled failure -> assert (phys_equal failure sentinel));
    assert !observed;
    ignore (await view fixture ~on_view:Fn.ignore |> S.ok : C.Projection.t))
;;

let%test_unit "a second waiter rejects without disturbing the admitted waiter" =
  let view = V.create S.session_id in
  S.with_handle ~on_update:(V.observe view) (fun fixture ->
    let entered, signal_entered = Eio.Promise.create () in
    let signalled = ref false in
    Eio.Fiber.both
      (fun () ->
         ignore
           (await view fixture ~on_view:(fun _ ->
              if not !signalled
              then (
                signalled := true;
                Eio.Promise.resolve signal_entered ()))
            |> S.ok
            : C.Projection.t))
      (fun () ->
         Eio.Promise.await entered;
         await view fixture ~on_view:(fun _ -> failwith "second callback must not run")
         |> fun result ->
         assert_error result Conflict;
         complete fixture))
;;

let%test_unit "a foreign observation rejects even a matching valid handle" =
  let foreign = P.Id.Session.of_string "ses_foreign_completion" |> S.ok in
  let view = V.create S.session_id in
  S.with_handle ~on_update:Fn.ignore (fun fixture ->
    V.observe view (S.foreign_projection foreign);
    await view fixture ~on_view:Fn.ignore |> fun result -> assert_error result Conflict)
;;
