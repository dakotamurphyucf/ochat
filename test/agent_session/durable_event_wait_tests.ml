open Core
open Fixtures
module P = Agent_protocol
module Log = Agent_session.Durable_event_log

let%expect_test
    "committed-event waits broadcast without lost wakeups or cancellation coupling"
  =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
      Eio.Switch.run (fun sw ->
        let log = Log.create ~capacity:1 [] |> protocol_ok in
        let before_read = Log.changed log in
        let event sequence =
          P.Event.Durable.of_payload
            ~session_id
            ~sequence
            ~revision:sequence
            ~timestamp
            (History_appended [])
        in
        (* The actor commits after a reader's snapshot and before its await. *)
        Log.append log [ event 1L ];
        Eio.Promise.await before_read;
        let next = Log.changed log in
        assert (Option.is_none (Eio.Promise.peek next));
        let cancelled, cancel_ready = Eio.Promise.create () in
        let exited, exit_ready = Eio.Promise.create () in
        Eio.Fiber.fork ~sw (fun () ->
          (try
             Eio.Cancel.sub (fun context ->
               Eio.Promise.resolve cancel_ready context;
               Eio.Promise.await next;
               failwith "cancelled event waiter completed")
           with
           | Eio.Cancel.Cancelled _ -> ());
          Eio.Promise.resolve exit_ready ());
        Eio.Cancel.cancel (Eio.Promise.await cancelled) Exit;
        Eio.Promise.await exited;
        assert (Option.is_none (Eio.Promise.peek next));
        let results = List.init 2 ~f:(fun _ -> Eio.Promise.create ()) in
        List.iter results ~f:(fun (_, resolved) ->
          Eio.Fiber.fork ~sw (fun () ->
            Eio.Promise.await next;
            Eio.Promise.resolve resolved ()));
        Log.append log [];
        assert (Option.is_none (Eio.Promise.peek next));
        Log.append log [ event 2L ];
        List.iter results ~f:(fun (result, _) -> Eio.Promise.await result);
        (* Neither another reader nor replay-window eviction consumes the pulse. *)
        Eio.Promise.await next;
        Eio.Promise.await before_read;
        assert (Option.is_none (Eio.Promise.peek (Log.changed log)));
        print_endline
          "commit-before-await retained; cancelled waiter isolated; all readers wake")));
  [%expect
    {| commit-before-await retained; cancelled waiter isolated; all readers wake |}]
;;
