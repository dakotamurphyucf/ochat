open Core
open Support

exception Stop of int ref

let[@inline never] checkpoint_exception_marker marker = raise marker

let%test_unit "capture checkpoint exceptions retain identity and joined native cleanup" =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>checkpoint</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let polls = ref 0 in
      ignore
        (Owner.with_scope owner ~f:(fun scope ->
           capture env scope definition ~checkpoint:(fun () -> incr polls))
         |> ok
         : Owner.Capture.t);
      assert (!polls > 0);
      let before = descriptors env in
      let previous = Stdlib.Printexc.backtrace_status () in
      Fun.protect
        ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous)
        (fun () ->
           Stdlib.Printexc.record_backtrace true;
           for stop = 1 to !polls do
             let marker = Stop (ref stop) in
             let visited = ref 0 in
             (match
                Owner.with_scope owner ~f:(fun scope ->
                  capture env scope definition ~checkpoint:(fun () ->
                    incr visited;
                    if !visited = stop then checkpoint_exception_marker marker))
              with
              | exception exn ->
                let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                assert (phys_equal exn marker);
                assert (
                  String.is_substring
                    (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                    ~substring:"checkpoint_exception_marker")
              | Ok _ | Error _ -> assert false);
             assert (descriptors env = before)
           done);
      ignore
        (Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
         : Owner.Capture.t)))
;;

let%test_unit
    "early callback return cancels and joins a pure checkpoint worker before releasing \
     scope"
  =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>pending</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let entered, signal = Eio.Promise.create () in
      let block, _ = Eio.Promise.create () in
      let finished, finish = Eio.Promise.create () in
      let escaped = ref None in
      let worker_finished = ref false in
      let outcome =
        Owner.with_scope owner ~f:(fun scope ->
          escaped := Some scope;
          Eio.Fiber.fork ~sw (fun () ->
            Fun.protect
              ~finally:(fun () ->
                worker_finished := true;
                Eio.Promise.resolve finish ())
              (fun () ->
                 try
                   ignore
                     (capture env scope definition ~checkpoint:(fun () ->
                        Eio.Promise.resolve signal ();
                        Eio.Promise.await block)
                      : (_, _) Result.t)
                 with
                 | Eio.Cancel.Cancelled _ -> ()));
          Eio.Promise.await entered;
          Ok ())
      in
      error outcome Busy;
      Eio.Promise.await finished;
      assert !worker_finished;
      error (Owner.Scope.check (Option.value_exn !escaped)) Closed;
      ignore
        (Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
         : Owner.Capture.t)))
;;

let%test_unit
    "callback failure while capture is pending preserves original failure after join"
  =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>pending</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let entered, signal = Eio.Promise.create () in
      let block, _ = Eio.Promise.create () in
      let finished, finish = Eio.Promise.create () in
      let marker = Stop (ref 9) in
      (match
         Owner.with_scope owner ~f:(fun scope ->
           Eio.Fiber.fork ~sw (fun () ->
             Fun.protect
               ~finally:(fun () -> Eio.Promise.resolve finish ())
               (fun () ->
                  try
                    ignore
                      (capture env scope definition ~checkpoint:(fun () ->
                         Eio.Promise.resolve signal ();
                         Eio.Promise.await block)
                       : (_, _) Result.t)
                  with
                  | Eio.Cancel.Cancelled _ -> ()));
           Eio.Promise.await entered;
           raise marker)
       with
       | exception exn -> assert (phys_equal exn marker)
       | Ok _ | Error _ -> assert false);
      Eio.Promise.await finished;
      ignore
        (Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
         : Owner.Capture.t)))
;;

let%test_unit
    "busy poison survives caught rejection and revocation is checked after user \
     checkpoint"
  =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>busy</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let entered, signal = Eio.Promise.create () in
      let continue, resume = Eio.Promise.create () in
      let finished, finish = Eio.Promise.create () in
      let rejected =
        Owner.with_scope owner ~f:(fun scope ->
          Eio.Fiber.fork ~sw (fun () ->
            ignore
              (capture env scope definition ~checkpoint:(fun () ->
                 Eio.Promise.resolve signal ();
                 Eio.Promise.await continue)
               : (_, _) Result.t);
            Eio.Promise.resolve finish ());
          Eio.Promise.await entered;
          error (capture env scope definition) Busy;
          Eio.Promise.resolve resume ();
          Eio.Promise.await finished;
          Ok ())
      in
      error rejected Busy;
      let revoked =
        Owner.with_scope owner ~f:(fun scope ->
          capture env scope definition ~checkpoint:(fun () ->
            Owner.Policy.revoke (Owner.policy owner)))
      in
      error revoked Revoked))
;;

let%test_unit
    "cancelled owner switch refuses capture before fork without unresolved cleanup"
  =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>cancel before fork</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let invoked = ref false in
      (match
         Eio.Cancel.sub (fun cancel ->
           Owner.with_scope owner ~f:(fun scope ->
             Eio.Cancel.cancel cancel (Stop (ref 1));
             capture env scope definition ~checkpoint:(fun () -> invoked := true)))
       with
       | exception Eio.Cancel.Cancelled _ -> ()
       | Ok _ | Error _ -> assert false);
      assert (not !invoked);
      ignore
        (Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
         : Owner.Capture.t)))
;;

let%test_unit "cancelled nested caller cannot admit work to a retained outer scope" =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>caller cancelled</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let marker = Stop (ref 12) in
      let before = descriptors env in
      let invoked = ref false in
      let preprocessed = ref false in
      Owner.with_scope owner ~f:(fun scope ->
        (match
           Eio.Cancel.sub (fun caller ->
             Eio.Cancel.cancel caller marker;
             Owner.Scope.capture
               scope
               ~env
               ~definition
               ~preprocess:(fun text ->
                 preprocessed := true;
                 text)
               ~limits:(parser_limits ())
               ~checkpoint:(fun () -> invoked := true))
         with
         | exception Eio.Cancel.Cancelled reason -> assert (phys_equal reason marker)
         | Ok _ | Error _ -> assert false);
        assert (not !invoked);
        assert (not !preprocessed);
        assert (descriptors env = before);
        capture env scope definition)
      |> ok
      |> ignore))
;;

let%test_unit "cancellation of a resumed caller prevents completed capture delivery" =
  with_workspace (fun env _ root ->
    save root "root.chatmd" "<user>delivery cancelled</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      let polls = ref 0 in
      Owner.with_scope owner ~f:(fun scope ->
        capture env scope definition ~checkpoint:(fun () -> incr polls))
      |> ok
      |> ignore;
      assert (!polls > 0);
      let marker = Stop (ref 13) in
      let delivered = ref false in
      let cancellation_ran = ref false in
      let visited = ref 0 in
      (match
         Owner.with_scope owner ~f:(fun scope ->
           Eio.Cancel.sub (fun caller ->
             let result =
               capture env scope definition ~checkpoint:(fun () ->
                 incr visited;
                 if !visited = !polls
                 then
                   Eio.Fiber.fork ~sw (fun () ->
                     (* Fork runs the helper first. Yield lets the worker finish
                        and resolve its join before this helper cancels the
                        resumed caller. A resolved Promise.await does not poll. *)
                     Eio.Fiber.yield ();
                     cancellation_ran := true;
                     Eio.Cancel.cancel caller marker))
             in
             delivered := true;
             result))
       with
       | exception Eio.Cancel.Cancelled reason -> assert (phys_equal reason marker)
       | Ok _ | Error _ -> assert false);
      assert !cancellation_ran;
      assert (!visited = !polls);
      assert (not !delivered);
      Owner.with_scope owner ~f:(fun scope -> capture env scope definition)
      |> ok
      |> ignore))
;;
