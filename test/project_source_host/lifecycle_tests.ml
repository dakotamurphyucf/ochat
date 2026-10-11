open Core
open Support

exception Marker of int ref

let%expect_test
    "default host does not provision Git and revokes borrowers on explicit close"
  =
  with_workspace (fun env root data ->
    Eio.Switch.run (fun sw ->
      let host = open_host env data sw in
      let routes =
        Host.with_host host ~f:(fun _ sources ->
          List.length sources.git_grants, Option.is_none sources.git_store)
        |> ok
      in
      print_s
        [%sexp
          (routes : int * bool)
        , ((match
              Eio.Path.kind ~follow:false Eio.Path.(root / "data" / "project-git")
            with
            | `Not_found -> true
            | _ -> false)
           : bool)];
      let marker = Marker (ref 4) in
      (match Host.with_host host ~f:(fun _ _ -> raise marker) with
       | exception exn -> assert (phys_equal exn marker)
       | Ok _ | Error _ -> assert false);
      let marker = Agent_session.Runtime_activity.Closed in
      (match Host.with_host host ~f:(fun _ _ -> raise marker) with
       | exception exn -> assert (phys_equal exn marker)
       | Ok _ | Error _ -> assert false);
      Host.close_and_wait host;
      closed (Host.with_host host ~f:(fun _ _ -> ()));
      print_endline "closed"));
  [%expect
    {|
    ((0 true) true)
    closed
    |}]
;;

let%test_unit "normal owner closure cancels and joins a foreign borrower before release" =
  with_workspace (fun env _ data ->
    Eio.Switch.run (fun callers ->
      let finished = ref false in
      let escaped = ref None in
      let entered, signal = Eio.Promise.create () in
      let blocked, _ = Eio.Promise.create () in
      let outcome, finish = Eio.Promise.create () in
      Eio.Switch.run (fun owned ->
        let host = open_host env data owned in
        escaped := Some host;
        Eio.Fiber.fork ~sw:callers (fun () ->
          Eio.Promise.resolve
            finish
            (try
               ignore
                 (Host.with_host host ~f:(fun _ _ ->
                    Fun.protect
                      ~finally:(fun () -> finished := true)
                      (fun () ->
                         Eio.Promise.resolve signal ();
                         Eio.Promise.await blocked))
                  : (unit, _) Result.t);
               false
             with
             | Eio.Cancel.Cancelled _ -> true));
        Eio.Promise.await entered);
      assert !finished;
      assert (Eio.Promise.await outcome);
      closed (Host.with_host (Option.value_exn !escaped) ~f:(fun _ _ -> ()))))
;;

let%test_unit "explicit close waits for protected cleanup and revokes before the join" =
  with_workspace (fun env _ data ->
    Eio.Switch.run (fun sw ->
      let host = open_host env data sw in
      let entered, signal = Eio.Promise.create () in
      let cleaning, clean = Eio.Promise.create () in
      let release, unblock = Eio.Promise.create () in
      let blocked, _ = Eio.Promise.create () in
      let finished = ref false in
      Eio.Fiber.fork ~sw (fun () ->
        try
          ignore
            (Host.with_host host ~f:(fun _ _ ->
               Fun.protect
                 ~finally:(fun () ->
                   Eio.Cancel.protect (fun () ->
                     Eio.Promise.resolve clean ();
                     Eio.Promise.await release;
                     finished := true))
                 (fun () ->
                    Eio.Promise.resolve signal ();
                    Eio.Promise.await blocked))
             : (unit, _) Result.t)
        with
        | Eio.Cancel.Cancelled _ -> ());
      Eio.Promise.await entered;
      let closed_done, done_ = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Host.close_and_wait host;
        Eio.Promise.resolve done_ ());
      Eio.Promise.await cleaning;
      assert (not (Eio.Promise.is_resolved closed_done));
      closed (Host.with_host host ~f:(fun _ _ -> ()));
      Eio.Promise.resolve unblock ();
      Eio.Promise.await closed_done;
      assert !finished))
;;

let%test_unit
    "parent cancellation joins borrowed activity and pending result delivery stays \
     cancelled"
  =
  with_workspace (fun env _ data ->
    Eio.Switch.run (fun callers ->
      let entered, signal = Eio.Promise.create () in
      let blocked, _ = Eio.Promise.create () in
      let done_, finish = Eio.Promise.create () in
      let cleaned = ref false in
      (try
         Eio.Cancel.sub (fun parent ->
           Eio.Switch.run (fun sw ->
             let host = open_host env data sw in
             Eio.Fiber.fork ~sw:callers (fun () ->
               (try
                  ignore
                    (Host.with_host host ~f:(fun _ _ ->
                       Fun.protect
                         ~finally:(fun () -> cleaned := true)
                         (fun () ->
                            Eio.Promise.resolve signal ();
                            Eio.Promise.await blocked))
                     : (unit, _) Result.t)
                with
                | Eio.Cancel.Cancelled _ -> ());
               Eio.Promise.resolve finish ());
             Eio.Promise.await entered;
             Eio.Cancel.cancel parent Exit;
             Eio.Fiber.check ()))
       with
       | Eio.Cancel.Cancelled _ -> ());
      Eio.Promise.await done_;
      assert !cleaned;
      Eio.Switch.run (fun sw ->
        let host = open_host env data sw in
        let delivered = ref false in
        (try
           Eio.Cancel.sub (fun caller ->
             ignore
               (Host.with_host host ~f:(fun _ _ ->
                  Eio.Cancel.cancel caller Exit;
                  42)
                : (int, _) Result.t);
             delivered := true)
         with
         | Eio.Cancel.Cancelled _ -> ());
        assert (not !delivered))))
;;

let%test_unit
    "every local startup interruption preserves identity and permits a fresh owner"
  =
  with_workspace (fun env root data ->
    let polls = ref 0 in
    Eio.Switch.run (fun sw ->
      let host = open_host ~checkpoint:(fun () -> incr polls) env data sw in
      Host.close_and_wait host);
    assert (!polls > 0);
    let marker =
      try
        ignore (Eio.Path.load Eio.Path.(root / "missing") : string);
        assert false
      with
      | Eio.Io _ as exn -> exn
    in
    for stop = 1 to !polls do
      Eio.Switch.run (fun sw ->
        let visits = ref 0 in
        (match
           open_host
             ~checkpoint:(fun () ->
               incr visits;
               if !visits = stop then raise marker)
             env
             data
             sw
         with
         | exception exn -> assert (phys_equal exn marker)
         | _ -> assert false);
        assert (!visits = stop))
    done;
    Eio.Switch.run (fun sw ->
      let host = open_host env data sw in
      let rejected =
        Stdlib.Domain.spawn (fun () ->
          match Host.with_host host ~f:(fun _ _ -> assert false) with
          | Error Host.Error.Foreign_domain -> true
          | Ok _ | Error _ -> false)
        |> Stdlib.Domain.join
      in
      assert rejected;
      Host.with_host host ~f:(fun _ _ -> ()) |> ok))
;;

let%test_unit
    "already cancelled owner startup preserves cancellation without admitting a host"
  =
  with_workspace (fun env _ data ->
    Eio.Switch.run (fun sw ->
      let returned = ref false in
      (try
         Eio.Cancel.sub (fun caller ->
           Eio.Cancel.cancel caller Exit;
           ignore (open_host env data sw);
           returned := true)
       with
       | Eio.Cancel.Cancelled _ -> ());
      assert (not !returned)))
;;
