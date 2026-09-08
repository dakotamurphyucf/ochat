open Core
module G = Chat_response.Execution_gate

let enter gate f =
  match G.with_access gate f with
  | Ok result -> result
  | Error e -> Error e
;;

let ok = function
  | Ok value -> value
  | Error e -> failwith (G.error_message e)
;;

let is expected = function
  | Error actual -> Poly.equal expected actual
  | Ok _ -> false
;;

let%test_unit "synchronous legacy calls reject direct and indirect reentrancy" =
  let a = G.create ()
  and b = G.create () in
  G.with_access a (fun () ->
    assert (is G.Reentrant (G.with_access a (fun () -> assert false)));
    G.with_access b (fun () ->
      assert (is G.Reentrant (G.with_access a (fun () -> assert false)))))
  |> ok
  |> ok;
  (match G.with_access a (fun () -> raise Exit) with
   | _ -> assert false
   | exception Exit -> ());
  G.with_access a ignore |> ok
;;

let%test_unit "independent callers queue and execute one at a time" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Switch.run (fun sw ->
        let gate = G.create () in
        let held, held_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let attempted, attempted_u = Eio.Promise.create () in
        let second_done, second_done_u = Eio.Promise.create () in
        let order = ref [] in
        Eio.Fiber.fork ~sw (fun () ->
          G.with_access gate (fun () ->
            Eio.Promise.resolve held_u ();
            Eio.Promise.await release;
            order := !order @ [ 1 ])
          |> ok);
        Eio.Promise.await held;
        Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.resolve attempted_u ();
          G.with_access gate (fun () -> order := !order @ [ 2 ]) |> ok;
          Eio.Promise.resolve second_done_u ());
        Eio.Promise.await attempted;
        Eio.Fiber.yield ();
        assert (List.is_empty !order);
        Eio.Promise.resolve release_u ();
        Eio.Promise.await second_done;
        assert (Poly.equal !order [ 1; 2 ]))))
;;

let%test_unit "two and three independent owners reject the cycle-closing call" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      List.iter [ 2; 3 ] ~f:(fun count ->
        Eio.Switch.run (fun sw ->
          let gates = Array.init count ~f:(fun _ -> G.create ()) in
          let held = Array.init count ~f:(fun _ -> Eio.Promise.create ()) in
          let proceed = Array.init count ~f:(fun _ -> Eio.Promise.create ()) in
          let done_ = Array.init count ~f:(fun _ -> Eio.Promise.create ()) in
          Array.iteri gates ~f:(fun i gate ->
            Eio.Fiber.fork ~sw (fun () ->
              let result =
                enter gate (fun () ->
                  Eio.Promise.resolve (snd held.(i)) ();
                  Eio.Promise.await (fst proceed.(i));
                  G.with_access gates.((i + 1) mod count) ignore)
              in
              Eio.Promise.resolve (snd done_.(i)) result));
          Array.iter held ~f:(fun (promise, _) -> Eio.Promise.await promise);
          Array.iter proceed ~f:(fun (_, resolver) ->
            Eio.Promise.resolve resolver ();
            Eio.Fiber.yield ());
          Array.iteri done_ ~f:(fun i (promise, _) ->
            let result = Eio.Promise.await promise in
            if i = count - 1 then assert (is G.Wait_cycle result) else ok result);
          Array.iter gates ~f:(fun gate -> G.with_access gate ignore |> ok)))))
;;

let%test_unit "cancelled wait removes its dependency and releases its ancestor" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Switch.run (fun sw ->
        let a = G.create ()
        and b = G.create () in
        let held, held_u = Eio.Promise.create () in
        let continue, continue_u = Eio.Promise.create () in
        let done_, done_u = Eio.Promise.create () in
        Eio.Fiber.fork ~sw (fun () ->
          enter b (fun () ->
            Eio.Promise.resolve held_u ();
            Eio.Promise.await continue;
            G.with_access a ignore)
          |> ok;
          Eio.Promise.resolve done_u ());
        Eio.Promise.await held;
        (match
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.05 (fun () ->
             enter a (fun () -> G.with_access b (fun () -> assert false)))
         with
         | _ -> assert false
         | exception Eio.Time.Timeout -> ());
        Eio.Promise.resolve continue_u ();
        Eio.Promise.await done_)))
;;

let%test_unit "forked fibers inherit active ownership but not expired ownership" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Switch.run (fun sw ->
        let gate = G.create () in
        let later, later_u = Eio.Promise.create () in
        let done_, done_u = Eio.Promise.create () in
        G.with_access gate (fun () ->
          Eio.Fiber.all
            [ (fun () -> assert (is G.Reentrant (G.with_access gate ignore))) ];
          Eio.Fiber.fork ~sw (fun () ->
            Eio.Promise.await later;
            G.with_access gate ignore |> ok;
            Eio.Promise.resolve done_u ()))
        |> ok;
        Eio.Promise.resolve later_u ();
        Eio.Promise.await done_)))
;;

let%test_unit "coordination works across Eio domains" =
  Eio_main.run (fun env ->
    let gate = G.create () in
    let count = ref 0 in
    Eio.Fiber.all
      (List.init 2 ~f:(fun _ () ->
         Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
           for _ = 1 to 50 do
             G.with_access gate (fun () ->
               let before = !count in
               Eio.Fiber.yield ();
               count := before + 1)
             |> ok
           done)));
    assert (!count = 100))
;;

let%test_unit "ancestry is bounded and rejection leaves owners reusable" =
  let gates = List.init 65 ~f:(fun _ -> G.create ()) in
  let rec descend = function
    | [] -> failwith "expected ancestry limit"
    | gate :: rest -> enter gate (fun () -> descend rest)
  in
  assert (is G.Resource_limit (descend gates));
  List.iter gates ~f:(fun gate -> G.with_access gate ignore |> ok)
;;

let%test_unit "host domain handoffs preserve nested owner ancestry explicitly" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      let gate = G.create () in
      G.with_access gate (fun () ->
        Eio.Domain_manager.run
          (Eio.Stdenv.domain_mgr env)
          (G.inherit_context (fun () ->
             assert (is G.Reentrant (G.with_access gate ignore)))))
      |> ok))
;;
