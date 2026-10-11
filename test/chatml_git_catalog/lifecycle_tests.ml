open Core
module G = Chatml_git_catalog
module H = Chatml_git_host
open Fixture

type expected =
  | Reader_closed
  | Reader_poisoned
  | Reader_busy
  | Foreign_declaration

let is_problem error expected =
  match G.Error.problem error, expected with
  | Reader_closed, Reader_closed
  | Reader_poisoned, Reader_poisoned
  | Reader_busy, Reader_busy
  | Foreign_declaration, Foreign_declaration -> true
  | _ -> false
;;

let%test_unit "scoped declarations and errors retain only their actual source owner" =
  Support.with_workspace (fun env anchor _ _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env) in
    let (saved, escaped), _ =
      Support.acquire
        store
        (repository fixture)
        (old_pin fixture)
        ~limits:(Support.limits ())
        ~f:(fun repo ->
          reader repo ~f:(fun r ->
            let open Result.Let_syntax in
            let%map d = declaration r "pkg" in
            d, r))
      |> native_ok
      |> fst
    in
    assert (Result.is_error (G.Reader.accounting escaped));
    let _, _ =
      Support.acquire
        store
        (repository fixture)
        (old_pin fixture)
        ~limits:(Support.limits ())
        ~f:(fun repo ->
          reader repo ~f:(fun r ->
            match G.Reader.capture r saved ~request with
            | Error e ->
              assert (is_problem e Foreign_declaration);
              Error e
            | Ok _ -> failwith "foreign declaration accepted"))
      |> fun result ->
      match result with
      | Error (`Callback error) ->
        assert (is_problem error Foreign_declaration);
        (), ()
      | Ok _ | Error (`Host _) -> failwith "foreign declaration did not fail in Reader"
    in
    let _ =
      Support.acquire
        store
        (repository fixture)
        (old_pin fixture)
        ~limits:(Support.limits ())
        ~f:(fun repo ->
          reader repo ~f:(fun r ->
            let open Result.Let_syntax in
            let%bind a = declaration r "pkg" in
            let%bind _ = declaration r "other" in
            let missing =
              Chatml_project.Selection.Entry { name = "absent"; environment = None }
            in
            match G.Reader.capture r a ~request:missing with
            | Error e ->
              let at = G.Error.declared_at e |> Option.value_exn in
              assert (
                String.equal
                  (Chatml_source.Id.package_namespace
                     (Chatml_source.id (Chatml_project.Declared_at.source at)))
                  "alpha");
              Error e
            | Ok _ -> failwith "missing entry accepted"))
      |> function
      | Error (`Callback _) -> ()
      | Ok _ | Error (`Host _) -> failwith "wrong error boundary"
    in
    ignore
      (Support.acquire
         store
         (repository fixture)
         (old_pin fixture)
         ~limits:(Support.limits ())
         ~f:(fun repo ->
           reader repo ~f:(fun r ->
             let open Result.Let_syntax in
             let%bind _ = declaration r "pkg" in
             match declaration r "absent" with
             | Error e ->
               assert (Option.is_none (G.Error.declared_at e));
               Error e
             | Ok _ -> failwith "missing bootstrap accepted"))
       |> function
       | Error (`Callback _) -> ()
       | Ok _ | Error (`Host _) -> failwith "bootstrap wrong boundary"))
;;

let%test_unit "busy operation poisons outer commit even when first capture succeeds" =
  Support.with_workspace (fun env anchor cache _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env) in
    let active = ref None in
    let armed = ref false in
    let denied = ref false in
    let first_succeeded = ref false in
    let checkpoint () =
      if !armed
      then (
        armed := false;
        let r = Option.value_exn !active in
        match G.Reader.accounting r with
        | Error e ->
          assert (is_problem e Reader_busy);
          denied := true
        | Ok _ -> failwith "reentrant operation admitted")
    in
    let result =
      Support.acquire
        store
        (repository fixture)
        (old_pin fixture)
        ~limits:(Support.limits ())
        ~f:(fun repo ->
          reader ~checkpoint repo ~f:(fun r ->
            active := Some r;
            let open Result.Let_syntax in
            let%bind d = declaration r "pkg" in
            armed := true;
            let%map _ = G.Reader.capture r d ~request in
            first_succeeded := true))
    in
    assert (!denied && !first_succeeded);
    (match result with
     | Error (`Callback e) -> assert (is_problem e Reader_busy)
     | Ok _ | Error (`Host _) -> failwith "poisoned capture committed");
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages"))))
;;

let pending_cleanup ~raise_callback =
  Support.with_workspace (fun env anchor cache _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env) in
    let gated, signal_gated = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let active = ref None in
    let armed = ref false in
    let finalized = ref false in
    let denied_during_close = ref false in
    let polls = ref 0 in
    let marker = Failure "caller while semantic operation is active" in
    let checkpoint () =
      incr polls;
      if !armed
      then (
        armed := false;
        Fun.protect
          ~finally:(fun () ->
            finalized := true;
            let before = !polls in
            let r = Option.value_exn !active in
            (match G.Reader.accounting r with
             | Error e ->
               assert (is_problem e Reader_closed || is_problem e Reader_poisoned);
               denied_during_close := true
             | Ok _ -> failwith "escaped reader admitted during close");
            assert (before = !polls))
          (fun () ->
             Eio.Promise.resolve signal_gated ();
             Eio.Promise.await never))
    in
    Eio.Switch.run (fun sw ->
      let external_done, signal_external_done = Eio.Promise.create () in
      let outcome =
        try
          Ok
            (Support.acquire
               store
               (repository fixture)
               (old_pin fixture)
               ~limits:(Support.limits ())
               ~f:(fun repo ->
                 Fun.protect
                   ~finally:(fun () -> assert (!finalized && !denied_during_close))
                   (fun () ->
                      reader ~checkpoint repo ~f:(fun r ->
                        active := Some r;
                        let open Result.Let_syntax in
                        let%bind d = declaration r "pkg" in
                        armed := true;
                        Eio.Fiber.fork ~sw (fun () ->
                          Fun.protect
                            ~finally:(fun () ->
                              Eio.Promise.resolve signal_external_done ())
                            (fun () ->
                               match G.Reader.capture r d ~request with
                               | Ok _ | Error _ ->
                                 failwith "gated operation returned without cancellation"
                               | exception Eio.Cancel.Cancelled _ -> ()));
                        Eio.Promise.await gated;
                        if raise_callback then raise marker;
                        Ok ()))))
        with
        | exn -> Error exn
      in
      Eio.Promise.await external_done;
      (match outcome with
       | Error exn -> assert (raise_callback && phys_equal exn marker)
       | Ok (Error (`Callback e)) ->
         assert ((not raise_callback) && is_problem e Reader_busy)
       | Ok (Ok _) | Ok (Error (`Host _)) ->
         failwith "pending work authorized cache publication");
      let before = !polls in
      assert (Result.is_error (G.Reader.accounting (Option.value_exn !active)));
      assert (before = !polls);
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages")))))
;;

let%test_unit
    "callback return cancels and joins active semantic work before releasing repository"
  =
  pending_cleanup ~raise_callback:false
;;

let%test_unit "callback exception remains identical after active semantic work joins" =
  pending_cleanup ~raise_callback:true
;;

let%test_unit "cancelling an operation waiter joins its owned worker before returning" =
  Support.with_workspace (fun env anchor cache _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env) in
    let gated, signal_gated = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let context = ref None in
    let armed = ref false in
    let worker_finalized = ref false in
    let caller_finalized = ref false in
    let checkpoint () =
      if !armed
      then (
        armed := false;
        Fun.protect
          ~finally:(fun () -> worker_finalized := true)
          (fun () ->
             Eio.Promise.resolve signal_gated ();
             Eio.Promise.await never))
    in
    Eio.Switch.run (fun sw ->
      let done_, signal_done = Eio.Promise.create () in
      let result =
        Support.acquire
          store
          (repository fixture)
          (old_pin fixture)
          ~limits:(Support.limits ())
          ~f:(fun repo ->
            reader ~checkpoint repo ~f:(fun r ->
              let open Result.Let_syntax in
              let%bind d = declaration r "pkg" in
              armed := true;
              Eio.Fiber.fork ~sw (fun () ->
                Fun.protect
                  ~finally:(fun () ->
                    assert !worker_finalized;
                    caller_finalized := true;
                    Eio.Promise.resolve signal_done ())
                  (fun () ->
                     match
                       Eio.Cancel.sub (fun cc ->
                         context := Some cc;
                         ignore (G.Reader.capture r d ~request))
                     with
                     | () -> failwith "cancelled operation returned normally"
                     | exception Eio.Cancel.Cancelled _ -> ()));
              Eio.Promise.await gated;
              Eio.Cancel.cancel
                (Option.value_exn !context)
                (Failure "caller cancellation");
              Eio.Promise.await done_;
              assert (!worker_finalized && !caller_finalized);
              Ok ()))
      in
      (match result with
       | Error (`Callback e) -> assert (is_problem e Reader_poisoned)
       | Ok _ | Error (`Host _) -> failwith "caught caller cancellation committed");
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages")))))
;;

module Checkpoint_phase = struct
  type t =
    | Bootstrap
    | Capture
    | Receipt
  [@@deriving equal]
end

let%test_unit
    "caller checkpoint identity survives bootstrap, capture and final receipt boundaries"
  =
  Support.with_workspace (fun env anchor cache _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha256 ~git:(Support.run_git env) in
    List.iter [ Checkpoint_phase.Bootstrap; Capture; Receipt ] ~f:(fun target ->
      let phase = ref Checkpoint_phase.Bootstrap in
      let observed = ref false in
      let marker = Failure "exact caller checkpoint instance" in
      let checkpoint () =
        if Checkpoint_phase.equal !phase target
        then (
          observed := true;
          raise marker)
      in
      (match
         Support.acquire
           store
           (repository fixture)
           (old_pin fixture)
           ~limits:(Support.limits ())
           ~f:(fun repo ->
             reader ~checkpoint repo ~f:(fun r ->
               let open Result.Let_syntax in
               let%bind d = declaration r "pkg" in
               phase := Capture;
               let%map _ = G.Reader.capture r d ~request in
               phase := Receipt))
       with
       | Ok _ | Error _ -> failwith "caller checkpoint converted to result"
       | exception exn -> assert (phys_equal marker exn));
      assert !observed;
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages"))));
    ignore
      (Support.acquire
         store
         (repository fixture)
         (old_pin fixture)
         ~limits:(Support.limits ())
         ~f:(fun repo ->
           reader repo ~f:(fun r ->
             let open Result.Let_syntax in
             let%bind d = declaration r "pkg" in
             G.Reader.capture r d ~request))
       |> native_ok))
;;
