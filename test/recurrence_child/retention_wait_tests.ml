open! Core
open Support
open Controller_retention_fixture

let expect_error expected = function
  | Error (error : P.Error.t) -> [%test_eq: P.Error.code] expected error.code
  | Ok () -> failwith "unavailable owner passed retention admission"
;;

let check_waits sw daemon session (entry : Registry.entry) =
  let registry = Daemon.registry daemon in
  let await check_current =
    Factory.await_held_recurrence_controller
      (Daemon.factory daemon)
      entry
      ~session
      ~check_current
  in
  let id = P.Session_ref.session_id session in
  let no_suspend hold =
    Registry.For_testing.with_loaded_retention_wait_observer
      registry
      entry
      (fun started ->
         hold (fun () -> await (fun ~resumed:_ -> Ok ()) |> expect_error Conflict);
         assert (Option.is_none (Eio.Promise.peek started)))
  in
  no_suspend (fun f ->
    Registry.with_lifecycle registry id (fun _ -> Ok (f ())) |> protocol_ok);
  no_suspend (fun f ->
    Registry.with_loaded_read registry id (fun _ -> Ok (f ())) |> protocol_ok);
  let current = ref true in
  let stale =
    with_retention sw registry entry (fun () ->
      Registry.For_testing.with_loaded_retention_wait_observer
        registry
        entry
        (fun started ->
           let cancel, set_cancel = Eio.Promise.create () in
           let waiter =
             Eio.Fiber.fork_promise ~sw (fun () ->
               try
                 Eio.Cancel.sub (fun context ->
                   Eio.Promise.resolve set_cancel context;
                   await (fun ~resumed:_ -> Ok ()) |> protocol_ok);
                 false
               with
               | Eio.Cancel.Cancelled Exit -> true)
           in
           await_started started waiter;
           Eio.Cancel.cancel (Eio.Promise.await cancel) Exit;
           assert (Eio.Promise.await_exn waiter);
           assert (Registry.lifecycle_reserved registry id));
      Registry.For_testing.with_loaded_retention_wait_observer
        registry
        entry
        (fun started ->
           let waiter =
             Eio.Fiber.fork_promise ~sw (fun () ->
               await (fun ~resumed ->
                 if !current
                 then Ok ()
                 else (
                   assert resumed;
                   Error
                     (P.Error.create
                        Permission_denied
                        ~message:"grant ended"
                        ~retryable:false
                        ()))))
           in
           await_started started waiter;
           current := false;
           waiter))
  in
  Eio.Promise.await_exn stale |> expect_error Permission_denied;
  assert (not (Registry.lifecycle_reserved registry id));
  await (fun ~resumed ->
    assert (not resumed);
    Ok ())
  |> protocol_ok
;;

let%test_unit
    "held retention wait preserves cancellation, invalidation and exclusive fences"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    protect_cleanup
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
      (fun () ->
         let prompt = Filename.concat root "controller.chatmd" in
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(path / "controller.chatmd")
           "<developer>Controller.</developer>";
         with_daemon
           env
           root
           (T.config root root prompt)
           ~provider_calls:(ref 0)
           (fun sw daemon ->
              let connection = T.connection daemon principal in
              protect_cleanup
                ~finally:(fun () -> Agent_client.Connection.close connection)
                (fun () ->
                   T.initialize connection;
                   let created, _ = T.create_session connection in
                   let entry = loaded daemon created.id in
                   let session =
                     P.Session_ref.create
                       ~server_id:(S.server_id (Daemon.store daemon))
                       ~session_id:created.id
                   in
                   check_waits sw daemon session entry))))
;;
