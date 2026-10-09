open! Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module Cleanup = Agent_server.Embedded_host_cleanup

exception Detach_failed

let with_configuration env root f =
  let workspace = Filename.concat root "workspace" in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
  let prompt = Filename.concat root "root.chatmd" in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    Eio.Path.(Eio.Stdenv.fs env / prompt)
    "<developer>Cleanup fixture.</developer>";
  f (config root workspace prompt)
;;

let start sw env root configuration =
  D.start
    ~sw
    ~env
    ~config:configuration
    ~tool_dir:root
    ~home:root
    ~process_start_identity:None
    ~options:{ D.default_options with startup_mode = On_demand }
    ()
  |> protocol_ok
;;

let failing_connection ~close =
  let completed = ref false in
  let close_actual () =
    if not !completed
    then (
      close ();
      completed := true)
  in
  let connection =
    Agent_client.In_memory.create
      ~request:(fun _ -> Error (P.Error.invalid_request "fixture has no requests"))
      ~notifications:(Eio.Stream.create 1)
      ~close:close_actual
  in
  Cleanup.Connection_owner.create ~connection ~close_actual
;;

let fail_once () =
  let failed = ref false in
  fun () ->
    if not !failed
    then (
      failed := true;
      raise Detach_failed)
;;

let%expect_test "embedded detach failure retains daemon until same owner retry" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        with_configuration env root (fun configuration ->
          Eio.Switch.run (fun sw ->
            let daemon = start sw env root configuration in
            let cleanup = Cleanup.create ~sw ~env ~daemon ~temporary_root:(Some root) in
            let calls = ref 0 in
            Cleanup.adopt_connection_exn
              cleanup
              (failing_connection ~close:(fun () ->
                 Int.incr calls;
                 if Int.equal !calls 1 then raise Detach_failed));
            let primary_preserved =
              match Cleanup.close cleanup with
              | () -> false
              | exception Detach_failed -> true
            in
            let closing = Cleanup.is_closing cleanup in
            let released = Agent_store.Session_store.is_closed (D.store daemon) in
            let removed =
              not (Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / root))
            in
            Cleanup.close cleanup;
            print_s
              [%sexp
                ((primary_preserved, closing, released, removed, !calls)
                 : bool * bool * bool * bool * int)]))));
  [%expect {| (true true false false 2) |}]
;;

let%expect_test "construction cleanup failure preserves primary and fails owning scope" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        with_configuration env root (fun configuration ->
          let failure =
            match
              Eio.Switch.run (fun sw ->
                let daemon = start sw env root configuration in
                let cleanup =
                  Cleanup.create ~sw ~env ~daemon ~temporary_root:(Some root)
                in
                Cleanup.adopt_connection_exn
                  cleanup
                  (failing_connection ~close:(fail_once ()));
                Cleanup.protect cleanup (fun () ->
                  Error
                    (P.Error.create
                       Conflict
                       ~message:"original constructor rejection"
                       ~retryable:false
                       ()))
                |> ignore)
            with
            | () -> None
            | exception Cleanup.Cleanup_failed failure -> Some failure
          in
          let primary_preserved, secondary_preserved =
            match failure with
            | None -> false, false
            | Some failure ->
              let primary = Cleanup.Failure.primary failure in
              let secondary = Cleanup.Failure.cleanup failure in
              ( P.Error.equal_code
                  (Agent_server.Session_registry.Cleanup_failure.error primary).code
                  Conflict
              , (match
                   Agent_server.Session_registry.Cleanup_failure.exception_and_backtrace
                     secondary
                 with
                 | Some (Detach_failed, _) -> true
                 | Some _ | None -> false) )
          in
          print_s
            [%sexp
              (( primary_preserved
               , secondary_preserved
               , not (Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / root)) )
               : bool * bool * bool)])));
  [%expect {| (true true true) |}]
;;

let%expect_test "construction cancellation closes owned host and propagates cancellation" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        with_configuration env root (fun configuration ->
          Eio.Switch.run (fun sw ->
            let daemon = start sw env root configuration in
            let cleanup = Cleanup.create ~sw ~env ~daemon ~temporary_root:(Some root) in
            let detached = ref 0 in
            Cleanup.adopt_connection_exn
              cleanup
              (failing_connection ~close:(fun () ->
                 Int.incr detached;
                 Eio.Fiber.yield ()));
            let cancelled =
              match
                Eio.Cancel.sub (fun cancel ->
                  Cleanup.protect cleanup (fun () ->
                    Eio.Cancel.cancel cancel Detach_failed;
                    Eio.Fiber.yield ();
                    Ok ()))
              with
              | Ok () | Error _ -> false
              | exception Eio.Cancel.Cancelled _ -> true
            in
            print_s
              [%sexp
                (( cancelled
                 , Cleanup.is_closing cleanup
                 , Agent_store.Session_store.is_closed (D.store daemon)
                 , not (Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / root))
                 , !detached )
                 : bool * bool * bool * bool * int)]))));
  [%expect {| (true true true true 1) |}]
;;

let%expect_test "started session scope preserves rejection through operator close retry" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        with_configuration env root (fun _configuration ->
          let attempts = ref 0 in
          let daemon_options =
            { D.default_options with
              inference_policy =
                inference_policy
                  ~default_model:"fixture-model"
                  ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "unexpected provider")
            ; provider_operator_factory =
                Some
                  (fun ~sw:_ ~server_id:_ ->
                    Ok
                      (Agent_server.Provider_operator_port.create
                         ~dispatch:(fun ~actor:_ _ -> failwith "unexpected dispatch")
                         ~receipt:(fun ~actor:_ _ -> failwith "unexpected receipt")
                         ~close:(fun () ->
                           Int.incr attempts;
                           if Int.equal !attempts 1 then raise Detach_failed)))
            }
          in
          let options : Agent_server.Embedded.options =
            { prompt_file = Filename.concat root "root.chatmd"
            ; workspace = Filename.concat root "workspace"
            ; tool_dir = root
            ; home = Some root
            ; storage = Transient
            ; start_immediately = false
            ; permission_profile = Agent_server.Embedded.default_permission_profile
            ; attachment_mode = Read_write
            ; event_capacity = 32
            }
          in
          let failure =
            match
              Eio.Switch.run (fun sw ->
                let session =
                  Agent_server.Embedded.start ~sw ~env ~daemon_options options
                  |> protocol_ok
                in
                Agent_server.Embedded.with_session session ~f:(fun _ ->
                  Error
                    (P.Error.create
                       Conflict
                       ~message:"original started-session rejection"
                       ~retryable:false
                       ()))
                |> ignore)
            with
            | () -> None
            | exception Cleanup.Cleanup_failed failure -> Some failure
          in
          let primary, secondary =
            match failure with
            | None -> false, false
            | Some failure ->
              ( P.Error.equal_code
                  (Agent_server.Session_registry.Cleanup_failure.error
                     (Cleanup.Failure.primary failure))
                    .code
                  Conflict
              , (match
                   Agent_server.Session_registry.Cleanup_failure.exception_and_backtrace
                     (Cleanup.Failure.cleanup failure)
                 with
                 | Some (Detach_failed, _) -> true
                 | Some _ | None -> false) )
          in
          print_s [%sexp ((primary, secondary, !attempts) : bool * bool * int)])));
  [%expect {| (true true 2) |}]
;;

let%expect_test
    "owned additional connection failure blocks release and closed adoption constructs \
     nothing"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        with_configuration env root (fun configuration ->
          Eio.Switch.run (fun sw ->
            let daemon = start sw env root configuration in
            let cleanup = Cleanup.create ~sw ~env ~daemon ~temporary_root:(Some root) in
            let primary_closes = ref 0 in
            let extra_closes = ref 0 in
            Cleanup.adopt_connection_exn
              cleanup
              (failing_connection ~close:(fun () -> Int.incr primary_closes));
            Cleanup.adopt_additional_connection cleanup ~create:(fun () ->
              failing_connection ~close:(fun () ->
                Int.incr extra_closes;
                if Int.equal !extra_closes 1 then raise Detach_failed))
            |> protocol_ok
            |> ignore;
            let failed =
              match Cleanup.close cleanup with
              | () -> false
              | exception Detach_failed -> true
            in
            let retained = not (Agent_store.Session_store.is_closed (D.store daemon)) in
            let constructed = ref 0 in
            let rejected =
              match
                Cleanup.adopt_additional_connection cleanup ~create:(fun () ->
                  Int.incr constructed;
                  failing_connection ~close:(fun () -> ()))
              with
              | Error error -> P.Error.equal_code error.code Interrupted
              | Ok _ -> false
            in
            Cleanup.close cleanup;
            print_s
              [%sexp
                (( failed
                 , retained
                 , rejected
                 , !constructed
                 , !primary_closes
                 , !extra_closes
                 , Agent_store.Session_store.is_closed (D.store daemon) )
                 : bool * bool * bool * int * int * int * bool)]))));
  [%expect {| (true true true 0 1 2 true) |}]
;;

exception Detach_retry_failed

let%expect_test
    "permanent actual detach failure retains primary and latest scope diagnostic"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        with_configuration env root (fun configuration ->
          let attempts = ref 0 in
          let release_blocked = ref true in
          let rec cleanup_failures = function
            | Cleanup.Cleanup_failed failure -> [ failure ]
            | Eio.Exn.Multiple failures ->
              List.concat_map failures ~f:(fun (exn, _) -> cleanup_failures exn)
            | exn -> raise exn
          in
          let failures =
            match
              Eio.Switch.run (fun sw ->
                let daemon = start sw env root configuration in
                let cleanup =
                  Cleanup.create ~sw ~env ~daemon ~temporary_root:(Some root)
                in
                Cleanup.adopt_connection_exn
                  cleanup
                  (failing_connection ~close:(fun () ->
                     Int.incr attempts;
                     release_blocked
                     := !release_blocked
                        && (not (Agent_store.Session_store.is_closed (D.store daemon)))
                        && Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / root);
                     if Int.equal !attempts 1
                     then raise Detach_failed
                     else raise Detach_retry_failed));
                Cleanup.protect cleanup (fun () ->
                  Error
                    (P.Error.create
                       Conflict
                       ~message:"original permanent-detach rejection"
                       ~retryable:false
                       ()))
                |> ignore)
            with
            | () -> []
            | exception exn -> cleanup_failures exn
          in
          let original_and_latest =
            List.exists failures ~f:(fun failure ->
              P.Error.equal_code
                (Agent_server.Session_registry.Cleanup_failure.error
                   (Cleanup.Failure.primary failure))
                  .code
                Conflict
              &&
              match
                Agent_server.Session_registry.Cleanup_failure.exception_and_backtrace
                  (Cleanup.Failure.cleanup failure)
              with
              | Some (Detach_retry_failed, _) -> true
              | Some _ | None -> false)
          in
          print_s
            [%sexp
              (( original_and_latest
               , !release_blocked
               , !attempts
               , Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / root) )
               : bool * bool * int * bool)])));
  [%expect {| (true true 2 true) |}]
;;
