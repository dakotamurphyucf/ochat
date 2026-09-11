open Core
module P = Agent_protocol
module B = Agent_session.Runtime_builder

exception Cleanup_failed of P.Error.t

let interrupted message = P.Error.create Interrupted ~message ~retryable:false ()

let prepare_with_lease ~with_parent ~sw ~parent ~on_revoked ~build =
  let ready, ready_u = Eio.Promise.create () in
  let finished, finished_u = Eio.Promise.create () in
  let stop, stop_u = Eio.Promise.create () in
  let requested = Atomic.make false in
  let published = ref false in
  let completion = ref (Ok ()) in
  let terminal = ref None in
  let signal_stop () =
    match Atomic.exchange requested true with
    | true -> ()
    | false -> Eio.Promise.resolve stop_u ()
  in
  let unwrap = function
    | Ok result -> result
    | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  in
  let close () =
    Eio.Cancel.protect (fun () ->
      signal_stop ();
      match Eio.Promise.await finished |> unwrap with
      | Ok () -> ()
      | Error error -> raise (Cleanup_failed error))
  in
  Eio.Fiber.fork ~sw (fun () ->
    let outcome =
      try
        let result =
          try
            with_parent parent (fun parent_runtime ->
              let created = ref None in
              Exn.protect
                ~finally:(fun () ->
                  Eio.Cancel.protect (fun () ->
                    Option.iter !created ~f:(fun runtime -> runtime.B.close ())))
                ~f:(fun () ->
                  let result =
                    try
                      Eio.Cancel.sub (fun context ->
                        Eio.Switch.run (fun runtime_sw ->
                          Eio.Fiber.fork_daemon ~sw:runtime_sw (fun () ->
                            Eio.Promise.await stop;
                            Eio.Cancel.cancel context Exit;
                            `Stop_daemon);
                          match build ~sw:runtime_sw parent_runtime with
                          | Error error -> Error error
                          | Ok (runtime : B.t) ->
                            created := Some runtime;
                            let check_execution () =
                              match !terminal with
                              | Some (Ok (Error error)) -> Error error
                              | Some _ ->
                                Error
                                  (interrupted
                                     "delegation.runtime_closed: inherited runtime scope \
                                      has ended")
                              | None ->
                                (match runtime.check_execution with
                                 | None -> Ok ()
                                 | Some check -> check ())
                            in
                            published := true;
                            Eio.Promise.resolve
                              ready_u
                              (Ok
                                 (Ok
                                    { runtime with
                                      close
                                    ; check_execution = Some check_execution
                                    }));
                            Eio.Promise.await stop;
                            Ok ()))
                    with
                    | Eio.Cancel.Cancelled _ ->
                      if !published
                      then Ok ()
                      else
                        Error
                          (interrupted
                             "delegation.runtime_cancelled: parent stopped during \
                              preparation")
                  in
                  (* The switch has joined foreground, background and owner event
                     activities before this actor-only lifecycle callback. It must
                     not acquire the child's runtime-owner mutex: close may hold it
                     while awaiting this scope's completion. *)
                  let result =
                    match !published, Atomic.get requested, result with
                    | true, false, Ok () -> Eio.Cancel.protect on_revoked
                    | _ -> result
                  in
                  completion := result;
                  result))
          with
          | Eio.Cancel.Cancelled _ ->
            if !published
            then !completion
            else
              Error
                (interrupted
                   "delegation.runtime_cancelled: parent stopped during preparation")
        in
        Ok result
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    terminal := Some outcome;
    Eio.Promise.resolve finished_u outcome;
    match !published with
    | true -> ()
    | false ->
      Eio.Promise.resolve
        ready_u
        (match outcome with
         | Ok (Error error) -> Ok (Error error)
         | Error error -> Error error
         | Ok (Ok ()) ->
           Ok
             (Error
                (interrupted "delegation.runtime_cancelled: runtime was not published"))));
  match Eio.Promise.await ready with
  | result -> unwrap result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () ->
      signal_stop ();
      ignore (Eio.Promise.await finished));
    Exn.raise_with_original_backtrace exn backtrace
;;

let prepare = prepare_with_lease ~with_parent:Runtime_owner.with_background_runtime

let prepare_independent =
  prepare_with_lease ~with_parent:Runtime_owner.with_delegation_resources
;;
