open! Core

exception Stop

type failure = exn * Stdlib.Printexc.raw_backtrace

let with_switch f =
  let primary = ref None in
  let stop, stop_u = Eio.Promise.create () in
  let joined =
    try
      Eio.Cancel.sub (fun context ->
        Eio.Switch.run (fun sw ->
          Eio.Fiber.fork_daemon ~sw (fun () ->
            Eio.Promise.await stop;
            Eio.Cancel.cancel context Stop;
            `Stop_daemon);
          let outcome =
            try Ok (f sw) with
            | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
          in
          primary := Some outcome;
          Eio.Promise.resolve stop_u ()));
      Ok ()
    with
    | Eio.Cancel.Cancelled Stop when Option.is_some !primary -> Ok ()
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  match !primary, joined with
  | Some (Error (exn, backtrace)), (Ok () | Error _) ->
    Stdlib.Printexc.raise_with_backtrace exn backtrace
  | Some (Ok result), Ok () -> result
  | Some (Ok _), Error (exn, backtrace) ->
    Stdlib.Printexc.raise_with_backtrace exn backtrace
  | None, Error (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
  | None, Ok () -> failwith "claimed runtime switch exited before preparation completed"
;;
