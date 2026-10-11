open Core

let run ~checkpoint ~f ~on_parser_error =
  let exception Callback of exn * Stdlib.Printexc.raw_backtrace in
  let guarded () =
    try checkpoint () with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      raise (Callback (exn, backtrace))
  in
  try f guarded with
  | Callback (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Exn.raise_with_original_backtrace exn backtrace
  | exn -> on_parser_error exn
;;
