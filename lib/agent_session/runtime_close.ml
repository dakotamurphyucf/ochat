open! Core

type t = { close : unit -> unit Close_observation.t }

let raised exn backtrace =
  let diagnostic =
    Cleanup_diagnostic.create [ Callback_cleanup.Failure.Raised (exn, backtrace) ]
  in
  Close_observation.create
    (Raised (exn, backtrace))
    ~diagnostics:[ diagnostic ]
    ~primary:diagnostic
    ()
;;

let reported close =
  { close =
      (fun () ->
        match close () with
        | observation -> observation
        | exception exn -> raised exn (Stdlib.Printexc.get_raw_backtrace ()))
  }
;;

let legacy close =
  reported (fun () ->
    close ();
    Close_observation.create (Returned ()) ~diagnostics:[] ())
;;

let run_reported t = t.close ()
let run t = Close_observation.resume (run_reported t)
