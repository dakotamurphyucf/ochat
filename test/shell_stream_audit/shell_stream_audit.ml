open! Core

let run env =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
    Filter_cases.run ();
    Executor_cases.run env;
    Tool_cases.run env);
  let sexp = [%sexp (("shell-stream-audit", "PASS") : string * string)] in
  Eio.Flow.copy_string (Sexp.to_string_hum sexp ^ "\n") (Eio.Stdenv.stdout env)
;;

let () =
  Eio_main.run (fun env ->
    match Array.to_list (Sys.get_argv ()) with
    | [ _; "--child"; root; mode ] ->
      Executor_cases.child env Eio.Path.(Eio.Stdenv.fs env / root) mode
    | [ _ ] -> run env
    | _ -> failwith "usage: shell_stream_audit")
;;
