(* In-process delay keeps cancellation qualification free of orphanable shell
   subprocesses. Admission must terminate this worker at its own deadline. *)
let () =
  Unix.sleepf 0.6;
  Chatml_compilation.worker_main ()
;;
