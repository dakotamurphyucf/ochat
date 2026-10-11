open Core

module Provider = struct
  type t = Eio.Domain_manager.ty Eio.Resource.t * (unit -> unit) * (unit -> unit)

  let run (Eio.Resource.T (state, operations), before, finally) f =
    let module Actual = (val Eio.Resource.get operations Eio.Domain_manager.Pi.Mgr) in
    Actual.run state (fun ~cancelled ->
      (* Backtrace recording is domain-local; enable it only for this fresh
         test-owned worker so transport assertions have an original trace. *)
      Stdlib.Printexc.record_backtrace true;
      Exn.protect ~finally ~f:(fun () ->
        Eio.Fiber.first
          (fun () -> raise (Eio.Promise.await cancelled))
          (fun () ->
             before ();
             f ~cancelled)))
  ;;

  let run_raw (manager, _, _) f = Eio.Domain_manager.run_raw manager f
end

let environment (env : Eio_unix.Stdenv.base) ~before ~finally =
  let manager =
    Eio.Resource.T
      ((env#domain_mgr, before, finally), Eio.Domain_manager.Pi.mgr (module Provider))
  in
  object
    method fs = env#fs
    method cwd = env#cwd
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method domain_mgr = manager
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;
