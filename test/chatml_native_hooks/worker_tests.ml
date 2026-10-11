open Core
module Compiler = Chatml_compilation
module C = Chatml.Chatml_compiler_contract
module B = Chatml.Chatml_builtin_spec

(* Narrow observer around the REAL native domain provider. Its gate stops at the
   worker boundary before running compiler code; cancellation must still join it.
   No clock race, delay, fake compiler or abandoned native domain. *)
module Observed_manager = struct
  type t =
    { manager : Eio.Domain_manager.ty Eio.Resource.t
    ; entered : unit Eio.Promise.u
    ; finished : bool Atomic.t
    }

  let run t f =
    let (Eio.Resource.T (data, ops)) = t.manager in
    let module Native = (val Eio.Resource.get ops Eio.Domain_manager.Pi.Mgr) in
    Native.run data (fun ~cancelled ->
      Exn.protect
        ~finally:(fun () -> Atomic.set t.finished true)
        ~f:(fun () ->
          Eio.Promise.resolve t.entered ();
          (* We deliberately keep the worker alive until actual owner cancellation.
           The immutable compiler closure stays borrowed until this returns. *)
          let _ = f in
          raise (Eio.Promise.await cancelled)))
  ;;

  let run_raw t f = Eio.Domain_manager.run_raw t.manager f
end

exception Owner_cancelled of int ref

let%test_unit "native prepared cancellation preserves reason and joins actual worker" =
  Eio_main.run (fun env ->
    let entered, resolver = Eio.Promise.create () in
    let finished = Atomic.make false in
    let observed =
      Observed_manager.
        { manager = Eio.Stdenv.domain_mgr env; entered = resolver; finished }
    in
    let manager =
      Eio.Resource.T (observed, Eio.Domain_manager.Pi.mgr (module Observed_manager))
    in
    let wrapped : Eio_unix.Stdenv.base =
      object
        method domain_mgr = manager
        method stdin = env#stdin
        method stdout = env#stdout
        method stderr = env#stderr
        method net = env#net
        method process_mgr = env#process_mgr
        method clock = env#clock
        method mono_clock = env#mono_clock
        method fs = env#fs
        method cwd = env#cwd
        method secure_random = env#secure_random
        method debug = env#debug
        method backend_id = env#backend_id
      end
    in
    let contract =
      C.for_static_surface
        (Chatml.Chatml_static_surface.of_runtime_surface
           Chatml.Chatml_builtin_surface.core_surface
           ~checkpoint:Support.checkpoint)
        ~required_bindings:
          [ "initial_state", B.TInt; "hook", B.TFun ([ TInt; TInt ], B.task_ty TInt) ]
    in
    let input =
      Support.input
        contract
        [ ( "entry.chatml"
          , "let initial_state = 0 let hook event state = Task.pure(state)"
          , [] )
        ]
    in
    let identity = ref 17 in
    (match
       Eio.Cancel.sub (fun cancel ->
         Eio.Fiber.both
           (fun () ->
              Eio.Promise.await entered;
              Eio.Cancel.cancel cancel (Owner_cancelled identity))
           (fun () ->
              ignore
                (Compiler.compile_prepared_with_contract
                   input
                   ~contract
                   ~limits:Support.compiler_limits
                   ~max_dependency_depth:16
                   ~env:wrapped
                   ()
                 : (Chatml_linked_program.t, Compiler.error) Result.t)))
     with
     | _ -> assert false
     | exception Eio.Cancel.Cancelled (Owner_cancelled actual) ->
       assert (phys_equal identity actual));
    assert (Atomic.get finished);
    (* Same immutable input remains reusable after joined cleanup. *)
    ignore
      (Compiler.compile_prepared_with_contract
         input
         ~contract
         ~limits:Support.compiler_limits
         ~max_dependency_depth:16
         ~env
         ()
       |> Support.ok
       : Chatml_linked_program.t))
;;
