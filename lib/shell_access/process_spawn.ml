open Core
module Action = Eio_unix.Private.Fork_action
module Fd = Eio_unix.Fd
module Lock = Stdlib.Mutex

type setup =
  { limits : (int * int) list
  ; close_extra_fds : bool
  }

external fork : Caml_unix.file_descr -> Action.c_action list -> int = "ochat_spawn"
external setup_action : unit -> Action.fork_fn = "ochat_spawn_setup_action"

let setup_action = setup_action ()

let action setup =
  Action.
    { run = (fun k -> k (Obj.repr (setup_action, setup.limits, setup.close_extra_fds))) }
;;

type child =
  { pid : int
  ; exit_status : Caml_unix.process_status Eio.Promise.t
  ; lock : Lock.t
  }

let with_lock lock f =
  Lock.lock lock;
  Exn.protect ~finally:(fun () -> Lock.unlock lock) ~f
;;

let signal child signal =
  with_lock child.lock (fun () ->
    match Eio.Promise.is_resolved child.exit_status with
    | true -> ()
    | false -> Caml_unix.kill child.pid signal)
;;

let rec waitpid pid =
  match Caml_unix.waitpid [ WNOHANG ] pid with
  | result -> result
  | exception Caml_unix.Unix_error (EINTR, _, _) -> waitpid pid
;;

let reap child resolver =
  Eio.Condition.loop_no_mutex Eio_unix.Process.sigchld (fun () ->
    with_lock child.lock (fun () ->
      match waitpid child.pid with
      | 0, _ -> None
      | _, status ->
        Eio.Promise.resolve resolver status;
        Some ()))
;;

module Process = struct
  type t = child

  type tag =
    [ `Generic
    | `Unix
    ]

  let pid child = child.pid
  let signal = signal

  let await child =
    match Eio.Promise.await child.exit_status with
    | WEXITED code -> `Exited code
    | WSIGNALED signal -> `Signaled signal
    | WSTOPPED _ -> assert false
  ;;
end

let process child = Eio.Resource.T (child, Eio.Process.Pi.process (module Process))

(* Use Eio's shared signal condition on both the POSIX and Linux schedulers.
   The installed handler cooperates with Eio rather than consuming other child
   statuses: every waiter reaps only the PID it owns. *)
let signal_handler_lock = Lock.create ()
let signal_handler = lazy (Eio_unix.Process.install_sigchld_handler ())

let spawn ~sw actions =
  Eio.Switch.run (fun errors_sw ->
    let errors_r, errors_w = Eio_unix.pipe errors_sw in
    Action.with_actions actions (fun actions ->
      Eio.Switch.check sw;
      let exit_status, resolver = Eio.Promise.create () in
      let pid =
        Fd.use_exn "spawn" (Eio_unix.Resource.fd errors_w) (fun fd -> fork fd actions)
      in
      Eio.Flow.close errors_w;
      let child = { pid; exit_status; lock = Lock.create () } in
      let hook =
        Eio.Switch.on_release_cancellable sw (fun () ->
          signal child Stdlib.Sys.sigkill;
          match Eio.Promise.is_resolved exit_status with
          | true -> ()
          | false -> reap child resolver)
      in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        reap child resolver;
        Eio.Switch.remove_hook hook;
        `Stop_daemon);
      let error = Eio.Buf_read.of_flow errors_r ~max_size:4096 |> Eio.Buf_read.take_all in
      match error with
      | "" -> process child
      | error -> failwith error))
;;

module Manager = Eio_unix.Process.Make_mgr (struct
    type t = setup

    let spawn_unix setup ~sw ?cwd ~env ~fds ~executable argv =
      List.iter fds ~f:(fun (target, _, _) ->
        match target >= 0 && target <= 4 with
        | true -> ()
        | false ->
          invalid_arg "child setup only supports inherited descriptors 0 through 4");
      let actions =
        [ Action.inherit_fds fds
        ; action setup
        ; Action.execve executable ~argv:(Array.of_list argv) ~env
        ]
      in
      match cwd with
      | None -> spawn ~sw actions
      | Some cwd ->
        Eio.Switch.run (fun cwd_sw ->
          let path = Eio.Path.native_exn cwd in
          let fd =
            Eio.Cancel.protect (fun () ->
              let raw =
                Eio_unix.run_in_systhread (fun () ->
                  Caml_unix.openfile path [ O_RDONLY; O_NONBLOCK; O_CLOEXEC ] 0)
              in
              Fd.of_unix ~sw:cwd_sw ~close_unix:true raw)
          in
          spawn ~sw (Action.fchdir fd :: actions))
    ;;
  end)

let manager ~cpu_seconds ~memory_bytes ~file_size_bytes ~open_files ~close_extra_fds =
  let limits =
    [ 0, cpu_seconds; 1, memory_bytes; 2, file_size_bytes; 3, open_files ]
    |> List.filter_map ~f:(fun (resource, value) ->
      Option.map value ~f:(fun value ->
        match value < 0 with
        | true -> invalid_arg "resource limit must be nonnegative"
        | false -> resource, value))
  in
  with_lock signal_handler_lock (fun () -> Lazy.force signal_handler);
  Eio.Resource.T
    ({ limits; close_extra_fds }, Eio_unix.Process.Pi.mgr_unix (module Manager))
;;
