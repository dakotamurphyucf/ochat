open Core
module Action = Eio_unix.Private.Fork_action
module Fd = Eio_unix.Fd
module Lock = Stdlib.Mutex

type setup =
  { limits : (int * int) list
  ; close_extra_fds : bool
  ; owned_group : bool
  }

external fork
  :  Caml_unix.file_descr
  -> (Caml_unix.file_descr
     * Caml_unix.file_descr
     * (int * int) list
     * Caml_unix.file_descr
     * Caml_unix.file_descr)
       option
  -> Action.c_action list
  -> int
  = "ochat_spawn"

external setup_action : unit -> Action.fork_fn = "ochat_spawn_setup_action"
external group_supported : unit -> bool = "ochat_owned_group_supported"
external exited_without_reaping : int -> bool = "ochat_child_exited_without_reaping"
external normalize_signal : int -> int = "ochat_normalize_signal" [@@noalloc]

let setup_action = setup_action ()

let action setup =
  Action.
    { run =
        (fun k ->
          k
            (Obj.repr
               (setup_action, setup.limits, setup.close_extra_fds, setup.owned_group)))
    }
;;

module Anchor = struct
  type stopped =
    | No_command
    | Group_signalled
    | Stop_failed

  type stage =
    | Preparing
    | Started
    | Stopped of stopped

  type failure =
    | Unsupported
    | Lost
    | Unexpected of exn * Stdlib.Printexc.raw_backtrace

  type t =
    { status : Eio_unix.source_ty Eio.Resource.t
    ; control : Eio_unix.sink_ty Eio.Resource.t
    ; mutable stage : stage
    ; mutable failure : failure option
    }

  let fail t failure = if Option.is_none t.failure then t.failure <- Some failure
end

type ownership =
  | Leader_only
  | Anchored of Anchor.t

type child =
  { pid : int
  ; exit_status : Caml_unix.process_status Eio.Promise.t
  ; lock : Lock.t
  ; ownership : ownership
  }

let with_lock lock f =
  Lock.lock lock;
  Exn.protect ~finally:(fun () -> Lock.unlock lock) ~f
;;

let rec kill pid signal =
  match Caml_unix.kill pid signal with
  | () -> ()
  | exception Caml_unix.Unix_error (EINTR, _, _) -> kill pid signal
;;

let kill_leader child anchor =
  match kill child.pid Stdlib.Sys.sigkill with
  | () -> ()
  | exception Caml_unix.Unix_error (ESRCH, _, _) -> ()
  | exception Caml_unix.Unix_error _ -> Anchor.fail anchor Lost
;;

let stop_locked child anchor =
  match anchor.Anchor.stage with
  | Stopped (No_command | Group_signalled | Stop_failed) -> ()
  | Preparing ->
    (* The command cannot exist until Go. Positive-PID cleanup is sufficient. *)
    anchor.stage <- Stopped No_command;
    kill_leader child anchor
  | Started ->
    (match kill (-child.pid) Stdlib.Sys.sigkill with
     | () -> anchor.stage <- Stopped Group_signalled
     | exception Caml_unix.Unix_error _ ->
       (* Permission failure/ESRCH is not evidence that helpers stopped. Keep
          the identity until best-effort direct termination/reaping completes. *)
       Anchor.fail anchor Lost;
       anchor.stage <- Stopped Stop_failed;
       kill_leader child anchor)
;;

let signal child signal =
  with_lock child.lock (fun () ->
    if not (Eio.Promise.is_resolved child.exit_status)
    then (
      match child.ownership with
      | Leader_only -> Caml_unix.kill child.pid signal
      | Anchored anchor -> stop_locked child anchor))
;;

let rec waitpid pid =
  match Caml_unix.waitpid [ WNOHANG ] pid with
  | result -> result
  | exception Caml_unix.Unix_error (EINTR, _, _) -> waitpid pid
;;

let anchor_exited child anchor =
  match exited_without_reaping child.pid with
  | false -> false
  | true ->
    (match anchor.Anchor.stage with
     | Preparing | Started -> Anchor.fail anchor Lost
     | Stopped (No_command | Group_signalled | Stop_failed) -> ());
    stop_locked child anchor;
    true
  | exception Caml_unix.Unix_error ((ENOSYS | EINVAL), _, _) ->
    Anchor.fail anchor Unsupported;
    stop_locked child anchor;
    true
  | exception exn ->
    Anchor.fail anchor (Unexpected (exn, Stdlib.Printexc.get_raw_backtrace ()));
    stop_locked child anchor;
    true
;;

let reap child resolver =
  Eio.Condition.loop_no_mutex Eio_unix.Process.sigchld (fun () ->
    with_lock child.lock (fun () ->
      if Eio.Promise.is_resolved child.exit_status
      then Some ()
      else (
        let ready =
          match child.ownership with
          | Leader_only -> true
          | Anchored anchor -> anchor_exited child anchor
        in
        match if ready then waitpid child.pid else 0, Caml_unix.WEXITED 0 with
        | 0, _ -> None
        | _, status ->
          Eio.Promise.resolve resolver status;
          Some ())))
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

(* Waiters share Eio's signal condition and reap only their own direct PID. *)
let signal_handler_lock = Lock.create ()
let signal_handler = lazy (Eio_unix.Process.install_sigchld_handler ())

type ports =
  { status_r : Eio_unix.source_ty Eio.Resource.t
  ; status_w : Eio_unix.sink_ty Eio.Resource.t
  ; control_r : Eio_unix.source_ty Eio.Resource.t
  ; control_w : Eio_unix.sink_ty Eio.Resource.t
  ; limits : (int * int) list
  }

type spawn_error =
  | Exec_failed of string
  | Protocol_failed

let read_frame source =
  let bytes = Cstruct.create Process_control.frame_bytes in
  match Eio.Flow.read_exact source bytes with
  | () -> Process_control.decode (Cstruct.to_string bytes)
  | exception End_of_file -> None
;;

let start_anchor child anchor =
  match read_frame anchor.Anchor.status with
  | Some Ready ->
    let started =
      with_lock child.lock (fun () ->
        match anchor.stage with
        | Preparing when not (Eio.Promise.is_resolved child.exit_status) ->
          anchor.stage <- Started;
          true
        | Preparing | Started | Stopped (No_command | Group_signalled | Stop_failed) ->
          false)
    in
    if started
    then (
      (* Record command possibility before this cooperative write, never hold
         the ownership mutex across Eio I/O or a callback. *)
      Eio.Flow.write anchor.control [ Cstruct.of_string "G" ];
      Ok ())
    else Error Protocol_failed
  | Some Failed -> Error (Exec_failed "anchor setup failed")
  | Some (Exited _ | Signaled _) | None ->
    with_lock child.lock (fun () -> Anchor.fail anchor Lost);
    Error Protocol_failed
;;

let spawn ~sw ~owned actions =
  Eio.Switch.run (fun errors_sw ->
    let errors_r, errors_w = Eio_unix.pipe errors_sw in
    Action.with_actions actions (fun actions ->
      Eio.Switch.check sw;
      let exit_status, resolver = Eio.Promise.create () in
      let ownership =
        match owned with
        | None -> Leader_only
        | Some ports ->
          Anchored
            { status = ports.status_r
            ; control = ports.control_w
            ; stage = Preparing
            ; failure = None
            }
      in
      let lock = Lock.create () in
      let child, hook =
        Eio.Cancel.protect (fun () ->
          let pid =
            Fd.use_exn "spawn" (Eio_unix.Resource.fd errors_w) (fun errors ->
              match owned with
              | None -> fork errors None actions
              | Some ports ->
                Fd.use_exn
                  "anchor status"
                  (Eio_unix.Resource.fd ports.status_w)
                  (fun status ->
                     Fd.use_exn
                       "anchor control"
                       (Eio_unix.Resource.fd ports.control_r)
                       (fun control ->
                          Fd.use_exn
                            "anchor parent status"
                            (Eio_unix.Resource.fd ports.status_r)
                            (fun status_parent ->
                               Fd.use_exn
                                 "anchor parent control"
                                 (Eio_unix.Resource.fd ports.control_w)
                                 (fun control_parent ->
                                    fork
                                      errors
                                      (Some
                                         ( status
                                         , control
                                         , ports.limits
                                         , status_parent
                                         , control_parent ))
                                      actions)))))
          in
          let child = { pid; exit_status; lock; ownership } in
          (* Install ownership before any cooperative close/read after fork. *)
          let hook =
            Eio.Switch.on_release_cancellable sw (fun () ->
              signal child Stdlib.Sys.sigkill;
              if not (Eio.Promise.is_resolved exit_status) then reap child resolver)
          in
          child, hook)
      in
      Eio.Flow.close errors_w;
      (match owned with
       | None -> ()
       | Some ports ->
         Eio.Flow.close ports.status_w;
         Eio.Flow.close ports.control_r);
      Eio.Fiber.fork_daemon ~sw (fun () ->
        reap child resolver;
        Eio.Switch.remove_hook hook;
        `Stop_daemon);
      let started =
        match ownership with
        | Leader_only -> Ok ()
        | Anchored anchor -> start_anchor child anchor
      in
      match started with
      | Error error -> Error error
      | Ok () ->
        let error =
          Eio.Buf_read.of_flow errors_r ~max_size:4096 |> Eio.Buf_read.take_all
        in
        if String.is_empty error then Ok child else Error (Exec_failed error)))
;;

let legacy_process = function
  | Ok child -> process child
  | Error (Exec_failed error) -> failwith error
  | Error Protocol_failed -> assert false
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
      | None -> spawn ~sw ~owned:None actions |> legacy_process
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
          spawn ~sw ~owned:None (Action.fchdir fd :: actions) |> legacy_process)
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
    ( { limits; close_extra_fds; owned_group = false }
    , Eio_unix.Process.Pi.mgr_unix (module Manager) )
;;

module Owned_group = struct
  module Limits = struct
    type t = (int * int) list

    let create ~cpu_seconds ~memory_bytes ~file_size_bytes ~open_files =
      if cpu_seconds <= 0 || memory_bytes <= 0 || file_size_bytes < 0 || open_files < 3
      then Or_error.error_string "invalid child resource limits"
      else Ok [ 0, cpu_seconds; 1, memory_bytes; 2, file_size_bytes; 3, open_files; 4, 0 ]
    ;;
  end

  type error =
    | Unsupported_platform
    | Spawn_failed
    | Process_failed
  [@@deriving equal, sexp_of]

  type t = child

  let stop child = signal child Stdlib.Sys.sigkill

  let failure child anchor =
    with_lock child.lock (fun () ->
      match anchor.Anchor.failure with
      | None -> None
      | Some Unsupported -> Some Unsupported_platform
      | Some Lost -> Some Process_failed
      | Some (Unexpected (exn, backtrace)) ->
        Stdlib.Printexc.raise_with_backtrace exn backtrace)
  ;;

  let await child =
    match child.ownership with
    | Leader_only -> assert false
    | Anchored anchor ->
      let terminal =
        match read_frame anchor.status with
        | Some (Exited code) -> Ok (`Exited code)
        | Some (Signaled signal) -> Ok (`Signaled (normalize_signal signal))
        | Some (Ready | Failed) | None -> Error Process_failed
      in
      stop child;
      ignore (Eio.Promise.await child.exit_status : Caml_unix.process_status);
      (match failure child anchor with
       | Some error -> Error error
       | None ->
         (match terminal with
          | Error error -> Error error
          | Ok status ->
            (* The anchor was reaped; no writer may retain this private endpoint.
              One bounded lookahead rejects unexpected data instead of accepting
              a valid prefix followed by an unvalidated frame. *)
            let extra = Cstruct.create 1 in
            (match Eio.Flow.single_read anchor.status extra with
             | _ -> Error Process_failed
             | exception End_of_file -> Ok status)))
  ;;

  let spawn ~sw ~limits ~cwd ~executable ~argv ~env ~stdin ~stdout ~stderr =
    if not (group_supported ())
    then Error Unsupported_platform
    else (
      with_lock signal_handler_lock (fun () -> Lazy.force signal_handler);
      let status_r, status_w = Eio_unix.pipe sw in
      let control_r, control_w = Eio_unix.pipe sw in
      let owned = Some { status_r; status_w; control_r; control_w; limits } in
      let setup = { limits; close_extra_fds = true; owned_group = true } in
      let actions =
        [ Action.fchdir cwd
        ; Action.inherit_fds
            [ 0, stdin, `Blocking; 1, stdout, `Blocking; 2, stderr, `Blocking ]
        ; action setup
        ; Action.execve executable ~argv ~env
        ]
      in
      match spawn ~sw ~owned actions with
      | Ok child -> Ok child
      | Error (Exec_failed _) -> Error Spawn_failed
      | Error Protocol_failed -> Error Process_failed)
  ;;
end
