open Core
module Action = Eio_unix.Private.Fork_action
module Fd = Eio_unix.Fd
module Control = Shell_access__Process_control

(* Exercise the actual native Ready/Go transport independently of the production
   owner's lifecycle. This private bridge is the same rooted fork entry; it adds
   no production callback, command, instrumentation or executable authority. *)
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

external virtual_bytes : unit -> int = "ochat_test_virtual_bytes"

type phase =
  | Before_ready
  | Ready_without_go
  | Running

let exact source size =
  let bytes = Cstruct.create size in
  Eio.Flow.read_exact source bytes;
  Cstruct.to_string bytes
;;

let eof source =
  match Eio.Flow.single_read source (Cstruct.create 1) with
  | _ -> assert false
  | exception End_of_file -> ()
;;

let with_native_anchor phase =
  Eio_main.run (fun env ->
    Eio_unix.Process.install_sigchld_handler ();
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Switch.run (fun sw ->
        let errors_r, errors_w = Eio_unix.pipe sw in
        let status_r, status_w = Eio_unix.pipe sw in
        let control_r, control_w = Eio_unix.pipe sw in
        let input_r, input_w = Eio_unix.pipe sw in
        let output_r, output_w = Eio_unix.pipe sw in
        let discarded_r, discarded_w = Eio_unix.pipe sw in
        let directory = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.cwd env / ".") in
        (match (Eio.File.stat directory).kind with
         | `Directory -> ()
         | _ -> failwith "native gate fixture requires a directory");
        let cwd =
          match Eio_unix.Resource.fd_opt directory with
          | Some fd -> fd
          | None -> failwith "native gate fixture requires a retained directory FD"
        in
        let executable =
          Eio.Path.native_exn Eio.Path.(Eio.Stdenv.cwd env / "native_fixture.exe")
          |> Eio_posix.Low_level.realpath
        in
        let actions =
          [ Action.fchdir cwd
          ; Action.inherit_fds
              [ 0, Eio_unix.Resource.fd input_r, `Blocking
              ; 1, Eio_unix.Resource.fd output_w, `Blocking
              ; 2, Eio_unix.Resource.fd discarded_w, `Blocking
              ]
          ; Action.execve executable ~argv:[| executable; "gate-ready" |] ~env:[||]
          ]
        in
        let memory = virtual_bytes () in
        let headroom = 256 * 1024 * 1024 in
        assert (memory <= Int.max_value - headroom);
        let limits = [ 0, 2; 1, memory + headroom; 2, 1_048_576; 3, 64; 4, 0 ] in
        let use resource f =
          Fd.use_exn "native gate fixture" (Eio_unix.Resource.fd resource) f
        in
        let pid =
          Action.with_actions actions (fun actions ->
            Eio.Cancel.protect (fun () ->
              use errors_w (fun errors ->
                use status_w (fun status ->
                  use control_r (fun control ->
                    use status_r (fun parent_status ->
                      use control_w (fun parent_control ->
                        fork
                          errors
                          (Some (status, control, limits, parent_status, parent_control))
                          actions)))))))
        in
        let reaped = ref None in
        let launched = ref false in
        let rec poll () =
          match Caml_unix.waitpid [ WNOHANG ] pid with
          | 0, _ -> None
          | owned, status ->
            assert (owned = pid);
            reaped := Some status;
            Some status
          | exception Caml_unix.Unix_error (EINTR, _, _) -> poll ()
        in
        let await () =
          match !reaped with
          | Some status -> status
          | None -> Eio.Condition.loop_no_mutex Eio_unix.Process.sigchld poll
        in
        Eio.Switch.on_release sw (fun () ->
          if Option.is_none !reaped
          then
            Exn.protect
              ~f:(fun () ->
                Caml_unix.kill (if !launched then -pid else pid) Stdlib.Sys.sigkill)
              ~finally:(fun () -> ignore (await () : Caml_unix.process_status)));
        List.iter [ errors_w; status_w; output_w; discarded_w ] ~f:Eio.Flow.close;
        Eio.Flow.close control_r;
        Eio.Flow.close input_r;
        Eio.Flow.close input_w;
        (match phase with
         | Before_ready ->
           Eio.Flow.close status_r;
           Eio.Flow.close control_w
         | Ready_without_go | Running ->
           assert (
             Option.equal Control.equal (Control.decode (exact status_r 16)) (Some Ready));
           (match phase with
            | Before_ready -> assert false
            | Ready_without_go -> ()
            | Running ->
              launched := true;
              Eio.Flow.write control_w [ Cstruct.of_string "G" ];
              assert (String.equal (exact output_r 14) "command-ready\n"));
           Eio.Flow.close control_w);
        (* Owner EOF must terminate the live anchor, including while the command
           runs. EOF proves no trusted fixture writer remains holding these pipes. *)
        eof output_r;
        eof discarded_r;
        eof errors_r;
        (match phase with
         | Before_ready -> ()
         | Ready_without_go | Running -> eof status_r);
        match await () with
        | WSIGNALED signal -> assert (signal = Stdlib.Sys.sigkill)
        | WEXITED _ | WSTOPPED _ -> assert false)))
;;

let%test_unit "native owner loss before Ready, before Go and during execution joins" =
  List.iter [ Before_ready; Ready_without_go; Running ] ~f:with_native_anchor
;;
