open Core

type t =
  { master : Eio.File.rw_ty Eio.Resource.t
  ; slave : [ Eio.Flow.source_ty | Eio.Flow.sink_ty | `Unix_fd | `Close ] Eio.Resource.t
  ; process : Eio_unix.Process.ty Eio.Process.t
  ; reader : Process_reader.t
  ; capture : Buffer.t
  ; overflow : bool ref
  ; original : Sexp.t
  }

let with_fd flow f =
  let fd = Eio_unix.Resource.fd_opt flow |> Option.value_exn in
  Eio_unix.Fd.use_exn "E2E PTY" fd (fun raw ->
    Eio_unix.run_in_systhread (fun () -> f raw))
;;

let terminal_state slave =
  with_fd slave (fun fd ->
    Core_unix.Terminal_io.tcgetattr fd |> Core_unix.Terminal_io.sexp_of_t)
;;

module Slave = struct
  type t = Eio_unix.Fd.t

  let read_methods = []

  let single_read fd buffer =
    let bytes = Bytes.create (Cstruct.length buffer) in
    let count = Eio_posix.Low_level.read fd bytes 0 (Bytes.length bytes) in
    if count = 0 then raise End_of_file;
    Cstruct.blit_from_bytes bytes 0 buffer 0 count;
    count
  ;;

  let single_write fd buffers =
    match List.find buffers ~f:(fun buffer -> Cstruct.length buffer > 0) with
    | None -> 0
    | Some buffer ->
      let bytes = Cstruct.to_bytes buffer in
      Eio_posix.Low_level.write fd bytes 0 (Bytes.length bytes)
  ;;

  let copy fd ~src = Eio.Flow.Pi.simple_copy ~single_write fd ~src
end

let open_slave ~sw path =
  let fd =
    Eio_posix.Low_level.openat
      ~sw
      ~mode:0
      Fs
      path
      Eio_posix.Low_level.Open_flags.(rdwr + noctty)
  in
  let handler =
    Eio.Resource.handler
      [ H (Eio.Flow.Pi.Source, (module Slave))
      ; H (Eio.Flow.Pi.Sink, (module Slave))
      ; H (Eio_unix.Resource.T, Fn.id)
      ; H (Eio.Resource.Close, Eio_unix.Fd.close)
      ]
  in
  Eio.Resource.T (fd, handler)
;;

let open_pair ~sw env =
  let master =
    Eio.Path.open_out ~sw ~create:`Never Eio.Path.(Eio.Stdenv.fs env / "/dev/ptmx")
  in
  let slave_name =
    with_fd master (fun fd ->
      (Or_error.ok_exn Unix_pseudo_terminal.grantpt) fd;
      (Or_error.ok_exn Unix_pseudo_terminal.unlockpt) fd;
      (Or_error.ok_exn Unix_pseudo_terminal.ptsname) fd)
  in
  let slave = open_slave ~sw slave_name in
  master, slave
;;

let set_size env slave ~columns ~rows =
  let manager = Eio.Stdenv.process_mgr env in
  Eio.Process.run
    manager
    ~stdin:slave
    [ "/bin/stty"; "rows"; Int.to_string rows; "cols"; Int.to_string columns ];
  let actual =
    Eio.Process.parse_out
      manager
      Eio.Buf_read.take_all
      ~stdin:slave
      [ "/bin/stty"; "size" ]
  in
  if not (String.equal (String.strip actual) (sprintf "%d %d" rows columns))
  then failwith "PTY dimensions differ"
;;

let read master capture overflow =
  let scratch = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read master scratch with
    | count ->
      let remaining = (2 * 1024 * 1024) - Buffer.length capture in
      if count > remaining then overflow := true;
      Buffer.add_string
        capture
        (Cstruct.to_string (Cstruct.sub scratch 0 (Int.min count remaining)));
      loop ()
    | exception End_of_file -> ()
  in
  loop ()
;;

let spawn ~sw ~env ~cwd ~environment ~columns ~rows argv =
  let master, slave = open_pair ~sw env in
  set_size env slave ~columns ~rows;
  let original = terminal_state slave in
  let capture = Buffer.create 8192 in
  let overflow = ref false in
  let reader = Process_reader.start ~sw (fun () -> read master capture overflow) in
  let process =
    Eio.Process.spawn
      ~sw
      (Eio.Stdenv.process_mgr env)
      ~cwd
      ~env:environment
      ~stdin:slave
      ~stdout:slave
      ~stderr:slave
      argv
  in
  { master; slave; process; reader; capture; overflow; original }
;;

let output t = Buffer.contents t.capture
let send t text = Eio.Flow.copy_string text t.master

let await_text t ~clock text =
  let rec wait () =
    if !(t.overflow) then failwith "PTY transcript exceeded capture bound";
    if String.is_substring (output t) ~substring:text
    then ()
    else (
      Eio.Time.sleep clock 0.01;
      wait ())
  in
  try Eio.Time.with_timeout_exn clock 5. wait with
  | Eio.Time.Timeout ->
    raise_s [%sexp "PTY output deadline", (text : string), (output t : string)]
;;

let await_exit t ~clock =
  try Eio.Time.with_timeout_exn clock 5. (fun () -> Eio.Process.await t.process) with
  | Eio.Time.Timeout -> raise_s [%sexp "PTY exit deadline", (output t : string)]
;;

let assert_restored t ~clock =
  List.iter [ "\027[?1049l"; "\027[?25h"; "\027[?2004l" ] ~f:(await_text t ~clock);
  let actual = terminal_state t.slave in
  if not (Sexp.equal t.original actual)
  then
    raise_s
      [%sexp "terminal attributes not restored", (t.original : Sexp.t), (actual : Sexp.t)];
  Process_reader.stop t.reader;
  Eio.Time.with_timeout_exn clock 0.5 (fun () -> Process_reader.await t.reader)
;;
