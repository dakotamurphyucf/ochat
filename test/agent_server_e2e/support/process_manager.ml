open Core

type exit =
  | Exited of int
  | Signaled of int
[@@deriving equal, sexp]

type output =
  { contents : string
  ; bytes_seen : int
  ; truncated : bool
  }
[@@deriving sexp]

type result =
  { exit : exit
  ; stdout : output
  ; stderr : output
  }
[@@deriving sexp]

type termination =
  { forced : bool
  ; result : result
  }
[@@deriving sexp]

type readiness_error =
  | Timeout
  | Exited_before_ready of result
[@@deriving sexp]

type capture =
  { buffer : Buffer.t
  ; limit : int
  ; mutable bytes_seen : int
  }

type t =
  { process : Eio_unix.Process.ty Eio.Process.t
  ; stdout_capture : capture
  ; stderr_capture : capture
  ; stdout_done : Process_reader.t
  ; stderr_done : Process_reader.t
  ; drain : unit -> unit
  ; exit_status : Eio.Process.exit_status Eio.Promise.t
  ; mutable result : result option
  }

let create_capture limit = { buffer = Buffer.create limit; limit; bytes_seen = 0 }

let add capture scratch count =
  capture.bytes_seen <- capture.bytes_seen + count;
  let available = capture.limit - Buffer.length capture.buffer in
  if available > 0
  then
    Buffer.add_string
      capture.buffer
      (Cstruct.to_string (Cstruct.sub scratch 0 (Int.min count available)))
;;

let read_capture source capture =
  let scratch = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read source scratch with
    | count ->
      add capture scratch count;
      loop ()
    | exception End_of_file -> ()
  in
  loop ()
;;

let fork_reader ~sw source capture =
  Process_reader.start ~sw (fun () -> read_capture source capture)
;;

let fork_waiter ~sw process =
  let status, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
    try Eio.Process.await process |> Eio.Promise.resolve resolver with
    | Eio.Cancel.Cancelled _ -> Eio.Process.signal process Stdlib.Sys.sigkill);
  status, resolver
;;

let spawn_process ~sw ~env ?cwd ?environment ~stdin ~stdout ~stderr argv =
  Eio.Process.spawn
    ~sw
    (Eio.Stdenv.process_mgr env)
    ?cwd
    ?env:environment
    ~stdin
    ~stdout
    ~stderr
    argv
;;

let spawn ~sw ~env ?cwd ?environment ~max_output_bytes argv =
  if max_output_bytes < 0 then invalid_arg "max_output_bytes must be nonnegative";
  let manager = Eio.Stdenv.process_mgr env in
  let stdout_source, stdout_sink = Eio.Process.pipe ~sw manager in
  let stderr_source, stderr_sink = Eio.Process.pipe ~sw manager in
  let process =
    spawn_process
      ~sw
      ~env
      ?cwd
      ?environment
      ~stdin:(Eio.Flow.string_source "")
      ~stdout:stdout_sink
      ~stderr:stderr_sink
      argv
  in
  Eio.Flow.close stdout_sink;
  Eio.Flow.close stderr_sink;
  let stdout_capture = create_capture max_output_bytes in
  let stderr_capture = create_capture max_output_bytes in
  let stdout_done = fork_reader ~sw stdout_source stdout_capture in
  let stderr_done = fork_reader ~sw stderr_source stderr_capture in
  let drain () =
    Process_reader.drain ~clock:(Eio.Stdenv.clock env) [ stdout_done; stderr_done ]
  in
  let exit_status, _exit_resolver = fork_waiter ~sw process in
  { process
  ; stdout_capture
  ; stderr_capture
  ; stdout_done
  ; stderr_done
  ; drain
  ; exit_status
  ; result = None
  }
;;

let pid t = Eio.Process.pid t.process
let signal t signal = Eio.Process.signal t.process signal

let output capture =
  { contents = Buffer.contents capture.buffer
  ; bytes_seen = capture.bytes_seen
  ; truncated = capture.bytes_seen > capture.limit
  }
;;

let captured capture reader =
  let output = output capture in
  { output with truncated = output.truncated || Process_reader.interrupted reader }
;;

let stdout t = captured t.stdout_capture t.stdout_done
let stderr t = captured t.stderr_capture t.stderr_done

let exit = function
  | `Exited code -> Exited code
  | `Signaled signal -> Signaled signal
;;

let await_uncached t =
  let status = Eio.Promise.await t.exit_status in
  t.drain ();
  { exit = exit status; stdout = stdout t; stderr = stderr t }
;;

let await t =
  match t.result with
  | Some result -> result
  | None ->
    let result = await_uncached t in
    t.result <- Some result;
    result
;;

let poll_result t =
  match t.result, Eio.Promise.peek t.exit_status with
  | Some result, _ -> Some result
  | None, None -> None
  | None, Some _ ->
    if Process_reader.finished t.stdout_done && Process_reader.finished t.stderr_done
    then Some (await t)
    else None
;;

let terminate t ~clock ~grace_seconds =
  signal t Stdlib.Sys.sigterm;
  match
    Eio.Time.with_timeout clock grace_seconds (fun () ->
      Ok (Eio.Promise.await t.exit_status))
  with
  | Ok _ -> { forced = false; result = await t }
  | Error `Timeout ->
    signal t Stdlib.Sys.sigkill;
    { forced = true; result = Eio.Time.with_timeout_exn clock 2. (fun () -> await t) }
;;

let ready_or_exited t ~ready =
  if ready (stdout t).contents
  then Some (Ok ())
  else (
    match Eio.Promise.peek t.exit_status with
    | None -> None
    | Some _ -> Some (Error (Exited_before_ready (await t))))
;;

let wait_for_stdout t ~clock ~timeout_seconds ~ready =
  let rec wait () =
    match ready_or_exited t ~ready with
    | Some result -> Ok result
    | None ->
      Eio.Time.sleep clock 0.01;
      wait ()
  in
  match Eio.Time.with_timeout clock timeout_seconds wait with
  | Ok result -> result
  | Error `Timeout -> Error Timeout
;;
