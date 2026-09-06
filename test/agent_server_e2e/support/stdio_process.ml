open Core

type stdout_item =
  | Envelope of Agent_protocol.Envelope.t
  | Invalid_line of
      { line : string
      ; error : Agent_protocol.Error.t
      }
  | Read_error of string
  | End_of_file
[@@deriving sexp]

type capture =
  { buffer : Buffer.t
  ; limit : int
  ; mutable bytes_seen : int
  }

type t =
  { process : Eio_unix.Process.ty Eio.Process.t
  ; stdin : [ Eio.Flow.sink_ty | `Close ] Eio.Resource.t
  ; stdout_items : stdout_item Eio.Stream.t
  ; stdout_error : string option ref
  ; stdout_capture : capture
  ; stderr_capture : capture
  ; stdout_done : Process_reader.t
  ; stderr_done : Process_reader.t
  ; drain : unit -> unit
  ; exit_status : Eio.Process.exit_status Eio.Promise.t
  ; mutable stdin_closed : bool
  ; mutable result : Process_manager.result option
  }

let create_capture limit = { buffer = Buffer.create limit; limit; bytes_seen = 0 }

let add_string capture value =
  capture.bytes_seen <- capture.bytes_seen + String.length value;
  let available = capture.limit - Buffer.length capture.buffer in
  if available > 0
  then
    Buffer.add_substring
      capture.buffer
      value
      ~pos:0
      ~len:(Int.min available (String.length value))
;;

let add_bytes capture scratch count =
  capture.bytes_seen <- capture.bytes_seen + count;
  let available = capture.limit - Buffer.length capture.buffer in
  if available > 0
  then
    Buffer.add_string
      capture.buffer
      (Cstruct.to_string (Cstruct.sub scratch 0 (Int.min count available)))
;;

let decode_line line =
  Result.try_with (fun () -> Jsonaf.of_string line)
  |> Result.map_error ~f:(fun exn ->
    Agent_protocol.Error.invalid_request ("invalid stdout JSON: " ^ Exn.to_string exn))
  |> Result.bind ~f:Agent_protocol.Envelope.of_json
;;

let report_error items error message =
  if Option.is_none !error
  then (
    error := Some message;
    if Eio.Stream.length items < 1_024 then Eio.Stream.add items (Read_error message))
;;

let publish_line items error line =
  let item =
    match decode_line line with
    | Ok envelope -> Envelope envelope
    | Error error -> Invalid_line { line; error }
  in
  if Option.is_none !error
  then
    if Eio.Stream.length items < 1_024
    then Eio.Stream.add items item
    else report_error items error "stdout notification queue exceeded 1024 items"
;;

let publish_complete_lines items error pending chunk =
  let lines = String.split (!pending ^ chunk) ~on:'\n' in
  let complete, remainder = List.split_n lines (List.length lines - 1) in
  List.iter complete ~f:(fun line ->
    if String.length line > 1_048_576
    then report_error items error "stdout line exceeded 1048576 bytes"
    else publish_line items error line);
  let remainder = List.hd_exn remainder in
  if String.length remainder > 1_048_576
  then report_error items error "stdout line exceeded 1048576 bytes";
  pending := if Option.is_some !error then "" else remainder
;;

let read_stdout source items error capture =
  let scratch = Cstruct.create 4_096 in
  let pending = ref "" in
  let rec loop () =
    match Eio.Flow.single_read source scratch with
    | count ->
      let chunk = Cstruct.to_string (Cstruct.sub scratch 0 count) in
      add_string capture chunk;
      if Option.is_none !error then publish_complete_lines items error pending chunk;
      loop ()
    | exception End_of_file ->
      if not (String.is_empty !pending) then publish_line items error !pending;
      if Eio.Stream.length items < 1_024 then Eio.Stream.add items End_of_file
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception exn -> report_error items error (Exn.to_string exn)
  in
  loop ()
;;

let read_stderr source capture =
  let scratch = Cstruct.create 4_096 in
  let rec loop () =
    match Eio.Flow.single_read source scratch with
    | count ->
      add_bytes capture scratch count;
      loop ()
    | exception End_of_file -> ()
  in
  loop ()
;;

let fork_waiter ~sw process =
  let status, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
    try Eio.Process.await process |> Eio.Promise.resolve resolver with
    | Eio.Cancel.Cancelled _ -> Eio.Process.signal process Stdlib.Sys.sigkill);
  status
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
  let stdin_source, stdin = Eio.Process.pipe ~sw manager in
  let stdout, stdout_sink = Eio.Process.pipe ~sw manager in
  let stderr, stderr_sink = Eio.Process.pipe ~sw manager in
  let process =
    spawn_process
      ~sw
      ~env
      ?cwd
      ?environment
      ~stdin:stdin_source
      ~stdout:stdout_sink
      ~stderr:stderr_sink
      argv
  in
  Eio.Flow.close stdin_source;
  Eio.Flow.close stdout_sink;
  Eio.Flow.close stderr_sink;
  let stdout_items = Eio.Stream.create 1_024 in
  let stdout_error = ref None in
  let stdout_capture = create_capture max_output_bytes in
  let stderr_capture = create_capture max_output_bytes in
  let stdout_done =
    Process_reader.start ~sw (fun () ->
      read_stdout stdout stdout_items stdout_error stdout_capture)
  in
  let stderr_done =
    Process_reader.start ~sw (fun () -> read_stderr stderr stderr_capture)
  in
  let drain () =
    Process_reader.drain ~clock:(Eio.Stdenv.clock env) [ stdout_done; stderr_done ]
  in
  let exit_status = fork_waiter ~sw process in
  { process
  ; stdin
  ; stdout_items
  ; stdout_error
  ; stdout_capture
  ; stderr_capture
  ; stdout_done
  ; stderr_done
  ; drain
  ; exit_status
  ; stdin_closed = false
  ; result = None
  }
;;

let send_line t line =
  if t.stdin_closed then invalid_arg "stdio process stdin is closed";
  Eio.Flow.copy_string (line ^ "\n") t.stdin
;;

let pid t = Eio.Process.pid t.process

let send_string t value =
  if t.stdin_closed then invalid_arg "stdio process stdin is closed";
  Eio.Flow.copy_string value t.stdin
;;

let close_stdin t =
  if not t.stdin_closed
  then (
    t.stdin_closed <- true;
    Eio.Flow.close t.stdin)
;;

let next_stdout t ~clock ~timeout_seconds =
  let take () =
    match !(t.stdout_error) with
    | Some message -> Read_error message
    | None when Process_reader.interrupted t.stdout_done ->
      Read_error "stdout drainage interrupted"
    | None ->
      (match Eio.Stream.take_nonblocking t.stdout_items with
       | Some item -> item
       | None when Process_reader.finished t.stdout_done -> End_of_file
       | None -> Eio.Stream.take t.stdout_items)
  in
  match Eio.Time.with_timeout clock timeout_seconds (fun () -> Ok (take ())) with
  | Ok item -> `Item item
  | Error `Timeout -> `Timeout
;;

let output capture =
  Process_manager.
    { contents = Buffer.contents capture.buffer
    ; bytes_seen = capture.bytes_seen
    ; truncated = capture.bytes_seen > capture.limit
    }
;;

let captured capture reader =
  let output = output capture in
  Process_manager.
    { output with truncated = output.truncated || Process_reader.interrupted reader }
;;

let stdout t =
  let output = captured t.stdout_capture t.stdout_done in
  Process_manager.
    { output with truncated = output.truncated || Option.is_some !(t.stdout_error) }
;;

let stderr t = captured t.stderr_capture t.stderr_done

let exit = function
  | `Exited code -> Process_manager.Exited code
  | `Signaled signal -> Process_manager.Signaled signal
;;

let await_uncached t =
  let status = Eio.Promise.await t.exit_status in
  t.drain ();
  let stdout_output = stdout t in
  let stderr_output = stderr t in
  Process_manager.{ exit = exit status; stdout = stdout_output; stderr = stderr_output }
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
  close_stdin t;
  Eio.Process.signal t.process Stdlib.Sys.sigterm;
  match
    Eio.Time.with_timeout clock grace_seconds (fun () ->
      Ok (Eio.Promise.await t.exit_status))
  with
  | Ok _ ->
    let result = await t in
    Process_manager.{ forced = false; result }
  | Error `Timeout ->
    Eio.Process.signal t.process Stdlib.Sys.sigkill;
    let result = Eio.Time.with_timeout_exn clock 2. (fun () -> await t) in
    Process_manager.{ forced = true; result }
;;
