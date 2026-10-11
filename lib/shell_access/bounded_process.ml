open Core
module Group = Process_spawn.Owned_group

module Error = struct
  type t =
    | Invalid_request
    | Unsupported_platform
    | Spawn_failed
    | Process_failed
    | Io_failed
    | Output_limit
    | Timed_out
  [@@deriving equal, sexp_of]
end

module Limits = struct
  type t =
    { maximum_request_bytes : int
    ; maximum_output_bytes : int
    ; maximum_wall_time : Time_ns.Span.t
    ; kernel : Group.Limits.t
    }

  let create
        ~maximum_request_bytes
        ~maximum_output_bytes
        ~maximum_wall_time
        ~cpu_seconds_per_process
        ~memory_bytes_per_process
        ~file_size_bytes
        ~open_files_per_process
    =
    if
      maximum_request_bytes <= 0
      || maximum_output_bytes < 0
      || Time_ns.Span.( <= ) maximum_wall_time Time_ns.Span.zero
    then Or_error.error_string "invalid invocation limits"
    else
      Group.Limits.create
        ~cpu_seconds:cpu_seconds_per_process
        ~memory_bytes:memory_bytes_per_process
        ~file_size_bytes
        ~open_files:open_files_per_process
      |> Result.map ~f:(fun kernel ->
        { maximum_request_bytes; maximum_output_bytes; maximum_wall_time; kernel })
  ;;
end

module Outcome = struct
  type t =
    { status : Eio.Process.exit_status
    ; stdout : string
    ; stderr_bytes : int
    }

  let status t = t.status
  let stdout t = t.stdout
  let stderr_bytes t = t.stderr_bytes
end

type t =
  { limits : Limits.t
  ; executable : string
  ; arguments : string list
  ; environment : (string * string) list
  ; stdin : string
  }

let create ~limits ~executable ~arguments ~environment ~stdin =
  let remaining = ref limits.Limits.maximum_request_bytes in
  let charge bytes =
    if bytes > !remaining
    then false
    else (
      remaining := !remaining - bytes;
      true)
  in
  let string text =
    charge (String.length text) && charge 1 && not (String.mem text '\000')
  in
  let rec valid_arguments = function
    | [] -> true
    | argument :: rest -> string argument && valid_arguments rest
  in
  let rec valid_environment names = function
    | [] -> true
    | (name, value) :: rest ->
      charge (String.length name)
      && charge (String.length value)
      && charge 2
      && (not (String.mem name '\000'))
      && (not (String.mem value '\000'))
      && (not (String.is_empty name))
      && (not (String.mem name '='))
      && (not (Set.mem names name))
      && valid_environment (Set.add names name) rest
  in
  (* Executable and argv[0] are both charged; env's '=' and NUL terminator count.
     Traversals stop at the first failed budget check, before constructing arrays. *)
  if
    string executable
    && String.is_prefix executable ~prefix:"/"
    && string executable
    && valid_arguments arguments
    && valid_environment (Set.empty (module String)) environment
    && charge (String.length stdin)
  then Ok { limits; executable; arguments; environment; stdin }
  else Error Error.Invalid_request
;;

exception Stop

type failure =
  | Expected of Error.t
  | Unexpected of exn * Stdlib.Printexc.raw_backtrace

let broken_input = function
  | Caml_unix.Unix_error (EPIPE, _, _)
  | Eio.Io (Eio.Net.E (Connection_reset (Eio_unix.Unix_error (EPIPE, _, _))), _) -> true
  | _ -> false
;;

let run t ~cwd ~clock ~checkpoint =
  (* All workers belong to one Eio domain. Record failures at their origin, before
     Switch may combine them, so callbacks retain their actual identity/trace. *)
  let failure = ref None in
  let stop reason =
    if Option.is_none !failure then failure := Some reason;
    raise Stop
  in
  let checkpoint () =
    match checkpoint () with
    | () -> ()
    | exception exn -> stop (Unexpected (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  let guard f =
    match f () with
    | result -> result
    | exception (Stop as exn) -> raise exn
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception (Eio.Io _ | Caml_unix.Unix_error _) -> stop (Expected Io_failed)
    | exception exn -> stop (Unexpected (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  let invoke () =
    guard (fun () ->
      checkpoint ();
      Eio.Switch.run (fun sw ->
        let input_r, input_w = Eio_unix.pipe sw in
        let output_r, output_w = Eio_unix.pipe sw in
        let error_r, error_w = Eio_unix.pipe sw in
        let argv = Array.of_list (t.executable :: t.arguments) in
        let env =
          Array.of_list_map t.environment ~f:(fun (key, value) -> key ^ "=" ^ value)
        in
        let child =
          match
            Group.spawn
              ~sw
              ~limits:t.limits.kernel
              ~cwd
              ~executable:t.executable
              ~argv
              ~env
              ~stdin:(Eio_unix.Resource.fd input_r)
              ~stdout:(Eio_unix.Resource.fd output_w)
              ~stderr:(Eio_unix.Resource.fd error_w)
          with
          | Ok child -> child
          | Error Unsupported_platform -> stop (Expected Unsupported_platform)
          | Error Spawn_failed -> stop (Expected Spawn_failed)
          | Error Process_failed -> stop (Expected Process_failed)
        in
        Eio.Flow.close input_r;
        Eio.Flow.close output_w;
        Eio.Flow.close error_w;
        let output = Buffer.create (Int.min 4096 t.limits.maximum_output_bytes) in
        let remaining = ref t.limits.maximum_output_bytes in
        let stderr_bytes = ref 0 in
        let status = ref None in
        let read source ~retain =
          guard (fun () ->
            let buffer = Cstruct.create 4096 in
            let rec loop () =
              checkpoint ();
              match Eio.Flow.single_read source buffer with
              | bytes ->
                if bytes > !remaining then stop (Expected Output_limit);
                remaining := !remaining - bytes;
                if retain
                then Buffer.add_string output (Cstruct.to_string ~len:bytes buffer)
                else stderr_bytes := !stderr_bytes + bytes;
                loop ()
              | exception End_of_file -> ()
            in
            loop ())
        in
        let write () =
          guard (fun () ->
            Exn.protect
              ~finally:(fun () -> Eio.Flow.close input_w)
              ~f:(fun () ->
                let rec loop offset =
                  if offset < String.length t.stdin
                  then (
                    checkpoint ();
                    let length = Int.min 4096 (String.length t.stdin - offset) in
                    let bytes = Cstruct.of_string ~off:offset ~len:length t.stdin in
                    match Eio.Flow.write input_w [ bytes ] with
                    | () -> loop (offset + length)
                    | exception exn when broken_input exn -> ())
                in
                loop 0))
        in
        let await () =
          guard (fun () ->
            checkpoint ();
            match Group.await child with
            | Ok result -> status := Some result
            | Error Unsupported_platform -> stop (Expected Unsupported_platform)
            | Error Spawn_failed -> stop (Expected Spawn_failed)
            | Error Process_failed -> stop (Expected Process_failed))
        in
        (* Await does not cancel readers on normal exit: buffered stdout/stderr
           must be drained to EOF. The group stop closes child input readers, so
           unconsumed stdin ends with EPIPE rather than preventing completion. *)
        Eio.Fiber.all
          [ write
          ; (fun () -> read output_r ~retain:true)
          ; (fun () -> read error_r ~retain:false)
          ; await
          ];
        let status = Option.value_exn !status in
        Ok
          ({ status; stdout = Buffer.contents output; stderr_bytes = !stderr_bytes }
           : Outcome.t)))
  in
  match
    Eio.Time.Timeout.run
      (Eio.Time.Timeout.seconds clock (Time_ns.Span.to_sec t.limits.maximum_wall_time))
      invoke
  with
  | Ok outcome -> Ok outcome
  | Error `Timeout ->
    (match !failure with
     | Some (Unexpected (original, trace)) ->
       Stdlib.Printexc.raise_with_backtrace original trace
     | Some (Expected error) -> Error error
     | None -> Error Error.Timed_out)
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (match !failure with
     | Some (Expected error) -> Error error
     | Some (Unexpected (original, trace)) ->
       Stdlib.Printexc.raise_with_backtrace original trace
     | None -> Stdlib.Printexc.raise_with_backtrace exn backtrace)
;;
