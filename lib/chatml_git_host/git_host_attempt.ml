open Core
module Error = Git_host_profile.Error
module Process = Shell_access.Bounded_process

module Limits = struct
  type t =
    { metadata_file_bytes : int
    ; pack_file_bytes : int
    ; acquisition_payload_bytes : int
    ; maximum_request_bytes : int
    ; maximum_output_bytes : int
    ; maximum_work_steps : int
    ; wall_time : Time_ns.Span.t
    ; cpu_seconds_per_process : int
    ; memory_bytes_per_process : int
    ; open_files_per_process : int
    }

  let metadata_file_bytes t = t.metadata_file_bytes
  let pack_file_bytes t = t.pack_file_bytes
  let acquisition_payload_bytes t = t.acquisition_payload_bytes
  let maximum_request_bytes t = t.maximum_request_bytes
  let wall_time t = t.wall_time

  let create
        ~metadata_file_bytes
        ~pack_file_bytes
        ~max_acquisition_payload_bytes
        ~max_request_bytes
        ~max_output_bytes
        ~max_work_steps
        ~wall_time
        ~cpu_seconds_per_process
        ~memory_bytes_per_process
        ~open_files_per_process
    =
    let open Or_error.Let_syntax in
    if
      metadata_file_bytes <= 0
      || pack_file_bytes < 0
      || max_acquisition_payload_bytes < 0
      || max_work_steps <= 0
    then Or_error.error_string "invalid Git acquisition limits"
    else (
      let%map (_ : Process.Limits.t) =
        Process.Limits.create
          ~maximum_request_bytes:max_request_bytes
          ~maximum_output_bytes:max_output_bytes
          ~maximum_wall_time:wall_time
          ~cpu_seconds_per_process
          ~memory_bytes_per_process
          ~file_size_bytes:pack_file_bytes
          ~open_files_per_process
      in
      { metadata_file_bytes
      ; pack_file_bytes
      ; acquisition_payload_bytes = max_acquisition_payload_bytes
      ; maximum_request_bytes = max_request_bytes
      ; maximum_output_bytes = max_output_bytes
      ; maximum_work_steps = max_work_steps
      ; wall_time
      ; cpu_seconds_per_process
      ; memory_bytes_per_process
      ; open_files_per_process
      })
  ;;
end

type t =
  { limits : Limits.t
  ; clock : Eio.Time.Mono.ty Eio.Time.Mono.t
  ; started : Mtime.t
  ; mutable poisoned : bool
  ; mutable requests : int
  ; mutable output : int
  ; mutable work : int
  }

let create limits ~clock =
  { limits
  ; clock :> Eio.Time.Mono.ty Eio.Time.Mono.t
  ; started = Eio.Time.Mono.now clock
  ; poisoned = false
  ; requests = 0
  ; output = 0
  ; work = 0
  }
;;

let poison t = t.poisoned <- true
let is_poisoned t = t.poisoned
let request_bytes t = t.requests
let output_bytes t = t.output
let work_steps t = t.work

let fail t code =
  poison t;
  Error (Error.create code)
;;

let remaining_time t =
  Time_ns.Span.to_sec t.limits.wall_time
  -. (Mtime.Span.to_float_ns (Mtime.span t.started (Eio.Time.Mono.now t.clock)) /. 1e9)
;;

let poll t ~checkpoint =
  if t.poisoned
  then Error (Error.create Process_failed)
  else (
    (try
       Eio.Fiber.check ();
       checkpoint ()
     with
     | exn ->
       let bt = Stdlib.Printexc.get_raw_backtrace () in
       poison t;
       Stdlib.Printexc.raise_with_backtrace exn bt);
    if Float.(remaining_time t <= 0.) then fail t Timed_out else Ok ())
;;

let reserve_work t units =
  if t.poisoned
  then Error (Error.create Process_failed)
  else if units < 0 || units > t.limits.maximum_work_steps - t.work
  then fail t Limit_exceeded
  else (
    t.work <- t.work + units;
    Ok ())
;;

let process_limits t ~file_size_bytes ~maximum_output_bytes =
  if t.poisoned
  then Error (Error.create Process_failed)
  else if file_size_bytes < 0 || maximum_output_bytes < 0
  then fail t Invalid_input
  else (
    let seconds = remaining_time t in
    if Float.(seconds <= 0.)
    then fail t Timed_out
    else (
      match
        Process.Limits.create
          ~maximum_request_bytes:(t.limits.maximum_request_bytes - t.requests)
          ~maximum_output_bytes:
            (Int.min maximum_output_bytes (t.limits.maximum_output_bytes - t.output))
          ~maximum_wall_time:(Time_ns.Span.of_sec seconds)
          ~cpu_seconds_per_process:t.limits.cpu_seconds_per_process
          ~memory_bytes_per_process:t.limits.memory_bytes_per_process
          ~file_size_bytes
          ~open_files_per_process:t.limits.open_files_per_process
      with
      | Ok limits -> Ok limits
      | Error _ -> fail t Limit_exceeded))
;;

let charge_request t ~executable ~arguments ~environment ~stdin =
  let remaining = t.limits.maximum_request_bytes - t.requests in
  let add count bytes =
    if bytes < 0 || bytes > remaining - count then Error () else Ok (count + bytes)
  in
  let terminated count input =
    let open Result.Let_syntax in
    let%bind count = add count (String.length input) in
    add count 1
  in
  let open Result.Let_syntax in
  let total =
    let%bind count = terminated 0 executable in
    (* Bounded_process charges both the executable path and argv[0]. *)
    let%bind count = terminated count executable in
    let%bind count = List.fold_result arguments ~init:count ~f:terminated in
    let%bind count =
      List.fold_result environment ~init:count ~f:(fun count (name, value) ->
        let%bind count = terminated count name in
        terminated count value)
    in
    add count (String.length stdin)
  in
  if t.poisoned
  then Error (Error.create Process_failed)
  else (
    match total with
    | Error () -> fail t Limit_exceeded
    | Ok bytes ->
      t.requests <- t.requests + bytes;
      Ok ())
;;

let charge_output t ~stdout ~stderr_bytes =
  let remaining = t.limits.maximum_output_bytes - t.output in
  let bytes = String.length stdout in
  if t.poisoned
  then Error (Error.create Process_failed)
  else if stderr_bytes < 0 || bytes > remaining || stderr_bytes > remaining - bytes
  then fail t Limit_exceeded
  else (
    t.output <- t.output + bytes + stderr_bytes;
    Ok ())
;;

let acquisition_reservation limits =
  let multiply n value =
    if value > Int.max_value / n
    then Error (Error.create Limit_exceeded)
    else Ok (n * value)
  in
  let add a b =
    if b > Int.max_value - a then Error (Error.create Limit_exceeded) else Ok (a + b)
  in
  let open Result.Let_syntax in
  let%bind init = multiply 3 limits.Limits.metadata_file_bytes in
  let%bind init = add init 7 in
  let%bind metadata_overlap = multiply 4 limits.metadata_file_bytes in
  let%bind pack_overlap = multiply 8 limits.pack_file_bytes in
  let%bind fetch_and_copy = add metadata_overlap pack_overlap in
  let reservation = Int.max init fetch_and_copy in
  if reservation > limits.acquisition_payload_bytes
  then Error (Error.create Limit_exceeded)
  else Ok reservation
;;

let acquisition_work_reservation limits =
  let open Result.Let_syntax in
  let%bind bytes = acquisition_reservation limits in
  (* Four weighted owner units per reserved payload byte covers two inventories,
     bounded metadata parsing, single-byte copy reads/writes and EOF checks. Fixed
     setup/path/sort/cleanup stages use 1024 additional units. This is an adapter
     admission model, not a syscall, CPU or wall-clock proof. *)
  if bytes > (Int.max_value - 1024) / 4
  then Error (Error.create Limit_exceeded)
  else Ok ((4 * bytes) + 1024)
;;
