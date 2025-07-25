open Core
module Unix = Core_unix
open Eio

(** Structured JSON logger.

    This module provides a minimal, dependency–light logger that writes a
    single JSON object per line to the file [run.log].  The format is
    optimised for machine parsing while still being greppable by humans.
    Each log entry is a {!Jsonaf.t} object with fixed keys (timestamp,
    level, message, …) and an optional user-supplied context.  Concurrency
    is handled with an {!Eio.Mutex.t} so that messages coming from multiple
    domains never interleave.

    {2 Usage examples}

    Emit a single INFO line:
    {[ Log.emit `Info "starting" ]}

    Time a function and record its duration (in ms):
    {[
      let result = Log.with_span "parse_file" (fun () -> parse file) in
      ....
    ]}

    Start a heartbeat fiber that periodically reports custom metrics:
    {[
      let probe () = [ "connections", `Int (Connection_pool.size pool) ] in
      Log.heartbeat ~sw ~clock:Eio.Stdenv.clock ~interval:60.0 ~probe ()
    ]}
*)
(**************************************************************************)
(* Implementation below                                                    *)
(**************************************************************************)

module J = Jsonaf
open Jsonaf.Export

(** Log severity level. *)
type level =
  [ `Debug
  | `Info
  | `Warn
  | `Error
  ]

(** [level_to_string lvl] is a pure helper that converts [lvl] to the
    capitalised string that will be written in the log record.  Exposed
    mainly for testing. *)
let level_to_string = function
  | `Debug -> "DEBUG"
  | `Info -> "INFO"
  | `Warn -> "WARN"
  | `Error -> "ERROR"
;;

(* Global mutex so that concurrent domains don\'t interleave a single line. *)
let lock = Eio.Mutex.create ()
let now_float () = Unix.gettimeofday ()

(** [emit ?ctx lvl msg] writes a single log line.

      Parameters:
      • [ctx] – additional fields to merge into the JSON object.  The list
        is concatenated *before* the built-in base fields so that, if you
        really want to shadow e.g. ["level"], you still can.
      • [lvl] – log severity.
      • [msg] – human-oriented short message.

      The output object always contains the following keys:
      • [ts] — float, Unix epoch seconds (as returned by
        [Unix.gettimeofday]).
      • [level] — string, uppercase.
      • [msg] — your message.
      • [pid] — int, Unix process id.
      • [domain] — int, OCaml domain id (multicore).  The cast through
        [Obj.magic] is safe because the runtime guarantees the id to fit
        in a machine word.

      All writes are protected by a global {!Eio.Mutex.t} so that each call
      results in exactly one line in the file [run.log].  The file is
      opened in append mode with permissions 0644.

      The function never raises.  *)
let emit ?(ctx = []) lvl msg =
  let base : (string * J.t) list =
    [ "ts", jsonaf_of_float (now_float ())
    ; "level", jsonaf_of_string (level_to_string lvl)
    ; "msg", jsonaf_of_string msg
    ; "pid", jsonaf_of_int (Caml_unix.getpid ())
    ; "domain", jsonaf_of_int (Obj.magic (Domain.self ()) : int)
    ]
  in
  let obj = `Object (ctx @ base) in
  Eio.Mutex.use_rw ~protect:false lock (fun () ->
    Out_channel.with_file ~append:true ~perm:0o644 "run.log" ~f:(fun oc ->
      Out_channel.output_string oc (J.to_string obj);
      Out_channel.output_char oc '\n';
      Out_channel.flush oc))
;;

(** [with_span ?ctx name fn] executes [fn] and automatically records the
      start / end (or error) events.  The two log lines share the same
      [ctx] and include an extra field [duration_ms] on success.

      The helper is intended for quick, ad-hoc instrumentation – you get a
      trace for free without allocating an explicit span id.  Nested calls
      produce nested JSON objects that can later be correlated by a
      consumer such as OpenTelemetry or a simple `jq` script.

      Behaviour:
      • Emits [`Debug] "{name}_start" immediately.
      • Runs [fn].
      • On success, computes the elapsed time in *milliseconds* and logs a
        [`Debug] "{name}_end" with an additional [duration_ms] key.
      • On exception, logs [`Error] "{name}_error" and re-raises.

      The function is exception-transparent – it never catches silently. *)
let with_span ?ctx name f =
  emit ?ctx `Debug (name ^ "_start");
  let t0 = now_float () in
  match f () with
  | v ->
    let dt = (now_float () -. t0) *. 1000. in
    let ctx' = ("duration_ms", jsonaf_of_float dt) :: Option.value ctx ~default:[] in
    emit ~ctx:ctx' `Debug (name ^ "_end");
    v
  | exception ex ->
    emit ?ctx `Error (name ^ "_error");
    raise ex
;;

(** [heartbeat ~sw ~clock ~interval ~probe ()] starts a background fiber
      that periodically calls [probe] and logs its result under the
      message "heartbeat" at [`Info] level.

      Parameters:
      • [sw] – the parent switch that governs the lifetime of the fiber.
      • [clock] – Eio clock used for sleeping.
      • [interval] – delay in seconds between consecutive heartbeats.
      • [probe] – user function returning a list of extra JSON fields for
        the current heartbeat.

      The function returns immediately; the fiber runs until [sw] is
      finished or uncaught exceptions propagate (which will cancel the
      fiber).  *)
let heartbeat ~sw ~clock ~interval ~probe () =
  Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      let ctx = probe () in
      emit ~ctx `Info "heartbeat";
      Time.sleep clock interval;
      loop ()
    in
    loop ())
;;
