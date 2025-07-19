open Core
module Unix = Core_unix
open Eio

(**************************************************************************)
(* Tiny structured logger using Jsonaf                                     *)
(**************************************************************************)

module J = Jsonaf
open Jsonaf.Export

type level =
  [ `Debug
  | `Info
  | `Warn
  | `Error
  ]

let level_to_string = function
  | `Debug -> "DEBUG"
  | `Info -> "INFO"
  | `Warn -> "WARN"
  | `Error -> "ERROR"
;;

(* Global mutex so that concurrent domains don\'t interleave a single line. *)
let lock = Eio.Mutex.create ()
let now_float () = Unix.gettimeofday ()

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

(* Heartbeat utility *)
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
