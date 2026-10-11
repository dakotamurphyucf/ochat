open Core
module Error = Project_prepare_error

type t =
  { started : Mtime.t
  ; seconds : float
  }

let create ~clock ~wall_time =
  let seconds = Time_ns.Span.to_sec wall_time in
  if Time_ns.Span.(wall_time <= zero) || not (Float.is_finite seconds)
  then Or_error.error_string "invalid project preparation deadline"
  else Ok { started = Eio.Time.Mono.now clock; seconds }
;;

let seconds_left t ~clock =
  t.seconds
  -. (Mtime.Span.to_float_ns (Mtime.span t.started (Eio.Time.Mono.now clock)) /. 1e9)
;;

let check t ~clock =
  if Float.(seconds_left t ~clock <= 0.) then Error (Error.create Timed_out) else Ok ()
;;
