open Core

type t =
  { decoder : Uutf.decoder
  ; mutable is_finished : bool
  }

let create () = { decoder = Uutf.decoder ~encoding:`UTF_8 `Manual; is_finished = false }

let ensure_open t =
  if t.is_finished then invalid_arg "Utf8_ingest: decoder is already finished"
;;

let drain decoder =
  let output = Buffer.create 256 in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar uchar ->
      Uutf.Buffer.add_utf_8 output uchar;
      loop ()
    | `Malformed _ ->
      Uutf.Buffer.add_utf_8 output Uutf.u_rep;
      loop ()
    | `Await | `End -> Buffer.contents output
  in
  loop ()
;;

let add t bytes =
  ensure_open t;
  if String.is_empty bytes
  then ""
  else (
    let bytes = Bytes.of_string bytes in
    Uutf.Manual.src t.decoder bytes 0 (Bytes.length bytes);
    drain t.decoder)
;;

let finish t =
  ensure_open t;
  t.is_finished <- true;
  Uutf.Manual.src t.decoder (Bytes.create 0) 0 0;
  drain t.decoder
;;
