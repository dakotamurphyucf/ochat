(* key_dump.ml --- Print raw Notty input events.

   Run this helper in the terminal where you experience shortcut problems
   to see the exact [`Notty.Unescape.event] values produced by your
   terminal/OS for each keypress.  This makes it easy to extend the
   controller’s pattern-matching with the right variants.

   Quit with either   q   or   Ctrl-C. *)

open Core
open Eio.Std

let string_of_special = function
  | `Escape -> "Escape"
  | `Enter -> "Enter"
  | `Tab -> "Tab"
  | `Backspace -> "Backspace"
  | `Insert -> "Insert"
  | `Delete -> "Delete"
  | `Home -> "Home"
  | `End -> "End"
  | `Arrow d ->
    "Arrow "
    ^
      (match d with
      | `Up -> "Up"
      | `Down -> "Down"
      | `Left -> "Left"
      | `Right -> "Right")
  | `Page d ->
    "Page "
    ^
      (match d with
      | `Up -> "Up"
      | `Down -> "Down")
  | `Function n -> sprintf "Function %d" n
;;

let string_of_key = function
  | `ASCII c -> sprintf "ASCII %C (0x%02x)" c (Char.to_int c)
  | `Uchar u -> sprintf "Uchar U+%04X" (Uchar.to_scalar u)
  | #Notty.Unescape.special as s -> string_of_special s
;;

let string_of_mods mods =
  match mods with
  | [] -> "[]"
  | ms ->
    ms
    |> List.map ~f:(function
      | `Ctrl -> "Ctrl"
      | `Meta -> "Meta"
      | `Shift -> "Shift")
    |> String.concat ~sep:","
    |> sprintf "[%s]"
;;

let event_to_string : [ Notty.Unescape.event | `Resize ] -> string = function
  | `Key (k, mods) ->
    sprintf "Key   %-25s mods=%s" (string_of_key k) (string_of_mods mods)
  | `Mouse (((`Press _ | `Drag | `Release) as m), (x, y), mods) ->
    sprintf
      "Mouse %s at (%d,%d) mods=%s"
      (match m with
       | `Press b ->
         (match b with
          | `Left -> "Press Left"
          | `Middle -> "Press Middle"
          | `Right -> "Press Right"
          | `Scroll `Up -> "Scroll Up"
          | `Scroll `Down -> "Scroll Down")
       | `Drag -> "Drag"
       | `Release -> "Release")
      x
      y
      (string_of_mods mods)
  | `Paste `Start -> "Paste Start"
  | `Paste `End -> "Paste End"
  | `Resize -> sprintf "Resize "
;;

let main env =
  Switch.run
  @@ fun _ ->
  let a, b = Promise.create () in
  (* We don't need mouse reporting/bpaste for keyboard debugging. *)
  Notty_eio.Term.run
    ~nosig:false
    ~mouse:false
    ~bpaste:false
    ~input:env#stdin
    ~output:env#stdout
    ~on_event:(fun ev ->
      Format.printf "%s\n%!" (event_to_string ev);
      match ev with
      | `Key (`ASCII 'q', _) | `Key (`ASCII 'C', [ `Ctrl ]) -> Promise.resolve b ()
      | _ -> ())
    (fun _term -> Promise.await a)
;;

let () = Eio_main.run main

(* Key   ASCII 'A' (0x41)          mods=[Ctrl] Ctrl-A
   Key   ASCII 'E' (0x45)          mods=[Ctrl ] Ctrl-E
   Key   Enter                     mods=[]      Ctrl-J
   Key   ASCII 'G' (0x47)          mods=[Ctrl] Ctrl-G
   Key   ASCII 'K' (0x4b)          mods=[Ctrl] Ctrl-K
   Key   ASCII 'L' (0x4c)          mods=[Ctrl] Ctrl-L
  Key   ASCII 'N' (0x4e)           mods=[Ctrl] Ctrl-N
  Key   ASCII 'W' (0x57)           mods=[Ctrl] Ctrl-W
  Key   ASCII 'P' (0x50)           mods=[Ctrl] Ctrl-P
  Key   ASCII 'U' (0x55)           mods=[Ctrl] Ctrl-U
  Key   ASCII 'b' (0x62)           mods=[Meta] Meta-left-arrow
  Key   ASCII 'f' (0x66)           mods=[Meta] Meta-right-arrow
  Key   Uchar U+00DA               mods=[]     Meta-Shift-Colon
  Key   Uchar U+00DF               mods=[]     Meta-s
*)
