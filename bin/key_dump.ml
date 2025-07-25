(** Print raw terminal input events as decoded by Notty.

    This executable is a tiny diagnostic utility for inspecting the exact
    {!Notty.Unescape.event} values produced by your terminal (and operating
    system) for every key-press, mouse action, paste or window resize.  It is
    useful when you need to extend key bindings in a {!Notty}-based TUI and
    are unsure which constructor or modifier combination to match on.

    Run the program in the problematic terminal and press the keys you are
    interested in – each event is printed on its own line in a human-readable
    form.  Exit with either {!kbd:q} or {!kbd:Ctrl-C}.

    Example session (pressing **Ctrl-A** followed by the *Up* arrow):

    {[
      $ key-dump
      Key   ASCII 'A' (0x41)          mods=[Ctrl]
      Key   Arrow Up                  mods=[]
    ]}

    @see <https://pqwy.github.io/notty/doc/Notty.Unescape.html> Notty.Unescape
    for the full definition of the event type.
*)

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

(** [event_to_string ev] converts a {!Notty.Unescape.event} (or [`Resize]) into
    a single human-readable line suitable for logging.  The output is wide
    enough to align the most common variants, making it easy to visually scan
    the stream while pressing keys. *)

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

(** [main env] starts a fullscreen Notty session and pretty-prints every
    decoded input event obtained from [env#stdin].  The function blocks until
    the user presses either {b q} or {b Ctrl-C}. *)

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
