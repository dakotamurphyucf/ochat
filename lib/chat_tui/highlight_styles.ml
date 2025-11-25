(** Implementation of reusable Notty styles and helpers.

    See the interface for documentation and invariants. Utilities here
    construct {!Notty.A.t} values for use across the highlighting code. *)

let ( ++ ) = Notty.A.( ++ )
let bold = Notty.A.(st bold)
let italic = Notty.A.(st italic)
let underline = Notty.A.(st underline)
let empty = Notty.A.empty

(* Grayscale helpers use Notty's extended palette ramp. *)
let fg_gray n = Notty.A.(fg (gray n))
let bg_gray n = Notty.A.(bg (gray n))

(* Truecolor (24-bit) constructors. *)
let fg_rgb ~r ~g ~b = Notty.A.(fg (rgb_888 ~r ~g ~b))
let bg_rgb ~r ~g ~b = Notty.A.(bg (rgb_888 ~r ~g ~b))

(* Extended 256-color cube convenience, for legacy themes using 0..5 channels. *)
let _map6 = function
  | 0 -> 0x00
  | 1 -> 0x5f
  | 2 -> 0x87
  | 3 -> 0xaf
  | 4 -> 0xd7
  | 5 -> 0xff
  | n -> invalid_arg (Printf.sprintf "rgb6 channel out of range: %d" n)
;;

let fg_rgb6 ~r ~g ~b =
  let r = _map6 r
  and g = _map6 g
  and b = _map6 b in
  fg_rgb ~r ~g ~b
;;

let bg_rgb6 ~r ~g ~b =
  let r = _map6 r
  and g = _map6 g
  and b = _map6 b in
  bg_rgb ~r ~g ~b
;;

(* ANSI convenience colours. *)
let fg_black = Notty.A.(fg black)
let fg_red = Notty.A.(fg red)
let fg_green = Notty.A.(fg green)
let fg_yellow = Notty.A.(fg yellow)
let fg_blue = Notty.A.(fg blue)
let fg_magenta = Notty.A.(fg magenta)
let fg_cyan = Notty.A.(fg cyan)
let fg_lightwhite = Notty.A.(fg lightwhite)
let bg_black = Notty.A.(bg black)
let bg_white = Notty.A.(bg white)
let bg_lightwhite = Notty.A.(bg lightwhite)

(* Hex utilities. Accepts "#RRGGBB", "RRGGBB", "#RRGGBBAA" (alpha ignored),
   and short forms "#RGB"/"RGB" (nibbles doubled). Returns [None] on parse
   error or out-of-range channel values. *)
let hex_to_rgb s : (int * int * int) option =
  let len = String.length s in
  let s = if len > 0 && s.[0] = '#' then String.sub s 1 (len - 1) else s in
  let len = String.length s in
  let expand_short s =
    let b = Bytes.create 6 in
    if String.length s < 3
    then None
    else (
      Bytes.set b 0 s.[0];
      Bytes.set b 1 s.[0];
      Bytes.set b 2 s.[1];
      Bytes.set b 3 s.[1];
      Bytes.set b 4 s.[2];
      Bytes.set b 5 s.[2];
      Some (Bytes.unsafe_to_string b))
  in
  let s =
    match len with
    | 3 -> expand_short s
    | 4 -> expand_short (String.sub s 0 3) (* ignore alpha nibble *)
    | 6 -> Some s
    | 8 -> Some (String.sub s 0 6) (* ignore alpha *)
    | _ -> None
  in
  match s with
  | None -> None
  | Some s ->
    let parse_byte i =
      let sub = String.sub s i 2 in
      match int_of_string_opt ("0x" ^ sub) with
      | None -> None
      | Some v -> if v < 0 || v > 255 then None else Some v
    in
    (match parse_byte 0, parse_byte 2, parse_byte 4 with
     | Some r, Some g, Some b -> Some (r, g, b)
     | _ -> None)
;;

let fg_hex hex =
  match hex_to_rgb hex with
  | None -> empty
  | Some (r, g, b) -> fg_rgb ~r ~g ~b
;;

let bg_hex hex =
  match hex_to_rgb hex with
  | None -> empty
  | Some (r, g, b) -> bg_rgb ~r ~g ~b
;;
