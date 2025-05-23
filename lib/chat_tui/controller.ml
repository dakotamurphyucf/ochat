open Core
module Scroll_box = Notty_scroll_box

(** Outcome of handling a keyboard event. *)
type reaction =
  | Redraw (** Model was modified – caller should redraw.      *)
  | Submit_input (** User pressed Meta+Enter – send current input.   *)
  | Cancel_or_quit (** ESC – cancel request if running, else quit.     *)
  | Quit (** Immediate quit (Ctrl-C / q).                    *)
  | Unhandled (** Event not recognised by this layer.            *)

(* -------------------------------------------------------------------- *)
(* Helper – update the input_line ref while keeping it UTF-8 safe.       *)
(* For the purpose of the demo we take the simple approach of slicing   *)
(* bytes which works as long as the terminal only inputs ASCII.         *)
(* -------------------------------------------------------------------- *)

let append_char (model : Model.t) c =
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  let s = !input_ref in
  let pos = !pos_ref in
  let before = String.sub s ~pos:0 ~len:pos in
  let after = String.sub s ~pos ~len:(String.length s - pos) in
  input_ref := before ^ String.of_char c ^ after;
  pos_ref := pos + 1
;;

let backspace (model : Model.t) =
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  let pos = !pos_ref in
  let s = !input_ref in
  if pos > 0
  then (
    let before = String.sub s ~pos:0 ~len:(pos - 1) in
    let after = String.sub s ~pos ~len:(String.length s - pos) in
    input_ref := before ^ after;
    pos_ref := pos - 1)
;;

(* -------------------------------------------------------------------- *)
(* Scrolling helpers                                                    *)
(* -------------------------------------------------------------------- *)

let scroll_by_lines (model : Model.t) ~term delta =
  let _, screen_h = Notty_eio.Term.size term in
  (* Number of lines occupied by the multiline input editor. *)
  let input_height =
    match String.split_lines !(Model.input_line model) with
    | [] -> 1
    | ls -> List.length ls
  in
  let history_h = Int.max 1 (screen_h - input_height) in
  Scroll_box.scroll_by model.scroll_box ~height:history_h delta
;;

let page_size ~term (model : Model.t) =
  let _, screen_h = Notty_eio.Term.size term in
  let input_height =
    match String.split_lines !(Model.input_line model) with
    | [] -> 1
    | ls -> List.length ls
  in
  screen_h - input_height
;;

(* -------------------------------------------------------------------- *)
(* Main dispatcher                                                      *)
(* -------------------------------------------------------------------- *)

let handle_key ~(model : Model.t) ~term (ev : Notty.Unescape.event) : reaction =
  match ev with
  | `Key (`ASCII c, mods) when List.is_empty mods ->
    append_char model c;
    Redraw
  | `Key (`Backspace, _) ->
    backspace model;
    Redraw
  | `Key (`Arrow `Up, _) ->
    model.auto_follow := false;
    scroll_by_lines model ~term (-1);
    Redraw
  | `Key (`Arrow `Left, _) ->
    let pos_ref = Model.cursor_pos model in
    if !pos_ref > 0 then pos_ref := !pos_ref - 1;
    Redraw
  | `Key (`Arrow `Right, _) ->
    let pos_ref = Model.cursor_pos model in
    let input_ref = Model.input_line model in
    if !pos_ref < String.length !input_ref then pos_ref := !pos_ref + 1;
    Redraw
  | `Key (`Arrow `Down, _) ->
    model.auto_follow := false;
    scroll_by_lines model ~term 1;
    Redraw
  | `Key (`Page `Up, _) ->
    model.auto_follow := false;
    let ps = page_size ~term model in
    scroll_by_lines model ~term (-ps);
    Redraw
  | `Key (`Page `Down, _) ->
    model.auto_follow := false;
    let ps = page_size ~term model in
    scroll_by_lines model ~term ps;
    Redraw
  | `Key (`Home, _) ->
    model.auto_follow := false;
    Scroll_box.scroll_to_top model.scroll_box;
    Redraw
  | `Key (`End, _) ->
    model.auto_follow := true;
    let _, screen_h = Notty_eio.Term.size term in
    let input_h =
      match String.split_lines !(Model.input_line model) with
      | [] -> 1
      | ls -> List.length ls
    in
    Scroll_box.scroll_to_bottom model.scroll_box ~height:(screen_h - input_h);
    Redraw
  | `Key (`Enter, []) ->
    (* Literal newline inside the input buffer *)
    append_char model '\n';
    Redraw
  (* High-level actions -------------------------------------------------- *)
  | `Key (`Enter, mods) when List.mem mods `Meta ~equal:Poly.equal ->
    (* Submit user input for processing *)
    Submit_input
  | `Key (`Escape, _) -> Cancel_or_quit
  | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`ASCII 'q', _) -> Quit
  | _ -> Unhandled
;;
