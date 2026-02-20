(** Chat page layout helper.

    This module centralises the chat page geometry calculations so both the
    renderer and controller can agree on:

    - how tall the input box is allowed to get (it is capped and becomes
      internally scrollable when the buffer exceeds that height), and
    - the resulting transcript viewport height used by {!Notty_scroll_box}.

    The chat page has three vertical regions:

    - history (at least 1 row),
    - status bar (exactly 1 row), and
    - input box (framed; 2 rows of border + N rows of content). *)

open Core

type t =
  { input_box_height : int
  ; history_height : int
  ; sticky_height : int
  ; scroll_height : int
  }

(** [max_input_box_height ~screen_h] is the maximum total height (in terminal
    cells) allocated to the framed input box.

    The result leaves room for at least 1 history row and the 1-row status bar. *)
val max_input_box_height : screen_h:int -> int

(** [compute ~screen_w ~screen_h ~model] returns the current chat page layout.

    The [input_box_height] is capped to [max_input_box_height ~screen_h] and
    grows with the number of display rows required to render the prompt input
    when soft-wrapped to [screen_w].

    [history_height] is then derived from the remaining space and clamped to be
    at least 1 row. *)
val compute : screen_w:int -> screen_h:int -> model:Model.t -> t
