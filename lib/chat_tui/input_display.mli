open Core

type break_kind =
  [ `Newline
  | `Wrap
  | `Eof
  ]

type row =
  { start : int
  ; stop : int
  ; next_pos : int
  ; cursor_limit : int
  ; text : string
  ; width : int
  ; break_kind : break_kind
  }

type cursor =
  { row : int
  ; byte_in_row : int
  ; col : int
  }

type t =
  { rows : row list
  ; cursor : cursor
  }

(** Prompt prefix and indent used by the input box.

    The first display row is prefixed with [prefix]; all subsequent rows use
    [indent] to keep wrapped lines aligned. *)
val prompt_prefix_and_indent : model:Model.t -> string * string

(** [content_row_count ~box_width ~model] returns the number of display rows
    required to render the active input buffer (Insert/Normal: [Model.input_line],
    Cmdline: [Model.cmdline]) with soft-wrapping at [box_width].

    The count is based on the underlying content only; it does not add a virtual
    trailing row for the caret when the cursor is at end-of-buffer. *)
val content_row_count : box_width:int -> model:Model.t -> int

(** [layout_for_render ~box_width ~model] computes the display rows for the
    active input buffer and the corresponding caret location.

    The returned [cursor.col] is measured in terminal cells (taking wide glyphs
    into account), while [cursor.byte_in_row] refers to the byte offset inside
    the row's [text]. *)
val layout_for_render : box_width:int -> model:Model.t -> t

(** [cursor_pos_after_vertical_move ~box_width ~model ~dir] computes the new
    cursor position when moving the caret by one visual row.

    Returns [None] if vertical movement is not applicable (e.g. Cmdline mode) or
    if the cursor cannot move further in the requested direction. *)
val cursor_pos_after_vertical_move
  :  box_width:int
  -> model:Model.t
  -> dir:int
  -> int option
