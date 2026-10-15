(** Compatibility re-export of the shared terminal highlighting theme. *)
include
  module type of Terminal_highlighting.Highlight_theme
  with type t = Terminal_highlighting.Highlight_theme.t
