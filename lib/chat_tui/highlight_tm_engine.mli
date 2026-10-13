(** Compatibility re-export of the shared TextMate span engine. *)
include
  module type of Terminal_highlighting.Highlight_tm_engine
  with type t = Terminal_highlighting.Highlight_tm_engine.t
   and type span = Terminal_highlighting.Highlight_tm_engine.span
   and type scoped_span = Terminal_highlighting.Highlight_tm_engine.scoped_span
   and type fallback_reason = Terminal_highlighting.Highlight_tm_engine.fallback_reason
   and type info = Terminal_highlighting.Highlight_tm_engine.info
