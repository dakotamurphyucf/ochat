(** Compatibility re-export of the shared TextMate loader. *)
include
  module type of Terminal_highlighting.Highlight_tm_loader
  with type registry = Terminal_highlighting.Highlight_tm_loader.registry

(** Legacy blocking file loader retained for existing callers. New frontends
    use Eio to load bytes and call [add_grammar_jsonaf] on their own registry. *)
val add_grammar_jsonaf_file : registry -> path:string -> unit Core.Or_error.t
