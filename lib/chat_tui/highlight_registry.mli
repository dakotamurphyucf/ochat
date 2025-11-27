(** Process-wide registry of built-in TextMate grammars.

     [Chat_tui.Highlight_registry] exposes a singleton
     {!Highlight_tm_loader.registry} that is lazily constructed and reused
     across the UI. The registry is populated with the curated grammars from
     {!Highlight_grammars} (OCaml, Dune, OPAM, shell scripts, diffs, JSON,
     HTML, Markdown, and the internal [ochat-apply-patch] format).

     Typical usage is to obtain the registry via {!get} and attach it to a
     {!Highlight_tm_engine.t} with {!Highlight_tm_engine.with_registry}. For
     custom sets of grammars, build your own registry via
     {!Highlight_tm_loader.create_registry} and call the
     {!Highlight_grammars.add_*} helpers directly. *)

(** [get ()] returns the shared registry pre-populated with the built-in
    grammars used by the TUI.

    The first call constructs a fresh registry, installs the curated
    grammars from {!Highlight_grammars}, and logs any load failures to
    standard output using [Core.printf]. Subsequent calls return the same
    value.

    The returned registry is shared by all callers. It is safe to add extra
    grammars to it using {!Highlight_tm_loader.add_grammar_jsonaf} or
    {!Highlight_tm_loader.add_grammar_jsonaf_file}, but there is no support
    for removing grammars once installed. *)
val get : unit -> Highlight_tm_loader.registry
