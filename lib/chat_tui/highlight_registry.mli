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

(** [create ()] returns a distinct registry populated with the bundled
    grammars. No registry or decoded grammar state is shared with registries
    returned by other calls. *)
val create : unit -> Highlight_tm_loader.registry

(** [create_with_sources sources] constructs a private bundled registry and
    installs [sources] in order. *)
val create_with_sources
  :  Highlight_grammar_discovery.Source.t list
  -> Highlight_tm_loader.registry Core.Or_error.t

(** [get ()] returns the shared registry pre-populated with the built-in
    grammars used by the TUI.

    The first call constructs a fresh registry, installs the curated
    grammars from {!Highlight_grammars}, and logs any load failures to
    standard output using [Core.printf]. Subsequent calls return the same
    value.

    The returned registry remains current until {!replace} installs a fully
    constructed replacement. *)
val get : unit -> Highlight_tm_loader.registry

(** [replace registry] transfers ownership of [registry] to the synchronous
    renderer. *)
val replace : Highlight_tm_loader.registry -> unit

(** [generation ()] changes whenever {!replace} installs a registry. *)
val generation : unit -> int
