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

module Baseline : sig
  type t

  (** True only when the engine borrows this registry and the selected language
      still resolves to its bundled grammar. Unrelated overrides do not affect
      this decision. *)
  val uses_bundled_grammar
    :  t
    -> Highlight_tm_engine.t
    -> language:[ `Chatml | `Chatmd ]
    -> bool
end

(** Create a distinct bundled registry and its immutable per-language baseline.
    Keep the pair together while adding optional custom grammars. *)
val create_with_baseline : unit -> Highlight_tm_loader.registry * Baseline.t

(** [create_with_sources_and_baseline sources] constructs a private bundled
    registry, captures its exact bundled grammar identities, then installs
    [sources] in order. *)
val create_with_sources_and_baseline
  :  Highlight_grammar_discovery.Source.t list
  -> (Highlight_tm_loader.registry * Baseline.t) Core.Or_error.t

(** Compatibility helper returning only the registry. Without its baseline,
    an explicit renderer stays on the existing TextMate path. *)
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

val current_baseline : unit -> Baseline.t option

(** [replace ?baseline registry] transfers ownership to the synchronous
    renderer and changes its grammar generation. Omitting the baseline keeps
    compatible callers on TextMate for ChatML/ChatMD. *)
val replace : ?baseline:Baseline.t -> Highlight_tm_loader.registry -> unit

(** [generation ()] changes whenever {!replace} installs a registry. *)
val generation : unit -> int
