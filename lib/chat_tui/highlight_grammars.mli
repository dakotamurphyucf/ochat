(** Vendored or built-in TextMate grammars bundled with the TUI.

    This module exposes helpers to register a curated set of grammars into a
    {!Highlight_tm_loader.registry}.  Each function is independent so that
    call-sites can choose exactly which grammars to include.

    The concrete grammar contents may be updated independently from callers. *)

open Core

(** Add the OCaml grammar (scopeName = ["source.ocaml"]) mapping both
      .ml and .mli files and common aliases (handled by the loader).

      Returns [Ok ()] on success or an [Error] if the embedded JSON cannot be
      parsed or converted to a [textmate-language] grammar. *)
val add_ocaml : Highlight_tm_loader.registry -> unit Or_error.t

(** Add the Dune grammar (scopeName = ["source.dune"]). Maps [dune],
      [dune-project], and [dune-workspace]. *)
val add_dune : Highlight_tm_loader.registry -> unit Or_error.t

(** Add the OPAM grammar (scopeName = ["source.opam"]). Maps [*.opam]. *)
val add_opam : Highlight_tm_loader.registry -> unit Or_error.t

(** Add the Shell/Bash grammar (scopeName = ["source.shell"]). Maps
      [sh], [bash]. *)
val add_shell : Highlight_tm_loader.registry -> unit Or_error.t

(** Add the Diff grammar (scopeName = ["source.diff"]). Maps [diff], [patch]. *)
val add_diff : Highlight_tm_loader.registry -> unit Or_error.t

(** Add the JSON grammar (scopeName = ["source.json"]). Maps [json]. *)
val add_json : Highlight_tm_loader.registry -> unit Or_error.t

(** Add the Markdown grammar (scopeName = ["source.gfm"]). Maps [md], [markdown], [gfm]. *)
val add_markdown : Highlight_tm_loader.registry -> unit Or_error.t

(** Add a minimal HTML grammar and a shim for [text.html.derivative].
    This enables Markdown’s embedded HTML handling (inline and blocks).

    - Provides [text.html.basic] with basic tag/attribute/entity/comment rules.
    - Provides [text.html.derivative] as a thin include of [text.html.basic]. *)
val add_html : Highlight_tm_loader.registry -> unit Or_error.t
