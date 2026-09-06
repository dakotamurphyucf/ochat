(** Vendored or built-in TextMate grammars bundled with the TUI.

    [Chat_tui.Highlight_grammars] exposes helpers that register a curated set
    of TextMate grammars into a [Highlight_tm_loader.registry]. Each function
    is independent so that call-sites can choose exactly which grammars to
    install, or they can rely on {!Chat_tui.Highlight_registry.get} for a
    pre-populated registry.

    Grammar contents and patterns are implementation details: they may evolve
    over time without breaking callers, as long as scope names and language
    tags remain stable. *)

open Core

(** [add_ocaml reg] adds the bundled OCaml grammar
    (scopeName = ["source.ocaml"]) to [reg].

    The grammar is embedded at compile time, so loading does not depend on
    the process working directory.

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_ocaml : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_python reg] adds the bundled VS Code Python grammar and its regular
    expression companion grammar (scope names ["source.python"] and
    ["source.regexp.python"]) to [reg].

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_python : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_rust reg] adds the bundled Rust grammar
    (scopeName = ["source.rust"]) to [reg].

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_rust : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_javascript reg] adds the bundled VS Code JavaScript and JSX grammars
    (scope names ["source.js"] and ["source.js.jsx"]) to [reg].

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_javascript : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_typescript reg] adds the bundled VS Code TypeScript and TSX grammars
    (scope names ["source.ts"] and ["source.tsx"]) to [reg].

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_typescript : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_dune reg] adds a Dune grammar (scopeName = ["source.dune"]) to
    [reg]. The grammar is embedded in the binary and covers typical
    [dune], [dune-project], and [dune-workspace] forms.

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_dune : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_opam reg] adds an OPAM grammar (scopeName = ["source.opam"]) to
    [reg]. The grammar is embedded and targets typical [.opam] manifest
    files.

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_opam : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_shell reg] adds a Shell/Bash grammar (scopeName = ["source.shell"]) to
    [reg]. The grammar is embedded and recognises POSIX shell and Bash
    constructs; common extensions [sh] and [bash] are resolved by the
    loader.

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_shell : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_diff reg] adds a Diff grammar (scopeName = ["source.diff"]) to
    [reg]. The grammar is embedded and highlights unified diff headers,
    hunk markers, and line prefixes for insertions, deletions, and context.

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_diff : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_ochat_apply_patch reg] adds the ochat [apply_patch] grammar
    (scopeName = ["source.ochat-apply-patch"]) to [reg].

    The grammar is tuned for the multi-pane patch view produced by the
    [ochat.apply_patch] tool. It recognises:

    {ul
    {- the Unicode banner line starting with ["┏━["] and captures the
        filename separately;}
    {- per-file operation headers such as ["*** Add File:"],
       ["*** Update File:"], and ["*** Delete File:"];}
    {- update summary and anchor lines such as
       ["Update of file successful. 2 insertions, 1 deletion, 1 hunk."]
       and ["@ scope[2]: let render value ="];}
    {- hunk labels such as ["[hunk 1/2]"];}
    {- compact snippet lines of the form
       ["o:  42 n:  42 | code"] and
       ["o:   - n:  42 | +code"], plus replacement markers such as
       ["o:   - n:   - | ~ replaced by ~"], highlighting the compact
       old/new margin and the right-hand diff marker/content.}}

    This is intended for internal tooling rather than general-purpose patch
    files.

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_ochat_apply_patch : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_json reg] adds a JSON grammar (scopeName = ["source.json"]) to
    [reg]. The grammar is embedded and recognises object keys, strings,
    numbers, booleans, and [null].

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_json : Highlight_tm_loader.registry -> unit Or_error.t

(** [add_markdown reg] adds the bundled Markdown grammar
    (scopeName = ["text.html.markdown"]) to [reg]. The grammar is intended
    for GitHub-flavoured Markdown and is used for prose paragraphs.

    The grammar is embedded at compile time, so loading does not depend on
    the process working directory.

    @return [Ok ()] on success, or [Error _] if the embedded grammar cannot
    be parsed or converted. *)
val add_markdown : Highlight_tm_loader.registry -> unit Or_error.t

(** Add a minimal HTML grammar and a shim for [text.html.derivative].
    This enables Markdown’s embedded HTML handling (inline and blocks).

    [add_html reg] adds two related grammars to [reg]:
    {ul
    {- [text.html.basic], a small built-in grammar with basic
       tag/attribute/entity/comment rules, or a vendored grammar from
       [lib/chat_tui/grammars/html.tmLanguage.json] when present;}
    {- [text.html.derivative], a thin shim that simply includes
       [text.html.basic], keeping Markdown grammars that refer to
       [text.html.derivative] working.}}

    @return [Ok ()] on success, or [Error _] if both the vendored and built-in
    HTML grammars fail to load. *)
val add_html : Highlight_tm_loader.registry -> unit Or_error.t
