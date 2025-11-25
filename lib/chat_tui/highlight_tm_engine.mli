(** Terminal text-highlighting engine producing Notty spans.

    Convert plain text into per-line sequences of [(attr * text)] spans that
    render with {!Notty.I.string}. [attr] is a {!Notty.A.t} describing the
    styling to apply to [text].

    When configured with a TextMate registry via {!with_registry}, tokenize
    each line using the resolved grammar for [lang] with the
    textmate-language library and map token scopes to {!Notty.A.t} via
    {!Highlight_theme}. If no registry is provided or [lang] cannot be
    resolved, fall back to a plain rendering that produces a single
    {!Notty.A.empty} span per line.

    Invariants
    {ul
    {- Each span belongs to exactly one input line and contains no newlines.}
    {- The number of output lines equals [Core.String.split_lines text].}
    {- Concatenating all [text] fragments of a line reconstructs that line.}}

    Notes
    {ul
    {- {!Notty.I.string} rejects control characters (including newlines). The
       engine never emits newlines in span text but does not filter other
       control characters. Ensure [text] is renderable by Notty.}
    {- The TextMate tokenizer requires newline-terminated input lines; the
       engine appends a newline when tokenizing and strips it from spans.}}

    See also
    {ul
    {- {!module:Highlight_theme} for how themes map scopes to attributes.}
    {- {!module:Highlight_tm_loader} for loading and resolving TextMate
       grammars to use with {!with_registry}.}} *)

(** Opaque highlighter handle. Holds the selected theme and, optionally,
    a grammar registry used to resolve [lang] hints. *)
type t

(** A highlighted text fragment: [(attr, segment)].

    [segment] is a substring of a single input line (never includes a newline);
    [attr] is the display attribute to use when rendering that segment. *)
type span = Notty.A.t * string

(** [create ~theme] creates a highlighter configured to use [theme] when
    mapping token scopes to attributes.

    The [theme] is used to convert TextMate scopes (e.g. ["keyword"],
    ["string"], ["entity.name.function"]) to Notty attributes.

    Quick start – create an engine and highlight a short snippet:
    {[
      let engine =
        Chat_tui.Highlight_tm_engine.create
          ~theme:Chat_tui.Highlight_theme.default_dark
      in
      let lines =
        Chat_tui.Highlight_tm_engine.highlight_text
          engine ~lang:(Some "ocaml") ~text:"let x = 1\nin x"
      in
      List.length lines = 2
    ]} *)
val create : theme:Highlight_theme.t -> t

(** [with_theme t ~theme] returns a highlighter identical to [t] but using
    [theme].  Useful for switching between light/dark palettes. *)
val with_theme : t -> theme:Highlight_theme.t -> t

(** Extend the highlighter with a TextMate grammar registry.  The
    registry is created and populated via {!Highlight_tm_loader}.  When
    present, {!highlight_text} will attempt to resolve [lang] against the
    registry and colourise the text accordingly.  Without a registry the
    function falls back to plain, monospaced rendering.

    The call is side-effect free; it returns a new value and does not
    mutate the original highlighter. *)
val with_registry : t -> registry:Highlight_tm_loader.registry -> t

(** Additional information about a highlighting run. *)
type fallback_reason =
  | No_registry
  | Unknown_language of string
  | Tokenize_error

(** Reason a plain, non-colourized fallback was used.

    - [No_registry] — the highlighter did not carry a TextMate registry.
    - [Unknown_language l] — no grammar could be resolved for [l].
    - [Tokenize_error] — tokenization failed for at least one line. *)

type info =
  { fallback : fallback_reason option
    (** [fallback] is [Some _] if {!highlight_text} used plain rendering for
          all lines. When [None], colourisation was applied. *)
  }

(** [highlight_text t ~lang ~text] splits [text] into lines and produces, for
    each line, a list of [(attr, text)] spans to render left-to-right.

    Behaviour
    {ul
    {-  With a registry and known [lang], returns zero or more spans per
        input line, each with an attribute derived from the token’s scopes.}
    {-  Without a registry or on lookup/tokenization failure, returns one span
        per input line with attribute {!Notty.A.empty}.}
    {-  [lang] is a common language tag such as ["ocaml"], ["bash"],
        ["diff"], resolved via
        {!Highlight_tm_loader.find_grammar_by_lang_tag}.}
    {-  The TextMate tokenizer requires newline-terminated input lines; the
        engine appends a newline internally before tokenizing and strips it
        from the produced spans.}}

    Turning spans into a Notty image:
    {[
      let engine =
        Chat_tui.Highlight_tm_engine.create
          ~theme:Chat_tui.Highlight_theme.default_dark
      in
      let lines =
        Chat_tui.Highlight_tm_engine.highlight_text
          engine ~lang:None ~text:"hello\nworld"
      in
      (* Horizontally compose spans on a line, then stack lines vertically. *)
      let row spans =
        List.fold_left
          (fun img (attr, s) -> Notty.I.(img <|> Notty.I.string attr s))
          Notty.I.empty
          spans
      in
      let image =
        List.fold_left (fun acc l -> Notty.I.(acc <-> row l)) Notty.I.empty lines
      in
      let (_ : Notty.image) = image in
      ()
    ]} *)
val highlight_text : t -> lang:string option -> text:string -> span list list

(** [highlight_text_with_info] is like {!highlight_text} but also returns
    diagnostic information describing whether a fallback occurred.

    [fallback = Some (Unknown_language l)] indicates that no grammar could be
    resolved for [l]; [Some No_registry] means the engine was not configured
    with a registry; [Some Tokenize_error] signals that tokenization failed
    and plain rendering was used. *)
val highlight_text_with_info
  :  t
  -> lang:string option
  -> text:string
  -> span list list * info
