(** Attribute theming for syntax highlighting.

    Map TextMate-style scope names (e.g. ["keyword.operator"], ["string"]) to
    {!Notty.A.t} attributes used by the TUI renderer.

    Definitions
    {ul
    {-  A theme is an ordered list of rules, each rule pairing a scope
        prefix with a {!Notty.A.t}.}
    {-  A rule matches a scope when its [prefix] is a dot-segment-aware prefix
        of the scope: it matches if the scope is exactly [prefix] or starts
        with [prefix ^ "."]. This avoids accidental matches such as
        ["source.js"] matching ["source.json"].}}

    Resolution
    {ul
    {-  For each scope in [scopes], consider all rules whose [prefix] matches
        that scope on dot-segment boundaries (see above).}
    {-  Each matching rule is assigned a specificity key
        [(segments, exact)] where [segments] is the number of dot-separated
        segments in [prefix] and [exact] is [1] for an exact match and [0] for
        a proper prefix. Rules with larger keys are more specific.}
    {-  Among all matches across all scopes, only rules with maximal
        specificity contribute to the result. Their attributes are composed in
        theme order using {!Terminal_highlighting.Highlight_styles.(++)}. Later rules in
        this maximum-specificity group override earlier ones for overlapping
        properties (e.g. foreground colour).}
    {-  The order of [scopes] does not affect the result; they are treated as
        a set. If none of the supplied scopes match any rule, or [scopes] is
        empty, {!Terminal_highlighting.Highlight_styles.empty} (equivalently
        {!Notty.A.empty}) is returned.}}

    Notes
    {ul
    {-  Colours and styles come from {!module:Notty.A}. The visual result
        depends on terminal capabilities and palette configuration. See
        {!Notty.A} for attribute semantics.}
    {-  Helpers for colours and styles live in {!Terminal_highlighting.Highlight_styles}.}
    {-  The theme is used by {!Terminal_highlighting.Highlight_tm_engine} to colourize
        tokens. Callers can further compose attributes with
        {!Terminal_highlighting.Highlight_styles.(++)}.}
    {-  Resolution uses an immutable precompiled index. Direct calls are pure;
        {!Highlight_theme_resolver} owns the bounded cache used by the span
        engine for repeated per-token lookups.}}
  *)

(** Opaque theme value. Represents an ordered list of [prefix -> attr] rules. *)
type t

(** [empty] is the theme with no rules. Always returns {!Notty.A.empty}. *)
val empty : t

(** [github_dark] matches GitHub Dark Default token colours using truecolor
    (24-bit). Strings are light azure,
    keywords salmon, constants/support azure, functions and types purple,
    HTML/XML tags green, variables orange, comments muted gray. Links are
    underlined; inline code uses a subtle chip background.

    Uses helpers from {!Terminal_highlighting.Highlight_styles} (e.g. [fg_hex], [bg_hex])
    to approximate the theme’s truecolor definitions. *)
val github_dark : t

(** [attr_of_scopes theme ~scopes] computes the attribute for [scopes] using
    [theme].

    Among all rules whose prefixes match at least one scope on dot-segment
    boundaries, rules with maximal specificity are selected, where specificity
    is [(segments, exact)] as described in the module documentation. Their
    attributes are composed in theme order using
    {!Terminal_highlighting.Highlight_styles.(++)}. If no rule matches, the result is
    {!Terminal_highlighting.Highlight_styles.empty}.

    This direct lookup is pure and uses the immutable precompiled index.
    The per-engine {!Highlight_theme_resolver} caches repeated per-token
    lookups with bounded retained state.

    Example – obtain an attribute for an OCaml keyword and add underline:
    {[
      let theme = Terminal_highlighting.Highlight_theme.github_dark in
      let base =
        Terminal_highlighting.Highlight_theme.attr_of_scopes
          theme ~scopes:[ "keyword"; "source.ocaml" ]
      in
      let attr = Terminal_highlighting.Highlight_styles.(base ++ underline) in
      let (_ : Notty.A.t) = attr in
      ()
    ]} *)
val attr_of_scopes : t -> scopes:string list -> Notty.A.t
