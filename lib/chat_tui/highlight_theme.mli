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
    {-  Given a non-empty list of scopes, select the attribute from the rule
        with the highest specificity across all scopes. Specificity is ordered
        by the tuple: (number of dot-separated segments in the selector; exact
        match vs prefix match; selector length in characters; earlier rule in
        the theme list).}
    {-  Exact matches win over prefix matches. More segments win over fewer. On
        a true tie, the earlier rule in the theme list wins.}
    {-  The order of [scopes] does not affect the result; the single best match
        across all provided scopes is returned.}
    {-  If none of the supplied scopes match any rule, or [scopes] is empty,
        {!Notty.A.empty} is returned.}}

    Notes
    {ul
    {-  Colours and styles come from {!module:Notty.A}. Compose attributes with
        {!val:Notty.A.(++)}. The visual result depends on terminal capabilities
        and palette configuration. See {!Notty.A} for attribute semantics.}
    {-  Helpers for colours and styles live in {!Chat_tui.Highlight_styles}.}
    {-  The theme is used by {!Chat_tui.Highlight_tm_engine} to colourize
        tokens. Callers can further compose attributes with {!Notty.A.(++)}.}
    {-  Resolution cost is linear in the number of rules times the number of
        scopes provided.}}
  *)

(** Opaque theme value. Represents an ordered list of [prefix -> attr] rules. *)
type t

(** [empty] is the theme with no rules. Always returns {!Notty.A.empty}. *)
val empty : t

(** [default_dark] is a built-in palette optimised for dark terminals.

    - Uses the ANSI 16-colour names and extended 256-colour helpers from
      {!Notty.A} (e.g. {!Notty.A.gray}, truecolor via {!Notty.A.rgb_888}, and {!Notty.A.lightwhite}).
    - Aims for good contrast on dark backgrounds while keeping tokens
      distinguishable. *)
val default_dark : t

(** [default_light] mirrors {!default_dark} but is tuned for light backgrounds. *)
val default_light : t

(** [github_dark] matches GitHub Dark Default token colours using truecolor
    (24-bit). Strings are light azure,
    keywords salmon, constants/support azure, functions and types purple,
    HTML/XML tags green, variables orange, comments muted gray. Links are
    underlined; inline code uses a subtle chip background.

    Uses helpers from {!Chat_tui.Highlight_styles} (e.g. [fg_hex], [bg_hex])
    to approximate the theme’s truecolor definitions. *)
val github_dark : t

(** [attr_of_scopes theme ~scopes] returns the attribute from the rule with the
    highest specificity across all [scopes]. Specificity is determined by:
    segment count (more segments win), exactness (exact match wins over prefix
    match), selector length (longer wins), then earlier appearance in [theme].
    The order of [scopes] is irrelevant.

    The function is inexpensive (linear in the number of rules times the
    number of scopes) and suitable for per-token calls in the renderer.

    Example – obtain an attribute for an OCaml keyword and add underline:
    {[
      let open Chat_tui.Highlight_theme in
      let base = attr_of_scopes default_dark ~scopes:[ "keyword"; "source.ocaml" ] in
      let attr = Notty.A.(base ++ st underline) in
      let (_ : Notty.A.t) = attr in
      ()
    ]} *)
val attr_of_scopes : t -> scopes:string list -> Notty.A.t
