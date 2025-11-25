# Highlight_styles - concise constructors for Notty attributes

chat_tui/highlight_styles provides small, readable helpers for building
Notty.A.t attributes used across the TUI highlighting pipeline. It wraps
common Notty.A constructors (bold/italic/underline, grayscale ramp, 6×6×6
color cube helpers, truecolor, and ANSI named colours) into short functions
and values, making theme definitions concise and uniform.

Used by: [Highlight_theme](highlight_theme.doc.md)

---

## Overview

- Encapsulates Notty.A attribute construction in a tiny API.
- Encourages composition with the (++) operator from Notty.A.
- Exposes helpers for grayscale and the 256-color RGB cube, truecolor (24-bit),
  plus named ANSI colours.
- Values are plain Notty.A.t, so they compose with any other Notty attributes.

Terminal support caveats follow Notty:
- Grayscale levels are integers in [0, 23].
- 256-color cube channels are in [0, 5] per channel.
- Truecolor channels are in [0, 255] per channel.
- Terminals may remap or ignore extended/true colours; behaviour is terminal-dependent.

See also the upstream Notty.A docs for full semantics of attributes and their
composition and terminal support notes.

---

## API Cheatsheet

- (++) : Notty.A.t -> Notty.A.t -> Notty.A.t — compose attributes, left-associative.
- bold, italic, underline — style attributes.
- empty — terminal defaults (no styles set).
- fg_gray : int -> Notty.A.t / bg_gray : int -> Notty.A.t — grayscale 0-23.
- fg_rgb : r:int -> g:int -> b:int -> Notty.A.t / bg_rgb : ... — truecolor 0-255 per channel.
- fg_rgb6/bg_rgb6 : r:int -> g:int -> b:int -> Notty.A.t — 256-color cube helpers (0-5 per channel) mapped to truecolor approximations.
- fg_hex/bg_hex : string -> Notty.A.t — parse hex colors like #RRGGBB, #RGB; fall back to empty on parse failure.
- Named foreground: fg_black, fg_red, fg_green, fg_yellow, fg_blue, fg_magenta, fg_cyan, fg_lightwhite.
- Named background: bg_black, bg_white, bg_lightwhite.

---

## Detailed function reference

### (++)
Compose two attributes. Later values take precedence for foreground/background if set; styles are unioned. Forms a monoid with empty.

Example:
```ocaml
let a = Notty.A.(fg red)
let b = Notty.A.(st bold)
let composed = Chat_tui.Highlight_styles.(a ++ b)
(* composed: red, bold *)
```

### bold, italic, underline
Set the corresponding text style. Compose with other attributes using ( ++ ).

Example:
```ocaml
let emph = Notty.A.(Chat_tui.Highlight_styles.bold ++ fg Notty.A.lightwhite)
let img = Notty.I.string emph "important"
```

### empty
The empty attribute: default terminal colours, no styles.

### fg_gray n / bg_gray n
Set foreground/background to grayscale level n.
- Precondition: 0 <= n <= 23
- Raises Invalid_argument if n is out of range (from Notty.A.gray).

Example:
```ocaml
let subtle = Chat_tui.Highlight_styles.fg_gray 9
let img = Notty.I.string subtle "."
```

### fg_rgb ~r ~g ~b / bg_rgb ~r ~g ~b
Set foreground/background using truecolor (24-bit).
- Precondition: 0 <= r,g,b <= 255
- Raises Invalid_argument if a channel is out of range (from Notty.A.rgb_888).

Example:
```ocaml
let cyan = Chat_tui.Highlight_styles.fg_rgb ~r:0 ~g:200 ~b:200
let chip = Notty.I.string Notty.A.(cyan ++ st Notty.A.bold) "link"
```

### fg_rgb6 ~r ~g ~b / bg_rgb6 ~r ~g ~b
Set using the 256-color RGB cube helpers (channels 0..5), mapped to their XTerm approximations and applied via truecolor. Useful for legacy palettes and portability across 256-color terminals.
- Precondition: 0 <= r,g,b <= 5
- Raises Invalid_argument if a channel is out of range.

Example:
```ocaml
let orange = Chat_tui.Highlight_styles.fg_rgb6 ~r:5 ~g:3 ~b:0
let img = Notty.I.string orange "warning"
```

### fg_hex hex / bg_hex hex
Parse a hex color and apply it to foreground/background. Accepted forms: #RRGGBB, RRGGBB, #RRGGBBAA/RRGGBBAA (alpha ignored), #RGB/RGB. Returns empty if parsing fails.

Examples:
```ocaml
let accent = Chat_tui.Highlight_styles.fg_hex "#0aa"
let bg = Chat_tui.Highlight_styles.bg_hex "112233"
let img = Notty.I.string Notty.A.(accent ++ bg) "hex"
```

### Named ANSI colours (foreground)
Convenience attributes: fg_black, fg_red, fg_green, fg_yellow,
fg_blue, fg_magenta, fg_cyan, fg_lightwhite.

Example:
```ocaml
let ok = Chat_tui.Highlight_styles.fg_green
let err = Chat_tui.Highlight_styles.fg_red
let img = Notty.I.(string ok "\226\156\147" <|> string err "\226\156\151")
```

### Named ANSI colours (background)
Convenience attributes: bg_black, bg_white, bg_lightwhite.

---

## Examples

Building a minimal theme entry for Markdown headings:
```ocaml
let heading_attr =
  Chat_tui.Highlight_styles.(fg_rgb ~r:0 ~g:200 ~b:200 ++ bold)
```

Using attributes to render a row:
```ocaml
let render_pair (a1,s1) (a2,s2) =
  Notty.I.(string a1 s1 <|> void 1 0 <|> string a2 s2)

let img =
  let open Chat_tui.Highlight_styles in
  render_pair (fg_gray 10, "comment") (fg_rgb6 ~r:5 ~g:4 ~b:0, "number")
```

---

## Known issues and limitations

- Terminal differences: Extended colours (grayscale, 256-color) and truecolor depend on terminal support; some terminals remap palettes or ignore unsupported modes.
- Composition order matters: (a ++ b) prefers b's colours when set; styles are unioned.

---

## See also

- Notty.A documentation — colour spaces, styles, and attribute composition
- [Highlight_theme](highlight_theme.doc.md) — uses these helpers to map scopes to attributes
