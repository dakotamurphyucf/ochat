# `Md_render` – minimal Markdown pretty-printer

`Md_render` converts an [`Omd.doc`](https://github.com/ocaml/omd) – the
abstract syntax tree used by the *Omd* Markdown library – into a textual
Markdown string.  It is a **loss-less companion** to
[`Html_to_md`](./html_to_md.doc.md): the latter translates noisy HTML into an
AST, the former prints the AST back as clean, readable Markdown.

---

## Why another renderer?

Most Markdown renderers aim for *fidelity* to the original source.  In the
web-scraping workflow of *webpage_markdown* the goal is different: we start
from arbitrary HTML, not from Markdown.  The only requirement is that the
output:

1. Renders correctly on GitHub, GitLab and most CommonMark viewers.
2. Preserves every bit of information, even when the input contains elements
   that have no direct Markdown equivalent.

`Md_render` achieves this by supporting exactly the subset of the AST that
`Html_to_md` produces and falling back to fenced `html` blocks otherwise.

---

## Supported constructs

* **Paragraphs** – separated by a blank line.
* **Headings** – `#`, `##`, … up to level 6.
* **Inline formatting** – emphasis (`*italic*`), strong emphasis
  (`**bold**`), code spans with automatic back-tick escaping, links and
  images.
* **Lists** – bullet (`*`) and ordered (`1.`) lists with proper indentation
  for nested items.
* **Block quotes** – `> ` prefix with recursive support for multiple levels.
* **Code blocks** – fenced blocks with an optional language tag.  The fence
  length is picked to be one back-tick longer than the longest run inside the
  code so that the result cannot be ambiguous.
* **Tables** – GitHub-style pipe tables.

Unsupported nodes are emitted verbatim inside:

```markdown
```html
<original-html/>
```
```

---

## Quick example

```ocaml
open Omd

let doc : Omd.doc =
  [ Heading ([], 2, Text ([], "Example"));
    Paragraph
      ( [],
        Concat
          ( [],
            [ Text ([], "Escaping ");
              Code ([], "*weird* `chars`");
              Text ([], " is automatic.") ] ) ) ]

let md = Md_render.to_string doc
(* md =
   "## Example\n\nEscaping `*weird* `chars`` is automatic." *)
```

---

## Reference

```ocaml
val to_string : Omd.doc -> string
```

O( n ) in the size of the AST.  Never raises.

---

## Limitations & gotchas

* **Footnotes, strikethrough, task lists** – not currently emitted by
  `Html_to_md`; therefore not handled here.
* **Whitespace inside cells** – pipe tables keep the original whitespace, so
  column alignment is left to the renderer.
* **Absolute determinism** – the output is stable for a given AST, but the
  AST produced by `Html_to_md` can vary slightly between versions.  Cache the
  rendered string if that matters for you.

---

## See also

* [`Html_to_md`](./html_to_md.doc.md) – HTML → Markdown converter.
* [`driver`](./driver.doc.md) – fetches remote URLs before conversion.

