# `Chat_response.Fetch`

Asynchronous helpers for turning a *URL* – either a local path or an
`http(s)` address – into the textual representation expected by the
ChatMarkdown → OpenAI pipeline.

The implementation purposefully avoids sophisticated features such as
redirect-following, streaming or connection pooling in favour of a
minimal surface area that is easy to reason about and adequate for the
small (≤ 1 MiB) resources typically embedded in prompts.

## Quick tour

```ocaml
(* Assume we already have an execution context: *)
let ctx = Ctx.of_env ~env ~cache:(Cache.create ())

(* 1.  Read a local README *)
let readme = Fetch.get ~ctx "README.md" ~is_local:true

(* 2.  Grab OCaml’s homepage as visible text *)
let ocaml_txt =
  Fetch.get_html ~ctx "https://ocaml.org" ~is_local:false

(* 3.  Prepare a multi-line assistant reply *)
let payload =
  "<assistant>" ^ (Fetch.tab_on_newline "Line one\nLine two") ^ "</assistant>"
```

## Function reference

### `get`

`get ~ctx url ~is_local` → `string`

Retrieve *raw* content.

* `~ctx` — immutable execution context providing network (`ctx#net`) and
  file-system (`Ctx.dir ctx`) capabilities.
* `url` — file path or `http(s)` address.
* `~is_local` — `true` for disk access, `false` for network.

Resolution rules for local paths:

1. First try `Filename.concat (Ctx.dir ctx) url`.
2. If the previous step failed *and* `url` is relative, retry relative
   to `ctx#cwd` (so that paths pasted interactively behave like a shell
   prompt).

An exception is raised when the file cannot be found or the HTTP
request fails (non-200 status or network error).

### `get_html`

Identical to `get` but returns a *sanitised* version of an HTML
document:

* Attempts a best-effort gzip decompression first (many servers enable
  transparent compression).
* Uses *LambdaSoup* to strip markup and collapse consecutive
  whitespace.


### `tab_on_newline`

`tab_on_newline s` → `string`

Insert two tab characters after every newline in `s`.  The helper is a
small convenience for embedding multi-line payloads into *indented*
ChatMarkdown blocks where correct indentation is required (e.g.
`<assistant>` raw payloads).

## Known limitations

* **No redirect handling** — a `301`, `302`, … response raises.
* **No streaming support** — large files are read entirely into memory.
* **Shallow sanitisation** — `get_html` removes tags and compresses
  whitespace but does not decode entities or execute powerful cleaning
  such as CSS/JS removal.

## See also

* [`Io`](../Io.doc.md) for generic file and network utilities.
* [`Chat_response.Ctx`](./ctx.doc.md) for the execution context type
  used by *Fetch*.
* [`Chat_response.Converter`](./converter.doc.md) for the module that
  consumes *Fetch* to build OpenAI request payloads.

