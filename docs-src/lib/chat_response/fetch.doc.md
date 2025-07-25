Fetch – Document retrieval utilities
====================================

High-level overview
-------------------

`Fetch` wraps the low-level I/O primitives of the **ChatMarkdown** stack.
It answers two common needs:

* Bring a local or remote resource into memory as an OCaml `string`.
* Convert HTML pages to plain text so that language-model context is not
  wasted on markup.

API reference
-------------

| Function | Purpose |
|----------|---------|
| `clean_html` | Strip markup &amp; compress whitespace from an HTML document. |
| `tab_on_newline` | Indent every newline with two TABs – handy for raw XML blocks. |
| `get_remote` | Blocking HTTP GET via Eio (`Accept: */*`). |
| `get` | General entry point – fallbacks to local disk when `is_local` is `true`. |
| `get_html` | Same as `get` but post-processed by `clean_html`. |


Usage example
-------------

```ocaml
open Chat_response.Fetch

let summary_of_readme ctx repo_url =
  (* GitHub raw URL – we want only text *)
  let raw_html = get_html ~ctx repo_url ~is_local:false in
  (* Pass plain text to the LLM or do further processing … *)
  raw_html
```


Implementation notes
--------------------

* Remote requests rely on `Io.Net.get`, therefore redirects are **not**
  automatically followed.
* A very small subset of HTML sanitisation is performed – the goal is to
  keep the helper lightweight and dependency-free.
* The helpers are **synchronous**; they should only be used for assets
  below ~1 MiB.

Known limitations
-----------------

* `get_remote` does not handle HTTP status codes other than `200` – the
  caller must wrap it in a `try…with` if they need structured error
  handling.

