# `Fetch` – Minimal HTML downloader

> *“A big hammer for a small nail: pull  the page, parse it later.”*

`Fetch` is a **tiny wrapper** around `Io.Net.get` that retrieves a web
page as a raw UTF-8 string while applying a handful of hard safety
guards (gzip-aware, 5 MB cap, content-type filter).  It does **not**
attempt to parse or sanitise the document — that job belongs to
[`Html_to_md`](html_to_md.doc.md) and friends.

---

## Table of contents

1. [Quick start](#quick-start)
2. [API](#api)
3. [Behaviour details](#behaviour-details)
4. [Examples](#examples)
5. [Internals](#internals)
6. [Limitations](#limitations)

---

## Quick start

```ocaml
open Eio
open Webpage_markdown.Fetch

Eio_main.run @@ fun env ->
  match Fetch.get ~net:(Stdenv.net env) "https://ocaml.org" with
  | Ok html   -> Printf.printf "Page length: %d bytes\n" (String.length html)
  | Error err -> prerr_endline err
```

The call blocks until the server responds, decompresses the body if
needed, and enforces the 5 MB limit.

---

## API

```ocaml
val get :
  net:#Eio.Net.t ->               (* network capability *)
  string ->                       (* absolute URL        *)
  (string, string) Result.t       (* Ok html | Error msg *)
```

* `net` – capability obtained from `Eio.Stdenv.net`.  Using an explicit
  parameter makes the function *capability-safe*; tests can inject a
  mock implementation.
* `string` – absolute `https://…` URL.  Relative paths are accepted but
  the behaviour is undefined.

---

## Behaviour details

1. **TLS only** – Internally `Io.Net.get` establishes an HTTPS
   connection; plain-HTTP URLs are not rejected but will almost always
   fail because port 443 is used.
2. **One request / one response** – No redirects, no cookies, no
   keep-alives.  The function is deliberately minimal.
3. **Content-type filter** – Responses whose `Content-Type` header does
   not start with `text/html`, `text/plain`, or that are explicitly
   `application/json` are rejected.
4. **Gzip / deflate** – `Ezgzip.decompress` is invoked unconditionally.
   If the body is *not* compressed the function is essentially a no-op.
5. **Safety cap** – Pages larger than **5 000 000 bytes** after
   decompression are refused.
6. **JSON shortcut** – Some APIs return JSON with a misleading
   `text/html` header.  When detected, the JSON payload is returned as
   the error message so the caller can decide whether to handle it.

---

## Examples

### Downloading documentation and piping it to `lynx`

```ocaml
Eio_main.run @@ fun env ->
  match Fetch.get ~net:(Stdenv.net env) "https://ocaml.org/manual/" with
  | Ok html ->
      (* save the file so we can browse it with a text-based browser *)
      let cwd = Stdenv.cwd env in
      Io.save_doc ~dir:cwd "manual.html" html
  | Error e -> prerr_endline e
```

### Ignoring JSON replies

```ocaml
let load_or_cache_json ~net url =
  match Fetch.get ~net url with
  | Error json when String.is_prefix json ~prefix:"{" ->
      (* server lied about content-type, but we can still parse *)
      Jsonaf.pretty_to_string (Jsonaf.of_string json)
  | Ok _ | Error _ as res ->
      (* html was returned, or a genuine network error occurred *)
      Result.error "expected JSON"
```

---

## Internals

`Fetch.get` is a ~35-line function (see
[`fetch.ml`](fetch.ml)):

1. Splits the URL into `[host]` and `[path]` with helpers from
   `Io.Net`.
2. Issues an HTTPS GET and captures the raw response (`Raw` variant).
3. Applies the content-type checks described above.
4. Reads the full body with `Eio.Buf_read.take_all` (bounded by
   `max_size_bytes`).
5. Decompresses with `Ezgzip.decompress`.
6. Returns `Ok` / `Error` accordingly.

The function never raises; all failures are surfaced through the
`Result.t` channel.

---

## Limitations

* **No streaming** – Entire body is materialised in memory.
* **No redirects** – 3xx codes are not followed.
* **Single-shot** – No retry logic; wrap the call in your own retry loop
  if you need resilience.
* **5 MB hard limit** – Increase `max_size_bytes` in the implementation
  if you need bigger pages.


