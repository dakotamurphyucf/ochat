# `Mime` – tiny helpers for content-type detection

`Mime` is a microscopic utility module that solves two everyday tasks that
come up whenever you need to serve files over HTTP or embed data in a JSON
payload:

1. **Guess a MIME type from a filename**
2. **Decide whether that MIME type is textual**

Both functions live in `lib/mime.{ml,mli}` and the library is published as
`ochat.mime`.

---

## Quick start

```ocaml
open Chatochat.Mime   (* or simply Mime if you opened the whole library *)

let guess  = guess_mime_type "report.pdf"    (* = Some "application/pdf" *)
let is_txt = is_text_mime "application/json" (* = false *)
```

---

## API overview

```ocaml
val guess_mime_type : string -> string option
val is_text_mime   : string -> bool
```

### `val guess_mime_type`

`guess_mime_type filename` looks at `filename`’s extension and returns one of
the built-in mappings from the table below.  The comparison is
case-insensitive.  If the filename has *no* extension the function returns
`None`.  When it does have an extension that is **not** in the table, the
generic fallback `application/octet-stream` is returned.

| Extension(s)                   | MIME type                 |
|--------------------------------|---------------------------|
| `.ml` `.mli` `.txt` `.md`      | `text/plain`              |
| `.json`                        | `application/json`        |
| `.csv`                         | `text/csv`                |
| `.png`                         | `image/png`               |
| `.jpg` `.jpeg`                 | `image/jpeg`              |
| `.gif`                         | `image/gif`               |
| `.pdf`                         | `application/pdf`         |
| *(anything else)*              | `application/octet-stream`|


### `val is_text_mime`

Simple predicate: `is_text_mime mime` is `true` if `mime` starts with the
prefix `"text/"` (ASCII, case-sensitive).  Useful when building JSON
structures where text can be inlined but binary data needs base64
encoding.


---

## Examples

### Serve a directory via Eio and label files correctly

```ocaml
let serve_file path flow =
  let filename = Filename.basename path in
  match Mime.guess_mime_type filename with
  | None -> respond_404 flow
  | Some mime ->
      respond_with_file ~headers:[ "Content-Type", mime ] path flow
```

### Inline text, base64-encode binaries

```ocaml
let add_blob_to_json path json_fields =
  let contents = Stdlib.In_channel.read_all path in
  let mime = Mime.guess_mime_type path |> Option.value ~default:"application/octet-stream" in
  let payload, field_name =
    if Mime.is_text_mime mime
    then (`String contents, "text")
    else (`String (Base64.encode_exn contents), "blob")
  in
  (field_name, payload) :: ("mimeType", `String mime) :: json_fields
```

---

## Design notes & limitations

* The extension table is intentionally *tiny* – it covers the file types we
  actually encounter in the host project.  Extend it if your application
  needs more.
* `is_text_mime` does not attempt to parse MIME parameters (e.g.
  `charset=utf-8`).  It merely checks a prefix.
* The implementation relies solely on `Core`’s `String` and `Filename`
  functions and therefore carries no extra dependencies.

---

## Implementation in a nutshell

```ocaml
let guess_mime_type filename =
  let _, ext_opt = Filename.split_extension filename in
  match ext_opt with
  | None -> None
  | Some ext ->
      Some (match String.lowercase ext with
            | ".ml" | ".mli" | ".txt" | ".md" -> "text/plain"
            | ".json" -> "application/json"
            | ".csv"  -> "text/csv"
            | ".png"  -> "image/png"
            | ".jpg" | ".jpeg" -> "image/jpeg"
            | ".gif"  -> "image/gif"
            | ".pdf"  -> "application/pdf"
            | _ -> "application/octet-stream")

let is_text_mime mime =
  String.is_prefix mime ~prefix:"text/"
```

Roughly two dozen lines – robust enough for small tooling but not meant as
an exhaustive solution.

---

## Acknowledgements

The extension-to-MIME mapping is a distilled subset of the IANA media type
registry.

