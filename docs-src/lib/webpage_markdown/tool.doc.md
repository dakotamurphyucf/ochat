# `Webpage_markdown.Tool`

## 1  Overview

Register the built-in `webpage_to_markdown` tool as an `Ochat_function.t`.
The driver fetches content, handles supported GitHub blob URLs, and converts
HTML to Markdown. A process-global, URL-keyed TTL/LRU cache stores up to 128
conversions for five minutes. It is shared by registered instances, not
partitioned per agent or persisted across restarts.

## 2  API

`register ~env ~dir ~net` returns tool metadata and typed execution functions.
`dir` is unused. Callers supply serialized JSON containing a `url` field,
not a bare URL string. The runner returns
`Openai.Responses.Tool_output.Output.t`, not a plain string.

## 3  Examples

### 3.1  Minimal integration

```ocaml
open! Core

let register env =
  let tool =
    Webpage_markdown.Tool.register
      ~env
      ~dir:(Eio.Stdenv.cwd env)
      ~net:(Eio.Stdenv.net env)
  in
  let metadata, dispatch = Ochat_function.functions [ tool ] in
  let run = Hashtbl.find_exn dispatch "webpage_to_markdown" in
  let fetch url =
    let arguments = Jsonaf.to_string (`Object [ "url", `String url ]) in
    run ~invocation:Ochat_function.Invocation.silent arguments
  in
  metadata, fetch
;;

let fetch_text env url =
  Webpage_markdown.Driver.fetch_and_convert ~env ~net:(Eio.Stdenv.net env) url
  |> Webpage_markdown.Driver.Markdown.to_string
;;
```

Calling `register env` only constructs metadata and a callable; invoking the
returned `fetch` performs network work. Its result is currently `Output.Text`.
The dispatch table is keyed by tool name; the runner requires an invocation.
Use an observed invocation for tools that emit progress, or `Invocation.silent`
when only the final output is needed. This adapter does not emit live progress.

This [compiled example](../../../test/agent_docs/docs_example_webpage.ml) is
checked for exact source parity by the opt-in documentation gate. Verification
registers the tool without invoking its network callable.

### 3.2  Programmatic use

The `fetch_text` helper above bypasses tool dispatch and explicitly converts
the driver's opaque `Markdown.t` through `Markdown.to_string`.
For local HTML without a network request use
`Driver.convert_html_file path |> Driver.Markdown.to_string`.

## 4  Error semantics

Exceptions inside the URL runner become `Output.Text` containing
`Error fetching <url>: <exception-message>\n`; this broad catch includes
cancellation. JSON argument decoding happens before that catch and can raise.
Failures already represented as diagnostic text by the driver can be cached
like successful Markdown. A text result is not proof of a successful fetch.

## 5  Limitations & notes

- Inherit the driver's [fetch and fallback limitations](driver.doc.md) and
  [network/size/cancellation boundaries](fetch.doc.md).
- Only the Chrome fallback has its own 60-second timeout; that is not a
  deadline for the complete tool invocation.
- There is no JavaScript interaction, semantic validation or fresh-content
  guarantee. Cached entries expire lazily; concurrent misses can fetch twice.
- The adapter is not an authorization boundary. The owning host must enforce
  its tool policy before invoking the runner.
