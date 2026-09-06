# Webpage_markdown.Fetch — HTML/text downloader

[Interface](../../../lib/webpage_markdown/fetch.mli) ·
[source](../../../lib/webpage_markdown/fetch.ml).

## Quick start

```ocaml
let fetch_page env url =
  Webpage_markdown.Fetch.get ~net:(Eio.Stdenv.net env) url
```

The Result contains the body or an error string. This performs real network I/O;
it is not an offline example. See [web ingestion](../../overview/tools.md).

## API

`get ~net url : (string, string) result` downloads a whole body.
It does not parse HTML into Markdown; the driver/converter does that separately.

## Behaviour details

1. The adapter extracts host and path, then Io.Net.get constructs HTTPS.
   The supplied URL's query, fragment and explicit port are not preserved.
   Plain HTTP is not a supported passthrough.
2. There is no redirect-following, retry, or HTTP-status validation layer here.
3. A missing Content-Type is accepted. Case-sensitive prefixes text/html,
   text/plain and application/json are recognized; other types are rejected.
4. Accepted bodies are read with a 5,000,000-byte input bound.
   Ezgzip.decompress is attempted; decompression failure retains the original
   body. A successfully expanded body is checked against the same length.
5. **JSON is identified only by the Content-Type prefix.** Its body is returned
   as Error; JSON mislabeled as text/html is returned as Ok without detection.
6. Exceptions, including Eio cancellation, are caught and converted to strings.

## Examples

Inspect errors without assuming every Error contains safe diagnostics: a JSON
response can itself contain arbitrary private/server-controlled content.
Do not parse an error as JSON unless your application intentionally expects it.

## Internals

The compressed-size reader bound and the **post-decompression** length check
are different. Decompression can allocate an expanded body before rejecting its
size. This is not a decompression-memory or total request-time bound.

## Limitations

- Io.Net's null TLS authenticator does not verify certificates.
- URL query/port loss can fetch a different resource than the caller intended.
- Cancellation is not preserved as an exception at this boundary.
- Decompression can allocate beyond the advertised final body limit.
- Whole-body reads and unrestricted hosts are not SSRF protection.

These are existing implementation limitations, not guarantees introduced by
Result-returning APIs. The [audit ledger](../../development/code-documentation-audit.md)
records follow-up hardening separately.
