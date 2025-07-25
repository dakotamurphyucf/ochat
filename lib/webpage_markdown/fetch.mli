open! Core

(** Lightweight HTTP helper to retrieve remote HTML documents.

    {1 Overview}

    The [Fetch] module is a single-function API built on top of
    {!Io.Net.get}.  It takes care of:

    • Following *only* the HTTPS GET happy-path (no redirects).
    • Transparent *gzip* / *deflate* decompression (even when the server
      forgets to set a [Content-Encoding] header).
    • Basic *content-type* vetting – `text/html`, `text/plain` or no
      header are accepted; everything else is rejected early.
    • A safety cap of 5 MB on the {e decompressed} body to avoid
      pathological pages.

    The implementation is intentionally minimal because downstream
    modules ({!Webpage_markdown.Html_to_md}, {!Webpage_markdown.Md_render}
    …) only need a raw HTML string – no cookies, no CSP, no streaming.
*)

(** [get ~net url] downloads the body of [url] and returns it as a
    UTF-8 string.

    A best-effort attempt is made to decompress [gzip] and [deflate]
    payloads.  The function fails if the resulting string is larger than
    five megabytes.

    @param net Active {!Eio.Net} capability obtained from the ambient
               environment (e.g. [Eio.Stdenv.net env]).

    @return [Ok html] on success.
    @return [Error msg] on:
            {ul
              {- network failure (timeout, TLS error, …)}
              {- unsupported [Content-Type] header}
              {- decompressed size > 5 MB}
              {- `application/json` responses – the JSON is returned in
                 [msg] for diagnostics}}
*)
val get : net:_ Eio.Net.t -> string -> (string, string) Result.t
