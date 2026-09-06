# Context_compaction.Summarizer

Render ordered conversation items and request a textual summary for the
[compactor](../../context_compaction/compactor.doc.md). The compactor selects
the inputs. `Summarizer.summarise` itself does not grade them; the calling
compactor can apply opt-in relevance filtering before invoking this helper.

## High-level algorithm

1. Render supported input/output messages and function/custom-tool calls/results
   as role-labelled text. Multipart content stays ordered. Images become textual
   `<image src="..."/>` placeholders, not a separate vision request. Unsupported
   item kinds are omitted by `render_item`.
2. Without an Eio environment or without `OPENAI_API_KEY`, return `Ok` with the
   first 2000 **bytes** of this transcript. Presence, not nonblank validity, is
   the key check. This deterministic stub is not a semantic summary and its byte
   truncation is not UTF-8-boundary-aware.
3. Otherwise use nonstreaming Responses with the hard-coded `gpt-5.6-sol` model,
   developer instructions and user transcript, `max_output_tokens=100000`, no
   explicit temperature, and no explicit reasoning effort. This model/budget is
   independent of both the agent's ChatMD settings and typeahead settings.
4. Retry `Response_parsing_error` and `Response_stream_parsing_error` up to
   three attempts per request, sleeping one then two seconds. Other errors
   return directly; no general rate-limit/network retry is promised here.
5. If the full-history parsing retries exhaust and at least two compactable
   groups exist, try two sequential halves. Function/custom call-output groups
   remain together. Both halves include shared instructions/reminders; the first
   result is rolling context for the second. Each half has the same retry policy.
   Return the two summaries in labelled `<compaction-part>` blocks, without a
   final merge call. This path can make up to nine requests, not just one.
6. Return `Ok summary` or `Error exn`; cancellation is re-raised. Missing output
   text is an error, not a silently installed stub.

## Public API

```ocaml
val summarise
  :  relevant_items:Openai.Responses.Item.t list
  -> env:Eio_unix.Stdenv.base option
  -> (string, exn) result
```

`For_testing.render_transcript` exposes deterministic rendering;
`For_testing.summarise_with` injects the request and sleep callbacks for offline
retry/splitting tests. See the [interface](../../../lib/context_compaction/summarizer.mli).

## Usage example

```ocaml
let summarize_offline items =
  Context_compaction.Summarizer.summarise ~relevant_items:items ~env:None
```

The caller must handle the result and install history only after success.
Supplying an environment with a key opts into real requests; provider token
limits are not a monetary spending cap or a guaranteed summary length.

## Internal helpers

`retry_request` retries parsing failures; `grouped_items` keeps related tool
traffic together; `is_shared_context` identifies instructions and reminders.
The source [implementation](../../../lib/context_compaction/summarizer.ml) owns
the prompt and exact rendering rules.

## Known limitations / future work

- Summary quality is model-dependent and lossy. This standalone helper does not
  enforce a resulting-history budget or select relevant groups. The calling
  [compactor](../../context_compaction/compactor.doc.md) applies its configured
  token estimate and optional relevance threshold; use that entry point when
  those controls are required.
- The online path uses ordinary provider logging and records errors on stderr
  and through `Io.log`; it is **not** typeahead's private no-log transport.
- Multipart retries increase latency and potential cost. No new whole-operation
  timeout or billing cap is implemented by this helper.
- Offline truncation does not establish model-quality behavior.

## See also

- [Compaction policy and host ownership](../../context_compaction/compactor.doc.md)
- [Separate relevance-scoring helper](../../context_compaction/relevance_judge.doc.md)
- [Provider behavior](../openai/responses.doc.md)
