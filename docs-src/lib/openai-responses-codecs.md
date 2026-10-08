# Responses wire codecs

`Openai.Responses.Codec` is the pure protocol boundary for new OpenAI provider integrations. `Request` validates and encodes selected locally managed requests; `Wire` captures output JSON with typed projections; `Stream` frames and validates SSE. These modules perform no network calls, authentication, retries, tool execution or session persistence.

The common Responses driver and inference adapter use this lossless boundary. The unused historical `Responses.post_response` transport has been removed. Historical DTOs remain explicit projections; the older reasoning record cannot retain encrypted content. Do not project a capture through that record and expect lossless history.

## Requests

`Request.create` requires a model ID, complete local input array and explicit streaming choice. It emits `store:false`, `truncation:"disabled"` and, by default, `include:["reasoning.encrypted_content"]`. It introduces no default model, token cap or output format. `Field.Absent`, `Null` and `Value` have field-specific validation: nullable instructions can be null; `text.format` cannot. Omission of the text format remains distinct from explicit `{ "type": "text" }`.

Selected options include reasoning effort/summary, text verbosity and structured output formats, local function/custom schemas and tool choices, generation settings, and cache settings. These describe wire shapes; the host must still check model/account/endpoint eligibility and local tool permissions. Async declarations do not activate async tool execution. Namespace declarations and tool discovery are deferred to their capability integration work.

Provider conversations, previous-response references, hosted tools, remote compaction, provider file IDs and automatic truncation are rejected by this selected request profile. Local text/image/file inputs and tool outputs remain available. The host owns asset access and call/result correlation.

## Captures and streaming

Create `Wire.Origin` from the resolved provider/account/endpoint references, then decode response or event JSON. Origin labels are caller-supplied compatibility information, not authentication or a replay permission. Captures retain the immutable original JSON value and unknown fields, ordered content, phase, refusal, exact argument/custom-input strings and opaque reasoning. Raw access preserves JSON values and field order, not original JSON whitespace/escape spelling. Keep captures private; the codecs do not log them.

Malformed selected semantic fields produce a structured error. This is a semantic projection validator, not a complete SDK schema validator: unprojected response configuration remains raw, and unavailable optional envelope data is not invented. Unknown item/event/part types remain raw, without granting local execution eligibility. Namespaced, asynchronous or unsupported-caller tool calls are not emitted as selected local calls. `local_call` is a semantic projection, never authorization.

`Stream.feed_line` accepts newline-stripped SSE lines, including CRLF, comments and multiline data. A blank line dispatches a frame. EOF or `[DONE]` cannot manufacture completion. `finish` distinguishes missing-terminal truncation from completed, incomplete and provider-failed inference; refusal remains inspectable content/outcome. Inference completion does not complete an OChat operation or goal.

Each stream owns one tracker for one inference attempt. It reconciles output slots, provider item IDs and final payloads, rejects contradictory finalization and suppresses repeated finalized calls. `newly_finalized` is evidence for the existing host admission path, not a second executor. Do not retry or switch transports after uncertain delivery on the strength of a codec result alone.

Usage counts are nonnegative `int64` values. Missing/null usage remains unavailable. Cached, cache-write and reasoning detail counts are components, not extra tokens to add to totals. Usage aggregation and revision ownership belong to the provider/runtime integration.

## Sources and profile qualification

The selected shapes were checked against the current [Responses create reference](https://developers.openai.com/api/reference/typescript/resources/responses/methods/create), [response types](https://developers.openai.com/api/reference/typescript/resources/responses), and [streaming events](https://developers.openai.com/api/reference/resources/responses/streaming-events), retrieved October 6, 2026. Codex source was pinned to [`73178e7ca60fe8655c49727e60a467d7c03f896c`](https://github.com/openai/codex/tree/73178e7ca60fe8655c49727e60a467d7c03f896c), the observed `main` at the start of this implementation.

| Feature | Public API evidence | Codex/OAuth evidence and qualification |
| --- | --- | --- |
| Full input, store false, streaming and opaque reasoning include | Selected documented request shapes | Present in pinned Codex request construction; not live account/model qualification |
| Local function/custom calls, phase and encrypted reasoning | Selected documented item shapes | Pinned Codex item models retain related fields; first-party extras remain opaque |
| Structured text format, reasoning/generation/cache settings | Shape and nullability checked independently | Sharing names does not establish route support; preparation must gate each option |
| Usage and detail nulls | Reference signatures and examples differ | Official SSE examples and Codex fixtures admit null/missing values; retain unavailable values rather than defaulting to zero |
| Async, namespaces and discovery | Some schemas expose these options | No execution/profile entitlement inferred; separate integration work must qualify them |
| Provider state, hosted tools and compaction | Exist in broader API | Excluded from this locally managed profile |

Offline tests use independently authored request JSON, expected emitted keys, raw output objects and SSE sequences. They test actual PPX optional/nullable behavior, exact call strings, refusal/opaque data, unknown fields, malformed inputs, terminal reconciliation and truncation. They do not claim live endpoint, OAuth, WebSocket or runtime migration qualification.
