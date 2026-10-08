# Responses DTOs and codec validation

`Openai.Responses` retains historical typed input, output and stream records for
explicit local projections. It performs no environment discovery, authentication
or HTTP requests. The unused `post_response` and `post_private_response_exn`
network entry points have been removed; their module-level key and endpoint
capture could not support deliberate profile selection or reliable removal.

Provider requests use the common [Responses driver](../../../lib/openai/responses_driver.mli)
through the [explicit inference host](../neutral-inference.md). The runtime host
owns credentials and selects the exact profile/account/endpoint before dispatch.
Ordinary inference never initiates login or chooses another account when a key
is missing. Prepared plans capture authorization epochs independently; dispatch
still freshly authorizes and checks the source lease before publishing headers.

Use [Responses wire codecs](../openai-responses-codecs.md) for new integrations.
Those captures retain unknown fields, opaque reasoning and absent/null/value
semantics. Historical DTO projections cannot preserve every wire field and are
not a substitute for the lossless history boundary.

`validate_response_stream` remains a pure lazy validator. It requires exactly one
successful terminal response and rejects failure, incomplete, error, trailing
events or termination before completion. `read_private_response_exn` remains an
explicit bounded historical body reader, with a 256 KiB ceiling and no request
or authentication. Its raw exceptions may contain private data and must not be
printed or persisted.

Private helpers such as [typeahead](../chat_tui/type_ahead_provider.doc.md) use a
bounded host execution through the same driver. They receive no alternate key,
account or ambient endpoint fallback. Provider dispatch does not log raw bodies.
