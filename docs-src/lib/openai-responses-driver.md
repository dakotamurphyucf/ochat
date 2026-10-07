# Explicit Responses preparation and HTTP/SSE driver

`Openai.Responses_driver` is the provider-local inference boundary around
`Openai.Responses.Codec`. The [neutral inference runtime](neutral-inference.md)
selects it through `Openai.Inference_adapter`. The driver neither executes tools nor
allocates or persists conversation entries. Host moderation, final admission,
canonical history, tool permissions, execution, recovery and final run completion
remain with their existing owners.

## Preparation and profile declarations

Construct an immutable `Profile` with a nonsecret profile ID, optional account
reference, canonical absolute Responses endpoint, capability declarations and
profile defaults. HTTPS endpoints require DNS hostnames and use certificate chain
and hostname verification with system roots via `Ca_certs` and `Tls_eio`.
HTTP endpoints are admitted only for literal loopback integration testing.
Userinfo, query, fragments, malformed hostnames and invalid ports reject.

There is no model catalog or default model. Each preparation explicitly supplies
its model string. `Capability.create` validates a declared baseline and optional
model declarations. The baseline applies to arbitrary model strings. Optional
features default to `Unknown`; optional requested features require `Supported`.
An explicit `Unsupported` at either layer wins over affirmative support. A
profile declaration grants eligibility only for this adapter's implemented
encoders; it does not enable hosted tools, provider-owned history, WebSockets,
OAuth login or a future asynchronous tool mapping. M2 can supply declarations
from its catalog without introducing another inference driver.

Settings use the codec's omission/null/value semantics and record provenance.
The effective order is execution override, captured prompt, profile default,
then omission (provider default). `Absent` does not erase a lower layer; `Null`
is an explicit effective value, validated by the codec's field policy. Duplicate
keys within one layer and unknown setting names reject. Each effective optional
setting requires affirmative capability support; unsupported/unknown settings
and invalid values fail during pure preparation, before authentication/network.

`Prepared.create` receives the complete effective ordered history, selected tool
schemas and settings. Final guidance must already be present. Raw history
objects retain unknown fields, exact call strings, presence and opaque reasoning.
Opaque replay requires affirmative support; the host is responsible for matching
capture origin before passing raw opaque history. A direct caller marker is replayable and eligible for the existing local tool
path; unknown/program caller metadata, non-null namespace metadata, and
`async=true` reject until their host mapping is selected.
Null metadata is retained only where the selected codec field is nullable;
for example, `function_call.namespace` remains non-null while
`function_call_output.namespace` permits null. Tool names are unique, and
schemas grant no authority.

The host resolves assets before capture. Images use validated base64 data URIs;
files use base64 bytes or base64 data URIs. Remote URLs and provider-only file IDs
cannot represent the immutable effective asset and reject. Captured assets are
inside the request JSON; no file path or mutable asset resolver is consulted at
dispatch. The request always carries complete `input`, `store=false`,
`truncation=disabled`, `stream=true`, and the selected tools. It never uses a
provider conversation or `previous_response_id`.

The SHA-256 fingerprint includes profile/account/endpoint identity, the exact
encoded request and effective setting provenance. Final guidance and asset/tool
changes therefore change the fingerprint. Changing any prepared input requires
preparing again and repeating the host's final admission. The fingerprint is an
admission/correlation aid, not a new persisted-history identity or a signature.

## Temporary authentication and dispatch

`Auth.bearer` validates a bounded, header-safe nonempty secret into an opaque
lease without serialization or a public secret accessor. The host resolver gets
exactly the captured profile/account identity at dispatch under the attempt's
switch. Credentials are absent from `Profile`, `Prepared` and fingerprints.
The resolver may silently renew but cannot initiate an interactive login.
No module initialization reads environment variables or captures credentials.
The embedding host initializes `Mirage_crypto_rng` once before HTTPS use,
as the repository executables already do; the driver does not change the host's
random generator or defaults.

```ocaml
let prepared =
  Responses_driver.Prepared.create profile
    ~model:"operator-selected-model"
    ~history:effective_history ~tools:selected_tools ~settings
  |> Or_error.ok_exn
in
(* Host performs its final admission against Prepared.fingerprint here. *)
Responses_driver.run driver ~auth:host_resolver ~prepared
  ~on_event:host_fold
```

`run` is synchronous. Its nested Eio switch owns the connection and temporary
resolver resources; nothing escapes. The caller fiber's cancellation context
owns the call. The deadline includes authentication, DNS, TLS and I/O. An auth
error (including deadline expiry during resolution) returns `Error` before any
events. Unexpected resolver failures and cancellation propagate.

Limits bound request bytes, aggregate response headers, aggregate entity bytes,
individual SSE frames and cumulative transfer-framing bytes.
`max_framing_bytes` defaults to 1 MiB and charges chunk sizes/extensions,
chunk separators and trailers separately from entity bytes. HTTP lines require
CRLF; field names/values, trailer fields and token/quoted chunk extensions are
validated. Transfer-framing exhaustion reports `Framing_limit`. HTTP status other
than
200, unsupported content encoding/type, interim responses, duplicate/ambiguous
lengths, invalid chunks and premature entity EOF produce typed failures. This
selected one-request HTTP/1.1 client does not negotiate interim responses,
redirects, compression or connection reuse. It avoids the legacy `Io.Net` null
TLS authenticator and Cohttp's plaintext protocol debug logging/unbounded reader.

Validated nonterminal `Event.Update` values arrive incrementally; exact duplicate
codec events are suppressed. Terminal-only newly finalized items arrive in
`Event.Finalized` before `Event.Terminal`. The terminal retains the complete
lossless response, usage, refusal/incomplete reason and error evidence. For every
normal `Ok outcome`, exactly one `Event.Terminal outcome` is delivered. A terminal
means inference ended; it does not finish an actor operation, pending tool, goal
or CLI run. The driver stops at the first validated semantic terminal and closes
the socket; trailing provider bytes cannot trigger tool execution.

Before an HTTP write begins, a transport failure is `Definitely_not_submitted`.
Once any write is attempted it is `Possibly_submitted`; once a validated update
has been published it is `Response_started`. Failed attempts retain already
published callback evidence. No state automatically retries, falls back to a
transport, restarts an inference or replays an effect. A qualified retry decision
belongs to the host. Callback exceptions, including exceptions with transport-like
types, preserve their original exception/backtrace. If attempt cleanup also
fails, Eio retains both original exceptions in its aggregate error. Unexpected
authentication-resolver exceptions also propagate, including `Eio.Time.Timeout`;
only expiry of the driver-owned deadline becomes a typed timeout outcome. Cancellation
propagates
through cleanup. Neither path manufactures a normal terminal.

Failures carry bounded typed diagnostics without request, credential, response,
endpoint or exception text. Provider/raw captures delivered to the host are
private conversation evidence; consumers must not automatically log them.

## Offline completion evidence

`test/responses_driver` uses independently authored loopback HTTP/SSE fixtures
and fake clocks. It exercises exact full-history/tools transmission, first event
before server completion, declared arbitrary models, settings precedence and
fingerprints, concurrent profiles/credentials, null replay metadata, immutable
assets, completed/refused/incomplete/failed outcomes, terminal-only finalizations,
HTTP/framing/body limits, cumulative chunk metadata limits, strict CRLF and
header/trailer/extension syntax, malformed streams, uncertain delivery without
retry, consumer exceptions with concurrent cleanup failures, cancellation and
authentication deadlines. A real local TLS
server with an intentionally untrusted certificate verifies the secure adapter
rejects it before receiving any decrypted application bytes. No live model calls,
external inference endpoint or real credentials are used.

Run the focused evidence with:

```sh
opam exec --switch=default -- dune runtest test/responses_driver test/responses_codec --root . --build-dir _build
```

The selected HTTP/SSE behavior is consistent with the official
[streaming guide](https://developers.openai.com/api/docs/guides/streaming-responses).
The locally pinned OCH-50 codec defines the selected field/event profile;
[this codec reference](openai-responses-codecs.md) documents that boundary.
