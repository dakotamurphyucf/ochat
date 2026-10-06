# Selected Responses request fixtures

`Responses_request` is a pure, validated wire request boundary. The host still owns
canonical identity/call pairing, asset admission, origin compatibility, model/profile
capability eligibility, tool authority and scheduling. These tests do not call a
provider or prove public API/OAuth eligibility.

The current request constructor emits full ordered input, `store:false`,
`truncation:"disabled"`, the selected stream Boolean, and explicit encrypted
reasoning inclusion by default. A host-prepared profile without reasoning may
explicitly disable that inclusion. Independently supplied requests may omit
truncation/stream and use their documented disabled/false defaults; store=false
remains mandatory. Remote response references, Conversations, hosted tools,
background mode and provider compaction are excluded.

Fixtures use literal emitted JSON and independently authored inputs. They cover
absent/null/value separately, reasoning including xhigh/max and correctly spelled
concise, output-format omission versus explicit text/JSON variants, verbosity,
function/custom schema/format/choice/options, independent cache retention and TTL,
raw ordered replay including namespace/phase/opaque data and exact call strings,
duplicate keys, known optional call/caller types, invalid numbers/ranges and
validation budgets. Function-result media nullability is tested separately from
custom-result/message media; it is not a blanket content option policy.

Top-level function parameters/strict are required nullable fields. Custom
description/format/async, text/text.format, tools/choice and cache options are
nonnull. Explicit null remains a field value wherever the selected schema permits
it; generated OCaml options are not used as omission/null codecs. Async has data
encoding support only; its execution mapping is a separate host feature.
JSON Schema/grammar content is preserved and shape-checked, not proven satisfiable
or normalized. Namespace declarations, deferred discovery, hosted/programmatic
tools, reasoning context/mode, cache prewarm/remote diagnostics and unselected
request options fail closed until an explicit supported profile adds them.

Schema sources checked 2026-10-06: [Responses create](https://developers.openai.com/api/reference/typescript/resources/responses/methods/create),
[Responses types](https://developers.openai.com/api/reference/typescript/resources/responses),
[function calling](https://developers.openai.com/api/docs/guides/function-calling)
and [prompt caching](https://developers.openai.com/api/docs/guides/prompt-caching).
Public optionality is distinct from the stricter local profile invariants above;
for example, locally generated results require a nonempty call ID even though
the public function-output union also contains more permissive alternatives.

Offline check from the repository/worktree root:

```sh
opam exec --switch=default -- dune runtest --root . --build-dir _build-request test/responses_request
```

The switch is consumed read-only. Use a separate build directory when other workers
share the worktree. Formatter/PPX and runtime release qualification belong to the
actual supported toolchain; a local passing codec suite is not whole-M1 completion.
