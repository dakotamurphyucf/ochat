# Authoring evaluation harness (A01.09, in progress)

This developer-only harness compares authoring under three experimental guidance
conditions. It is not installed with Ochat or embedded in its authoring corpus.
Task prompts contain no held-out answers. CI uses scripted providers only.

```sh
opam exec -- dune build @test/authoring_evaluation/runtest
opam exec -- dune exec test/authoring_evaluation/manifest.exe
```

The manifest command exports eight tasks, fixed step/attempt limits, selected
reference roots and acceptance thresholds. Its SHA-256 binds all of those bytes;
use it as the result's `suite_revision`. Thresholds are prospective targets, not
claims that model quality has been measured. No provider is invoked by either
command above.

## Runner contract

`Runner.run` accepts a trusted execution backend, a provider callback and an
injected monotonic clock. The provider receives only messages and a step number.
It returns a documentation query, a candidate validation request, or a decline.
It never supplies validation or execution scores. A backend validates every
submission and executes only statically valid candidates through its task oracle.
`Not_run` explicitly prevents an unimplemented oracle from claiming success.
Validation or runtime failures produce feedback for the next attempt.

The three conditions share task prompts, tool descriptions and a minimal primer:

- `Minimal` adds no initial topic text; the provider can request references.
- `Automatic_retrieval` assembles the installed task preparation before step one.
- `Selected_preload` retrieves the task manifest's selected topic closures first.

These are evaluation conditions, not aliases for ChatMD's `auto`/`manual`/`preload`
policy implementations. In particular, a normal automatic primer does not eagerly
retrieve a full task package. The reference backend's selected-topic arm currently
retains separate topic response envelopes, including any repeated prerequisites;
this cost is measured rather than treated as deduplicated runtime preloading.

`Reference_backend` uses Ochat's actual scoped, bounded reference query and static
validator. Initial references follow pagination to completion; provider retrieval
returns one page and exposes its continuation normally. `initial_reference_roots`
counts preparation/selected roots, not individual page requests. `retrieval_calls`
counts provider-requested queries, including failures and continuation requests.
Source/host/capability identity and admission remain controlled by those services.

A configured context-loss stimulus discards prior conversation/reference text,
retains the task, primer and tools, and inserts a rediscovery instruction. This
checks provider behavior after context loss. Actual durable compaction and its
receipts are qualified separately by the session/composition tests.

## Metrics and comparison

Each result records evidence provenance, model identity, suite/runtime revisions,
limits, every validation/execution result, first-pass compilation, eventual runtime
success, repair count, provider steps, reference queries, context-loss events and
elapsed time. Validation failures distinguish parser, semantic and capability
errors; execution oracles also have an infrastructure category.

Token estimates use `ceil(UTF-8 bytes / 3)` per message. Primer, tool-description,
delivered documentation, repeated effective documentation and total effective
input costs are separate. All messages still present are counted on every model
request. Provider-reported input tokens are stored separately and are `None` if
any step lacks that measurement. Estimates exclude provider-specific chat framing
and are neither exact token counts nor billing estimates.

`Runner.compare` requires exactly one result per manifest task per policy, with
the same provenance, model identity, suite/runtime revisions and limits. It rejects
missing/duplicate rows or mixed runs. Supply the exact model/configuration identity
in `model`, and identify the actual runtime contract in `runtime_revision`.
Offline scores must retain `Offline_transcript`; changing that label does not
turn a scripted provider into evidence of model quality.

The integration fixture exercises all three conditions with actual installed
references, readonly validation and bounded execution of a pure identity program.
The OCaml-style call fails type checking, a type-correct wrong answer fails the
execution oracle, and the repaired ChatML candidate succeeds. Rejected source
never executes; a missing selected tool is a capability failure. All eight manifest tasks' preload selections
resolve against the installed corpus. Other tests exercise repair accounting,
context loss, unavailable execution, retrieval-only step exhaustion and invalid
comparisons. These fixtures do not solve or score the held-out task suite.

## Required work remaining

`Execution_cases` now implements the one-off ledger and standalone delta task
oracles. They create an actual embedded session, serialize fake-provider tool
calls, and inspect outcomes returned through the public session protocol. The
candidate source runs through ordinary admission, invocation dispatch, and file/
schema enforcement. No new tool implementation is accepted from candidate ChatMD:
the harness fixes the root definition and allowed file roots itself. Every host
uses a temporary workspace and closes before its directory is removed.

The ledger cases cover duplicate accounts, cancellation of zero balances, order
independence, empty input, missing/malformed files and root escape. The standalone
cases cover sorted set differences, duplicates, empty inputs, Unicode/prefix
ordering and input rejection. Mutations that return a plausible wrong value,
weaken the input schema, contradict the output schema or omit the read binding
fail the oracle. The standalone task is also run through all three guidance arms
with the actual validator and execution oracle. These scripted scores are offline
pipeline qualification, not real-model performance.

Solutions live only in `fixtures/`, outside installed documentation. The runtime
currently has integer ordering but no `String.compare`; the scripts compose a
UTF-8 byte comparator from existing string/JSON operations. Standalone array
results use explicit `json` annotations to retain the full recursive result type.
The scripts do not add host primitives, shell access, or native dependencies.

`Moderator_cases` evaluates the quota task through eight sequential calls within
one embedded session. It checks successful decrement, rejection without decrement,
zero/negative requests, fractional/wrong-type schema failures, exact exhaustion,
and subsequent rejection. Mutations that reset successful state, alter rejected
state, leave a call unresolved or resolve it twice fail. The default synchronous
host still sends one batch; sequential mode explicitly expects one provider step
per call and one final response, and still rejects unexpected jobs/extra work.

The quota candidate has exactly `source`, `binding`, `input_schema` and
`output_schema` fields. This is an evaluation submission envelope, not a new
`ochat_validate` input shape. Its binding is parsed using a captured source bundle
without preprocessing or filesystem fallback. Only synchronous `reserve` owned by
moderator `quota`, with no tool dependencies or additional declarations, is admitted.
The harness then constructs the unchanged native moderator validation request.
The positive task runs through actual validation/execution in all three guidance
conditions. Attempting to add a native reader is rejected before runtime creation.
The clarified task prompt specifies the call/result/error and binding contract;
the suite manifest fingerprint changes accordingly.

Add execution oracles/transcripts for background and generated-child families,
plus the extra repair/capability/compaction tasks.
Integrate ledger scoring with the invoking host's actual read capability metadata.
Add the reproducible optional
provider driver, fixed model settings/seeds/repetitions and deadlines, transcript
and metric artifacts, execution-oracle revision identity, and threshold evaluation.
Adapt host startup/timeout failures into reported infrastructure outcomes while
preserving cancellation. Bind that driver to actual runtime
guidance/compaction when comparing production policies. Report safety violations
independently of compile/runtime rates. An explicitly authorized real-model run is
optional; none has occurred here. A01.09/T15 remain incomplete until the required
offline driver, task coverage and reporting are qualified.
