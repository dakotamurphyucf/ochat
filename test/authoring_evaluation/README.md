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

`Background_cases` checks immediate acknowledgement of one running probe job,
then releases the probe or cancels it through the public job protocol. It requires
one retained notification whose work ID and full completion match the retained
job, exactly one success wake, no cancellation wake, and process exit before the
embedded host closes. A failed moderator or extra job/notification/model request
fails the scenario. The fixed shell probe uses an isolated `direct_unsafe` test
policy; this is not a production sandbox recommendation.

The background envelope has the same four fields as the quota envelope, with
`begin_work` owned by `observer`. Captured parsing admits no additional native
declarations. The candidate moderator is statically checked with only the actual
host probe selected. A fixture replays the completion payload with updated handler
state and produces no second publication; a conflicting publication, missing
acknowledgement and added reader are rejected. This tests application callback
replay, not daemon journal replay or restart recovery. Two publication attempts in
one callback transaction are rejected by the runtime, including repeated keys;
the fixture removes completed work from its state before handling a replay.

Ledger and background scoring now run through all three guidance conditions with
metadata from actual native registrations. `Execution_host.with_capabilities`
constructs the fixed evaluator-owned declarations using `Agent_runtime`, retains
their descriptors, schemas and metadata, and closes resources on scope exit.
It runs no candidate/tool/model code. Execution hosts independently admit those
same fixed declarations under their own temporary roots; their capability IDs
are distinct and validation does not transfer authority between hosts. The
background integration requires both release and cancellation to pass.

`Child_cases` submits a captured definition through the real `agent_create`
registration in a temporary durable embedded host. The evaluation envelope has
`create`, `send` and `read` fields: a native creation request and native request
templates with literal `$session_id`, `$message`, `$key` and `$cursor` substitutions.
Static validation requires an owned running child selecting only `read_file`,
explicit model/reasoning settings and imported companion developer instructions.
Unused companion bytes do not satisfy that last requirement. This task's companion
check replaces non-root captured files with whitespace and requires successful
parsing with changed effective developer instructions; it is an evaluator contract,
not a new restriction on Ochat's general generated-bundle format.

The fake child provider calls the actual inherited reader twice, checks evidence
content and retained prior assistant output on follow-up, and emits distinguishable
answers. The submitted send/read templates must deliver both rounds to the created
session and use the previous cursor without repeating older output. The host waits
on actual submission receipts, stops the child through `agent_stop`, and checks
that it reaches stopped state before teardown. Missing/unused companion files,
new native file roots, foreign IDs and omitted cursors fail. All three guidance
conditions run actual static validation and execution. This proves local persisted
lifecycle integration, not crash/restart recovery or real-model review quality.

`Execution_host.with_session` shares startup/cleanup between these adaptive
lifecycle calls and the existing synchronous/background scenarios. Durable storage
is explicitly selected only for the child scenario; defaults remain transient.

The remaining three task transcripts now have execution oracles:

- OCaml transfer first fails actual type checking, then submits a type-correct
  wrong count that fails execution, then counts empty/mixed/nested arrays correctly
  and rejects non-array input. Both valid candidates run through `run_chatml` in
  an embedded session; the rejected source never executes.
- The tally task first submits a moderator that leaves the tool invocation
  unresolved. After the configured context-loss stimulus, the provider has no
  documentation messages, retrieves the installed event contract again, and
  repairs the program. Six sequential embedded-session calls check retained state,
  negative/zero amounts, exact cancellation to zero and rejection of a fraction.
- The missing-Process task uses a private generic moderator host with an actual
  pure SHA-256 native registration. Its delegated surface excludes Process/Model.
  It uses the real extension compiler, moderator manager, invocation preparation,
  resolution validation and bounded execution. The oracle requires one selected
  digest call per input, unchanged arguments and unchanged output for empty,
  ASCII and Unicode text. Bypassed calls and modified output fail. This is not a
  new Ochat builtin, shell tool, embedded-session or persistence test.

All three guidance conditions run each transcript. Tests check the precise attempt
sequence (static rejection versus wrong runtime result), repair/retrieval counts
and context-loss count. The tally stimulus tests reference reacquisition in the
evaluation runner, not durable daemon compaction. The missing Process symbol is
reported by the compiler as a semantic/type failure; it is not relabelled as a
native authorization denial. No real model-quality evidence is implied.

Generalize replay scoring beyond the private
background fixture so arbitrary submitted handlers receive the same stimulus.
Add the reproducible optional
provider driver, fixed model settings/seeds/repetitions and deadlines, transcript
and metric artifacts, execution-oracle revision identity, and threshold evaluation.
Adapt host startup/timeout failures into reported infrastructure outcomes while
preserving cancellation. Bind that driver to actual runtime
guidance/compaction when comparing production policies. Report safety violations
independently of compile/runtime rates. An explicitly authorized real-model run is
optional; none has occurred here. A01.09/T15 remain incomplete until the required
offline driver, task coverage and reporting are qualified.
