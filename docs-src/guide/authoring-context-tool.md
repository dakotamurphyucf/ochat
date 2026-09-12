# Query authoring documentation from an agent

`ochat_authoring_context` retrieves installed ChatML, ChatMD and runtime reference
text without network access or model calls. On an internally qualified extension
host with an authoring validation target, explicitly declare
`<tool name="ochat_authoring_context"/>` to expose the native tool. General public
enablement, automatic helper installation and primer/preload insertion are still
pending. The current corpus is a reviewed foundation, not complete feature coverage.

An explicitly authorized shell helper can query the same service using
`operation: "reference"` in the [private helper envelope](../bin/ochat_agent_helper.doc.md#authoring-requests),
with the strict request below as `arguments`. This lets a ChatML moderator implement
its own authoring tool through an admitted shell integration. The helper can also
request non-executing validation with `operation: "validate"`. Each operation needs
its own host grant and a configured authoring target; neither requires a native
authoring tool in the agent's tool list.

Start with `prepare`. Its first item is a flat feature map explaining what each
feature enables, when to use it and the guides to read before implementing it:

| Feature | Why an author would use it |
|---|---|
| ChatML language and tasks | Write deterministic transformations and compose tool effects with `let*` |
| One-off scripts | Combine tools and logic without starting another agent conversation |
| ChatMD and standalone tools | Package a computation behind a reusable schema and handler |
| Stateful moderators | Respond to session events and implement custom tool behavior with retained state |
| Background jobs | Acknowledge long-running work and deliver the result later |
| Subscriptions and timers | Poll for results, enforce deadlines and reject stale completion events |
| Notifications and external events | Deliver data, request a model turn or integrate an external producer |
| Persisted child agents | Delegate ongoing work with separate instructions and tracked session outputs |
| Authority and validation | Check generated code and understand inherited tool, shell and file constraints |
| Recovery | Design cancellation, retries and restart behavior around actual commit guarantees |

The map encourages the author to consider capabilities useful to its task, then
retrieve their contracts. It links directly to substantial sections; prerequisites
are included automatically. It does not require navigating a deep documentation
tree. Whole sections and complete examples remain intact when paged.
Every task package includes the broad `chatml.programs` guide and its checked
OCaml-differences prerequisites, covering source syntax, control flow, matching,
types, structured-data utilities and effect boundaries before the task's runtime
contract. Read all pages when the expanded package needs continuation.

The `agent_create` tool description also explains why to create a persisted
specialist, reviewer or ongoing worker, describes the follow-up management tools,
and points to `prepare` with `task: "child_agent"` when the reference helper is
available. This gives the model a reason to discover the authoring guides before
it attempts an unfamiliar ChatMD definition or moderator.

## One strict request schema

Every operation uses the same eight required fields. Unused fields must be `null`;
extra properties are rejected. `version` is always `1`.

```json tool=ochat_authoring_context
{
  "version": 1,
  "operation": "prepare",
  "task": "moderator_tool",
  "query": null,
  "topic_id": null,
  "features": ["background_tools", "timers", "notifications"],
  "cursor": null,
  "max_tokens": null
}
```

| Operation | Required non-null fields | Optional non-null fields |
|---|---|---|
| `prepare` | `task` | `features`, `max_tokens` |
| `topic` | `task`, `topic_id` | `max_tokens` |
| `search` | `task`, `query` | `max_tokens` |
| `continue` | `cursor` | `max_tokens` |

Task IDs are `one_off_script`, `standalone_tool`, `moderator_tool`, `child_agent`
and `background_workflow`. Feature IDs are `background_tools`, `subscriptions`,
`timers`, `notifications`, `external_events` and `child_sessions`. A task cannot
enable a target absent from the invoking host. Requesting a topic incompatible
with the selected target fails explicitly.

For example, read timer semantics and their prerequisites:

```json tool=ochat_authoring_context
{
  "version": 1,
  "operation": "topic",
  "task": "background_workflow",
  "query": null,
  "topic_id": "runtime.jobs.timers",
  "features": null,
  "cursor": null,
  "max_tokens": null
}
```

`search` matches words, symbols and topic IDs against surface-compatible installed
topics. Results contain stable IDs, titles, excerpts, hashes and prerequisites.
Fetch a result with `topic` to read the actual reference; an excerpt is not a
replacement for its contract. No semantic model or vector service is involved.
Search also matches compiler symbol names/signatures and selected tool names and
descriptions. These matches point to the corresponding flat reference topic;
results identify matching symbols and a bounded excerpt. Prose matches appear
first to retain their semantic guidance, followed by matching reference inventories.

Two direct reference topics complement the prose guides:

- `reference.signatures` returns the selected compiler surface's globals, module
  exports, aliases and required entrypoints. Its first item explains the readable
  signature notation. Fetch all continuation pages so alias definitions are included.
- `reference.tools` returns the invoking scope's exact selected tool descriptions,
  input schemas, strictness and output contract. It does not discover or grant tools
  outside that selection. `native_output` means native output remains opaque;
  `invocation_v1` identifies the runtime's structured invocation outcome contract.

Both are also included after the prose in `prepare`, using the same budget and
continuation mechanism. Schemas and signature declarations are never truncated.
Signatures are grouped into whole sections for entrypoints, aliases, globals and
each module, keeping navigation flat and avoiding a separate page per function.

## Interpret availability and completeness

The orientation reports the invoking scope's selected tool names/descriptions and
enabled authoring targets. Each guide identifies whether its topics are readable
on the current surface and whether its suggested authoring task is enabled.
These are distinct from runtime effect availability: compiler/reference support
and successful validation do not prove that a host service is installed, or grant
permission to use a tool. Execution still checks current bindings and authority.

Responses identify the runtime, surface, corpus and capability fingerprint.
`coverage` currently reports `reviewed_foundation_not_full_feature_coverage`, and
`prepare` reports `package_complete: false`. Selected native schemas and compiler
signatures are included, but broader language/ChatMD/runtime semantic coverage and
context lifecycle integration remain unfinished. `topic_sequence` identifies the assembled reference topics, including
ones on later pages; `items` contains only this page. `complete` describes pagination
of this query, not completion of the full authoring reference.

## Budgets and continuation

The default target is 12,000 estimated tokens, with a configurable 32,000-token host
ceiling. `max_tokens: null` selects the default. The current estimate is
`ceil(UTF-8 bytes of the full JSON response / 3)` and is labelled
`utf8_bytes_div_3_estimate`. It is not an exact tokenizer, a token upper bound or a
provider billing measurement. The host rejects requests above its ceiling.

When `complete` is false, pass `next_cursor` to `continue`, setting `task`, `query`,
`topic_id` and `features` to null. If the next intact section cannot fit, the page
may be empty and `budget.minimum_next_tokens` reports the estimated space needed.
Increase the budget within the host ceiling; no source text is silently truncated.

Cursors are signed and bound to the original query, corpus, host target, selected
capabilities and invoking session/generation. Changing that context or the host's
signing key invalidates them; repeat the original query. A continuation can change
its response budget, but cannot switch to another session's authority or corpus.

See the [corpus reference](authoring-topic-corpus.md),
[query interface](../../lib/chat_response/authoring_context.mli) and
[native registration](../../lib/agent_session/authoring_context_tool.mli).
