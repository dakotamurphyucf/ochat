# Native computation and validation requests

Use `run_chatml` for a deterministic computation over JSON and an explicit subset
of your tools. Use `ochat_validate` to check a proposed script or captured agent
definition before executing or creating it. Validation produces diagnostics; it
does not run the candidate. Read `reference.tools` for the exact selected tool
schemas and `reference.signatures` for the actual compiler surface.

## Execute one computation

`run_chatml` requires `source`, `input` and `tools`. `source` is ChatML text with
`main : json -> json task`. `input` is any JSON value, including null. `tools` is
a duplicate-free array of exact selected names; `[]` gives the computation no
tool access. Omit optional fields instead of supplying null. Unknown fields
reject. This request has no `version`, session ID, model or reasoning parameter.

The following complete request totals a JSON array without calling any tools.
It returns the invocation outcome `{"type":"complete","value":10}`.

```json tool=run_chatml
{
  "source": "let main input = match input with | `Array(values) -> Task.pure(`Number(Array.fold(values, 0.0, fun total value -> match value with | `Number(n) -> total +. n | _ -> fail(\"expected numbers\")))) | _ -> Task.fail(\"expected an array\")",
  "input": [2, 3, 5],
  "tools": [],
  "limits": {"max_calls": 0}
}
```

Optional `timeout_ms` is a positive integer deadline in milliseconds for the
whole request, including preparation. Compilation time reduces the time left for
execution; a surrounding invocation's earlier deadline also applies. Optional
`limits` accepts the following integer fields:

| Field | Meaning |
|---|---|
| `fuel` | Evaluation step budget |
| `max_tasks` | Task operation budget; zero is allowed |
| `max_calls` | Nested tool-call budget; zero is allowed |
| `max_invocation_depth` | Nested invocation depth |
| `allocation_bytes` | Interpreter allocation budget |
| `max_value_bytes` | Encoded runtime value limit |
| `max_output_bytes` | Returned output limit |
| `max_array_items` | Runtime collection size limit |
| `max_depth` | Encoded value nesting limit |
| `max_source_bytes` | UTF-8 source size limit |
| `compile_timeout_ms` | Compilation deadline in milliseconds |

All other listed limits must be positive. Overrides can only lower the calling
host's limits; omission retains the host setting. An excessive request fails
with `chatml.limit_escalation`, rather than silently raising or clipping a limit.
There is no request-level unlimited sentinel. These are limits on this tool's
execution contract, not a restriction on every trusted embedding of ChatML.
Tool selection and limits never expand file roots, shell rules or approval rights.

The result follows `invocation_v1`: success has `type: "complete"` and JSON
`value`; failure has `type: "fail"`, `code`, `message`, `retryable` and `details`;
cancellation has `type: "cancelled"` and `reason`. Preparation failures can put
structured diagnostics in `details.diagnostics`. Inspect the outcome before
using its value. This computation does not create an agent session or implicitly
ask a model for an answer. Calling a tool from the script can still have external
effects. A failed computation does not undo those effects, and an uncertain retry
must not assume that earlier calls did nothing.

Read `runtime.invocations.one-off` for a complete file-tool workflow and
`runtime.authority.tool-selection` for capability boundaries. A one-off script can
use a selected job-starting capability, but its return value remains JSON; do not
substitute a moderator/standalone `Pending` result for its `main` contract.

## Validate a candidate

Every `ochat_validate` request has `version: 1`, a `target` and an explicit
duplicate-free `tools` selection. The tool exposes one object schema; the service
also checks target-specific required and forbidden fields. Passing the outer
schema alone does not establish a valid request. The target cannot enable a
surface or tool unavailable to the caller.

| Target | Required candidate fields | Additional rules |
|---|---|---|
| `one_off_script` | `source` | `main : json -> json task`; schemas are forbidden |
| `standalone_tool` | `source`, `input_schema`, `output_schema` | `run ctx input` returns an invocation-outcome task; both schemas use Ochat's schema dialect |
| `moderator` | `source` | `initial_state` and `on_event ctx state event`; schemas are forbidden |
| `generated_chatmd` | `root_file`, `sources` | Each source has exactly `path` and `text`; inline `source` and schemas are forbidden |

These four complete requests are checked without evaluating their initializers:

```json tool=ochat_validate
{
  "version": 1,
  "target": "one_off_script",
  "source": "let main input = Task.pure(input)",
  "tools": []
}
```

```json tool=ochat_validate
{
  "version": 1,
  "target": "standalone_tool",
  "source": "let run ctx input = Task.pure(`Complete(input))",
  "tools": [],
  "input_schema": true,
  "output_schema": true
}
```

```json tool=ochat_validate
{
  "version": 1,
  "target": "moderator",
  "source": "let initial_state = fail(\"validation must not initialize me\")\nlet on_event ctx state event = Task.pure(state)",
  "tools": []
}
```

```json tool=ochat_validate
{
  "version": 1,
  "target": "generated_chatmd",
  "root_file": "agent.chatmd",
  "sources": [
    {"path": "agent.chatmd", "text": "<developer>Review the evidence supplied by your parent.</developer>"}
  ],
  "tools": []
}
```

The moderator example is intentionally unsafe to execute: its initializer fails.
It demonstrates the validation boundary. Replace that initializer with valid
state before running it. `moderator` selects the caller's ordinary or delegated
moderator surface; it does not allow a request to choose the broader one.

For generated definitions, include every imported file as captured bytes. Do not
include creation-only `idempotency_key`, `start_immediately`, `lifetime` or
`display_name`. Read `chatmd.definitions` and `runtime.delegation.generated` for
allowed declarations, inherited capabilities and source restrictions. Inline
standalone validation checks the candidate and schemas; it does not validate a
complete surrounding ChatMD tool binding or a completion schema.

The native validator's output is a JSON report, using the `native_output`
convention. It contains `version`, `scope` (`inline_script` or `generated_bundle`),
`valid`, `target`, `source`, `validation_id`, `compiler_contract`,
`capability_fingerprint`, `runtime_identity`, `diagnostics`, `checked` and `deferred`.
Identity fields can be null when validation fails before they are established.
Each diagnostics entry contains a nested `diagnostic` and `topic_ids`: retrieve
those topics, repair the candidate and validate the same intended bytes again.
Do not confuse successful tool delivery with `valid: true`.

`checked` lists completed checks; `deferred` lists execution-time obligations.
The validator does not evaluate initializers, invoke tools or models, create jobs
or sessions, or approve requests. It cannot prove computed tool names, runtime
input/output, state serialization, remote model availability or future authority.
Neither a successful report nor its identity is a reusable execution permit.
Execution/creation must re-admit the candidate under current authority.

The invocation transport can wrap native text in its own completion envelope.
Likewise, a helper-backed call follows its enclosing tool's declared result
convention. Read the selected `reference.tools` entry and decode that wrapper
before inspecting the validator's JSON report; do not assume all tool outputs use
the same envelope.
