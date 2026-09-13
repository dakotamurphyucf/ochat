# Invocation context and moderator events

A standalone tool receives `run(ctx, input)`, where ctx is tool_context and input
is validated JSON. A moderator receives `on_event(ctx, state, event)`, where ctx
is the conversation context. During Tool_invoked, the tool context is
`payload.context`, and its input is `payload.input`. These contexts have different
fields and different purposes. Neither exposes an unrestricted host object.

Use [execution contracts](chatml-authoring-runtime.md) for declarations, outcomes
and state ownership, and [moderator data](chatml-moderator-data.md) for projected
conversation history. The following aliases are available to standalone tools
and both moderator surfaces: tool_context, tool_limits, tool_capability, tool_error,
tool_outcome. tool_invocation, moderator_event and Invocation.resolve are
moderator-only. The one-off main function receives JSON without these aliases.

## Tool context fields

| Field | Type and meaning |
|---|---|
| version | Integer ABI version; currently 1 for this context. |
| invocation_id | Host-issued runtime invocation ID string. |
| provider_call_id | Optional provider call ID string; a separate correlation value. |
| session_id | Owning session ID string. |
| generation | Integer session generation used in admission. |
| origin | Model, Moderator, Script, Delegated_agent or External_adapter variant. |
| parent_invocation | Optional runtime parent invocation ID string. |
| parent_event | Optional host moderator-execution ID string. |
| parent_job | Optional background job ID string. |
| tool_name | Name of the invoked registered tool. |
| implementation_revision | Captured implementation identity string. |
| capability_fingerprint | Captured invocation capability fingerprint string. |
| created_at_ms | Integer timestamp in milliseconds since the Unix epoch. |
| deadline_ms | Optional integer deadline timestamp in the same units. |
| limits | tool_limits record described below. |
| available_tools | Array of tool_capability records for this implementation's selected dependencies. |

All fields are present in the typed record; optional fields are None/Some values.
Use backticks on variant constructors in source. Treat IDs and fingerprints as
opaque. A provider call ID can be reused and is not an alternative to invocation_id.
Parent links describe causality; they do not authorize a foreign invocation or job.

For a managed standalone call, the invocation identity records the caller's
admitted selection, while available_tools contains the implementation's declared
dependencies. Consequently capability_fingerprint need not be a hash of that
displayed array. The host privately binds both sides of admission. Copying,
editing or manufacturing these data values does not change the captured registry,
grant new tools or supply a valid host execution scope.

There is no user-cancellable flag or process handle to mutate in this record.
Cancellation is enforced by the active host execution scope and propagates through
evaluation/effects. Do not use absence of a deadline or an old context snapshot
as evidence that an operation may continue after cancellation or session shutdown.
Canonical call/output history IDs remain host-side persistence fields.

## Capability and limit records

A tool_capability contains exactly id, name, implementation_revision, fingerprint
(all strings), and input_schema (JSON). input_schema describes accepted input,
not a file/shell permission grant. A standalone tool's uses declarations select
existing registered implementations; they do not configure those implementations.
Use Tool.call with the selected name and valid JSON, subject to current admission.

tool_limits contains eight integers: fuel, max_tasks, max_value_bytes,
max_output_bytes, max_array_items, max_depth, max_nested_calls and
max_invocation_depth. These are the invocation ABI's limits, not live remaining
balances. The standard bridge reports the declared script limits and its nested
call/depth ceilings; enclosing runtime, ancestor and host policies can impose
tighter controls. Do not use a displayed field to override those controls.
The record is distinct from the JSON limit overrides accepted by run_chatml or
Job.start_script; see [background values](chatml-background-values.md).

## Outcomes and explicit resolution

A tool_outcome is Complete(json), Fail(tool_error), or
Pending(work_ref, acknowledgement_json). A tool_error has exactly code and message
strings, retryable boolean and details JSON. A structured Fail is a successful
return of a failure outcome; it differs from Task.fail aborting the handler.
The host checks Complete and Pending acknowledgement payloads against output_schema.
Pending also needs already admitted owned work and a valid completion path;
inventing a Job or Subscription string is insufficient.

A standalone tool returns its outcome from run. A moderator calls
Invocation.resolve(invocation_id, outcome), yielding a unit task, and returns
replacement state separately. Resolve the exact active Tool_invoked invocation
once. Resolution is buffered until the handler and host commit succeed; code after
resolve can still run and can fail the transaction.

Zero surviving resolutions fail as unhandled. More than one, a different ID,
malformed output, invalid schema/work ownership or rejected persistence prevents
the proposed state and response from committing. A resolution discarded by a
caught failure does not count as a surviving response. Resolution outside an
active dispatched invocation is invalid. It is neither a general message-send API
nor an asynchronous callback handle; use retained jobs/subscriptions/notifications
for later results.

Pre_tool_call is the separate approval/rewrite hook, not a replacement for
Tool_invoked. Tool_observed reports a completed invocation and cannot resolve it
again. The [checked moderator example](chatml-authoring-runtime.md#moderator-tools-and-session-owned-state)
demonstrates normal resolution, duplicate resolution rejection and missing handlers.

## Moderator event values

The moderator_event alias describes these constructors. Only the host delivers
an event; constructing its shape does not dispatch tools or bypass admission.

| Constructor | Payload and use |
|---|---|
| Session_start | No payload; initial session lifecycle hook. |
| Session_resume | No payload; host resume hook. |
| Turn_start | No payload; beginning of a model turn. |
| Item_appended(item) | Projected item appended; its context phase is message_appended. |
| Pre_tool_call(tool_call) | Pending proposal; approve/reject/rewrite/redirect through Tool. |
| Post_tool_response(tool_result) | Tool response boundary in the ordinary conversation flow. |
| Turn_end | No payload; end-of-turn hook, where request_turn is permitted. |
| Internal_event(json) | Host-admitted data, including versioned emit/timer payloads. |
| Tool_invoked(tool_invocation) | Dispatched moderator-handled tool; explicit resolution required. |
| Tool_observed(record) | Invocation observation with the shape below. |
| Job_completed(work_completion) | Compiler-shaped completion value; do not assume the host emits this constructor. |
| Subscription_expired(work_completion) | Compiler-shaped expiry value; inspect the installed host delivery contract. |

tool_invocation is a record with version (integer, currently 1), context
(tool_context) and input (JSON). Tool_observed has version (integer, currently 2),
invocation_id (string), optional parent_invocation and parent_event, tool_name
(string), origin (the same origin variants), and outcome (JSON). Its outcome is
the protocol outcome encoding, not a directly pattern-matchable tool_outcome.
A nested observation can carry parent linkage distinct from a provider call.

The [background reference](chatml-authoring-background.md) documents actual
Internal_event completion and external-data envelopes; the
[work-value reference](chatml-background-values.md) distinguishes compiler-shaped
work_completion from delivery promises. Runtime.emit accepts JSON on v1: writing
a constructor name in user JSON does not fabricate a privileged native event.
Use a fallback branch for events your workflow does not handle, preserving state.
A fallback in Tool_invoked that never resolves still produces an unhandled call.

## Checked context-inspection tool

This complete standalone source reports its host-provided context as JSON and
returns a structured failure for the input string decline. It performs no tool
calls. Bind it with kind tool, entrypoint run and JSON input/output schemas;
declare uses dependencies only for tools whose metadata it should see.

The integration test binds read_file as inspect's only dependency and invokes
inspect both directly and through a separate wrapper tool. It checks actual
runtime/parent/provider identity, admitted revision/fingerprint, dependency
selection and structured failure while making no file reads. The documentation
gate checks the exact source against the standalone entrypoint contract.

<!-- ochat-authoring-example: {"id":"invocation.context-inspection","surface":"tool_v1","fixture":"test/chatml_extensibility_fixtures/authoring-invocation/inspect.chatml"} -->
```ocaml
let number value = Json.parse(to_string(value))
let optional value = match value with | `None -> `Null | `Some(text) -> `String(text)
let capability : tool_capability -> json = fun value ->
  `Object([
    {key = "id"; value = `String(value.id)},
    {key = "name"; value = `String(value.name)},
    {key = "implementation_revision"; value = `String(value.implementation_revision)},
    {key = "fingerprint"; value = `String(value.fingerprint)},
    {key = "input_schema"; value = value.input_schema}
  ])
let limits : tool_limits -> json = fun value ->
  `Object([
    {key = "fuel"; value = number(value.fuel)},
    {key = "max_tasks"; value = number(value.max_tasks)},
    {key = "max_value_bytes"; value = number(value.max_value_bytes)},
    {key = "max_output_bytes"; value = number(value.max_output_bytes)},
    {key = "max_array_items"; value = number(value.max_array_items)},
    {key = "max_depth"; value = number(value.max_depth)},
    {key = "max_nested_calls"; value = number(value.max_nested_calls)},
    {key = "max_invocation_depth"; value = number(value.max_invocation_depth)}
  ])
let refusal : tool_error =
  {code = "inspection.declined"; message = "Inspection declined"; retryable = false; details = `Null}
let run : tool_context -> json -> tool_outcome task = fun ctx input ->
  match input with
  | `String("decline") -> Task.pure(`Fail(refusal))
  | _ -> Task.pure(`Complete(`Object([
    {key = "version"; value = number(ctx.version)},
    {key = "invocation_id"; value = `String(ctx.invocation_id)},
    {key = "provider_call_id"; value = optional(ctx.provider_call_id)},
    {key = "session_id"; value = `String(ctx.session_id)},
    {key = "generation"; value = number(ctx.generation)},
    {key = "origin"; value = `String(variant_tag(ctx.origin))},
    {key = "parent_invocation"; value = optional(ctx.parent_invocation)},
    {key = "parent_event"; value = optional(ctx.parent_event)},
    {key = "parent_job"; value = optional(ctx.parent_job)},
    {key = "tool_name"; value = `String(ctx.tool_name)},
    {key = "implementation_revision"; value = `String(ctx.implementation_revision)},
    {key = "capability_fingerprint"; value = `String(ctx.capability_fingerprint)},
    {key = "created_at_ms"; value = number(ctx.created_at_ms)},
    {key = "deadline_ms"; value = (match ctx.deadline_ms with | `None -> `Null | `Some(ms) -> number(ms))},
    {key = "limits"; value = limits(ctx.limits)},
    {key = "available_tools"; value = `Array(Array.map(ctx.available_tools, capability))}
  ])))
```

The [standalone integration tests](../../test/chatml_composition/standalone_tests.ml)
exercise this source through the actual daemon with fake provider streams.
The [moderator invocation tests](../../test/moderation/moderator_invocation_test.ml)
cover no/double/wrong-ID resolution, invalid outcomes and rejected saves. These
checks do not call a paid model. Compiler coverage pins describe all bindings on
the four extensibility surfaces; they do not establish complete language,
ChatMD, native-tool or recovery qualification on their own.
