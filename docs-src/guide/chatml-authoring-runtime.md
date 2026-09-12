# ChatML authoring: execution and invocation contracts

This reference covers the three ways a ChatML program participates in an Ochat
session. These versioned extension paths are implemented on internally qualified
hosts; public authoring/helper rollout remains in progress. A compiler surface
does not enable a host feature. Use the entrypoint, selected capabilities and
limits supplied by the host that will execute your candidate.

The examples are the actual X01, X04 and X02 integration fixtures. The documentation
gate checks source parity and compiles each against its exact entrypoint contract.
The linked integration tests exercise their behavior with fake provider streams
and real local runtime/storage/tool implementations; they do not call a paid model.
See [language differences](chatml-ocaml-differences.md) for grammar and JSON syntax.

## Choose the execution contract

| Program | Required definitions | Result and state |
|---|---|---|
| One-off computation | `main(input)` | Task of JSON; fresh program environment per invocation |
| Standalone tool | `run(context, input)` | Task of a tool outcome; fresh program environment per invocation |
| Conversation moderator | `initial_state` value and `on_event(context, state, event)` | Task of replacement moderator state; invocation resolution is a separate operation |

Function arity is exact: one, two and three arguments respectively. Static
validation checks the final bindings, including shadowing, without evaluating
initializers. A function with a familiar name and the wrong result type is not
a valid entrypoint. A standalone tool does not need a synthetic moderator or a
new agent conversation. A moderator can orchestrate without asking its owning
model for a response, but its returned value is still state, not a tool answer.

## Bind scripts and schemas in ChatMD

A standalone script declaration uses `language="chatml"`, `kind="tool"`, an
explicit `id` and inline or `src` source. Its tool declaration uses `type="chatml"`,
the script ID, `entrypoint="run"`, and `input_schema`/`output_schema` paths.
Each `uses` child selects one exact registered tool; omission selects none.
Unknown/duplicate names and cyclic managed-tool dependencies fail preparation.
The selection is bound to actual implementations, not just names that can be
replaced after validation.

A moderator script uses `kind="moderator"` and `api="extensibility-v1"`. Its tool
uses `type="moderator"` and the moderator ID. At most one conversation moderator
is allowed. Moderator tools use the owner's configured capabilities; standalone
`uses` declarations do not apply to them. A legacy moderator without this API
version does not receive the new invocation contract.

Schema/source paths are relative local dependencies inside the source boundary;
captured artifacts retain their bytes and provenance. They are not URLs to fetch
at execution time. Live file changes do not silently change a captured program.
Input, output and optional `completion_schema` have different roles: immediate
completion/acknowledgement follows the output contract, while owned background
completion follows its declared completion contract and delivery rules.

Ochat's `ochat.tool-schema.v1` validator accepts boolean schemas and a bounded
JSON-schema subset. `type` accepts a scalar type or nonempty type union. Supported
constraints are `properties`, `required`, `additionalProperties`, `items`, `enum`,
`const`, `anyOf`, `minItems`, `maxItems`, `minLength`, `maxLength`, `minimum` and
`maximum`; `title`, `description` and `$comment` are string metadata. Unknown
keywords, including `$ref`, are rejected instead of ignored or remotely resolved.
Object properties and array items are unconstrained when the corresponding
constraint is omitted; declare closed shapes explicitly when required.

The validator uses exact decimal comparisons and Unicode scalar string lengths.
Its source/value limits are 1 MiB, 128 levels and 100,000 nodes, with a separate
validation work budget. ChatML's `Number` payload still uses floating-point values;
schema comparison rules do not make ChatML arithmetic arbitrary-precision.
See the [validator contract](../../lib/chatmd_shell_spec/tool_schema.mli) and the
complete example declarations linked below. These schema checks neither authorize
file/shell effects nor replace current invocation admission.

## One-off tool-using computations

The native `run_chatml` request supplies `source`, JSON `input` and an explicit
`tools` list. An empty list selects no tools. The request can lower host-approved
time/resource limits; it cannot request unrestricted execution or expand the
caller's tool/file/shell authority. Trusted embeddings of the language have
separate configurable execution policies.

The following script reads approved report files and groups failed checks. Supply
an input such as `["report-a.json", "report-b.json"]`, and select `read_file` in
`tools`. Its [agent declaration](../../test/chatml_extensibility_fixtures/x01-report/agent.chatmd)
configures the `reports` read root; the
[fixture bundle](../../test/chatml_extensibility_fixtures/x01-report/README.md)
contains the input reports and expected aggregate.

<!-- ochat-authoring-example: {"id":"runtime.one-off.report","surface":"one_off_v1","fixture":"test/chatml_extensibility_fixtures/x01-report/aggregate.chatml"} -->
```ocaml
(* read_file returns two metadata lines before the file body. *)
let after_line text = match String.find(text, "\n") with
  | `Some(index) -> String.slice(text, index + 1, String.length(text) - index - 1)
  | `None -> fail("expected a complete read_file response")

let field text key = match Json.get_field(text, key) with
  | `Some(`String(value)) -> value
  | _ -> fail("expected string field " ++ key)

let add_failure groups check =
  if Array.exists(groups, fun group -> group.check == check) then
    Array.map(groups, fun group ->
      if group.check == check then { check = check; failures = group.failures +. 1.0 }
      else group)
  else Array.append(groups, [{ check = check; failures = 1.0 }])

let add_report groups report = match report with
  | `Array(checks) -> Array.fold(checks, groups, fun acc item ->
      let status = field(item, "status") in
      if status == "failed" then add_failure(acc, field(item, "check"))
      else if status == "passed" then acc
      else fail("unsupported report status"))
  | _ -> fail("expected an array of checks")

let rec read_reports files index groups =
  if index == Array.length(files) then Task.pure(groups)
  else match files[index] with
    | `String(file) ->
      let* result = Tool.call("read_file", `Object([
        { key = "root"; value = `String("reports") },
        { key = "file"; value = `String(file) }
      ])) in
      (match result with
        | `Ok(`String(text)) ->
          let report = Json.parse(after_line(after_line(text))) in
          read_reports(files, index + 1, add_report(groups, report))
        | `Ok(_) -> Task.fail("read_file returned a non-text result")
        | `Error(code) -> Task.fail(code))
    | _ -> Task.fail("expected report filenames")

let main input = match input with
  | `Array(files) ->
      let+ groups = read_reports(files, 0, []) in
      `Array(Array.map(groups, fun group -> `Object([
        { key = "check"; value = `String(group.check) },
        { key = "failures"; value = `Number(group.failures) }
      ])))
  | _ -> Task.fail("expected an array of report filenames")
```

`Tool.call(name, json)` returns a task yielding `Ok(json)` or `Error(message)`.
A native tool's successful transport result can contain opaque text or its own
application-level error format; do not treat `Ok` as proof that an application
operation succeeded. This example explicitly decodes the documented `read_file`
text envelope before parsing its JSON body. Other tools have their own contracts.

Returning JSON from `main` completes the script computation. The native tool
wraps that result in its versioned invocation outcome envelope. Returning an
ordinary ChatML record, or a standalone `Complete` outcome instead of JSON, does
not satisfy `main`'s contract. A failed task and host cancellation are also
distinct from a successful JSON result.

The [X01 tests](../../test/chatml_composition/one_off_tests.ml) verify real report
aggregation, nested selected-tool calls, no new conversation and denial outside
the configured read root.

## Standalone tools and explicit outcomes

A standalone declaration connects a named ChatML script to input/output schemas
and exact `uses` dependencies. The
[comparison declaration](../../test/chatml_extensibility_fixtures/x04-standalone/agent.chatmd)
binds `compare_reports` to `compare.chatml`, its `run` entrypoint and the
[input](../../test/chatml_extensibility_fixtures/x04-standalone/input.json) and
[output](../../test/chatml_extensibility_fixtures/x04-standalone/output.json) schemas.
`uses` selects existing registered implementations; it does not reconfigure them.

<!-- ochat-authoring-example: {"id":"runtime.standalone.compare","surface":"tool_v1","fixture":"test/chatml_extensibility_fixtures/x04-standalone/compare.chatml"} -->
```ocaml
(* Each invocation must start with a fresh mutable global. *)
let calls = [0.0]

let field input name = match Json.get_field(input, name) with
  | `Some(`String(value)) -> value
  | _ -> fail("expected a filename")

let read file = Tool.call("read_file", `Object([
  { key = "root"; value = `String("reports") },
  { key = "file"; value = `String(file) }
]))

let run ctx input =
  let left = field(input, "left") in
  let right = field(input, "right") in
  if left == right then Task.pure(`Fail({
    code = "reports.same_file"; message = "Choose two different reports.";
    retryable = false; details = `Null
  }))
  else
    let ignored = calls[0] <- calls[0] +. 1.0 in
    let* first = read(left) in
    let* second = read(right) in
      match first with
      | `Error(code) -> Task.fail(code)
      | `Ok(left_text) -> (match second with
          | `Error(code) -> Task.fail(code)
          | `Ok(right_text) -> Task.pure(`Complete(`Object([
              { key = "invocation_count"; value = `Number(calls[0]) },
              { key = "left"; value = left_text },
              { key = "right"; value = right_text }
            ]))))
```

The host validates input before calling `run`. Its context contains invocation,
session, tool, capability and limit information; the separate input is the
validated JSON request. Context fields are snapshots, not editable authority.
Changing a copied context or a capability descriptor cannot select another tool
or alter permissions. No filesystem handle or callable OCaml value is exposed.

Tool outcomes use ChatML variants:

- `Complete(json)` supplies an immediate result checked against the output schema.
- `Fail({code; message; retryable; details})` supplies a structured tool failure.
- `Pending(work_ref, acknowledgement)` supplies an immediate acknowledgement
  associated with already-admitted owned work and a valid completion path.

Use backticks when constructing these variants in source, as the example does.
`Pending` is not an arbitrary promise: inventing a `Job(id)` or
`Subscription(id)` does not create work. The runtime rechecks ownership, generation
and completion/schema requirements. Host cancellation cannot be constructed as
a successful outcome or erased by catching a task failure.

The mutable `calls` global demonstrates invocation isolation. Two concurrent
invocations each report count 1; a standalone script's globals are not persistent
tool state. Use a moderator-owned state or another explicitly owned durable
resource when state must survive calls. The
[X04 tests](../../test/chatml_composition/standalone_tests.ml) also check schema
rejection before file effects, declared failures and output-schema rejection.

## Moderator tools and session-owned state

A moderator-handled tool has no independent OCaml implementation to run after
its handler. Its declaration selects the versioned moderator, input/output
schemas and optionally a completion schema. The
[review declaration](../../test/chatml_extensibility_fixtures/x02-review/agent.chatmd)
binds `begin_review` and two failure-probe tools to the same moderator.

A valid invocation arrives as `Tool_invoked(payload)`. The payload contains the
host-prepared invocation context and input. Resolve its exact invocation ID once
with `Invocation.resolve(id, outcome)`, then return the new moderator state.
Returning state alone does not answer the tool call. `Pre_tool_call` remains
the separate interception/policy hook; it does not replace `Tool_invoked`.

<!-- ochat-authoring-example: {"id":"runtime.moderator.reviews","surface":"moderator_v1","also_check":["delegated_moderator_v1"],"fixture":"test/chatml_extensibility_fixtures/x02-review/review.chatml"} -->
```ocaml
type review = { revision : string; reference : string }

let initial_state : review array = []

let on_event = fun ctx state event -> match event with
| `Tool_invoked(p) ->
  (match p.context.tool_name with
  | "begin_review" ->
    let revision = match Json.get_field(p.input, "revision") with
      | `Some(`String(value)) -> value
      | _ -> fail("revision is required")
    in
    (match Array.find(state, fun review -> review.revision == revision) with
    | `Some(review) ->
      let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String(review.reference))) in
      Task.pure(state)
    | `None ->
      let review = { revision = revision; reference = "review-" ++ to_string(Array.length(state) + 1) } in
      let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String(review.reference))) in
      Task.pure(Array.append(state, [review])))
  | "double_resolve" ->
    let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("first"))) in
    let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("second"))) in
    Task.pure(state)
  | _ -> Task.pure(state))
| _ -> Task.pure(state)
```

The normal `begin_review` path deduplicates by revision in moderator state.
The `double_resolve` branch is intentionally invalid behavior: its two resolutions
are rejected. An unhandled tool invocation also fails; silence is not a successful
result. Other ordinary events return state unchanged in this example.

The moderator manager serializes ownership of the session state. Validated
resolution, replacement state and buffered local effects are committed through
the owning runtime/actor transaction. A failure does not commit half a local
resolution/state update. Already-performed external effects are not automatically
undone or retried. Keep durable workflow state in serializable moderator state
or explicitly owned records; mutable globals are outside its rollback contract.

The [X02 restart test](../../test/agent_server_restart_test.ml) verifies concurrent
review deduplication, retained references after restart and rejection of missing
or duplicate resolutions. These properties come from the persisted invocation
and moderator machinery, not from the example's string naming convention.

## Authority and target surfaces

One-off and standalone surfaces provide pure computation, tasks, diagnostic
logging, selected `Tool.call` access and qualified job operations. They do not
provide ambient `Model`/`Process`, conversation mutation or timer/UI operations.
`Tool.spawn` aliases `Job.start_tool`; job starts stage intent until the owning
commit. Availability in a signature table does not install the corresponding
host operation or grant permission to execute it.

An ordinary versioned moderator has its own broader host surface. A generated
child's moderator uses `delegated_moderator_v1`, which excludes direct `Model`
and `Process` access. Its selected tool calls remain under inherited authority.
Do not form a new target by combining all compiler inventories. A child may
choose supported model/reasoning settings while remaining within its delegated
tool, shell and file rules.

## Non-executing validation

Non-executing validation checks parsing, types, exact entrypoints, declared
schemas and static capability/source selection. It does not execute initializers,
invoke tools, create work/sessions, approve requests or prove dynamically computed
tool names/JSON values valid. Actual execution rechecks current permissions,
schemas, deadlines, ownership and operation phases. A successful report is
evidence about a candidate under a specific host/capability identity, not an
execution grant.

See [invocation internals](../agent-server/extensibility-foundations.md),
[signature inventories](chatml-surface-inventory.md) and
[topic assembly](authoring-topic-corpus.md) for the supporting contracts.
Background acknowledgement/delivery/recovery and full child-session lifecycle
guidance require their own topics; these synchronous examples are not substitutes
for those contracts.
