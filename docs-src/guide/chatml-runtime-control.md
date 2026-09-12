# Model, process and runtime control

These APIs construct tasks; use `let*` to interpret them in sequence. Runtime is
available on ordinary and delegated extensibility-v1 moderators. Model and Process
are available only on ordinary moderators. None of these three modules is installed
on the one-off or standalone-tool surface. A declared compiler name does not itself
install a host service, authorize a command or register a model recipe.

Use [selected tools](chatml-host-effects.md) and
[owned jobs](chatml-authoring-background.md) for reusable work within delegated
authority. Use [persisted children](chatml-authoring-children.md) when later messages,
status, outputs and cancellation should address the same child session.

## Host-managed model recipes

| Call | Task result |
|---|---|
| `Model.call(recipe, payload)` | `Ok(json)`, `Refused(string)` or `Error(string)`. |
| `Model.call_json(recipe, payload)` | Alias of call with the same result. |
| `Model.call_text(recipe, text)` | Wraps the input as a JSON string; returns the same result variant. |
| `Model.spawn(recipe, payload)` | Host job ID string, without waiting for model completion. |
| `Model.spawn_text(recipe, text)` | Wraps the input as a JSON string and uses spawn. |

Recipe names are exact host registrations, not arbitrary model names or provider
IDs. Payloads and successful JSON results follow that recipe's contract.
`call_text` does not extract text or remove the result wrapper. Pattern-match
the result, then inspect the successful JSON. A returned refusal/error variant
is a value, so it does not trigger Task.catch. A missing recipe or failed host
operation can fail the task; catch those failures separately if recovery is useful.

The ordinary Ochat conversation host registers `agent_prompt_v1`. Its payload is
an object with required string fields `prompt` and `input`, plus optional boolean
`is_local` (default false), boolean `history_compaction` (default false), and string
`session_id` (default the prompt name). Success contains `recipe`, `prompt`,
`is_local`, `session_id`, `final_text` and `terminated_normally`. A JSON string
alone does not satisfy this recipe: the text helpers are useful only for a
registration whose input contract accepts a string.

This recipe invokes an agent prompt through the host's existing runner. Its
`session_id` field is not the persisted generated-child lifecycle API. Do not use
this recipe to bypass delegated tool/file/shell restrictions; delegated moderators
exclude Model and Process and the host removes their registrations.

Synchronous calls may execute external work before the moderator commits;
a later failure cannot undo a provider request. Model.spawn also uses an external
callback: do not assume Task.catch or a rejected moderator commit cancels work
already admitted by that callback. It is not the transactional Job.start_tool
operation. On the durable session host, the callback records a session-owned model
job through the job service. On the legacy embedding host, spawn uses its older
callback/lifecycle and completion events.
Do not infer retention, retry, cancellation or notification guarantees from the
string return type alone. Follow the installed host's
[job contract](../agent-server/chatml-orchestration.md).
A job ID acknowledges work; it is not the final answer or proof that a new
model turn has been requested.

## Shell-backed process execution

`Process.run(command, arguments)` takes a command string and an array of string
arguments. The ordinary shell adapter builds structured argv, waits for the shell
executor, then returns stdout concatenated with stderr. It does not insert a
separator or preserve interleaving between the two streams. Its return string is
not a background-job ID, despite the low-level operation's asynchronous category.

The binding requires a ChatMD moderator_runtime declaration selecting a declared
shell runtime. See [shell runtime integration](chatml-moderator-runtime.md#shell-runtime-integration)
for the declaration and [shell configuration](chatmd-shell-extensions.md) for the
authorized runtime. The adapter follows the same resolver, effects, approval,
backend, limits and auditing path as shell tools. Without a binding it fails;
it never falls back to unrestricted process spawning.

Arguments remain separate argv entries. Text such as a space or semicolon in an
argument is not implicitly parsed as a shell program. Explicitly invoking a shell
with its command-string option changes that interpretation and remains subject
to the selected runtime's policy. Executor errors fail the task. This string API
does not expose structured exit status or separate output streams; use an
appropriately declared shell tool when those fields matter. Use a background job
for later status/results rather than treating Process.run output as a job handle.
A completed command's effects are not undone by moderator rollback.

## Transactional session requests

| Call | Meaning |
|---|---|
| `Runtime.emit(json)` | Queue a JSON payload for a later `Internal_event(payload)`. |
| `Runtime.request_compaction()` | Ask the host to compact at an appropriate boundary. |
| `Runtime.request_turn()` | Ask the host for an automatic follow-up model turn. |
| `Runtime.end_session(reason)` | Record an end-session request and halt the moderator when committed. |

All four return unit tasks. Extensibility-v1 emit accepts JSON, unlike the legacy
unversioned operation that accepts an arbitrary ChatML event value. Do not wrap
the payload in Internal_event yourself. Emission queues a later moderator event;
it does not directly append a conversation message or wake the model. Use the
[notification and delivery APIs](chatml-authoring-background.md) for retained
messages, explicit wake-up and acknowledgement.

request_turn is allowed in turn_end, internal_event, tool_observed and tool_invoked.
It fails in phases such as session_start, turn_start and pre_tool_call. Emit,
request_compaction and end_session have no additional phase restriction in the
standard low-level registry. Host limits and policy still apply: an accepted
request is not a promise of unlimited turns, immediate compaction or delivery
after the session ends. Repeated requests are coalesced by the host policy.

These are transactional intents. Catch discards requests and queued events staged
inside the failed catch scope. Whole-handler failure or rejected persistence
prevents installation of prospective state, events and halt status. Previously
committed events remain queued. On success, emitted events are appended in order
and processed later by the host; they do not recursively invoke on_event within
the current handler.

end_session does not terminate the current task continuation. Code after it can
run and can still fail the transaction. Its committed halt prevents subsequent
normal moderator processing; it is not an operating-system process exit.
`Turn.halt` is a separate overlay operation, described in
[turn effects](chatml-host-effects.md). A host owns cancellation/cleanup of its
associated work; do not treat an end-session request as evidence that every
external action has already stopped.

## Checked session-control workflow

This source runs on both moderator surfaces. The Turn_start branch deliberately
demonstrates a disallowed request_turn phase; a production moderator should
request another turn at one of the allowed boundaries instead.

The offline checker uses actual JSON-event adapters, task interpretation and host
outcome decoding. It checks queue order, catch rollback, whole-handler failure,
rejected commit, phase rejection and continuation after end_session. It does not
run a provider or claim a daemon restart.

<!-- ochat-authoring-example: {"id":"runtime-control.workflow","surface":"moderator_v1","fixture":"test/chatml_extensibility_fixtures/authoring-control/runtime.chatml","also_check":["delegated_moderator_v1"]} -->
```ocaml
let initial_state = 0
let on_event ctx state event =
  match event with
  | `Session_start ->
    let* () = Runtime.emit(`String("ready")) in
    Task.pure(state + 1)
  | `Internal_event(command) ->
    (match Json.as_string(command) with
     | `Some("recover") ->
       let* () = Runtime.request_compaction() in
       let* () = Task.catch(
         (let* () = Runtime.emit(`String("discarded")) in
          let* () = Runtime.request_turn() in
          let* () = Runtime.end_session("discarded") in
          Task.fail("recover")),
         fun message -> Runtime.emit(`String(message))) in
       Task.pure(state + 1)
     | `Some("continue") ->
       let* () = Runtime.request_turn() in
       let* () = Runtime.emit(`String("first")) in
       let* () = Runtime.emit(`String("second")) in
       Task.pure(state + 1)
     | `Some("fail") ->
       let* () = Runtime.end_session("must not commit") in
       let* () = Runtime.emit(`String("must not publish")) in
       Task.fail("whole handler failed")
     | `Some("finish") ->
       let* () = Runtime.end_session("work complete") in
       let* () = Log.info("continuation after end_session") in
       Task.pure(state + 1)
     | _ -> Task.pure(state))
  | `Turn_start ->
    let* () = Runtime.request_turn() in
    Task.pure(state + 1)
  | _ -> Task.pure(state)
```

## Checked recipe and process workflow

These fixture registrations are illustrative: echo, refuse, fail, missing and
fixture-command are not built-in Ochat recipes or commands. An offline fake host
checks exact input wrapping, JSON/result variants, task failure recovery, distinct
spawn IDs and preservation of argv entries. It performs no model or shell call.
The source compiles only on the ordinary moderator surface, as required by its
Model and Process operations.

<!-- ochat-authoring-example: {"id":"runtime-control.model-process","surface":"moderator_v1","fixture":"test/chatml_extensibility_fixtures/authoring-control/model-process.chatml"} -->
```ocaml
let describe result =
  match result with
  | `Ok(value) -> value
  | `Refused(message) -> `String(String.concat("refused: ", message))
  | `Error(message) -> `String(String.concat("error: ", message))
let initial_state = `Null
let on_event ctx state event =
  match event with
  | `Session_start ->
    let* text = Model.call_text("echo", "hello") in
    let* data = Model.call_json("echo", `Object([{key = "count"; value = `Number(2.0)}])) in
    let* refused = Model.call("refuse", `Null) in
    let* failed = Model.call("fail", `Null) in
    let* missing = Task.catch(
      Model.call("missing", `Null),
      fun message -> Task.pure(`Error(message))) in
    let* first_job = Model.spawn("echo", `Null) in
    let* second_job = Model.spawn_text("echo", "later") in
    let* output = Process.run("fixture-command", ["a b", ";literal"]) in
    Task.pure(`Object([
      {key = "text"; value = describe(text)},
      {key = "data"; value = describe(data)},
      {key = "refused"; value = describe(refused)},
      {key = "failed"; value = describe(failed)},
      {key = "missing"; value = describe(missing)},
      {key = "jobs"; value = `Array([`String(first_job), `String(second_job)])},
      {key = "output"; value = `String(output)}]))
  | _ -> Task.pure(state)
```
