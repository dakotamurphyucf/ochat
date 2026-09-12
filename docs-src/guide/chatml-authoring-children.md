# ChatML authoring: persisted child sessions

A persisted child has its own captured ChatMD definition, conversation, moderator
state and session ID. Its parent can submit work and inspect results over multiple
calls. Use a one-off ChatML computation when you only need deterministic logic
over existing tools; create a child when you need another retained agent session.

The operations below are implemented on internally qualified durable hosts.
Ordinary agents do not receive them automatically. General exposure still depends
on authoring guidance and host qualification. Reading this reference does not
install a tool, enable a compiler operation or authorize a model request.

## Capture and validate a generated definition

`agent_create` accepts a version-1 object. Required fields are `version`,
`root_file`, `sources`, `tools` and `idempotency_key`. Each source has exactly
`path` and `text`. Supply the root and all its imported sources as captured bytes;
these names are bundle paths, not permission to read arbitrary server files.
Missing imports, duplicate source names and paths outside the bundle reject.
The host bounds file count, individual source size and aggregate size.

`tools` is an explicit, duplicate-free selection from the invoking agent's
delegable capabilities. Inside the generated ChatMD, select exact names using
`<tool type="inherited" name="read_file"/>`. Declarations narrow the request's
selection again. An inherited reference does not alias a binding or change its
implementation, file roots, shell rules, environment or connected resources.
An empty selection is valid when the child needs no tools.

Generated definitions can provide plaintext instructions and a single generation
configuration. Model and reasoning settings belong in `<config .../>`, not in
the creation request. Session identity is host-owned. Static checks reject invalid
configuration and unsupported reasoning values; they do not prove that a chosen
model is remotely available or authorized to run.

A generated definition may include one `extensibility-v1` lifecycle moderator,
with `initial_state` and `on_event ctx state event`. Its compiler contract is
`delegated_moderator_v1`: it has no direct `Model`, `Process` or stdout `print`.
The child's ordinary agent loop can still call its configured model. Moderator
effects must use operations admitted by the host and the selected inherited
tools. Generated declarations cannot create new native, shell, MCP, agent,
standalone or moderator-handled tool implementations, replace inherited authoring
metadata, load implicit message resources, or inject tool results and reasoning
history. Imports do not bypass these restrictions.

This complete creation request captures an imported file-tool reference and a
pass-through lifecycle moderator. The documentation gate checks its exact fixture
bytes, creation-request decoding and non-executing generated-definition admission.
It does not create a child or call a provider. `read_file` must already be an
actual delegable binding; substitute instructions and a fresh creation key for
your application.

```json
{
  "version": 1,
  "root_file": "child.chatmd",
  "sources": [
    {
      "path": "child.chatmd",
      "text": "<developer>Review the report requested by your parent. Read only through your inherited file tool.</developer>\n<import src=\"tools.chatmd\"/>\n<script id=\"lifecycle\" language=\"chatml\" kind=\"moderator\" api=\"extensibility-v1\">\nlet initial_state = 0\nlet on_event ctx state event = Task.pure(state)\n</script>"
    },
    {
      "path": "tools.chatmd",
      "text": "<tool type=\"inherited\" name=\"read_file\"/>"
    }
  ],
  "tools": ["read_file"],
  "start_immediately": true,
  "lifetime": "owned",
  "idempotency_key": "report-review-child-1"
}
```

For `ochat_validate`, retain `version`, `root_file`, `sources` and `tools`, and
add `target: "generated_chatmd"`. Omit creation-only fields, including the retry
key, start flag, lifetime and display name. The validator checks captured imports,
declarations, selected capabilities, configuration, authoring policy and moderator
compilation without evaluating initializers or constructing a runtime. A valid
report defers creation, model use, current authority, approvals and runtime effects.
Creation must admit the same bytes and current bindings again.

## Create, retry and retain authority

Set `start_immediately: true` when the parent needs to send work to the child.
The default is **false**: creation retains a stopped session. None of the six
management operations starts a stopped child, and `agent_send` does not resume it.
Starting a retained stopped child requires a separately authorized host operation.
`display_name` is optional display metadata, not a selector or identity.

The default `lifetime` is `owned`. Parent stop propagates cancellation and cleanup
through owned children. `independent` requires explicit host authorization and
retains the authority/resource relationship even when its parent stops; it does
not remove tool restrictions or ongoing revocation checks. Do not assume a host
supports independent lifetime for every inherited runtime configuration.

Keep the creation `idempotency_key` with the exact intended request. Retry that
request and key after an uncertain result. A changed payload conflicts; a new key
can create another child. Successful creation returns `session_id`,
`definition_revision`, a session summary, effective tool names and `management`
parent/child IDs. These identify a retained relationship and are not bearer
credentials. A retry does not replay the child's conversation or restart a child
that has since stopped. Captured sources remain pinned across live file edits.

Management uses the actual calling session's direct recorded relationship and
current policy. Knowing an ID, sharing an operator, or being an ancestor does not
grant access. A nested script uses its narrowed capability set, not the entire
parent registry. Tool/resource restrictions are rechecked after waits and before
effects or disclosure. Children can narrow authority but cannot widen it through
their instructions or model configuration.

Management permission does not grant permission to answer an approval request.
The child owns its approvals. An unattended child may remain waiting or encounter
a denial under the host's policy; status inspection cannot approve its work.

## Submit work and track completion

`agent_send` requires `session_id`, plaintext `message` and `idempotency_key`.
Keep a new key for each intended message and reuse it only for identical retries.
The response is a durable submission receipt, including `receipt_id`, status,
terminal flag and operation correlation when known. It is not the child's answer
or a promise that a turn completed. Busy sessions can defer submissions, and
multiple receipts can be processed by the same operation.

Retained retries do not resubmit work, including after daemon restart or stop.
New messages to stopped children reject. Reset and input removal can invalidate
unresolved correlation; reusing an old key must not be treated as a fresh send.
Host limits bound new message/receipt admission while preserving retained retries.

`agent_status` takes only `session_id`. It reports bounded lifecycle/operation
metadata and a pending-permission count. It omits transcript, tool arguments,
permission details and failure text. An idle session or an intermediate assistant
message does not establish that a particular submission finished.

Use `agent_wait` with `session_id` and `receipt_id` to await that submission's
terminal outcome. Inspect the returned receipt status: terminal can mean success,
failure, cancellation or interruption. `timeout_ms` defaults to 10000 and accepts
0–30000; zero checks immediately. A timeout or cancellation of the waiting caller
does not stop or resume the child, consume its results or resolve an approval.
Longer workflows can schedule further bounded checks through background work.

## Read output and recover cursors

`agent_read` requires `session_id`; optional fields are `receipt_id`, `cursor`
and `limit` (default 16, range 1–128 records). Reads return committed assistant
output after the initial prompt, with provenance and operation/receipt correlation.
They omit system/developer messages, reasoning and tool traffic. Readers do not
consume each other's output. Redacted records retain a marker with a null payload.

Keep `next_cursor` even when `caught_up` is true. It identifies a position in that
query's output, not completion of the child or its operation. Continue with the
same session and receipt filter. To wait for new output, call `agent_wait` with
that cursor and the same receipt filter, then call `agent_read` with the original
cursor. An output wait does not consume output. A terminal receipt alone does
not satisfy an output wait when no unread assistant output remains.

Pages are byte-bounded as well as record-bounded. An ordinary item has
`kind: "output"` and a complete `value`. Large records use
`kind: "output_fragment"` with `entry_id`, `byte_offset`, `total_bytes`, `text`
and `complete`. Concatenate fragments of the same entry in byte-offset order,
then decode the resulting JSON when complete. Do not interpret each fragment as
an independently complete assistant response.

Cursors bind the relationship, generation, query and output history. Host restart,
expiry, history replacement/retention gaps and changed bindings can require a
fresh snapshot. `agent.read.cursor_expired` and `agent.read.snapshot_required`
are recovery signals, not empty pages; wait uses corresponding `agent.wait`
errors. Discard an unusable cursor and start a bounded read without it. Reconcile
stable entry IDs before processing output again. If a receipt's output has been
removed, an unfiltered fresh read can inspect retained history but cannot recover
missing output. Durable receipt identity does not make all transcript data permanent.

## Stop and use the shared helper path

`agent_stop` requires `session_id`, `idempotency_key` and `mode` (`graceful` or
`cancel`). Graceful stop lets admitted work finish; cancel requests cancellation.
Both preserve stored history. The stop receipt proves durable admission, not a
join of all descendant/resource cleanup. Inspect `progress` and current status.
`stopping` and `stopped` describe the applicable stop; `superseded` means that the
child entered a later lifetime or stop epoch. Replaying an old key cannot stop a
new lifetime. Reuse the same key/mode for uncertain retries; use a new key for a
new stop or escalation from graceful to cancel.

All six native registrations declare the invocation outcome contract. A direct
model-facing result is an envelope with `type: "complete"` and `value`, or
`type: "fail"` and structured error fields. A ChatML `Tool.call` through the
qualified invocation dispatcher already decodes that declared contract: success
is `Ok(value)` and failure is `Error(code)`. Do not decode the successful value a
second time. A successful send still means receipt admission, not child completion.
Ordinary tool text remains opaque when its binding has no declared outcome
contract; do not recognize envelopes by guessing from the text.

The shared helper adapter accepts `version: 1`, `operation` and `arguments`.
Operations are `create`, `send`, `read`, `status`, `wait` and `stop`, with the same
argument decoders and outcomes as the native tools. Its envelope grants no access.
A trusted host must explicitly grant the named shell tool a scoped helper channel;
the current invocation supplies caller identity, expiry and allowed operations.
The child tool selection is separate from that operation grant. Ordinary shell
access or an ambient operator CLI token is not this constrained helper interface.

The [helper request script](../../test/chatml_extensibility_fixtures/x07-helper-session/request.chatml)
calls a shell binding whose success value is stdout text, then explicitly decodes
that helper's documented JSON envelope into a standalone outcome. This is a
different boundary from calling a native lifecycle binding. Its
[moderator](../../test/chatml_extensibility_fixtures/x07-helper-session/moderator.chatml)
retains asynchronous invocation/job correlation. The
[response watcher](../../test/chatml_extensibility_fixtures/x06-response-watcher/README.md)
composes receipt/cursor checks with jobs, timers and notifications. These use the
same persisted sessions; there is no second session runtime for scripts.

See the [creation decoder](../../lib/agent_session/generated_session_request.mli),
[shared adapter](../../lib/agent_session/session_management.mli),
[management service](../../lib/agent_session/managed_session_service.mli) and
[generated admission](../../lib/chat_response/generated_admission.mli) for host
contracts. The lifecycle evidence includes
[generated execution](../../test/agent_server_generated_test.ml),
[child-owned shell approvals](../../test/agent_server_generated_shell_test.ml) and
[helper integration](../../test/agent_server_helper_test.ml). This reference and
its static example check do not close the remaining full lifecycle qualification.
