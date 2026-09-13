# Run checks and deliver background results

An agent can start useful work, answer other requests, and receive its results
later. In this Lantern example, a ChatML moderator starts the documentation
checker as an owned job, acknowledges the request, and publishes the completed
result as a new runtime notification.

The checker runs real project code under a configured shell runtime. Its failing
verification check gives the agent concrete evidence to explain. The example
also exposes progress and cancellation through moderator-handled tools.

## Launch the complete checker

Start with [stateful tools](stateful-workflow.md) and
[shell guardrails](shell-guardrails.md). Configure provider access to `gpt-6-astra`.
Download **Lantern background checks** below, extract it, and inspect every file
in the source reader, especially `runtimes/checks.chatmd`.

The bundle contains the root ChatMD, coordinator, runtime, two schemas and the
complete Lantern sample project. From its extracted `background-results/` directory:

```sh
chat-tui --no-config --local --authorize-shell-manifest -file agent.chatmd
```

The explicit authorization flag admits this reviewed shell manifest for the
native local host. It does not override command policy or grant permanent access.
If you build commands in the repository instead of installing them, substitute
the absolute path to `_build/default/bin/chat_tui.exe` while keeping the extracted
directory as your working directory.

The runtime requires a supported confinement backend, reads the sample project
and system command directories, uses a selected environment, and grants no
writable project root or network access. `/bin/sh` runs the fixed checker with
`--check all`; the tool accepts no caller-supplied arguments. Child processes
are enabled because the script uses `awk`. A backend/admission failure must be
resolved from its diagnostics; do not change this example to an unsafe direct
backend merely to make the command run.

**Keep this TUI process running.** Background means independent of the current
conversation turn, not independent of the host. The native local example is
process-bound. Quit or stop the host and its active work is stopped; it will not
resume on a new local launch. Use a [durable daemon](../agent-server/tutorials/unix-daemon.md)
with an appropriate manifest grant when work must outlive its client.

## Start a check and inspect its result

Send:

> Run the Lantern documentation checks in the background. Explain their evidence
> when the result arrives. Do not apply changes or start another check automatically.

The model calls `begin_checks` with `{}`. Its initial response contains a job ID
and `status: "accepted"`. This is an acknowledgement, not a passing check and
not the final output. The sample is deliberately quick, so the notification may
follow almost immediately; the history still contains separate records.

The moderator starts the existing shell tool and resolves the exact invocation:

```chatml
let* job = Job.start_tool("check_docs", `Object([])) in
let* () = Invocation.resolve(invocation,
  `Pending(`Job(job), progress(job, "accepted"))) in
Task.pure(`Some({ job = job; invocation = invocation; delivered = false }))
```

The job captures `check_docs` and its input. It does not invent shell authority
from a tool name. The worker starts after the owning transaction commits, then
rechecks the admitted capability and current policy. The script retains both
identities so the eventual result can be correlated with its acknowledgement.

After completion, expect the shell result to contain exit status 1 and this stdout:

```json
[
  {"check":"setup instructions","status":"passed"},
  {"check":"source links","status":"passed"},
  {"check":"verification steps","status":"failed"}
]
```

The agent should explain that Lantern's setup document lacks verification
guidance. This is evidence of a check actually run by this tool, unlike the
pre-recorded `expected-report.json` included for comparison. Neither operation
edits the tutorial or saves a new report file.

## Follow the completion path

The host delivers an `Internal_event` whose JSON `kind` is
`background_job_completed`. The coordinator matches its job ID against retained
state and uses `Job.read_result` to materialize the result. It does not assume a
large completion will always be inline in the event.

The completion decoder keeps the canonical `Succeeded`, `Failed`, `Cancelled`
or `Expired` value. Publication passes that result through unchanged:

```chatml
let correlation = {
  key = "documentation-check";
  invocation_id = `Some(check.invocation);
  work = `Some(`Job(check.job))
} in
let* delivery = Notification.publish(
  correlation, completion(result), `Request_turn) in
Task.pure(`Some({ check with delivered = true }))
```

The correlation identifies the original invocation and its job. A work-backed
notification must match that work's actual terminal completion; transformed prose
cannot be relabeled as the job's original result. Let the model interpret the
evidence afterward, or run a separate transformation with its own contract.

Publication records delivery intent. The runtime inserts eligible data at a safe
input boundary, after the original acknowledgement has been published.
`Request_turn` asks for a follow-up turn; host pause, rate and consecutive-turn
policies still apply. Data delivery and permission to wake the model are separate.
See [notification semantics](../guide/chatml-authoring-background.md#publish-data-and-request-a-model-turn).

The original `Pending` tool response remains in history. The final result is a
new notification, not a second ordinary response or a rewritten acknowledgement.
The coordinator retains only its most recent job descriptor and suppresses a
duplicate completion after delivery has been staged. The runtime manages its own
job/result retention; replacing that descriptor does not delete earlier history.

## Query progress and cancel work

`check_progress` and `cancel_checks` also take `{}` and operate on this session's
most recent background check. They cannot target an arbitrary job ID.

| Request | Meaning |
| --- | --- |
| `begin_checks` | Admit a new job, or reject while the current completion is still outstanding. |
| `check_progress` | Return the retained job ID and current job status without consuming output. |
| `cancel_checks` | Request cancellation and return the currently observed status. |

To make the waiting state visible without adding artificial work, try the
approval variant. Exit the current local session, change the `run-checker` rule
in `runtimes/checks.chatmd` from `action="allow"` to `action="ask"`, review that
configuration, and relaunch with the same explicit manifest authorization flag.
Begin a check and leave its shell approval pending. A pending shell approval pauses
the session's foreground model work too, so the model cannot call a progress or
cancellation tool while that decision remains unresolved. Use the TUI's permission
controls to approve or deny it. Do not confuse a job's reported status with this
session-level permission wait.

While an admitted check is running, ask the agent to call `cancel_checks`.
Another `begin_checks` is rejected with `checks.busy` until the current completion
has been handled. This small check may finish before you can request cancellation;
the same pattern becomes more useful with longer project checks.
Cancellation is a request, not proof that
work stopped before any effects occurred. Wait for terminal evidence before
claiming it finished. Cancelling an already completed job does not turn its result
into a cancellation. `Job.cancel` is an immediate host operation: a later handler
failure does not roll it back. Restore `action="allow"` before using the original
variant again.

## Distinguish check failures from execution failures

| Observed result | What to explain |
| --- | --- |
| Job succeeded; shell exited 0 | The selected checks passed for the files actually inspected. |
| Job succeeded; shell exited 1 | The checker ran and found a documentation problem. Read stdout evidence. |
| Job succeeded; shell exited 2 | The checker rejected its setup or input. Read stderr. |
| Job failed, cancelled or expired | Work did not produce an ordinary successful job completion. Inspect that terminal result. |

For a setup-failure exercise, rename `sample-project/docs/setup.md` temporarily
and start a new check. Expect exit 2 with a missing-file diagnostic. Restore the
file before continuing. This is different from the expected verification failure.
If the host itself stops, do not promise that the moderator can publish a final
notification after its process has gone away.

If no follow-up appears, distinguish a queued or permission-blocked job from a
completed job whose notification has not yet been inserted, and from delivered
data whose automatic wake was declined. A progress snapshot is not a transcript
or evidence that a model response succeeded. Avoid repeatedly starting new jobs
to compensate for a missing notification.

## Finish and grow the workflow

Inspect the final result, cancel any unfinished check, then quit the TUI with
Esc, `:q`, Enter. The native local session does not resume on the next launch.
Archive or remove only your extracted example directory after the host exits.

For a longer workflow, combine this pattern with
[persistent specialists](persistent-specialist.md) or
[generated reviewers](generated-specialist.md). A coordinator can check tutorials,
assign their failures to reviewers, retain their conversations and collect
follow-up evidence. Those children need the durable host described in their lessons.
Build that composition in the [complete living documentation lab](../applications/documentation-lab.md):
background checks, narrowed generated reviewers, a persistent writer, response
watchers and an actual staged correction followed by a recheck.
Return to [ChatML workflows](../chatml/README.md) to choose the appropriate
script form, or [run and operate](../agent-server/README.md) for host lifecycle details.
