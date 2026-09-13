# Keep a review ledger with a stateful tool

A reviewer often needs to revise a finding without losing the rest of the
investigation. A moderator-handled tool can keep that working state for the
conversation, while presenting an ordinary named tool to the model.

This Lantern example gives the agent `review_ledger`. It records a note for a
sample file, replaces that note on follow-up, and returns all current findings.
The ChatML moderator owns the ledger. The model decides what to record, and the
runtime admits each tool call and publishes its result.

## Open the complete example

Complete [your first local agent](../agent-server/tutorials/local-tui.md) and
[provider setup](../agent-server/quickstart.md), with access to `gpt-6-astra`.
The [reusable script-tool lesson](chatml-tool.md) introduces input/output schemas;
[the three-turn moderator](workflow.md) introduces event-driven state.

Download **Lantern review ledger** below and extract it. Open its root ChatMD,
moderator, schemas and sample files in the source reader before launching it.
From the extracted `stateful-workflow/` directory:

```sh
chat-tui --no-config --local -file agent.chatmd
```

This uses the native local host. It needs no shell manifest because the agent
has a scoped file reader and the ledger tool, with no shell capability. Respond
to any tool approval requests deliberately. The file reader's `project` root is
`sample-project/`; moderator state does not give it access outside that root.

The bundle includes:

```text
stateful-workflow/
  agent.chatmd
  scripts/ledger.chatml
  schemas/ledger-input.json
  schemas/ledger-output.json
  sample-project/docs/setup.md
  sample-project/docs/reference.md
  sample-project/expected-report.json
  sample-project/scripts/check-docs.sh
  LICENSE.txt
```

The checker is included to keep the sample project complete; this agent cannot
execute it. The report is recorded sample evidence, not a new run.

## Record two findings, then revise one

Send the agent:

> Read the Lantern setup, reference and recorded report with root project. Record
> a finding for the missing verification guidance in docs/setup.md, and another
> for how docs/reference.md explains the check results. Show the ledger.

A recording call has one stable input shape:

```json
{
  "action": "record",
  "file": "docs/setup.md",
  "note": "Add a verification section explaining the expected failing check."
}
```

The tool returns `findings`, an array of current `{file, note}` objects. Recording
`docs/reference.md` adds another entry. It does not replace the setup finding.
The exact prose chosen by a live model varies.

Now send a separate follow-up:

> Refine the setup finding to mention exit status 1 and the missing Verification
> heading. Keep the reference finding. Show the ledger again.

Recording the same file replaces its note in place. A summary call reads the
retained state without changing it:

```json
{
  "action": "summary",
  "file": null,
  "note": null
}
```

Check that the result contains two entries, that the setup note reflects the
follow-up, and that the reference note remains. Opening `sample-project/docs/setup.md`
should still show the original source: recording a proposed change is not applying
or testing that change.

## See where the state lives

The root connects a moderator to its tool:

```xml
<script id="ledger" language="chatml" kind="moderator" api="extensibility-v1"
        src="scripts/ledger.chatml"/>
<tool name="review_ledger" type="moderator" moderator="ledger"
      input_schema="schemas/ledger-input.json"
      output_schema="schemas/ledger-output.json"/>
```

There is no native `review_ledger` implementation hidden behind that name. Its
behavior is supplied by `scripts/ledger.chatml`. Such tools are sometimes called
*ghost tools*: their declared moderator must handle `Tool_invoked` and resolve the
specific invocation. A normal `Pre_tool_call` hook is a different extension point.

The script starts with an empty array of findings. Its record operation keeps
the other entries and replaces the matching file:

```chatml
let record findings updated =
  let findings : finding array = findings in
  let updated : finding = updated in
  if Array.exists(findings, fun finding ->
    let finding : finding = finding in finding.file == updated.file) then
    Array.map(findings, fun finding ->
      let finding : finding = finding in
      if finding.file == updated.file then updated else finding)
  else
    Array.append(findings, [updated])
```

The input schema permits only the three named sample files, so this ledger holds
at most three findings. That is a deliberate application rule, not a language
restriction. The schema also bounds note length.

After calculating the new state, the handler performs two distinct operations:

```chatml
let updated = record(state, finding) in
let* () = Invocation.resolve(
  call.context.invocation_id, `Complete(report(updated))) in
Task.pure(updated)
```

`Invocation.resolve` supplies the model-visible result. Returning `updated`
supplies the moderator state for subsequent events. Returning state alone would
not answer the tool call. Resolve the invocation exactly once; an unhandled or
double-resolved call is an error rather than an implicit empty success.

`let*` sequences tasks. The runtime executes their effects under the admitted
invocation context and applies the moderator's state transition. The script
ignores unrelated events by returning its existing state. See the
[execution contract](../guide/chatml-authoring-runtime.md) for transaction and
failure details. A standalone script tool starts with a fresh program environment
per call; this moderator is the right form for the retained ledger.

## Try a rejected update

Ask the agent to call `review_ledger` with `action: "record"`, the setup filename
and a whitespace-only note. The schema admits a string, but the handler rejects
this domain-level mistake with `ledger.invalid_request`. Read the summary again:
the earlier findings should remain intact.

An unsupported filename or extra input field fails schema validation before the
handler runs. A `summary` request with non-null file or note is rejected by the
handler. These are useful separate boundaries: structural validation and
application-specific meaning. An error is not a successful empty ledger.

If startup reports a missing script or schema, extract the complete bundle and
preserve its directory structure. If state seems to reset, confirm that you are
still in the same native local session. Restarting this example creates a new
process-bound session with an empty ledger. This tutorial does not configure
durable host recovery.

## Continue with background work

When finished, press Esc, type `:q`, and press Enter. Archive or remove only your
extracted example directory after exiting. Source files were not modified by the
ledger tool, and its local working state does not survive a new launch.

Next, [run checks and deliver background results](background-results.md). That
lesson keeps invocation/job correlation in moderator state, answers a tool call
before the work finishes, and later notifies the agent with the real result.
For a different execution form, return to [Choose a ChatML workflow](../chatml/README.md).
