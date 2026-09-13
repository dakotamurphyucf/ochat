# Make a reusable ChatML tool

Give the agent a tool named `summarize_checks` that accepts report filenames and
returns failure counts. This wraps the [previous lesson's program](chatml-program.md)
in a declaration with input/output schemas and an explicit selected-tool dependency.
The calculation stays in one maintained source file.

Use a standalone tool for logic that can start fresh on every invocation.
For a tool that needs to remember a review queue or coordinate later results,
use a [moderator-handled tool](../guide/chatml-authoring-runtime.md#moderator-tools-and-session-owned-state)
instead.

## Before you start

Use an installed native Ochat TUI, provider configuration and access to
`gpt-6-astra`. No daemon or moderator is needed for this synchronous tool. Open
the source viewer below to read every file; download and extract the
**Reusable check-summary tool** bundle to run it:

```text
chatml-tool/
  agent.chatmd
  scripts/aggregate.chatml
  schemas/input.json
  schemas/output.json
  reports/report-a.json
  reports/report-b.json
  LICENSE.txt
```

From that extracted directory:

```sh
chat-tui --no-config --local -file agent.chatmd
```

Ask: **Use summarize_checks on report-a.json and report-b.json, and explain which
checks repeatedly failed.** Interactive use makes model requests; the offline
composition tests supply deterministic provider responses.

## Read the declaration

The complete [root ChatMD](../examples/learning/check-reports/tool.chatmd) declares
the file reader, attaches the script as `kind="tool"`, and exposes its `run`
entry point as a named tool. The binding portion is:

```xml
<script id="report_program" language="chatml" kind="tool" src="scripts/aggregate.chatml"/>
<tool name="summarize_checks" type="chatml" script="report_program" entrypoint="run"
      input_schema="schemas/input.json" output_schema="schemas/output.json"
      description="Count failures by check name across the selected report files.">
  <uses tool="read_file"/>
</tool>
```

This excerpt depends on the file-reader declaration and companions in the bundle.
The schemas describe an object with a `files` array as input and an array of
`{check, failures}` objects as output. They describe data shapes, not file access.

`<uses tool="read_file"/>` selects an existing binding. It does not add a general
filesystem API or a new root. In this root, the file reader permits only the
bundled reports directory. The script has no need to read its own source:
Ochat loads the attached file as part of the definition.

## Reuse the calculation, change the calling contract

The shared [aggregate script](../examples/learning/check-reports/scripts/aggregate.chatml)
exports both entry points. `main(input)` calculates the summary. `run(ctx, input)`
extracts `input.files`, sequences that task and wraps its JSON in the tool's `Complete`
outcome. `ctx` is the supplied invocation context; this calculation does not need
to inspect it.

The model sees the named tool and its schema. It submits `{"files": ["report-a.json", "report-b.json"]}`,
not a program or an arbitrary function name. The runtime validates the input,
executes the attached handler with its admitted dependencies, and validates the
result against the output schema.

## Observe success and failure

The two bundled reports return setup-instruction failures twice and a
verification-step failure once, just as in the previous lesson. Ask for either
report alone to change the calculation. Repeating a call starts with fresh script
globals; this is not a retained specialist conversation or persistent queue.

An input missing `files`, or containing a non-array `files`, is rejected before file reads.
A missing or denied file fails the operation. Invalid report contents fail during
decoding. An output that violates the declared schema is rejected rather than
presented as a valid tool result. Inspect the actual tool outcome, not just the
model's explanation.

Keep root, script and schema files together. Missing files cause source/admission
errors. After editing the source, reopen a new session to capture the new
definition; modifying a file does not silently replace a running captured artifact.

## Finish and extend

Quit with Esc, then `:q` and Enter after the request completes. To reuse this
tool, carry its script, schemas and file-access declaration with the root.

To retain state across calls, learn about
[stateful moderator tools](../guide/chatml-authoring-runtime.md#moderator-tools-and-session-owned-state).
To return promptly while work continues, see
[background work and notifications](../guide/chatml-authoring-background.md).
For the exact declaration and outcome rules, keep the
[execution-contract reference](../guide/chatml-authoring-runtime.md) nearby.
