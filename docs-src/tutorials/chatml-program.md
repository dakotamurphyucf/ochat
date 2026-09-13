# Summarize check reports with a ChatML program

Two check runs can contain the same underlying failure. Instead of asking a
model to count repeated results, use a program to read the reports, validate
their shape and group failures by check name. The model can then explain the
evidence and suggest what to investigate.

This lesson uses supplied reports from the Lantern documentation project.
Reading them does not run a build, test or documentation checker. The useful
operation here is deterministic aggregation through the agent's file tool.

## Before you start

Complete [the file-tool lesson](file-tool.md) and read
[ChatML execution choices](../chatml/README.md). Use an installed Ochat with the
native local TUI, a configured provider, and access to the selected `gpt-6-astra`
model. Interactive requests use your provider; the documented offline checks
use a deterministic provider fixture.

Open the complete source files below to inspect this layout. Download and extract
the **ChatML report program** bundle when you are ready to run it:

```text
chatml-program/
  agent.chatmd
  scripts/aggregate.chatml
  reports/report-a.json
  reports/report-b.json
  LICENSE.txt
```

Run from the extracted directory containing `agent.chatmd`:

```sh
chat-tui --no-config --local -file agent.chatmd
```

The root declares `run_chatml` and a file reader with two roots: `reports` for
the data and `programs` for the maintained script. The script's selected file
binding retains both roots; selecting a tool does not reconfigure its authority.

## Ask for a repeatable calculation

Send this request:

> Read aggregate.chatml through the programs root. Run that exact program
> with input ["report-a.json", "report-b.json"] and tools ["read_file"]. Tell me
> which checks failed and how often. Do not run the checks themselves.

The agent reads the script and supplies its contents as the `source` parameter
to `run_chatml`. The other parameters are:

```json
{
  "input": ["report-a.json", "report-b.json"],
  "tools": ["read_file"]
}
```

This is a parameter excerpt, not a complete call: `source` must contain the full
program. The [native request reference](../guide/chatml-native-requests.md)
documents the exact request schema. Inspect the actual tool activity to confirm
the program ran; a model-written answer with the same numbers is not evidence.

## Follow the program

The complete [aggregate script](../examples/learning/check-reports/scripts/aggregate.chatml)
separates three kinds of work:

1. `read_report` calls the admitted file reader. It checks the tool result and
   removes the file reader's two metadata lines before parsing the JSON body.
2. `add_report` validates each reported status and accumulates failed checks.
   A successful check contributes no failure. Unexpected data fails visibly.
3. `main(input)` sequences the reads with `let*` and returns a JSON summary.

The task returned by `Tool.call` describes work for the runtime. `let*` waits for
that task's result before decoding it and reading the next file. Script execution
itself does not require another model conversation, a moderator, or a child agent.

The shared source also exports `run(ctx, input)`. That small wrapper is for the
[next lesson's reusable tool](chatml-tool.md); `run_chatml` uses `main(input)`.

## Check the result and the boundary

For the bundled reports, the deterministic result is:

```json
[
  { "check": "setup instructions", "failures": 2 },
  { "check": "verification steps", "failures": 1 }
]
```

Try a request for `../agent.chatmd` through the `reports` root. The native file
reader rejects the escape and aggregation fails. The script uses the same file
boundary as a direct model tool call; being code does not enlarge that access.

If you remove `read_file` from the selected tools, the program cannot perform its
reads. If a report is missing, truncated, invalid JSON or has an unsupported
status, inspect the error rather than asking the model to fabricate a result.
For larger files, adapt the reader to its documented pagination/truncation
contract; this small example expects complete reports.

## Finish and continue

Quit the TUI with Esc, then `:q` and Enter when the request is finished. No
background work was started by this program. Preserve the source files if you
want to reuse it; invocation globals do not provide persistent workflow memory.

Next, [package the same program as a named tool](chatml-tool.md). The agent will
then supply report filenames without reading and submitting source on every call.
