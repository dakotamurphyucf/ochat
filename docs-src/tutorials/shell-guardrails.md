# Design shell capabilities and guardrails

Reading a tutorial and running project code need different capabilities. Give
each operation its own runtime, then ask for approval when a check should leave
an output file behind.

This complete Lantern checkpoint combines inspection with a real documentation
checker. It finds an intentional missing verification section. You will see
failed-check evidence, invalid inputs, and an approved or declined report write.

## Before you start

Complete [useful shell inspection](../agent-server/tutorials/shell-agent.md).
Use installed Ochat, a configured provider with access to `gpt-6-astra`, a supported
required sandbox backend, and `/bin/sh` plus `awk`. Those Unix tools are this sample
project's dependencies; ChatML itself does not depend on awk.

The **Lantern shell guardrails** bundle is independent of the earlier download.
Inspect its complete sources below and extract the bundle:

```text
shell-guardrails/
  agent.chatmd
  runtimes/inspection.chatmd
  runtimes/checks.chatmd
  sample-project/
    docs/setup.md
    docs/reference.md
    scripts/check-docs.sh
    expected-report.json
  LICENSE.txt
```

From the directory containing `agent.chatmd`, create the output directory before
Ochat resolves the writable root:

```sh
mkdir -p reports
ochat shell inspect agent.chatmd -canonical
chat-tui --no-config --local -file agent.chatmd --authorize-shell-manifest
```

If using checkout binaries, substitute their absolute paths as described in the
[first lesson](../agent-server/tutorials/shell-agent.md#prerequisites-and-command-context).
Keep the extracted bundle as the working directory. Opening the prompt sends no
initial model request; requests you submit use your configured provider.

## Two tools, two runtimes

| | `inspect_setup` / `inspection` | `check_docs` / `checks` |
| --- | --- | --- |
| Operation | Read a fixed setup file with `cat`. | Run the maintained checker through `sh`. |
| Model input | No parameters. | A JSON array of 2–3 literal arguments. |
| Project reads | `sample-project`. | `sample-project`, plus `/bin` and `/usr/bin` for interpreter helpers. |
| Project writes | None. | `reports`, outside the input project directory. |
| Child processes / project code | Not enabled. | Enabled because the script invokes awk. |
| Network / privilege changes | Not enabled. | Not enabled. |
| Approval | Fixed inspection is allowed. | Ask when `--write-report` is present. |
| Limits / confinement | Bounded time/output; required backend. | Bounded time/output; required backend. |

Both tools use `mode="fixed"`: the author fixes the executable and leading
arguments. The checker has a structured JSON input, rather than a free-form shell
command. This differs from Ochat's `mode="structured"`, which lets the model select
an executable too. Use the narrow interface that fits the work.

The checker declaration fixes the script path before model-supplied arguments:

```xml
<tool name="check_docs" type="shell" mode="fixed" runtime="checks"
      result="structured" nonzero="result">
  <command program="/bin/sh">
    <path_arg base="workspace" path="sample-project/scripts/check-docs.sh"/>
  </command>
  <arguments mode="required" min_count="2" max_count="3" max_item_bytes="64"/>
</tool>
```

The model cannot replace that script path. The script validates check names; the
schema bounds argument count and size. The runtime allows the interpreter and
asks about report-writing requests:

```xml
<policy default="deny" merge="replace">
  <rule id="run-checker" action="allow"><basename value="sh"/></rule>
  <rule id="review-report" action="ask"><argument value="--write-report"/></rule>
</policy>
```

Open `checks.chatmd` for the full filesystem roots, selected environment, limits
and capabilities. An `ask` match takes precedence over an `allow` match; a hard
denial would still win. These are runtime rules; the fixed script is the tool's
operation.

## Run checks and explain the evidence

Send:

> Inspect the setup tutorial, then run all its checks. Do not write a report yet.
> Explain the failed check using the tool results.

The checker call is:

```json
{ "arguments": ["--check", "all"] }
```

Its structured shell result has exit status 1 and this JSON in stdout:

```json
[
  { "check": "setup instructions", "status": "passed" },
  { "check": "source links", "status": "passed" },
  { "check": "verification steps", "status": "failed" }
]
```

`nonzero="result"` preserves failed-check evidence as a normal tool result. Exit 1
means a check failed; it does not mean the program failed to start. Exit 2 means
invalid arguments or a missing input. The checker tests three documented
conventions, not arbitrary prose accuracy. A reviewer still needs to assess the
instructions' usefulness.

For only the link convention, use:

```json
{ "arguments": ["--check", "links"] }
```

Inspect `sample-project/docs/reference.md` for all supported checks.

## Approve or decline a report

Send:

> Save the current check evidence to a report so I can inspect it afterward.

The request becomes:

```json
{ "arguments": ["--check", "all", "--write-report"] }
```

The `checks` runtime requests approval. Until you answer, this request must not
create `reports/latest.json`. On Shell Security, select **approve once** and submit
the choice. The command returns the same failure evidence and saves it to that
file. A report is output data; it does not fix the tutorial.

Try declining in a fresh run without a report. The tool reports denial and writes
no report. If an earlier report exists, declining a new request does not delete
it: inspect the new tool result rather than using file existence as evidence of
a new execution.

Other already-running calls can complete while a sibling waits for approval.
The waiting command itself does not run until approved. Session-scope approval,
when offered, has different reuse behavior; **approve once** makes this lesson
easy to observe.

## Understand the boundaries

Ochat analyzes each requested command before applying capability and policy
checks. Compare the two operations:

| Request | Built-in effect analysis | What this means |
| --- | --- | --- |
| `cat` with the fixed setup path | Read that path. | The read must fit the admitted roots. |
| `sh` with the checker script | Arbitrary code and child processes. | The runtime must admit both capabilities, even when this particular invocation only checks files. |

Effect analysis describes the command conservatively; it does not trace the
checker and infer its complete file access. In particular, adding
`--write-report` does not turn the interpreter analysis into a precise write-path
list. The explicit argument rule requests approval, while the required backend
restricts actual writes to the configured roots. `shell inspect` displays the
compiled declarations before execution; per-command analysis happens when a
concrete tool request is prepared.

Try check name `all; touch injected`. It is one literal argument, not shell source.
The checker rejects it with exit 2 and no injected command runs. Extra arguments
to `inspect_setup` fail that tool's input schema.

The checker runtime permits project code and child processes. Its `sh` allow rule
does not inspect every action inside the script. The backend enforces supported
filesystem and network boundaries. The script chooses a fixed filename within
`reports`; the runtime grants that directory, not just one filename. Review the
script before authorizing it, especially when adapting this pattern to a build
system that executes more code.

On macOS, child executable paths must be inside admitted read roots (including
the backend's system support roots). The explicit `/bin` and `/usr/bin` reads let
`sh` start its interpreter and awk helpers. These are executable directories, not
permission to read your home directory or to write outside `reports`.

The example selects its environment, including `PATH` and `LC_ALL`, and needs no
credentials or network. If output is truncated or a limit terminates the command,
treat the result as incomplete evidence.

## Change the configuration deliberately

| Change | Expected consequence |
| --- | --- |
| Omit `reports` before admission | Root resolution may fail; create it before startup. |
| Remove the writable root | Report creation loses its declared write capability; ordinary checks need no report writes. |
| Change the report rule from `ask` to `deny` | Report requests are rejected despite manifest startup authorization. |
| Disable child processes for the checker | Its awk subprocess loses its admitted capability. |
| Lower time/output limits | Slow or large checks can terminate or return truncated evidence. |

Reinspect and restart after editing declarations. Authorization of an earlier
manifest is not trust for changed configuration. Read the
[runtime reference](../overview/chatmd-shell-runtime.md) for exact merge and platform
behavior and [host integration](../guide/chatmd-shell-host-integration.md) for daemon use.

## Finish and continue

Quit with Esc, then `:q` and Enter after work finishes. Keep the report as evidence
or remove the disposable bundle. No detached work was started.

Next, explore [custom decisions with ChatML and reviewer agents](../shell/README.md#customize-decisions-with-scripts-and-agents).
Static rules handle a report flag; hooks and reviewer agents help when a decision
depends on project-specific context.
