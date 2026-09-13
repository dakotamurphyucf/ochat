# Customize shell decisions with ChatML

A useful agent can run checks and save evidence. A project-specific reviewer can
decide which reports are worth saving, remember earlier decisions, and involve
the user when a later request needs attention.

This Lantern checkpoint keeps the two shell runtimes from the
[guardrails lesson](shell-guardrails.md). It adds a ChatML reviewer with three
observable outcomes: reject a selective report, approve the first full report,
and ask the user about subsequent full reports. A separate entry point shows
model review without changing the main example's deterministic behavior.

## Start the complete project

Use installed Ochat, provider configuration for `gpt-6-astra`, `/bin/sh` and `awk`,
and a supported required confinement backend. Download **Lantern custom shell
decisions**, inspect its sources below, and extract it. The bundle includes the
sample inputs and checker; you do not need an earlier download.

```text
shell-customization/
  agent.chatmd
  model-agent.chatmd
  tools.chatmd
  runtimes/
    inspection.chatmd
    checks-base.chatmd
    checks.chatmd
    model-checks.chatmd
  scripts/report-reviewer.chatml
  sample-project/
    docs/setup.md
    docs/reference.md
    scripts/check-docs.sh
    expected-report.json
  LICENSE.txt
```

From the extracted directory:

```sh
mkdir -p reports
ochat shell inspect agent.chatmd -canonical
chat-tui --no-config --local -file agent.chatmd --authorize-shell-manifest
```

Checkout users can substitute absolute build paths as described in
[shell inspection](../agent-server/tutorials/shell-agent.md#prerequisites-and-command-context).
Keep the extracted project as the working directory. Interactive requests use
your provider; the recorded offline tests use deterministic provider responses.

## Connect the reviewer

`tools.chatmd` binds `check_docs` to the `checks` runtime. Its fixed command starts
the maintained checker; the model supplies literal check arguments. The base
runtime defines the read/write roots, environment, limits, and report `ask` rule.
The root declares the script:

```xml
<script id="report-reviewer" language="chatml" kind="shell_reviewer"
        src="scripts/report-reviewer.chatml"/>
```

`runtimes/checks.chatmd` extends the base and attaches that script as a reviewer:

```xml
<shell_access id="checks" extends="checks-base">
  <reviewers strategy="first_terminal">
    <reviewer id="project" kind="chatml" script="report-reviewer"
              lifecycle="session" failure="deny"/>
    <reviewer id="human" kind="ui"/>
  </reviewers>
</shell_access>
```

The runtime's `ask` rule starts this chain. `Review.approve()` or
`Review.deny(reason)` ends it; `Review.defer()` passes the request to the next
reviewer. A script error follows `failure="deny"`, so it does not silently pass a
broken decision to the user.

This hook is a small program running inside the shell-review surface. It receives
normalized command context through `Shell.argv(event)` and retains the state
returned by `review(event, state)`. It cannot call file tools, another agent, or
`Process.run`. Use a conversation moderator for that broader orchestration.

## Read the decision logic

The script begins with `initial_state = false`, meaning it has not yet issued its
automatic full-report approval. It recognizes the fixed command's five-element
argv: interpreter, checker path, `--check`, `all`, `--write-report`.

```ocaml
let review event already_approved =
  let argv = Shell.argv(event) in
  if is_full_report(argv) then
    if already_approved then
      let* () = Review.defer() in
      Task.pure(already_approved)
    else
      let* () = Review.approve() in
      Task.pure(true)
  else
    let* () = Review.deny("Saved reports must contain all check categories.") in
    Task.pure(already_approved)
```

Open `scripts/report-reviewer.chatml` below for the complete predicate and script.
`let*` sequences the review task and returned state. Constructing a task alone
does not perform its action. A valid terminal action and its state commit
together; a failed hook invocation does not commit partial state.

The Boolean records **an approval decision**, not a successful write. If the
approved command subsequently fails, the next report still reaches the user.
Likewise, starting a new local session resets this state even if a report already
exists on disk. This is a review-budget example, not a file-existence lock.

## Observe denial, approval, and deferral

Use a fresh session and ask for these operations separately:

| Request | Expected behavior |
| --- | --- |
| Check links without saving. | The base policy allows the check; the reviewer is not needed. |
| Save a report containing only links. | The ChatML reviewer denies it. No new report is written. |
| Save a report containing all checks. | The script approves this first full-report request without a human prompt. |
| Save another full report. | The script defers to the user. The request waits for your choice. |

For the first full report, the tool call is:

```json
{ "arguments": ["--check", "all", "--write-report"] }
```

The checker returns exit 1 and the same three-result JSON as the previous lesson:
setup and links pass; verification fails. It writes `reports/latest.json`.
Script approval does not turn a failing check into a passing result.

On the subsequent request, choose **deny** to retain the existing report, or
**approve once** to replace it with fresh evidence. Denial does not delete a file
written by a previous request. A remembered broader approval, if you deliberately
choose one, may allow matching requests without consulting this reviewer again.

Hard policy denials and capability restrictions remain in force. The reviewer
cannot grant network access, add writable roots, or replace the fixed tool's
script path. Changes to the manifest require inspection and fresh authorization.

## Try the separate model-review variant

Quit the current UI with Esc, then `:q` and Enter. Inspect and start the alternate
entry point from the same directory:

```sh
ochat shell inspect model-agent.chatmd -canonical
chat-tui --no-config --local -file model-agent.chatmd --authorize-shell-manifest
```

This variant replaces the stateful ChatML/human chain. Its policy denies selective
report writes before review, and a separate `gpt-6-astra` request assesses each
full-report request that needs approval:

```xml
<reviewer id="model-security" kind="model" agent="lantern-report-review"
          model="gpt-6-astra" failure="deny"/>
```

The stock Ochat adapter uses a built-in, tool-free reviewer prompt. Here `agent`
is the reviewer's identity label, **not a path or a named ChatMD agent-tool lookup**.
Declaring a separate agent tool with that name does not customize the reviewer's
instructions. A custom host can provide its own model-completion adapter; that
is a different integration surface.

The reviewer sees a bounded, redacted description of the command, executable
identity, working directory, effects, and policy. It cannot read the tutorial or
execute the proposed command. This can add judgment about a command's context;
it cannot establish that project prose is correct. Keep hard project constraints
in policy or deterministic hooks.

It must return strict decision JSON, such as `{"decision":"allow_once"}` or a
denial with a reason. Its decisions may vary. Malformed output, timeout, transport
failure, or an unsupported approval scope fails closed. There is no automatic
human fallback in this variant. It makes an additional provider request; offline
protocol checks do not demonstrate live model judgment.

## Adapt the pattern

Combine this review policy with file/search tools, saved-report processing and
a specialist in the [guarded engineering application](../applications/guarded-engineering.md).
Its complete bundle shows how the capabilities work together in one investigation.

Use a ChatML reviewer when the decision depends on normalized command details or
retained review state. Use a matcher for selecting a policy rule, a before-hook
to reject or rewrite a request, and an after-hook to control result disclosure.
Any rewrite is checked again; an after-hook cannot undo a command that ran.

The [extension guide](../guide/chatmd-shell-extensions.md) documents each surface,
state lifetime, and failure behavior. For a specialist that reads actual findings
and discusses them, use an [agent tool](specialist.md) or
[persistent specialist](../guide/subagents.md), separately from shell approval.
