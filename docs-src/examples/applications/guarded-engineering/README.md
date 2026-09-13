# Guarded engineering assistant

Investigate Lantern's intentionally incomplete setup guide, run real checks,
save evidence under a reviewed capability, summarize that report with ChatML,
and ask a specialist to assess a proposed documentation improvement.

Use the complete **Guarded engineering assistant** website bundle. It assembles
this root and its shared canonical companions; copying this source subdirectory
alone omits required runtime, script, schema and sample files.

## Setup and run

Install Ochat and activate its opam environment. Configure provider access to
`gpt-6-astra`; interactive parent, specialist and optional model-review requests
are provider work. The sample needs `/bin/sh`, `/bin/cat`, `/usr/bin/grep` and
`awk`, plus an available Ochat confinement backend. Those commands are sample
dependencies. The default is required confinement, with no network capability.

Extract the complete archive and change into `guarded-engineering/`. Read both
runtime definitions, tool declarations and scripts before authorizing them:

```sh
mkdir -p reports
ochat shell inspect engineer.chatmd -canonical
chat-tui --no-config --local --authorize-shell-manifest -file engineer.chatmd
```

When using an uninstalled checkout, substitute absolute paths to the built
executables while retaining the extracted directory as the working directory.
The `reports` directory is the only writable project output location. The prompt
has no file-editing tool. Never give the sample a broader runtime merely to get
past a missing-backend error; diagnose the host setup first.

Ask:

> Investigate the Lantern setup guide. Read the source and search for verification
> guidance, run all checks, save the full report, summarize latest.json, and give
> the actual evidence to review_docs. Propose a fix with file references. Do not
> edit the tutorial or rerun report writes after an approval is denied.

Expect setup and link checks to pass, verification to fail, and the checker to
exit 1 with JSON evidence. The first full saved report is approved by the ChatML
reviewer; the actual file is `reports/latest.json`. Summarization should report
one failure for `verification steps`. Specialist wording varies; it should be
supported by the supplied source and results, not a claim of a completed fix.

## Meaningful variations

- Ask `check_docs` for `--check links --write-report`: the reviewer rejects the
  selective report. Inspect the denial; do not report a new file as saved.
- Ask for another full saved report: the reviewer defers to the user. Denying the
  request preserves the earlier report. Retained reviewer state belongs to this
  session; it records decisions, not the existence of output files.
  A new local session resets that state, so archive older reports if you need
  to retain multiple runs before requesting a new first report.
- Ask `search_docs` for a nonexistent string in `docs`: exit 1 means no match,
  not a runtime failure. A path outside the sample's read roots is out of scope.
- Temporarily rename `sample-project/docs/setup.md`, run a check, and restore it:
  exit 2 reports missing input. This differs from the expected verification failure.
- To repair the example as a human, add a `## Verification` heading and a line
  beginning `Expected result:` to setup.md. Describe the initial expected failing
  check and why it becomes passing after this correction. Run the checker again;
  all three conventions should pass. The checker does not establish prose quality.

For the separate model-review variant, exit and launch `model-engineer.chatmd`
with the same flags. It reuses the capability set and static selective-report
denial, then asks a tool-free model callback about full reports. Review its
additional provider cost and failure behavior. The callback's `agent` attribute
is a runtime identity label; `agents/reviewer.chatmd` is the separate evidence
specialist called through `review_docs`.

## Finish

Wait for work to finish, then press Esc, type `:q`, and press Enter. This native
local host is process-bound; it leaves no detached agent session running.
Reports and source files remain in the extracted directory. After exit, archive
that directory or remove only that exact copy. Provider logs and runtime caches
may be stored separately. No publishing, Git operation or network shell command
is part of this application.

The website walkthrough explains each stage, capability and failure. Its
verification record distinguishes offline runtime checks, simulated model
responses and any actual provider run.
