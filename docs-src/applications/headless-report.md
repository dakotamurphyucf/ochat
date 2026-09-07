# Generate a project report from a script

Run one request without the terminal UI and save the conversation as an inspectable artifact.

Use the same file-based authoring model from a shell script or CI job, with a fresh transcript for each independent run.

## The outcome

Input: A project status note.

Output: A saved ChatMD report.

## Produce a saved report

Complete [installation and provider setup](../agent-server/quickstart.md), extract
the headless-report bundle, and run from its directory:

```sh
mkdir -p runs
ochat chat-completion -prompt-file agent.chatmd -output-file runs/report-01.chatmd
```

`agent.chatmd` includes a request to summarize the fictional Lantern status note.
Open `runs/report-01.chatmd` to inspect the conversation and report. Expect
Completed, Blocked, and Next steps sections grounded in `reference/status.txt`.
The preview is an illustration; no provider-generated result is implied.

## Put it in a script or CI job

Install Ochat and configure the provider environment in the runner. Supply the
input files, set the working directory to the extracted example, and choose a
fresh output path for each independent invocation. Do not reuse an existing
transcript unless you intend the [batch resume behavior](../cli/chat-completion.md).
Keep credentials in the runner's secret configuration and publish transcripts
only when their contents are suitable for the intended audience.

## Adapt the artifact

Use the prompt for release-readiness notes, a documentation checklist, or a
summary of supplied test results. The saved output is ChatMD, not a standalone
HTML report or an automatic success/failure gate. Define and validate any
machine-consumed report contract before using it to control a pipeline.

[Explore another application](README.md) or [follow the tutorial curriculum](../tutorials/README.md).
