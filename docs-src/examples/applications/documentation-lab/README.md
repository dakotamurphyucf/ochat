# Living documentation lab

Check two tutorials, create a task-specific reviewer, collect its response through
a timer-driven watcher, ask a retained writer for a proposal, stage that text
through a pre-authorized fixed capability and run a real recheck. The
conversational parent makes assignments; one ChatML moderator owns background
work, watchers and the evidence ledger.

Use the complete website archive. Sample inputs are assembled under
workspace/sample-project/ beside the scripts, prompts and private server config.
The raw repository source subdirectory is not the assembled runnable bundle.

## Setup

Install Ochat, configure provider access to gpt-6-astra, and use a supported
required confinement backend. The sample checker needs /bin/sh and awk; staging
also uses cat through PATH=/usr/bin:/bin. No helper service or second daemon is
required. The included staging/README.txt ensures the writable directory exists.

Read lab.chatmd, runtimes/tutorial-checks.chatmd and server.sexp before startup.
The private lab permission profile explicitly assumes authorization of this
inspected prompt's shell manifest and allows its declared tools. This enables
background reads/checks; it is an operator choice in this example configuration.
Required confinement, no network and fixed staging-directory writes still
apply. This profile does not prompt for individual commands: tool_default allow
also supplies the host's automatic shell approver. Do not describe an ask rule
under this profile as requiring a human click. Do not reuse the profile for
unreviewed prompts. Exact manifest grants are
an alternative when adapting the deployment.

From the extracted directory:

```sh
ochat shell inspect lab.chatmd -canonical
ochat-agent-server -config "$PWD/server.sexp" -validate-only
ochat-agent-server -config "$PWD/server.sexp"
```

In a second terminal in the same directory:

```sh
chat-tui --no-config --connect "unix://$PWD/agent.sock" \
  --new-daemon-session --prompt lab --workspace lab
```

Checkout users can substitute absolute built executable paths. The private store
is private-data/, outside workspace/. File tools expose only the sample project;
generated reviewers inherit the existing read_file binding, not the whole parent
tool set. A transient local TUI cannot replace this durable daemon setup.

## Run the workflow

Ask the parent to check the original tutorials in the background, investigate
the failed tutorial with a dynamically created read-only reviewer, and ask the
persistent writer for a proposed Verification section. Have it show the proposed
text before requesting staging.

1. begin_lab_checks with phase original returns an accepted run/job ID. Inspect
   the later terminal notification and lab_report. Expected original evidence is
   one passing and one failing tutorial; expected/original.json is a comparison
   fixture, not evidence of execution.
2. The parent queries authoring guidance, captures a task-specific ChatMD, validates
   it and creates an owned running child selecting only inherited read_file.
   agents/reviewer.chatmd illustrates the captured shape; it is not implicitly
   launched merely because that file exists.
3. agent_send returns the submission receipt. watch_lab_review takes tutorial_id,
   role reviewer, session_id and receipt_id. It returns a watch ID immediately,
   then polls the receipt with jobs/timers. The same receipt cannot be registered
   twice. A new follow-up receipt may be watched in the same retained session.
4. The parent calls propose_fix with evidence. This authored writer is persistent;
   keep its session ID and register its receipt with role writer. Give it reviewer
   findings explicitly; it cannot read another conversation by itself.
5. stage_lab_proposal takes arguments ["broken", "the proposed section"]. It
   acknowledges background work and stages a copy through the fixed capability
   already authorized at startup. Only workspace/sample-project/staging/broken.md
   can be written. A different tutorial ID is rejected before writing. The
   proposal is a literal argument, never evaluated as shell code.
6. Only after a successful stage, begin_lab_checks with phase staged runs the real
   checker again. Passing still checks the original passing tutorial; broken
   checks the staged copy. The original broken tutorial stays unchanged.
7. lab_report contains actual check/stage results and correlated review snapshots.
   Read extra output pages when needed, retain next_cursor for the same query,
   and reconstruct fragments. Preserve failures, uncertainty and disagreements.
8. close_lab closes the coordinator, cancels pending jobs/watches and requests
   cancellation of known reviewer sessions. Inspect stop progress; admission is
   not proof that all cleanup has joined. Stop any child created but never
   registered with a watch separately.

For a walkthrough that requests human approval before writing a report, use the
guarded engineering application with its interactive native-local host. This
private lab deliberately authorizes its narrow sample staging tool up front so
background work and retained children can progress unattended.

The checker requires an exact ## Verification heading and a line beginning
Expected result:. A mechanically passing proposal is not a prose-quality judgment.
Nothing publishes changes or changes the original tutorials.

## Watch and failure behavior

Each watch polls every two seconds for at most five minutes. These are example
choices in scripts/coordinator.chatml. Only the active job/timer epoch can advance
the watch. A completed watch records one receipt/output snapshot and stops rearming;
it is not a subscription to all future messages in that conversation.

A succeeded watch can contain a failed, cancelled or interrupted CHILD receipt.
Inspect wait.receipt.status and output instead of equating the watcher finishing
with a successful review. Idle and caught_up are not receipt-success signals.
Missing output, a probe failure or the deadline leaves an explicit result; use
direct lifecycle reads to investigate before deciding on replacement work.

The sample keeps at most eight run records and eight watcher records per parent.
Only one check/stage job is pending at a time. Close the lab and use a new parent
session deliberately for another investigation; do not spin indefinitely.

## Cancellation, interruption and shutdown

close_lab does not undo a completed or partially executed external write. Inspect
the staged file after interruption and deliberately recheck it. A rejected stage
or missing staged input is not a passing recheck. The fixed stager replaces its
one destination; preserve a copy yourself before requesting replacement evidence.

Durable records do not resume an interrupted process or arbitrary program counter.
An interrupted probe is reported as a failed watch rather than retried silently.
Overdue timer delivery rechecks the application's deadline. Recover retained job,
receipt and output state through the host; authorize a fresh attempt only after
reconciling possible external effects. Do not describe a restart as an automatic
continuation of every stage.

Quit the TUI with Esc then :q and Enter; stop the daemon with Ctrl-C. Parent stop
cancels owned children. Keep the extracted directory for retained session data;
deleting private-data/ removes this example's store. Original input files and
staged proposals remain separate from those durable session records.
