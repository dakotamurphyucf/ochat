# Stage a Goal and its first episode

Create and start a fresh authored session from `agent.chatmd` on an
execution-enabled host with extensibility-v1 and Goal tools configured. Use an
authorized account with transcript-view and send access. The session starts
without any Goals or an admitted Workflow. Keep the sibling library at
`../../libraries/goal-requests` and add that exact library directory to the
host's `server.project_sources.external_roots`. The moderator imports its checked
request constructors from that directory. The prompt imports the built-in
`goals` instructions and selects Get, Create and Start. The host checks source
and tool access when the session runs.

During the genuine first `Session_start` callback, the moderator reads the actual
catalog and source, creates a held Goal with finite turn and attempt limits,
then reads its projected Goal and catalog revisions. Start supplies an explicit
`user_submission` for the first Turn because the current callback has already
consumed authored startup. The example may therefore submit model work after
the callback completes. Use a configured host and workspace where you intend
that first Turn to run.

The tool response confirms staging in the current callback. The host must
acknowledge the callback and release its source before the execution scheduler
can admit the first Workflow. Inspect the durable Start Invocation and actual
Goal and Run to distinguish staged intent, admitted work and completion. The
moderator's Boolean is a local checkpoint for staging. On reopen, the host
reconciles actual retained data under its original authority. The example defers staging if it finds an existing Goal or if the host
denies that callback transcript-view access. When the installed source changes,
use a host that exposes the authorized source-adoption command. Adopting a
source preserves the old episode and its accounting. Start a new episode after
the old Run settles. Resume applies to a held Run on its original installation.

The pure package also builds objective, progress, hold and abandonment
requests. Select each corresponding native tool and obtain a current Get
before a new edit. Preserve the complete request and key for transport
retries. Use the checked `Goal.finish` orchestration action from its authorized
Workflow callback when you have evidence that the Goal is complete. To continue
a held episode on a host that supports Resume, select `goal_resume` and use
the linked Run revision from `current_run` in the same `goal_get` row. A null
`current_run` means there is no eligible Run in that read.
