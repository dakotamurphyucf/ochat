# Review a release-note handoff on a recurring schedule

Open [agent.chatmd](agent.chatmd) on a durable Ochat host with Goal and
recurrence authoring enabled. Keep the [project](chatml.project.json),
[moderator](moderator.chatml), [worker](worker.chatmd), and the capability
libraries at their relative locations. The host needs current source and
project access, a planning and execution grant for the controller and its
captured worker, and permission to read the worker runs when reviewing outcomes.
Use a fresh controller session for the example's stable Goal and recurrence
identities. [Recurring automation host setup](../../../agent-server/recurring-automation.md)

To include project and skill guidance, configure `guidance/` as an admitted
instruction root. Capture and explicitly activate its [AGENTS.md](guidance/AGENTS.md)
and [release-review skill](guidance/release-review/SKILL.md) before the first
review. The saved selection retains its captured bytes across compaction and
reopen. Refreshing the directory discovers new digests. Activate a new capture
to change what the controller receives. The moderator records the selected
revision and counts in its own component.
[Discover, capture and activate guidance](../../../guide/admitted-instructions.md)

Unattended recovery can reach `Session_resume` without a current operator. If
`instructions_status` returns `permission_denied`, the moderator retains its
last recorded observation. A current writer can inspect the saved selection
and submit new work after starting the session.

The initial callback creates the review Goal and stages its start. On the first
Goal-owned turn, the controller creates and enables a recurrence for the captured
worker. The first due time is the host's UTC timestamp on that Goal Run's
admission receipt. The default five-minute grace allows the first occurrence to
launch as soon as the controller finishes setting it up. The fixed-delay rule
makes the second occurrence due 60 seconds after the first worker's actual
terminal receipt. Both workers need time to finish and publish their receipts
within the default 180-second observation window. For a longer task, adjust
the grace, delay and observation window together before capturing the source.

The Goal and recurrence each request the same exact wait. Their checks stay in
separate state fields, and the lifecycle adapter stages one wait on the owned
timer. The first check is due after 90 seconds. If an occurrence is still pending,
the controller can stage up to three additional checks 30 seconds apart. Each
check uses its own timer delivery. A rejected or uncertain occurrence stops the
review, and expiration of the observation window surfaces a distinct failure.

`Goal_requests`, `Recurrence_requests`, `Lifecycle_run.compose` and
`Lifecycle.root_finish` prepare requests and apply policy to data. The selected
native tools and `Schedule.after_ms` request operations from the host.
`Lifecycle_run.stage` and `Goal.finish` stage decisions in the current actor
callback. The host commits and consumes those decisions at its scheduling
boundary. The source imports and pure helpers grant no tool access.

The controller reads the exact first two occurrence slots. Each must have its
own worker session and Run, an acknowledged admission, a visible completed Run
and a matching retained terminal outcome. After both components finish, the
controller stages the separate Goal completion request. The Goal becomes
Achieved when the host records the matching Completed terminal receipt for its
Run. [Run and Goal settlement](../../../guide/chatml-authoring-goals.md)

The waiting controller uses its original live Run authority. Explicitly
retiring that runtime retires this native Start's authority. When the timer
comes due, the host interrupts the Run and holds the Goal for source review
before delivering the timer.

The Goal and the recurrence have separate lifetimes. The example requests Goal
completion after two reviews. Its recurrence definition still belongs to the
controller. Disconnecting your client leaves the controller and its schedule
running on the daemon. To suspend just the recurrence, use its pause control.
Stopping the controller withdraws ready occurrences and pauses future launches.
Archiving the stopped controller retires the definition. Restoring and resuming
the session preserves its history and leaves that definition retired. Source
replacement and reset remain host operations. The example requires operator
review when its stored definition or source changes. Authorized readers can
inspect the acknowledged history.
[Recurrence control and recovery](../../../agent-server/recurring-automation.md)
