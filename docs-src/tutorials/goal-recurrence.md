# Review release notes on a recurring schedule

A controller can retain the objective “Review the release-note handoff,” launch
independent review workers, and request completion after it has evidence from two
actual Runs. This example shows how Goals, recurrence, captured instructions and
ordinary ChatML libraries fit together on a durable host.

## Prepare the controller

In the [Examples catalog](../examples/README.md), choose **Download recurring
release-note review bundle**. Put the archive in a new directory, then extract it and enter the bundle directory.

```sh
tar -xf goal-recurrence.tar
cd goal-recurrence
```

Keep the extracted source tree together. The controller's definition is
`examples/learning/goal-recurrence/agent.chatmd` relative to this directory.
Its ChatML project imports ordinary libraries under `examples/libraries/`. Its
separately declared worker lives beside the controller in `worker.chatmd`.
Add a prompt for the controller and explicitly allow the extracted
`examples/libraries/` directory under `server.project_sources.external_roots`.
[Daemon configuration](../agent-server/configuration.md) ·
[Installed sources and explicit export](../guide/chatml-installed-libraries.md)

Send `session.create` with `start_immediately` set to `false` in its
session specification. Use the returned session ID in the daemon's
`recurrence_automation` and `recurrence_execution` settings. Grant captured-child
execution for this controller and give the workflow the source reads, model access
and worker Run visibility it needs.
[Session protocol](../agent-server/protocol.md#sessions-and-messages) ·
[Planning and execution configuration](../agent-server/configuration.md#recurrence-planning-and-execution)

To include the example's project guidance and skill, also admit
`examples/learning/goal-recurrence/guidance/` as an external root and grant its
exact root to the selecting operator in `server.authoring_project_access.read_grants`
before restarting the daemon.

Restart the daemon with the configured store and attach to the same session with
write access. If you configured the guidance root, refresh it at project scope
and as a skill root. Capture the returned `AGENTS.md` and `release-review/SKILL.md`
in their respective selections. Activate each capture with the current generation,
selection revision and a fresh key.
[Unix daemon setup](../agent-server/tutorials/unix-daemon.md) ·
[Select project guidance and skills](../guide/admitted-instructions.md#explicit-operator-and-headless-client-controls)

Call `session.start` after setup and keep the planning and execution host live
while the workers run.

The checked local workflow uses a controlled provider. Interactive model requests
use your configured provider and may incur costs. Use a fresh controller session
when rerunning the example because its Goal and recurrence IDs are stable.

## Follow the two reviews

1. Startup creates the Goal and stages its Workflow. The actual Run admission
   provides the first due timestamp and the association with the Goal.
2. The controller creates a paused fixed-delay recurrence for the declared
   worker, then enables it. The second occurrence becomes due 60 seconds after
   the first worker's acknowledged terminal receipt. Read the controller's
   status and each occurrence's own retained outcome.
3. The Goal and recurrence components keep separate state and propose the same
   wait. The lifecycle helper coalesces it into one request. If the first review
   check finds an unfinished occurrence, the example can wait and read again.
4. After the controller sees two distinct completed workers and their receipts,
   it requests `Goal.finish`. The Goal may stay open while the host joins the
   work. The matching Completed terminal receipt settles it as Achieved.

The example's `builtin:` imports supply instructions to the model. An explicit
project or skill selection preserves exact instruction bytes. Editing a live
file changes what a new refresh can discover. A resumed saved revision uses its
captured code and selected guidance. Provider submission still checks current
authority. [Builtin instruction assets](../guide/builtin-instructions.md)

## Inspect evidence and stop future work

Use [Goal inspection](../agent-server/protocol.md#inspect-and-control-persistent-goals)
to compare the current objective, its Run association and observed usage. A
missing provider count stays unknown. An observed-token ceiling is a host policy
on available counts. Your provider determines what it bills.

Use the controller's [recurrence status and history](../agent-server/recurring-automation.md)
to find the two occurrences. Target Run details require separate visibility on
each worker. An acknowledged schedule change confirms the schedule. Each actual
occurrence has its own admission and outcome evidence.

Client detach leaves the daemon and its grants in place. Pause the recurrence to
fence future launches. Stopping the controller pauses future occurrences, and
restarting the daemon does not automatically enable them. Already admitted or
uncertain effects retain their original evidence. Current session recovery uses
saved checkpoints and receipts. Multi-window context rollover is outside this
example. [Session recovery](../guide/chatml-session-lifecycle.md)
