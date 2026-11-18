# Keep a workflow running after you disconnect

Learn the host lifecycle with a real timer, then extend it with background research jobs.

Separate the lifetime of the work from the lifetime of the terminal. Background research uses the documented model-job recipe as an extension.

## The outcome

Input: A daemon session and a scheduled event.

Output: An observable completion after client detach.

## Start with the observable timer

Follow the [background-agent tutorial](../agent-server/tutorials/background-agent.md).
Its source bundle demonstrates a host-owned scheduled event that stops the
session even when no TUI is attached. This timer needs no model request; it is
a small, reproducible way to understand where the work lives.

## Extend the pattern to research

A daemon-hosted ChatML moderator can start a specialist job with
`Model.spawn("agent_prompt_v1", payload)`. The recipe payload supplies `prompt`,
`input`, and `is_local`. A legacy moderator handles `Model_job_succeeded` and
`Model_job_failed`; an extensibility-v1 moderator handles `Internal_event` JSON
with `kind: "model_job_completed"` and its checked stored-completion `result`.
Use these notifications to collect results or record failure. Follow the complete contracts and tested
recipe in [ChatML orchestration](../agent-server/chatml-orchestration.md).

For example, a workflow can dispatch a question to a researcher, remain alive
while its client disconnects, and collect the result when the job completes.
That research extension requires your prompt, provider configuration, job
capacity, and completion handling; the included timer does not perform research.

Read the [agent hosting overview](../agent-server/README.md) for the deployment model and operations path.

## Understand the lifetime

Use the documented job, schedule, and session views to inspect progress after
reconnecting. `Schedule` delivers a one-shot event. Use a separate persistent
[recurrence definition](../agent-server/recurring-automation.md) to create repeated
occurrences with explicit timing and target rules. Restart recovery
does not resume arbitrary process stacks or guarantee exactly-once external
effects. [Sessions and workspaces](../agent-server/sessions-and-workspaces.md)
explains the host's ownership and persistence boundaries.

## Move from a timer to a useful background workflow

The timer introduces host lifetime; it does not run the full research extension.
Continue with [real background check results](../tutorials/background-results.md),
then the complete [living documentation lab](documentation-lab.md). The lab
combines jobs, retained state, child-response polling and notifications, including
failure, cancellation and interrupted-work recovery.

For a durable objective with repeated workers, try the
[recurring release-note review](../tutorials/goal-recurrence.md).

[Explore another application](README.md) or [follow the tutorial curriculum](../tutorials/README.md).
