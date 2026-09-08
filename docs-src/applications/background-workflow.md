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
`input`, and `is_local`. Handle `Model_job_succeeded` and `Model_job_failed` events
to collect results or record failure. Follow the complete contracts and tested
recipe in [ChatML orchestration](../agent-server/chatml-orchestration.md).

For example, a workflow can dispatch a question to a researcher, remain alive
while its client disconnects, and collect the result when the job completes.
That research extension requires your prompt, provider configuration, job
capacity, and completion handling; the included timer does not perform research.

Read the [agent hosting overview](../agent-server/README.md) for the deployment model and operations path.

## Understand the lifetime

Use the documented job, schedule, and session views to inspect progress after
reconnecting. Schedules are one-shot events, not a cron service. Restart recovery
does not resume arbitrary process stacks or guarantee exactly-once external
effects. [Sessions and workspaces](../agent-server/sessions-and-workspaces.md)
explains the host's ownership and persistence boundaries.

[Explore another application](README.md) or [follow the tutorial curriculum](../tutorials/README.md).
