# Learn Ochat by building

This tutorial curriculum starts with a local conversation, adds a file tool,
and connects a specialist. Then choose a path into scripting or hosted agents.
Each lesson shows its outcome, prerequisites, and complete example files.

## Before your first lesson

[Build and configure Ochat](../agent-server/quickstart.md), then consult
[build troubleshooting](../guide/build-troubleshooting.md) if needed. Model-backed
lessons require a configured provider and access to their selected models.
The background timer lesson can demonstrate scheduling without a model request.

## Choose your path

The first three lessons form the authoring foundation. Then choose a capability
to add; lesson numbers are stable identifiers, not a required global sequence.

- **Tools and shell access:** [inspect a real project](../agent-server/tutorials/shell-agent.md),
  [run checks with separate capabilities and approved report writes](shell-guardrails.md),
  then [customize review decisions with ChatML](shell-customization.md).
- **ChatML workflows:** [summarize project reports](chatml-program.md),
  [turn the program into a reusable tool](chatml-tool.md), then learn how a
  [moderator controls a conversation](workflow.md),
  [retain a review ledger](stateful-workflow.md), and
  [deliver real background check results](background-results.md).
- **Subagents and agent teams:** [keep a specialist conversation](persistent-specialist.md),
  then [create a reviewer for the current task](generated-specialist.md).
- **Run and host:** save batch requests or keep sessions in a daemon.
- **Connect external clients:** integrate through stdio or authenticated HTTP.

Previous/next links stay within each path and return here at its end. You do not
need a daemon to complete the local authoring or synchronous scripting lessons.

[Explore real applications](../applications/README.md) when you want a goal to
work toward, or [browse source examples](../examples/README.md) to inspect files.
