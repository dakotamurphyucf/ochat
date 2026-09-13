# Learn Ochat by building

This tutorial curriculum starts with a local conversation, adds a file tool,
and connects a specialist. Then choose shell capabilities, agent teams or ChatML
workflows, with hosting and external clients available when your project needs them.
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
- **Run and host:** [save a batch request](../cli/chat-completion.md) or
  [keep sessions in a private daemon](../agent-server/tutorials/unix-daemon.md).
- **Connect external clients:** integrate through
  [stdio](../agent-server/tutorials/stdio-client.md) or
  [authenticated HTTP](../agent-server/tutorials/http-client.md).

Previous/next links stay within each path and return here at its end. You do not
need a daemon to complete the local authoring or synchronous scripting lessons.

## Follow Lantern from a brief to working checks

The examples share a small documentation project called **Lantern**. The file
reader and first specialist start with its short project brief. The shell path
introduces real setup and reference pages, a deterministic checker, and an
intentionally missing verification step. Later specialists review that evidence;
stateful and background workflows retain findings and run checks.

The sample checker uses a Unix shell and `awk`, with no network or model request.
Those programs are sample-project prerequisites, not a new Ochat runtime. Its
rules test named documentation conventions, not whether arbitrary prose is
correct. A model reviewer can assess the usefulness of the instructions after
the checker has supplied concrete evidence.

Every lesson has an independently complete source bundle. Extract a fresh bundle
and follow that lesson's launch instructions: do not accumulate edits across
earlier temporary directories. Some checkpoints read recorded report data;
others actually run the checker. Each lesson distinguishes the two and shows
the expected output. Persistent-specialist lessons include the private durable
host setup they require; the local workflow lessons explain their process lifetime.

Open the inline source reader to inspect the root agent, scripts, schemas,
runtime declarations and sample files before running an example. You can view
all of them in the browser without downloading anything.

[Explore real applications](../applications/README.md) when you want a goal to
work toward, or [browse source examples](../examples/README.md) to inspect files.
