# Subagents and agent teams

A specialist gives a part of your workflow its own instructions and conversation.
A code reviewer can focus on correctness while a documentation reviewer checks
whether the same change is understandable. The parent gathers their findings
and decides what to do next.

Ochat supports both specialists you define ahead of time and agents created for
a particular task. Choose the conversation lifetime as deliberately as the role:
some questions need one answer, while an investigation benefits from follow-up.

## Choose a delegation pattern

| What you need | Pattern | Start here |
| --- | --- | --- |
| One focused answer | One-off authored agent tool | [Specialist tutorial](../tutorials/specialist.md) |
| Follow-up with the same specialist | Persistent authored agent tool | [Keep a specialist conversation](../tutorials/persistent-specialist.md) |
| Let the model decide whether to retain the conversation | Authored tool with optional persistence | [Choose the specialist's lifetime](../tutorials/persistent-specialist.md#choose-the-specialists-lifetime) |
| Define a specialist for the current task | Dynamically generated persistent child | [Create a task-specific specialist](../tutorials/generated-specialist.md) |
| Coordinate several continuing specialists | ChatML over the session lifecycle tools | [Background coordination and response watching](chatml-authoring-background.md) |

An agent tool is a declaration in the parent's ChatMD. A child session is a
particular conversation created from a definition. Reusing a declaration does
not imply reusing the same conversation: retain and supply the returned session
ID when you want to continue it.

## Authored specialists: define the role once

The [first specialist lesson](../tutorials/specialist.md) keeps the example
small: a parent reads project information and asks a companion ChatMD reviewer
for an answer in a separate conversation.

For a longer review, persistence retains the specialist conversation. An optional
persistence declaration lets the model choose a one-off call or request a
persistent instance. Its default remains one-off. A fixed persistent declaration
always uses the persistent behavior and does not need the optional mode choice.

The complete [specialist conversation lesson](../tutorials/persistent-specialist.md)
contains both lifetime declarations, the reviewer, sample evidence and private
daemon setup. Follow a review with a request to refine the proposed verification
guidance, keeping the same session and the relevant receipt. Its catalog records
the scope of available verification; interactive use calls your configured model.

## Generated specialists: adapt the role to the task

A parent can use `agent_create` to submit a captured ChatMD definition for a new
child. For example, a documentation coordinator can create a reviewer with
instructions specific to one failing tutorial and give it only the existing
tools needed to inspect the evidence.

The generated definition may choose supported model/reasoning settings,
instructions and moderation logic. It selects inherited tool bindings; it cannot
create broader shell access by declaring a new shell implementation. Read
[share tools and narrow authority](delegated-tools.md) before designing the role.

Because ChatMD and ChatML may be unfamiliar to a model, authoring tools can supply
installed guidance and non-executing validation. The
[authoring-context guide](authoring-context-tool.md) explains automatic, manual
and preloaded documentation policies. Validation checks a candidate; it does not
create the child or prove the future conversation will succeed.

## One lifecycle for persistent specialists

Persistent authored and generated children use the same lifecycle family when
those tools are available to the caller:

| Tool | Purpose |
| --- | --- |
| `agent_create` | Create a generated child from captured source and selected inherited bindings. |
| `agent_send` | Submit a message to an existing child and retain its submission reference. |
| `agent_status` | Inspect the child's current state. |
| `agent_read` | Read retained output with the supported output/cursor contract. |
| `agent_wait` | Wait for the selected lifecycle/submission condition. |
| `agent_stop` | End active child work through the supported stop behavior. |

Creation and readiness are distinct. A generated creation request defaults to
not starting immediately; request supported immediate start when the next step
is to send work. Sending a message does not implicitly start a stopped child.
Read the [exact requests and lifecycle rules](chatml-authoring-children.md)
before constructing calls.

Keep both session IDs and submission receipts. “This child is active” does not
answer “has it finished the review I just submitted?” Likewise, a continuation
cursor identifies output progress, not a new specialist.

## Coordinate a team without hiding the work

A ChatML coordinator can assign work, retain identities, collect findings and
handle unsuccessful reviewers. It can acknowledge a foreground request and
deliver useful results later rather than making the caller block throughout.
The current [response watcher](../../test/chatml_extensibility_fixtures/x06-response-watcher/README.md)
does this with timers and lifecycle polling. It is not a native child-response
push subscription.

Persistent children require an admitted durable host. Use the example's exact
host setup and keep private configuration/store data outside the data workspace.
Stopping a client, stopping a session and restarting the daemon have different
effects; see [session lifecycle](chatml-session-lifecycle.md).

For temporary delegation, the built-in `fork` tool is a different mechanism: its
child conversation can use tools under parent checks, but does not acquire the
persistent session-management API merely by returning a result.
