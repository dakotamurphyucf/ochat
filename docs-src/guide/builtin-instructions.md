# Builtin instruction assets

Use `<doc src="builtin:ID"/>` inside a ChatMD message to include concise reviewed
instructions from the installed build. The catalog has these exact IDs:

| ID | Guidance |
| --- | --- |
| `chatml` | ChatML syntax starter with checked one-off examples |
| `goals` | Persistent objective, budgets, evidence and pending finish requests |
| `recurrence` | Controller versus occurrence, explicit schedules and observed outcomes |
| `skills` | Admitted roots, discovery versus selection, exact retained skill bytes |
| `project-instructions` | Applicable root-to-target AGENTS.md guidance and precedence |
| `context-recovery` | Local session recovery and explicit sharing policy |
| `packages` | Ordinary checked dependencies and explicit installed-source export |
| `lifecycle` | Component obligations, compatible decisions and explicit root finish |

These assets provide guidance. Importing one creates no goal, schedule, tool,
source grant, skill activation or runtime. In particular, `context-recovery`
supplies policy text only: it provides no context-management storage, tool,
automatic recovery or cross-session sharing functionality. Agents must use only
the capabilities actually supplied and authorized in the current session.

## Compose messages

The [complete ChatMD example](../examples/learning/builtin-capabilities/agent.chatmd)
composes three assets in ordinary message content:

```xml
<system><doc src="builtin:goals"/><doc src="builtin:recurrence"/></system>
<developer><doc src="builtin:lifecycle"/></developer>
<user>Explain the next bounded step for my current work and the evidence needed to
finish it. Use only controls actually supplied in this session.</user>
```

This is document composition, distinct from structural `<import>` expansion and
executable ChatML module dependencies. It demonstrates no native execution or
background scheduling. A host still needs to supply any relevant runtime and
controls. See [admitted instruction sources](admitted-instructions.md) for skill
and project discovery, [installed libraries](chatml-installed-libraries.md) for
ordinary source export, and [session lifecycle](chatml-session-lifecycle.md) for
host decisions.

## Exact capture and deliberate update

IDs are exact lowercase ASCII names. The `builtin:` namespace is reserved for
the embedded catalog: an authored file, URL, current directory or installed path
cannot override it. Lookup and completion use the same reviewed table. Unknown
IDs fail without filesystem or HTTP fallback. For example, `builtin:not-installed`
is unknown; `builtin:goals@2` is invalid identifier syntax. There is no document
version-selector grammar or alias resolution.

At explicit prompt preparation, the host captures the selected source identity,
exact byte length and SHA-256, together with the rendered output. A saved prompt
revision retains its captured edition, including inactive child definitions.
Resuming that revision uses retained output without consulting the current
catalog or converting the content again. Identical imported bytes have the same
content identity across working directories and packaged layouts; declaration
and parser identities still retain their own source provenance.

A fresh explicit preparation or configuration reload selects the currently
installed bytes and creates a new immutable revision. An installation update
does not rewrite an existing revision's instructions. Discovery refresh and
instruction activation are separate host operations; a document import never
activates a discovered skill or replaces an active selection.

For acquisition, custody limits and restoration policy, see
[captured document custody](../development/chatmd-content-custody.md).
