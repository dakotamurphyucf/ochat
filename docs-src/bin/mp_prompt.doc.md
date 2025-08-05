# mp_prompt – Recursive Meta-Prompting on the command-line

`mp_prompt` is a **thin wrapper** around
[`Chat_response.Driver.run_completion_stream`](../../lib/chat_response/driver.mli)
that exposes the Recursive Meta-Prompting pipeline as a standalone CLI tool.

It is the fastest way to run a *one-off* refinement job or to integrate RMP
into shell scripts and CI pipelines without writing additional OCaml code.

---

## 1 Synopsis

```console
$ mp-prompt [-prompt-file FILE]
            [-output-file FILE]
            [-meta-refine]
            [-parallel-tool-calls BOOL]
```

---

## 2 Flags

| Flag | Default | Description |
|------|---------|-------------|
| `-prompt-file FILE` | *(none)* | Optional ChatMarkdown template inserted *once* at the beginning of the conversation.  If omitted the session is resumed entirely from `-output-file`. |
| `-output-file FILE` | `./prompts/default.mp.chatmd` | Destination buffer that stores the *complete* conversation including system, user and assistant messages.  Created automatically when missing. |
| `-meta-refine` | *(false)* | When present, sets the `OCHAT_META_REFINE` environment variable thereby enabling **Recursive Meta-Prompting** for the next user message. |
| `-parallel-tool-calls BOOL` | `true` | Permit the assistant to invoke **multiple** function calls concurrently.  Disable if determinism or strict ordering is required. |

### Example – design brainstorm

```console
$ mp-prompt \
    -prompt-file prompts/brainstorm.chatmd \
    -output-file sessions/brainstorm.chatmd \
    -meta-refine

🚀  refining prompt …                         (iteration 0 ➜ 1)
✅  refinement accepted (score Δ +0.23)
💬  generating assistant response …           (OpenAI gpt-4o)

assistant> Here are five design directions …
```

The command streams live deltas to stdout so that you see the refined prompt
*and* the assistant’s answer as soon as they are available.

---

## 3 Exit codes

| Code | Meaning |
|------|---------|
| `0`  | Normal termination. |
| `≠0` | Unhandled exception – inspect the console for the stack-trace. |

---

## 4 Relationship to other front-ends

• **Chat-TUI** – interactive terminal UI built on the same backend.  Toggle
  RMP with `/meta_refine`.
• **MCP `meta_refine` tool** – JSON-RPC endpoint suitable for remote agents.

All three front-ends call `Recursive_mp.refine` under the hood and therefore
share the exact same behaviour.

---

*Document last updated: 2025-07-28*

