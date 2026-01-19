# ochat chat-completion – script-friendly cousin of chat_tui

`ochat chat-completion` runs a ChatMarkdown prompt non-interactively from the
command line. It is the script- and CI-friendly counterpart to the
interactive `chat_tui` UI.

---

## 1 30-second smoke-test

Run a single command that verifies **ChatMD parsing → tool-calling → OpenAI
round-trip** before you start wiring Ochat into your own workflows:

```console
$ ochat chat-completion \
    -prompt-file prompts/hello.chatmd \
    -output-file .chatmd/smoke.chatmd
```

Open `.chatmd/smoke.chatmd` and you should see something along the lines of:

```xml
<tool_call id="1" name="echo">{"text":"Hello ChatMD"}</tool_call>
<tool_response id="1">{"reply":"Hello ChatMD"}</tool_response>
```

If you do **not** get a reply, check that `OPENAI_API_KEY` is set and
reachable from the shell session.

---

## 2 Basic usage

```console
$ ochat chat-completion [flags]
```

The command reads a ChatMD prompt and appends messages, tool calls and
assistant responses to the output file. The entire conversation stays in a
single `.chatmd` document.

Typical invocation:

```console
$ ochat chat-completion \
    -prompt-file prompts/hello.chatmd \
    -output-file .chatmd/session.chatmd
```

Re-run the command with the same `-output-file` to extend the chat history.

---

## 3 Frequently-used flags

| Flag | Purpose | Default |
|------|---------|---------|
| `-prompt-file` | File prepended **once** at the *start* of the transcript (usually a template with `<system>` / `<developer>` rules). | *(none)* |
| `-output-file` | Chat log that *persists* across invocations (created if absent, **appended** otherwise). Use `$(mktemp)` or `/dev/stdout` when you want an *ephemeral* transcript. | `./prompts/default.md` |

---

## 4 Conversation state lives in a file

The file supplied to `-output-file` is the *single* source of truth for the
conversation: tool-calls, reasoning deltas, assistant messages – everything is
captured in ChatMarkdown.

Re-run the command with the *same* output file to extend the history:

```console
# Turn 1
$ ochat chat-completion -prompt-file prompts/hello.chatmd \
    -output-file .chatmd/tech_support.chatmd

# Turn 2 (assistant sees full history)
$ echo '<user>My computer is on fire!</user>' >> .chatmd/tech_support.chatmd
$ ochat chat-completion -output-file .chatmd/tech_support.chatmd
```

Open the result at any time in the interactive UI:

```console
$ dune exec chat_tui -- -file .chatmd/tech_support.chatmd
```

`chat_tui` lets you keep chatting as if the session had always been
interactive.

---

## 5 Ephemeral runs

Nothing prevents you from pointing `-output-file` to a temporary file or
standard output when you only care about the final transcript.

```console
# Linux / macOS – leave zero artefacts after the run
$ tmp=$(mktemp /tmp/ochat.XXXX) \
  && ochat chat-completion -prompt-file prompts/hello.chatmd \
       -output-file "$tmp" \
  && cat "$tmp" \
  && rm "$tmp"

# Portable one-liner (store under /dev/shm when available)
$ ochat chat-completion -prompt-file ask_weather.chatmd \
       -output-file /dev/stdout
```

The first variant leaves **zero** artefacts after the run; the second streams
the final ChatMD document directly to the console while still giving the
runtime a valid *file descriptor* to append to — a requirement of the current
implementation.

---

## 6 Exit codes

| Code | Meaning |
|------|---------|
| `0` | Assistant replied successfully. |
| `1` | Prompt malformed or missing OpenAI key. |
| `2` | At least one tool call failed. |
| `≥3` | Unexpected OCaml exception. |

Run `ochat --help` for an exhaustive list of sub-commands and defaults.

