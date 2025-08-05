# `Chat_tui.Ui_helpers` – post-loop terminal prompts

`Chat_tui.Ui_helpers` bundles **synchronous** helpers that are executed
*after* the Notty-based TUI has already been torn down.  The code must
therefore rely on plain stdin/stdout primitives instead of the
Notty/Eio event-loop.  At the moment the module exposes a single
utility: [`prompt_archive`](#prompt_archive).

---

## Table of contents

1. [Asking to archive the conversation – `prompt_archive`](#prompt_archive)
2. [Known limitations](#limitations)

---

### 1  `prompt_archive` <a id="prompt_archive"></a>

```ocaml
val prompt_archive : ?timeout_s:float -> ?default:bool -> unit -> bool
```

Displays the line

```text
Archive conversation to ChatMarkdown file? [y/N] ⏎
```

on **stdout**, flushes the channel and waits for a single line of input
on **stdin**.

Parsing rules (case-insensitive):

| Input | Result |
|-------|--------|
| `y`, `yes` | `true`  |
| `n`, `no`  | `false` |
| anything else / empty line | *fallback* |

The *fallback* value is controlled by the optional
`?default` parameter (defaults to `false`).

Timeout behaviour:

* If `timeout_s` is **positive** the function blocks for *at most* that
  number of seconds (≈ 10 s by default).  
* If `timeout_s` is **zero or negative** it returns immediately with
  the default value.

Internally the helper uses `Core_unix.select` which makes the
implementation **Unix-only**.  The prompt is flushed explicitly so that
it becomes visible even when the Notty terminal has just been released
back to the shell.

#### Usage example

```ocaml
let () =
  if Chat_tui.Ui_helpers.prompt_archive () then
    Format.printf "→ User opted-in, exporting …@."
  else
    Format.printf "→ Keeping transcript in memory only.@."
```

---

### 2  Known limitations <a id="limitations"></a>

1. **Unix dependency.**  Windows is not supported because the helper
   relies on `select(2)` semantics.
2. **Single character granularity.**  Users must press ⏎ which makes
   the interaction slightly clunkier than reading a single key stroke.

---

*Last updated*: 2025-08-05

