# chat_tui – Terminal Ochat client

`chat_tui` is a convenience wrapper around the high-level
`Chat_tui.App` module.  It turns the library into an end-user binary
that you can launch from a shell.

---

## 1 Synopsis

```console
$ chat-tui [-file FILE]
```

## 2 Command-line flags

| Flag | Default | Description |
|------|---------|-------------|
| `-file FILE` | `./prompts/interactive.md` | Markdown (or *.chatmd*) file used to initialise the conversation history, declare tools and persist settings. |

If the file does not exist it is created on exit so you can resume the
session later.

## 3 Behaviour

1. `chat_tui` creates (if needed) the hidden directory `.chatmd` in the
   current working directory.  The directory holds cache files and
   transient artefacts produced by tools.
2. It then calls `Io.run_main` which in turn delegates to
   `Eio_main.run` to bootstrap an event loop.
3. Finally it invokes `Chat_tui.App.run_chat` with the chosen prompt
   file.  Control is handed over to the TUI engine; the wrapper will
   not return until the user quits (`/quit` or *Ctrl-c*).

## 4 Programmatic embedding

Applications can reuse the TUI component directly instead of spawning
an external process:

```ocaml
let () =
  Io.run_main @@ fun env ->
  Chat_tui.App.run_chat ~env ~prompt_file:"./prompts/demo.chatmd" ()
```

This is exactly what the `chat_tui` binary does under the hood.

## 5 Exit codes

| Code | Meaning |
|------|---------|
| 0 | Normal termination (user quit). |
| ≠0 | Unhandled exception – inspect the console for the stack-trace. |

## 6 Limitations & notes

* **Single window** – each execution manages one conversation.  Run
  multiple instances for parallel chats.
* **Unix-only** – relies on `Eio_main` and `Notty`, therefore does not
  currently work in JavaScript or MirageOS environments.
* **No live reload** – editing `FILE` while the TUI is running has no
  effect; restart to load the changes.


