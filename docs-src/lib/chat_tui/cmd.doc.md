# `Chat_tui.Cmd`

Interpreter for impure commands emitted by the pure controller layer.

The terminal UI follows an Elm-style architecture:

1. **Model –** an immutable record representing the full application state
   (`Chat_tui.Model.t`).
2. **Update –** pure functions that transform the model and return
   `{ new_model; cmds }` where `cmds : Chat_tui.Types.cmd list` describes
   what needs to happen in the outside world (file IO, HTTP requests, …).
3. **View –** a renderer that converts the model into Notty images.

`Chat_tui.Cmd` is the only module that is allowed to perform side-effects.
It interprets the opaque closures stored in the open variant type
`Chat_tui.Types.cmd` and executes them, typically by spawning background
fibres so that the UI remains responsive.

---

## API

### `run : cmd -> unit`

Execute a single command.  The current constructors are:

* `Persist_session f` – write the current conversation transcript to disk
  by running `f` in a background fibre.
* `Start_streaming f` – begin an OpenAI streaming request handled by `f`.
* `Cancel_streaming f` – abort the in-flight request via `f`.

`run` is deliberately generic – it does not inspect the internals of the
thunk and therefore imposes no constraints on the calling layer.

#### Example

```ocaml
let persist path model =
  Chat_tui.Cmd.run (Persist_session (fun () -> Chat_tui.Persistence.save path model))
```

### `run_all : cmd list -> unit`

Sequentially executes a list of commands with `List.iter`.  This is a small
convenience wrapper used by the controller functions that already work with
command lists.

```ocaml
(* inside a controller *)
let { updates; cmds } = handle_keypress model key_event in
let new_model = Chat_tui.Model.apply updates model in
Chat_tui.Cmd.run_all cmds;
```

---

## Design Rationale

* Only this module knows {i how} to perform impure work; the rest of the
  codebase only knows {i that} work is necessary.
* Commands carry ready-to-run thunks.  This keeps the interpreter trivial
  and avoids a proliferation of smart constructors and global dependency
  injection.
* The variant is {e open}, enabling feature-driven extensions from any
  compilation unit without having to touch a central enum.

---

## Limitations / Future Work

* The command set is minimal.  Future steps will add network requests,
  cancellation time-outs, clipboard interaction, etc.
* Error handling is delegated to the supplied thunks.  A more advanced
  interpreter could wrap each call in a supervisor that logs exceptions and
  displays error notifications in the UI.


