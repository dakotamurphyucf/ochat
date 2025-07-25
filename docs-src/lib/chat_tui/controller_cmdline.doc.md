# `Chat_tui.Controller_cmdline`

A **Vim-style command palette** for the Ochat terminal UI.

This helper module is activated whenever the editor switches to
`Model.Cmdline` mode – the mode in which the bottom line of the UI starts
with a `:` and waits for a command, very similar to the *ex* prompt in
Vim.  It is responsible for turning the raw keystrokes reported by
**Notty** into high-level reactions and for manipulating the mutable
state stored in `Chat_tui.Model`.

---

## 1 Overview

The module exports a single entry-point

```ocaml
val handle_key_cmdline :
  model:Model.t ->
  term:Notty_eio.Term.t ->
  Notty.Unescape.event ->
  Controller_types.reaction
```

which is called by the outer `Controller.handle_key` dispatcher while the
editor is in command-line mode.  Besides that, the file only contains a
few small helper functions (`insert_char`, `backspace`, `execute_command`)
that perform local edits and are **not** intended to be used elsewhere.

The command palette is intentionally small, mainly offering a proof-of-
concept UI for experimenting with different editor modes.  All commands
are matched *case-insensitively* and may be abbreviated to their first
letter.

| Command | Effect | Returned reaction |
|---------|--------|-------------------|
| `q`, `quit` | Quit the application immediately | `Quit` |
| `w` | *Write* – submit the current input buffer to the assistant | `Submit_input` |
| `wq` | Submit and then quit | `Quit` |
| `d`, `delete` | Delete the currently selected message | `Redraw` |
| `e`, `edit` | Copy selected message into the prompt and switch to *Insert* mode | `Redraw` |

Any other string is silently ignored (the prompt is cleared and the screen
is redrawn).

---

## 2 Function reference

### `insert_char`

```ocaml
val insert_char : Model.t -> char -> unit
```

Inserts a printable ASCII character at the current cursor position inside
the command-line buffer and moves the caret one position to the right.

### `backspace`

```ocaml
val backspace : Model.t -> unit
```

Deletes the character immediately to the *left* of the caret (if any) and
updates the cursor accordingly.

### `execute_command`

```ocaml
val execute_command : Model.t -> string -> Controller_types.reaction
```

Parses and executes a fully-typed command (the `:` prefix has already been
removed).  The function always resets the command-line state before
returning – even when the command is unknown.

### `handle_key_cmdline`

```ocaml
val handle_key_cmdline :
  model:Model.t ->
  term:Notty_eio.Term.t ->
  Notty.Unescape.event ->
  Controller_types.reaction
```

The public entry-point.  Delegates printable characters and cursor
movement to the helpers, handles *Enter*, *Esc* and *Backspace*
explicitly, and forwards anything else to the caller (`Unhandled`).

---

## 3 Usage example

Below is a simplified version of the integration code found in
`Chat_tui.Controller` that shows how the command-line controller is
invoked.  The snippet assumes that the surrounding code has already set
`model.mode` to `Cmdline` after the user pressed `:` in normal mode.

```ocaml
let rec event_loop term model =
  match Notty_eio.Term.event term with
  | None -> ()
  | Some ev ->
      let reaction =
        Chat_tui.Controller_cmdline.handle_key_cmdline
          ~model ~term ev
      in
      (match reaction with
       | Redraw        -> Renderer.render term model; event_loop term model
       | Submit_input  -> send_to_openai model
       | Quit          -> exit 0
       | Cancel_or_quit | Unhandled -> (* fall back to other handlers *)
      )
```

---

## 4 Limitations & Future work

* UTF-8 handling is *byte-based* for now.  Multibyte characters will break
  the cursor logic.
* The palette only recognises a handful of commands.  Extending the list
  is straightforward but requires a design decision on how to expose more
  functionality without turning this module into a monolith.
* Error feedback is limited – unknown commands simply disappear.


