# Controller_normal – Normal-mode key handling

This document complements the inline odoc comments of
`controller_normal.ml` and explains the public behaviour of the module at a
slightly higher level: what it does, which functions matter to the outside
world, and how to integrate it into the Chat-TUI event loop.

## 1  Purpose and scope

`Controller_normal` implements the subset of Vim-like key bindings that are
active while the input area is in **Normal** editor mode.  It is the
counterpart to `controller_cmdline` (command-line prompt) and the Insert-mode
handler embedded in `controller.ml`.

* The module is **pure** with regard to side-effects: it only mutates the
  in-memory `Chat_tui.Model.t`.  Network requests or disk IO are handled by
  higher-level parts of the application.
* All byte indices refer to the UTF-8 encoded `Model.input_line`.  The code
  therefore treats the string as an opaque byte array – full Unicode-grapheme
  support will be added later.


## 2  Public API

```ocaml
val handle_key_normal :
  model:Model.t ->
  term:Notty_eio.Term.t ->
  Notty.Unescape.event ->
  Controller_types.reaction
```

Dispatches **one** terminal event and returns a reaction that tells the caller
what to do next:

* `Redraw` – the visible state changed; rerender the viewport.
* `Submit_input` – user pressed *Meta+Enter* while in Normal mode.  The main
  loop should send the current prompt to the assistant.
* `Cancel_or_quit`, `Quit`, `Unhandled` – see `controller_types.ml`.

All other values and helpers inside the module are *implementation details*
that are **not** meant to be used from the outside.


## 3  Supported key bindings

The table lists the recognised commands.  Motions follow Vim semantics unless
stated otherwise.

| Key(s)            | Action                                   |
|-------------------|------------------------------------------|
| `h` / `l`         | Move cursor one byte left / right        |
| `k` / `j`         | Move cursor one visual line up / down    |
| `w` / `b`         | Next / previous word                     |
| `0` / `$`         | Start / end of current line (no newline) |
| `gg` / `G`        | Scroll to top / bottom of history        |
| `a`               | Append – switch to **Insert** mode       |
| `o` / `O`         | Insert new line below / above current    |
| `x`               | Delete character under cursor            |
| `dd`              | Delete current line                      |
| `u` / *Ctrl-r*    | Undo / redo                              |
| `r`               | Toggle *Raw-XML* draft mode              |
| `[` / `]`         | Previous / next message in history       |
| `:`               | Enter command-line mode                  |

Most bindings aim to be intuitive for regular Vim users while remaining easy
to learn for newcomers – no exotic motions, registers or text objects have
been added so far.


## 4  Examples

### 4.1  Integrating into the main loop

```ocaml
let rec read_events term model =
  Notty_eio.Term.events term |> Eio.Stream.iter (fun ev ->
    match Controller_normal.handle_key_normal ~model ~term ev with
    | Redraw -> Renderer.draw ~model ~term
    | Submit_input ->
        (* send Model.input_line to OpenAI and clear the prompt *)
    | Cancel_or_quit -> (* handle Cancel / ESC *)
    | Quit -> raise Exit
    | Unhandled -> ()
  )
```


### 4.2  Programmatic cursor motion

The lower-level helpers can be handy when tests need to replicate complex
motions without synthesising full terminal events.

```ocaml
(* Move the caret up by two visual lines *)
Controller_normal.move_cursor_vertically model ~dir:(-1);
Controller_normal.move_cursor_vertically model ~dir:(-1);

assert (Model.cursor_pos model = expected_pos);
```


## 5  Known limitations

* **UTF-8 awareness** – The cursor operates on byte offsets, therefore moving
  left or right may land inside a multi-byte sequence for non-ASCII text.
  Terminals typically only send ASCII, so the limitation is acceptable for
  now but will be addressed in a later milestone.
* **Partial Vim coverage** – Only a small subset of normal-mode commands is
  supported.  The implementation is intentionally minimalist; additional
  motions can be added as user demand arises.


---


