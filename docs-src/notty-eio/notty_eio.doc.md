# Notty-Eio – developer guide

`Notty_eio` glues the declarative TUI engine of
[**Notty**](https://github.com/pqwy/notty) to the structured-concurrency
runtime of **Eio**.  It provides a single entry-point
`Notty_eio.Term.run` that sets up an exclusive, full-screen terminal
session, spawns a background fiber for asynchronous input handling and
takes care of cleaning the TTY up afterwards.

The present document complements the inline `odoc` comments with a
deeper discussion, design rationales, and a few larger examples that do
not belong in the API reference.

## Table of contents

1.  Quick start
2.  Public API recap
3.  Event loop & threading model
4.  Implementation notes
5.  Limitations / future work

---

## 1  Quick start

```ocaml
open Eio_main

let () =
  Eio_main.run @@ fun env ->
  let stdin  = Eio.Stdenv.stdin  env in
  let stdout = Eio.Stdenv.stdout env in

  Notty_eio.Term.run
    ~input:stdin ~output:stdout
    ~on_event:(function
      | `Resize -> traceln "Window resized"
      | `Key (`ASCII 'q', _) -> raise Exit
      | _ -> ())
    (fun term ->
       let txt = Notty.I.string Notty.A.empty "Press q to quit" in
       Notty_eio.Term.image term txt;
       Eio.Fiber.await_cancel ())
```

Compile with

```sh
dune exec ./main.exe
```

Resize the window a few times and hit **q** to exit.  The terminal will
return to its original state automatically.

---

## 2  Public API recap

| Function | Purpose |
| -------- | ------- |
| `Term.run` | Enter full-screen mode and run user code.  Ensures cleanup. |
| `Term.image` | Render an image (frame). |
| `Term.refresh` | Force redraw of the last frame. |
| `Term.cursor` | Show/hide or move the cursor. |
| `Term.size` | Current `(cols, rows)` dimension. |
| `Term.release` | Early teardown (normally not needed). |

All operations are *imperative* and thus return [`unit`] except for
`run`, which yields whatever the user callback returns.

---

## 3  Event loop & threading model

`run` creates its own `Fiber.fork_daemon` that

1. decodes bytes from `input` with `Notty.Unescape`,
2. normalises window-size change notifications (`SIGWINCH`),
3. invokes the *non-blocking* `on_event` callback.

The daemon runs until the surrounding `Switch` is released.  Users
should therefore avoid performing expensive work directly in
`on_event`; spawn a new fiber (under the same switch) instead.

The `Term.t` handle is *affine*: while technically shareable between
fibers, its internal `Buffer.t` and `Tmachine.t` are not thread-safe.
Stick to the creating fiber unless you wrap calls with a mutex.

---

## 4  Implementation notes

* **Zero-copy** – The original `Notty_unix` performs a `Unix.write`
  straight from an internal buffer.  Because Eio’s `Flow.copy_string`
  still copies to a temporary, we currently keep an intermediate
  `Buffer.t`.  This may change once Eio exposes a more flexible API.

* **Signal handling** – `nosig=true` disables termios signals (e.g. Ctrl-Z)
  so that Notty can receive the full key range.  Set it to `false` if
  your application relies on traditional TTY job-control.

* **Resource safety** – All low-level handles (`Fd.t`, signal handlers,
  termios attributes) are attached to the local switch via
  `Switch.on_release_cancellable`.  Even an uncaught exception leaves
  the terminal in a sane state.

---

## 5  Limitations / future work

1. **Performance** – Rendering still incurs one extra string copy.
2. **Concurrency** – The rendering functions are not thread-safe.
3. **Windows / non-Unix backends** – Hard-coded to `Eio_unix`; porting
   to other backends would require conditional compilation.
4. **Alternate buffers** – Unlike some TUIs, the wrapper does not switch
   to the terminal’s *alternate screen* automatically.

Patches welcome!

