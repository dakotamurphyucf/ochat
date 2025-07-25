# key-dump – Inspect raw terminal events

`key-dump` is a small command-line tool that prints every input event decoded
by [Notty](https://github.com/pqwy/notty) in real time.  It is invaluable when
you need to figure out *exactly* which `Notty.Unescape.event` value your
terminal produces for a given key combination or mouse gesture so that you can
extend your application's pattern-matching accordingly.

---

## 1  Why you might need it

Terminal emulators, operating systems and keyboard layouts often disagree on
how to encode non-trivial key combinations.  For example, **Ctrl-Shift-Left**
may come through as a plain left-arrow in one terminal, or as
`` `Key (`ASCII 'b', [`Meta]) `` in another.  The only reliable way to support
all variants is to observe what your users’ terminals actually emit.  Running
`key-dump` directly in the problematic terminal gives you that information.

---

## 2  Usage

```console
$ key-dump        # or `dune exec bin/key_dump.exe` inside the repo
Key   ASCII 'A' (0x41)          mods=[Ctrl]
Key   ASCII 'E' (0x45)          mods=[Ctrl]
Mouse Press Left at (24,6)      mods=[]
Resize                          
```

Each line corresponds to a single value of the polymorphic variant
`[ Notty.Unescape.event | `Resize ]`:

* **Key**   keyboard input (`ASCII`, `Uchar` or one of the *special* keys)
* **Mouse** mouse press, drag, release or wheel scroll, complete with
  0-origin coordinates and active modifiers
* **Paste** bracketed-paste start/end delimiters
* **Resize** terminal window size changed

Quit the program with either the **q** key or **Ctrl-C**.

---

## 3  Internals (high level)

`key-dump` is a ~130-line OCaml program that relies on the following
libraries:

* **Eio** – structured-concurrency runtime used to set up the main *switch*
  and attach the stdin/stdout flows provided by the environment.
* **Notty_eio.Term** – Eio-aware wrapper around `Notty_unix.Term` that handles
  terminal initialisation, SIGWINCH, UTF-8 decoding and escape-sequence
  parsing in a background fiber.

The program starts an exclusive fullscreen Notty session with mouse reporting
and bracketed-paste *disabled* (not needed for keyboard debugging).  Every
decoded event is transformed into a fixed-width string via
`event_to_string` and printed using `Format.printf`.  The session ends when
the promise resolved by the quit keys is fulfilled.

---

## 4  API reference

`key-dump` is an **executable**, not a library, but the helper functions may
be useful elsewhere:

| Function | Description |
|----------|-------------|
| `string_of_special` | Human-readable representation of a `Notty.Unescape.special` key. |
| `string_of_key` | Pretty-print a `Notty.Unescape.key`. |
| `string_of_mods` | Convert modifier list (`[ \\`Ctrl | \\`Meta | \\`Shift ]`) to `"[Ctrl,Shift]"` etc. |
| `event_to_string` | Render `[ Notty.Unescape.event | \\`Resize ]` to a single line. |
| `main` | Entry point used by `Eio_main.run`. |

---

## 5  Limitations & gotchas

1. Mouse input and bracketed-paste are intentionally disabled.
2. Output is intended for humans, not for machine parsing.
3. Modifier detection varies between terminals; the output reflects what
   **Notty** reports, which in turn depends on the escape sequences received.

---

## 6  See also

* ODoc documentation of [`Notty.Unescape`](https://pqwy.github.io/notty/doc/Notty.Unescape.html)
* [`Notty_eio`](../lib/notty-eio/notty_eio.mli) source and docs – Eio bridge

