# `Controller_shared` – helpers shared by Insert- and Normal-mode controllers

`Controller_shared` is a microscopic utility module that exists purely to
break the dependency cycle between the two editing-state controllers of the
TUI:

* `Controller_normal` – normal / navigation mode
* `Controller_insert` – text-insertion mode

Currently it exposes **one** function, `line_bounds`, but any future helper
that must be available to both controllers (and that does **not** belong in
`Model`, `Util`, or another higher-level module) should live here.

---

## API

### `line_bounds : string -> int -> int * int`

`line_bounds s pos` returns the *byte* interval that delimits the line that
contains cursor position `pos` inside text buffer `s`.

Return value: `(start_idx, end_idx)`

* `start_idx` – index of the first byte of the line.
* `end_idx`   – index **just after** the terminating `\n`, or
  `String.length s` if the line is the last one in the buffer.

The bounds are therefore half-open: `[start_idx, end_idx)`.

**Pre-conditions**

* `0 <= pos <= String.length s` — callers must guard against invalid
  positions.

**Performance**

* Tail-recursive search to the left and right of `pos`.
* No intermediate strings are allocated (operates directly on the input
  buffer).

**Unicode caveat**

The routine works on raw bytes and is therefore *not* UTF-8-aware.  This is
identical to `Core.String` behaviour and is sufficient for locating
newlines, but callers must remember that the indices are byte offsets, not
character counts.

---

## Usage example

Highlight the line under the cursor in the renderer:

```ocaml
let highlight_current_line buffer cursor_pos =
  let start_idx, end_idx = Controller_shared.line_bounds buffer cursor_pos in
  Renderer.highlight buffer ~from:start_idx ~until:end_idx
```

---

## Future work / limitations

* Provide UTF-8-aware variant (e.g. based on `Uutf`) for character index
  computations.
* Expose additional helpers (e.g. word-bound searches) as needed by both
  controllers.

