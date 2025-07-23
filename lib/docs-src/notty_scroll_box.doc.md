# `Notty_scroll_box` – scrollable viewport for Notty images

`Notty_scroll_box` is a lightweight, state-ful helper that turns any
[`Notty.I.t`](https://pqwy.github.io/notty/doc/notty/Notty/I/index.html) image into a
vertically scrollable *viewport*.  The module is ideal when you already have a
piece of content rendered as a Notty image (a long log, a formatted Markdown
document, search results, …) and want to expose only a window of limited
height in a TUI while still allowing the user to scroll.

The implementation is fewer than 40 LOC and does **not** attempt to build a
fully-fledged widget library.  It simply remembers how many rows have been
scrolled off the top and uses `Notty.I.vcrop`, `Notty.I.vsnap`, and
`Notty.I.hsnap` to produce a view of the requested dimensions.

---

## API Overview

```ocaml
type t

val create           : ?scroll:int -> Notty.I.t -> t
val set_content      : t -> Notty.I.t -> unit
val content          : t -> Notty.I.t
val scroll           : t -> int

val max_scroll       : t -> height:int -> int
val clamp_scroll     : t -> height:int -> unit

val scroll_to        : t -> int -> unit
val scroll_by        : t -> height:int -> int -> unit
val scroll_to_top    : t -> unit
val scroll_to_bottom : t -> height:int -> unit

val render           : t -> width:int -> height:int -> Notty.I.t
```

### State accessors

* **`content`** – full underlying image.
* **`scroll`**   – number of rows hidden *above* the viewport (≥ 0).

### Mutators

* **`set_content`** – replace the image while keeping the current offset.
* **`scroll_to`** – blindly set the offset (clamped later by `render`).
* **`scroll_by`** – move relative and immediately clamp.
* **`scroll_to_top` / `scroll_to_bottom`** – jump to boundary positions.

### Helper functions

`max_scroll` computes the largest offset that still leaves at least one row
visible for a given viewport height; `clamp_scroll` forces the current offset
into the valid range.  Calling `render` also performs this clamping step –
manual use of `clamp_scroll` is therefore optional.

### Rendering

`render t ~width ~height` returns a `width × height` image constructed as

```ocaml
t.content
|> I.vcrop t.scroll 0        (* drop rows above the viewport           *)
|> I.vsnap ~align:`Top height (* pad/crop vertically to exact height   *)
|> I.hsnap ~align:`Left width (* pad/crop horizontally to exact width  *)
```

Alignment is fixed (`Top`/`Left`) to keep the top-left corner of the content
stable while scrolling.  Horizontal scrolling is intentionally out of scope –
when the content is wider than the viewport the right-hand side will simply be
cropped.

---

## Usage examples

### Displaying a long list with keyboard scrolling

```ocaml
open Core
open Notty
open Notty_unix

let make_content rows =
  let line n = I.string A.empty (Printf.sprintf "Row %04d" n) in
  I.vcat (List.init rows ~f:line)

let () =
  let term  = Notty_unix.Term.create () in
  let rows  = 1_000 in
  let box   = Notty_scroll_box.create (make_content rows) in

  let rec loop () =
    let (`Resize (w, h) | `Key ("j", _) | `Key ("k", _) | `Key ("q", _)) as ev =
      Notty_unix.Term.event term
    in
    (match ev with
     | `Key ("q", _) -> ()
     | `Key ("j", _) -> Notty_scroll_box.scroll_by box ~height:h 1
     | `Key ("k", _) -> Notty_scroll_box.scroll_by box ~height:h (-1)
     | `Resize _     -> ()
     | _             -> ());
    Notty_unix.Term.image term (Notty_scroll_box.render box ~width:w ~height:h);
    loop ()
  in
  loop ()
```

Run the snippet in a terminal and use <kbd>j</kbd>/<kbd>k</kbd> to scroll, or
<kbd>q</kbd> to quit.  The core scrolling logic is contained in just two calls
to `scroll_by` and `render`.

### Programmatic scrolling

```ocaml
let view_first_page   box ~width ~height =
  Notty_scroll_box.scroll_to_top box;
  Notty_scroll_box.render box ~width ~height

let view_last_page box ~width ~height =
  Notty_scroll_box.scroll_to_bottom box ~height;
  Notty_scroll_box.render box ~width ~height

let page_down box ~width ~height =
  Notty_scroll_box.scroll_by box ~height height;   (* += full screen *)
  Notty_scroll_box.render box ~width ~height
```

---

## Limitations & gotchas

1. **Vertical-only** – horizontal scrolling is out of scope.
2. **No buffering** – the underlying image is kept in memory as a single
   `Notty.I.t`; updating large images might be expensive.
3. **No built-in event handling** – the module only manipulates the state;
   mapping keyboard/mouse events to calls like `scroll_by` is up to the caller.

---

## Relationship to Notty

The module uses only the pure image-manipulation API (`Notty.I`).  It does
**not** depend on any of the IO back-ends (`Notty_unix`, `Notty_lwt`, …) and
is therefore safe to use in any environment supported by Notty.

---


