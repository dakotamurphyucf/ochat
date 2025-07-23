Path_completion – Interactive Path Autocompletion
=================================================

This document complements the inline odoc comments with a longer, narrative
description, usage patterns and design notes for the `Path_completion` module.

Overview
--------

`Path_completion` turns the text the user has typed so far in the TUI’s *command
mode* into a list of candidate file-system entries.  The algorithm is purposely
simple but already delivers sub-millisecond latencies for typical directory
sizes (≤ 10 000 entries) while keeping the implementation < 200 LOC.

High-level algorithm
--------------------

1. **Cache** the names of each directory encountered in a global
   `(string, entry) Hashtbl` (LRU-capped).
2. **Sort** the array once at refresh time.
3. For each keystroke:
   a. Perform a *lower-bound* binary search to find the first name that is
      `≥ prefix`.
   b. Perform the same search for `prefix ^ "\255"` (a sentinel greater than
      any legal path char).
   c. The slice between the two indices contains every matching entry – take
      up to 25 of them.

The directory cache is refreshed when it is older than `ttl = 2.0` s or has
been evicted due to the LRU size limit (`max_size = 1024`).

Public API
----------

### `type t`

Opaque record used to remember the *last* list of suggestions and the current
cursor position when the user presses <kbd>Tab</kbd> repeatedly.  Each input
field that wants independent cycling should allocate its own value with
[`create`](#VALcreate).

### `create : unit -> t`

Allocate a fresh autocompletion context.  O(1).

```ocaml
let completer = Path_completion.create ()
```

### `suggestions`

```ocaml
val suggestions :
  t ->
  fs:'a Eio.Path.t ->
  cwd:string ->
  prefix:string ->
  string list
```

Returns the (lexicographically) sorted list of entries whose names start with
`prefix`.

Parameters:

* `t`       : autocompletion context returned by `create()`.
* `fs`      : file-system capability obtained from e.g. `Eio.Stdenv.fs`.  All
              I/O stays within that capability.
* `cwd`     : absolute path that should be treated as the *current directory*.
* `prefix`  : user-typed text; may include a directory component (`foo/bar`).

Returns **at most 25** bare entry names (neither the directory component nor a
trailing `/`).  Directory entries `.` and `..` are filtered out.

### `next`

```ocaml
val next : t -> dir:[ `Fwd | `Back ] -> string option
```

When the user keeps pressing <kbd>Tab</kbd> (or <kbd>Shift</kbd>+<kbd>Tab</kbd>)
we call `next` to cycle through the last result as follows:

* `dir = \\`Fwd`  → move forward, wrapping at the end.
* `dir = \\`Back` → move backward, wrapping at the start.

Returns `None` when there is no cached suggestion list.

### `reset : t -> unit`

Clears the browsing state so that the next call to `next` will return `None`.
Does **not** clear the directory cache.

Usage example
-------------

```ocaml
open Eio.Std

let ( / ) = Eio.Path.( / )

let demo env =
  let fs  = Eio.Stdenv.fs  env in
  let cwd = Eio.Path.native (Eio.Stdenv.cwd env) in
  let pc  = Path_completion.create () in

  (* User typed "src/pa" – ask for completions                 *)
  let sugg = Path_completion.suggestions pc ~fs ~cwd ~prefix:"src/pa" in
  List.iter print_endline sugg;

  (* Pretend the user presses Tab repeatedly                     *)
  Path_completion.next pc ~dir:`Fwd |> Option.iter print_endline;
  Path_completion.next pc ~dir:`Fwd |> Option.iter print_endline;
  Path_completion.next pc ~dir:`Back |> Option.iter print_endline

let () = Eio_main.run demo
```

Known limitations
-----------------

* **Blocking I/O** – directory listing is performed synchronously.  This is
  acceptable for small directories but will eventually be replaced by a fully
  asynchronous version that performs the scan in a background fiber.
* **Fixed result cap** – callers cannot request more than 25 suggestions.
  This keeps the TUI responsive but might be made configurable later.
* **No case-folding** – completion is case-sensitive even on case-insensitive
  file-systems.

Internal details
----------------

Refer to `path_completion.md` for an exhaustive analysis of the algorithm and
its complexity characteristics.

