# `Merlin` – OCaml code-intelligence via the *ocamlmerlin* CLI

`Merlin` is the thinnest possible wrapper around the
[`ocamlmerlin`](https://github.com/ocaml/merlin) binary.  It hides the
JSON-over-stdin/out protocol behind a handful of high-level functions
suited for *interactive* use-cases: editors, REPLs, Ochat plugins,
console helpers, etc.

The module does **not** aim to expose all of Merlin’s capabilities –
only those needed by the surrounding code-base.  Feel free to fork or
extend it.

---

## Table of contents

1. [Quick start](#quick-start)
2. [API overview](#api-overview)
3. [Identifier occurrences](#identifier-occurrences)
4. [Code completion](#code-completion)
5. [Known limitations](#known-limitations)

---

## Quick start

```ocaml
open Merlin

Eio_main.run @@ fun env ->
  let merlin = Merlin.create () in          (* 1. prepare a session   *)
  Merlin.add_context merlin "open Core";   (* 2. keep context up-to-date *)

  (* 3. ask for completions *)
  let code   = "let _ = Strin" in          (* cursor after the 'n' *)
  let pos    = String.length code in
  let { cmpl_candidates ; _ } =
    Merlin.complete env ~pos merlin code
  in
  List.take cmpl_candidates 3
  |> List.iter (fun c ->
       Printf.printf "%-20s : %s\n" c.cmpl_name c.cmpl_type);

  (* 4. highlight every occurrence of an identifier *)
  let code   = "let foo x = x + foo 1" in
  let occs   = Merlin.occurrences env ~pos:4 merlin code in
  List.iter occs (fun {id_start; id_end} ->
    Printf.printf "from line %d col %d to line %d col %d\n"
      id_start.id_line id_start.id_col id_end.id_line id_end.id_col);
```

---

## API overview

### `create`

```ocaml
val create :
  ?server:bool -> ?bin_path:string -> ?dot_merlin:string -> unit -> Merlin.t
```

Prepare a new session.  The function merely stores configuration – it
does **not** launch the external process yet.  When [`server = true`]
(default) a persistent background instance is used; [`single`] mode is
selected otherwise.


### `add_context`

```ocaml
val add_context : Merlin.t -> string -> unit
```

Append a phrase that has already been *executed* so that later queries
see it.  Under the hood the function grows an internal buffer and
separates entries with " ;; ".


### `occurrences`

```ocaml
val occurrences :
  < process_mgr : _ ; .. > -> pos:int -> Merlin.t -> string -> ident_reply list
```

Return every (line, column) range that refers to the identifier located
at byte offset `pos` inside `code`.


### `complete`

```ocaml
val complete :
  < process_mgr : _ ; .. > ->
  ?doc:bool -> ?types:bool -> pos:int -> Merlin.t -> string -> reply
```

Auto-completion with optional documentation and precise type
information.  The returned [`reply`] record already contains corrected
`cmpl_start` / `cmpl_end` indices relative to the *given* code string
(Merlin’s originals include the context buffer and are therefore
shifted).


### Helper functions and types

* [`abs_position`] – convert a `(line, col)` pair into an absolute byte
  index.
* [`kind`], [`candidate`], [`reply`] – mirrors Merlin’s JSON schema.


---

## Identifier occurrences

Given a cursor position the module calls

```text
ocamlmerlin <mode> occurrences -identifier-at <pos>
```

parses the JSON answer and maps every start/end coordinate back to the
original buffer.  The logic is straightforward but fiddly – feel free
to borrow if you need something similar in another language.


---

## Code completion

`complete` is a convenience wrapper around the lengthier

```text
ocamlmerlin <mode> complete-prefix -position <offset> -prefix <string> \
                                         -doc y/n -types y/n
```

It performs three extra steps:

1. Computes the current prefix and the slice that needs replacement.
2. Translates Merlin’s *absolute* `start` / `end` fields to indices
   relative to the caller’s `code` string.
3. Decodes the JSON into typed records.


---

## Known limitations

* Only *occurrences* and *complete-prefix* are wrapped.  Extend the
  module if you need *type-enclosing*, *locate*, etc.
* The implementation spawns `ocamlmerlin` on every request when
  [`server = false`].  While convenient in scripts it is slow compared
  to the server mode.
* The module assumes **UTF-8, byte-based** positions (same as Merlin).
  If your editor uses character counts be sure to translate.

---


