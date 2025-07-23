# Snippet – Template Expansion Table

`Snippet` is a tiny utility module that maps short mnemonic names to ready-
made text fragments ("snippets").  The Chat-TUI inserts the selected snippet
verbatim into the composer when the user types:

```text
/expand <name>
```

The table is compiled into the binary and lives directly inside
`snippet.ml`, making it trivial to add or change entries without affecting
other modules.

---

## API

### `Snippet.find : string -> string option`

Returns `Some template` when **`name`** exists, or `None` otherwise.
Lookup is *case-sensitive*; the convention is to keep names lower-case.

Example:

```ocaml
open Chat_tui

match Snippet.find "sig" with
| Some t -> Stdio.print_endline t
| None   -> Stdio.eprintf "Unknown snippet\n"
```

### `Snippet.available : unit -> string list`

Returns all snippet names in the order they were declared.  Chat-TUI uses
this list to implement tab-completion after `/expand`.

```ocaml
# Snippet.available ();;
- : string list = ["sig"; "code"]
```

---

## Adding or Modifying Snippets

1. Open `lib/chat_tui/snippet.ml`.
2. Edit the `snippets` association list.  Each entry is a pair `(name, template)`.
3. Re-compile (`dune build`) and restart Chat-TUI.

Because the mapping is small, a simple list lookup (`Core.List.Assoc.find`)
is sufficient (O(n)).  If the table ever grows large, swapping in a hash
table would be straightforward.

---

## Known Limitations

* **Case sensitivity.** `String.equal` is used for lookup, therefore "Sig" ≠
  "sig".
* **Compile-time table.** Changing entries requires recompilation.  This is a
  deliberate design choice to keep the runtime simple and dependency-free.

