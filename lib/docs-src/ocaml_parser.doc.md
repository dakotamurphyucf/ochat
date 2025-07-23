# `Ocaml_parser` – lightweight OCaml source explorer  

`Ocaml_parser` is a tiny wrapper around *Ppxlib*’s parser and visitor
toolkit whose sole purpose is to turn real-world source files into a
ready-to-index data-structure.  The module was born to power the
ChatGPT-based code search & documentation features but is deliberately
generic and does **not** depend on the rest of the code-base (save for
some helpers in `Io`).


---

## Table of contents

1. [Quick start](#quick-start)  
2. [API overview](#api-overview)  
3. [Examples](#examples)  
4. [Design notes](#design-notes)  
5. [Known limitations](#known-limitations)


---

## Quick start

```ocaml
open Ocaml_parser

let get_all_results cwd path =
  match collect_ocaml_files cwd path with
  | Error e -> failwith e
  | Ok modules ->
    List.concat_map modules ~f:(fun m ->
      let mli, ml = parse_module_info cwd m in
      let run = Option.value_map ~default:[] ~f:traverse in
      run mli @ run ml)
```

`collect_ocaml_files` scans *path* recursively and groups `foo.mli` /
`foo.ml` pairs into a [`module_info`] record.  `parse_module_info`
performs the expensive lexing step once.  Finally `traverse` dives into
the AST and yields a list of [`parse_result`] with code, location and
docstrings.


---

## API overview

### Types

```ocaml
type ocaml_source = Interface | Implementation

type parse_result = {
  location     : string;      (* "File \"foo.ml\", line …"            *)
  file         : string;      (* basename                               *)
  module_path  : string;      (* "Foo.Bar.Baz"                           *)
  comments     : string list; (* ["(** … *)"; …]                         *)
  contents     : string;      (* raw code snippet                        *)
  ocaml_source : ocaml_source;
  line_start   : int;  char_start : int;
  line_end     : int;  char_end  : int;
}

type _ file_type  = Mli : mli file_type | Ml : ml file_type
and  mli = MLI
and  ml  = ML

type 'a file_info = { file_type : 'a file_type; file_name : string }

type module_info = {
  mli_file    : mli file_info option;
  ml_file     : ml  file_info option;
  module_path : string;          (* directory path on disk *)
}

type traverse_input   (* opaque – produced by [parse] *)
```

### Key functions

| Function | Purpose |
|----------|---------|
| `collect_ocaml_files dir path` | Recursively search *path* for `.ml` / `.mli` files and return their [`module_info`]. |
| `parse_module_info dir t` | Parse the files referenced by *t* and return up-to-date [`traverse_input`] values. |
| `traverse input` | Materialise a list of [`parse_result`] from an opaque `input`. |
| `format_parse_result r` | Turn a `parse_result` into `(header, body)` strings, suitable for pretty-printing or disk storage. |


---

## Examples

### Dump every docstring in a project

```ocaml
let dump_docstrings ~cwd root_dir =
  match Ocaml_parser.collect_ocaml_files cwd root_dir with
  | Error e -> eprintf "Error: %s\n" e
  | Ok modules ->
    modules
    |> List.concat_map ~f:(fun m ->
         let run = Option.value_map ~default:[] ~f:Ocaml_parser.traverse in
         let mli, ml = Ocaml_parser.parse_module_info cwd m in
         run mli @ run ml)
    |> List.iter ~f:(fun r ->
         List.iter r.comments ~f:(printf "[%s] %s\n" r.module_path))
```


### Convert results to Markdown

```ocaml
let to_markdown (r : Ocaml_parser.parse_result) : string =
  let header, body = Ocaml_parser.format_parse_result r in
  Printf.sprintf "```ocaml\n%s%s\n```" header body
```


---

## Design notes

* **Thread-friendly** – parsing occurs once; the returned
  `traverse_input` is data-only, so the actual AST traversal can safely
  be done in parallel domains.
* **No runtime dependencies** – aside from `Ppxlib` (parser / visitor)
  and [`Io`](Io.doc.md) for file handling.
* **Minimal surface area** – the goal is not to be a fully fledged
  compiler library; just to expose the bits needed for documentation and
  search.


---

## Known limitations

* The module only looks at attributes named `ocaml.doc` / `ocaml.text`.
  Other comment styles (`(**
  …*)` without a compiler attribute) are ignored.
* The entire file is loaded in memory – large generated sources will be
  slow and memory hungry.
* The parser does {b not} follow `#use` or `#require` directives.

