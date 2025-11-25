 # `Chat_tui.Highlight_tm_loader` – Load and resolve TextMate grammars

 `Highlight_tm_loader` manages a small registry of TextMate grammars and offers
 helpers to:

 - create an empty registry
 - add grammars from Jsonaf values or JSON files
 - resolve a grammar by language tag (e.g. "ocaml", "bash") or from a Markdown
   fenced‑code info string (e.g. "ocaml linenos")

 It is a thin layer over the `textmate-language` library. JSON is parsed with
 `Jsonaf` and converted to the Yojson tree expected by `textmate-language`.

 ---

 ## Table of contents

 1. [Creating a registry](#create_registry)
 2. [Adding grammars](#adding-grammars)
    - [`add_grammar_jsonaf`](#add_grammar_jsonaf)
    - [`add_grammar_jsonaf_file`](#add_grammar_jsonaf_file)
 3. [Finding grammars](#finding-grammars)
    - [`find_grammar_by_lang_tag`](#find_grammar_by_lang_tag)
    - [`find_grammar_for_info_string`](#find_grammar_for_info_string)
 4. [Invariants and behaviour](#invariants)
 5. [Known limitations](#limitations)

 ---

 ### Creating a registry <a id="create_registry"></a>

 ```ocaml
 val create_registry : unit -> registry
 ```

 Creates an empty `textmate-language` registry.

 Example – prepare a registry:
 ```ocaml
 let reg = Chat_tui.Highlight_tm_loader.create_registry ()
 ```

 ---

 ### Adding grammars <a id="adding-grammars"></a>

 #### `add_grammar_jsonaf` <a id="add_grammar_jsonaf"></a>

 ```ocaml
 val add_grammar_jsonaf : registry -> Jsonaf.t -> unit Core.Or_error.t
 ```

 Parses a TextMate grammar from a `Jsonaf.t` value and registers it.

 Notes
 - `Jsonaf` represents numbers as strings; the loader interprets them as
   integers when possible, otherwise as floats.
 - Uses `Jsonaf.parse` internally when loading from a file (see below). According
   to the Jsonaf docs, `Jsonaf.parse : string -> Jsonaf.t Or_error.t` parses a
   single JSON value and returns an error when the input is not exactly one
   object/value.

 Example – add a grammar already in memory:
 ```ocaml
 let reg = Chat_tui.Highlight_tm_loader.create_registry () in
 let j = Jsonaf.of_string "{\"scopeName\":\"source.ocaml\"}" in
 ignore (Chat_tui.Highlight_tm_loader.add_grammar_jsonaf reg j : unit Core.Or_error.t)
 ```

 #### `add_grammar_jsonaf_file` <a id="add_grammar_jsonaf_file"></a>

 ```ocaml
 val add_grammar_jsonaf_file : registry -> path:string -> unit Core.Or_error.t
 ```

 Reads `path`, parses it as JSON using `Jsonaf.parse`, converts the tree to the
 Yojson representation expected by `textmate-language`, and registers the
 resulting grammar.

 Example – load from a `.tmLanguage.json` file:
 ```ocaml
 let reg = Chat_tui.Highlight_tm_loader.create_registry () in
 match Chat_tui.Highlight_tm_loader.add_grammar_jsonaf_file reg ~path:"ocaml.tmLanguage.json" with
 | Ok () -> ()
 | Error e -> Core.eprintf "grammar load failed: %s\n" (Core.Error.to_string_hum e)
 ```

 ---

 ### Finding grammars <a id="finding-grammars"></a>

 #### `find_grammar_by_lang_tag` <a id="find_grammar_by_lang_tag"></a>

 ```ocaml
 val find_grammar_by_lang_tag : registry -> string -> TmLanguage.grammar option
 ```

 Resolves a grammar using a simple language tag like "ocaml", "bash", or
 "diff". Matching is case‑insensitive. The function consults a few common
 aliases (for example, OCaml accepts "ocaml", "ml", and "mli") and then tries
 three lookup strategies in order:

 1. by display name (e.g. "OCaml")
 2. by scope name (e.g. "source.ocaml")
 3. by file type / extension (e.g. "ml", "mli")

 Returns `Some grammar` on success, otherwise `None`.

 Example – look up OCaml after registering it:
 ```ocaml
 let reg = Chat_tui.Highlight_tm_loader.create_registry () in
 let (_ : unit Core.Or_error.t) =
   Chat_tui.Highlight_tm_loader.add_grammar_jsonaf_file reg ~path:"ocaml.tmLanguage.json"
 in
 match Chat_tui.Highlight_tm_loader.find_grammar_by_lang_tag reg "ocaml" with
 | Some _ -> ()
 | None -> failwith "grammar missing"
 ```

 #### `find_grammar_for_info_string` <a id="find_grammar_for_info_string"></a>

 ```ocaml
 val find_grammar_for_info_string : registry -> string option -> TmLanguage.grammar option
 ```

 Extracts the first word from a Markdown fenced‑code info string and resolves the
 corresponding grammar using `find_grammar_by_lang_tag`. For example,
 `Some "ocaml linenos"` resolves as if the tag were just `"ocaml"`.

 Returns `None` when the info string is empty or missing.

 ---

 ### Invariants and behaviour <a id="invariants"></a>

 - A grammar must be registered before it can be found.
 - Language tags are normalised to lowercase before matching.
 - Only JSON grammars are supported by this loader. If you have YAML grammars,
   convert them to JSON before loading.

 ---

 ### Known limitations <a id="limitations"></a>

 1. Input format is limited to JSON; `.tmLanguage` YAML files are not parsed
    directly.
 2. The set of built‑in language aliases is intentionally small (OCaml, Dune,
    OPAM, Bash/Shell, Diff). Other languages are tried verbatim and via the
    `"source.<lang>"` scope.
 3. The loader does not fetch dependencies or include files referenced by a
    grammar; ensure grammars are self‑contained or pre‑resolved.

 ---

 Last updated: 2025‑08‑10
