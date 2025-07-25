(** Internal catalogue of GPT function definitions.

    Each sub-module in this file implements {!Ochat_function.Def}.  The
    definitions are *pure metadata* – they do **not** perform any
    side-effects or I/O.  A caller must pair them with an
    implementation via {!Ochat_function.create_function} before the tool
    can be executed.

    See {!file:definitions.mli} for a high-level overview of the
    available tools.
*)

open Core

(** {1 Get_contents}

    Definition of the "read_file" tool.  The tool expects a JSON
    object with a single field [file] containing a path, and forwards
    the path unchanged as a string.

    Example payload accepted by {!input_of_string}:

    {[
      "{ \"file\": \"/tmp/example.txt\" }"
    ]}
*)

module Get_contents : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "read_file"
  let description = Some "reads contents of file with the given path/filename"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; "properties", `Object [ "file", `Object [ "type", `String "string" ] ]
      ; "required", `Array [ `String "file" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "file" j
  ;;
end

(* ---------------------------------------------------------------------- *)
(*  Markdown indexing & search                                             *)
(* ---------------------------------------------------------------------- *)

(** {1 Index_markdown_docs}

    Registers (or updates) a vector database built from a directory of
    Markdown files.  The tool takes the following JSON payload:

    {v
    {
      "root"           : "string",   // directory to crawl recursively
      "index_name"     : "string",   // logical identifier, e.g. "docs"
      "description"    : "string",   // one-line blurb for catalogue
      "vector_db_root" : "string"?   // where to store the index (optional)
    }
    v}

    It forwards the data as a 4-tuple [(root, index_name, description,
    vector_db_root)].  The implementation is responsible for running
    {!Markdown_indexer.index_directory}. *)

module Index_markdown_docs :
  Ochat_function.Def with type input = string * string * string * string option = struct
  type input = string * string * string * string option

  let name = "index_markdown_docs"

  let description =
    Some "Index a directory of Markdown files into a vector database for semantic search"
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "root", `Object [ "type", `String "string" ]
            ; "index_name", `Object [ "type", `String "string" ]
            ; "description", `Object [ "type", `String "string" ]
            ; "vector_db_root", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "root"; `String "index_name"; `String "description" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s : input =
    let j = Jsonaf.of_string s in
    let root = Jsonaf.string_exn @@ Jsonaf.member_exn "root" j in
    let index_name = Jsonaf.string_exn @@ Jsonaf.member_exn "index_name" j in
    let description = Jsonaf.string_exn @@ Jsonaf.member_exn "description" j in
    let vector_db_root =
      Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "vector_db_root" j
    in
    root, index_name, description, vector_db_root
  ;;
end

(** {1 Markdown_search}

    Semantic search tool for Markdown indices previously created with
    {!Index_markdown_docs}.  Expected JSON schema:

    {v
    {
      "query"          : "string",         // search string (required)
      "k"              : 5?,                // max hits (optional)
      "index_name"     : "string"?,        // index to query or "all"
      "vector_db_root" : "string"?         // root directory holding indices
    }
    v}
  *)

module Markdown_search :
  Ochat_function.Def with type input = string * int option * string option * string option =
struct
  type input = string * int option * string option * string option

  let name = "markdown_search"

  let description =
    Some
      "Perform a semantic search over one or more Markdown indices previously built with \
       `index_markdown_docs`."
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "query", `Object [ "type", `String "string" ]
            ; "k", `Object [ "type", `String "integer" ]
            ; "index_name", `Object [ "type", `String "string" ]
            ; "vector_db_root", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "query" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s : input =
    let j = Jsonaf.of_string s in
    let query = Jsonaf.string_exn @@ Jsonaf.member_exn "query" j in
    let k = Option.map ~f:Jsonaf.int_exn @@ Jsonaf.member "k" j in
    let index_name = Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "index_name" j in
    let vector_db_root =
      Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "vector_db_root" j
    in
    query, k, index_name, vector_db_root
  ;;
end

(* ---------------------------------------------------------------------- *)
(*  ODoc search tool                                                       *)
(* ---------------------------------------------------------------------- *)

(** {1 Odoc_search}

    Definition of an OCaml–specific documentation search tool that
    queries a locally-indexed `.odoc` corpus.  The [input] is a
    quadruplet [(query, k, index, package)]:

    • [query]   – free-form text or code snippet used for semantic search.
    • [k]       – optional upper bound on the number of hits (defaults to 5).
    • [index]   – optional path to a custom odoc search index.
    • [package] – the opam package name to scope the search or "all".
*)

module Odoc_search :
  Ochat_function.Def with type input = string * int option * string option * string =
struct
  type input = string * int option * string option * string

  let name = "odoc_search"

  let description =
    Some
      {|
ODoc semantic-search utility.

When to use
✓ The agent needs authoritative explanations, type signatures, or usage examples from OCaml libraries that are *already* installed and indexed locally.
✓ While planning a refactor, code generation, or bug-fix and concrete documentation snippets would accelerate reasoning.
✓ Quick recall of a package README or module docs without opening a browser.

When *not* to use
✗ You only need an exact substring / regex lookup in the user’s source code – prefer `grep_search` or similar tools.
✗ You require up-to-date *web* information about packages that are not present in the local index.
✗ You are scanning huge source files rather than documentation.

Guidelines for callers
1. Provide `query` in natural language or code fragments; rewriting user text is optional – optimise for precision.
2. Set `package` to the target opam package when the task is scoped; otherwise use "all" (default) to search every package via the package-level index.
3. Keep `k` small (≤10) unless more results are truly required; larger values add latency and noise.
4. Returned value is a Markdown list:
   `[rank] [package] <snippet-id>` followed by the first 8000 characters of each snippet.
5. Change `index` only when working with a non-standard documentation snapshot.
6. If the results are not satisfactory, consider refining the query or using a different package.

Query-crafting best practices
• Keep it concise and meaningful – avoid filler like “could you maybe…”.
• Use the corpus’ own vocabulary: module names (`Eio.Switch`), function names (`List.mapi`) or OCaml type signatures.
• Include a disambiguating keyword (package, type, module) in the same sentence when needed.
• Drop boiler-plate stop-words; they add noise to the embedding.
• Type-signature queries work well for API look-ups (`'a list -> f:(int -> 'a -> 'b) -> 'b list`).
• Iterate: if top-k looks generic, trim noise or add a specific term seen in a near-miss snippet, then re-query.

JSON input schema
```
{
  "query"   : "string",              // required
  "package" : "all" | "eio" | …,    // required
  "k"       : 5?,                     // optional, default 5
  "index"   : ".odoc_index"?         // optional, default
}
```
|}
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "query", `Object [ "type", `String "string" ]
            ; "k", `Object [ "type", `String "integer" ]
            ; "index", `Object [ "type", `String "string" ]
            ; "package", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "query"; `String "package" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    let query = Jsonaf.string_exn @@ Jsonaf.member_exn "query" j in
    let k = Option.map ~f:Jsonaf.int_exn @@ Jsonaf.member "k" j in
    let index = Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "index" j in
    let package = Jsonaf.string_exn @@ Jsonaf.member_exn "package" j in
    query, k, index, package
  ;;
end

(* ---------------------------------------------------------------------- *)
(*  Fork – clone the current agent and run a command in the clone          *)
(* ---------------------------------------------------------------------- *)

type fork_input =
  { command : string
  ; arguments : string list
  }

(** {1 Fork}

    Definition of a tool that spawns an auxiliary agent operating on
    the same workspace.  The [input] record specifies the [command]
    executed by the fork and an optional list of CLI-style
    [arguments].
*)

module Fork : Ochat_function.Def with type input = fork_input = struct
  [@@@warning "-69"]

  type input = fork_input

  let name = "fork"

  let description =
    Some
      {|Spawn an auxiliary **forked agent** that inherits your *entire* context and solves a focussed sub-task **without polluting the parent conversation**.

When to call
• Long or detail-heavy work (deep debugging, large code generation, extensive data exploration).
• Experiments that may generate irrelevant intermediate chatter.

Invocation
• `command` – command or task.
• `arguments` – optional arguments to pass to the command.

Prompting essentials for GPT-4.1 / O3 reasoning models
1. **Be explicit, be clear** – state goals and required output structure precisely.
2. **Structured output** – delimit sections so they’re machine-parsable.
3. **Expose reasoning** – include your full chain-of-thought in RESULT; it is valuable for audit.
4. **Self-verify** – re-check answers; note any open issues in PERSIST.
5. **Avoid redundant tokens** – no need for phrases like “let’s think step-by-step”; just think and write.

The fork Agent Returns exactly one assistant message, using this template:

```
===RESULT===
<Extremely detailed narrative of everything you did: reasoning, obstacles & fixes, code patches, logs, validation steps, etc.>

===PERSIST===
<Concise (≤20 items) bullet list of facts, artefacts, or next actions that the parent agent must remember. Bullets can be as detailed as needed, but should be succinct.>
```

• Use Markdown; wrap code or patches in fenced blocks.
• RESULT should be exhaustive; PERSIST should be succinct.
|}
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "command", `Object [ "type", `String "string" ]
            ; ( "arguments"
              , `Object
                  [ "type", `String "array"
                  ; "items", `Object [ "type", `String "string" ]
                  ] )
            ] )
      ; "required", `Array [ `String "command"; `String "arguments" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    let command = Jsonaf.string_exn @@ Jsonaf.member_exn "command" j in
    let arguments =
      match Jsonaf.member "arguments" j with
      | Some (`Array arr) -> List.map arr ~f:Jsonaf.string_exn
      | _ -> []
    in
    { command; arguments }
  ;;
end

(** {1 Webpage_to_markdown}

    Definition of the "webpage_to_markdown" tool.  Accepts a single
    [url] string and asks the implementation to download the document
    and convert it to Markdown.  The [input] type is therefore a plain
    string.
*)

module Webpage_to_markdown : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "webpage_to_markdown"

  let description =
    Some "Download a web page and return its contents converted to Markdown"
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; "properties", `Object [ "url", `Object [ "type", `String "string" ] ]
      ; "required", `Array [ `String "url" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "url" j
  ;;
end

(** {1 Add_line_numbers}

    Metadata for a trivial utility that prefixes every line of a text
    block with its 1-based index.  Receives the raw [text] as input
    and returns the annotated version.
*)

module Add_line_numbers : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "add_line_numbers"
  let description = Some "add line numbers to a snippet of text"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; "properties", `Object [ "text", `Object [ "type", `String "string" ] ]
      ; "required", `Array [ `String "text" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "text" j
  ;;
end

(** {1 Get_url_content}

    Tool definition that retrieves the raw body of the resource
    located at the given [url].  The [input] is that URL as a string.
*)

module Get_url_content : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "get_url_content"
  let description = Some "get the contents of a URL"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; "properties", `Object [ "url", `Object [ "type", `String "string" ] ]
      ; "required", `Array [ `String "url" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "url" j
  ;;
end

(** {1 Index_ocaml_code}

    Registers all OCaml sources found under [folder_to_index] in a
    vector search database located at [vector_db_folder].  The
    function later allows semantic code search via
    {!Query_vector_db}.
*)

module Index_ocaml_code : Ochat_function.Def with type input = string * string = struct
  type input = string * string

  let name = "index_ocaml_code"

  let description =
    Some
      "Index all OCaml code from a folder into a vector search database using OpenAI \
       embeddings"
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "folder_to_index", `Object [ "type", `String "string" ]
            ; "vector_db_folder", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "folder_to_index"; `String "vector_db_folder" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "folder_to_index" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "vector_db_folder" j )
  ;;
end

(** {1 Query_vector_db}

    Performs a semantic search over a previously built vector
    database.  The 4-tuple carried in [input] is

    • [vector_db_folder] – path to the database on disk
    • [query]            – natural-language search query
    • [num_results]      – maximum number of hits to return
    • [index]            – optional secondary index name
*)

module Query_vector_db :
  Ochat_function.Def with type input = string * string * int * string option = struct
  type input = string * string * int * string option

  let name = "query_vector_db"
  let description = Some "Query a vector database for code snippets given a user query"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "vector_db_folder", `Object [ "type", `String "string" ]
            ; "query", `Object [ "type", `String "string" ]
            ; "num_results", `Object [ "type", `String "integer" ]
            ; "index", `Object [ "type", `Array [ `String "string"; `String "null" ] ]
            ] )
      ; ( "required"
        , `Array
            [ `String "vector_db_folder"
            ; `String "query"
            ; `String "num_results"
            ; `String "index"
            ] )
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "vector_db_folder" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "query" j
    , Jsonaf.int_exn @@ Jsonaf.member_exn "num_results" j
    , Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "index" j )
  ;;
end

(** {1 Apply_patch}

    Specification of the *workspace mutation* tool.  The sole string
    input must contain a patch expressed in the project-specific V4A
    format delimited by

    {v
    *** Begin Patch
    ...
    *** End Patch
    v}

    The implementation is responsible for validating and applying the
    patch to the local git repository.
*)

module Apply_patch : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "apply_patch"

  let description =
    Some
      {|This is a custom utility that makes it more convenient to add, remove, move, or edit code files. `apply_patch` effectively allows you to execute a diff/patch against a file, but the format of the diff specification is unique to this task, so pay careful attention to these instructions. To use the `apply_patch` command, you should pass a message of the following structure as "input":

*** Begin Patch
[YOUR_PATCH]
*** End Patch

Where [YOUR_PATCH] is the actual content of your patch, specified in the following V4A diff format.

*** [ACTION] File: [path/to/file] -> ACTION can be one of Add, Update, or Delete.
For each snippet of code that needs to be changed, repeat the following:
[context_before] -> See below for further instructions on context.
- [old_code] -> Precede the old code with a minus sign.
+ [new_code] -> Precede the new, replacement code with a plus sign.
[context_after] -> See below for further instructions on context.

For instructions on [context_before] and [context_after]:
- By default, show 3 lines of code immediately above and 3 lines immediately below each change. If a change is within 3 lines of a previous change, do NOT duplicate the first change’s [context_after] lines in the second change’s [context_before] lines.
- If 3 lines of context is insufficient to uniquely identify the snippet of code within the file, use the @@ operator to indicate the class or function to which the snippet belongs. For instance, we might have:
@@ class BaseClass
[3 lines of pre-context]
- [old_code]
+ [new_code]
[3 lines of post-context]

- If a code block is repeated so many times in a class or function such that even a single @@ statement and 3 lines of context cannot uniquely identify the snippet of code, you can use multiple `@@` statements to jump to the right context. For instance:

@@ class BaseClass
@@ 	def method():
[3 lines of pre-context]
- [old_code]
+ [new_code]
[3 lines of post-context]

Note, then, that we do not use line numbers in this diff format, as the context is enough to uniquely identify code. An example of a message that you might pass as "input" to this function, in order to apply a patch, is shown below.

*** Begin Patch
*** Update File: pygorithm/searching/binary_search.py
@@ class BaseClass
@@     def search():
-          pass
+          raise NotImplementedError()

@@ class Subclass
@@     def search():
-          pass
+          raise NotImplementedError()

*** End Patch
|}
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ ( "input"
              , `Object
                  [ "type", `String "string"
                  ; ( "description"
                    , `String "The apply_patch command that you wish to execute." )
                  ] )
            ] )
      ; "required", `Array [ `String "input" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "input" j
  ;;
end

(** {1 Read_directory}

    Presents a thin wrapper around [Sys.readdir].  Given a [path]
    string, returns (via the implementation) the list of entries in
    that directory.
*)

module Read_directory : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "read_directory"

  let description =
    Some "Read the contents of a directory and return a list of files and directories."
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ ( "path"
              , `Object
                  [ "type", `String "string"
                  ; "description", `String "The path of the directory to read."
                  ] )
            ] )
      ; "required", `Array [ `String "path" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "path" j
  ;;
end

(** {1 Make_dir}

    Tool definition used to create a new directory on the filesystem.
    The [input] is the destination [path] supplied as a string.
*)

module Make_dir : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "mkdir"
  let description = Some "Create a directory at the specified path."

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ ( "path"
              , `Object
                  [ "type", `String "string"
                  ; "description", `String "The path of the directory to create."
                  ] )
            ] )
      ; "required", `Array [ `String "path" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "path" j
  ;;
end
