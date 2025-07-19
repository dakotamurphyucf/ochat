open Core

module Get_contents : Gpt_function.Def with type input = string = struct
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
(*  ODoc search tool                                                       *)
(* ---------------------------------------------------------------------- *)

module Odoc_search :
  Gpt_function.Def with type input = string * int option * string option * string = struct
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

module Fork : Gpt_function.Def with type input = fork_input = struct
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

module Webpage_to_markdown : Gpt_function.Def with type input = string = struct
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

module Add_line_numbers : Gpt_function.Def with type input = string = struct
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

module Get_url_content : Gpt_function.Def with type input = string = struct
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

module Index_ocaml_code : Gpt_function.Def with type input = string * string = struct
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

module Query_vector_db :
  Gpt_function.Def with type input = string * string * int * string option = struct
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

module Apply_patch : Gpt_function.Def with type input = string = struct
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

module Read_directory : Gpt_function.Def with type input = string = struct
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

module Make_dir : Gpt_function.Def with type input = string = struct
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
