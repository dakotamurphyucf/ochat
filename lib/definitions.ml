open Core

module Create_file : Gpt_function.Def with type input = string * string = struct
  type input = string * string

  let name = "create_file"
  let description = Some "create new file with the given path/filename"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "file", `Object [ "type", `String "string" ]
            ; "content", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "file"; `String "content" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "file" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "content" j )
  ;;
end

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

module Edit_code : Gpt_function.Def with type input = string * string = struct
  type input = string * string

  let name = "edit_code"

  let description =
    Some "Produces line edits for code given an edit instruction and the code to edit."
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "instruction", `Object [ "type", `String "string" ]
            ; "code", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "instruction"; `String "code" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "instruction" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "code" j )
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

module Update_file_lines :
  Gpt_function.Def with type input = string * (int * string) list = struct
  type input = string * (int * string) list

  let name = "update_file_lines"

  let description =
    Some "update a file with a list of line edits. Make sure to include white spaces"
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "file", `Object [ "type", `String "string" ]
            ; ( "edits"
              , `Object
                  [ "type", `String "array"
                  ; ( "items"
                    , `Object
                        [ "type", `String "object"
                        ; ( "properties"
                          , `Object
                              [ "line", `Object [ "type", `String "integer" ]
                              ; "content", `Object [ "type", `String "string" ]
                              ] )
                        ] )
                  ] )
            ] )
      ; "required", `Array [ `String "file"; `String "edits" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "file" j
    , List.map ~f:(fun j ->
        ( Jsonaf.int_exn @@ Jsonaf.member_exn "line" j
        , Jsonaf.string_exn @@ Jsonaf.member_exn "content" j ))
      @@ Jsonaf.list_exn
      @@ Jsonaf.member_exn "edits" j )
  ;;
end

module Insert_code : Gpt_function.Def with type input = string * int * string = struct
  type input = string * int * string

  let name = "insert_code"
  let description = Some "insert code to a file at a given line number"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "file", `Object [ "type", `String "string" ]
            ; "line", `Object [ "type", `String "integer" ]
            ; "code", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "file"; `String "line"; `String "code" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "file" j
    , Jsonaf.int_exn @@ Jsonaf.member_exn "line" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "code" j )
  ;;
end

module Append_to_file : Gpt_function.Def with type input = string * string = struct
  type input = string * string

  let name = "append_to_file"
  let description = Some "append content to a file"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "file", `Object [ "type", `String "string" ]
            ; "content", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "file"; `String "content" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "file" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "content" j )
  ;;
end

module Summarize_file : Gpt_function.Def with type input = string = struct
  type input = string

  let name = "summarize_file"
  let description = Some "summarize text in given file"

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

module Generate_interface : Gpt_function.Def with type input = string = struct
  type input = string

  let name = "generate_interface"
  let description = Some "generate interface for given OCaml file"

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

module Generate_code_context_from_query :
  Gpt_function.Def with type input = string * string * string = struct
  type input = string * string * string

  let name = "generate_code_context_from_query"

  let description =
    Some
      "Takes an OCaml file, a user query that describes some OCaml code to generate, and \
       aggregated context for the queryand returns any context \
       (functions/module/functors/types/examples) from the file that might be useful for \
       generating the code for the user query. The context can be added to your \
       aggregated context"
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "file", `Object [ "type", `String "string" ]
            ; "query", `Object [ "type", `String "string" ]
            ; "context", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "file"; `String "query"; `String "context" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "file" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "query" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "context" j )
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

module Replace_lines : Gpt_function.Def with type input = string * int * int * string =
struct
  type input = string * int * int * string

  let name = "replace_lines"
  let description = Some "replace a range of lines in a file with given text"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "file", `Object [ "type", `String "string" ]
            ; "start_line", `Object [ "type", `String "integer" ]
            ; "end_line", `Object [ "type", `String "integer" ]
            ; "text", `Object [ "type", `String "string" ]
            ] )
      ; ( "required"
        , `Array
            [ `String "file"; `String "start_line"; `String "end_line"; `String "text" ] )
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "file" j
    , Jsonaf.int_exn @@ Jsonaf.member_exn "start_line" j
    , Jsonaf.int_exn @@ Jsonaf.member_exn "end_line" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "text" j )
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
