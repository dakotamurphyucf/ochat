(** GPT function *definitions* exposed by the Ochat agent.

    This module is **data-only** – it bundles a catalogue of tool
    specifications that can be offered to the OpenAI function-calling
    API.  Each sub-module implements {!Ochat_function.Def} and therefore
    provides four values that describe a tool but do *not* implement
    its runtime behaviour:

    • [name] – unique identifier that the language model uses.
    • [description] – one-paragraph human-readable summary.
    • [parameters] – JSON schema (draft-07) of the expected input.
    • [input_of_string] – converts the JSON payload returned by the
      API into a strongly-typed OCaml value.

    To obtain an *executable* tool you must pair the definition with an
    implementation using {!Ochat_function.create_function}.

    Nothing in this module performs I/O; all operations are pure and
    total.

    {1  Catalogue}

    The current set of tool definitions shipped with the library:

    • {!Get_contents}        – read a file from the local filesystem
    • {!Odoc_search}         – search locally-indexed odoc docs
    • {!Fork}                – spawn a nested agent performing a task
    • {!Webpage_to_markdown} – download an URL and convert to Markdown
    • {!Add_line_numbers}    – prefix lines of text with numbers
    • {!Get_url_content}     – fetch raw contents of an URL
    • {!Index_ocaml_code}    – embed OCaml sources into a vector store
    • {!Query_vector_db}     – semantic search in a vector database
    • {!Apply_patch}         – apply a V4A diff/patch to the workspace
    • {!Read_directory}      – list entries of a directory
    • {!Make_dir}            – create a directory on the filesystem
*)

module Get_contents : Ochat_function.Def with type input = string

module Odoc_search :
  Ochat_function.Def with type input = string * int option * string option * string

type fork_input =
  { command : string
  ; arguments : string list
  }

module Fork : Ochat_function.Def with type input = fork_input
module Webpage_to_markdown : Ochat_function.Def with type input = string
module Add_line_numbers : Ochat_function.Def with type input = string
module Get_url_content : Ochat_function.Def with type input = string
module Index_ocaml_code : Ochat_function.Def with type input = string * string

module Query_vector_db :
  Ochat_function.Def with type input = string * string * int * string option

module Apply_patch : Ochat_function.Def with type input = string
module Read_directory : Ochat_function.Def with type input = string
module Make_dir : Ochat_function.Def with type input = string

(* ---------------------------------------------------------------------- *)
(*  Markdown indexing & search                                              *)
(* ---------------------------------------------------------------------- *)

(** {1 Index_markdown_docs}

    Indexes a folder containing Markdown documents into a vector
    database suitable for semantic search.  The 4-tuple carried in
    [input] is:

    • [root]            – directory to crawl recursively.
    • [index_name]      – logical identifier for the index (e.g.
      "docs").
    • [description]     – one-line blurb that describes the corpus.
    • [vector_db_root]  – optional destination directory for the
      vector database; defaults to ".md_index" when [None]. *)

module Index_markdown_docs :
  Ochat_function.Def with type input = string * string * string * string option

(** {1 Markdown_search}

    Performs a semantic search over one or more Markdown indices.  The
    4-tuple carried in [input] is:

    • [query]           – user query.
    • [k]               – optional upper bound on the number of hits
      (defaults to 5 when [None]).
    • [index_name]      – specific index to query or "all"; when
      [None] defaults to "all".
    • [vector_db_root]  – root directory housing the indices; defaults
      to ".md_index" when [None]. *)

module Markdown_search :
  Ochat_function.Def with type input = string * int option * string option * string option
