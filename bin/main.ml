(** {1 𝚐𝚙𝚝 – multi-purpose CLI for Ochat, embeddings and code search}

    This executable is installed under the {!val:main_command} {!Command.group}
    and exposed as the [ochat] binary by [dune].  It bundles several loosely
    related developer-oriented utilities that build on the libraries living in
    the *ochat* code-base:

    • {{!val:index_command} index}      – crawl an OCaml project and write a
      hybrid semantic / lexical search corpus (see {!module:Indexer}).

    • {{!val:query_command} query}      – run natural-language retrieval over a
      previously created corpus using {!module:Vector_db.query_hybrid}.

    • {{!val:chat_completion_command} chat-completion} – convenience wrapper
      around {!Chat_response.Driver.run_completion_stream} for chatmd prompt
      files.

    • {{!val:tokenize_command} tokenize} – count {e Tikitoken} tokens of an
      arbitrary file; useful for prompt budgeting.

    • {{!val:html_to_markdown_command} html-to-markdown} / {b h2md} – convert
      static HTML to Markdown and preview the chunking heuristics used by
      {!module:Odoc_snippet}.

    A maintainer-oriented walk-through of the source file can be found in
    {b docs-src/bin/main.doc.md} (generated together with this comment).

    Invoke [ochat help SUBCOMMAND] for the fine-grained flag reference rendered
    by {!module:Core.Command}.  The sections below document each helper in
    more depth than the auto-generated usage strings.
*)

open Core
open Eio
open Command.Let_syntax
open Io

(** [index_command] builds a dense-vector + BM25 corpus from a directory tree.

    The command is exposed as:

{[ ochat index -folder-to-index ./lib -vector-db-folder ./vector ]}

    Flags:
    • [-folder-to-index] — root of the source tree to scan (defaults to
      [./lib]).  Both [*.ml] and [*.mli] files are parsed with
      {!module:Ocaml_parser} and their ocamldoc comments are chunked into
      token-bounded snippets.

    • [-vector-db-folder] — destination directory for the generated
      [vectors.{ml,mli}.binio] and [bm25.{ml,mli}.binio] artefacts.

    The heavy-lifting is delegated to {!Indexer.index}.  Concurrency is managed
    by a fresh {!Eio.Switch.t}, while HTTP calls to the OpenAI *Embeddings* API
    reuse the network capability from [env].  The function blocks until all
    files are written to disk.

    Example:
    {[
      $ ochat index -folder-to-index ./lib -vector-db-folder ./vector
    ]}
*)
let index_command =
  Command.basic
    ~summary:
      "Index OCaml code in the specified folder for a code vector search database using \
       OpenAI embeddings."
    (let%map_open folder_to_index =
       flag
         "-folder-to-index"
         (optional_with_default "./lib" string)
         ~doc:"FOLDER Path to the folder containing OCaml code to index (default: ./)"
     and vector_db_folder =
       flag
         "-vector-db-folder"
         (optional_with_default "./vector" string)
         ~doc:
           "FOLDER Path to the folder to store vector database data (default: ./vector)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       log ~dir @@ sprintf "Indexing OCaml code in folder: %s\n" folder_to_index;
       log ~dir @@ sprintf "Storing vector database data in folder: %s\n" vector_db_folder;
       Switch.run
       @@ fun sw ->
       let pool =
         Eio.Executor_pool.create
           ~sw
           (Eio.Stdenv.domain_mgr env)
           ~domain_count:(Domain.recommended_domain_count () - 1)
       in
       Indexer.index ~dir ~pool ~net:env#net ~vector_db_folder ~folder_to_index)
;;

(** [query_command] performs hybrid semantic × lexical retrieval over a corpus.

    Usage example:
    {[ ochat query -vector-db-folder ./vector -query-text "tail-recursive map" ]}

    * [-vector-db-folder] must point at the directory that holds the artefacts
      produced by {{!val:index_command} index_command}.

    * [-query-text] is the natural-language prompt.  A single embedding is
      requested from the OpenAI API and compared against every column of the
      corpus matrix.

    * [-num-results] upper-bounds the number of lines printed to stdout.

    The ranking function is {!Vector_db.query_hybrid}.  A fallback empty
    {!module:Bm25} index is created if the BM25 file cannot be found so that
    cosine similarity still works in isolation.
*)
let query_command =
  Command.basic
    ~summary:"Query the indexed OCaml code using natural language."
    (let%map_open vector_db_folder =
       flag
         "-vector-db-folder"
         (optional_with_default "./vector" string)
         ~doc:
           "FOLDER Path to the folder containing vector database data (default: ./vector)"
     and query_text =
       flag
         "-query-text"
         (required string)
         ~doc:"TEXT Natural language query text to search the indexed OCaml code"
     and num_results =
       flag
         "-num-results"
         (optional_with_default 5 int)
         ~doc:"NUM Number of top results to return (default: 5)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       log ~dir @@ sprintf "Querying indexed OCaml code with text: **%s**\n" query_text;
       log ~dir
       @@ sprintf "Using vector database data from folder: **%s**\n" vector_db_folder;
       log ~dir @@ sprintf "Returning top **%d** results\n" num_results;
       let vf = Eio.Stdenv.fs env / vector_db_folder in
       let vec_file = String.concat [ vector_db_folder; "/vectors.ml.binio" ] in
       let bm25_file = String.concat [ vector_db_folder; "/bm25.ml.binio" ] in
       let vecs = Vector_db.Vec.read_vectors_from_disk (Eio.Stdenv.fs env / vec_file) in
       let corpus = Vector_db.create_corpus vecs in
       let bm25 =
         try Bm25.read_from_disk (Eio.Stdenv.fs env / bm25_file) with
         | _ -> Bm25.create []
       in
       let response =
         Openai.Embeddings.post_openai_embeddings env#net ~input:[ query_text ]
       in
       let query_vector =
         Owl.Mat.of_arrays [| Array.of_list (List.hd_exn response.data).embedding |]
         |> Owl.Mat.transpose
       in
       let top_indices =
         Vector_db.query_hybrid
           corpus
           ~bm25
           ~beta:0.1
           ~embedding:query_vector
           ~text:query_text
           ~k:num_results
       in
       let docs = Vector_db.get_docs vf corpus top_indices in
       List.iteri
         ~f:(fun i doc ->
           print_endline @@ sprintf "\n**Result %d:**\n" (i + 1);
           print_endline @@ sprintf "```ocaml\n%s\n```\n" doc)
         docs)
;;

(** [chat_completion_command] feeds a *chatmd* conversation to the OpenAI
    Chat Completion endpoint and streams the assistant’s reply to a Markdown
    file.

    Flags:
    • [-prompt-file] – optional template prepended exactly once at the start of
      the output file.  If omitted the conversation continues from the
      existing [output-file] only.

    • [-output-file] – path where the running transcript is stored (default:
      {b ./prompts/default.md}).  The file is created on first run so you can
      resume later.

    Implementation note: the heavy lifting is done by
    {!Chat_response.Driver.run_completion_stream} which handles tool calling
    and incremental rendering.
*)
let chat_completion_command =
  Command.basic
    ~summary:
      "Call OpenAI API to provide chat completion based on the content of a chatmd \
       prompt file ."
    (let%map_open prompt_file =
       flag
         "-prompt-file"
         (optional string)
         ~doc:
           "FILE Path to the file containing inital prompt (optional). Think of this as \
            a  prompt template, and output-file as a conversation instance using the \
            prompt-template. If you are trying to continue a previous conversation do \
            not include this flag and only provide the output file"
     and output_file =
       flag
         "-output-file"
         (optional_with_default "./prompts/default.md" string)
         ~doc:
           "FILE Path to the file to save the chat completion output (default: \
            /prompts/default.md). If prompt-file is provided contents of prompt file \
            will be appended to the output file. "
     in
     fun () ->
       run_main
       @@ fun env ->
       Chat_response.Driver.run_completion_stream ~env ?prompt_file ~output_file ())
;;

(** [tokenize_command] prints the number of {e Tikitoken} tokens in a file.

    This is a thin wrapper around {!Tikitoken.encode}.  It loads the
    {e cl100k_base} Byte Pair Encoding rules once (~500 KB), encodes the file
    and outputs a single integer.

    Typical usage:
    {[ ochat tokenize -file bin/main.ml ]}
*)
let tokenize_command =
  Command.basic
    ~summary:"Tokenize the provided file using the OpenAI Tikitoken spec"
    (let%map_open file =
       flag
         "-file"
         (optional_with_default "bin/main.ml" string)
         ~doc:"FILE Path to the file to tokenize (default: bin/main.ml)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       let tiki_token_bpe = Tiktoken_data.o200k_base in
       let text = load_doc ~dir:(Eio.Stdenv.fs env) file in
       let codec = Tikitoken.create_codec tiki_token_bpe in
       log ~dir @@ sprintf "Tokenizing file: %s\n" file;
       let encoded = Tikitoken.encode ~codec ~text in
       Io.console_log ~stdout:env#stdout @@ sprintf "tokens: %i\n" (List.length encoded))
;;

(** [html_to_markdown_command] converts static HTML to Markdown and shows the
    internal chunking performed by {!Odoc_snippet}.

    It is primarily a debugging helper used when tweaking the snippet
    extraction heuristics.  The command prints:

    1. The full Markdown rendering.
    2. A delimiter line followed by every block returned by
       {!Odoc_snippet.Chunker.chunk_by_heading_or_blank}.
    3. The final slice passed to the embedding pipeline (sexp-encoded).

    Example:
    {[ ochat html-to-markdown -file docs/tutorial.html ]}
*)
let html_to_markdown_command =
  Command.basic
    ~summary:"Convert HTML file to Markdown"
    (let%map_open file =
       flag
         "-file"
         (optional_with_default "bin/main.ml" string)
         ~doc:"FILE Path to the HTML file to convert (default: bin/main.ml)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       let path = Eio.Path.(dir / file) in
       let markdown =
         Webpage_markdown.Driver.(convert_html_file path |> Markdown.to_string)
       in
       (* let block_strings =
         markdown |> String.split_lines |> Odoc_snippet.Chunker.chunk_by_heading_or_blank
       in *)
       (* let slice =
         Odoc_snippet.slice
           ~pkg:"html_to_markdown"
           ~doc_path:file
           ~markdown
           ~tiki_token_bpe:(load_doc ~dir "./out-cl100k_base.tikitoken.txt")
           ()
       in
       let s =
         Sexp.to_string_hum ~indent:2 [%sexp (slice : (Odoc_snippet.meta * string) list)]
       in *)
       Io.console_log ~stdout:env#stdout @@ sprintf "%s" markdown)
;;

(** [main_command] is the top-level {!Command.group} executed by the [ochat]
    binary.  It merely delegates to the sub-commands documented above.

    Run {b ochat help} or {b ochat help SUBCOMMAND} for the auto-generated manual
    pages provided by {!module:Command_unix}.
*)
let main_command =
  Command.group
    ~summary:
      "A command-line apps for using OpenAI Models for running chat completion on chatmd \
       files. Also provides Ocaml specfic functionality for indexing files into a vector \
       database, and natural language search of that ocaml code."
    [ "chat-completion", chat_completion_command
    ; "index", index_command
    ; "query", query_command
    ; "tokenize", tokenize_command
    ; "html-to-markdown", html_to_markdown_command
    ; "h2md", html_to_markdown_command
    ]
;;

let () = Command_unix.run main_command
