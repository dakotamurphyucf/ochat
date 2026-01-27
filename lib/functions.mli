(** Ready-made Ochat **tools** implemented on top of {{!module:Ochat_function}Ochat_function}.

    The values exposed by this module are *registrations* – each one is the
    result of a call to {!Ochat_function.create_function}.  They can be mixed and
    matched freely when building the tool-list for
    {!Openai.Completions.post_chat_completion}:

    {[
      let tools, dispatch_tbl =
        Ochat_function.functions
          [ Functions.get_contents ~dir:cwd
          ; Functions.apply_patch  ~dir:cwd
          ; Functions.odoc_search  ~dir:cwd ~net
          ]
    ]}

    All helpers are *side-effect free* until their [`run`] callback is executed;
    the required capabilities (filesystem directory, network handle, domain
    manager, …) are injected explicitly via labelled arguments.  This
    capability-style design makes the functions easy to reason about in a
    concurrent [`Eio`] application.

    {1 Categories}

    • Filesystem – {{!val:get_contents}get_contents},
                   {{!val:apply_patch}apply_patch},
                   {{!val:append_to_file}append_to_file},
                   {{!val:find_and_replace}find_and_replace},
                   {{!val:read_dir}read_dir},
                   {{!val:mkdir}mkdir}

    • Search     – {{!val:odoc_search}odoc_search},
                   {{!val:query_vector_db}query_vector_db},
                   {{!val:markdown_search}markdown_search}

    • Indexing   – {{!val:index_ocaml_code}index_ocaml_code},
                   {{!val:index_markdown_docs}index_markdown_docs}

    • Web        – {{!val:get_url_content}get_url_content},
                   {{!val:webpage_to_markdown}webpage_to_markdown}
*)

(** Prefix each line of [text] with a 1-based line counter.

    The helper is primarily used for pretty-printing code snippets in tool
    responses so that large fragments can be referred to unambiguously by the
    LLM (e.g. “change line 42”).  No trailing newline is added. *)
val add_line_numbers : string -> string

(** {1 Filesystem helpers} *)

(** Register the [`read_file`] tool.

    • **Schema** – expects an argument object with:

      {ul
      {- [file] (string) – path to read (preferred field);}
      {- [path] (string) – legacy alias accepted by the decoder;}
      {- [offset] (int, optional) – 0-based line offset to start reading from;}
      {- [line_count] (int, optional) – number of lines to return.}}

    • **Behaviour** – returns the UTF-8 contents of [`file`], read via
      {!Eio.Buf_read} with a fixed byte cap. The returned text is prefixed with
      a small metadata header of the form:

      {[ path:START-END:\n[total_lines=N]\n... ]}

      The tool refuses to read binary files (best-effort heuristic) and returns
      a human-readable error message instead. *)
val get_contents : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** Register the [`get_url_content`] tool.

    Downloads an HTTP resource using {{!module:Io.Net}Io.Net}, strips all HTML
    tags with [LambdaSoup], and returns the visible text.  Content larger than
    the current chat context window is not truncated automatically – callers
    should post-process the string if necessary. *)
val get_url_content : net:_ Eio.Net.t -> Ochat_function.t

(** Register the [`index_ocaml_code`] tool.

    Recursively walks [folder_to_index] (argument) and builds a hybrid vector
    + BM-25 index under [vector_db_folder].  The heavy lifting is delegated to
    {!module:Indexer}.  Progress reporting happens on stdout; the returned
    string is always ["code has been indexed"]. *)
val index_ocaml_code
  :  env:Eio_unix.Stdenv.base
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> net:_ Eio.Net.t
  -> Ochat_function.t

(** Register the [`query_vector_db`] tool.

    Given a query string, combines OpenAI embeddings with a BM-25 overlay to
    search a pre-built index.  The result is a Markdown list of code snippets
    wrapped in [```ocaml] fences.  See {!Vector_db.query_hybrid} for the
    scoring details. *)
val query_vector_db : dir:Eio.Fs.dir_ty Eio.Path.t -> net:_ Eio.Net.t -> Ochat_function.t

(** Register the [`index_markdown_docs`] tool.
    Crawls a directory of Markdown files, chunks them into token–bounded
    windows, embeds the text with OpenAI, and writes a vector database under
    [.md_index/<index_name>].  The helper is a thin wrapper around
    {!Markdown_indexer.index_directory}. *)
val index_markdown_docs
  :  env:Eio_unix.Stdenv.base
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> Ochat_function.t

(** Register the [`markdown_search`] tool – semantic search across Markdown
    indices previously created with {!index_markdown_docs}. *)
val markdown_search : dir:Eio.Fs.dir_ty Eio.Path.t -> net:_ Eio.Net.t -> Ochat_function.t

(** Register the [`apply_patch`] tool that applies a *Ochat diff* to the
    workspace rooted at [dir].  The helper is a thin wrapper around
    {!Apply_patch.process_patch}.  It supports additions, deletions, in-place
    modifications, and file moves. *)
val apply_patch : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** Register the [`read_directory`] tool.  Returns the entry list of the given
    sub-directory without recursion. *)
val read_dir : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** Register the [`mkdir`] tool.  Creates the specified sub-directory with mode
    0o700.  The action is idempotent when the folder already exists. *)
val mkdir : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** Register the [`append_to_file`] tool.  Appends a string to a file, creating
    it if necessary.  The input is a tuple of [file] and [text].  The action is
    idempotent when the text is already present at the end of the file. *)
val append_to_file : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** Register the [`find_and_replace`] tool.  Searches for a string in a file and
    replaces it with another string.  The input is a tuple of [file], [search],
    [replace], and a boolean [all] that controls whether all occurrences should
    be replaced or only the first one.  The action is idempotent when the search
    string is not found or already replaced. *)
val find_and_replace : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** {1 Search helpers} *)

(** Register the [`odoc_search`] tool – a semantic search over locally indexed
    OCaml documentation.  The tool embeds the textual query with OpenAI and
    performs cosine similarity against an [Owl] matrix of pre-computed snippet
    embeddings.  Results are rendered in the same Markdown format as the
    original command-line utility bundled in this repository. *)
val odoc_search : dir:Eio.Fs.dir_ty Eio.Path.t -> net:_ Eio.Net.t -> Ochat_function.t

(** {1 Web helper} *)

(** Register the [`webpage_to_markdown`] tool that converts a remote web page
    to Markdown using a heuristic readability extractor.  See
    {!Webpage_markdown.Tool}.  The environment capability [env] is used to
    access the host’s standard network stack and DNS. *)
val webpage_to_markdown
  :  env:Eio_unix.Stdenv.base
  -> dir:_ Eio.Path.t
  -> net:_ Eio.Net.t
  -> Ochat_function.t

(** {1 Miscellaneous} *)

(** Placeholder registration for the [`fork`] tool.  Its implementation is a
    stub and should never be called directly; it exists only so that the JSON
    schema can be advertised to the model. *)
val fork : Ochat_function.t

(** Apply *Recursive Meta-Prompting* refinement to a raw prompt.  The tool
    receives the full prompt in its JSON [prompt] field and returns the
    improved version produced by {!Meta_prompting.Recursive_mp.refine}. *)
val meta_refine : env:Eio_unix.Stdenv.base -> Ochat_function.t

(** Register the [`import_image`] tool.

    The tool reads an image file from disk and returns a structured tool output
    payload containing a data-URI (base64) suitable for OpenAI “image input”
    parts.

    If the file does not exist, a textual error is returned instead. *)
val import_image : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t
