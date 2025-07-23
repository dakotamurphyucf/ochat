(** Hybrid semantic + lexical index over OCaml sources.

    The module exposes a single entry-point {!val:index} that crawls a
    directory tree, extracts ocamldoc comments from each [*.ml] and
    [*.mli] file, obtains OpenAI embeddings for every documentation
    snippet, and persists both the dense vectors and a BM-25 paragraph
    index to disk.  The resulting artefacts can later be loaded by
    {!module:Vector_db} and {!module:Bm25} for fast similarity search
    inside ChatGPT-style assistants or code-navigation tools.

    Internally the work is split into a producer/consumer pipeline:

    • A pool of background fibres created via {!module:Task_pool}
      traverses the parse tree (see {!module:Ocaml_parser}) and breaks
      the documentation stream into ~100–300-token chunks.  Each chunk
      is tagged with precise location metadata (file, line range,
      interface vs implementation).

    • Batches of chunks are sent to the OpenAI *Embeddings* endpoint via
      {!module:Openai.Embeddings}.  The returned vectors are normalised
      and written out using {!Vector_db.Vec.write_vectors_to_disk}.

    • In parallel the plain text of every snippet is fed into
      {!module:Bm25.create} so that keyword-based ranking is also
      available at query time.

    Concurrency is managed with {!module:Eio} fibres and
    {!Eio.Domain_manager}.  The function is fully blocking until the
    index has been flushed to disk.
*)

open Eio

(** Blocks until the index has been written.  May raise if the OpenAI
      request fails or the file system is read-only. *)
val index
  :  sw:Switch.t
       (** Parent switch for all fibres spawned by the indexer.  Cancelling
      the switch aborts outstanding HTTP requests and file IO. *)
  -> dir:Fs.dir_ty Path.t
       (** Root directory where output files are written and from which
      [folder_to_index] is resolved. *)
  -> dm:Domain_manager.ty Resource.t
       (** Optional {!Domain_manager} resource for multi-core execution of
      CPU-heavy parsing tasks. *)
  -> net:_ Net.t (** Network capability used for the HTTPS calls to the OpenAI API. *)
  -> vector_db_folder:string
       (** Sub-directory of [dir] that will hold
      - [`vectors.{ml,mli}.binio`] – serialised dense embeddings
      - [`bm25.{ml,mli}.binio`]   – serialised BM-25 indices             *)
  -> folder_to_index:string
       (** Source tree (relative to [dir]) that will be recursively scanned
      for OCaml files. *)
  -> unit
