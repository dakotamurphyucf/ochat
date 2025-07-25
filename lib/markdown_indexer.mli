open! Core

(** [index_directory ?vector_db_root ~env ~index_name ~description ~root]
    indexes every Markdown document under directory [root].

    The function performs the following high-level steps:

    1.  Traverse [root] recursively using {!Markdown_crawler.crawl}.  All
        files whose basename ends with [".md"], [".markdown"] or [".mdown"]
        and whose size is in the range (0&nbsp;B, 10&nbsp;MiB] are passed to the
        pipeline.
    2.  Each document is cut into overlapping, token-bounded windows using
        {!Markdown_snippet.slice}.  Window ids are stable MD5 hashes of the
        file path and byte offsets which makes the operation idempotent –
        re-indexing the same directory leaves existing embeddings untouched.
    3.  All snippets are embedded in batches via {!Embed_service.create}
        which wraps the OpenAI {e Embeddings} endpoint with retry logic and
        a configurable request rate.
    4.  The resulting dense vectors are written to
        [<vector_db_root>/<index_name>/vectors.binio] using
        {!Vector_db.Vec.write_vectors_to_disk}.  Each snippet body is stored
        as a separate Markdown file below [snippets/].
    5.  Finally, a centroid vector is computed and the global catalogue
        [<vector_db_root>/md_index_catalog.binio] is updated via
        {!Md_index_catalog.add_or_update} so that the new index becomes
        discoverable by {!Vector_db}-based search helpers.

    All I/O is executed through Eio’s non-blocking API and therefore runs in
    the caller’s fibre.  The whole operation is cancel-safe and can be
    composed with other Eio code.

    Parameters:
    • [vector_db_root] – top-level directory holding one sub-directory per
      logical index (default [".md_index"]).
    • [env] – standard Eio environment obtained from [Eio_main.run].
    • [index_name] – unique identifier of the index, used as on-disk folder
      name and as catalogue key.
    • [description] – free-text summary shown in UIs.
    • [root] – directory tree to be indexed.

    @raise Failure if [root] does not exist or is not a directory.
*)
val index_directory
  :  ?vector_db_root:string
  -> env:Eio_unix.Stdenv.base
  -> index_name:string
  -> description:string
  -> root:_ Eio.Path.t
  -> unit
