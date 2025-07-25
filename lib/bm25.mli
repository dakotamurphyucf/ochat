(** A document that can be indexed.

    The [id] must be unique within the corpus – it is returned by
    {!query} so that the caller can look the document up again.  The
    [text] field is free-form UTF-8.  It will be split into
    lower-case tokens by {!tokenize}. *)

(** Lightweight in-memory BM-25 index used as a fast fall-back to
    semantic vector search.

    {b Intended use-case} – corpora of up to ~50&nbsp;k short
    documents (code snippets, README paragraphs, etc.).

    All operations are single-threaded and allocate only standard OCaml
    data structures.  For very large datasets use a dedicated search
    engine (e.g. Tantivy, MeiliSearch) isnstead. *)

type doc =
  { id : int
  ; text : string
  }

(** In-memory BM25 index.  Use {!create} to build one, {!query} to
    search it, and {!write_to_disk}/{!read_from_disk} to persist it. *)
type t

(** [tokenize s] splits [s] into lower-case tokens.

    The splitter treats ASCII whitespace and a few punctuation
    characters – `(){}[] ,;` – as delimiters.  Tokens found in a very
    small stop-word list are removed (currently the list is empty –
    override it in {!Bm25}ʼs implementation if needed).

    The function performs no stemming or diacritics removal; adapt or
    replace it if your corpus requires more advanced processing. *)
val tokenize : string -> string list

(** [create docs] builds a BM25 index over [docs].

    Each document is tokenised with {!tokenize}.  The result contains
    enough information to evaluate BM25 with the usual parameters
    (k₁ = 1.5, b = 0.75).  Construction time is O(total-token-count)
    and memory usage is modest but the implementation has not been
    tuned for very large corpora (> ~50 k documents).

    @raise Invalid_argument if two documents share the same [id]. *)
val create : doc list -> t

(** [query t ~text ~k] returns up to [k] documents ranked by BM25
    relevance to [text].

    The score is a float > 0.  Results are sorted in descending order
    (best match first).  For ties the order is unspecified.

    The implementation applies an extra {b coverage weight}: if the
    query contains several distinct tokens, documents that contain a
    larger proportion of them are promoted.

    @param k  maximum number of hits to return (clamped to corpus size)

    Example – indexing three fragments and retrieving the best match:
    {[
      let docs =
        [ { Bm25.id = 0; text = "OCaml is a functional language" }
        ; { id = 1; text = "BM25 is a ranking function" }
        ; { id = 2; text = "OCaml and ReasonML share a syntax" }
        ]
      in
      let idx   = Bm25.create docs in
      let hits  = Bm25.query idx ~text:"ocaml ranking" ~k:2 in
      (* [hits] might be [ (0, 0.73); (2, 0.44) ] *)
    ]} *)
val query : t -> text:string -> k:int -> (int * float) list

(** [write_to_disk path t] serialises [t] to [path] using
    {!Bin_prot}.  The file is overwritten (or created) with
    permissions `0o600`.

    This function must run inside an [Eio] fibre – it performs
    non-blocking I/O via {!Eio.Path} and {!Eio.Flow}. *)
val write_to_disk : Eio.Fs.dir_ty Eio.Path.t -> t -> unit

(** [read_from_disk path] deserialises a snapshot previously written by
    {!write_to_disk}.

    @raise Failure if [path] does not contain exactly one valid
           snapshot. *)
val read_from_disk : Eio.Fs.dir_ty Eio.Path.t -> t

(** [dump_debug t] prints basic statistics about the index to stdout.
    Intended for interactive debugging only. *)
val dump_debug : t -> unit
