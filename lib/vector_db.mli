(** Dense vector search utilities.

    A {b vector database} maps small pieces of text (code snippets, README
    paragraphs, commit messages, …) to dense float embeddings and offers
    fast similarity search.  This implementation keeps the whole corpus
    in memory – every document embedding is stored as a {e column} of an
    Owl matrix – and therefore targets small-to-medium corpora (up to a
    few hundred thousand fragments on a typical developer machine).

    {1 Data model}

    •  [t] – immutable snapshot: a normalised embedding matrix
       {!field:corpus} together with the reverse lookup table
       {!field:index} that maps matrix columns back to the on-disk
       document id and its token length.

    •  {!module:Vec} – serialisable record bundling a document’s
       identifier, token length and raw float-array embedding.

    {1 Supported operations}

    •  Build a snapshot from an array of {!Vec.t}
      ({!val:create_corpus}).

    •  Retrieve the [k] most similar documents for a given embedding via
      cosine similarity ({!val:query}).

    •  Hybrid retrieval that linearly interpolates BM25 with vector
      similarity ({!val:query_hybrid}).

    •  Lazy loading: read an existing snapshot from disk ({!val:initialize})
      and resolve the matching document bodies ({!val:get_docs}).

    All public functions execute in the caller’s fibre; blocking I/O is
    performed through Eio’s non-blocking API. *)

(** In-memory snapshot of an embedding corpus.

    [corpus] is an `n × m` Owl matrix whose columns are length-1 (L2
    normalised) embeddings.  [index] maps a column number back to the
    original document identifier and its token length (used for length
    penalties and for fetching the document from disk). *)
type t =
  { corpus : Owl.Mat.mat
  ; index : (int, string * int) Core.Hashtbl.t
  }

type path = Eio.Fs.dir_ty Eio.Path.t

module Vec : sig
  module Float_array : sig
    type t = float array [@@deriving compare, bin_io, sexp]
  end

  type vector = Float_array.t [@@deriving compare, bin_io, sexp]

  (** Serialisable embedding record.

      •  [id]   – hashed file path (used as stable identifier on disk)
      •  [len]  – token length of the text snippet (used for length
        penalties)
      •  [vector] – raw, {b unnormalised} float embedding obtained from
        the encoder.  The caller {e need not} normalise – it is done by
        {!create_corpus}. *)
  type t =
    { id : string
    ; len : int
    ; vector : vector
    }
  [@@deriving compare, bin_io, sexp]

  module Io : module type of Bin_prot_utils_eio.With_file_methods (struct
      type nonrec t = t [@@deriving compare, bin_io, sexp]
    end)

  (** [write_vectors_to_disk vecs path] writes [vecs] to [path] using
      {!Bin_prot}.  The file is created (or truncated) with mode
      `0o600`.

      The function runs in the calling fibre and therefore must not be
      used inside a preemptively blocking context. *)
  val write_vectors_to_disk : t array -> path -> unit

  (** [read_vectors_from_disk path] deserialises an array previously
      written by {!write_vectors_to_disk}. *)
  val read_vectors_from_disk : path -> t array
end

(** [create_corpus docs] builds a new snapshot from raw embeddings.

    Each element of [docs] is L2-normalised and appended as a column of
    the resulting matrix; the original float array is not mutated.  The
    function guarantees:

    •  every column of the returned [corpus] has unit L2-norm;
    •  [Hashtbl.length index = Array.length docs].

    The operation is O(n·d) where n is the number of documents and d is
    the embedding dimension. *)
val create_corpus : Vec.t array -> t

(** [query t embedding k] returns the indices of the [k] neighbours that
    maximise cosine similarity to [embedding].

    [embedding] must already be L2-normalised and shaped as an
    n&nbsp;×&nbsp;1 Owl matrix where n equals the embedding dimension of
    the corpus.  If [k] exceeds the corpus size the result is clamped.

    The function allocates O(m) where m = corpus size.  It performs no
    heap allocations proportional to the embedding dimension. *)
val query : t -> Owl.Mat.mat -> int -> int array

(** [query_hybrid t ~bm25 ~beta ~embedding ~text ~k] combines dense and
    lexical search.

    1.   Cosine similarities between [embedding] and the whole corpus
         are computed.
    2.   The top 20·k candidates form a shortlist.
    3.   BM25 scores for [text] are evaluated on the same shortlist.
    4.   Final score = (1&nbsp;−&nbsp;β)·cos  +  β·normalised&nbsp;BM25
         and the best [k] hits are returned.

    [beta] ∈ [0, 1] controls the trade-off (0 = vector-only,
    1 = BM25-only). *)
val query_hybrid
  :  t
  -> bm25:Bm25.t
  -> beta:float
  -> embedding:Owl.Mat.mat
  -> text:string
  -> k:int
  -> int array

(** [add_doc corpus vec] returns [corpus] with [vec] appended as the
    last column.

    [vec] is L2-normalised on the fly; the original array is not
    modified.  The function is a convenience helper for incremental
    updates – it does {b not} update any {!field:index} mapping.  Callers
    are responsible for persisting the extended mapping themselves. *)
val add_doc : Owl.Mat.mat -> float array -> Owl.Mat.mat

(** [initialize path] reads a previously serialised array of {!Vec.t}
    from [path] and builds a snapshot via {!create_corpus}. *)
val initialize : path -> t

(** [get_docs dir t idxs] loads the bodies of the documents with indices
    [idxs].

    For every index the associated file path is looked up in
    [t.index] and the file is read with {!Io.load_doc}.  The result list
    preserves the order of [idxs]. *)
val get_docs : Eio.Fs.dir_ty Eio.Path.t -> t -> int array -> string list
