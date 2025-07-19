(** Vector Database

    This module provides functionality for creating and querying a vector database, which is a collection of document vectors and their associated file paths. The database is represented as a matrix of vector representations and an index that maps the index of a document in the matrix to the file path of the document.

    The main data type is [t], which represents the vector database. The module also provides functions for creating a corpus, querying the database, and managing document vectors.

    The [Vec] module defines the vector representation of documents and provides functions for reading and writing vectors to and from disk. *)

(** This represents the vector db. corpus is the matrix of vector representations of the underlying docs.
    the index is a hash table that maps the index of a doc in the matrix to the file path of the doc *)
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

  (** this data type holds the vector representation of the underlying document
      and the id field is the file path location for the doc that the vecctor represents *)
  type t =
    { id : string
    ; len : int
    ; vector : vector
    }
  [@@deriving compare, bin_io, sexp]

  module Io : module type of Bin_prot_utils_eio.With_file_methods (struct
      type nonrec t = t [@@deriving compare, bin_io, sexp]
    end)

  (** Writes an array of vectors to disk using the Io.File module
      @param vectors The array of vectors to be written to disk
      @param label The label used as the file name for the output file *)
  val write_vectors_to_disk : t array -> path -> unit

  (** Reads an array of vectors from disk using the Io.File module
      @param label The label used as the file name for the input file
      @return The array of vectors read from the file *)
  val read_vectors_from_disk : path -> t array
end

(** [create_corpus docs'] creates a vector database from an array of document vectors [docs'].

    The function normalizes each document vector and constructs a matrix [corpus] where each column represents a normalized document vector.
    It also creates an index, which is a hash table that maps the index of a document in the matrix to the file path of the document.

    @param docs' is an array of document vectors with their associated file paths.
    @return
      a record of type [t] containing the matrix of vector representations [corpus] and the index mapping document indices to file paths. *)
val create_corpus : Vec.t array -> t

(** [query t doc k] returns the top [k] most similar documents to the given [doc] in the vector database [t].
    The function computes the cosine similarity between the input [doc] and the documents in the database [t.corpus],
    and returns the indices of the top [k] most similar documents.
    @param t is the vector database containing the corpus and index.
    @param doc is the document vector to be compared with the documents in the database.
    @param k is the number of top similar documents to be returned.
    @return
      an array of indices corresponding to the top [k] most similar documents in the database. *)
val query : t -> Owl.Mat.mat -> int -> int array

(** Hybrid retrieval: fuse cosine similarity with BM25.  [beta] ∈ [0,1]
    controls how much weight is given to BM25 (0 = vector only, 1 = bm25 only). *)
val query_hybrid :
  t ->
  bm25:Bm25.t ->
  beta:float ->
  embedding:Owl.Mat.mat ->
  text:string ->
  k:int -> int array

val add_doc : Owl.Mat.mat -> float array -> Owl.Mat.mat

(** [initialize file] initializes a vector database by reading in an array of [Vec.t] from disk and creating a corpus.

    The function reads an array of document vectors with their associated file paths from disk using the [Vec.read_vectors_from_disk] function.
    It then creates a corpus and index using the [create_corpus] function.

    @param file is the file name used to read the array of document vectors from disk.
    @return
      a record of type [t] containing the matrix of vector representations [corpus] and the index mapping document indices to file paths. *)
val initialize : path -> t

(** [get_docs dir t indexs] reads the documents corresponding to the given indices from disk and returns an array of their contents.

    The function retrieves the file paths of the documents using the index hash table in [t] and reads the contents of the documents from disk using the [Doc.load_prompt] function.
    @param [dir] is the directory for loading the documents.
    @param t is the vector database containing the corpus and index.
    @param indexs
      is an array of indices corresponding to the documents to be read from disk.
    @return an list of strings containing the contents of the documents read from disk. *)
val get_docs : Eio.Fs.dir_ty Eio.Path.t -> t -> int array -> string list
