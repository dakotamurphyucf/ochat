open Core
open Owl

(** This represents the vector db. corpus is the matrix of vector representations of the underlying docs. 
  the index is a hash table that maps the index of a doc in the matrix to the file path of the doc   
*)
type t =
  { corpus : Mat.mat
  ; index : (int, string) Hashtbl.t
  }

type path = Eio.Fs.dir_ty Eio.Path.t

module Vec = struct
  module Float_array = struct
    type t = float array [@@deriving compare, bin_io, sexp]

    let hash_fold_t hash_state t =
      Array.fold t ~init:hash_state ~f:(fun hs elem -> Float.hash_fold_t hs elem)
    ;;

    let hash = Hash.of_fold hash_fold_t
  end

  type vector = Float_array.t [@@deriving hash, compare, bin_io, sexp]

  module T = struct
    (** this data type holds the vector representation of the underlying document 
      and the id field is the file path location for the doc that the vecctor represents *)
    type t =
      { id : string
      ; vector : vector
      }
    [@@deriving compare, hash, sexp, bin_io]
  end

  include T
  module Io = Bin_prot_utils_eio.With_file_methods (T)

  (** Writes an array of vectors to disk using the Io.File module
      @param vectors The array of vectors to be written to disk
      @param label The label used as the file name for the output file *)
  let write_vectors_to_disk vectors label =
    Io.File.write_all label @@ Array.to_list vectors
  ;;

  (** Reads an array of vectors from disk using the Io.File module
  @param label The label used as the file name for the input file
  @return The array of vectors read from the file *)
  let read_vectors_from_disk label = Array.of_list (Io.File.read_all label)
end

let normalize doc =
  let vec = Owl.Mat.of_array doc (Array.length doc) 1 in
  let l2norm = Mat.vecnorm' vec in
  Mat.map (fun x -> x /. l2norm) vec
;;

(** [create_corpus docs'] creates a vector database from an array of document vectors [docs'].

  The function normalizes each document vector and constructs a matrix [corpus] where each column represents a normalized document vector.
  It also creates an index, which is a hash table that maps the index of a document in the matrix to the file path of the document.

  @param docs' is an array of document vectors with their associated file paths.
  @return a record of type [t] containing the matrix of vector representations [corpus] and the index mapping document indices to file paths.
*)
let create_corpus docs' =
  let docs = Array.map ~f:(fun doc -> normalize doc.Vec.vector) docs' in
  let corpus = Mat.of_cols docs in
  let index = Hashtbl.create (module Int) ~size:(Array.length docs) in
  Array.iteri ~f:(fun i doc -> Hashtbl.add_exn index ~key:i ~data:doc.Vec.id) docs';
  { corpus; index }
;;

(** [query t doc k] returns the top [k] most similar documents to the given [doc] in the vector database [t].
    The function computes the cosine similarity between the input [doc] and the documents in the database [t.corpus],
    and returns the indices of the top [k] most similar documents.
    @param t is the vector database containing the corpus and index.
    @param doc is the document vector to be compared with the documents in the database.
    @param k is the number of top similar documents to be returned.
    @return an array of indices corresponding to the top [k] most similar documents in the database. *)
let query t doc k =
  let get_indexs arr = Array.map ~f:(fun t -> Array.get t 1) arr in
  let vec = Mat.transpose doc in
  let l = Mat.(vec *@ t.corpus) in
  get_indexs @@ Mat.top l k
;;

let add_doc corpus doc =
  Mat.of_cols @@ Array.concat [ Mat.to_cols corpus; Mat.to_cols (normalize doc) ]
;;

(** [initialize file] initializes a vector database by reading in an array of [Vec.t] from disk and creating a corpus.

  The function reads an array of document vectors with their associated file paths from disk using the [Vec.read_vectors_from_disk] function.
  It then creates a corpus and index using the [create_corpus] function.

  @param file is the file name used to read the array of document vectors from disk.
  @return a record of type [t] containing the matrix of vector representations [corpus] and the index mapping document indices to file paths.
*)
let initialize file =
  let docs' = Vec.read_vectors_from_disk file in
  create_corpus docs'
;;

(** [get_docs dir t indexs] reads the documents corresponding to the given indices from disk and returns an array of their contents.

  The function retrieves the file paths of the documents using the index hash table in [t] and reads the contents of the documents from disk using the [Doc.load_prompt] function.

  @param dir is the environment used for loading the documents.
  @param t is the vector database containing the corpus and index.
  @param indexs is an array of indices corresponding to the documents to be read from disk.
  @return an array of strings containing the contents of the documents read from disk.
*)
let get_docs dir t indexs =
  Eio.Fiber.List.map
    (fun idx ->
       let file_path = Hashtbl.find_exn t.index idx in
       Io.load_doc ~dir file_path)
    (Array.to_list indexs)
;;
