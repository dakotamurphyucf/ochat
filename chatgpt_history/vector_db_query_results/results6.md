Querying indexed OCaml code with text: **Vector_db functions**
Using vector database data from folder: **./vector-mli**
Returning top **20** results

**Result 1:**
```ocaml
(** 
Location: File "vector_db.mli", line 24, characters 2-64
Module Path: Vector_db.Vec
OCaml Source: Interface
*)


type vector = Float_array.t [@@deriving compare, bin_io, sexp]
```

**Result 2:**
```ocaml
(** 
Location: File "vector_db.mli", line 1, characters 0-667
Module Path: Vector_db
OCaml Source: Interface
*)


(** Vector Database

    This module provides functionality for creating and querying a vector database, which is a collection of document vectors and their associated file paths. The database is represented as a matrix of vector representations and an index that maps the index of a document in the matrix to the file path of the document.

    The main data type is [t], which represents the vector database. The module also provides functions for creating a corpus, querying the database, and managing document vectors.

    The [Vec] module defines the vector representation of documents and provides functions for reading and writing vectors to and from disk.
*)
```

**Result 3:**
```ocaml
(** 
Location: File "vector_db.mli", line 19, characters 0-1081
Module Path: Vector_db
OCaml Source: Interface
*)


module Vec : sig
  module Float_array : sig
    type t = float array [@@deriving compare, bin_io, sexp]
  end

  type vector = Float_array.t [@@deriving compare, bin_io, sexp]

  (** this data type holds the vector representation of the underlying document 
    and the id field is the file path location for the doc that the vecctor represents *)
  type t =
    { id : string
    ; vector : vector
    }
  [@@deriving compare, bin_io, sexp]

  module Io : module type of Bin_prot_utils.With_file_methods (struct
    type nonrec t = t [@@deriving compare, bin_io, sexp]
  end)

  (** Writes an array of vectors to disk using the Io.File module
      @param vectors The array of vectors to be written to disk
      @param label The label used as the file name for the output file *)
  val write_vectors_to_disk : t array -> string -> unit

  (** Reads an array of vectors from disk using the Io.File module
    @param label The label used as the file name for the input file
    @return The array of vectors read from the file *)
  val read_vectors_from_disk : string -> t array
end
```

**Result 4:**
```ocaml
(** 
Location: File "vector_db.mli", line 28, characters 2-93
Module Path: Vector_db.Vec
OCaml Source: Interface
*)

(**
 this data type holds the vector representation of the underlying document 
    and the id field is the file path location for the doc that the vecctor represents  *)
type t =
    { id : string
    ; vector : vector
    }
  [@@deriving compare, bin_io, sexp]
```

**Result 5:**
```ocaml
(** 
Location: File "vector_db.mli", line 20, characters 2-92
Module Path: Vector_db.Vec
OCaml Source: Interface
*)


module Float_array : sig
    type t = float array [@@deriving compare, bin_io, sexp]
  end
```

**Result 6:**
```ocaml
(** 
Location: File "vector_db.mli", line 21, characters 4-59
Module Path: Vector_db.Vec.Float_array
OCaml Source: Interface
*)


type t = float array [@@deriving compare, bin_io, sexp]
```

**Result 7:**
```ocaml
(** 
Location: File "vector_db.mli", line 14, characters 0-78
Module Path: Vector_db
OCaml Source: Interface
*)

(**
 
  This represents the vector db. corpus is the matrix of vector representations of the underlying docs. 
  the index is a hash table that maps the index of a doc in the matrix to the file path of the doc   
 *)
type t =
  { corpus : Owl.Mat.mat
  ; index : (int, string) Core.Hashtbl.t
  }
```

**Result 8:**
```ocaml
(** 
Location: File "vector_db.mli", line 34, characters 2-133
Module Path: Vector_db.Vec
OCaml Source: Interface
*)


module Io : module type of Bin_prot_utils.With_file_methods (struct
    type nonrec t = t [@@deriving compare, bin_io, sexp]
  end)
```

**Result 9:**
```ocaml
(** 
Location: File "vector_db.mli", line 35, characters 4-56
Module Path: Vector_db.Vec.Io
OCaml Source: Interface
*)


type nonrec t = t [@@deriving compare, bin_io, sexp]
```

**Result 10:**
```ocaml
(** 
Location: File "vector_db.mli", line 68, characters 0-55
Module Path: Vector_db
OCaml Source: Interface
*)


val add_doc : Owl.Mat.mat -> float array -> Owl.Mat.mat
```

**Result 11:**
```ocaml
(** 
Location: File "vector_db.mli", line 66, characters 0-48
Module Path: Vector_db
OCaml Source: Interface
*)

(**
 [query t doc k] returns the top [k] most similar documents to the given [doc] in the vector database [t].
    The function computes the cosine similarity between the input [doc] and the documents in the database [t.corpus],
    and returns the indices of the top [k] most similar documents.
    @param t is the vector database containing the corpus and index.
    @param doc is the document vector to be compared with the documents in the database.
    @param k is the number of top similar documents to be returned.
    @return an array of indices corresponding to the top [k] most similar documents in the database.  *)
val query : t -> Owl.Mat.mat -> int -> int array
```

**Result 12:**
```ocaml
(** 
Location: File "vector_db.mli", line 41, characters 2-55
Module Path: Vector_db.Vec
OCaml Source: Interface
*)

(**
 Writes an array of vectors to disk using the Io.File module
      @param vectors The array of vectors to be written to disk
      @param label The label used as the file name for the output file  *)
val write_vectors_to_disk : t array -> string -> unit
```

**Result 13:**
```ocaml
(** 
Location: File "vector_db.mli", line 78, characters 0-28
Module Path: Vector_db
OCaml Source: Interface
*)

(**
 [initialize file] initializes a vector database by reading in an array of [Vec.t] from disk and creating a corpus.

  The function reads an array of document vectors with their associated file paths from disk using the [Vec.read_vectors_from_disk] function.
  It then creates a corpus and index using the [create_corpus] function.

  @param file is the file name used to read the array of document vectors from disk.
  @return a record of type [t] containing the matrix of vector representations [corpus] and the index mapping document indices to file paths.
 *)
val initialize : string -> t
```

**Result 14:**
```ocaml
(** 
Location: File "vector_db.mli", line 46, characters 2-48
Module Path: Vector_db.Vec
OCaml Source: Interface
*)

(**
 Reads an array of vectors from disk using the Io.File module
    @param label The label used as the file name for the input file
    @return The array of vectors read from the file  *)
val read_vectors_from_disk : string -> t array
```

**Result 15:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 70, characters 0-1335
Module Path: Bin_prot_utils
OCaml Source: Interface
*)


module With_file_methods : functor (M : Bin_prot.Binable.S) -> sig
  type t = M.t [@@deriving bin_io]

  module File : sig
    (** [map ~f filename] maps the function [f] over the binary data in the file [filename] using the provided [M]. *)
    val map : f:(t -> 'a) -> string -> 'a list

    (** [fold ~f filename ~init] folds the function [f] over the binary data in the file [filename] using the provided [M], starting with the initial value [init]. *)
    val fold : f:('a -> t -> 'a) -> string -> init:'a -> 'a

    (** [iter filename ~f] iterates the function [f] over the binary data in the file [filename] using the provided [M]. *)
    val iter : string -> f:(t -> unit) -> unit

    (** [read_all filename] reads a list of binary values from the file [filename] using the provided [M]. *)
    val read_all : string -> t list

    (** [write_all filename data] writes the binary representation of a list of [data] to the file [filename] using the provided [M]. *)
    val write_all : string -> t list -> unit

    (** [read filename] reads the binary representation of a value from the file [filename] using the provided [M]. *)
    val read : string -> t

    (** [write filename data] writes the binary representation of [data] to the file [filename] using the provided [M]. *)
    val write : string -> t -> unit
  end
end
```

**Result 16:**
```ocaml
(** 
Location: File "openai.mli", line 14, characters 0-267
Module Path: Openai
OCaml Source: Interface
*)

(**
 Type definition for the response from the embeddings API. 
 Type definition for an individual embedding in the response.  *)
type response = { data : embedding list } [@@jsonaf.allow_extra_fields]

(** Type definition for an individual embedding in the response. *)
and embedding =
  { embedding : float list
  ; index : int
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]
```

**Result 17:**
```ocaml
(** 
Location: File "doc.mli", line 1, characters 0-254
Module Path: Doc
OCaml Source: Interface
*)


(** Document
    The [hash_string_md5] function creates a unique hash encoding of an input string using Core.Md5.

    The [save_prompt] and [load_prompt] functions are used for saving and loading the underlying document for a vector to and from disk. *)
```

**Result 18:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 73, characters 2-1228
Module Path: Bin_prot_utils.With_file_methods
OCaml Source: Interface
*)


module File : sig
    (** [map ~f filename] maps the function [f] over the binary data in the file [filename] using the provided [M]. *)
    val map : f:(t -> 'a) -> string -> 'a list

    (** [fold ~f filename ~init] folds the function [f] over the binary data in the file [filename] using the provided [M], starting with the initial value [init]. *)
    val fold : f:('a -> t -> 'a) -> string -> init:'a -> 'a

    (** [iter filename ~f] iterates the function [f] over the binary data in the file [filename] using the provided [M]. *)
    val iter : string -> f:(t -> unit) -> unit

    (** [read_all filename] reads a list of binary values from the file [filename] using the provided [M]. *)
    val read_all : string -> t list

    (** [write_all filename data] writes the binary representation of a list of [data] to the file [filename] using the provided [M]. *)
    val write_all : string -> t list -> unit

    (** [read filename] reads the binary representation of a value from the file [filename] using the provided [M]. *)
    val read : string -> t

    (** [write filename data] writes the binary representation of [data] to the file [filename] using the provided [M]. *)
    val write : string -> t -> unit
  end
```

**Result 19:**
```ocaml
(** 
Location: File "vector_db.mli", line 57, characters 0-36
Module Path: Vector_db
OCaml Source: Interface
*)

(**
 [create_corpus docs'] creates a vector database from an array of document vectors [docs'].

  The function normalizes each document vector and constructs a matrix [corpus] where each column represents a normalized document vector.
  It also creates an index, which is a hash table that maps the index of a document in the matrix to the file path of the document.

  @param docs' is an array of document vectors with their associated file paths.
  @return a record of type [t] containing the matrix of vector representations [corpus] and the index mapping document indices to file paths.
 *)
val create_corpus : Vec.t array -> t
```

**Result 20:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 71, characters 2-34
Module Path: Bin_prot_utils.With_file_methods
OCaml Source: Interface
*)


type t = M.t [@@deriving bin_io]
```
