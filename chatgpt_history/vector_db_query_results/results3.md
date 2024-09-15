Querying indexed OCaml code with text: **Vector_db.mli**
Using vector database data from folder: **./vector**
Returning top **50** results

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
Location: File "vector_db.ml", line 23, characters 2-70
Module Path: Vector_db.Vec
OCaml Source: Implementation
*)


type vector = Float_array.t [@@deriving hash, compare, bin_io, sexp]
```

**Result 3:**
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
Location: File "vector_db.ml", line 12, characters 0-1303
Module Path: Vector_db
OCaml Source: Implementation
*)


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
  module Io = Bin_prot_utils.With_file_methods (T)

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

**Result 9:**
```ocaml
(** 
Location: File "vector_db.ml", line 25, characters 2-310
Module Path: Vector_db.Vec
OCaml Source: Implementation
*)


module T = struct
    (** this data type holds the vector representation of the underlying document 
      and the id field is the file path location for the doc that the vecctor represents *)
    type t =
      { id : string
      ; vector : vector
      }
    [@@deriving compare, hash, sexp, bin_io]
  end
```

**Result 10:**
```ocaml
(** 
Location: File "vector_db.ml", line 36, characters 2-50
Module Path: Vector_db.Vec
OCaml Source: Implementation
*)


module Io = Bin_prot_utils.With_file_methods (T)
```

**Result 11:**
```ocaml
(** 
Location: File "vector_db.mli", line 21, characters 4-59
Module Path: Vector_db.Vec.Float_array
OCaml Source: Interface
*)


type t = float array [@@deriving compare, bin_io, sexp]
```

**Result 12:**
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

**Result 13:**
```ocaml
(** 
Location: File "vector_db.ml", line 28, characters 4-109
Module Path: Vector_db.Vec.T
OCaml Source: Implementation
*)

(**
 this data type holds the vector representation of the underlying document 
      and the id field is the file path location for the doc that the vecctor represents  *)
type t =
      { id : string
      ; vector : vector
      }
    [@@deriving compare, hash, sexp, bin_io]
```

**Result 14:**
```ocaml
(** 
Location: File "vector_db.ml", line 7, characters 0-69
Module Path: Vector_db
OCaml Source: Implementation
*)

(**
 This represents the vector db. corpus is the matrix of vector representations of the underlying docs. 
  the index is a hash table that maps the index of a doc in the matrix to the file path of the doc   
 *)
type t =
  { corpus : Mat.mat
  ; index : (int, string) Hashtbl.t
  }
```

**Result 15:**
```ocaml
(** 
Location: File "vector_db.ml", line 13, characters 2-261
Module Path: Vector_db.Vec
OCaml Source: Implementation
*)


module Float_array = struct
    type t = float array [@@deriving compare, bin_io, sexp]

    let hash_fold_t hash_state t =
      Array.fold t ~init:hash_state ~f:(fun hs elem -> Float.hash_fold_t hs elem)
    ;;

    let hash = Hash.of_fold hash_fold_t
  end
```

**Result 16:**
```ocaml
(** 
Location: File "vector_db.ml", line 14, characters 4-59
Module Path: Vector_db.Vec.Float_array
OCaml Source: Implementation
*)


type t = float array [@@deriving compare, bin_io, sexp]
```

**Result 17:**
```ocaml
(** 
Location: File "vector_db.mli", line 35, characters 4-56
Module Path: Vector_db.Vec.Io
OCaml Source: Interface
*)


type nonrec t = t [@@deriving compare, bin_io, sexp]
```

**Result 18:**
```ocaml
(** 
Location: File "vector_db.ml", line 51, characters 0-148
Module Path: Vector_db
OCaml Source: Implementation
*)


let normalize doc =
  let vec = Owl.Mat.of_array doc (Array.length doc) 1 in
  let l2norm = Mat.vecnorm' vec in
  Mat.map (fun x -> x /. l2norm) vec
```

**Result 19:**
```ocaml
(** 
Location: File "vector_db.ml", line 87, characters 0-106
Module Path: Vector_db
OCaml Source: Implementation
*)


let add_doc corpus doc =
  Mat.of_cols @@ Array.concat [ Mat.to_cols corpus; Mat.to_cols (normalize doc) ]
```

**Result 20:**
```ocaml
(** 
Location: File "vector_db.ml", line 20, characters 4-39
Module Path: Vector_db.Vec.Float_array
OCaml Source: Implementation
*)


let hash = Hash.of_fold hash_fold_t
```

**Result 21:**
```ocaml
(** 
Location: File "vector_db.mli", line 68, characters 0-55
Module Path: Vector_db
OCaml Source: Interface
*)


val add_doc : Owl.Mat.mat -> float array -> Owl.Mat.mat
```

**Result 22:**
```ocaml
(** 
Location: File "vector_db.ml", line 48, characters 2-75
Module Path: Vector_db.Vec
OCaml Source: Implementation
*)

(**
 Reads an array of vectors from disk using the Io.File module
  @param label The label used as the file name for the input file
  @return The array of vectors read from the file  *)
let read_vectors_from_disk label = Array.of_list (Io.File.read_all label)
```

**Result 23:**
```ocaml
(** 
Location: File "vector_db.ml", line 41, characters 2-96
Module Path: Vector_db.Vec
OCaml Source: Implementation
*)

(**
 Writes an array of vectors to disk using the Io.File module
      @param vectors The array of vectors to be written to disk
      @param label The label used as the file name for the output file  *)
let write_vectors_to_disk vectors label =
    Io.File.write_all label @@ Array.to_list vectors
```

**Result 24:**
```ocaml
(** 
Location: File "vector_db.ml", line 16, characters 4-116
Module Path: Vector_db.Vec.Float_array
OCaml Source: Implementation
*)


let hash_fold_t hash_state t =
      Array.fold t ~init:hash_state ~f:(fun hs elem -> Float.hash_fold_t hs elem)
```

**Result 25:**
```ocaml
(** 
Location: File "chatgpt.mli", line 35, characters 0-91
Module Path: Chatgpt
OCaml Source: Interface
*)


type _ file_type =
  | Mli : mli file_type
  | Ml : ml file_type

and mli = MLI
and ml = ML
```

**Result 26:**
```ocaml
(** 
Location: File "doc.ml", line 2, characters 0-26
Module Path: Doc
OCaml Source: Implementation
*)


let ( / ) = Eio.Path.( / )
```

**Result 27:**
```ocaml
(** 
Location: File "vector_db.ml", line 99, characters 0-92
Module Path: Vector_db
OCaml Source: Implementation
*)

(**
 [initialize file] initializes a vector database by reading in an array of [Vec.t] from disk and creating a corpus.

  The function reads an array of document vectors with their associated file paths from disk using the [Vec.read_vectors_from_disk] function.
  It then creates a corpus and index using the [create_corpus] function.

  @param file is the file name used to read the array of document vectors from disk.
  @return a record of type [t] containing the matrix of vector representations [corpus] and the index mapping document indices to file paths.
 *)
let initialize file =
  let docs' = Vec.read_vectors_from_disk file in
  create_corpus docs'
```

**Result 28:**
```ocaml
(** 
Location: File "chatgpt.mli", line 57, characters 0-132
Module Path: Chatgpt
OCaml Source: Interface
*)

(**
 [module_info] is a record type representing the metadata of an OCaml module,
    combining the interface (mli) and implementation (ml) files.  *)
type module_info =
  { mli_file : mli file_info option
  ; ml_file : ml file_info option
  ; module_path : Eio.Fs.dir Eio.Path.t
  }
```

**Result 29:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 100, characters 2-355
Module Path: Bin_prot_utils.With_file_methods
OCaml Source: Implementation
*)


module File = struct
    let map ~f = map_bin_prot_list (module M) ~f
    let fold ~f = fold_bin_prot_list (module M) ~f
    let iter = iter_bin_prot_list (module M)
    let read_all = read_bin_prot_list (module M)
    let write_all = write_bin_prot_list (module M)
    let read = read_bin_prot (module M)
    let write = write_bin_prot (module M)
  end
```

**Result 30:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 71, characters 2-34
Module Path: Bin_prot_utils.With_file_methods
OCaml Source: Interface
*)


type t = M.t [@@deriving bin_io]
```

**Result 31:**
```ocaml
(** 
Location: File "chatgpt.ml", line 207, characters 0-91
Module Path: Chatgpt
OCaml Source: Implementation
*)


type _ file_type =
  | Mli : mli file_type
  | Ml : ml file_type

and mli = MLI
and ml = ML
```

**Result 32:**
```ocaml
(** 
Location: File "chatgpt.ml", line 229, characters 0-132
Module Path: Chatgpt
OCaml Source: Implementation
*)

(**
 [module_info] is a record type representing the metadata of an OCaml module,
    combining the interface (mli) and implementation (ml) files.  *)
type module_info =
  { mli_file : mli file_info option
  ; ml_file : ml file_info option
  ; module_path : Eio.Fs.dir Eio.Path.t
  }
```

**Result 33:**
```ocaml
(** 
Location: File "vector_db.ml", line 80, characters 0-183
Module Path: Vector_db
OCaml Source: Implementation
*)

(**
 [query t doc k] returns the top [k] most similar documents to the given [doc] in the vector database [t].
    The function computes the cosine similarity between the input [doc] and the documents in the database [t.corpus],
    and returns the indices of the top [k] most similar documents.
    @param t is the vector database containing the corpus and index.
    @param doc is the document vector to be compared with the documents in the database.
    @param k is the number of top similar documents to be returned.
    @return an array of indices corresponding to the top [k] most similar documents in the database.  *)
let query t doc k =
  let get_indexs arr = Array.map ~f:(fun t -> Array.get t 1) arr in
  let vec = Mat.transpose doc in
  let l = Mat.(vec *@ t.corpus) in
  get_indexs @@ Mat.top l k
```

**Result 34:**
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

**Result 35:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 97, characters 0-431
Module Path: Bin_prot_utils
OCaml Source: Implementation
*)


module With_file_methods (M : Bin_prot.Binable.S) = struct
  include M

  module File = struct
    let map ~f = map_bin_prot_list (module M) ~f
    let fold ~f = fold_bin_prot_list (module M) ~f
    let iter = iter_bin_prot_list (module M)
    let read_all = read_bin_prot_list (module M)
    let write_all = write_bin_prot_list (module M)
    let read = read_bin_prot (module M)
    let write = write_bin_prot (module M)
  end
end
```

**Result 36:**
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

**Result 37:**
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

**Result 38:**
```ocaml
(** 
Location: File "vector_db.ml", line 65, characters 0-309
Module Path: Vector_db
OCaml Source: Implementation
*)

(**
 [create_corpus docs'] creates a vector database from an array of document vectors [docs'].

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
```

**Result 39:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 103, characters 4-44
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Implementation
*)


let iter = iter_bin_prot_list (module M)
```

**Result 40:**
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

**Result 41:**
```ocaml
(** 
Location: File "chatgpt.mli", line 43, characters 0-75
Module Path: Chatgpt
OCaml Source: Interface
*)

(**
 Now, the file_type is encoded in the type system, and you can create file_info values with specific file types:

    {[
      let mli_file = mli file_info { file_type = Mli; file_name = "example.mli" }
      let ml_file : ml file_info = { file_type = Ml; file_name = "example.ml" }
    ]}

 [file_info] is a record type that contains the file_type and file_name.  *)
type 'a file_info =
  { file_type : 'a file_type
  ; file_name : string
  }
```

**Result 42:**
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

**Result 43:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 21, characters 0-233
Module Path: Bin_prot_utils
OCaml Source: Implementation
*)


let write_bin_prot' file writer v =
  let f fd =
    let buf = Bin_prot.Utils.bin_dump ~header:true writer v in
    Bigstring_unix.really_write fd buf
  in
  Core_unix.with_file file ~mode:[ Core_unix.O_WRONLY; Core_unix.O_CREAT ] ~f
```

**Result 44:**
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

**Result 45:**
```ocaml
(** 
Location: File "chatgpt.ml", line 151, characters 0-26
Module Path: Chatgpt
OCaml Source: Implementation
*)


let ( / ) = Eio.Path.( / )
```

**Result 46:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 107, characters 4-41
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Implementation
*)


let write = write_bin_prot (module M)
```

**Result 47:**
```ocaml
(** 
Location: File "chatgpt.ml", line 14, characters 0-4343
Module Path: Chatgpt
OCaml Source: Implementation
*)

(**
 This module defines ast traversing functionality  *)
module Traverse = struct
  (** This module defines extract_docs_from_attributes *)
  let extract_docs_from_attributes attrs =
    List.fold_left
      (fun acc attr ->
        match attr.attr_name.txt with
        | "ocaml.doc" | "ocaml.text" ->
          (match attr.attr_payload with
           | PStr
               [ { pstr_desc =
                     Pstr_eval
                       ({ pexp_desc = Pexp_constant (Pconst_string (doc, _, _)); _ }, _)
                 ; _
                 }
               ] -> doc :: acc
           | _ -> acc)
        | _ -> acc)
      []
      attrs
  ;;

  let extract_docs_from_structure_item item =
    match item.pstr_desc with
    | Pstr_value (_, value_bindings) ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.pvb_attributes)
        []
        value_bindings
    | Pstr_primitive des -> extract_docs_from_attributes des.pval_attributes
    | Pstr_type (_, value_bindings) ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.ptype_attributes)
        []
        value_bindings
    | Pstr_typext ext -> extract_docs_from_attributes ext.ptyext_attributes
    | Pstr_module bind -> extract_docs_from_attributes bind.pmb_attributes
    | Pstr_recmodule bindings ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.pmb_attributes)
        []
        bindings
    | Pstr_modtype dec -> extract_docs_from_attributes dec.pmtd_attributes
    | _ -> []
  ;;

  let extract_docs_from_signiture_item item =
    match item.psig_desc with
    | Psig_value v -> extract_docs_from_attributes v.pval_attributes
    | Psig_type (_, value_bindings) ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.ptype_attributes)
        []
        value_bindings
    | Psig_typext ext -> extract_docs_from_attributes ext.ptyext_attributes
    | Psig_module bind -> extract_docs_from_attributes bind.pmd_attributes
    | Psig_recmodule bindings ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.pmd_attributes)
        []
        bindings
    | Psig_modtype dec -> extract_docs_from_attributes dec.pmtd_attributes
    | _ -> []
  ;;

  let s payload =
    let checker =
      object (self)
        inherit
          [string * (string * location * string list) list] Ast_traverse.fold as super

        method! structure items (path, loc) =
          (* Format.printf "%a\n" Astlib.Pprintast.structure  items; *)
          (* print_endline @@ Astlib.Pprintast.string_of_structure items; *)
          let a =
            List.filter_map
              (fun item ->
                match item.pstr_desc with
                | Pstr_attribute _
                | Pstr_extension _
                | Pstr_open _
                | Pstr_include _
                | Pstr_eval _ -> None
                | _ ->
                  (* print_endline "yoyo"; *)
                  (* let res = snd (self#structure_item item (path, [])) in *)
                  (* Format.printf "%a\n" Astlib.Pprintast.structure_item  item; *)
                  Some (snd (self#structure_item item (path, []))))
              items
          in
          path, loc @ List.flatten a

        method! structure_item item (path, loc) =
          (* print_loc item.pstr_loc; *)
          super#structure_item
            item
            (path, (path, item.pstr_loc, extract_docs_from_structure_item item) :: loc)

        method! signature items (path, loc) =
          let a =
            List.map (fun item -> snd (self#signature_item item (path, []))) items
          in
          path, loc @ List.flatten a

        method! signature_item item (path, loc) =
          (* print_loc item.pstr_loc; *)
          super#signature_item
            item
            (path, (path, item.psig_loc, extract_docs_from_signiture_item item) :: loc)

        method! module_binding mb (path, loc) =
          super#module_binding mb (Helpers.enter_opt mb.pmb_name.txt path, loc)

        method! module_declaration md (path, loc) =
          super#module_declaration md (Helpers.enter_opt md.pmd_name.txt path, loc)

        method! module_type_declaration mtd (path, loc) =
          super#module_type_declaration mtd (Helpers.enter mtd.pmtd_name.txt path, loc)
      end
    in
    checker#payload payload
  ;;
end
```

**Result 48:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 9, characters 0-315
Module Path: Bin_prot_utils
OCaml Source: Implementation
*)


let append_bin_list_to_file file writer lst =
  let f fd =
    List.iter lst ~f:(fun v ->
        let buf = Bin_prot.Utils.bin_dump ~header:true writer v in
        Bigstring_unix.really_write fd buf)
  in
  Core_unix.with_file
    file
    ~mode:[ Core_unix.O_APPEND; Core_unix.O_WRONLY; Core_unix.O_CREAT ]
    ~f
```

**Result 49:**
```ocaml
(** 
Location: File "chatgpt.ml", line 153, characters 0-739
Module Path: Chatgpt
OCaml Source: Implementation
*)


let traverse ~doc payload module_name ocaml_source = 
  let _, payload = Traverse.s payload (module_name, []) in
  List.map
    ~f:(fun (path, loc, docs) ->
      let contents =
        String.sub
          doc
          ~pos:loc.loc_start.pos_cnum
          ~len:(loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
      in
      let location t =
        Format.sprintf
          "File \"%s\", line %d, characters %d-%d"
          t.loc_start.pos_fname
          t.loc_start.pos_lnum
          (t.loc_start.pos_cnum - t.loc_start.pos_bol)
          (t.loc_end.pos_cnum - t.loc_start.pos_bol)
      in
      { location = location loc
      ; module_path = path
      ; comments = docs
      ; contents
      ; ocaml_source
      })
    payload
```

**Result 50:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 64, characters 0-130
Module Path: Bin_prot_utils
OCaml Source: Implementation
*)


let write_bin_prot (type a) (module M : Bin_prot.Binable.S with type t = a) file (v : a) =
  write_bin_prot' file M.bin_writer_t v
```
