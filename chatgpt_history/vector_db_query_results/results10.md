Querying indexed OCaml code with text: **line 71, characters 2-34**
Using vector database data from folder: **./vector-mli**
Returning top **20** results

**Result 1:**
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

**Result 2:**
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

**Result 3:**
```ocaml
(** 
Location: File "chatgpt.mli", line 9, characters 0-151
Module Path: Chatgpt
OCaml Source: Interface
*)


type parse_result =
  { location : string
  ; module_path : string
  ; comments : string list
  ; contents : string
  ; ocaml_source : ocaml_source
  }
```

**Result 4:**
```ocaml
(** 
Location: File "chatgpt.mli", line 5, characters 0-52
Module Path: Chatgpt
OCaml Source: Interface
*)


type ocaml_source =
  | Interface
  | Implementation
```

**Result 5:**
```ocaml
(** 
Location: File "vector_db.mli", line 35, characters 4-56
Module Path: Vector_db.Vec.Io
OCaml Source: Interface
*)


type nonrec t = t [@@deriving compare, bin_io, sexp]
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

**Result 8:**
```ocaml
(** 
Location: File "vector_db.mli", line 24, characters 2-64
Module Path: Vector_db.Vec
OCaml Source: Interface
*)


type vector = Float_array.t [@@deriving compare, bin_io, sexp]
```

**Result 9:**
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

**Result 10:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 71, characters 2-34
Module Path: Bin_prot_utils.With_file_methods
OCaml Source: Interface
*)


type t = M.t [@@deriving bin_io]
```

**Result 11:**
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

**Result 12:**
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

**Result 13:**
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

**Result 14:**
```ocaml
(** 
Location: File "doc.mli", line 6, characters 0-67
Module Path: Doc
OCaml Source: Interface
*)


val save_prompt : Eio.Fs.dir Eio.Path.t -> string -> string -> unit
```

**Result 15:**
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

**Result 16:**
```ocaml
(** 
Location: File "doc.mli", line 7, characters 0-59
Module Path: Doc
OCaml Source: Interface
*)


val load_prompt : Eio.Fs.dir Eio.Path.t -> string -> string
```

**Result 17:**
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

**Result 18:**
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

**Result 19:**
```ocaml
(** 
Location: File "chatgpt.mli", line 1, characters 0-281
Module Path: Chatgpt
OCaml Source: Interface
*)


(** The [file_type] and [file_info] types are used to represent OCaml source files, 
    and the [module_info] type represents the metadata of an OCaml module. 
    The [collect_ocaml_files] function is used to collect OCaml source files from a directory and its subdirectories. *)
```

**Result 20:**
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
Querying indexed OCaml code with text: **snipets of ocaml with line 71, characters 2-34 in the comments**
Using vector database data from folder: **./vector-mli**
Returning top **20** results

**Result 1:**
```ocaml
(** 
Location: File "chatgpt.mli", line 9, characters 0-151
Module Path: Chatgpt
OCaml Source: Interface
*)


type parse_result =
  { location : string
  ; module_path : string
  ; comments : string list
  ; contents : string
  ; ocaml_source : ocaml_source
  }
```

**Result 2:**
```ocaml
(** 
Location: File "chatgpt.mli", line 5, characters 0-52
Module Path: Chatgpt
OCaml Source: Interface
*)


type ocaml_source =
  | Interface
  | Implementation
```

**Result 3:**
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

**Result 4:**
```ocaml
(** 
Location: File "vector_db.mli", line 35, characters 4-56
Module Path: Vector_db.Vec.Io
OCaml Source: Interface
*)


type nonrec t = t [@@deriving compare, bin_io, sexp]
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

**Result 7:**
```ocaml
(** 
Location: File "vector_db.mli", line 21, characters 4-59
Module Path: Vector_db.Vec.Float_array
OCaml Source: Interface
*)


type t = float array [@@deriving compare, bin_io, sexp]
```

**Result 8:**
```ocaml
(** 
Location: File "chatgpt.mli", line 1, characters 0-281
Module Path: Chatgpt
OCaml Source: Interface
*)


(** The [file_type] and [file_info] types are used to represent OCaml source files, 
    and the [module_info] type represents the metadata of an OCaml module. 
    The [collect_ocaml_files] function is used to collect OCaml source files from a directory and its subdirectories. *)
```

**Result 9:**
```ocaml
(** 
Location: File "vector_db.mli", line 24, characters 2-64
Module Path: Vector_db.Vec
OCaml Source: Interface
*)


type vector = Float_array.t [@@deriving compare, bin_io, sexp]
```

**Result 10:**
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

**Result 11:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 71, characters 2-34
Module Path: Bin_prot_utils.With_file_methods
OCaml Source: Interface
*)


type t = M.t [@@deriving bin_io]
```

**Result 12:**
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

**Result 13:**
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

**Result 14:**
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

**Result 15:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 1, characters 0-110
Module Path: Bin_prot_utils
OCaml Source: Interface
*)


(** This module provides utility functions for reading and writing binary files using the Bin_prot library. *)
```

**Result 16:**
```ocaml
(** 
Location: File "doc.mli", line 6, characters 0-67
Module Path: Doc
OCaml Source: Interface
*)


val save_prompt : Eio.Fs.dir Eio.Path.t -> string -> string -> unit
```

**Result 17:**
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

**Result 18:**
```ocaml
(** 
Location: File "openai.mli", line 7, characters 0-104
Module Path: Openai
OCaml Source: Interface
*)

(**
 Type definition for the input to the embeddings API.  *)
type embeddings_input =
  { model : string
  ; input : string list
  }
[@@deriving jsonaf, sexp, bin_io]
```

**Result 19:**
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

**Result 20:**
```ocaml
(** 
Location: File "doc.mli", line 7, characters 0-59
Module Path: Doc
OCaml Source: Interface
*)


val load_prompt : Eio.Fs.dir Eio.Path.t -> string -> string
```
Querying indexed OCaml code with text: **(** 
Location: File bin_prot_utils.mli, line 71, characters 2-34
Module Path: Bin_prot_utils.With_file_methods
OCaml Source: Interface
*)**
Using vector database data from folder: **./vector-mli**
Returning top **20** results

**Result 1:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 71, characters 2-34
Module Path: Bin_prot_utils.With_file_methods
OCaml Source: Interface
*)


type t = M.t [@@deriving bin_io]
```

**Result 2:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 1, characters 0-110
Module Path: Bin_prot_utils
OCaml Source: Interface
*)


(** This module provides utility functions for reading and writing binary files using the Bin_prot library. *)
```

**Result 3:**
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

**Result 4:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 90, characters 4-26
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Interface
*)

(**
 [read filename] reads the binary representation of a value from the file [filename] using the provided [M].  *)
val read : string -> t
```

**Result 5:**
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

**Result 6:**
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

**Result 7:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 75, characters 4-46
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Interface
*)

(**
 [map ~f filename] maps the function [f] over the binary data in the file [filename] using the provided [M].  *)
val map : f:(t -> 'a) -> string -> 'a list
```

**Result 8:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 84, characters 4-35
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Interface
*)

(**
 [read_all filename] reads a list of binary values from the file [filename] using the provided [M].  *)
val read_all : string -> t list
```

**Result 9:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 81, characters 4-46
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Interface
*)

(**
 [iter filename ~f] iterates the function [f] over the binary data in the file [filename] using the provided [M].  *)
val iter : string -> f:(t -> unit) -> unit
```

**Result 10:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 93, characters 4-35
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Interface
*)

(**
 [write filename data] writes the binary representation of [data] to the file [filename] using the provided [M].  *)
val write : string -> t -> unit
```

**Result 11:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 78, characters 4-59
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Interface
*)

(**
 [fold ~f filename ~init] folds the function [f] over the binary data in the file [filename] using the provided [M], starting with the initial value [init].  *)
val fold : f:('a -> t -> 'a) -> string -> init:'a -> 'a
```

**Result 12:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 87, characters 4-44
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Interface
*)

(**
 [write_all filename data] writes the binary representation of a list of [data] to the file [filename] using the provided [M].  *)
val write_all : string -> t list -> unit
```

**Result 13:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 36, characters 0-80
Module Path: Bin_prot_utils
OCaml Source: Interface
*)

(**
 [read_bin_prot module filename] reads the binary representation of a value from the file [filename] using the provided [module].  *)
val read_bin_prot : (module Bin_prot.Binable.S with type t = 'a) -> string -> 'a
```

**Result 14:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 33, characters 0-89
Module Path: Bin_prot_utils
OCaml Source: Interface
*)

(**
 [write_bin_prot module filename data] writes the binary representation of [data] to the file [filename] using the provided [module].  *)
val write_bin_prot : (module Bin_prot.Binable.S with type t = 'a) -> string -> 'a -> unit
```

**Result 15:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 46, characters 0-90
Module Path: Bin_prot_utils
OCaml Source: Interface
*)

(**
 [read_bin_prot_list module filename] reads a list of binary values from the file [filename] using the provided [module].  *)
val read_bin_prot_list : (module Bin_prot.Binable.S with type t = 'a) -> string -> 'a list
```

**Result 16:**
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

**Result 17:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 64, characters 0-114
Module Path: Bin_prot_utils
OCaml Source: Interface
*)

(**
 [map_bin_prot_list module filename ~f] maps the function [f] over the binary data in the file [filename] using the provided [module].  *)
val map_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> string
  -> f:('a -> 'b)
  -> 'b list
```

**Result 18:**
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

**Result 19:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 39, characters 0-108
Module Path: Bin_prot_utils
OCaml Source: Interface
*)

(**
 [write_bin_prot_list module filename data] writes the binary representation of a list of [data] to the file [filename] using the provided [module].  *)
val write_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> string
  -> 'a list
  -> unit
```

**Result 20:**
```ocaml
(** 
Location: File "bin_prot_utils.mli", line 49, characters 0-114
Module Path: Bin_prot_utils
OCaml Source: Interface
*)

(**
 [iter_bin_prot_list module filename ~f] iterates the function [f] over the binary data in the file [filename] using the provided [module].  *)
val iter_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> string
  -> f:('a -> unit)
  -> unit
```
