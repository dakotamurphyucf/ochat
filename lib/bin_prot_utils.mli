(** This module provides utility functions for reading and writing binary files using the Bin_prot library. *)

(** [grow_buffer buffer ~new_size_request] returns a new buffer with the requested size. *)
val grow_buffer : Core.Bigstring.t -> new_size_request:int -> Core.Bigstring.t

(** [append_bin_list_to_file filename writer data] appends the binary representation of [data] to the file [filename] using the provided [writer]. *)
val append_bin_list_to_file : string -> 'a Bin_prot.Type_class.writer -> 'a list -> unit

(** [write_bin_prot' filename writer data] writes the binary representation of [data] to the file [filename] using the provided [writer]. *)
val write_bin_prot' : string -> 'a Bin_prot.Type_class.writer -> 'a -> unit

(** [read_bin_prot' filename reader] reads the binary representation of a value from the file [filename] using the provided [reader]. *)
val read_bin_prot' : string -> 'a Bin_prot.Type_class.reader -> 'a

(** [fold_bin_file_list filename reader ~init ~f] folds the function [f] over the binary data in the file [filename] using the provided [reader], starting with the initial value [init]. *)
val fold_bin_file_list
  :  string
  -> 'a Bin_prot.Type_class.reader
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> 'b

(** [read_bin_file_list filename reader] reads a list of binary values from the file [filename] using the provided [reader]. *)
val read_bin_file_list : string -> 'a Bin_prot.Type_class.reader -> 'a list

(** [iter_bin_file_list ~f filename reader] iterates the function [f] over the binary data in the file [filename] using the provided [reader]. *)
val iter_bin_file_list : f:('a -> unit) -> string -> 'a Bin_prot.Type_class.reader -> unit

(** [map_bin_file_list ~f filename reader] maps the function [f] over the binary data in the file [filename] using the provided [reader]. *)
val map_bin_file_list : f:('a -> 'b) -> string -> 'a Bin_prot.Type_class.reader -> 'b list

(** [write_bin_prot module filename data] writes the binary representation of [data] to the file [filename] using the provided [module]. *)
val write_bin_prot : (module Bin_prot.Binable.S with type t = 'a) -> string -> 'a -> unit

(** [read_bin_prot module filename] reads the binary representation of a value from the file [filename] using the provided [module]. *)
val read_bin_prot : (module Bin_prot.Binable.S with type t = 'a) -> string -> 'a

(** [write_bin_prot_list module filename data] writes the binary representation of a list of [data] to the file [filename] using the provided [module]. *)
val write_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> string
  -> 'a list
  -> unit

(** [read_bin_prot_list module filename] reads a list of binary values from the file [filename] using the provided [module]. *)
val read_bin_prot_list : (module Bin_prot.Binable.S with type t = 'a) -> string -> 'a list

(** [iter_bin_prot_list module filename ~f] iterates the function [f] over the binary data in the file [filename] using the provided [module]. *)
val iter_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> string
  -> f:('a -> unit)
  -> unit

(** [fold_bin_prot_list module filename ~init ~f] folds the function [f] over the binary data in the file [filename] using the provided [module], starting with the initial value [init]. *)
val fold_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> string
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> 'b

(** [map_bin_prot_list module filename ~f] maps the function [f] over the binary data in the file [filename] using the provided [module]. *)
val map_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> string
  -> f:('a -> 'b)
  -> 'b list

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
