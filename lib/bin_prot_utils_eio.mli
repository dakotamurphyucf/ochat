(** This module provides utility functions for reading and writing binary files using the Bin_prot library. *)

open Core

type path = Eio.Fs.dir_ty Eio.Path.t

(** Grow a bigstring buffer. *)
val grow_buffer : Bigstring.t -> new_size_request:int -> Bigstring.t

(** {1 Low-level helpers} *)

val append_bin_list_to_file : path -> 'a Bin_prot.Type_class.writer -> 'a list -> unit
val write_bin_prot' : path -> 'a Bin_prot.Type_class.writer -> 'a -> unit
val read_bin_prot' : path -> 'a Bin_prot.Type_class.reader -> 'a

val fold_bin_file_list
  :  path
  -> 'a Bin_prot.Type_class.reader
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> 'b

val read_bin_file_list : path -> 'a Bin_prot.Type_class.reader -> 'a list
val iter_bin_file_list : f:('a -> unit) -> path -> 'a Bin_prot.Type_class.reader -> unit
val map_bin_file_list : f:('a -> 'b) -> path -> 'a Bin_prot.Type_class.reader -> 'b list

(** {1 Helpers parameterised by a Binable module} *)

val write_bin_prot : (module Bin_prot.Binable.S with type t = 'a) -> path -> 'a -> unit
val read_bin_prot : (module Bin_prot.Binable.S with type t = 'a) -> path -> 'a

val write_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> path
  -> 'a list
  -> unit

val read_bin_prot_list : (module Bin_prot.Binable.S with type t = 'a) -> path -> 'a list

val iter_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> path
  -> f:('a -> unit)
  -> unit

val fold_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> path
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> 'b

val map_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> path
  -> f:('a -> 'b)
  -> 'b list

(** {1 Convenience functor} *)

module With_file_methods (M : Bin_prot.Binable.S) : sig
  type t = M.t [@@deriving bin_io]

  module File : sig
    val map : f:(t -> 'a) -> path -> 'a list
    val fold : f:('a -> t -> 'a) -> path -> init:'a -> 'a
    val iter : path -> f:(t -> unit) -> unit
    val read_all : path -> t list
    val write_all : path -> t list -> unit
    val read : path -> t
    val write : path -> t -> unit
  end
end
