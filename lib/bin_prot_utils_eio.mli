(** Binary-protocol helpers backed by {!Eio}.

    This interface is a drop-in, non-blocking replacement for
    {!Bin_prot_utils}.  All functions use {!Eio.Path} and {!Eio.Flow}
    rather than blocking system calls and must therefore run from
    within an Eio fibre (e.g. the callback passed to
    {!Eio_main.run}).

    Values are encoded with {!Bin_prot.Utils.bin_dump}
    [~header:true]; each value is therefore stored as a
    size-prefixed blob that can be exchanged with other Bin_prot
    implementations.

    The helpers come in three flavours:

    {ul
    {- {b Low-level helpers} – take an explicit writer / reader.}
    {- {b Binable helpers} – accept a {!module:Bin_prot.Binable.S}
       module.}
    {- {b Functor} – {!module:With_file_methods} generates a
       {!module-File} sub-module specialised to a concrete type.}}
*)

open Core

type path = Eio.Fs.dir_ty Eio.Path.t

(** [grow_buffer buf ~new_size_request] resizes [buf] to at least
    [new_size_request] bytes, preserving the existing contents.

    This is a thin wrapper around
    {!Core.Bigstring.unsafe_destroy_and_resize}.

    @raise Assert_failure if [new_size_request] ≤ [Bigstring.length
           buf]. *)
val grow_buffer : Bigstring.t -> new_size_request:int -> Bigstring.t

(** [append_bin_list_to_file file writer lst] opens [file] in append
    mode (creating it with permissions [`0o600`] if it does not exist)
    and writes each element of [lst] using [writer].  Each element is
    encoded using {!Bin_prot.Utils.bin_dump} with [~header:true]. *)
val append_bin_list_to_file : path -> 'a Bin_prot.Type_class.writer -> 'a list -> unit

(** [write_bin_prot' file writer v] truncates (or creates) [file] and
    stores [v] using [writer]. *)
val write_bin_prot' : path -> 'a Bin_prot.Type_class.writer -> 'a -> unit

(** [read_bin_prot' file reader] loads the contents of [file] into
    memory and decodes a single value with [reader].

    @raise Failure if the file does not contain exactly one complete
           size-prefixed value. *)
val read_bin_prot' : path -> 'a Bin_prot.Type_class.reader -> 'a

(** [fold_bin_file_list file reader ~init ~f] folds [f] over the values
    stored in [file] from left to right without loading the whole file
    into memory. *)
val fold_bin_file_list
  :  path
  -> 'a Bin_prot.Type_class.reader
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> 'b

(** [read_bin_file_list file reader] reads every value in [file] and
    returns them as a list, preserving write order. *)
val read_bin_file_list : path -> 'a Bin_prot.Type_class.reader -> 'a list

(** [iter_bin_file_list ~f file reader] calls [f] on each value stored
    in [file] for its side-effects. *)
val iter_bin_file_list : f:('a -> unit) -> path -> 'a Bin_prot.Type_class.reader -> unit

(** [map_bin_file_list ~f file reader] maps [f] over every value in
    [file] and returns the new list. *)
val map_bin_file_list : f:('a -> 'b) -> path -> 'a Bin_prot.Type_class.reader -> 'b list

(** [write_bin_prot (module M) file v] writes [v] to [file] using
    [M.bin_writer_t], truncating any existing contents. *)
val write_bin_prot : (module Bin_prot.Binable.S with type t = 'a) -> path -> 'a -> unit

(** [read_bin_prot (module M) file] reads the unique value from [file]
    using [M.bin_reader_t]. *)
val read_bin_prot : (module Bin_prot.Binable.S with type t = 'a) -> path -> 'a

(** [write_bin_prot_list (module M) file xs] appends every element of
    [xs] to [file] using [M.bin_writer_t]. *)
val write_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> path
  -> 'a list
  -> unit

(** [read_bin_prot_list (module M) file] reads all values from [file]
    using [M.bin_reader_t]. *)
val read_bin_prot_list : (module Bin_prot.Binable.S with type t = 'a) -> path -> 'a list

(** [iter_bin_prot_list (module M) file ~f] iterates over the values
    for side-effects. *)
val iter_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> path
  -> f:('a -> unit)
  -> unit

(** [fold_bin_prot_list (module M) file ~init ~f] folds over the
    values in [file]. *)
val fold_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> path
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> 'b

(** [map_bin_prot_list (module M) file ~f] maps [f] over every value
    and collects the results. *)
val map_bin_prot_list
  :  (module Bin_prot.Binable.S with type t = 'a)
  -> path
  -> f:('a -> 'b)
  -> 'b list

module With_file_methods (M : Bin_prot.Binable.S) : sig
  type t = M.t [@@deriving bin_io]

  (** A [File] sub-module with the common operations specialised to
      [M.t].  Using it avoids passing writers, readers, or first-class
      modules at every call-site. *)

  module File : sig
    (** Map over every stored value. *)
    val map : f:(t -> 'a) -> path -> 'a list

    (** Fold over the stored values. *)
    val fold : f:('a -> t -> 'a) -> path -> init:'a -> 'a

    (** Iterate over the stored values for side-effects. *)
    val iter : path -> f:(t -> unit) -> unit

    (** Read the entire file into memory. *)
    val read_all : path -> t list

    (** Truncate and rewrite the file with the given list. *)
    val write_all : path -> t list -> unit

    (** Read exactly one value.  Fails if the file contains a
        different number of values. *)
    val read : path -> t

    (** Truncate the file and write a single value. *)
    val write : path -> t -> unit
  end
end
