open Core

(** Utilities for reading and writing Bin_prot values using
    Eio instead of Core_unix.  The implementation is largely the
    same as {!Bin_prot_utils} but uses the high-level Eio.Path and
    Eio.Flow APIs for all IO.  All operations are expected to be
    called from within an Eio fiber. *)

module Flow = Eio.Flow
module Path = Eio.Path

type path = Eio.Fs.dir_ty Path.t

(**************************************************************************)
(*  Simple helpers                                                        *)
(**************************************************************************)

let grow_buffer buf ~new_size_request =
  assert (new_size_request > Bigstring.length buf);
  Bigstring.unsafe_destroy_and_resize buf ~len:new_size_request
;;

(**************************************************************************)
(*  File helpers                                                          *)
(**************************************************************************)

let append_bin_list_to_file (file : path) writer lst =
  (* Open [file] for appending, creating it if it is missing. *)
  Path.with_open_out ~append:true ~create:(`If_missing 0o600) file
  @@ fun flow ->
  List.iter lst ~f:(fun v ->
    let buf = Bin_prot.Utils.bin_dump ~header:true writer v in
    (* Convert the bigstring returned by [bin_dump] to a cstruct and
         write it to the file. *)
    Flow.write flow [ Cstruct.of_bigarray buf ])
;;

let write_bin_prot' (file : path) writer v =
  let buf = Bin_prot.Utils.bin_dump ~header:true writer v in
  Path.save ~create:(`Or_truncate 0o600) file (Core.Bigstring.to_string buf)
;;

let read_bin_prot' (file : path) reader =
  (* Load the whole file into a string and parse it. *)
  let data = Path.load file in
  let buf = Core.Bigstring.of_string data in
  match Bigstring_unix.read_bin_prot buf reader with
  | Error err -> failwith (Error.to_string_hum err)
  | Ok (v, _) -> v
;;

let fold_bin_file_list (file : path) reader ~init ~f =
  Path.with_open_in file
  @@ fun flow ->
  (* [bin_read_stream] expects a [read] function that fills the given
     bigstring.  We implement that on top of [Eio.Flow.read_exact]. *)
  let read buf ~pos ~len =
    let cs = Cstruct.of_bigarray ~off:pos ~len buf in
    Flow.read_exact flow cs
  in
  let rec aux acc =
    try
      let v = Bin_prot.Utils.bin_read_stream ~read reader in
      aux (f acc v)
    with
    | End_of_file -> acc
  in
  aux init
;;

let read_bin_file_list (file : path) reader =
  fold_bin_file_list file reader ~init:[] ~f:(fun acc v -> v :: acc)
;;

let iter_bin_file_list ~f (file : path) reader =
  ignore (fold_bin_file_list file reader ~init:() ~f:(fun () v -> f v))
;;

let map_bin_file_list ~f (file : path) reader =
  fold_bin_file_list file reader ~init:[] ~f:(fun acc v -> f v :: acc)
;;

(**************************************************************************)
(*  Top-level helpers that accept a Binable module                        *)
(**************************************************************************)

let write_bin_prot (type a) (module M : Bin_prot.Binable.S with type t = a) file (v : a) =
  write_bin_prot' file M.bin_writer_t v
;;

let read_bin_prot (type a) (module M : Bin_prot.Binable.S with type t = a) file =
  read_bin_prot' file M.bin_reader_t
;;

let write_bin_prot_list
      (type a)
      (module M : Bin_prot.Binable.S with type t = a)
      file
      (l : a list)
  =
  append_bin_list_to_file file M.bin_writer_t l
;;

let read_bin_prot_list (type a) (module M : Bin_prot.Binable.S with type t = a) file =
  read_bin_file_list file M.bin_reader_t
;;

let iter_bin_prot_list (type a) (module M : Bin_prot.Binable.S with type t = a) file ~f =
  iter_bin_file_list ~f file M.bin_reader_t
;;

let fold_bin_prot_list (type a) (module M : Bin_prot.Binable.S with type t = a) file =
  fold_bin_file_list file M.bin_reader_t
;;

let map_bin_prot_list (type a) (module M : Bin_prot.Binable.S with type t = a) file =
  map_bin_file_list file M.bin_reader_t
;;

(**************************************************************************)
(*  Functor with convenient [File] sub-module                             *)
(**************************************************************************)

module With_file_methods (M : Bin_prot.Binable.S) = struct
  include M

  module File = struct
    let map ~f file = map_bin_prot_list (module M) file ~f
    let fold ~f file ~init = fold_bin_prot_list (module M) file ~init ~f
    let iter file ~f = iter_bin_prot_list (module M) file ~f
    let read_all file = read_bin_prot_list (module M) file
    let write_all file data = write_bin_prot_list (module M) file data
    let read file = read_bin_prot (module M) file
    let write file v = write_bin_prot (module M) file v
  end
end
