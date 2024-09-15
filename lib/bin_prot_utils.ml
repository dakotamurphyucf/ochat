open Core

let grow_buffer buf ~new_size_request =
  assert (new_size_request > Bigstring.length buf);
  Bigstring.unsafe_destroy_and_resize buf ~len:new_size_request
;;

(* need test for this *)
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
;;

let write_bin_prot' file writer v =
  let f fd =
    let buf = Bin_prot.Utils.bin_dump ~header:true writer v in
    Bigstring_unix.really_write fd buf
  in
  Core_unix.with_file file ~mode:[ Core_unix.O_WRONLY; Core_unix.O_CREAT ] ~f
;;

let read_bin_prot' file reader =
  let f fd =
    let buf =
      Bin_prot.Common.create_buf (Int64.to_int_exn (Core_unix.fstat fd).st_size)
    in
    Bigstring_unix.really_read fd buf;
    let res = Bigstring_unix.read_bin_prot buf reader in
    match res with
    | Error err -> failwith (Error.to_string_hum err)
    | Ok (v, _) -> v
  in
  Core_unix.with_file file ~mode:[ Core_unix.O_RDONLY ] ~f
;;

let fold_bin_file_list file reader ~init ~f =
  let f fd =
    let channel = Core_unix.in_channel_of_descr fd in
    let size = In_channel.length channel in
    let read buf ~pos ~len = Bigstring_unix.really_input channel ~pos ~len buf in
    let rec aux acc =
      if Int64.(In_channel.pos channel = size)
      then acc
      else (
        let v = Bin_prot.Utils.bin_read_stream ~read reader in
        aux (f acc v))
    in
    aux init
  in
  Core_unix.with_file file ~mode:[ Core_unix.O_RDONLY; Core_unix.O_CREAT ] ~f
;;

let read_bin_file_list = fold_bin_file_list ~init:[] ~f:(fun acc v -> v :: acc)
let iter_bin_file_list ~f = fold_bin_file_list ~init:() ~f:(fun () v -> f v)
let map_bin_file_list ~f = fold_bin_file_list ~init:[] ~f:(fun acc v -> f v :: acc)

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

let iter_bin_prot_list (type a) (module M : Bin_prot.Binable.S with type t = a) file =
  iter_bin_file_list file M.bin_reader_t
;;

let fold_bin_prot_list (type a) (module M : Bin_prot.Binable.S with type t = a) file =
  fold_bin_file_list file M.bin_reader_t
;;

let map_bin_prot_list (type a) (module M : Bin_prot.Binable.S with type t = a) file =
  map_bin_file_list file M.bin_reader_t
;;

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