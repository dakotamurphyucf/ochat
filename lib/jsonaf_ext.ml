open Core
include Jsonaf (* the real library *)

(* 1 ─ Shape  -------------------------------------------------------- *)
let bin_shape_t = Bin_prot.Shape.bin_shape_string

(* 2 ─ Writer  ------------------------------------------------------- *)
let bin_size_t (t : Jsonaf.t) =
  (* serialise -> string -> ask bin_prot for its size *)
  Bin_prot.Size.bin_size_string (Jsonaf.to_string t)
;;

let bin_write_t buf ~pos (t : Jsonaf.t) =
  Bin_prot.Write.bin_write_string buf ~pos (Jsonaf.to_string t)
;;

let bin_writer_t : Jsonaf.t Bin_prot.Type_class.writer =
  { size = bin_size_t; write = bin_write_t }
;;

(* 3 ─ Reader  ------------------------------------------------------- *)
let bin_read_t buf ~pos_ref =
  let json_txt = Bin_prot.Read.bin_read_string buf ~pos_ref in
  Jsonaf.of_string json_txt
;;

let __bin_read_t__ buf ~pos_ref _len = bin_read_t buf ~pos_ref

let bin_reader_t : Jsonaf.t Bin_prot.Type_class.reader =
  { read = bin_read_t; vtag_read = __bin_read_t__ }
;;

(* 4 ─ Type-class bundle -------------------------------------------- *)
let bin_t : Jsonaf.t Bin_prot.Type_class.t =
  { writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
;;
