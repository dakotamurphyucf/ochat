open Core
open Jsonaf.Export

module Wire = struct
  type t =
    { id : string
    ; kind : Chatmd_script_spec.kind
    ; entrypoint : string
    ; program_fingerprint : string
    }
  [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]
end

include Wire

let valid_name value ~checkpoint =
  let length = String.length value in
  let rec loop offset =
    if offset >= length
    then true
    else (
      if offset mod 1024 = 0 then checkpoint ();
      if Char.equal value.[offset] '\000' then false else loop (offset + 1))
  in
  length > 0 && loop 0
;;

let valid_digest value =
  String.length value = 64
  && String.for_all value ~f:(fun c ->
    Char.is_digit c || (Char.(c >= 'a') && Char.(c <= 'f')))
;;

let validate t ~checkpoint =
  if
    valid_name t.id ~checkpoint
    && valid_name t.entrypoint ~checkpoint
    && valid_digest t.program_fingerprint
  then Ok t
  else Or_error.error_string "invalid prepared hook identity"
;;

let create ~id ~kind ~entrypoint ~program_fingerprint ~checkpoint =
  validate { id; kind; entrypoint; program_fingerprint } ~checkpoint
;;

let id t = t.id
let kind t = t.kind
let entrypoint t = t.entrypoint
let program_fingerprint t = t.program_fingerprint
let decoded_exn t = validate t ~checkpoint:(fun () -> ()) |> Or_error.ok_exn
let t_of_sexp value = Wire.t_of_sexp value |> decoded_exn
let t_of_jsonaf value = Wire.t_of_jsonaf value |> decoded_exn
let bin_read_t buffer ~pos_ref = Wire.bin_read_t buffer ~pos_ref |> decoded_exn

let __bin_read_t__ buffer ~pos_ref tag =
  Wire.__bin_read_t__ buffer ~pos_ref tag |> decoded_exn
;;

let bin_reader_t = { Bin_prot.Type_class.read = bin_read_t; vtag_read = __bin_read_t__ }
let bin_t = { Wire.bin_t with reader = bin_reader_t }
