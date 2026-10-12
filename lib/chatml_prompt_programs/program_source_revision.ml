open Core
module Captured = Chatml_source

type t =
  { id : Captured.Id.t
  ; digest : Captured.Digest.t
  ; byte_length : int
  }
[@@deriving sexp_of]

let id t = t.id
let digest t = t.digest
let byte_length t = t.byte_length

let compare left right =
  match Captured.Id.compare left.id right.id with
  | 0 ->
    (match
       String.compare
         (Captured.Digest.to_hex left.digest)
         (Captured.Digest.to_hex right.digest)
     with
     | 0 -> Int.compare left.byte_length right.byte_length
     | order -> order)
  | order -> order
;;

let equal left right = Int.equal (compare left right) 0

include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

let create id ~digest ~byte_length =
  if byte_length < 0
  then Or_error.error_string "Negative source revision length"
  else Ok { id; digest; byte_length }
;;

let of_source source =
  { id = Captured.id source
  ; digest = Captured.digest source
  ; byte_length = Captured.byte_length source
  }
;;

let matches t source = equal t (of_source source)

let key t ~checkpoint =
  let context = ref Digestif.SHA256.empty in
  List.iter
    [ Captured.Id.package_namespace t.id
    ; Captured.Id.relative_path t.id
    ; Captured.Digest.to_hex t.digest
    ; Int.to_string t.byte_length
    ]
    ~f:(fun field ->
      checkpoint ();
      context
      := Digestif.SHA256.feed_string !context (Int.to_string (String.length field) ^ ":");
      let offset = ref 0 in
      while !offset < String.length field do
        checkpoint ();
        let len = Int.min 4096 (String.length field - !offset) in
        context := Digestif.SHA256.feed_string !context field ~off:!offset ~len;
        offset := !offset + len
      done);
  checkpoint ();
  Digestif.SHA256.(get !context |> to_hex)
;;
