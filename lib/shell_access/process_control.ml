open Core

type t =
  | Ready
  | Exited of int
  | Signaled of int
  | Failed
[@@deriving equal, sexp_of]

let frame_bytes = 16

let decode bytes =
  if String.length bytes <> frame_bytes
  then None
  else if
    (not (String.equal (String.prefix bytes 5) "OCHG\001"))
    || (not (Char.equal bytes.[6] '\000'))
    || (not (Char.equal bytes.[7] '\000'))
    || (not (String.equal (String.sub bytes ~pos:8 ~len:3) "\000\000\000"))
    || not (String.equal (String.sub bytes ~pos:12 ~len:4) "\000\000\000\000")
  then None
  else (
    (* Every valid finite status fits in the low byte. Reject high bytes before
       conversion, avoiding overflow even on a smaller OCaml integer host. *)
    let status = Char.to_int bytes.[11] in
    match Char.to_int bytes.[5] with
    | 1 when status = 0 -> Some Ready
    | 2 when status <= 255 -> Some (Exited status)
    | 3 when status > 0 && status < 128 -> Some (Signaled status)
    | 4 when status = 0 -> Some Failed
    | _ -> None)
;;
