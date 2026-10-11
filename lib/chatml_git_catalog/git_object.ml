open Core
module Id = Chatml_git_host.Object_id

module Kind = struct
  type t =
    | Commit
    | Tree
    | Blob
  [@@deriving equal, sexp_of]

  let name = function
    | Commit -> "commit"
    | Tree -> "tree"
    | Blob -> "blob"
  ;;
end

module Error = struct
  type t =
    | Missing
    | Malformed_protocol
    | Hash_mismatch
    | Wrong_kind
    | Body_limit
  [@@deriving equal, sexp_of]
end

type t =
  { id : Id.t
  ; kind : Kind.t
  ; body : string
  }

let id t = t.id
let kind t = t.kind
let body t = t.body

let decimal wire ~start ~stop =
  let rec loop offset value =
    if offset = stop
    then if offset = start then None else Some value
    else (
      let digit = Char.to_int wire.[offset] - Char.to_int '0' in
      if digit < 0 || digit > 9 || value > (Int.max_value - digit) / 10
      then None
      else loop (offset + 1) ((value * 10) + digit))
  in
  loop start 0
;;

let framed id wire expected =
  let hex = Id.to_string id in
  let width = String.length hex in
  let bytes = String.length wire in
  if String.equal wire (hex ^ " missing\n")
  then Error Error.Missing
  else if bytes < width + 3 || not (String.is_prefix wire ~prefix:(hex ^ " "))
  then Error Error.Malformed_protocol
  else (
    let rec find offset stop char =
      if offset >= stop
      then None
      else if Char.equal wire.[offset] char
      then Some offset
      else find (offset + 1) stop char
    in
    let header_end = find (width + 1) (Int.min bytes (width + 33)) '\n' in
    match header_end with
    | None -> Error Error.Malformed_protocol
    | Some stop when stop > width + 32 -> Error Error.Malformed_protocol
    | Some stop ->
      (match find (width + 1) stop ' ' with
       | None -> Error Error.Malformed_protocol
       | Some split when split >= stop -> Error Error.Malformed_protocol
       | Some split ->
         let kind = String.sub wire ~pos:(width + 1) ~len:(split - width - 1) in
         (match decimal wire ~start:(split + 1) ~stop with
          | None -> Error Error.Malformed_protocol
          | Some length ->
            let start = stop + 1 in
            if
              length > bytes - start
              || length <> bytes - start - 1
              || not (Char.equal wire.[bytes - 1] '\n')
            then Error Error.Malformed_protocol
            else if not (String.equal kind (Kind.name expected))
            then Error Error.Wrong_kind
            else Ok (start, length))))
;;

let verify id ~wire ~expected ~max_bytes ~admit_bytes ~checkpoint =
  checkpoint ();
  let open Result.Let_syntax in
  let%bind start, length = framed id wire expected in
  if max_bytes < 0
  then Error Error.Malformed_protocol
  else if length > max_bytes
  then Error Error.Body_limit
  else (
    admit_bytes length;
    let header = Kind.name expected ^ " " ^ Int.to_string length ^ "\000" in
    let body = Bytes.create length in
    let sha1 = ref (Digestif.SHA1.feed_string Digestif.SHA1.empty header) in
    let sha256 = ref (Digestif.SHA256.feed_string Digestif.SHA256.empty header) in
    let format = Id.object_format id in
    let rec copy offset =
      if offset < length
      then (
        checkpoint ();
        let len = Int.min 4096 (length - offset) in
        Stdlib.Bytes.blit_string wire (start + offset) body offset len;
        (match format with
         | Sha1 -> sha1 := Digestif.SHA1.feed_string !sha1 ~off:(start + offset) ~len wire
         | Sha256 ->
           sha256 := Digestif.SHA256.feed_string !sha256 ~off:(start + offset) ~len wire);
        copy (offset + len))
    in
    copy 0;
    checkpoint ();
    let actual =
      match format with
      | Sha1 -> Digestif.SHA1.(to_hex (get !sha1))
      | Sha256 -> Digestif.SHA256.(to_hex (get !sha256))
    in
    if not (String.equal actual (Id.to_string id))
    then Error Error.Hash_mismatch
    else
      Ok
        { id
        ; kind = expected
        ; body = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:body
        })
;;
