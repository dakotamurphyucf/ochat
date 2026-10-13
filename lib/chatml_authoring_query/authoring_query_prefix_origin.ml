open Core
module Captured = Chatml_source
module Semantics = Chatml_semantics

type failure =
  | Evidence_mismatch
  | Stale_source

type t =
  { full_source : Captured.t
  ; prefix_id : Captured.Id.t
  ; prefix_digest : Captured.Digest.t
  ; end_byte : int
  }

let equal_string left right ~checkpoint =
  let length = String.length left in
  if not (Int.equal length (String.length right))
  then false
  else (
    let offset = ref 0 in
    let same = ref true in
    while !same && !offset < length do
      checkpoint ();
      same := Char.equal left.[!offset] right.[!offset];
      Int.incr offset
    done;
    !same)
;;

let equal_id left right ~checkpoint =
  equal_string
    (Captured.Id.package_namespace left)
    (Captured.Id.package_namespace right)
    ~checkpoint
  && equal_string
       (Captured.Id.relative_path left)
       (Captured.Id.relative_path right)
       ~checkpoint
;;

let find_prefix snapshot id ~checkpoint =
  let rec loop = function
    | [] -> None
    | source :: tail ->
      checkpoint ();
      if equal_id (Captured.id source) id ~checkpoint then Some source else loop tail
  in
  loop (Semantics.Snapshot.sources snapshot)
;;

let same_prefix full prefix length ~checkpoint =
  let full_bytes = Captured.contents full in
  let prefix_bytes = Captured.contents prefix in
  let offset = ref 0 in
  let same = ref true in
  while !same && !offset < length do
    checkpoint ();
    same := Char.equal full_bytes.[!offset] prefix_bytes.[!offset];
    Int.incr offset
  done;
  !same
;;

let create provisional ~full_source ~checkpoint =
  let id = Semantics.Provisional.selected_source provisional in
  let end_byte = Semantics.Provisional.end_byte provisional in
  if
    (not (equal_id (Captured.id full_source) id ~checkpoint))
    || (not
          (Captured.Digest.equal
             (Captured.digest full_source)
             (Semantics.Provisional.full_source_digest provisional)))
    || end_byte < 0
    || end_byte > Captured.byte_length full_source
  then Error Evidence_mismatch
  else (
    match find_prefix (Semantics.Provisional.snapshot provisional) id ~checkpoint with
    | None -> Error Evidence_mismatch
    | Some prefix ->
      if
        (not
           (Captured.Digest.equal
              (Captured.digest prefix)
              (Semantics.Provisional.prefix_source_digest provisional)))
        || (not (Int.equal (Captured.byte_length prefix) end_byte))
        || not (same_prefix full_source prefix end_byte ~checkpoint)
      then Error Evidence_mismatch
      else
        Ok
          { full_source
          ; prefix_id = id
          ; prefix_digest = Captured.digest prefix
          ; end_byte
          })
;;

let full_location t location ~checkpoint =
  checkpoint ();
  if not (equal_id (Captured.Location.source location) t.prefix_id ~checkpoint)
  then Ok None
  else if
    not (Captured.Digest.equal (Captured.Location.source_digest location) t.prefix_digest)
  then Error Stale_source
  else (
    let span = Captured.Location.span location in
    if span.left.offset < 0 || span.right.offset > t.end_byte
    then Error Stale_source
    else
      Captured.locate
        t.full_source
        ~start_byte:span.left.offset
        ~end_byte:span.right.offset
        ~checkpoint
      |> Result.map_error ~f:(fun _ -> Evidence_mismatch)
      |> Result.map ~f:Option.some)
;;
