open Core

module Id = struct
  module T = struct
    type t =
      { package_namespace : string
      ; relative_path : string
      }
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let create ~package_namespace ~relative_path =
    let open Or_error.Let_syntax in
    if String.is_empty package_namespace
    then Or_error.error_string "source package namespace is empty"
    else (
      let%map path = Source.Portable_path.create relative_path in
      { package_namespace; relative_path = Source.Portable_path.to_string path })
  ;;

  let package_namespace t = t.package_namespace
  let relative_path t = t.relative_path
end

module Digest = struct
  type t = string [@@deriving equal, sexp_of]

  let of_hex string =
    if
      String.length string = 64
      && String.for_all string ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    then Ok string
    else Or_error.error_string "expected 64 lowercase SHA-256 hexadecimal bytes"
  ;;

  let to_hex t = t

  let of_contents contents ~checkpoint =
    let length = String.length contents in
    let rec loop offset context =
      checkpoint ();
      if offset = length
      then Digestif.SHA256.(to_hex (get context))
      else (
        let len = Int.min 4096 (length - offset) in
        let context = Digestif.SHA256.feed_string context ~off:offset ~len contents in
        loop (offset + len) context)
    in
    loop 0 (Digestif.SHA256.init ())
  ;;
end

module Location = struct
  type t =
    { source : Id.t
    ; source_digest : Digest.t
    ; span : Source.span
    }
  [@@deriving sexp_of]

  let source t = t.source
  let source_digest t = t.source_digest
  let span t = t.span
end

module Position_index = Source.Position_index

type t =
  { id : Id.t
  ; contents : string
  ; digest : Digest.t
  ; position_index : Position_index.t
  }

let reidentify t ~id = { t with id }
let id t = t.id
let contents t = t.contents
let digest t = t.digest
let byte_length t = String.length t.contents

let create id ~contents ~expected_digest ~max_bytes ~checkpoint =
  checkpoint ();
  let open Or_error.Let_syntax in
  let%bind () =
    match max_bytes with
    | Some maximum when maximum <= 0 -> Or_error.error_string "invalid source byte limit"
    | Some maximum when String.length contents > maximum ->
      Or_error.error_string "source byte limit exceeded"
    | None | Some _ -> Ok ()
  in
  let digest = Digest.of_contents contents ~checkpoint in
  match expected_digest with
  | Some expected when not (Digest.equal expected digest) ->
    Or_error.error_string "captured source digest mismatch"
  | None | Some _ ->
    let position_index = Position_index.create contents ~checkpoint in
    Ok { id; contents; digest; position_index }
;;

let locate t ~start_byte ~end_byte ~checkpoint =
  checkpoint ();
  if start_byte < 0 || end_byte < start_byte || end_byte > byte_length t
  then Or_error.error_string "source byte range is out of bounds"
  else (
    let position byte_offset =
      Position_index.position t.position_index ~byte_offset ~checkpoint |> Or_error.ok_exn
    in
    let left = position start_byte in
    let right = position end_byte in
    Ok { Location.source = t.id; source_digest = t.digest; span = { Source.left; right } })
;;

let same_position (a : Source.position) (b : Source.position) =
  Int.equal a.offset b.offset && Int.equal a.line b.line && Int.equal a.column b.column
;;

let of_span t (span : Source.span) ~checkpoint =
  let open Or_error.Let_syntax in
  let%bind location =
    locate t ~start_byte:span.left.offset ~end_byte:span.right.offset ~checkpoint
  in
  if
    same_position span.left location.span.left
    && same_position span.right location.span.right
  then Ok location
  else Or_error.error_string "source span coordinates disagree with captured bytes"
;;

let read t location =
  if
    (not (Id.equal t.id (Location.source location)))
    || not (Digest.equal t.digest (Location.source_digest location))
  then Or_error.error_string "location belongs to another source or revision"
  else (
    let span = Location.span location in
    if
      span.left.offset < 0
      || span.right.offset < span.left.offset
      || span.right.offset > byte_length t
    then Or_error.error_string "source byte range is out of bounds"
    else
      Ok
        (String.sub
           t.contents
           ~pos:span.left.offset
           ~len:(span.right.offset - span.left.offset)))
;;
