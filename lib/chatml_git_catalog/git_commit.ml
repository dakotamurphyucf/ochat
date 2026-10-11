open Core
module Id = Chatml_git_host.Object_id

module Error = struct
  type t =
    | Wrong_kind
    | Invalid
  [@@deriving equal, sexp_of]
end

let root_tree object_ ~checkpoint =
  if not (Git_object.Kind.equal (Git_object.kind object_) Commit)
  then Error Error.Wrong_kind
  else (
    let text = Git_object.body object_ in
    let format = Id.object_format (Git_object.id object_) in
    let width =
      match format with
      | Sha1 -> 40
      | Sha256 -> 64
    in
    let bytes = String.length text in
    if
      bytes < width + 7
      || (not (String.is_prefix text ~prefix:"tree "))
      || not (Char.equal text.[width + 5] '\n')
    then Error Error.Invalid
    else (
      match Id.create ~object_format:format (String.sub text ~pos:5 ~len:width) with
      | Error _ -> Error Error.Invalid
      | Ok id ->
        let rec line offset =
          checkpoint ();
          if offset >= bytes
          then Error Error.Invalid
          else if Char.equal text.[offset] '\n'
          then Ok id
          else if String.is_substring_at text ~pos:offset ~substring:"tree "
          then Error Error.Invalid
          else scan offset offset
        and scan start offset =
          if offset >= bytes
          then Error Error.Invalid
          else if Char.equal text.[offset] '\n'
          then line (offset + 1)
          else (
            if (offset - start) mod 4096 = 0 then checkpoint ();
            if Char.equal text.[offset] '\000'
            then Error Error.Invalid
            else scan start (offset + 1))
        in
        line (width + 6)))
;;
