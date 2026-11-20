open! Core

type t =
  { media_type : string
  ; contents : string
  }

type error =
  | Unsupported
  | Invalid_encoding
  | Media_mismatch
  | Size_limit
[@@deriving equal, sexp_of]

let media_type t = t.media_type
let contents t = t.contents

let media_type_of_contents contents =
  if String.is_prefix contents ~prefix:"\x89PNG\r\n\x1a\n"
  then Some "image/png"
  else if String.is_prefix contents ~prefix:"\xff\xd8\xff"
  then Some "image/jpeg"
  else if
    String.is_prefix contents ~prefix:"GIF87a"
    || String.is_prefix contents ~prefix:"GIF89a"
  then Some "image/gif"
  else if
    String.length contents >= 12
    && String.is_prefix contents ~prefix:"RIFF"
    && String.equal (String.sub contents ~pos:8 ~len:4) "WEBP"
  then Some "image/webp"
  else None
;;

let formats =
  List.map [ "image/png"; "image/jpeg"; "image/gif"; "image/webp" ] ~f:(fun media_type ->
    media_type, "data:" ^ media_type ^ ";base64,")
;;

let of_data_uri uri ~max_bytes =
  if max_bytes < 1 || max_bytes > 1_048_576
  then Error Size_limit
  else (
    match List.find formats ~f:(fun (_, prefix) -> String.is_prefix uri ~prefix) with
    | None -> Error Unsupported
    | Some (media_type, prefix) ->
      let offset = String.length prefix in
      let encoded = String.length uri - offset in
      if encoded > (max_bytes + 2) / 3 * 4
      then Error Size_limit
      else if encoded = 0 || encoded mod 4 <> 0
      then Error Invalid_encoding
      else (
        let padding =
          if String.is_suffix uri ~suffix:"=="
          then 2
          else if String.is_suffix uri ~suffix:"="
          then 1
          else 0
        in
        let length = (encoded / 4 * 3) - padding in
        if length > max_bytes
        then Error Size_limit
        else (
          match Base64.decode ~pad:true ~off:offset ~len:encoded uri with
          | Error _ -> Error Invalid_encoding
          | Ok contents ->
            if
              String.length contents <> length
              || not
                   (String.is_substring_at
                      uri
                      ~pos:offset
                      ~substring:(Base64.encode_exn contents))
            then Error Invalid_encoding
            else if
              not
                (Option.value_map
                   (media_type_of_contents contents)
                   ~default:false
                   ~f:(String.equal media_type))
            then Error Media_mismatch
            else Ok { media_type; contents })))
;;
