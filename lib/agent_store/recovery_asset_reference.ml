open! Core
module P = Agent_protocol
module Citation = P.Recovery_window.Citation
module J = P.Json_codec

type t =
  { citation : Citation.t
  ; part : int
  ; blob : P.Blob.Metadata.t
  ; allowed_use : string
  }

type error =
  | Corrupt
  | Unsupported
  | Limit
[@@deriving equal, sexp_of]

let citation t = t.citation
let part t = t.part
let blob t = t.blob
let allowed_use t = t.allowed_use

let equal a b =
  Citation.equal a.citation b.citation
  && Int.equal a.part b.part
  && Blob_metadata.blob_equal a.blob b.blob
;;

let valid_media_type media_type =
  let token value =
    (not (String.is_empty value))
    && String.for_all value ~f:(function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '!'
      | '#'
      | '$'
      | '%'
      | '&'
      | '\''
      | '*'
      | '+'
      | '-'
      | '.'
      | '^'
      | '_'
      | '`'
      | '|'
      | '~' -> true
      | _ -> false)
  in
  String.length media_type <= 255
  &&
  match String.split media_type ~on:'/' with
  | [ kind; subtype ] -> token kind && token subtype
  | _ -> false
;;

let create citation ~part ~(blob : P.Blob.Metadata.t) =
  let open Result.Let_syntax in
  let%bind () =
    match blob.kind with
    | Image | File -> Ok ()
    | Audio | Binary -> Error Unsupported
  in
  let%bind allowed_use =
    Blob_metadata.Recovery_use.for_part citation ~part
    |> Result.map_error ~f:(fun _ -> Limit)
  in
  if
    Int64.(blob.byte_length < zero)
    || (not (valid_media_type blob.media_type))
    || Option.is_some blob.display_name
    || String.length blob.digest <> 64
    || not
         (String.for_all blob.digest ~f:(function
            | '0' .. '9' | 'a' .. 'f' -> true
            | _ -> false))
  then Error Corrupt
  else (
    let%map _ =
      P.Blob.Metadata.of_json (P.Blob.Metadata.to_json blob)
      |> Result.map_error ~f:(fun _ -> Corrupt)
    in
    { citation; part; blob; allowed_use })
;;

let to_json t =
  `Object
    [ "citation", Citation.to_json t.citation
    ; "part", `Number (Int.to_string t.part)
    ; "blob", P.Blob.Metadata.to_json t.blob
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let decode result = Result.map_error result ~f:(fun _ -> Corrupt) in
  let%bind fields = decode (J.fields json) in
  let%bind citation = decode (J.required_as fields "citation" Citation.of_json) in
  let%bind part = decode (J.required_as fields "part" (J.bounded_int ~min:0 ~max:4095)) in
  let%bind blob = decode (J.required_as fields "blob" P.Blob.Metadata.of_json) in
  create citation ~part ~blob
;;
