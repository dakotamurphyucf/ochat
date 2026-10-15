open! Core
module D = Document_schema
module S = Chatml_source

module Location = struct
  type t =
    { source : S.Id.t
    ; digest : S.Digest.t
    ; source_bytes : int
    ; start_byte : int
    ; end_byte : int
    }
  [@@deriving sexp_of]
end

type t =
  { code : string
  ; message : string
  ; location : Location.t option
  }
[@@deriving sexp_of]

let invalid name reason = Error (D.Error.Invalid_field { path = [ name ]; reason })

let validate t =
  if String.is_empty t.code || String.length t.code > 256
  then invalid "code" "diagnostic code must be 1..256 bytes"
  else if String.length t.message > 16 * 1024
  then invalid "message" "diagnostic message exceeds 16 KiB"
  else (
    match t.location with
    | None -> Ok ()
    | Some location ->
      if
        location.source_bytes < 0
        || location.start_byte < 0
        || location.end_byte < location.start_byte
        || location.end_byte > location.source_bytes
      then invalid "location" "range exceeds the declared captured source"
      else Ok ())
;;

let to_json t =
  let location_json (location : Location.t) =
    `Object
      [ "namespace", `String (S.Id.package_namespace location.source)
      ; "member", `String (S.Id.relative_path location.source)
      ; "digest", `String (S.Digest.to_hex location.digest)
      ; "source_bytes", `Number (Int.to_string location.source_bytes)
      ; "start_byte", `Number (Int.to_string location.start_byte)
      ; "end_byte", `Number (Int.to_string location.end_byte)
      ]
  in
  `Object
    ([ "code", `String t.code; "message", `String t.message ]
     @
     match t.location with
     | None -> []
     | Some location -> [ "location", location_json location ])
;;
