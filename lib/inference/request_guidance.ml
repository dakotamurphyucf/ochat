open! Core
module D = Document_schema

type t =
  { text : string
  ; provenance : string
  }
[@@deriving equal]

let text t = t.text
let provenance t = t.provenance

let to_json t =
  `Object
    [ "role", `String "user"; "text", `String t.text; "provenance", `String t.provenance ]
;;

let create ~text ~provenance ~limits =
  if String.is_empty text || String.length text > 524288
  then
    Error
      (D.Error.Invalid_field
         { path = [ "guidance"; "text" ]; reason = "requires 1–524288 UTF-8 bytes" })
  else if String.length provenance > 1024 || String.is_empty (String.strip provenance)
  then
    Error
      (D.Error.Invalid_field
         { path = [ "guidance"; "provenance" ]
         ; reason = "requires a nonblank label of at most 1024 UTF-8 bytes"
         })
  else (
    let value = { text; provenance } in
    let%map.Result () = D.Json.validate ~limits (to_json value) in
    value)
;;
