open Core

(* MIME type helper ------------------------------------------------------- *)

(** [guess_mime_type filename] performs a very small extension→MIME mapping.
    If [filename] has no extension the function returns [None].  Otherwise it
    returns [Some mime_type], falling back to
    ["application/octet-stream"] for unrecognised extensions. *)
let guess_mime_type (filename : string) : string option =
  let _, ext_opt = Filename.split_extension filename in
  match ext_opt with
  | None -> None
  | Some ext ->
    let ext = String.lowercase ext in
    let table = function
      | ".ml" | ".mli" | ".txt" | ".md" -> "text/plain"
      | ".json" -> "application/json"
      | ".csv" -> "text/csv"
      | ".png" -> "image/png"
      | ".jpg" | ".jpeg" -> "image/jpeg"
      | ".gif" -> "image/gif"
      | ".pdf" -> "application/pdf"
      | _ -> "application/octet-stream"
    in
    Some (table ext)
;;

(** [is_text_mime mime] is a simple prefix check: it returns [true] when
    [mime] starts with ["text/"].  Handy when deciding whether to Base64-encode
    binary data for JSON transport. *)
let is_text_mime (mime : string) : bool = String.is_prefix mime ~prefix:"text/"
