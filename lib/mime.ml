open Core

(* MIME type helper ------------------------------------------------------- *)

(** [guess_mime_type filename] returns a best-effort MIME type based on the
    filename extension.  Only a handful of common types are recognised.  When
    the extension is unknown the function returns [None]. *)
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

(** Predicate telling whether the supplied mime-type is textual and can be
    inlined into JSON without Base64 encoding. *)
let is_text_mime (mime : string) : bool = String.is_prefix mime ~prefix:"text/"
