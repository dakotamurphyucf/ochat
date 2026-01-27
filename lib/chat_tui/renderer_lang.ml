open Core

let lang_of_path (path : string) : string option =
  let _, ext_opt = Filename.split_extension path in
  match ext_opt with
  | None -> None
  | Some ext ->
    let ext =
      if String.length ext > 0 && Char.( = ) (String.get ext 0) '.'
      then String.sub ext ~pos:1 ~len:(String.length ext - 1)
      else ext
    in
    let ext = String.lowercase ext in
    (match ext with
     | "ml" | "mli" -> Some "ocaml"
     | "md" -> Some "markdown"
     | "json" -> Some "json"
     | "sh" -> Some "bash"
     | "txt" -> None
     | _ -> None)
;;
