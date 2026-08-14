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
     | "py" | "pyw" | "pyi" -> Some "python"
     | "rs" -> Some "rust"
     | "js" | "mjs" | "cjs" -> Some "javascript"
     | "jsx" -> Some "jsx"
     | "ts" | "mts" | "cts" -> Some "typescript"
     | "tsx" -> Some "tsx"
     | "md" -> Some "markdown"
     | "json" -> Some "json"
     | "sh" -> Some "bash"
     | "txt" -> None
     | _ -> None)
;;
