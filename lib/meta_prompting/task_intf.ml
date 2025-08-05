open Core

type t =
  { description : string
  ; context : string option
  ; tags : string list
  }

let make ?context ?(tags = []) description = { description; context; tags }

let to_markdown (t : t) : string =
  let tags_line =
    match t.tags with
    | [] -> ""
    | lst -> Printf.sprintf "\nTags: %s" (String.concat ~sep:", " lst)
  in
  let context_block =
    match t.context with
    | None | Some "" -> ""
    | Some c -> Printf.sprintf "\n\n### Context\n%s" c
  in
  Printf.sprintf "## Task\n%s%s%s" t.description context_block tags_line
;;
