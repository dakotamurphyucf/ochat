open Core

let convert soup = Html_to_md_core.convert (Conversion_budget.legacy ()) soup

let to_markdown_string soup =
  try convert soup |> Md_render.to_string |> String.strip with
  | Soup.Parse_error msg -> Printf.sprintf "Error parsing HTML: %s" msg
  | exn -> Printf.sprintf "Unexpected error: %s" (Exn.to_string exn)
;;
