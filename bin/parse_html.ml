open Soup
let has_class c node = List.mem c @@ classes node
let _inline_elements = ["b"; "big"; "i"; "small"; "tt";
"abbr"; "acronym"; "cite"; "code"; "dfn"; "em"; "kbd"; "strong"; "samp"; "var";
"a"; "bdo"; "br"; "img"; "map"; "object"; "q"; "script"; "span"; "sub"; "sup";
"button"; "input"; "label"; "select"; "textarea";]



let rec process_node node =
  if not @@ is_element node then R.leaf_text node else
  let node = R.element node in
  let e_name = name  node in

  match e_name with
  | "h1" -> Printf.sprintf "# %s\n" @@ odoc_html_to_markdown @@ coerce node
  | "h2" -> Printf.sprintf "## %s\n" @@ odoc_html_to_markdown @@ coerce node
  | "h3" -> Printf.sprintf "### %s\n" @@ odoc_html_to_markdown @@ coerce node
  | "h4" -> Printf.sprintf "#### %s\n" @@ odoc_html_to_markdown @@ coerce node
  | "h5" -> Printf.sprintf "##### %s\n" @@ odoc_html_to_markdown @@ coerce node
  | "h6" -> Printf.sprintf "###### %s\n" @@ odoc_html_to_markdown @@ coerce node
  | "p" -> Printf.sprintf "%s\n\n" @@ odoc_html_to_markdown @@ coerce node
  | "ul" ->
    let items = children node |> to_list 
    |> List.map (fun li -> Printf.sprintf "- %s" ( String.concat " " @@ trimmed_texts li))  in
    String.concat "\n" items ^ "\n\n"
  | "ol" ->
    let items = children node |> to_list |> List.mapi (fun i li -> Printf.sprintf "%d. %s" (i + 1) (String.concat " " @@ trimmed_texts li))  in
    String.concat "\n" items ^ "\n\n"
  | "pre" when has_class "code" node ->
    let code = String.concat "" @@ trimmed_texts node in
    Printf.sprintf "```ocaml\n%s\n```\n\n" code
  | "nav" -> ""
  | "head" -> ""

  | "div" when has_class "odoc-content" node || has_class "odoc-spec" node ->
    (odoc_html_to_markdown ~sep:"\n" @@ coerce node) 
  | "details" ->
    children node |> to_list  |> List.filter_map (function
    | n when is_element n -> 
      let n = R.element n in
      if name n = "summary"  then Some (Printf.sprintf "\n###### %s\n\n" (String.concat " " @@ trimmed_texts n)) else
      Some (process_node @@ coerce n)
    | _ -> None)  |> String.concat ""
  | "code" -> 

    let code = String.concat " " @@ trimmed_texts node in
    Printf.sprintf "%s" code
  | _ -> odoc_html_to_markdown ~sep:"\n" @@ coerce node

and odoc_html_to_markdown ?(sep = "") soup =
  (* let soup = parse html in *)
  children soup
  |> to_list
  |> List.map process_node

  |> String.concat sep 


(*  https://developer.mozilla.org/de/docs/Web/HTML/Inline_elemente 
   
is_empty
*)
let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;


let _remove_empty_lines (input: string) : string =
  let lines = String.split_on_char '\n' input in
  let non_empty_lines = List.filter (fun line -> String.trim line <> "") lines in
  String.concat "\n" non_empty_lines

let split_string input = String.split_on_char ' ' input

let _parse_type_info html_content =
  let soup = parse html_content in
  let type_elements = soup $$ ".odoc-spec" in
  iter
    (fun el ->
      el
      $$ ".spec.type.anchored"
      |> iter (fun el -> texts el |> String.concat "" |> print_endline);
      el
      $$ ".spec.module.anchored"
      |> iter (fun el ->
           let t = texts el |> String.concat "" in
           match split_string t with
           | [ "module"; m; ":"; "sig"; "..."; "end" ] ->
             [ "module"; m; "="; m ] |> String.concat " " |> print_endline
           | _ -> t |> print_endline);
        el
           $$ ".spec.module-type.anchored"
           |> iter (fun el ->
                let t = texts el |> String.concat "" in
                match split_string t with
                | [ "module"; m; ":"; "sig"; "..."; "end" ] ->
                  [ "module"; m; "="; m ] |> String.concat " " |> print_endline
                | _ -> t |> print_endline);
      el
      $$ ".spec.value.anchored"
      |> iter (fun el -> texts el |> String.concat "" |> print_endline);
      el
      $$ ".spec-doc"
      |> iter (fun el ->
           print_string "(** ";
           texts el |> String.concat "" |> print_string;
           print_endline " *)");
      print_endline "")
    type_elements
;;

let () =
  let filename = Sys.argv.(1) in
  let html_content = read_file filename in
  print_endline  @@ odoc_html_to_markdown @@ coerce (parse html_content)
;;
