open Core
open Omd

(******************************************************************************)
(* Helpers                                                                    *)
(******************************************************************************)

type attr = (string * string) list

(* Short aliases for constructors to keep code readable *)
let txt s = Text ([], s)
let concat lst = Concat ([], lst)

(* Collapse runs of whitespace to a single space (HTML rendering rule) *)
let collapse_whitespace s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec loop i last_ws =
    if i = len
    then ()
    else (
      match s.[i] with
      | ' ' | '\n' | '\t' | '\r' ->
        if not last_ws then Buffer.add_char buf ' ';
        loop (i + 1) true
      | c ->
        Buffer.add_char buf c;
        loop (i + 1) false)
  in
  loop 0 false;
  Buffer.contents buf
;;

(******************************************************************************)
(* Noise filtering helpers                                                    *)
(******************************************************************************)

(* Heuristic: text node likely CSS / style junk *)
let is_css_noise (s : string) : bool =
  String.is_substring s ~substring:"/*!sc*/"
  ||
  let len = String.length s in
  len > 400 && not (String.exists s ~f:Char.is_whitespace)
;;

(* Remove script/style/chrome and GitHub UI noise in-place *)
let prune_noise (soup : _ Soup.node) : unit =
  (* 1. Obvious non-content tags *)
  let noise_tags =
    [ "script"; "style"; "noscript"; "template"; "svg"; "canvas"; "iframe" ]
  in
  (* Avoid CSS selector engine entirely: traverse descendants and match tag *)
  let nodes_by_tag (root : _ Soup.node) (tag : string) : _ Soup.node list =
    Soup.descendants root
    |> Soup.to_list
    |> List.filter ~f:(fun n ->
      match Soup.element n with
      | Some el -> String.equal (Soup.name el) tag
      | None -> false)
  in
  List.iter noise_tags ~f:(fun tag -> nodes_by_tag soup tag |> List.iter ~f:Soup.delete);
  (* 2. Site chrome tags *)
  [ "header"; "nav"; "footer"; "aside" ]
  |> List.iter ~f:(fun tag ->
    nodes_by_tag soup tag
    |> List.iter ~f:(fun node ->
      match tag with
      | "header" ->
        (match Soup.attribute "class" (Soup.element node |> Option.value_exn) with
         | Some cls when String.is_substring cls ~substring:"odoc-preamble" -> ()
         | _ -> Soup.delete node)
      | _ -> Soup.delete node));
  (* 3. Heuristic class/id substrings *)
  let junk_subs = [ "octicon"; "tooltipped"; "sr-only"; "Tooltip__" ] in
  let attr_is_junk = function
    | None -> false
    | Some v -> List.exists junk_subs ~f:(fun sub -> String.is_substring v ~substring:sub)
  in
  (* Using [Soup.descendants] instead of the CSS selector "*" avoids potential
     crashes in the selector parser on some pathological pages where the CSS
     selector may get confused. [descendants] enumerates the full tree in a
     safe way. *)
  Soup.descendants soup
  |> Soup.to_list
  |> List.iter ~f:(fun node ->
    let el_opt = Soup.element node in
    match el_opt with
    | None -> ()
    | Some el ->
      if attr_is_junk (Soup.attribute "class" el) || attr_is_junk (Soup.attribute "id" el)
      then Soup.delete node)
;;

(* 4. If GitHub/GitLab style rendered markdown article exists, keep only that *)
let isolate_main_content (soup : _ Soup.node) : _ Soup.node =
  let article =
    Soup.descendants soup
    |> Soup.to_list
    |> List.find_map ~f:(fun n ->
      match Soup.element n with
      | Some el when String.equal (Soup.name el) "article" ->
        (match Soup.attribute "class" el with
         | Some cls when String.is_substring cls ~substring:"markdown-body" ->
           Some (Soup.coerce el)
         | _ -> None)
      | _ -> None)
  in
  match article with
  | Some art -> Soup.coerce art
  | None -> soup
;;

(******************************************************************************)
(* Inline conversion                                                          *)
(******************************************************************************)

(* Forward declarations to allow mutual recursion *)
let rec inline_of_node : _ Soup.node -> attr inline list =
  fun node ->
  match Soup.element node with
  | None ->
    let raw = Soup.texts node |> String.concat ~sep:"" |> collapse_whitespace in
    if String.is_empty (String.strip raw) || is_css_noise raw then [] else [ txt raw ]
  | Some el ->
    (match Soup.name el with
     | "strong" | "b" -> [ Strong ([], concat (inline_children el)) ]
     | "em" | "i" -> [ Emph ([], concat (inline_children el)) ]
     | "code" ->
       let code = Soup.texts el |> String.concat ~sep:"" |> String.strip in
       [ Code ([], code) ]
     | "a" ->
       let href = Option.value ~default:"" (Soup.attribute "href" el) in
       let label_children = inline_children el in
       (* Detect anchors with no visible text: if the concatenated children are
             empty (after stripping whitespace), skip emitting the link to avoid
             artifacts like "[](#anchor)". *)
       let is_empty_inline lst =
         List.for_all lst ~f:(function
           | Text (_, s) -> String.is_empty (String.strip s)
           | Concat (_, inner) ->
             let rec flat l =
               List.for_all l ~f:(function
                 | Text (_, s) -> String.is_empty (String.strip s)
                 | Concat (_, sub) -> flat sub
                 | _ -> false)
             in
             flat inner
           | Soft_break _ | Hard_break _ -> true
           | Html _ -> true
           | _ -> false)
       in
       if
         (String.is_prefix href ~prefix:"#" || String.is_substring href ~substring:"#")
         && is_empty_inline label_children
       then label_children
       else if is_empty_inline label_children
       then label_children
       else (
         let label = concat label_children in
         let link = { label; destination = href; title = None } in
         [ Link ([], link) ])
     | "img" ->
       let src = Option.value ~default:"" (Soup.attribute "src" el) in
       let alt = Option.value ~default:"" (Soup.attribute "alt" el) in
       let link = { label = txt alt; destination = src; title = None } in
       [ Image ([], link) ]
     | "br" -> [ Soft_break [] ]
     | _ -> inline_children el)

and inline_children el =
  Soup.children el |> Soup.to_list |> List.concat_map ~f:inline_of_node
;;

(******************************************************************************)
(* Block conversion                                                           *)
(******************************************************************************)

(* Safely convert a node to blocks, swallowing any Soup.Parse_error or other
   parsing exceptions so that one malformed subtree does not abort the entire
   document.  We mutually recurse with [block_of_node] defined just below. *)
let rec safe_block_of_node (node : _ Soup.node) : attr block list =
  try block_of_node node with
  | Soup.Parse_error _ | Failure _ | Invalid_argument _ -> []

and block_of_node (node : _ Soup.node) : attr block list =
  match Soup.element node with
  | None ->
    let txt_raw =
      Soup.texts node |> String.concat ~sep:"" |> collapse_whitespace |> String.strip
    in
    if String.is_empty txt_raw || is_css_noise txt_raw
    then []
    else [ Paragraph ([], txt txt_raw) ]
  | Some el ->
    (match Soup.name el with
     | tag when String.is_prefix tag ~prefix:"h" ->
       let lvl =
         try Int.of_string (String.drop_prefix tag 1) with
         | _ -> 1
       in
       let inls = concat (inline_children el) in
       [ Heading ([], Int.min 6 lvl, inls) ]
     | "p" -> [ Paragraph ([], concat (inline_children el)) ]
     | "hr" -> [ Thematic_break [] ]
     | "blockquote" ->
       let inner = children_blocks el in
       [ Blockquote ([], inner) ]
     | "pre" ->
       let code = Soup.texts el |> String.concat ~sep:"" |> String.rstrip in
       [ Code_block ([], "", code) ]
     | "code" ->
       (* Stand-alone <code> outside <pre>: render as an inline-code paragraph so
             API signatures keep their monospace formatting. *)
       let code_txt = Soup.texts el |> String.concat ~sep:"" |> String.strip in
       [ Paragraph ([], Code ([], code_txt)) ]
     | "ul" -> list_block ~ordered:false el
     | "ol" -> list_block ~ordered:true el
     | "table" -> table_block el
     | "dl" -> definition_list_block el
     | "br" -> [ Paragraph ([], concat [ Soft_break [] ]) ]
     | _ ->
       (* Unknown container: first try to recurse into its children, and if that
             yields blocks keep them; otherwise fall back to plain-text paragraph *)
       let inner = children_blocks el in
       if not (List.is_empty inner)
       then inner
       else (
         let txt_raw =
           Soup.texts el |> String.concat ~sep:"" |> collapse_whitespace |> String.strip
         in
         if String.is_empty txt_raw || is_css_noise txt_raw
         then []
         else [ Paragraph ([], txt txt_raw) ]))

and children_blocks (el : _ Soup.node) =
  Soup.children el |> Soup.to_list |> List.concat_map ~f:safe_block_of_node

and list_block ~ordered el : attr block list =
  let li_nodes =
    Soup.children el
    |> Soup.to_list
    |> List.filter ~f:(fun n ->
      match Soup.element n with
      | Some li -> String.equal (Soup.name li) "li"
      | None -> false)
  in
  let items =
    List.map li_nodes ~f:(fun li ->
      Soup.children li |> Soup.to_list |> List.concat_map ~f:block_of_node)
  in
  let list_type =
    if ordered
    then (
      let start_attr = Soup.attribute "start" el in
      let start_n = Option.value_map start_attr ~f:Int.of_string ~default:1 in
      Ordered (start_n, '.'))
    else Bullet '-'
  in
  let spacing = Tight in
  [ List ([], list_type, spacing, items) ]

and table_block tbl : attr block list =
  let all_rows =
    Soup.descendants tbl
    |> Soup.to_list
    |> List.filter ~f:(fun n ->
      match Soup.element n with
      | Some el -> String.equal (Soup.name el) "tr"
      | None -> false)
  in
  let cell_inline cell = concat (inline_children cell) in
  let collect_cells tr =
    Soup.children tr
    |> Soup.to_list
    |> List.filter_map ~f:(fun child ->
      match Soup.element child with
      | Some el ->
        (match Soup.name el with
         | "th" | "td" ->
           let inl = cell_inline el in
           (* Skip cells that are visually empty to avoid stray pipes. *)
           (match inl with
            | Text (_, s) when String.is_empty (String.strip s) -> None
            | Concat (_, lst) when List.is_empty lst -> None
            | _ -> Some inl)
         | _ -> None)
      | None -> None)
  in
  let rows =
    all_rows
    |> List.filter_map ~f:(fun tr ->
      let cells = collect_cells tr in
      if List.is_empty cells then None else Some cells)
  in
  match rows with
  | [] -> []
  | first :: _ as all_rows_nonempty ->
    (* Heuristic 1: if every row contains exactly one cell, treat this as a
         “simple list” table (odoc uses this for variant listings). Convert it
         to a bullet list instead of a Markdown pipe-table to avoid the noisy
         “Col1 | ---” header and triple-backtick artefacts. *)
    let single_col = List.for_all all_rows_nonempty ~f:(fun r -> List.length r = 1) in
    if single_col
    then (
      let clean_variant_string raw =
        let s = String.strip raw in
        (* Strip surrounding backticks of any length, but keep leading '|' *)
        let rec drop_backticks s =
          if
            String.length s >= 2
            && Char.equal s.[0] '`'
            && Char.equal s.[String.length s - 1] '`'
          then drop_backticks (String.sub s ~pos:1 ~len:(String.length s - 2))
          else s
        in
        drop_backticks s |> String.strip
      in
      let rec inline_to_string inl =
        match inl with
        | Text (_, s) | Code (_, s) -> s
        | Concat (_, lst) -> String.concat ~sep:"" (List.map lst ~f:inline_to_string)
        | Emph (_, inner) | Strong (_, inner) -> inline_to_string inner
        | _ -> ""
      in
      let row_to_blocks cells =
        match cells with
        | inl :: _ ->
          let raw = inline_to_string inl |> clean_variant_string in
          if String.is_empty raw then [] else [ Paragraph ([], Code ([], raw)) ]
        | [] -> []
      in
      let items = List.map all_rows_nonempty ~f:row_to_blocks in
      [ List ([], Bullet '-', Tight, items) ])
    else (
      (* Regular table conversion path *)
      (* Determine whether [first] is a header row by checking if the original
           HTML row had <th> elements. *)
      let is_header_row tr =
        Soup.children tr
        |> Soup.to_list
        |> List.exists ~f:(fun child ->
          match Soup.element child with
          | Some el -> String.equal (Soup.name el) "th"
          | None -> false)
      in
      let header_opt =
        match all_rows with
        | hdr :: _ when is_header_row hdr -> Some first
        | _ -> None
      in
      let body_rows =
        if Option.is_some header_opt
        then List.tl_exn all_rows_nonempty
        else all_rows_nonempty
      in
      let header_cells, alignments =
        match header_opt with
        | Some hdr_cells -> hdr_cells, List.map hdr_cells ~f:(fun _ -> Left)
        | None ->
          (* Synthesize a header row from body cell count. *)
          let synthetic =
            List.mapi first ~f:(fun i _ -> txt (Printf.sprintf "Col%d" (i + 1)))
          in
          synthetic, List.map synthetic ~f:(fun _ -> Left)
      in
      let header = List.zip_exn header_cells alignments in
      let body = List.map body_rows ~f:(fun r -> r) in
      [ Table ([], header, body) ])

and definition_list_block dl : attr block list =
  (* Convert <dl><dt>term</dt><dd>definition</dd> … </dl> into
     a sequence of blocks where each term is a bold paragraph followed by
     the blocks of its definition. *)
  let children = Soup.children dl |> Soup.to_list in
  let rec loop acc current_term = function
    | [] -> List.rev acc
    | node :: tl ->
      (match Soup.element node with
       | Some el when String.equal (Soup.name el) "dt" ->
         let term_inl = concat (inline_children el) in
         loop acc (Some term_inl) tl
       | Some el when String.equal (Soup.name el) "dd" ->
         (match current_term with
          | None -> loop acc None tl (* malformed, skip *)
          | Some term ->
            let term_para = Paragraph ([], Strong ([], term)) in
            let def_blocks = children_blocks el in
            loop (List.rev_append (term_para :: def_blocks) acc) None tl)
       | _ -> loop acc current_term tl)
  in
  loop [] None children
;;

(******************************************************************************)
(* Public API                                                                 *)
(******************************************************************************)

let convert (soup_original : Soup.soup Soup.node) : Omd.doc =
  (* 1. Trim to main content if recognisable, then prune generic noise *)
  let soup_poly = Soup.coerce soup_original in
  let soup = isolate_main_content soup_poly in
  prune_noise soup;
  (* 2. Proceed with normal conversion *)
  let root =
    (* Find <body> tag without using selector engine *)
    Soup.descendants soup
    |> Soup.to_list
    |> List.find_map ~f:(fun n ->
      match Soup.element n with
      | Some el when String.equal (Soup.name el) "body" -> Some (Soup.coerce el)
      | _ -> None)
    |> Option.value ~default:(Soup.coerce soup)
  in
  Soup.children root |> Soup.to_list |> List.concat_map ~f:safe_block_of_node
;;

let to_markdown_string soup : string =
  try
    let md = convert soup |> Md_render.to_string in
    String.strip md
  with
  | Soup.Parse_error msg -> Printf.sprintf "Error parsing HTML: %s" msg
  | exn -> Printf.sprintf "Unexpected error: %s" (Exn.to_string exn)
;;
