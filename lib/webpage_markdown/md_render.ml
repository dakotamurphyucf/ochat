(* Minimal Markdown escaping for inline text (mirrors Html_to_md.escape_md). *)
let escape_md txt =
  let buf = Buffer.create (String.length txt) in
  String.iter
    (fun ch ->
       match ch with
       | '\\'
       | '`'
       | '*'
       | '_'
       | '{'
       | '}'
       | '['
       | ']'
       | '('
       | ')'
       | '#'
       | '+'
       | '-'
       | '!'
       | '|'
       | '>'
       | '~'
       | '.' ->
         Buffer.add_char buf '\\';
         Buffer.add_char buf ch
       | _ -> Buffer.add_char buf ch)
    txt;
  Buffer.contents buf
;;

open Core
open Omd

let rec render_inline : _ Omd.inline -> string = function
  | Text (_, s) -> escape_md s
  | Strong (_, inner) -> "**" ^ render_inline inner ^ "**"
  | Emph (_, inner) -> "*" ^ render_inline inner ^ "*"
  | Code (_, code) ->
    (* Choose a delimiter that is one backtick longer than the longest run
         of backticks inside [code].  If run ≥6 fall back to fenced block. *)
    let longest_run =
      let max_run = ref 0
      and cur = ref 0 in
      String.iter code ~f:(fun ch ->
        if Char.equal ch '`'
        then incr cur
        else (
          max_run := Int.max !max_run !cur;
          cur := 0));
      max_run := Int.max !max_run !cur;
      !max_run
    in
    if longest_run >= 6
    then "```\n" ^ code ^ "\n```"
    else (
      let delim_len = longest_run + 1 in
      let delim = String.init ~f:(fun _ -> '`') delim_len in
      if delim_len = 1 then delim ^ code ^ delim else delim ^ " " ^ code ^ " " ^ delim)
  | Link (_, { label; destination; _ }) ->
    Printf.sprintf "[%s](%s)" (render_inline label) destination
  | Image (_, { label; destination; _ }) ->
    Printf.sprintf "![%s](%s)" (render_inline label) destination
  | Concat (_, lst) -> String.concat ~sep:"" (List.map ~f:render_inline lst)
  | Soft_break _ -> " "
  | Hard_break _ -> "  \n"
  | Html (_, s) -> s
;;

let rec render_blocks ?(indent = "") blocks : string list =
  let push s = if String.equal s "" then [] else [ indent ^ s ] in
  List.concat_map blocks ~f:(function
    | Paragraph (_, inline) -> push (render_inline inline)
    | Heading (_, level, inline) ->
      let hashes = String.init level ~f:(fun _ -> '#') in
      push (hashes ^ " " ^ render_inline inline)
    | Thematic_break _ -> [ indent ^ "---" ]
    | Code_block (_, lang, code) ->
      [ (indent ^ "```" ^ if String.equal lang "" then "" else lang)
      ; indent ^ code
      ; indent ^ "```"
      ]
    | Blockquote (_, inner) -> render_blocks ~indent:(indent ^ "> ") inner
    | List (_, lt, _spacing, items) ->
      let ordered, start =
        match lt with
        | Ordered (n, _) -> true, n
        | Bullet _ -> false, 1
      in
      let marker i = if ordered then Printf.sprintf "%d." (start + i) else "*" in
      List.concat_mapi items ~f:(fun i item_blocks ->
        match render_blocks ~indent:(indent ^ "  ") item_blocks with
        | [] -> []
        | first :: rest ->
          let first_line = indent ^ marker i ^ " " ^ String.strip first in
          first_line :: rest)
    | Html_block (_, html) -> push (Printf.sprintf "```html\n%s\n```" html)
    | Table (_, header, rows) ->
      let render_row cells = "| " ^ String.concat ~sep:" | " cells ^ " |" in
      let header_cells = List.map header ~f:(fun (inl, _) -> render_inline inl) in
      let sep =
        "| "
        ^ String.concat
            ~sep:" | "
            (List.init (List.length header_cells) ~f:(fun _ -> "---"))
        ^ " |"
      in
      let body_rows =
        List.map rows ~f:(fun row ->
          let cells = List.map row ~f:render_inline in
          render_row cells)
      in
      render_row header_cells :: sep :: body_rows
    | _ -> [])
;;

let to_string (doc : Omd.doc) : string = render_blocks doc |> String.concat ~sep:"\n\n"
