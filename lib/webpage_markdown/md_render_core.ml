open Core
open Omd
module B = Conversion_budget
module O = Conversion_output

let rec render_inline budget depth inline =
  B.depth budget depth;
  B.nodes budget 8;
  let text = O.text budget
  and join = O.concat budget in
  let recur = render_inline budget (depth + 1) in
  match inline with
  | Text (_, s) -> O.escape_markdown budget s
  | Strong (_, inner) -> join [ text "**"; recur inner; text "**" ]
  | Emph (_, inner) -> join [ text "*"; recur inner; text "*" ]
  | Code (_, code) ->
    let max_run = ref 0
    and cur = ref 0 in
    String.iter code ~f:(fun c ->
      B.work budget 1;
      if Char.equal c '`'
      then incr cur
      else (
        max_run := Int.max !max_run !cur;
        cur := 0));
    max_run := Int.max !max_run !cur;
    if !max_run >= 6
    then join [ text "```\n"; text code; text "\n```" ]
    else (
      let n = !max_run + 1 in
      let delim = O.repeat budget '`' n in
      if n = 1
      then join [ delim; text code; delim ]
      else join [ delim; text " "; text code; text " "; delim ])
  | Link (_, { label; destination; _ }) ->
    join [ text "["; recur label; text "]("; text destination; text ")" ]
  | Image (_, { label; destination; _ }) ->
    join [ text "!["; recur label; text "]("; text destination; text ")" ]
  | Concat (_, xs) -> join (B.map budget xs ~f:recur)
  | Soft_break _ -> text " "
  | Hard_break _ -> text "  \n"
  | Html (_, s) -> text s
;;

let rec render_blocks budget depth ~indent blocks =
  B.depth budget depth;
  let text = O.text budget
  and join = O.concat budget in
  let push piece =
    B.nodes budget 4;
    if O.byte_length piece = 0 then [] else [ join [ indent; piece ] ]
  in
  B.concat_map budget blocks ~f:(fun block ->
    B.nodes budget 12;
    match block with
    | Paragraph (_, inline) -> push (render_inline budget (depth + 1) inline)
    | Heading (_, level, inline) ->
      if level < 0 then B.reject_html budget;
      push
        (join
           [ O.repeat budget '#' level
           ; text " "
           ; render_inline budget (depth + 1) inline
           ])
    | Thematic_break _ -> [ join [ indent; text "---" ] ]
    | Code_block (_, lang, code) ->
      [ join [ indent; text "```"; text lang ]
      ; join [ indent; text code ]
      ; join [ indent; text "```" ]
      ]
    | Blockquote (_, inner) ->
      render_blocks budget (depth + 1) ~indent:(join [ indent; text "> " ]) inner
    | List (_, lt, _spacing, items) ->
      let ordered, start =
        match lt with
        | Ordered (n, _) -> true, n
        | Bullet _ -> false, 1
      in
      B.concat_map
        budget
        (B.mapi budget items ~f:(fun i item ->
           match
             render_blocks budget (depth + 1) ~indent:(join [ indent; text "  " ]) item
           with
           | [] -> []
           | first :: rest ->
             let marker = if ordered then Int.to_string (start + i) ^ "." else "*" in
             B.nodes budget 6;
             join [ indent; text marker; text " "; O.strip budget first ] :: rest))
        ~f:Fn.id
    | Html_block (_, html) -> push (join [ text "```html\n"; text html; text "\n```" ])
    | Table (_, header, rows) ->
      let between pieces =
        B.nodes budget 4;
        B.concat_map
          budget
          (B.mapi budget pieces ~f:(fun i p ->
             B.nodes budget 2;
             if i = 0 then [ p ] else [ text " | "; p ]))
          ~f:Fn.id
      in
      let row cells =
        B.nodes budget 3;
        join [ text "| "; join (between cells); text " |" ]
      in
      let heads =
        B.map budget header ~f:(fun (inl, _) -> render_inline budget (depth + 1) inl)
      in
      let sep = row (B.map budget header ~f:(fun _ -> text "---")) in
      let body =
        B.map budget rows ~f:(fun cells ->
          row (B.map budget cells ~f:(render_inline budget (depth + 1))))
      in
      B.nodes budget 2;
      row heads :: sep :: body
    | _ -> [])
;;

let to_string budget doc =
  let lines = render_blocks budget 1 ~indent:(O.text budget "") doc in
  let pieces =
    B.concat_map
      budget
      (B.mapi budget lines ~f:(fun i line ->
         B.nodes budget 2;
         if i = 0 then [ line ] else [ O.text budget "\n\n"; line ]))
      ~f:Fn.id
  in
  O.to_string budget (O.concat budget pieces)
;;
