type segment =
  | Text of string
  | Code_block of
      { lang : string option
      ; code : string
      }

let join_lines = function
  | [] -> ""
  | [ x ] -> x
  | xs -> String.concat "\n" xs
;;

let is_fence_line s =
  let len = String.length s in
  let rec skip_ws i n =
    if i < len && s.[i] = ' ' && n > 0 then skip_ws (i + 1) (n - 1) else i
  in
  let i = skip_ws 0 3 in
  if i + 3 <= len
  then (
    let c = s.[i] in
    if (c = '`' || c = '~') && s.[i + 1] = c && s.[i + 2] = c
    then Some (i, String.make 3 c)
    else None)
  else None
;;

let parse_info s i fence =
  let j = i + String.length fence in
  let j =
    let rec skip j =
      if j < String.length s && (s.[j] = ' ' || s.[j] = '\t') then skip (j + 1) else j
    in
    skip j
  in
  if j >= String.length s
  then None
  else (
    let rec find_end k =
      if k >= String.length s
      then k
      else if s.[k] = ' ' || s.[k] = '\t'
      then k
      else find_end (k + 1)
    in
    let k = find_end j in
    if k = j then None else Some (String.sub s j (k - j)))
;;

let split s =
  let lines = String.split_on_char '\n' s in
  let rec loop acc text_acc in_code code_acc code_open_line fence_lang fence_delim
    = function
    | [] ->
      (match in_code with
       | None ->
         let acc =
           if text_acc = [] then acc else Text (join_lines (List.rev text_acc)) :: acc
         in
         List.rev acc
       | Some _ ->
         let text =
           match code_open_line with
           | None -> join_lines (List.rev text_acc)
           | Some ol -> join_lines (List.rev (ol :: code_acc))
         in
         let acc = if text = "" then acc else Text text :: acc in
         List.rev acc)
    | l :: ls ->
      (match in_code with
       | None ->
         (match is_fence_line l with
          | Some (i, fence) ->
            let lang = parse_info l i fence in
            let acc =
              if text_acc = [] then acc else Text (join_lines (List.rev text_acc)) :: acc
            in
            loop acc [] (Some fence) [] (Some l) lang (Some fence) ls
          | None -> loop acc (l :: text_acc) None [] None None None ls)
       | Some fence ->
         (match is_fence_line l with
          | Some (_, fence') when fence' = fence ->
            let code = join_lines (List.rev code_acc) in
            let seg = Code_block { lang = fence_lang; code } in
            loop (seg :: acc) [] None [] None None None ls
          | _ ->
            loop
              acc
              text_acc
              (Some fence)
              (l :: code_acc)
              code_open_line
              fence_lang
              fence_delim
              ls))
  in
  loop [] [] None [] None None None lines
;;

type inline =
  | Inline_text of string
  | Inline_code of string

let split_inline s =
  let len = String.length s in
  let buf = Buffer.create len in
  let flush_text acc =
    if Buffer.length buf = 0
    then acc
    else (
      let t = Buffer.contents buf in
      Buffer.clear buf;
      Inline_text t :: acc)
  in
  let is_escaped i = i > 0 && Char.equal s.[i - 1] '\\' in
  let count_run i =
    let rec loop j =
      if j < len && Char.equal s.[j] '`' then loop (j + 1) else j
    in
    loop i - i
  in
  let is_run_at i ~n =
    i + n <= len
    && (let rec loop k =
          if k = n
          then true
          else if Char.equal s.[i + k] '`'
          then loop (k + 1)
          else false
        in
        loop 0)
  in
  let find_close ~from ~n =
    let rec loop i =
      if i + n > len
      then None
      else if (not (is_escaped i)) && is_run_at i ~n
      then Some i
      else loop (i + 1)
    in
    loop from
  in
  let rec loop acc i =
    if i >= len
    then (
      let acc = flush_text acc in
      List.rev acc)
    else if Char.equal s.[i] '`' && not (is_escaped i)
    then (
      let n = count_run i in
      match find_close ~from:(i + n) ~n with
      | Some j when j > i + n ->
        let acc = flush_text acc in
        let code = String.sub s (i + n) (j - (i + n)) in
        loop (Inline_code code :: acc) (j + n)
      | _ ->
        Buffer.add_substring buf s i n;
        loop acc (i + n))
    else (
      Buffer.add_char buf s.[i];
      loop acc (i + 1))
  in
  loop [] 0
;;
