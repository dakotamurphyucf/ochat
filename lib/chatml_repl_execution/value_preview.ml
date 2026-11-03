open! Core
module Lang = Chatml.Chatml_lang

module Limits = struct
  type t =
    { max_bytes : int
    ; max_nodes : int
    ; max_depth : int
    ; columns : int
    }

  let create ~max_bytes ~max_nodes ~max_depth ~columns =
    if
      max_bytes < 1
      || max_bytes > 65_536
      || max_nodes < 1
      || max_nodes > 4_096
      || max_depth < 1
      || max_depth > 128
      || columns < 8
      || columns > 512
    then Or_error.error_string "invalid REPL value preview limits"
    else Ok { max_bytes; max_nodes; max_depth; columns }
  ;;
end

type document =
  | Text of string
  | Field of string * document
  | Group of
      { left : string
      ; right : string
      ; separator : string
      ; padding : int
      ; children : document list
      }

let utf8_prefix text max_bytes =
  let rec loop offset =
    if offset >= String.length text
    then offset
    else (
      let decoded = Stdlib.String.get_utf_8_uchar text offset in
      let next = offset + Stdlib.Uchar.utf_decode_length decoded in
      if next > max_bytes then offset else loop next)
  in
  String.prefix text (loop 0)
;;

let escaped_prefix text ~max_bytes ~quoted =
  let inner_budget = Int.max 0 (max_bytes - if quoted then 2 else 0) in
  let fragments = ref [] in
  let used = ref 0 in
  let truncated = ref (quoted && max_bytes < 2) in
  let rec loop offset =
    if offset < String.length text
    then (
      let decoded = Stdlib.String.get_utf_8_uchar text offset in
      let width =
        if Stdlib.Uchar.utf_decode_is_valid decoded
        then Stdlib.Uchar.utf_decode_length decoded
        else 1
      in
      let code = Stdlib.Uchar.to_int (Stdlib.Uchar.utf_decode_uchar decoded) in
      let fragment =
        if not (Stdlib.Uchar.utf_decode_is_valid decoded)
        then Printf.sprintf "\\x%02x" (Char.to_int text.[offset])
        else (
          match code with
          | 10 -> "\\n"
          | 13 -> "\\r"
          | 9 -> "\\t"
          | 34 when quoted -> "\\\""
          | 92 -> "\\\\"
          | code when code < 32 || code = 127 || (code >= 128 && code <= 159) ->
            Printf.sprintf "\\u{%04x}" code
          | _ -> String.sub text ~pos:offset ~len:width)
      in
      if String.length fragment > inner_budget - !used
      then truncated := true
      else (
        fragments := fragment :: !fragments;
        used := !used + String.length fragment;
        loop (offset + width)))
  in
  loop 0;
  if !truncated
  then (
    let marker = String.prefix "..." inner_budget in
    let rec reserve = function
      | fragment :: rest when !used > inner_budget - String.length marker ->
        used := !used - String.length fragment;
        reserve rest
      | fragments -> fragments
    in
    fragments := marker :: reserve !fragments);
  let inner = String.concat (List.rev !fragments) in
  (if quoted && max_bytes >= 2 then "\"" ^ inner ^ "\"" else inner), !truncated
;;

let to_string ~limits value =
  let remaining_nodes = ref limits.Limits.max_nodes in
  let remaining_bytes = ref limits.max_bytes in
  let limited = ref false in
  let bounded_text ?(quoted = false) text =
    let text, truncated = escaped_prefix text ~max_bytes:!remaining_bytes ~quoted in
    remaining_bytes := !remaining_bytes - String.length text;
    limited := !limited || truncated;
    text
  in
  let atom ?quoted text = Text (bounded_text ?quoted text) in
  let group ~left ~right ~separator ?(padding = 0) children =
    Group { left; right; separator; padding; children }
  in
  let rec visit depth (value : Lang.value) =
    if !remaining_nodes <= 0 || !remaining_bytes <= 0 || depth >= limits.max_depth
    then (
      limited := true;
      Text "...")
    else (
      decr remaining_nodes;
      let child = visit (depth + 1) in
      let children sequence =
        let rec loop acc sequence =
          match Sequence.next sequence with
          | None -> List.rev acc
          | Some (value, rest) ->
            if !remaining_nodes <= 0 || !remaining_bytes <= 0
            then (
              limited := true;
              List.rev (Text "..." :: acc))
            else loop (child value :: acc) rest
        in
        loop [] sequence
      in
      match value with
      | VInt value -> atom (Int.to_string value)
      | VBool value -> atom (Bool.to_string value)
      | VFloat value -> atom (Float.to_string value)
      | VString value -> atom ~quoted:true value
      | VUnit -> atom "()"
      | VClosure _ -> atom "<closure>"
      | VBuiltin _ -> atom "<builtin>"
      | VModule _ -> atom "<module>"
      | VRef value -> group ~left:"ref(" ~right:")" ~separator:"," [ child !value ]
      | VArray values ->
        group ~left:"[" ~right:"]" ~separator:"," (children (Array.to_sequence values))
      | VTuple values ->
        let values = Sequence.of_list (values.first :: values.second :: values.rest) in
        group ~left:"(|" ~right:"|)" ~separator:"," (children values)
      | VRecord fields ->
        let rec fields_loop acc sequence =
          match Sequence.next sequence with
          | None -> List.rev acc
          | Some ((name, value), rest) ->
            if !remaining_nodes <= 0 || !remaining_bytes <= 0
            then (
              limited := true;
              List.rev (Text "..." :: acc))
            else (
              let name = bounded_text name in
              fields_loop (Field (name, child value) :: acc) rest)
        in
        group
          ~left:"{"
          ~right:"}"
          ~separator:";"
          ~padding:1
          (fields_loop [] (Map.to_sequence fields))
      | VVariant (tag, values) ->
        let tag = "`" ^ bounded_text tag in
        (match values with
         | [] -> Text tag
         | _ ->
           group
             ~left:(tag ^ "(")
             ~right:")"
             ~separator:","
             (children (Sequence.of_list values)))
      | VTask task ->
        (match task.step with
         | TPerform operation ->
           group ~left:"<Task perform " ~right:">" ~separator:"," [ atom operation.op ]
         | TSpawn operation ->
           group ~left:"<Task spawn " ~right:">" ~separator:"," [ atom operation.op ]
         | TPure _ -> atom "<Task pure>"
         | TBind _ -> atom "<Task bind>"
         | TMap _ -> atom "<Task map>"
         | TArray (mode, _, _) ->
           atom ("<Task " ^ Chatml.Chatml_task_array_mode.name mode ^ ">")
         | TFail _ -> atom "<Task fail>"
         | TCatch _ -> atom "<Task catch>"))
  in
  let document = visit 0 value in
  let output = Buffer.create (Int.min limits.max_bytes 1_024) in
  let formatter = Stdlib.Format.formatter_of_buffer output in
  Stdlib.Format.pp_set_margin formatter limits.columns;
  Stdlib.Format.pp_set_max_indent formatter (Int.max 2 (limits.columns - 2));
  let rec print = function
    | Text text -> Stdlib.Format.pp_print_string formatter text
    | Field (name, value) ->
      Stdlib.Format.pp_open_hovbox formatter 2;
      Stdlib.Format.pp_print_string formatter (name ^ " =");
      Stdlib.Format.pp_print_break formatter 1 0;
      print value;
      Stdlib.Format.pp_close_box formatter ()
    | Group { left; right; separator; padding; children } ->
      Stdlib.Format.pp_open_hovbox formatter 2;
      Stdlib.Format.pp_print_string formatter left;
      (match children with
       | [] -> ()
       | first :: rest ->
         Stdlib.Format.pp_print_break formatter padding 0;
         print first;
         List.iter rest ~f:(fun child ->
           Stdlib.Format.pp_print_string formatter separator;
           Stdlib.Format.pp_print_break formatter 1 0;
           print child);
         Stdlib.Format.pp_print_break formatter padding (-2));
      Stdlib.Format.pp_print_string formatter right;
      Stdlib.Format.pp_close_box formatter ()
  in
  print document;
  Stdlib.Format.pp_print_flush formatter ();
  let text = Buffer.contents output in
  if String.length text > limits.max_bytes || !limited
  then (
    let marker = " ... [truncated]" in
    if limits.max_bytes < String.length marker
    then
      utf8_prefix text (Int.max 0 (limits.max_bytes - 3))
      ^ String.prefix "..." limits.max_bytes
    else utf8_prefix text (limits.max_bytes - String.length marker) ^ marker)
  else text
;;
