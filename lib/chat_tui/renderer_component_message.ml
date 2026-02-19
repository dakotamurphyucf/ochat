open Core
open Notty
open Types
module Styles = Highlight_styles

module Theme = struct
  let attr_of_role = function
    | "assistant" -> Styles.fg_hex "#FF9800"
    | "user" -> Styles.fg_hex "#FFD700"
    | "developer" -> Styles.fg_hex "#FF5C38"
    | "tool" -> Styles.fg_hex "#13A3F2"
    | "fork" -> Styles.fg_hex "#FFB454"
    | "reasoning" -> Styles.fg_hex "#13F2A3"
    | "system" -> Styles.fg_hex "#C7CCD8"
    | "tool_output" -> Styles.fg_hex "#8BD649"
    | "error" -> A.(Styles.fg_hex "#FF5370" ++ st reverse)
    | _ -> A.empty
  ;;

  let selection_attr base = A.(base ++ st reverse)
end

let safe_string attr s =
  match I.string attr s with
  | img -> img
  | exception _e -> I.string attr ""
;;

module Roles = struct
  let is_toollike = function
    | "tool" | "tool_output" -> true
    | _ -> false
  ;;

  let label_of_role (r : role) = r
end

module Spans = struct
  type run = Notty.A.t * string
  type line = run list
end

module Wrap = struct
  open Spans

  let width_of_text s = I.width (safe_string A.empty s)

  let push_run acc a s =
    match acc with
    | (a', s') :: tl when phys_equal a a' -> (a, s' ^ s) :: tl
    | _ -> (a, s) :: acc
  ;;

  let flush_line acc cur = List.rev cur :: acc

  let utf8_len byte =
    if byte land 0x80 = 0
    then 1
    else if byte land 0xE0 = 0xC0
    then 2
    else if byte land 0xF0 = 0xE0
    then 3
    else if byte land 0xF8 = 0xF0
    then 4
    else 1
  ;;

  let next_piece s pos =
    if pos >= String.length s
    then None
    else (
      let code = Char.to_int (String.unsafe_get s pos) in
      let len = utf8_len code |> Int.max 1 in
      let len = Int.min len (String.length s - pos) in
      Some (String.sub s ~pos ~len, pos + len))
  ;;

  let rec add_text ~limit acc cur cur_w a s pos =
    match next_piece s pos with
    | None -> acc, cur, cur_w
    | Some (piece, next_pos) ->
      let ch_w = width_of_text piece in
      if cur_w + ch_w > limit && not (List.is_empty cur)
      then add_text ~limit (flush_line acc cur) [] 0 a s pos
      else (
        let cur = push_run cur a piece in
        add_text ~limit acc cur (cur_w + ch_w) a s next_pos)
  ;;

  let rec loop ~limit acc cur cur_w = function
    | [] -> List.rev (if List.is_empty cur then acc else flush_line acc cur)
    | (a, s) :: rest ->
      let acc, cur, cur_w = add_text ~limit acc cur cur_w a s 0 in
      loop ~limit acc cur cur_w rest
  ;;

  let wrap_runs ~limit (runs : run list) : line list =
    if limit <= 0 then [ runs ] else loop ~limit [] [] 0 runs
  ;;
end

module Blocks = struct
  type t =
    | Text of string
    | Code of
        { lang : string option
        ; code : string
        }

  let of_message_text s =
    Markdown_fences.split s
    |> List.map ~f:(function
      | Markdown_fences.Text t -> Text t
      | Markdown_fences.Code_block { lang; code } -> Code { lang; code })
  ;;
end

module Code_cache2 = struct
  type role_class =
    | Toollike
    | Userlike

  let bucket_size = 8

  let bucket_for_width w =
    if w <= 0 then 0 else (w + bucket_size - 1) / bucket_size * bucket_size
  ;;

  let role_tag = function
    | Toollike -> 'T'
    | Userlike -> 'U'
  ;;

  let key ~klass ~lang ~digest ~wb =
    let l = Option.value lang ~default:"-" in
    String.concat
      [ String.of_char (role_tag klass); "|"; l; "|"; digest; "|"; Int.to_string wb ]
  ;;

  type entry =
    { mutable last_used : int
    ; img : I.t
    }

  let capacity = 128
  let tbl : (string, entry) Hashtbl.t = Hashtbl.create (module String)
  let tick = ref 0

  let get ~klass ~lang ~digest ~wb =
    incr tick;
    let k = key ~klass ~lang ~digest ~wb in
    match Hashtbl.find tbl k with
    | None -> None
    | Some e ->
      e.last_used <- !tick;
      Some e.img
  ;;

  let evict_if_needed () =
    if Hashtbl.length tbl > capacity
    then (
      let oldest =
        Hashtbl.fold tbl ~init:None ~f:(fun ~key ~data acc ->
          match acc with
          | None -> Some (key, data.last_used)
          | Some (_k, t) -> if data.last_used < t then Some (key, data.last_used) else acc)
      in
      match oldest with
      | None -> ()
      | Some (k, _) -> Hashtbl.remove tbl k)
  ;;

  let set ~klass ~lang ~digest ~wb img =
    incr tick;
    let k = key ~klass ~lang ~digest ~wb in
    Hashtbl.set tbl ~key:k ~data:{ last_used = !tick; img };
    evict_if_needed ()
  ;;
end

module Render_context = struct
  type t =
    { width : int
    ; selected : bool
    ; role : string
    ; tool_output : tool_output_kind option
    ; hi_engine : Highlight_tm_engine.t
    }

  let make ~width ~selected ~role ~tool_output ~hi_engine =
    { width; selected; role; tool_output; hi_engine }
  ;;

  let prefix_first _t = ""
  let prefix_cont _t = ""
end

let lang_of_path = Renderer_lang.lang_of_path
let has_scope_prefix scopes ~prefix = List.exists scopes ~f:(String.is_prefix ~prefix)

let is_only_char s ~char =
  (not (String.is_empty s)) && String.for_all s ~f:(Char.equal char)
;;

let should_drop_markdown_delimiter ~scopes ~text =
  if has_scope_prefix scopes ~prefix:"punctuation.definition.raw"
  then is_only_char text ~char:'`'
  else if
    has_scope_prefix scopes ~prefix:"punctuation.definition.bold"
    || has_scope_prefix scopes ~prefix:"punctuation.definition.italic"
  then is_only_char text ~char:'*' || is_only_char text ~char:'_'
  else false
;;

let suppress_markdown_delimiters (spans : Highlight_tm_engine.scoped_span list) =
  List.filter spans ~f:(fun s ->
    not (should_drop_markdown_delimiter ~scopes:s.scopes ~text:s.text))
;;

module Paint = struct
  open Render_context

  let apply_selection (ctx : t) a = if ctx.selected then Theme.selection_attr a else a

  let first_prefix (ctx : t) ~(is_first : bool) =
    if is_first then prefix_first ctx else prefix_cont ctx
  ;;

  let cont_prefix (ctx : t) = prefix_cont ctx
  let paragraph_limit (ctx : t) = Int.max 1 (ctx.width - String.length (prefix_first ctx))

  let render_runs (ctx : t) ~(is_first : bool) (runs : Spans.run list) : I.t list =
    let limit = paragraph_limit ctx in
    let wrapped = Wrap.wrap_runs ~limit runs in
    let render_line ~pref line_runs =
      let content_img =
        List.map line_runs ~f:(fun (a, s) -> safe_string (apply_selection ctx a) s)
        |> I.hcat
      in
      Notty.Infix.(safe_string A.empty pref <|> content_img)
      |> I.hsnap ~align:`Left ctx.width
    in
    match wrapped with
    | [] -> []
    | l0 :: rest ->
      let row0 = render_line ~pref:(first_prefix ctx ~is_first) l0 in
      row0 :: List.map rest ~f:(render_line ~pref:(cont_prefix ctx))
  ;;

  let is_read_directory (ctx : t) =
    match ctx.tool_output with
    | Some (Read_directory _) -> true
    | _ -> false
  ;;

  type fallback_style =
    { bold : bool
    ; italic : bool
    }

  let fallback_bold_attr =
    Highlight_theme.attr_of_scopes
      Highlight_theme.github_dark
      ~scopes:[ "markup.bold.markdown" ]
  ;;

  let fallback_italic_attr =
    Highlight_theme.attr_of_scopes
      Highlight_theme.github_dark
      ~scopes:[ "markup.italic.markdown" ]
  ;;

  let fallback_inline_code_attr =
    Highlight_theme.attr_of_scopes
      Highlight_theme.github_dark
      ~scopes:[ "markup.inline.raw.string.markdown" ]
  ;;

  let fallback_attr_of_style { bold; italic } =
    match bold, italic with
    | false, false -> A.empty
    | true, false -> fallback_bold_attr
    | false, true -> fallback_italic_attr
    | true, true -> Styles.(fallback_bold_attr ++ fallback_italic_attr)
  ;;

  let is_escaped (s : string) i = i > 0 && Char.equal s.[i - 1] '\\'

  let is_intraword ~prev ~next =
    Option.value_map prev ~default:false ~f:Char.is_alphanum
    && Option.value_map next ~default:false ~f:Char.is_alphanum
  ;;

  let marker_at (s : string) ~pos ~limit =
    if pos >= limit
    then None
    else (
      match s.[pos] with
      | '*' when pos + 1 < limit && Char.equal s.[pos + 1] '*' -> Some ('*', 2, `Bold)
      | '_' when pos + 1 < limit && Char.equal s.[pos + 1] '_' -> Some ('_', 2, `Bold)
      | '*' -> Some ('*', 1, `Italic)
      | '_' -> Some ('_', 1, `Italic)
      | _ -> None)
  ;;

  let can_open_marker (s : string) ~pos ~len ~limit ~ch =
    let next =
      let i = pos + len in
      if i < limit then Some s.[i] else None
    in
    let prev = if pos > 0 then Some s.[pos - 1] else None in
    (not (is_escaped s pos))
    && Option.value_map next ~default:false ~f:(fun c -> not (Char.is_whitespace c))
    && not (is_intraword ~prev ~next)
  ;;

  let can_close_marker (s : string) ~pos ~len ~limit =
    let prev = if pos > 0 then Some s.[pos - 1] else None in
    let next =
      let i = pos + len in
      if i < limit then Some s.[i] else None
    in
    (not (is_escaped s pos))
    && Option.value_map prev ~default:false ~f:(fun c -> not (Char.is_whitespace c))
    && not (is_intraword ~prev ~next)
  ;;

  let find_next_open_marker (s : string) ~pos ~limit =
    let rec loop i =
      if i >= limit
      then None
      else (
        match marker_at s ~pos:i ~limit with
        | Some (ch, len, kind) when can_open_marker s ~pos:i ~len ~limit ~ch ->
          if Int.equal len 1 && i > pos && i > 0 && Char.equal s.[i - 1] ch
          then loop (i + 1)
          else Some (i, ch, len, kind)
        | _ -> loop (i + 1))
    in
    loop pos
  ;;

  let is_marker_at (s : string) ~pos ~len ~limit ~ch =
    pos + len <= limit
    && List.for_all (List.init len ~f:Fn.id) ~f:(fun k -> Char.equal s.[pos + k] ch)
  ;;

  let find_close_marker (s : string) ~from ~limit ~ch ~len =
    let rec loop i =
      if i + len > limit
      then None
      else if
        is_marker_at s ~pos:i ~len ~limit ~ch && can_close_marker s ~pos:i ~len ~limit
      then Some i
      else loop (i + 1)
    in
    loop from
  ;;

  let apply_style style = function
    | `Bold -> { style with bold = true }
    | `Italic -> { style with italic = true }
  ;;

  let rec parse_emphasis_range (s : string) ~style ~pos ~limit =
    match find_next_open_marker s ~pos ~limit with
    | None ->
      let text = String.sub s ~pos ~len:(limit - pos) in
      if String.is_empty text then [] else [ fallback_attr_of_style style, text ]
    | Some (open_pos, ch, len, kind) ->
      let before = String.sub s ~pos ~len:(open_pos - pos) in
      let open_end = open_pos + len in
      (match find_close_marker s ~from:open_end ~limit ~ch ~len with
       | None ->
         let rest = parse_emphasis_range s ~style ~pos:open_end ~limit in
         let marker = String.make len ch in
         List.concat
           [ (if String.is_empty before
              then []
              else [ fallback_attr_of_style style, before ])
           ; [ fallback_attr_of_style style, marker ]
           ; rest
           ]
       | Some close_pos when close_pos = open_end ->
         let marker = String.make len ch in
         let rest = parse_emphasis_range s ~style ~pos:open_end ~limit in
         List.concat
           [ (if String.is_empty before
              then []
              else [ fallback_attr_of_style style, before ])
           ; [ fallback_attr_of_style style, marker ]
           ; rest
           ]
       | Some close_pos ->
         let inner =
           parse_emphasis_range
             s
             ~style:(apply_style style kind)
             ~pos:open_end
             ~limit:close_pos
         in
         let rest = parse_emphasis_range s ~style ~pos:(close_pos + len) ~limit in
         List.concat
           [ (if String.is_empty before
              then []
              else [ fallback_attr_of_style style, before ])
           ; inner
           ; rest
           ])
  ;;

  let fallback_emphasis_spans s =
    parse_emphasis_range
      s
      ~style:{ bold = false; italic = false }
      ~pos:0
      ~limit:(String.length s)
  ;;

  let fallback_markdown_spans (para : string) : (A.t * string) list =
    Markdown_fences.split_inline para
    |> List.concat_map ~f:(function
      | Markdown_fences.Inline_text s -> fallback_emphasis_spans s
      | Markdown_fences.Inline_code code ->
        if String.is_empty code then [] else [ fallback_inline_code_attr, code ])
  ;;

  let flatten_highlighted_lines lines =
    match lines with
    | [ xs ] -> xs
    | xs -> List.concat xs
  ;;

  let compress_adjacent_spans spans =
    List.fold spans ~init:[] ~f:(fun acc (a, s) ->
      match acc with
      | (a', s') :: tl when phys_equal a a' -> (a, s' ^ s) :: tl
      | _ -> (a, s) :: acc)
    |> List.rev
  ;;

  let markdown_spans (ctx : t) ~(para : string) : (A.t * string) list =
    let lines, info =
      Highlight_tm_engine.highlight_text_with_scopes_with_info
        ctx.hi_engine
        ~lang:(Some "markdown")
        ~text:para
    in
    let spans =
      match info.Highlight_tm_engine.fallback with
      | None ->
        lines
        |> List.map ~f:suppress_markdown_delimiters
        |> flatten_highlighted_lines
        |> List.map ~f:(fun s -> s.attr, s.text)
      | Some _ ->
        let spans = fallback_markdown_spans para in
        if List.is_empty spans then [ A.empty, para ] else spans
    in
    let spans =
      if is_read_directory ctx
      then (
        let dir_attr = Styles.fg_gray 13 in
        List.map spans ~f:(fun (_a, s) -> dir_attr, s))
      else spans
    in
    compress_adjacent_spans spans
  ;;

  let render_markdown (ctx : t) ~(is_first : bool) ~(para : string) : I.t list =
    let blank = I.hsnap ~align:`Left ctx.width (I.string A.empty "") in
    if String.is_empty para
    then [ blank ]
    else (
      let spans = markdown_spans ctx ~para in
      if List.is_empty spans then [ blank ] else spans |> render_runs ctx ~is_first)
  ;;

  let open_paren_index para = String.lfindi para ~f:(fun _ c -> Char.( = ) c '(')

  let split_at_open para open_idx =
    let prefix = String.sub para ~pos:0 ~len:open_idx in
    let total_len = String.length para in
    if open_idx + 1 > total_len
    then None
    else (
      let after_open_len = total_len - open_idx - 1 in
      let after_open = String.sub para ~pos:(open_idx + 1) ~len:after_open_len in
      Some (prefix, after_open))
  ;;

  let name_and_ws prefix =
    let prefix_trimmed = String.rstrip prefix in
    if String.is_empty prefix_trimmed
    then None
    else (
      let ws_len = String.length prefix - String.length prefix_trimmed in
      let ws_after_name =
        if ws_len > 0
        then String.sub prefix ~pos:(String.length prefix_trimmed) ~len:ws_len
        else ""
      in
      Some (prefix_trimmed, ws_after_name))
  ;;

  let args_and_closing after_open =
    let len_after = String.length after_open in
    if len_after > 0 && Char.(String.get after_open (len_after - 1) = ')')
    then String.sub after_open ~pos:0 ~len:(len_after - 1), ")"
    else after_open, ""
  ;;

  let tool_call_parts (para : string) =
    match open_paren_index para with
    | None -> None
    | Some open_idx ->
      (match split_at_open para open_idx with
       | None -> None
       | Some (prefix, after_open) ->
         (match name_and_ws prefix with
          | None -> None
          | Some (name, ws_after_name) ->
            let args, closing = args_and_closing after_open in
            Some (name, ws_after_name, args, closing)))
  ;;

  let tool_call_spans (ctx : t) ~(para : string) : (A.t * string) list option =
    let base_attr = Theme.attr_of_role ctx.role in
    match tool_call_parts para with
    | None -> None
    | Some (name, ws_after_name, args, closing) ->
      let tool_name_attr = Styles.(base_attr ++ bold ++ fg_hex "#FFCC66") in
      let name_spans = if String.is_empty name then [] else [ tool_name_attr, name ] in
      let ws_spans =
        if String.is_empty ws_after_name then [] else [ base_attr, ws_after_name ]
      in
      let open_paren_spans = [ base_attr, "(" ] in
      let args_spans =
        if String.is_empty args
        then []
        else
          Highlight_tm_engine.highlight_text ctx.hi_engine ~lang:(Some "json") ~text:args
          |> List.concat
      in
      let closing_spans =
        if String.is_empty closing then [] else [ base_attr, closing ]
      in
      Some
        (List.concat
           [ name_spans; ws_spans; open_paren_spans; args_spans; closing_spans ])
  ;;

  let render_paragraph (ctx : t) ~(is_first : bool) ~(para : string) : I.t list =
    let is_tool_call = String.equal ctx.role "tool" && Option.is_none ctx.tool_output in
    if (not is_tool_call) || String.is_empty para
    then render_markdown ctx ~is_first ~para
    else (
      match tool_call_spans ctx ~para with
      | None -> render_markdown ctx ~is_first ~para
      | Some spans -> spans |> render_runs ctx ~is_first)
  ;;

  let highlight_lines (ctx : t) ~(lang : string option) ~(text : string) =
    Highlight_tm_engine.highlight_text ctx.hi_engine ~lang ~text
  ;;

  let render_code_row ~selected ~w line_spans =
    List.map line_spans ~f:(fun (a, s) ->
      safe_string (if selected then A.(a ++ st reverse) else a) s)
    |> I.hcat
    |> I.hsnap ~align:`Left w
  ;;

  let render_code_content (ctx : t) ~w ~selected ~(lang : string option) ~(code : string) =
    highlight_lines ctx ~lang ~text:code
    |> List.map ~f:(render_code_row ~selected ~w)
    |> I.vcat
  ;;

  let prefix_attr (ctx : t) =
    Theme.attr_of_role ctx.role
    |> fun a -> if ctx.selected then Theme.selection_attr a else a
  ;;

  let render_code_block_no_space
        (ctx : t)
        ~(first_pref : string)
        ~(indent : string)
        ~(lang : string option)
        ~(code : string)
    =
    highlight_lines ctx ~lang ~text:code
    |> List.mapi ~f:(fun i line_spans ->
      let pref = if Int.equal i 0 then first_pref else indent in
      let content_img =
        List.map line_spans ~f:(fun (a, s) ->
          safe_string (if ctx.selected then A.(a ++ st reverse) else a) s)
        |> I.hcat
      in
      Notty.Infix.(safe_string (prefix_attr ctx) pref <|> content_img)
      |> I.hsnap ~align:`Left ctx.width)
  ;;

  let cached_code_content
        (ctx : t)
        ~(klass : Code_cache2.role_class)
        ~(lang : string option)
        ~(code : string)
        ~(content_w_first : int)
    =
    if ctx.selected
    then render_code_content ctx ~w:content_w_first ~selected:true ~lang ~code
    else (
      let bucket = Code_cache2.bucket_for_width content_w_first in
      let digest =
        Md5.(to_hex (digest_string (Option.value lang ~default:"-" ^ "\x00" ^ code)))
      in
      match Code_cache2.get ~klass ~lang ~digest ~wb:bucket with
      | Some img -> img
      | None ->
        let img = render_code_content ctx ~w:bucket ~selected:false ~lang ~code in
        Code_cache2.set ~klass ~lang ~digest ~wb:bucket img;
        img)
  ;;

  let prefix_image (ctx : t) ~(first_pref : string) ~(indent : string) ~height =
    let base_attr = prefix_attr ctx in
    let row0 = safe_string base_attr first_pref in
    let rowi = safe_string base_attr indent in
    if height <= 0
    then I.empty
    else I.vcat (row0 :: List.init (height - 1) ~f:(fun _ -> rowi))
  ;;

  let render_code_block
        (ctx : t)
        ~(is_first : bool)
        ~(lang : string option)
        ~(code : string)
        ~(klass : Code_cache2.role_class)
    : I.t list
    =
    let first_pref0 = prefix_first ctx in
    let indent = prefix_cont ctx in
    let first_pref = if is_first then first_pref0 else indent in
    let content_w_first = Int.max 0 (ctx.width - String.length first_pref) in
    if content_w_first <= 0
    then render_code_block_no_space ctx ~first_pref ~indent ~lang ~code
    else (
      let content_img = cached_code_content ctx ~klass ~lang ~code ~content_w_first in
      let prefix_img =
        prefix_image ctx ~first_pref ~indent ~height:(I.height content_img)
      in
      [ Notty.Infix.(prefix_img <|> content_img) |> I.hsnap ~align:`Left ctx.width ])
  ;;
end

module Message = struct
  open Render_context

  let sanitize_developer role text =
    if String.equal role "developer"
    then (
      let s = String.lstrip text in
      let label = role ^ ":" in
      let s_lower = String.lowercase s in
      let label_lower = String.lowercase label in
      if String.is_prefix s_lower ~prefix:label_lower
      then String.lstrip (String.drop_prefix s (String.length label))
      else text)
    else text
  ;;

  let header_attr (ctx : Render_context.t) =
    let base_attr = Theme.attr_of_role ctx.role in
    if ctx.selected then Theme.selection_attr base_attr else base_attr
  ;;

  let icon_of_role = function
    | "assistant" -> "💡 "
    | "user" -> "🙋 "
    | "developer" -> "🧑‍💻 "
    | "tool" -> "🛠  "
    | "system" -> "🛡 "
    | "reasoning" -> "🧠 "
    | "tool_output" -> "📬 "
    | "fork" -> "🌿 "
    | "error" -> "❌ "
    | _ -> ""
  ;;

  let capitalise_first s =
    if String.is_empty s
    then s
    else String.mapi s ~f:(fun i c -> if Int.equal i 0 then Char.uppercase c else c)
  ;;

  let render_header_line (ctx : Render_context.t) : I.t =
    let icon = icon_of_role ctx.role in
    let label = Roles.label_of_role ctx.role |> capitalise_first in
    safe_string (header_attr ctx) (icon ^ label) |> I.hsnap ~align:`Left ctx.width
  ;;

  let render_paras (ctx : Render_context.t) ~(first_row : bool ref) ~(text : string) =
    String.split_lines text
    |> List.concat_map ~f:(fun para ->
      let rs = Paint.render_paragraph ctx ~is_first:!first_row ~para in
      if not (List.is_empty rs) then first_row := false;
      rs)
  ;;

  let render_code
        (ctx : Render_context.t)
        ~(first_row : bool ref)
        ~(klass : Code_cache2.role_class)
        ~(advance_first : bool)
        ~(lang : string option)
        ~(code : string)
    =
    let rs = Paint.render_code_block ctx ~is_first:!first_row ~lang ~code ~klass in
    if advance_first && not (List.is_empty rs) then first_row := false;
    rs
  ;;

  let render_body_default (ctx : Render_context.t) ~(role : string) ~(text : string)
    : I.t list
    =
    let blocks = Blocks.of_message_text text in
    let first_row = ref true in
    List.concat_map blocks ~f:(function
      | Blocks.Text s | Blocks.Code { lang = Some "html"; code = s } ->
        render_paras ctx ~first_row ~text:s
      | Blocks.Code { lang; code } ->
        let klass =
          if Roles.is_toollike role then Code_cache2.Toollike else Code_cache2.Userlike
        in
        render_code
          ctx
          ~first_row
          ~klass
          ~advance_first:(not (Roles.is_toollike role))
          ~lang
          ~code)
  ;;

  let split_status_and_patch (lines : string list) =
    let rec loop acc = function
      | [] -> List.rev acc, []
      | ("" as l) :: rest -> List.rev (l :: acc), rest
      | l :: rest -> loop (l :: acc) rest
    in
    loop [] lines
  ;;

  let render_body_apply_patch (ctx : Render_context.t) ~(role : string) ~(text : string)
    : I.t list
    =
    let status_lines, patch_lines = String.split_lines text |> split_status_and_patch in
    let first_row = ref true in
    let status_rows =
      List.concat_map status_lines ~f:(fun para -> render_paras ctx ~first_row ~text:para)
    in
    let patch_rows =
      match patch_lines with
      | [] -> []
      | _ ->
        let code = String.concat ~sep:"\n" patch_lines in
        let klass =
          if Roles.is_toollike role then Code_cache2.Toollike else Code_cache2.Userlike
        in
        render_code
          ctx
          ~first_row
          ~klass
          ~advance_first:true
          ~lang:(Some "ochat-apply-patch")
          ~code
    in
    status_rows @ patch_rows
  ;;

  let render_body_read_file
        (ctx : Render_context.t)
        ~(role : string)
        ~(text : string)
        ~(path : string option)
    : I.t list
    =
    match path with
    | None -> render_body_default ctx ~role ~text
    | Some p ->
      (match lang_of_path p with
       | None -> render_body_default ctx ~role ~text
       | Some "markdown" -> render_body_default ctx ~role ~text
       | Some lang ->
         let klass =
           if Roles.is_toollike role then Code_cache2.Toollike else Code_cache2.Userlike
         in
         Paint.render_code_block ctx ~is_first:true ~lang:(Some lang) ~code:text ~klass)
  ;;

  let blank_row (ctx : Render_context.t) =
    I.hsnap ~align:`Left ctx.width (I.string A.empty "")
  ;;

  let gap_row (ctx : Render_context.t) =
    I.hsnap ~align:`Left ctx.width (I.string A.empty " ")
  ;;

  let render_body_rows (ctx : Render_context.t) ~(role : string) ~(text : string) =
    match ctx.tool_output with
    | Some Apply_patch -> render_body_apply_patch ctx ~role ~text
    | Some (Read_file { path }) -> render_body_read_file ctx ~role ~text ~path
    | _ -> render_body_default ctx ~role ~text
  ;;

  let render (ctx : Render_context.t) ((role, text) : message) : I.t =
    let text = Util.sanitize ~strip:false text |> sanitize_developer role in
    let trimmed = String.strip text in
    if String.is_empty trimmed
    then I.empty
    else (
      let body_rows = render_body_rows ctx ~role ~text in
      I.vcat
        ((blank_row ctx :: render_header_line ctx :: blank_row ctx :: body_rows)
         @ [ gap_row ctx ]))
  ;;
end

let render_message ~width ~selected ~tool_output ~role ~text ~hi_engine =
  let ctx = Render_context.make ~width ~selected ~role ~tool_output ~hi_engine in
  Message.render ctx (role, text)
;;

let render_header_line ~width ~selected ~role ~hi_engine =
  let ctx = Render_context.make ~width ~selected ~role ~tool_output:None ~hi_engine in
  Message.render_header_line ctx
;;
