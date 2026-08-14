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
    | role when String.is_suffix role ~suffix:" Agent" -> Styles.fg_hex "#13A3F2"
    | _ -> A.empty
  ;;

  let selection_attr base = A.(base ++ Styles.bg_gray 23)
end

let tool_call_status = function
  | Ochat_function.Trace.Returned -> Styles.fg_green, "✓ Returned"
  | Raised -> Styles.fg_red, "✗ Raised"
  | Cancelled -> Styles.fg_yellow, "⊘ Cancelled"
;;

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

module Search_highlight = struct
  (* Apply a background only to substrings matching [query] within the
     concatenated visible text of [spans]. Case-insensitive substring match. *)

  let normalize s = String.lowercase s

  let find_all ~haystack ~needle : (int * int) list =
    if String.is_empty needle
    then []
    else (
      let h = normalize haystack in
      let n = normalize needle in
      let nlen = String.length n in
      let rec loop acc pos =
        match String.substr_index h ~pos ~pattern:n with
        | None -> List.rev acc
        | Some i -> loop ((i, i + nlen) :: acc) (i + nlen)
      in
      loop [] 0)
  ;;

  let apply_to_spans
        ~(query : string)
        ~(hit_attr : Notty.A.t)
        (spans : (A.t * string) list)
    : (A.t * string) list
    =
    if String.is_empty (String.strip query)
    then spans
    else (
      let rendered = spans |> List.map ~f:snd |> String.concat in
      let ranges = find_all ~haystack:rendered ~needle:(String.strip query) in
      if List.is_empty ranges
      then spans
      else (
        (* ranges are sorted, non-overlapping (because we advance by needle length) *)
        let ranges = ref ranges in
        let take_ranges_for_segment ~seg_start ~seg_stop =
          (* Consume from [ranges] those that intersect [seg_start, seg_stop). *)
          let rec consume acc =
            match !ranges with
            | [] -> List.rev acc
            | (a, b) :: tl ->
              if b <= seg_start
              then (
                ranges := tl;
                consume acc)
              else if a >= seg_stop
              then List.rev acc
              else (
                (* intersection exists; keep it but do not drop it entirely
                   unless it ends within the segment. *)
                let a' = Int.max a seg_start in
                let b' = Int.min b seg_stop in
                let acc = (a', b') :: acc in
                if b <= seg_stop
                then (
                  ranges := tl;
                  consume acc)
                else List.rev acc)
          in
          consume []
        in
        let rec split_text ~base_attr ~global_off s =
          let len = String.length s in
          let seg_start = global_off in
          let seg_stop = global_off + len in
          let rs = take_ranges_for_segment ~seg_start ~seg_stop in
          if List.is_empty rs
          then [ base_attr, s ]
          else (
            (* Build pieces in increasing order *)
            let rec build acc cursor = function
              | [] ->
                if cursor < seg_stop
                then (
                  let piece =
                    String.sub s ~pos:(cursor - seg_start) ~len:(seg_stop - cursor)
                  in
                  List.rev ((base_attr, piece) :: acc))
                else List.rev acc
              | (a, b) :: tl ->
                let acc =
                  if cursor < a
                  then (
                    let piece =
                      String.sub s ~pos:(cursor - seg_start) ~len:(a - cursor)
                    in
                    (base_attr, piece) :: acc)
                  else acc
                in
                let hit_piece = String.sub s ~pos:(a - seg_start) ~len:(b - a) in
                let acc = (A.(base_attr ++ hit_attr), hit_piece) :: acc in
                build acc b tl
            in
            build [] seg_start rs)
        in
        let rec loop acc off = function
          | [] -> List.rev acc
          | (a, s) :: tl ->
            let pieces = split_text ~base_attr:a ~global_off:off s in
            let off = off + String.length s in
            loop (List.rev_append pieces acc) off tl
        in
        loop [] 0 spans))
  ;;
end

module Spans = struct
  type run = Notty.A.t * string
  type line = run list
end

module Render_job = Chat_message_render_job

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

  let first_width = function
    | [] -> 0
    | (_, text) :: _ ->
      Option.value_map (next_piece text 0) ~default:0 ~f:(fun (piece, _) ->
        width_of_text piece)
  ;;

  let layout_plan lines =
    let widths =
      List.map lines ~f:(fun line ->
        List.sum (module Int) line ~f:(fun (_, text) -> width_of_text text))
    in
    let min_width = List.max_elt widths ~compare:Int.compare |> Option.value ~default:0 in
    let rec boundary_limits acc = function
      | line :: (next_line :: _ as rest) ->
        let width = List.sum (module Int) line ~f:(fun (_, text) -> width_of_text text) in
        let next_width = first_width next_line in
        let acc = if next_width <= 0 then acc else (width + next_width - 1) :: acc in
        boundary_limits acc rest
      | [] | [ _ ] -> acc
    in
    let max_width = boundary_limits [] lines |> List.min_elt ~compare:Int.compare in
    Render_job.Layout_plan.{ min_width; max_width }
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

module Code_cache = Render_job.Code_cache
module Highlight_cache = Render_job.Highlight_cache
module Prepared = Render_job.Prepared_message
module Prepared_cache = Render_job.Prepared_cache
module Wrapped_cache = Render_job.Wrapped_cache

let shared_code_cache = Code_cache.create ~capacity:128
let shared_highlight_cache = Highlight_cache.create ~capacity:128
let shared_prepared_cache = Prepared_cache.create ~capacity:128
let shared_wrapped_cache = Wrapped_cache.create ~capacity:256

let clear_code_cache () =
  Code_cache.clear shared_code_cache;
  Highlight_cache.clear shared_highlight_cache;
  Prepared_cache.clear shared_prepared_cache;
  Wrapped_cache.clear shared_wrapped_cache
;;

module Render_context = struct
  type t =
    { width : int
    ; role : string
    ; tool_output : tool_output_kind option
    ; runtime : Render_job.Runtime.t
    ; grammar_generation : int
    ; tool_call_outcome : Ochat_function.Trace.outcome option
    ; mutable layout_plan : Render_job.Layout_plan.t
    }

  let make ~width ~role ~tool_output ~runtime ~grammar_generation ~tool_call_outcome =
    { width
    ; role
    ; tool_output
    ; runtime
    ; grammar_generation
    ; tool_call_outcome
    ; layout_plan = Render_job.Layout_plan.unconstrained
    }
  ;;

  let prefix_first _t = ""
  let prefix_cont _t = ""

  let note_layout_plan t plan =
    t.layout_plan <- Render_job.Layout_plan.intersect t.layout_plan plan
  ;;
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

  let first_prefix (ctx : t) ~(is_first : bool) =
    if is_first then prefix_first ctx else prefix_cont ctx
  ;;

  let cont_prefix (ctx : t) = prefix_cont ctx
  let paragraph_limit (ctx : t) = Int.max 1 (ctx.width - String.length (prefix_first ctx))

  let render_runs (ctx : t) ~(is_first : bool) (runs : Spans.run list)
    : Render_job.Layout.line list
    =
    let limit = paragraph_limit ctx in
    let key =
      let text = List.map runs ~f:snd |> String.concat in
      String.concat
        ~sep:"|"
        [ ctx.role
        ; Int.to_string limit
        ; Md5.(to_hex (digest_string text))
        ; Int.to_string (Stdlib.Hashtbl.hash runs)
        ]
    in
    let wrapped =
      match Render_job.Runtime.wrapped_cache ctx.runtime with
      | None -> Wrap.wrap_runs ~limit runs
      | Some cache ->
        (match Wrapped_cache.find cache key with
         | Some wrapped -> wrapped
         | None ->
           let wrapped = Wrap.wrap_runs ~limit runs in
           Wrapped_cache.set cache key wrapped;
           wrapped)
    in
    Render_context.note_layout_plan ctx (Wrap.layout_plan wrapped);
    let render_line ~pref line_runs =
      (if String.is_empty pref then [] else [ A.empty, pref ]) @ line_runs
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

  let markdown_spans
        (ctx : t)
        ~(para : string)
        ~(fallback_spans : (A.t * string) list option)
    =
    let lines, info =
      let theme_generation = Render_job.Runtime.theme_generation ctx.runtime in
      let grammar_generation = Render_job.Runtime.grammar_generation ctx.runtime in
      match Render_job.Runtime.highlight_cache ctx.runtime with
      | None ->
        Highlight_tm_engine.highlight_text_with_scopes_with_info_interruptible
          (Render_job.Runtime.hi_engine ctx.runtime)
          ~is_cancelled:(Render_job.Runtime.is_cancelled ctx.runtime)
          ~lang:(Some "markdown")
          ~text:para
      | Some cache ->
        (match
           Highlight_cache.find_scoped
             cache
             ~theme_generation
             ~grammar_generation
             ~lang:(Some "markdown")
             ~text:para
         with
         | Some result -> result
         | None ->
           let result =
             Highlight_tm_engine.highlight_text_with_scopes_with_info_interruptible
               (Render_job.Runtime.hi_engine ctx.runtime)
               ~is_cancelled:(Render_job.Runtime.is_cancelled ctx.runtime)
               ~lang:(Some "markdown")
               ~text:para
           in
           Highlight_cache.set_scoped
             cache
             ~theme_generation
             ~grammar_generation
             ~lang:(Some "markdown")
             ~text:para
             result;
           result)
    in
    let spans =
      match info.Highlight_tm_engine.fallback with
      | None ->
        lines
        |> List.map ~f:suppress_markdown_delimiters
        |> flatten_highlighted_lines
        |> List.map ~f:(fun s -> s.attr, s.text)
      | Some _ ->
        let spans = Option.value fallback_spans ~default:(fallback_markdown_spans para) in
        if List.is_empty spans then [ A.empty, para ] else spans
    in
    let spans =
      if is_read_directory ctx
      then (
        let dir_attr = Styles.fg_gray 13 in
        List.map spans ~f:(fun (_a, s) -> dir_attr, s))
      else spans
    in
    let spans = compress_adjacent_spans spans in
    spans
  ;;

  let render_markdown (ctx : t) ~(is_first : bool) ~(para : string) ?fallback_spans () =
    let blank = [] in
    if String.is_empty para
    then [ blank ]
    else (
      let spans = markdown_spans ctx ~para ~fallback_spans in
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

  let rec tool_call_spans (ctx : t) ~(para : string) ?parts () =
    let base_attr = Theme.attr_of_role ctx.role in
    match Option.value parts ~default:(tool_call_parts para) with
    | None -> None
    | Some (name, ws_after_name, args, closing) ->
      let tool_name_attr = Styles.(base_attr ++ bold ++ fg_hex "#FFCC66") in
      let name_spans = if String.is_empty name then [] else [ tool_name_attr, name ] in
      let ws_spans =
        if String.is_empty ws_after_name then [] else [ base_attr, ws_after_name ]
      in
      let open_paren_spans = [ base_attr, "(" ] in
      let args_lines = if String.is_empty args then [ [] ] else json_lines ctx args in
      let lines =
        match args_lines with
        | [] ->
          [ List.concat
              [ name_spans
              ; ws_spans
              ; open_paren_spans
              ; (if String.is_empty closing then [] else [ base_attr, closing ])
              ]
          ]
        | first :: rest ->
          let first = List.concat [ name_spans; ws_spans; open_paren_spans; first ] in
          let lines = first :: rest in
          if String.is_empty closing
          then lines
          else
            List.rev lines
            |> (function
             | [] -> [ [ base_attr, closing ] ]
             | last :: previous -> List.rev ((last @ [ base_attr, closing ]) :: previous))
      in
      Some lines

  and highlight_lines (ctx : t) ~(lang : string option) ~(text : string) =
    let theme_generation = Render_job.Runtime.theme_generation ctx.runtime in
    let grammar_generation = Render_job.Runtime.grammar_generation ctx.runtime in
    match Render_job.Runtime.highlight_cache ctx.runtime with
    | None ->
      Highlight_tm_engine.highlight_text_interruptible
        (Render_job.Runtime.hi_engine ctx.runtime)
        ~is_cancelled:(Render_job.Runtime.is_cancelled ctx.runtime)
        ~lang
        ~text
    | Some cache ->
      (match
         Highlight_cache.find_plain
           cache
           ~theme_generation
           ~grammar_generation
           ~lang
           ~text
       with
       | Some lines -> lines
       | None ->
         let lines =
           Highlight_tm_engine.highlight_text_interruptible
             (Render_job.Runtime.hi_engine ctx.runtime)
             ~is_cancelled:(Render_job.Runtime.is_cancelled ctx.runtime)
             ~lang
             ~text
         in
         Highlight_cache.set_plain
           cache
           ~theme_generation
           ~grammar_generation
           ~lang
           ~text
           lines;
         lines)

  and json_lines ctx text =
    let lines = highlight_lines ctx ~lang:(Some "json") ~text in
    let has_syntax_attributes =
      List.exists lines ~f:(List.exists ~f:(fun (attr, _) -> not (A.equal attr A.empty)))
    in
    if has_syntax_attributes then lines else Renderer_json_highlight.highlight text
  ;;

  let code_lines (ctx : t) ~(lang : string option) ~(code : string) =
    String.split_lines code
    |> List.map ~f:(fun line ->
      if String.is_empty line
      then []
      else highlight_lines ctx ~lang ~text:line |> List.concat)
  ;;

  let render_paragraph
        (ctx : t)
        ~(is_first : bool)
        ~(para : string)
        ?fallback_spans
        ~(parts : (string * string * string * string) option)
        ()
    =
    let is_tool_call =
      (String.equal ctx.role "tool" || String.is_suffix ctx.role ~suffix:" Agent")
      && Option.is_none ctx.tool_output
    in
    if (not is_tool_call) || String.is_empty para
    then render_markdown ctx ~is_first ~para ?fallback_spans ()
    else (
      match tool_call_spans ctx ~para ~parts () with
      | None -> render_markdown ctx ~is_first ~para ?fallback_spans ()
      | Some lines ->
        List.concat_mapi lines ~f:(fun index spans ->
          render_runs ctx ~is_first:(is_first && Int.equal index 0) spans))
  ;;

  let render_code_block_no_space
        (ctx : t)
        ~(first_pref : string)
        ~(indent : string)
        ~(lang : string option)
        ~(code : string)
    =
    code_lines ctx ~lang ~code
    |> List.mapi ~f:(fun i line_spans ->
      let pref = if Int.equal i 0 then first_pref else indent in
      (if String.is_empty pref then [] else [ Theme.attr_of_role ctx.role, pref ])
      @ line_spans)
  ;;

  let cached_code_content
        (ctx : t)
        ~(role_class : Code_cache.role_class)
        ~(lang : string option)
        ~(code : string)
        ~(content_w_first : int)
    =
    match Render_job.Runtime.code_cache ctx.runtime with
    | None -> code_lines ctx ~lang ~code
    | Some cache ->
      (match
         Code_cache.find
           cache
           ~role_class
           ~grammar_generation:ctx.grammar_generation
           ~lang
           ~code
           ~width:content_w_first
       with
       | Some lines -> lines
       | None ->
         let lines = code_lines ctx ~lang ~code in
         Code_cache.set
           cache
           ~role_class
           ~grammar_generation:ctx.grammar_generation
           ~lang
           ~code
           ~width:content_w_first
           lines;
         lines)
  ;;

  let render_code_block
        (ctx : t)
        ~(is_first : bool)
        ~(lang : string option)
        ~(code : string)
        ~(role_class : Code_cache.role_class)
    : Render_job.Layout.line list
    =
    let first_pref0 = prefix_first ctx in
    let indent = prefix_cont ctx in
    let first_pref = if is_first then first_pref0 else indent in
    let content_w_first = Int.max 0 (ctx.width - String.length first_pref) in
    if content_w_first <= 0
    then render_code_block_no_space ctx ~first_pref ~indent ~lang ~code
    else (
      let content_lines =
        cached_code_content ctx ~role_class ~lang ~code ~content_w_first
      in
      List.mapi content_lines ~f:(fun index line ->
        let prefix = if Int.equal index 0 then first_pref else indent in
        (if String.is_empty prefix then [] else [ Theme.attr_of_role ctx.role, prefix ])
        @ line))
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
    | role when String.is_suffix role ~suffix:" Agent" -> "🛠  "
    | _ -> ""
  ;;

  let capitalise_first s =
    if String.is_empty s
    then s
    else String.mapi s ~f:(fun i c -> if Int.equal i 0 then Char.uppercase c else c)
  ;;

  let render_header_line (ctx : Render_context.t) : Render_job.Layout.line =
    let icon = icon_of_role ctx.role in
    let label = Roles.label_of_role ctx.role |> capitalise_first in
    [ Theme.attr_of_role ctx.role, icon ^ label ]
  ;;

  let render_paras (ctx : Render_context.t) ~(first_row : bool ref) ~(text : string) =
    String.split_lines text
    |> List.concat_map ~f:(fun para ->
      let rs = Paint.render_paragraph ctx ~is_first:!first_row ~para ~parts:None () in
      if not (List.is_empty rs) then first_row := false;
      rs)
  ;;

  let render_code
        (ctx : Render_context.t)
        ~(first_row : bool ref)
        ~(role_class : Code_cache.role_class)
        ~(advance_first : bool)
        ~(lang : string option)
        ~(code : string)
    =
    let rs = Paint.render_code_block ctx ~is_first:!first_row ~lang ~code ~role_class in
    if advance_first && not (List.is_empty rs) then first_row := false;
    rs
  ;;

  let render_body_default (ctx : Render_context.t) ~(role : string) ~(text : string)
    : Render_job.Layout.line list
    =
    let blocks = Blocks.of_message_text text in
    let first_row = ref true in
    List.concat_map blocks ~f:(function
      | Blocks.Text s -> render_paras ctx ~first_row ~text:s
      | Blocks.Code { lang; code } ->
        let role_class =
          if Roles.is_toollike role then Code_cache.Toollike else Code_cache.Userlike
        in
        render_code
          ctx
          ~first_row
          ~role_class
          ~advance_first:(not (Roles.is_toollike role))
          ~lang
          ~code)
  ;;

  let is_patch_start line =
    let line = String.strip line in
    List.exists
      [ "*** Begin Patch"
      ; "*** Add File: "
      ; "*** Update File: "
      ; "*** Delete File: "
      ; "*** Move to: "
      ; "┏━["
      ]
      ~f:(fun prefix -> String.is_prefix line ~prefix)
  ;;

  let split_status_and_patch (lines : string list) =
    let status, patch = List.split_while lines ~f:(Fn.non is_patch_start) in
    let status =
      List.rev status
      |> List.drop_while ~f:(fun line -> String.is_empty (String.strip line))
      |> List.rev
    in
    status, patch
  ;;

  let prepare_paragraph text =
    { Prepared.text
    ; fallback_spans = Paint.fallback_markdown_spans text
    ; tool_call_parts = Paint.tool_call_parts text
    }
  ;;

  let prepare_paragraphs text = String.split_lines text |> List.map ~f:prepare_paragraph

  let prepare_default ~role text =
    let is_tool_call =
      String.equal role "tool" || String.is_suffix role ~suffix:" Agent"
    in
    match if is_tool_call then Paint.tool_call_parts text else None with
    | Some _ -> [ Prepared.Text [ prepare_paragraph text ] ]
    | None ->
      Blocks.of_message_text text
      |> List.map ~f:(function
        | Blocks.Text text -> Prepared.Text (prepare_paragraphs text)
        | Blocks.Code { lang; code } -> Prepared.Code { lang; code })
  ;;

  let prepare_body ~role ~tool_output text =
    match tool_output with
    | Some Apply_patch ->
      let status_lines, patch_lines = String.split_lines text |> split_status_and_patch in
      let patch =
        match patch_lines with
        | [] -> None
        | _ -> Some (String.concat ~sep:"\n" patch_lines)
      in
      Prepared.Apply_patch { status = List.map status_lines ~f:prepare_paragraph; patch }
    | Some (Read_file { path = Some path }) ->
      (match lang_of_path path with
       | Some lang when not (String.equal lang "markdown") ->
         Prepared.Read_file { lang; code = text }
       | None | Some _ -> Prepared.Default (prepare_default ~role text))
    | None | Some (Read_file { path = None } | Read_directory _ | Other _) ->
      Prepared.Default (prepare_default ~role text)
  ;;

  let prepare ~role ~text ~tool_output =
    let text = Util.sanitize ~strip:false text |> sanitize_developer role in
    { Prepared.role; text; body = prepare_body ~role ~tool_output text }
  ;;

  let render_prepared_paragraph ctx ~first_row paragraph =
    let is_tool_call =
      (String.equal ctx.role "tool" || String.is_suffix ctx.role ~suffix:" Agent")
      && Option.is_none ctx.tool_output
    in
    let rows =
      if is_tool_call && Option.is_some paragraph.Prepared.tool_call_parts
      then
        Paint.render_paragraph
          ctx
          ~is_first:!first_row
          ~para:paragraph.text
          ~fallback_spans:paragraph.fallback_spans
          ~parts:paragraph.tool_call_parts
          ()
      else
        Paint.render_markdown
          ctx
          ~is_first:!first_row
          ~para:paragraph.text
          ~fallback_spans:paragraph.fallback_spans
          ()
    in
    if not (List.is_empty rows) then first_row := false;
    rows
  ;;

  let render_prepared_default ctx ~role blocks =
    let first_row = ref true in
    List.concat_map blocks ~f:(function
      | Prepared.Text paragraphs ->
        List.concat_map paragraphs ~f:(render_prepared_paragraph ctx ~first_row)
      | Prepared.Code { lang; code } ->
        let role_class =
          if Roles.is_toollike role then Code_cache.Toollike else Code_cache.Userlike
        in
        render_code
          ctx
          ~first_row
          ~role_class
          ~advance_first:(not (Roles.is_toollike role))
          ~lang
          ~code)
  ;;

  let render_body_apply_patch
        (ctx : Render_context.t)
        ~(role : string)
        ~(status : Prepared.paragraph list)
        ~(patch : string option)
    =
    let first_row = ref true in
    let status_rows =
      List.concat_map status ~f:(render_prepared_paragraph ctx ~first_row)
    in
    let patch_rows =
      match patch with
      | None -> []
      | Some code ->
        let role_class =
          if Roles.is_toollike role then Code_cache.Toollike else Code_cache.Userlike
        in
        render_code
          ctx
          ~first_row
          ~role_class
          ~advance_first:true
          ~lang:(Some "ochat-apply-patch")
          ~code
    in
    status_rows @ patch_rows
  ;;

  let blank_row (_ctx : Render_context.t) = []
  let gap_row (_ctx : Render_context.t) = [ A.empty, " " ]

  let render_body_rows (ctx : Render_context.t) (prepared : Prepared.t) =
    match prepared.body with
    | Default blocks -> render_prepared_default ctx ~role:prepared.role blocks
    | Apply_patch { status; patch } ->
      render_body_apply_patch ctx ~role:prepared.role ~status ~patch
    | Read_file { lang; code } ->
      let role_class =
        if Roles.is_toollike prepared.role
        then Code_cache.Toollike
        else Code_cache.Userlike
      in
      Paint.render_code_block ctx ~is_first:true ~lang:(Some lang) ~code ~role_class
  ;;

  let render_prepared (ctx : Render_context.t) (prepared : Prepared.t)
    : Render_job.Layout.line list
    =
    let trimmed = String.strip prepared.text in
    if String.is_empty trimmed
    then []
    else (
      let body_rows = render_body_rows ctx prepared in
      let status_rows =
        match ctx.tool_call_outcome with
        | None -> []
        | Some outcome ->
          let attr, text = tool_call_status outcome in
          [ [ attr, text ] ]
      in
      ((blank_row ctx :: render_header_line ctx :: blank_row ctx :: body_rows)
       @ status_rows)
      @ [ gap_row ctx ])
  ;;

  let render ctx ((role, text) : message) =
    prepare ~role ~text ~tool_output:ctx.tool_output |> render_prepared ctx
  ;;
end

let split_lines_by_lengths lines runs =
  let lengths =
    List.map lines ~f:(fun line ->
      List.sum (module Int) line ~f:(fun (_, text) -> String.length text))
  in
  let rec take_bytes remaining taken runs =
    match remaining, runs with
    | 0, _ -> List.rev taken, runs
    | _, [] -> List.rev taken, []
    | remaining, ((attr, text) as run) :: rest ->
      let length = String.length text in
      if length <= remaining
      then take_bytes (remaining - length) (run :: taken) rest
      else (
        let left = String.prefix text remaining in
        let right = String.drop_prefix text remaining in
        List.rev ((attr, left) :: taken), (attr, right) :: rest)
  in
  List.fold_map lengths ~init:runs ~f:(fun runs length ->
    let line, runs = take_bytes length [] runs in
    runs, line)
  |> snd
;;

let apply_overlay_to_lines ~selected ~search_query lines =
  match selected, search_query with
  | true, Some query when not (String.is_empty (String.strip query)) ->
    let highlighted =
      Search_highlight.apply_to_spans
        ~query
        ~hit_attr:(Styles.bg_gray 23)
        (List.concat lines)
    in
    split_lines_by_lengths lines highlighted
  | true, _ ->
    List.map lines ~f:(List.map ~f:(fun (attr, text) -> Theme.selection_attr attr, text))
  | false, _ -> lines
;;

let image_of_lines ~width lines =
  List.map lines ~f:(fun line ->
    match line with
    | [] -> I.void width 1
    | line ->
      List.map line ~f:(fun (attr, text) -> safe_string attr text)
      |> I.hcat
      |> I.hsnap ~align:`Left width)
  |> I.vcat
;;

let apply_overlay ~selected ~search_query (layout : Render_job.Layout.t) =
  apply_overlay_to_lines ~selected ~search_query layout.lines
  |> image_of_lines ~width:layout.width
;;

let render_detached ~(runtime : Render_job.Runtime.t) (job : Render_job.t) =
  let key = job.key in
  if not (Int.equal key.theme_generation (Render_job.Runtime.theme_generation runtime))
  then invalid_arg "render_detached: theme generation mismatch";
  if
    not (Int.equal key.grammar_generation (Render_job.Runtime.grammar_generation runtime))
  then invalid_arg "render_detached: grammar generation mismatch";
  Option.iter job.semantic_seed ~f:(fun seed ->
    Option.iter (Render_job.Runtime.prepared_cache runtime) ~f:(fun cache ->
      Prepared_cache.set
        cache
        ~row_id:key.row_id
        ~row_revision:key.row_revision
        ~role:key.role
        ~text:key.text
        ~tool_output:key.tool_output
        seed.prepared);
    Option.iter (Render_job.Runtime.highlight_cache runtime) ~f:(fun cache ->
      List.iter seed.highlights ~f:(Highlight_cache.install cache)));
  let ctx =
    Render_context.make
      ~width:key.width
      ~role:key.role
      ~tool_output:key.tool_output
      ~runtime
      ~grammar_generation:key.grammar_generation
      ~tool_call_outcome:key.tool_call_outcome
  in
  let prepared, lines, highlights =
    let prepared =
      match Render_job.Runtime.prepared_cache runtime with
      | None -> Message.prepare ~role:key.role ~text:key.text ~tool_output:key.tool_output
      | Some cache ->
        (match
           Prepared_cache.find
             cache
             ~row_id:key.row_id
             ~row_revision:key.row_revision
             ~role:key.role
             ~text:key.text
             ~tool_output:key.tool_output
         with
         | Some prepared -> prepared
         | None ->
           let prepared =
             Message.prepare ~role:key.role ~text:key.text ~tool_output:key.tool_output
           in
           Prepared_cache.set
             cache
             ~row_id:key.row_id
             ~row_revision:key.row_revision
             ~role:key.role
             ~text:key.text
             ~tool_output:key.tool_output
             prepared;
           prepared)
    in
    let lines, highlights =
      match Render_job.Runtime.highlight_cache runtime with
      | None -> Message.render_prepared ctx prepared, []
      | Some cache ->
        Highlight_cache.capture cache (fun () -> Message.render_prepared ctx prepared)
    in
    prepared, lines, highlights
  in
  let layout = Render_job.Layout.{ width = key.width; lines } in
  let image = image_of_lines ~width:key.width lines in
  Render_job.result job ~prepared ~layout ~image ~highlights ~layout_plan:ctx.layout_plan
;;

let render_synchronously ~hi_engine (job : Render_job.t) =
  let runtime =
    Render_job.Runtime.create
      ~hi_engine
      ~theme_generation:job.Render_job.key.theme_generation
      ~grammar_generation:job.key.grammar_generation
      ~code_cache:shared_code_cache
      ~highlight_cache:shared_highlight_cache
      ~prepared_cache:shared_prepared_cache
      ~wrapped_cache:shared_wrapped_cache
      ()
  in
  render_detached ~runtime job
;;

let install_highlights highlights =
  List.iter highlights ~f:(Highlight_cache.install shared_highlight_cache)
;;

let install_prepared ~row_id ~row_revision ~role ~text ~tool_output prepared =
  Prepared_cache.set
    shared_prepared_cache
    ~row_id
    ~row_revision
    ~role
    ~text
    ~tool_output
    prepared
;;

let render_message
      ~width
      ~selected
      ~tool_output
      ~role
      ~text
      ~hi_engine
      ?search_query
      ?tool_call_outcome
      ()
  =
  let row_id =
    Projected_message.Id.local ~namespace:"standalone-render" ~local_id:"message"
    |> Result.ok_or_failwith
  in
  let job =
    Render_job.create
      ~transcript_generation:0
      ~row_id
      ~row_revision:0
      ~message_index:(-1)
      ~message_revision:0
      ~width
      ~role
      ~text
      ~tool_output
      ~tool_call_outcome
      ~theme_generation:0
      ~grammar_generation:0
      ~geometry_generation:0
      ~request_generation:0
      ~render_generation:0
      ~submission_generation:0
      ~semantic_seed:None
      ~priority:Visible
  in
  let result = render_synchronously ~hi_engine job in
  apply_overlay ~selected ~search_query:(Option.join search_query) result.layout
;;

let render_header_line ~width ~selected ~role ~hi_engine ?search_query () =
  let runtime =
    Render_job.Runtime.create ~hi_engine ~theme_generation:0 ~grammar_generation:0 ()
  in
  let search_query = Option.join search_query in
  let ctx =
    Render_context.make
      ~width
      ~role
      ~tool_output:None
      ~runtime
      ~grammar_generation:0
      ~tool_call_outcome:None
  in
  let layout = Render_job.Layout.{ width; lines = [ Message.render_header_line ctx ] } in
  apply_overlay ~selected ~search_query layout
;;
