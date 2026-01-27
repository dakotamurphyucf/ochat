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
  | exception _e ->
    (* Printf.eprintf "Error rendering line: %s" (Exn.to_string e); *)
    I.string attr "[error: invalid input]"
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

  let wrap_runs ~limit (runs : run list) : line list =
    if limit <= 0
    then [ runs ]
    else (
      let push_run acc a s =
        match acc with
        | (a', s') :: tl when phys_equal a a' -> (a, s' ^ s) :: tl
        | _ -> (a, s) :: acc
      in
      let flush acc cur = List.rev cur :: acc in
      let rec add_text acc cur cur_w a s pos =
        if pos >= String.length s
        then acc, cur, cur_w
        else (
          let code = Char.to_int (String.unsafe_get s pos) in
          let clen =
            if code land 0x80 = 0
            then 1
            else if code land 0xE0 = 0xC0
            then 2
            else if code land 0xF0 = 0xE0
            then 3
            else if code land 0xF8 = 0xF0
            then 4
            else 1
          in
          let clen = if clen <= 0 then 1 else clen in
          let piece =
            if pos + clen <= String.length s
            then String.sub s ~pos ~len:clen
            else String.sub s ~pos ~len:(String.length s - pos)
          in
          let ch_w = width_of_text piece in
          if cur_w + ch_w > limit && not (List.is_empty cur)
          then (
            let acc = flush acc cur in
            add_text acc [] 0 a s pos)
          else (
            let cur = push_run cur a piece in
            add_text acc cur (cur_w + ch_w) a s (pos + clen)))
      in
      let rec loop acc cur cur_w = function
        | [] -> List.rev (if List.is_empty cur then acc else flush acc cur)
        | (a, s) :: rest ->
          let acc, cur, cur_w = add_text acc cur cur_w a s 0 in
          loop acc cur cur_w rest
      in
      loop [] [] 0 runs)
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

module Paint = struct
  open Render_context

  let render_paragraph (ctx : t) ~(is_first : bool) ~(para : string) : I.t list =
    let first_pref = prefix_first ctx in
    let indent = prefix_cont ctx in
    let sel = if ctx.selected then Theme.selection_attr else Fn.id in
    let limit = Int.max 1 (ctx.width - String.length first_pref) in
    let render_runs (runs : Spans.run list) : I.t list =
      let wrapped = Wrap.wrap_runs ~limit runs in
      let render_line ~pref (line_runs : Spans.run list) =
        let content_img =
          List.map line_runs ~f:(fun (a, s) -> safe_string (sel a) s) |> I.hcat
        in
        Notty.Infix.(safe_string A.empty pref <|> content_img)
        |> I.hsnap ~align:`Left ctx.width
      in
      match wrapped with
      | [] -> []
      | l0 :: rest ->
        let first_pref = if is_first then first_pref else indent in
        let row0 = render_line ~pref:first_pref l0 in
        let rest_rows = List.map rest ~f:(render_line ~pref:indent) in
        row0 :: rest_rows
    in
    let is_read_directory =
      match ctx.tool_output with
      | Some (Read_directory _) -> true
      | _ -> false
    in
    let render_markdown () : I.t list =
      if String.is_empty para
      then [ I.hsnap ~align:`Left ctx.width (I.string A.empty "") ]
      else (
        let lines, info =
          Highlight_tm_engine.highlight_text_with_info
            ctx.hi_engine
            ~lang:(Some "markdown")
            ~text:para
        in
        let spans =
          match info.Highlight_tm_engine.fallback with
          | None ->
            (match lines with
             | [ xs ] -> xs
             | xs -> List.concat xs)
          | Some _ ->
            let len = String.length para in
            if
              len >= 4
              && String.is_prefix para ~prefix:"**"
              && String.is_suffix para ~suffix:"**"
            then (
              let inner = String.sub para ~pos:2 ~len:(len - 4) in
              let bold_attr =
                Highlight_theme.attr_of_scopes
                  Highlight_theme.github_dark
                  ~scopes:[ "markup.bold" ]
              in
              [ bold_attr, inner ])
            else if
              len >= 4
              && String.is_prefix para ~prefix:"__"
              && String.is_suffix para ~suffix:"__"
            then (
              let inner = String.sub para ~pos:2 ~len:(len - 4) in
              let bold_attr =
                Highlight_theme.attr_of_scopes
                  Highlight_theme.github_dark
                  ~scopes:[ "markup.bold" ]
              in
              [ bold_attr, inner ])
            else [ A.empty, para ]
        in
        let spans =
          if is_read_directory
          then (
            let dir_attr = Styles.fg_gray 13 in
            List.map spans ~f:(fun (_a, s) -> dir_attr, s))
          else spans
        in
        let runs = List.map spans ~f:(fun (a, s) -> a, s) in
        render_runs runs)
    in
    let is_tool_call = String.equal ctx.role "tool" && Option.is_none ctx.tool_output in
    if (not is_tool_call) || String.is_empty para
    then render_markdown ()
    else (
      let tool_spans : (A.t * string) list option =
        match String.lfindi para ~f:(fun _ c -> Char.( = ) c '(') with
        | None -> None
        | Some open_idx ->
          let prefix = String.sub para ~pos:0 ~len:open_idx in
          let total_len = String.length para in
          if open_idx + 1 > total_len
          then None
          else (
            let after_open_len = total_len - open_idx - 1 in
            let after_open = String.sub para ~pos:(open_idx + 1) ~len:after_open_len in
            let prefix_trimmed = String.rstrip prefix in
            if String.is_empty prefix_trimmed
            then None
            else (
              let name = prefix_trimmed in
              let ws_len = String.length prefix - String.length prefix_trimmed in
              let ws_after_name =
                if ws_len > 0
                then String.sub prefix ~pos:(String.length prefix_trimmed) ~len:ws_len
                else ""
              in
              let closing, args =
                let len_after = String.length after_open in
                if len_after > 0 && Char.(String.get after_open (len_after - 1) = ')')
                then ")", String.sub after_open ~pos:0 ~len:(len_after - 1)
                else "", after_open
              in
              let base_attr = Theme.attr_of_role ctx.role in
              let tool_name_attr = Styles.(base_attr ++ bold ++ fg_hex "#FFCC66") in
              let name_spans =
                if String.is_empty name then [] else [ tool_name_attr, name ]
              in
              let ws_spans =
                if String.is_empty ws_after_name then [] else [ base_attr, ws_after_name ]
              in
              let open_paren_spans = [ base_attr, "(" ] in
              let args_spans =
                if String.is_empty args
                then []
                else (
                  let json_lines =
                    Highlight_tm_engine.highlight_text
                      ctx.hi_engine
                      ~lang:(Some "json")
                      ~text:args
                  in
                  List.concat json_lines)
              in
              let closing_spans =
                if String.is_empty closing then [] else [ base_attr, closing ]
              in
              Some
                (List.concat
                   [ name_spans; ws_spans; open_paren_spans; args_spans; closing_spans ])))
      in
      match tool_spans with
      | None -> render_markdown ()
      | Some spans ->
        let runs = List.map spans ~f:(fun (a, s) -> a, s) in
        render_runs runs)
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
    let pref_len = String.length first_pref in
    let content_w_first = Int.max 0 (ctx.width - pref_len) in
    let make_content ~w ~selected =
      let spans = Highlight_tm_engine.highlight_text ctx.hi_engine ~lang ~text:code in
      let rows =
        List.map spans ~f:(fun line_spans ->
          List.map line_spans ~f:(fun (a, s) ->
            safe_string (if selected then A.(a ++ st reverse) else a) s)
          |> I.hcat
          |> I.hsnap ~align:`Left w)
      in
      I.vcat rows
    in
    if content_w_first <= 0
    then (
      let spans = Highlight_tm_engine.highlight_text ctx.hi_engine ~lang ~text:code in
      let rows =
        List.mapi spans ~f:(fun i line_spans ->
          let pref = if Int.equal i 0 then first_pref else indent in
          let content_img =
            List.map line_spans ~f:(fun (a, s) ->
              safe_string (if ctx.selected then A.(a ++ st reverse) else a) s)
            |> I.hcat
          in
          Notty.Infix.(
            safe_string
              (Theme.attr_of_role ctx.role
               |> fun a -> if ctx.selected then Theme.selection_attr a else a)
              pref
            <|> content_img)
          |> I.hsnap ~align:`Left ctx.width)
      in
      rows)
    else (
      let bucket = Code_cache2.bucket_for_width content_w_first in
      let digest =
        Md5.(to_hex (digest_string (Option.value lang ~default:"-" ^ "\x00" ^ code)))
      in
      let content_img =
        if ctx.selected
        then make_content ~w:content_w_first ~selected:true
        else (
          match Code_cache2.get ~klass ~lang ~digest ~wb:bucket with
          | Some img -> img
          | None ->
            let img = make_content ~w:bucket ~selected:false in
            Code_cache2.set ~klass ~lang ~digest ~wb:bucket img;
            img)
      in
      let h = I.height content_img in
      let base_attr = Theme.attr_of_role ctx.role in
      let base_attr =
        if ctx.selected then Theme.selection_attr base_attr else base_attr
      in
      let prefix_img =
        let row0 = safe_string base_attr first_pref in
        let rowi = safe_string base_attr indent in
        let rows = if h <= 0 then [] else row0 :: List.init (h - 1) ~f:(fun _ -> rowi) in
        I.vcat rows
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

  let render_header_line (ctx : Render_context.t) : I.t =
    let base_attr = Theme.attr_of_role ctx.role in
    let attr = if ctx.selected then Theme.selection_attr base_attr else base_attr in
    let icon =
      match ctx.role with
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
    in
    let label = Roles.label_of_role ctx.role in
    let label =
      if String.is_empty label
      then label
      else String.mapi label ~f:(fun i c -> if Int.equal i 0 then Char.uppercase c else c)
    in
    let text_img = safe_string attr (icon ^ label) in
    I.hsnap ~align:`Left ctx.width text_img
  ;;

  let render_body_default (ctx : Render_context.t) ~(role : string) ~(text : string)
    : I.t list
    =
    let blocks = Blocks.of_message_text text in
    let first_row = ref true in
    List.concat_map blocks ~f:(fun block ->
      match block with
      | Blocks.Text s | Blocks.Code { lang = Some "html"; code = s } ->
        String.split_lines s
        |> List.concat_map ~f:(fun para ->
          let rs = Paint.render_paragraph ctx ~is_first:!first_row ~para in
          if not (List.is_empty rs) then first_row := false;
          rs)
      | Blocks.Code { lang; code } ->
        let klass =
          if Roles.is_toollike role then Code_cache2.Toollike else Code_cache2.Userlike
        in
        let rs = Paint.render_code_block ctx ~is_first:!first_row ~lang ~code ~klass in
        if (not (Roles.is_toollike role)) && not (List.is_empty rs)
        then first_row := false;
        rs)
  ;;

  let render_body_apply_patch (ctx : Render_context.t) ~(role : string) ~(text : string)
    : I.t list
    =
    let lines = String.split_lines text in
    let rec split_status acc = function
      | [] -> List.rev acc, []
      | ("" as l) :: rest -> List.rev (l :: acc), rest
      | l :: rest -> split_status (l :: acc) rest
    in
    let status_lines, patch_lines = split_status [] lines in
    let first_row = ref true in
    let status_rows =
      List.concat_map status_lines ~f:(fun para ->
        let rs = Paint.render_paragraph ctx ~is_first:!first_row ~para in
        if not (List.is_empty rs) then first_row := false;
        rs)
    in
    let patch_rows =
      match patch_lines with
      | [] -> []
      | _ ->
        let code = String.concat ~sep:"\n" patch_lines in
        let lang = Some "ochat-apply-patch" in
        let klass =
          if Roles.is_toollike role then Code_cache2.Toollike else Code_cache2.Userlike
        in
        let rs = Paint.render_code_block ctx ~is_first:!first_row ~lang ~code ~klass in
        if not (List.is_empty rs) then first_row := false;
        rs
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

  let render (ctx : Render_context.t) ((role, text) : message) : I.t =
    let text = Util.sanitize ~strip:false text |> sanitize_developer role in
    let trimmed = String.strip text in
    if String.is_empty trimmed
    then I.empty
    else (
      let blank_before_header = I.hsnap ~align:`Left ctx.width (I.string A.empty "") in
      let header = render_header_line ctx in
      let blank_after_header = I.hsnap ~align:`Left ctx.width (I.string A.empty "") in
      let body_rows =
        match ctx.tool_output with
        | Some Apply_patch -> render_body_apply_patch ctx ~role ~text
        | Some (Read_file { path }) -> render_body_read_file ctx ~role ~text ~path
        | _ -> render_body_default ctx ~role ~text
      in
      let gap = I.hsnap ~align:`Left ctx.width (I.string A.empty " ") in
      I.vcat ((blank_before_header :: header :: blank_after_header :: body_rows) @ [ gap ]))
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
