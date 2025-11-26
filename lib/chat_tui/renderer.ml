(** Implementation of {!Chat_tui.Renderer}.

    The public API and user-facing invariants are documented in
    [renderer.mli]; this module contains the concrete Notty-based
    rendering pipeline and internal helpers. *)

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
  | exception e ->
    Printf.eprintf "Error rendering line: %s" (Exn.to_string e);
    I.string attr "[error: invalid input]"
;;

let create_hi_engine () =
  let e = Highlight_tm_engine.create ~theme:Highlight_theme.github_dark in
  let reg = Highlight_registry.get () in
  Highlight_tm_engine.with_registry e ~registry:reg
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
    ; hi_engine : Highlight_tm_engine.t
    }

  let make ~width ~selected ~role ~hi_engine = { width; selected; role; hi_engine }

  (*
     Message content lines are rendered without an inline role label so that
    copying code or text from the terminal does not include
    "assistant: "/"user: " prefixes.  The role is rendered separately as a
    header line above the message body.
  *)
  let prefix_first _t = ""
  let prefix_cont _t = ""
end

module Paint = struct
  open Render_context

  let render_paragraph (ctx : t) ~(is_first : bool) ~(para : string) : I.t list =
    let first_pref = prefix_first ctx in
    let indent = prefix_cont ctx in
    let sel = if ctx.selected then Theme.selection_attr else Fn.id in
    let limit = Int.max 1 (ctx.width - String.length first_pref) in
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
          print_endline "uouououo";
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
      let runs = List.map spans ~f:(fun (a, s) -> a, s) in
      let wrapped = Wrap.wrap_runs ~limit runs in
      let render_line ~pref line_runs =
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
        row0 :: rest_rows)
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

  let render (ctx : Render_context.t) ((role, text) : message) : I.t =
    let text = Util.sanitize ~strip:false text |> sanitize_developer role in
    let trimmed = String.strip text in
    if String.is_empty trimmed
    then I.empty
    else (
      let blank_before_header = I.hsnap ~align:`Left ctx.width (I.string A.empty "") in
      let header = render_header_line ctx in
      let blank_after_header = I.hsnap ~align:`Left ctx.width (I.string A.empty "") in
      let blocks = Blocks.of_message_text text in
      let first_row = ref true in
      let body_rows =
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
              if Roles.is_toollike role
              then Code_cache2.Toollike
              else Code_cache2.Userlike
            in
            let rs =
              Paint.render_code_block ctx ~is_first:!first_row ~lang ~code ~klass
            in
            if (not (Roles.is_toollike role)) && not (List.is_empty rs)
            then first_row := false;
            rs)
      in
      let gap = I.hsnap ~align:`Left ctx.width (I.string A.empty " ") in
      I.vcat ((blank_before_header :: header :: blank_after_header :: body_rows) @ [ gap ]))
  ;;
end

module Viewport = struct
  let render
        ~(model : Model.t)
        ~(width : int)
        ~(height : int)
        ~(messages : message list)
        ~(selected_idx : int option)
        ~(render_message : selected:bool -> message -> I.t)
    : I.t
    =
    let len = List.length messages in
    let heights = Model.msg_heights model in
    let prefix = Model.height_prefix model in
    let get_height idx msg =
      match Model.find_img_cache model ~idx with
      | Some entry when entry.width = width && String.equal entry.text (snd msg) ->
        entry.height_unselected
      | _ ->
        let img_unselected = render_message ~selected:false msg in
        let h = I.height img_unselected in
        let entry =
          { Model.width
          ; text = snd msg
          ; img_unselected
          ; height_unselected = h
          ; img_selected = None
          ; height_selected = None
          }
        in
        Model.set_img_cache model ~idx entry;
        h
    in
    let ensure_arrays () =
      if Array.length heights <> len || Array.length prefix <> len + 1
      then (
        let heights' = Array.create ~len 0 in
        let prefix' = Array.create ~len:(len + 1) 0 in
        let rec fill i msgs =
          match msgs with
          | [] -> ()
          | msg :: rest ->
            let h = get_height i msg in
            heights'.(i) <- h;
            prefix'.(i + 1) <- prefix'.(i) + h;
            fill (i + 1) rest
        in
        fill 0 messages;
        Model.set_msg_heights model heights';
        Model.set_height_prefix model prefix')
      else (
        match Model.take_and_clear_dirty_height_indices model with
        | [] -> ()
        | dirty ->
          let dirty = List.dedup_and_sort ~compare:Int.compare dirty in
          List.iter dirty ~f:(fun idx ->
            if idx >= 0 && idx < len
            then (
              let msg = List.nth_exn messages idx in
              let old_h = heights.(idx) in
              let new_h = get_height idx msg in
              let delta = new_h - old_h in
              if not (Int.equal delta 0)
              then (
                heights.(idx) <- new_h;
                let n = Array.length prefix - 1 in
                for j = idx + 1 to n do
                  prefix.(j) <- prefix.(j) + delta
                done))))
    in
    ensure_arrays ();
    let prefix = Model.height_prefix model in
    let total_height = prefix.(len) in
    let max_scroll = Int.max 0 (total_height - height) in
    let scroll =
      if Model.auto_follow model
      then max_scroll
      else (
        let s = Notty_scroll_box.scroll (Model.scroll_box model) in
        Int.max 0 (Int.min s max_scroll))
    in
    let bsearch_first_gt arr ~len ~target =
      let lo = ref 0 in
      let hi = ref len in
      while !lo < !hi do
        let mid = (!lo + !hi) lsr 1 in
        if arr.(mid) <= target then lo := mid + 1 else hi := mid
      done;
      !lo
    in
    let bsearch_first_ge arr ~len ~target =
      let lo = ref 0 in
      let hi = ref len in
      while !lo < !hi do
        let mid = (!lo + !hi) lsr 1 in
        if arr.(mid) < target then lo := mid + 1 else hi := mid
      done;
      !lo
    in
    let start_k = bsearch_first_gt prefix ~len:(len + 1) ~target:scroll in
    let start_idx = Int.max 0 (start_k - 1) in
    let end_pos = Int.min total_height (scroll + height) in
    let end_k = bsearch_first_ge prefix ~len:(len + 1) ~target:end_pos in
    let last_idx = Int.max 0 (Int.min (len - 1) (end_k - 1)) in
    let top_blank = if len = 0 then 0 else prefix.(start_idx) in
    let bottom_blank = if len = 0 then 0 else total_height - prefix.(last_idx + 1) in
    let get_img idx msg ~selected =
      match Model.find_img_cache model ~idx with
      | Some entry when entry.width = width && String.equal entry.text (snd msg) ->
        (match selected, entry.img_selected with
         | true, Some img -> img
         | true, None ->
           let img = render_message ~selected:true msg in
           let entry' =
             { entry with img_selected = Some img; height_selected = Some (I.height img) }
           in
           Model.set_img_cache model ~idx entry';
           img
         | false, _ -> entry.img_unselected)
      | _ ->
        let img_unselected = render_message ~selected:false msg in
        let h = I.height img_unselected in
        let entry =
          { Model.width
          ; text = snd msg
          ; img_unselected
          ; height_unselected = h
          ; img_selected = None
          ; height_selected = None
          }
        in
        Model.set_img_cache model ~idx entry;
        if Option.value_map selected_idx ~default:false ~f:(Int.equal idx)
        then render_message ~selected:true msg
        else img_unselected
    in
    let body =
      if len = 0 || last_idx < start_idx
      then I.empty
      else (
        let imgs =
          List.init
            (last_idx - start_idx + 1)
            ~f:(fun off ->
              let idx = start_idx + off in
              let msg = List.nth_exn messages idx in
              let sel = Option.value_map selected_idx ~default:false ~f:(Int.equal idx) in
              get_img idx msg ~selected:sel)
        in
        I.vcat imgs)
    in
    let top_pad = I.void width top_blank in
    let bot_pad = I.void width bottom_blank in
    I.vcat [ top_pad; body; bot_pad ]
  ;;
end

module Status_bar = struct
  let render ~width ~(model : Model.t) =
    let bar_attr = A.(bg (gray 2) ++ fg (gray 15)) in
    let mode_txt =
      match Model.mode model with
      | Insert -> "-- INSERT --"
      | Normal -> "-- NORMAL --"
      | Cmdline -> "-- CMD --"
    in
    let raw_txt =
      match Model.draft_mode model with
      | Model.Raw_xml -> " -- RAW --"
      | Model.Plain -> ""
    in
    let text = mode_txt ^ raw_txt in
    let text_img = I.string bar_attr text in
    let pad_w = Int.max 0 (width - I.width text_img) in
    let pad = I.string bar_attr (String.make pad_w ' ') in
    Notty.Infix.(text_img <|> pad)
  ;;
end

module Input_box = struct
  let render ~width ~(model : Model.t) : I.t * (int * int) =
    let w = width in
    let input_lines =
      match Model.mode model with
      | Cmdline -> [ Model.cmdline model ]
      | _ ->
        (match String.split ~on:'\n' (Model.input_line model) with
         | [] -> [ "" ]
         | ls -> ls)
    in
    let border_attr = A.(fg (rgb ~r:1 ~g:4 ~b:5)) in
    let bg_attr = A.empty in
    let selection_attr base = A.(base ++ st reverse) in
    let prefix, indent =
      match Model.mode model with
      | Cmdline -> ":", ""
      | _ ->
        let p = "> " in
        p, String.make (String.length p) ' '
    in
    let sel_active = Model.selection_active model in
    let rows =
      let text_attr = bg_attr in
      let sel_attr = selection_attr text_attr in
      let rec build lines idx abs_off acc =
        match lines with
        | [] -> List.rev acc
        | line :: rest ->
          let line_prefix = if idx = 0 then prefix else indent in
          let line_len = String.length line in
          let line_start = abs_off in
          let line_end = abs_off + line_len in
          let overlap_start, overlap_end =
            if not sel_active
            then None, None
            else (
              match Model.selection_anchor model with
              | None -> None, None
              | Some anchor ->
                let cur = Model.cursor_pos model in
                let sel_start = Int.min anchor cur in
                let sel_end = Int.max anchor cur in
                let ov_start = Int.max sel_start line_start in
                let ov_end = Int.min sel_end line_end in
                if ov_start < ov_end then Some ov_start, Some ov_end else None, None)
          in
          let content_img =
            match overlap_start, overlap_end with
            | None, _ | _, None -> I.string text_attr (line_prefix ^ line)
            | Some ov_s, Some ov_e ->
              let local_start = ov_s - line_start in
              let local_end = ov_e - line_start in
              let before = String.sub line ~pos:0 ~len:local_start in
              let selected =
                String.sub line ~pos:local_start ~len:(local_end - local_start)
              in
              let after = String.sub line ~pos:local_end ~len:(line_len - local_end) in
              I.hcat
                [ I.string text_attr line_prefix
                ; I.string text_attr before
                ; I.string sel_attr selected
                ; I.string text_attr after
                ]
          in
          let inside = content_img |> I.hsnap ~align:`Left (w - 2) in
          let row_img =
            Notty.Infix.(I.string border_attr "│" <|> inside <|> I.string border_attr "│")
          in
          let next_abs = line_end + 1 in
          build rest (idx + 1) next_abs (row_img :: acc)
      in
      build input_lines 0 0 []
    in
    let hline len =
      let seg = "─" in
      String.concat ~sep:"" (List.init len ~f:(fun _ -> seg)) |> I.string border_attr
    in
    let top_border =
      Notty.Infix.(
        I.string border_attr "┌" <|> hline (w - 2) <|> I.string border_attr "┐")
    in
    let bottom_border =
      Notty.Infix.(
        I.string border_attr "└" <|> hline (w - 2) <|> I.string border_attr "┘")
    in
    let img = I.vcat ((top_border :: rows) @ [ bottom_border ]) in
    let total_index = Model.cursor_pos model in
    let rec row_col lines offset row =
      match lines with
      | [] -> row, 0
      | l :: ls ->
        let len = String.length l in
        if total_index <= offset + len
        then row, total_index - offset
        else row_col ls (offset + len + 1) (row + 1)
    in
    let row, col_in_line = row_col input_lines 0 0 in
    let cursor_x =
      (match Model.mode model with
       | Cmdline -> 2
       | _ -> 3)
      + col_in_line
    in
    img, (cursor_x, row)
  ;;
end

module Compose = struct
  let top_visible_index
        ~(model : Model.t)
        ~(scroll_height : int)
        ~(messages : message list)
    : int option
    =
    let len = List.length messages in
    if Int.equal len 0
    then None
    else (
      let prefix = Model.height_prefix model in
      if Array.length prefix < len + 1
      then None
      else (
        let total_height = prefix.(len) in
        let max_scroll = Int.max 0 (total_height - scroll_height) in
        let scroll =
          if Model.auto_follow model
          then max_scroll
          else (
            let s = Notty_scroll_box.scroll (Model.scroll_box model) in
            Int.max 0 (Int.min s max_scroll))
        in
        let bsearch_first_gt arr ~len ~target =
          let lo = ref 0 in
          let hi = ref len in
          while !lo < !hi do
            let mid = (!lo + !hi) lsr 1 in
            if arr.(mid) <= target then lo := mid + 1 else hi := mid
          done;
          !lo
        in
        let k = bsearch_first_gt prefix ~len:(len + 1) ~target:scroll in
        let idx = Int.max 0 (k - 1) in
        if idx >= len
        then None
        else (
          let message_start_y = prefix.(idx) in
          let header_y = message_start_y + 1 in
          let header_vpos = header_y - scroll in
          let exclusion_band = 2 in
          if header_vpos >= 0 && header_vpos < exclusion_band then None else Some idx)))
  ;;

  let render_full ~(size : int * int) ~(model : Model.t) : I.t * (int * int) =
    let w, h = size in
    let input_lines =
      match Model.mode model with
      | Cmdline -> [ Model.cmdline model ]
      | _ ->
        (match String.split ~on:'\n' (Model.input_line model) with
         | [] -> [ "" ]
         | ls -> ls)
    in
    let input_content_height = List.length input_lines in
    let border_rows = 2 in
    let history_height =
      let base = Int.max 1 (h - input_content_height - border_rows) in
      max 1 (base - 1)
    in
    let sticky_height = if history_height > 1 then 1 else 0 in
    let scroll_height = history_height - sticky_height in
    let hi_engine = create_hi_engine () in
    (match Model.last_history_width model with
     | Some prev when Int.equal prev w -> ()
     | _ ->
       Model.clear_all_img_caches model;
       Model.set_last_history_width model (Some w));
    let render_message ~selected ((role, _) as msg) =
      let ctx = Render_context.make ~width:w ~selected ~role ~hi_engine in
      Message.render ctx msg
    in
    let messages = Model.messages model in
    let history_img =
      Viewport.render
        ~model
        ~width:w
        ~height:scroll_height
        ~messages
        ~selected_idx:(Model.selected_msg model)
        ~render_message
    in
    Notty_scroll_box.set_content (Model.scroll_box model) history_img;
    if Model.auto_follow model
    then Notty_scroll_box.scroll_to_bottom (Model.scroll_box model) ~height:scroll_height;
    let top_visible_idx = top_visible_index ~model ~scroll_height ~messages in
    let scroll_view =
      Notty_scroll_box.render (Model.scroll_box model) ~width:w ~height:scroll_height
    in
    let sticky_header =
      if sticky_height <= 0
      then I.empty
      else (
        match top_visible_idx with
        | None -> I.hsnap ~align:`Left w (I.string A.empty "")
        | Some idx ->
          let role, _ = List.nth_exn messages idx in
          let selected =
            Option.value_map (Model.selected_msg model) ~default:false ~f:(Int.equal idx)
          in
          let ctx = Render_context.make ~width:w ~selected ~role ~hi_engine in
          Message.render_header_line ctx)
    in
    let history_view =
      if sticky_height <= 0 then scroll_view else I.vcat [ sticky_header; scroll_view ]
    in
    let status = Status_bar.render ~width:w ~model in
    let input_img, _ = Input_box.render ~width:w ~model in
    let full_img = Notty.Infix.(history_view <-> status <-> input_img) in
    let total_index = Model.cursor_pos model in
    let rec row_col lines offset row =
      match lines with
      | [] -> row, 0
      | l :: ls ->
        let len = String.length l in
        if total_index <= offset + len
        then row, total_index - offset
        else row_col ls (offset + len + 1) (row + 1)
    in
    let row_input, col_in_line = row_col input_lines 0 0 in
    let cursor_x =
      (match Model.mode model with
       | Cmdline -> 2
       | _ -> 3)
      + col_in_line
    in
    let cursor_y = history_height + 1 + 1 + row_input in
    full_img, (cursor_x, cursor_y)
  ;;
end

let render_full ~size ~model = Compose.render_full ~size ~model
