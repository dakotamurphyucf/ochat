open Core
open Types

type t =
  { mutable history_items : Openai.Responses.Item.t list
  ; mutable messages : message list
  ; mutable input_line : string
  ; mutable auto_follow : bool
  ; msg_buffers : (string, Types.msg_buffer) Base.Hashtbl.t
  ; function_name_by_id : (string, string) Base.Hashtbl.t
  ; reasoning_idx_by_id : (string, int ref) Base.Hashtbl.t
  ; mutable fetch_sw : Eio.Switch.t option
  ; scroll_box : Notty_scroll_box.t
  ; mutable cursor_pos : int
  ; mutable selection_anchor : int option (* Command-mode scaffolding *)
  ; mutable mode : editor_mode
  ; mutable draft_mode : draft_mode
  ; mutable selected_msg : int option
  ; mutable undo_stack : (string * int) list
  ; mutable redo_stack : (string * int) list
  ; mutable cmdline : string
  ; mutable cmdline_cursor : int
  ; mutable active_fork : string option
  ; mutable fork_start_index : int option
  }
[@@deriving fields ~getters ~setters]

and editor_mode =
  | Insert
  | Normal
  | Cmdline

and draft_mode =
  | Plain
  | Raw_xml

let create
      ~history_items
      ~messages
      ~input_line
      ~auto_follow
      ~msg_buffers
      ~function_name_by_id
      ~reasoning_idx_by_id
      ~fetch_sw
      ~scroll_box
      ~cursor_pos
      ~selection_anchor
      ~mode
      ~draft_mode
      ~selected_msg
      ~undo_stack
      ~redo_stack
      ~cmdline
      ~cmdline_cursor
  =
  { history_items
  ; messages
  ; input_line
  ; auto_follow
  ; msg_buffers
  ; function_name_by_id
  ; reasoning_idx_by_id
  ; fetch_sw
  ; scroll_box
  ; cursor_pos
  ; selection_anchor
  ; mode
  ; draft_mode
  ; selected_msg
  ; undo_stack
  ; redo_stack
  ; cmdline
  ; cmdline_cursor
  ; active_fork = None
  ; fork_start_index = None
  }
;;

let input_line t = t.input_line
let cursor_pos t = t.cursor_pos
let selection_anchor t = t.selection_anchor
let clear_selection t = t.selection_anchor <- None
let set_selection_anchor t idx = t.selection_anchor <- Some idx
let selection_active t = Option.is_some t.selection_anchor
let messages t = t.messages
let auto_follow t = t.auto_follow
let cmdline t = t.cmdline
let cmdline_cursor t = t.cmdline_cursor
let set_cmdline t s = t.cmdline <- s
let set_cmdline_cursor t n = t.cmdline_cursor <- n

(* ------------------------------------------------------------------------- *)
(*  Fork helpers                                                              *)
(* ------------------------------------------------------------------------- *)

let active_fork t = t.active_fork
let set_active_fork t v = t.active_fork <- v

let fork_start_index t = t.fork_start_index
let set_fork_start_index t v = t.fork_start_index <- v

(* ------------------------------------------------------------------------- *)
(*  Command-mode helpers                                                     *)
(* ------------------------------------------------------------------------- *)

let toggle_mode (t : t) : unit =
  t.mode
  <- (match t.mode with
      | Insert -> Normal
      | Normal -> Insert
      | Cmdline -> Insert)
;;

let set_draft_mode (t : t) (m : draft_mode) = t.draft_mode <- m
let select_message (t : t) (idx : int option) = t.selected_msg <- idx

(* ------------------------------------------------------------------------- *)
(*  Undo / Redo helpers                                                       *)
(* ------------------------------------------------------------------------- *)

let push_undo (t : t) : unit =
  t.undo_stack <- (t.input_line, t.cursor_pos) :: t.undo_stack;
  (* any new edit invalidates the redo ring *)
  t.redo_stack <- []
;;

let undo (t : t) : bool =
  match t.undo_stack with
  | (txt, pos) :: rest ->
    t.redo_stack <- (t.input_line, t.cursor_pos) :: t.redo_stack;
    t.input_line <- txt;
    t.cursor_pos <- Int.min (String.length txt) pos;
    t.undo_stack <- rest;
    true
  | [] -> false
;;

let redo (t : t) : bool =
  match t.redo_stack with
  | (txt, pos) :: rest ->
    t.undo_stack <- (t.input_line, t.cursor_pos) :: t.undo_stack;
    t.input_line <- txt;
    t.cursor_pos <- Int.min (String.length txt) pos;
    t.redo_stack <- rest;
    true
  | [] -> false
;;

(* ------------------------------------------------------------------------- *)
(*  Internal helpers – these are largely a direct carry-over from the mutable
    implementation present before refactoring step 6.                       *)
(* ------------------------------------------------------------------------- *)

let update_message_text (model : t) index new_txt =
  model.messages
  <- List.mapi model.messages ~f:(fun idx (role, txt) ->
       if idx = index then role, new_txt else role, txt)
;;

let ensure_buffer (model : t) ~(id : string) ~(role : string) : Types.msg_buffer =
  match Hashtbl.find model.msg_buffers id with
  | Some b -> b
  | None ->
    let index = List.length model.messages in
    let b = { Types.text = ref ""; index } in
    Hashtbl.set model.msg_buffers ~key:id ~data:b;
    (* add empty placeholder so the UI can render incrementally *)
    model.messages <- model.messages @ [ role, "" ];
    b
;;

(* ------------------------------------------------------------------------- *)
(*  Patch application                                                        *)
(* ------------------------------------------------------------------------- *)

let apply_patch (model : t) (p : Types.patch) : t =
  match p with
  | Types.Ensure_buffer { id; role } ->
    ignore (ensure_buffer model ~id ~role);
    model
  | Types.Append_text { id; role; text } ->
    let buf = ensure_buffer model ~id ~role in
    (* Append new text and update the corresponding visible message. *)
    buf.text := !(buf.text) ^ text;
    update_message_text model buf.index !(buf.text);
    model
  | Types.Set_function_name { id; name } ->
    Hashtbl.set model.function_name_by_id ~key:id ~data:name;
    model
  | Types.Set_function_output { id; output } ->
    let role =
      if Option.is_some model.active_fork then "fork" else "tool_output"
    in
    let buf =
      match Hashtbl.find model.msg_buffers id with
      | Some b -> b
      | None -> ensure_buffer model ~id ~role
    in
    let max_len = 2_000 in
    let txt = Util.sanitize output in
    let txt =
      if String.length txt > max_len
      then String.sub txt ~pos:0 ~len:max_len ^ "\n…truncated…"
      else txt
    in
    (* Overwrite buffer text *)
    buf.text := txt;
    update_message_text model buf.index txt;
    model
  | Types.Update_reasoning_idx { id; idx } ->
    (match Hashtbl.find model.reasoning_idx_by_id id with
     | Some r -> r := idx
     | None -> Hashtbl.set model.reasoning_idx_by_id ~key:id ~data:(ref idx));
    model
  | Types.Add_user_message { text } ->
    (* Construct a new history item representing the user's input and append
       it to both the canonical history list and the list of renderable
       messages.  For now we keep the simple implementation that mirrors the
       previous imperative code.  A future refactor might introduce a helper
       that converts user text into a history item in a single place. *)
    let content_item =
      Openai.Responses.Input_message.Text { text; _type = "input_text" }
    in
    let user_item : Openai.Responses.Item.t =
      let open Openai.Responses in
      Item.Input_message
        { Input_message.role = Input_message.User
        ; content = [ content_item ]
        ; _type = "message"
        }
    in
    (* Update underlying refs *)
    model.history_items <- model.history_items @ [ user_item ];
    model.messages <- model.messages @ [ "user", text ];
    model
  | Types.Add_placeholder_message { role; text } ->
    (* Only modify the visible message list – placeholders should never end
       up in the canonical conversation history. *)
    model.messages <- model.messages @ [ role, text ];
    model
;;

let apply_patches model patches = List.fold patches ~init:model ~f:apply_patch

let add_history_item (model : t) (item : Openai.Responses.Item.t) =
  model.history_items <- model.history_items @ [ item ];
  model
;;
