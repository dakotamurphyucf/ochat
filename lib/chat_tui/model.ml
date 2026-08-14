open Core
open Types
module Res_item = Openai.Responses.Item

type msg_img_cache =
  { row_revision : int
  ; width : int
  ; role : string
  ; text : string
  ; image : Notty.I.t
  ; height : int
  ; layout : Chat_message_render_job.Layout.t
  ; layout_plan : Chat_message_render_job.Layout_plan.t
  }

type msg_semantic_cache =
  { row_revision : int
  ; role : string
  ; text : string
  ; tool_output : Types.tool_output_kind option
  ; prepared : Chat_message_render_job.Prepared_message.t
  ; highlights : Chat_message_render_job.Highlight_cache.binding list
  }

type msg_buffer =
  { buf : Buffer.t
  ; row_id : Projected_message.Id.t
  }

type scroll_direction =
  | Toward_older
  | Toward_newer

type prepared_boundary_distances =
  { older : int
  ; newer : int
  }

type chat_scroll_result =
  { changed : bool
  ; clamped : bool
  ; distances : prepared_boundary_distances option
  }

module Resize_anchor = struct
  type row_key =
    { id : Projected_message.Id.t
    ; revision : int
    }

  type neighbor =
    { key : row_key
    ; side : [ `Older | `Newer ]
    }

  type t =
    | Follow_bottom
    | Manual_empty
    | Preserve_row of
        { key : row_key
        ; anchor : Renderer_virtual_list.Anchor.t
        ; neighbors : neighbor list
        }

  type resolution =
    | Followed_bottom
    | Preserved
    | Repaired
    | Empty
  [@@deriving sexp_of]
end

module Page_id = struct
  type t =
    | Chat
    | Agent
    | Shell_security
end

module Chat_page_state = struct
  module Destination = struct
    type reason =
      | Earlier_conversation
      | Search_result
      | Latest_conversation

    type placement =
      | Top
      | Center
      | Bottom

    type t =
      { id : Projected_message.Id.t
      ; revision : int
      ; reason : reason
      ; placement : placement
      }
  end

  type history_chunk_row =
    { row_id : Projected_message.Id.t
    ; row_revision : int
    }
  [@@deriving equal]

  type history_chunk =
    { rows : history_chunk_row array
    ; image : Notty.I.t
    ; height : int
    }

  type materialization =
    | Loading
    | Resizing
    | Corridor
    | Warm

  type history_image_cache =
    { width : int
    ; transcript_generation : int
    ; render_generation : int
    ; chunks : history_chunk array
    ; chunk_tree : Notty.I.t array
    ; chunk_tree_base : int
    ; image : Notty.I.t
    }

  type corridor_history_cache =
    { width : int
    ; request_generation : int
    ; rows : History_chunk.Range.t
    ; mutable image : Notty.I.t
    }

  type width_snapshot =
    { width : int
    ; transcript_generation : int
    ; render_generation : int
    ; entries : (Projected_message.Id.t * msg_img_cache) list
    ; heights : int array
    ; prefix : int array
    ; history_image_cache : history_image_cache
    }

  type width_preparation_status =
    | Preparing
    | Complete
    | Cancelled

  type preparation_layout =
    { input_box_height : int
    ; history_height : int
    ; sticky_height : int
    ; scroll_height : int
    }

  type preparing_width =
    { request_generation : int
    ; terminal_size : int * int
    ; layout : preparation_layout
    ; transcript_generation : int
    ; render_generation : int
    ; theme_generation : int
    ; grammar_generation : int
    ; anchor : Resize_anchor.t
    ; active_geometry : Renderer_virtual_list.Geometry.Snapshot.t
    ; active_rows : (Projected_message.Id.t * int) array
    ; scroll_direction : scroll_direction
    ; target_rows : (Projected_message.Id.t, msg_img_cache) Hashtbl.t
    ; exact_rows : Projected_message.Id.t Hash_set.t
    ; prepared_batches : Int.Hash_set.t
    ; prepared_chunks : Int.Hash_set.t
    ; partial_chunks : (int, history_chunk) Hashtbl.t
    ; mutable desired_corridor : History_chunk.Range.t option
    ; mutable published_corridor : History_chunk.Range.t option
    ; mutable destination : Destination.t option
    ; mutable status : width_preparation_status
    }

  type t =
    { scroll_box : Notty_scroll_box.t
    ; mutable msg_img_cache : (Projected_message.Id.t, msg_img_cache) Hashtbl.t
    ; msg_semantic_cache : (Projected_message.Id.t, msg_semantic_cache) Hashtbl.t
    ; mutable active_history_width : int option
    ; mutable preparing_width : preparing_width option
    ; geometry : Renderer_virtual_list.Geometry.t
    ; mutable dirty_height_rows : (Projected_message.Id.t * int) list
    ; mutable scroll_direction : scroll_direction
    ; mutable materialization : materialization
    ; mutable history_image_cache : history_image_cache option
    ; mutable corridor_history_cache : corridor_history_cache option
    ; mutable width_snapshots : width_snapshot list
    ; row_chunk_by_id : (Projected_message.Id.t, int) Hashtbl.t
    ; dirty_history_chunks : Int.Hash_set.t
    }
end

let recent_width_snapshot_capacity = 3

module Projected_state = struct
  type t =
    { mutable rows : Projected_message.t array
    ; index_by_id : (Projected_message.Id.t, int) Hashtbl.t
    ; mutable selected_id : Projected_message.Id.t option
    ; mutable reveal_id : Projected_message.Id.t option
    ; measured_height_by_id : (Projected_message.Id.t, int) Hashtbl.t
    ; geometry : Renderer_virtual_list.Geometry.t
    }

  let empty () =
    { rows = [||]
    ; index_by_id = Hashtbl.create (module Projected_message.Id)
    ; selected_id = None
    ; reveal_id = None
    ; measured_height_by_id = Hashtbl.create (module Projected_message.Id)
    ; geometry = Renderer_virtual_list.Geometry.create ()
    }
  ;;
end

module Agent_page_state = struct
  type text_entry =
    { render_id : int
    ; revision : int
    ; channel : Ochat_function.Progress.channel
    ; text : string
    ; replaceable : bool
    }

  type nested_call =
    { render_id : int
    ; mutable revision : int
    ; call_id : string
    ; name : string
    ; kind : Ochat_function.Trace.tool_kind
    ; mutable payload : string
    ; mutable progress : text_entry list
    ; mutable outcome : Ochat_function.Trace.outcome option
    ; mutable output : Openai.Responses.Tool_output.Output.t option
    }

  type progress_entry =
    | Text of text_entry
    | Tool of nested_call

  type render_block_view =
    | Invocation of
        { name : string
        ; payload : string
        ; agent_page_kind : Chat_response.Tool_execution_event.agent_page_kind
        }
    | Truncation
    | Waiting
    | Progress of progress_entry
    | Status of Ochat_function.Trace.outcome

  type render_block =
    { id : int
    ; revision : int
    ; view : render_block_view
    }

  type render_cache =
    { width : int
    ; revision : int
    ; image : Notty.I.t
    ; height : int
    }

  type call =
    { call_id : string
    ; name : string
    ; kind : [ `Function | `Custom ]
    ; payload : string
    ; agent_page_kind : Chat_response.Tool_execution_event.agent_page_kind
    ; start_order : int
    ; mutable entries : progress_entry list
    ; mutable retained_bytes : int
    ; mutable is_truncated : bool
    ; mutable outcome : Ochat_function.Trace.outcome option
    ; mutable output : Openai.Responses.Tool_output.Output.t option
    ; mutable next_render_id : int
    ; render_cache : (int, render_cache) Hashtbl.t
    ; mutable render_width : int option
    ; mutable render_block_ids : int array
    ; mutable render_block_revisions : int array
    ; render_geometry : Renderer_virtual_list.Geometry.t
    }

  type t =
    { calls : call String.Table.t
    ; terminal_call_ids : String.Hash_set.t
    ; mutable call_order : string list
    ; mutable selected_call_id : string option
    ; scroll_box : Notty_scroll_box.t
    ; mutable auto_follow : bool
    ; mutable next_start_order : int
    }

  let empty () =
    { calls = String.Table.create ()
    ; terminal_call_ids = String.Hash_set.create ()
    ; call_order = []
    ; selected_call_id = None
    ; scroll_box = Notty_scroll_box.create Notty.I.empty
    ; auto_follow = true
    ; next_start_order = 0
    }
  ;;
end

module Shell_security_page_state = Shell_security_page_state

module Pages = struct
  type t =
    { chat : Chat_page_state.t
    ; agent : Agent_page_state.t
    ; shell_security : Shell_security_page_state.t
    }
end

type typeahead_completion =
  { text : string
  ; base_input : string
  ; base_cursor : int
  ; generation : int
  }

type assistant_activity =
  | Thinking
  | Writing
  | Working

type activity =
  | Assistant of assistant_activity
  | Compacting

type viewport_relation =
  | Above
  | Visible
  | Below
  | Unknown

type projection_damage =
  | No_damage
  | Below_viewport
  | Visible_damage
  | Above_viewport
  | Unknown_damage

type t =
  { mutable history_items : History_entry.t list
  ; mutable message_array : message array
  ; mutable render_row_ids : Projected_message.Id.t array
  ; mutable transcript_generation : int
  ; mutable render_generation : int
  ; mutable message_revisions : int array
  ; mutable input_line : string
  ; mutable auto_follow : bool
  ; msg_buffers : (string, msg_buffer) Base.Hashtbl.t
  ; function_name_by_id : (string, string) Base.Hashtbl.t
  ; reasoning_idx_by_id : (string, int ref) Base.Hashtbl.t
  ; tool_output_by_index : (int, Types.tool_output_kind) Base.Hashtbl.t
  ; tool_output_by_id : (Projected_message.Id.t, Types.tool_output_kind) Base.Hashtbl.t
  ; call_id_by_item_id : (string, string) Base.Hashtbl.t
  ; tool_call_id_by_id : (Projected_message.Id.t, string) Base.Hashtbl.t
  ; tool_call_outcome_by_call_id : (string, Ochat_function.Trace.outcome) Base.Hashtbl.t
  ; tool_path_by_call_id : (string, string option) Base.Hashtbl.t
  ; mutable active_page : Page_id.t
  ; pages : Pages.t
  ; mutable tasks : Session.Task.t list
  ; kv_store : (string, string) Base.Hashtbl.t
  ; mutable fetch_sw : Eio.Switch.t option
  ; mutable cursor_pos : int
  ; mutable selection_anchor : int option
  ; mutable mode : editor_mode
  ; mutable draft_mode : draft_mode
  ; mutable undo_stack : (string * int) list
  ; mutable redo_stack : (string * int) list
  ; mutable cmdline : string
  ; mutable cmdline_cursor : int
  ; mutable search_query : string
  ; mutable search_cursor : int
  ; mutable last_search_query : string option
  ; mutable last_search_dir : search_dir option
  ; mutable typeahead_completion : typeahead_completion option
  ; mutable typeahead_preview_open : bool
  ; mutable typeahead_preview_scroll : int
  ; mutable typeahead_generation : int
  ; mutable activity : activity option
  ; mutable animation_frame : int
  ; mutable normal_input_enabled : bool
  ; projected : Projected_state.t
  }
[@@deriving fields ~getters ~setters]

and search_dir =
  | Forward
  | Backward

and editor_mode =
  | Insert
  | Normal
  | Cmdline
  | Search of search_dir

and draft_mode =
  | Plain
  | Raw_xml

let classify_tool_output ~(name_opt : string option) ~(path : string option)
  : Types.tool_output_kind
  =
  match Option.map ~f:String.lowercase name_opt with
  | Some "apply_patch" -> Types.Apply_patch
  | Some "read_file" -> Types.Read_file { path }
  | Some "read_directory" -> Types.Read_directory { path }
  | Some other -> Types.Other { name = Some other }
  | None -> Types.Other { name = None }
;;

let read_file_path_of_arguments (json_string : string) : string option =
  match Jsonaf.of_string json_string with
  | exception _ -> None
  | `Object fields ->
    List.find_map fields ~f:(fun (key, value) ->
      match key, value with
      | "file", `String p | "path", `String p -> Some p
      | _ -> None)
  | _ -> None
;;

let extract_path_from_call_text (s : string) : string option =
  match String.lsplit2 ~on:'(' s with
  | None -> None
  | Some (_, rest) ->
    let len = String.length rest in
    let args =
      if len > 0 && Char.( = ) (String.get rest (len - 1)) ')'
      then String.sub rest ~pos:0 ~len:(len - 1)
      else rest
    in
    read_file_path_of_arguments args
;;

let create
      ~history_items
      ~messages
      ~input_line
      ~auto_follow
      ~msg_buffers
      ~function_name_by_id
      ~reasoning_idx_by_id
      ~tool_output_by_index
      ~tasks
      ~kv_store
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
  let initial_row_id index =
    Projected_message.Id.local ~namespace:"initial-render" ~local_id:(Int.to_string index)
    |> Result.ok_or_failwith
  in
  let initial_row_ids = Array.init (List.length messages) ~f:initial_row_id in
  let row_chunk_by_id = Hashtbl.create (module Projected_message.Id) in
  Array.iteri initial_row_ids ~f:(fun index id ->
    Hashtbl.set
      row_chunk_by_id
      ~key:id
      ~data:(History_chunk.canonical_index_of_row_exn ~row_index:index));
  { history_items
  ; message_array = Array.of_list messages
  ; render_row_ids = initial_row_ids
  ; transcript_generation = 0
  ; render_generation = 0
  ; message_revisions = Array.create ~len:(List.length messages) 0
  ; input_line
  ; auto_follow
  ; msg_buffers
  ; function_name_by_id
  ; reasoning_idx_by_id
  ; tool_output_by_index
  ; tool_output_by_id = Hashtbl.create (module Projected_message.Id)
  ; call_id_by_item_id = Hashtbl.create (module String)
  ; tool_call_id_by_id = Hashtbl.create (module Projected_message.Id)
  ; tool_call_outcome_by_call_id = Hashtbl.create (module String)
  ; tool_path_by_call_id = Hashtbl.create (module String)
  ; active_page = Page_id.Chat
  ; pages =
      Pages.
        { chat =
            Chat_page_state.
              { scroll_box
              ; msg_img_cache = Hashtbl.create (module Projected_message.Id)
              ; msg_semantic_cache = Hashtbl.create (module Projected_message.Id)
              ; active_history_width = None
              ; preparing_width = None
              ; geometry = Renderer_virtual_list.Geometry.create ()
              ; dirty_height_rows = []
              ; scroll_direction = Toward_newer
              ; materialization = Warm
              ; history_image_cache = None
              ; corridor_history_cache = None
              ; width_snapshots = []
              ; row_chunk_by_id
              ; dirty_history_chunks = Hash_set.create (module Int)
              }
        ; agent = Agent_page_state.empty ()
        ; shell_security = Shell_security_page_state.empty ()
        }
  ; tasks
  ; kv_store
  ; fetch_sw
  ; cursor_pos
  ; selection_anchor
  ; mode
  ; draft_mode
  ; undo_stack
  ; redo_stack
  ; cmdline
  ; cmdline_cursor
  ; typeahead_completion = None
  ; typeahead_preview_open = false
  ; typeahead_preview_scroll = 0
  ; typeahead_generation = 0
  ; search_query = ""
  ; search_cursor = 0
  ; last_search_query = None
  ; last_search_dir = None
  ; activity = None
  ; animation_frame = 0
  ; normal_input_enabled = true
  ; projected = Projected_state.empty ()
  }
;;

let input_line t = t.input_line
let cursor_pos t = t.cursor_pos
let selection_anchor t = t.selection_anchor
let activity t = t.activity

let set_activity t activity =
  if not (Poly.equal t.activity activity)
  then (
    t.activity <- activity;
    t.animation_frame <- 0)
;;

let animation_frame t = t.animation_frame

let advance_animation_frame t =
  t.animation_frame
  <- (if Int.equal t.animation_frame Int.max_value then 0 else t.animation_frame + 1)
;;

let clear_selection t = t.selection_anchor <- None
let set_selection_anchor t idx = t.selection_anchor <- Some idx
let selection_active t = Option.is_some t.selection_anchor
let messages t = Array.to_list t.message_array

let reset_message_projection t messages =
  let old_render_row_ids = t.render_row_ids in
  t.message_array <- messages;
  t.render_row_ids
  <- (if Array.length t.projected.rows = Array.length messages
      then Array.map t.projected.rows ~f:(fun row -> row.Projected_message.id)
      else
        Array.init (Array.length messages) ~f:(fun index ->
          if index < Array.length old_render_row_ids
          then old_render_row_ids.(index)
          else
            Projected_message.Id.local
              ~namespace:"legacy-render"
              ~local_id:(Int.to_string index)
            |> Result.ok_or_failwith));
  t.transcript_generation <- t.transcript_generation + 1;
  t.render_generation <- t.render_generation + 1;
  t.message_revisions <- Array.create ~len:(Array.length messages) 0;
  let chat = t.pages.chat in
  Hashtbl.clear chat.row_chunk_by_id;
  Array.iteri t.render_row_ids ~f:(fun index id ->
    Hashtbl.set
      chat.row_chunk_by_id
      ~key:id
      ~data:(History_chunk.canonical_index_of_row_exn ~row_index:index));
  Hash_set.clear chat.dirty_history_chunks;
  List.iter
    (List.init
       (History_chunk.canonical_count ~row_count:(Array.length messages))
       ~f:Fn.id)
    ~f:(Hash_set.add chat.dirty_history_chunks);
  Hashtbl.clear t.msg_buffers;
  Hashtbl.clear t.reasoning_idx_by_id;
  Hashtbl.clear t.tool_output_by_index;
  Hashtbl.clear t.tool_output_by_id;
  Hashtbl.clear t.tool_call_id_by_id
;;

let set_messages t messages =
  reset_message_projection t (Array.of_list messages);
  let chat = t.pages.chat in
  Hashtbl.clear chat.msg_img_cache;
  Renderer_virtual_list.Geometry.clear chat.geometry;
  chat.dirty_height_rows <- [];
  Hash_set.clear chat.dirty_history_chunks;
  List.iter
    (List.init
       (History_chunk.canonical_count ~row_count:(Array.length t.message_array))
       ~f:Fn.id)
    ~f:(Hash_set.add chat.dirty_history_chunks)
;;

let common_prefix_length left right =
  let limit = Int.min (Array.length left) (Array.length right) in
  let rec loop index =
    if index >= limit || not Poly.(left.(index) = right.(index))
    then index
    else loop (index + 1)
  in
  loop 0
;;

let common_suffix_length left right ~prefix_length =
  let left_length = Array.length left in
  let right_length = Array.length right in
  let limit = Int.min (left_length - prefix_length) (right_length - prefix_length) in
  let rec loop length =
    if
      length >= limit
      || not Poly.(left.(left_length - length - 1) = right.(right_length - length - 1))
    then length
    else loop (length + 1)
  in
  loop 0
;;

let remap_anchor ~old_messages ~new_messages anchor =
  let old_index = Renderer_virtual_list.Anchor.index anchor in
  if old_index < 0 || old_index >= Array.length old_messages
  then None
  else (
    let prefix_length = common_prefix_length old_messages new_messages in
    let suffix_length = common_suffix_length old_messages new_messages ~prefix_length in
    let old_suffix_start = Array.length old_messages - suffix_length in
    if old_index < prefix_length
    then Some anchor
    else if old_index >= old_suffix_start
    then
      Some
        (Renderer_virtual_list.Anchor.remap_index
           anchor
           ~index:(Array.length new_messages - (Array.length old_messages - old_index)))
    else (
      let target = old_messages.(old_index) in
      let occurrence =
        Array.sub old_messages ~pos:0 ~len:(old_index + 1)
        |> Array.count ~f:(fun message -> Poly.(message = target))
      in
      let rec find index remaining =
        if index >= Array.length new_messages
        then None
        else if Poly.(new_messages.(index) = target)
        then
          if remaining = 1
          then Some (Renderer_virtual_list.Anchor.remap_index anchor ~index)
          else find (index + 1) (remaining - 1)
        else find (index + 1) remaining
      in
      find 0 occurrence))
;;

let render_row_identity t ~idx =
  if idx >= 0 && idx < Array.length t.message_array
  then (
    let id = t.render_row_ids.(idx) in
    let revision =
      match Hashtbl.find t.projected.index_by_id id with
      | Some projected_index -> t.projected.rows.(projected_index).revision
      | None -> t.message_revisions.(idx)
    in
    Some (id, revision))
  else None
;;

let render_index_by_id t ~id =
  match Hashtbl.find t.projected.index_by_id id with
  | Some index -> Some index
  | None ->
    Array.find_mapi t.render_row_ids ~f:(fun index candidate ->
      if Projected_message.Id.equal candidate id then Some index else None)
;;

let remove_width_preparation_row_from preparation ~id =
  Hashtbl.remove preparation.Chat_page_state.target_rows id;
  Hash_set.remove preparation.exact_rows id
;;

let invalidate_width_preparation_row t ~id =
  Option.iter t.pages.chat.preparing_width ~f:(fun preparation ->
    if not Poly.(preparation.status = Chat_page_state.Cancelled)
    then (
      remove_width_preparation_row_from preparation ~id;
      Option.iter (render_index_by_id t ~id) ~f:(fun index ->
        let batch_index =
          History_chunk.foreground_batch_index_of_row_exn ~row_index:index
        in
        let chunk_index = History_chunk.canonical_index_of_row_exn ~row_index:index in
        Hash_set.remove preparation.prepared_batches batch_index;
        Hash_set.remove preparation.prepared_chunks chunk_index;
        Hashtbl.remove preparation.partial_chunks chunk_index)))
;;

let reconcile_width_preparation t =
  Option.iter t.pages.chat.preparing_width ~f:(fun preparation ->
    if not Poly.(preparation.status = Chat_page_state.Cancelled)
    then (
      Hashtbl.filteri_inplace preparation.target_rows ~f:(fun ~key:id ~data:entry ->
        match render_index_by_id t ~id with
        | None ->
          Hash_set.remove preparation.exact_rows id;
          false
        | Some index ->
          let role, text = t.message_array.(index) in
          let revision = t.message_revisions.(index) in
          let current =
            Int.equal revision entry.row_revision
            && Int.equal entry.width (fst preparation.terminal_size)
            && String.equal role entry.role
            && String.equal text entry.text
          in
          if not current then Hash_set.remove preparation.exact_rows id;
          current);
      Hash_set.clear preparation.prepared_batches;
      Hash_set.clear preparation.prepared_chunks;
      Hashtbl.clear preparation.partial_chunks;
      preparation.desired_corridor
      <- Option.map preparation.desired_corridor ~f:(fun range ->
           History_chunk.Range.clamp range ~row_count:(Array.length t.render_row_ids));
      preparation.published_corridor
      <- Option.map preparation.published_corridor ~f:(fun range ->
           History_chunk.Range.clamp range ~row_count:(Array.length t.render_row_ids));
      preparation.destination
      <- Option.bind preparation.destination ~f:(fun destination ->
           if Option.is_some (render_index_by_id t ~id:destination.id)
           then Some destination
           else None)))
;;

let resize_anchor_row_key t index =
  render_row_identity t ~idx:index
  |> Option.map ~f:(fun (id, revision) -> { Resize_anchor.id; revision })
;;

let resize_anchor_neighbors t ~index =
  let length = Array.length t.render_row_ids in
  let rec loop distance acc =
    if index - distance < 0 && index + distance >= length
    then List.rev acc
    else (
      let acc =
        if index - distance < 0
        then acc
        else (
          match resize_anchor_row_key t (index - distance) with
          | None -> acc
          | Some key -> { Resize_anchor.key; side = `Older } :: acc)
      in
      let acc =
        if index + distance >= length
        then acc
        else (
          match resize_anchor_row_key t (index + distance) with
          | None -> acc
          | Some key -> { Resize_anchor.key; side = `Newer } :: acc)
      in
      loop (distance + 1) acc)
  in
  loop 1 []
;;

let capture_resize_anchor t ~viewport_height =
  if t.auto_follow
  then Resize_anchor.Follow_bottom
  else (
    let geometry = t.pages.chat.geometry in
    let viewport =
      Renderer_virtual_list.Viewport.compute
        ~geometry
        ~requested_scroll:(Notty_scroll_box.scroll t.pages.chat.scroll_box)
        ~height:viewport_height
        ~follow_bottom:false
    in
    match Renderer_virtual_list.Anchor.create ~geometry ~viewport ~screen_row:0 with
    | None -> Manual_empty
    | Some anchor ->
      let index = Renderer_virtual_list.Anchor.index anchor in
      (match resize_anchor_row_key t index with
       | None -> Manual_empty
       | Some key ->
         Preserve_row { key; anchor; neighbors = resize_anchor_neighbors t ~index }))
;;

let current_resize_anchor_index t (key : Resize_anchor.row_key) =
  render_index_by_id t ~id:key.id
  |> Option.filter ~f:(fun index ->
    Option.exists (render_row_identity t ~idx:index) ~f:(fun (id, revision) ->
      Projected_message.Id.equal id key.id && Int.equal revision key.revision))
;;

let restore_resize_anchor t ~viewport_height anchor =
  let chat = t.pages.chat in
  let geometry = chat.geometry in
  let max_scroll =
    Int.max
      0
      (Renderer_virtual_list.Geometry.total_height geometry - Int.max 0 viewport_height)
  in
  let scroll_to anchor =
    Renderer_virtual_list.Anchor.corrected_scroll anchor ~geometry
    |> Option.value ~default:0
    |> Int.min max_scroll
    |> Notty_scroll_box.scroll_to chat.scroll_box
  in
  match anchor with
  | Resize_anchor.Follow_bottom ->
    t.auto_follow <- true;
    chat.scroll_direction <- Toward_newer;
    Notty_scroll_box.scroll_to chat.scroll_box max_scroll;
    Resize_anchor.Followed_bottom
  | Manual_empty ->
    t.auto_follow <- false;
    Notty_scroll_box.scroll_to chat.scroll_box 0;
    Resize_anchor.Empty
  | Preserve_row { key; anchor; neighbors } ->
    t.auto_follow <- false;
    (match current_resize_anchor_index t key with
     | Some index ->
       scroll_to (Renderer_virtual_list.Anchor.remap_index anchor ~index);
       Resize_anchor.Preserved
     | None ->
       let replacement =
         List.find_map neighbors ~f:(fun neighbor ->
           current_resize_anchor_index t neighbor.key
           |> Option.map ~f:(fun index -> index, neighbor.side))
       in
       (match replacement with
        | Some (index, `Older) ->
          scroll_to (Renderer_virtual_list.Anchor.at_end ~index ~offset:0 ~screen_row:0);
          Resize_anchor.Repaired
        | Some (index, `Newer) ->
          scroll_to (Renderer_virtual_list.Anchor.at_start ~index ~offset:0 ~screen_row:0);
          Resize_anchor.Repaired
        | None ->
          if Renderer_virtual_list.Geometry.length geometry = 0
          then (
            Notty_scroll_box.scroll_to chat.scroll_box 0;
            Resize_anchor.Empty)
          else (
            scroll_to
              (Renderer_virtual_list.Anchor.at_start ~index:0 ~offset:0 ~screen_row:0);
            Resize_anchor.Repaired)))
;;

type viewport_anchor =
  { row_id : Projected_message.Id.t
  ; anchor : Renderer_virtual_list.Anchor.t
  }

let capture_viewport_anchor t =
  let chat = t.pages.chat in
  let geometry = chat.geometry in
  if t.auto_follow || Renderer_virtual_list.Geometry.length geometry = 0
  then None
  else
    Renderer_virtual_list.Anchor.create_at_scroll
      ~geometry
      ~scroll:(Notty_scroll_box.scroll chat.scroll_box)
    |> Option.bind ~f:(fun anchor ->
      let index = Renderer_virtual_list.Anchor.index anchor in
      if index < 0 || index >= Array.length t.render_row_ids
      then None
      else Some { row_id = t.render_row_ids.(index); anchor })
;;

let reconcile_messages_with_anchor t messages viewport_anchor =
  let old_messages = t.message_array in
  let old_render_row_ids = t.render_row_ids in
  let new_messages = Array.of_list messages in
  let new_render_row_ids =
    if Array.length t.projected.rows = Array.length new_messages
    then Array.map t.projected.rows ~f:(fun row -> row.Projected_message.id)
    else old_render_row_ids
  in
  let chat = t.pages.chat in
  let geometry = chat.geometry in
  let anchor =
    match viewport_anchor with
    | Some { row_id; anchor } ->
      Array.find_mapi new_render_row_ids ~f:(fun index candidate ->
        if Projected_message.Id.equal row_id candidate
        then Some (Renderer_virtual_list.Anchor.remap_index anchor ~index)
        else None)
    | None ->
      if t.auto_follow || Renderer_virtual_list.Geometry.length geometry = 0
      then None
      else
        Renderer_virtual_list.Anchor.create_at_scroll
          ~geometry
          ~scroll:(Notty_scroll_box.scroll chat.scroll_box)
        |> Option.bind ~f:(fun anchor ->
          let old_index = Renderer_virtual_list.Anchor.index anchor in
          if old_index < 0 || old_index >= Array.length old_render_row_ids
          then None
          else (
            let id = old_render_row_ids.(old_index) in
            match
              Array.find_mapi new_render_row_ids ~f:(fun index candidate ->
                if Projected_message.Id.equal id candidate then Some index else None)
            with
            | Some index -> Some (Renderer_virtual_list.Anchor.remap_index anchor ~index)
            | None -> remap_anchor ~old_messages ~new_messages anchor))
  in
  let preserved_length = common_prefix_length old_messages new_messages in
  reset_message_projection t new_messages;
  Hashtbl.filteri_inplace chat.msg_img_cache ~f:(fun ~key:id ~data:entry ->
    match render_index_by_id t ~id with
    | None -> false
    | Some index ->
      index < preserved_length
      && Option.equal
           Int.equal
           (render_row_identity t ~idx:index |> Option.map ~f:snd)
           (Some entry.row_revision));
  Renderer_virtual_list.Geometry.reconcile_prefix
    geometry
    ~preserved_length
    ~length:(Array.length new_messages)
    ~estimated_height_at_index:(fun _ -> 5);
  chat.dirty_height_rows <- [];
  if not t.auto_follow
  then
    Option.iter anchor ~f:(fun anchor ->
      Renderer_virtual_list.Anchor.corrected_scroll anchor ~geometry
      |> Option.iter ~f:(Notty_scroll_box.scroll_to chat.scroll_box))
;;

let reconcile_messages t messages = reconcile_messages_with_anchor t messages None
let projected_rows t = t.projected.rows

let projected_messages t =
  Array.to_list t.projected.rows |> List.map ~f:(fun row -> row.Projected_message.message)
;;

let projected_index t ~id = Hashtbl.find t.projected.index_by_id id

let projected_row t ~id =
  Option.map (projected_index t ~id) ~f:(fun index -> t.projected.rows.(index))
;;

let selected_projected_id t = t.projected.selected_id

let mark_selected_history_chunk_dirty t =
  Option.iter t.projected.selected_id ~f:(fun id ->
    Option.iter (Hashtbl.find t.pages.chat.row_chunk_by_id id) ~f:(fun index ->
      Hash_set.add t.pages.chat.dirty_history_chunks index))
;;

let select_projected t id =
  let previous = t.projected.selected_id in
  t.projected.selected_id
  <- Option.bind id ~f:(fun id -> Option.map (render_index_by_id t ~id) ~f:(fun _ -> id));
  if not (Option.equal Projected_message.Id.equal previous t.projected.selected_id)
  then (
    let mark id =
      Option.iter (Hashtbl.find t.pages.chat.row_chunk_by_id id) ~f:(fun index ->
        Hash_set.add t.pages.chat.dirty_history_chunks index)
    in
    Option.iter previous ~f:mark;
    Option.iter t.projected.selected_id ~f:mark)
;;

let request_projected_reveal t ~id =
  if Option.is_some (render_index_by_id t ~id) then t.projected.reveal_id <- Some id
;;

let take_projected_reveal_request t =
  let request = t.projected.reveal_id in
  t.projected.reveal_id <- None;
  request
;;

let set_projected_height t ~id ~height =
  if Hashtbl.mem t.projected.index_by_id id
  then Hashtbl.set t.projected.measured_height_by_id ~key:id ~data:(Int.max 1 height)
;;

let projected_height t ~id = Hashtbl.find t.projected.measured_height_by_id id

let selected_projected_row t =
  Option.bind t.projected.selected_id ~f:(fun id -> projected_row t ~id)
;;

let delete_selected_canonical_entry t =
  match selected_projected_row t with
  | None -> `Rejected "No projected row is selected."
  | Some row ->
    (match row.Projected_message.source with
     | Canonical { entry_id } ->
       t.history_items
       <- List.filter t.history_items ~f:(fun entry ->
            not (History_entry.Id.equal (History_entry.id entry) entry_id));
       `Deleted
     | Moderator_inserted _ | Moderator_replacement _ ->
       `Rejected "Cannot delete a moderator-projected row."
     | Streaming _ | Pending_approval _ | Placeholder _ ->
       `Rejected "Cannot delete a transient UI row.")
;;

let reconcile_projected_rows t rows =
  let old_rows = t.projected.rows in
  let old_selected_index =
    Option.bind t.projected.selected_id ~f:(fun id ->
      Hashtbl.find t.projected.index_by_id id)
  in
  let old_by_id = Hashtbl.create (module Projected_message.Id) in
  Array.iter t.projected.rows ~f:(fun row ->
    Hashtbl.set old_by_id ~key:row.Projected_message.id ~data:row);
  let rows =
    List.map rows ~f:(fun row ->
      Projected_message.reconcile
        ~previous:(Hashtbl.find old_by_id row.Projected_message.id)
        row)
    |> Array.of_list
  in
  let surviving = Hash_set.create (module Projected_message.Id) in
  Hashtbl.clear t.projected.index_by_id;
  Array.iteri rows ~f:(fun index row ->
    if Hash_set.mem surviving row.Projected_message.id
    then
      invalid_arg
        (Printf.sprintf
           "Duplicate projected row ID %s"
           (Projected_message.Id.to_string row.Projected_message.id));
    Hash_set.add surviving row.Projected_message.id;
    Hashtbl.set t.projected.index_by_id ~key:row.Projected_message.id ~data:index);
  Hashtbl.filter_keys_inplace
    t.projected.measured_height_by_id
    ~f:(Hash_set.mem surviving);
  Hashtbl.filteri_inplace t.pages.chat.msg_img_cache ~f:(fun ~key:id ~data:entry ->
    match Hashtbl.find t.projected.index_by_id id with
    | None -> false
    | Some index -> Int.equal rows.(index).revision entry.row_revision);
  Hashtbl.filteri_inplace t.pages.chat.msg_semantic_cache ~f:(fun ~key:id ~data:entry ->
    match Hashtbl.find t.projected.index_by_id id with
    | None -> false
    | Some index -> Int.equal rows.(index).revision entry.row_revision);
  Array.iter rows ~f:(fun row ->
    match Hashtbl.find old_by_id row.Projected_message.id with
    | Some previous when Int.equal previous.revision row.revision -> ()
    | None | Some _ ->
      t.pages.chat.dirty_height_rows
      <- (row.id, row.revision) :: t.pages.chat.dirty_height_rows);
  t.projected.selected_id
  <- (match t.projected.selected_id with
      | Some id when Hash_set.mem surviving id -> Some id
      | Some _ ->
        Option.bind old_selected_index ~f:(fun old_index ->
          if Array.is_empty rows
          then None
          else Some rows.(Int.min old_index (Array.length rows - 1)).id)
      | None -> None);
  t.projected.reveal_id
  <- Option.bind t.projected.reveal_id ~f:(fun id ->
       if Hash_set.mem surviving id then Some id else None);
  t.projected.rows <- rows;
  let first_changed =
    let limit = Int.min (Array.length old_rows) (Array.length rows) in
    let rec loop index =
      if index >= limit
      then
        if Int.equal (Array.length old_rows) (Array.length rows) then None else Some index
      else if
        Projected_message.Id.equal old_rows.(index).id rows.(index).id
        && Int.equal old_rows.(index).revision rows.(index).revision
      then loop (index + 1)
      else Some index
    in
    loop 0
  in
  Hashtbl.clear t.pages.chat.row_chunk_by_id;
  Array.iteri t.render_row_ids ~f:(fun index id ->
    Hashtbl.set
      t.pages.chat.row_chunk_by_id
      ~key:id
      ~data:(History_chunk.canonical_index_of_row_exn ~row_index:index));
  Option.iter first_changed ~f:(fun first ->
    let count = Int.max (Array.length old_rows) (Array.length rows) in
    History_chunk.Range.create_exn ~first ~past:count
    |> History_chunk.canonical_indices_intersecting ~row_count:count
    |> List.iter ~f:(Hash_set.add t.pages.chat.dirty_history_chunks));
  Renderer_virtual_list.Geometry.rebuild
    t.projected.geometry
    ~length:(Array.length rows)
    ~height_at_index:(fun index ->
      Hashtbl.find t.projected.measured_height_by_id rows.(index).Projected_message.id
      |> Option.value ~default:5)
;;

let reconcile_projected_messages t ~rows ~messages =
  let anchor = capture_viewport_anchor t in
  reconcile_projected_rows t rows;
  reconcile_messages_with_anchor t messages anchor;
  reconcile_width_preparation t
;;

let projection_damage_requires_redraw = function
  | No_damage | Below_viewport -> false
  | Visible_damage | Above_viewport | Unknown_damage -> true
;;

let classify_row_by_exact_prefix
      ~length
      ~prefix
      ~exact_prefix_length
      ~requested_scroll
      ~viewport_height
      ~index
  =
  if viewport_height <= 0 || index < 0 || index >= length
  then Unknown
  else (
    let view_start = Int.max 0 requested_scroll in
    let view_past =
      if view_start > Int.max_value - viewport_height
      then Int.max_value
      else view_start + viewport_height
    in
    let row_start = prefix.(index) in
    let row_past = prefix.(index + 1) in
    if index < exact_prefix_length && row_past <= view_start
    then Above
    else if index <= exact_prefix_length && row_start >= view_past
    then Below
    else if
      index < exact_prefix_length
      && row_start < view_past
      && row_past > view_start
      && prefix.(exact_prefix_length) >= view_past
    then Visible
    else Unknown)
;;

let classify_live_row t ~viewport_height ~index =
  if t.auto_follow
  then Unknown
  else (
    let geometry = t.pages.chat.geometry in
    classify_row_by_exact_prefix
      ~length:(Renderer_virtual_list.Geometry.length geometry)
      ~prefix:(Renderer_virtual_list.Geometry.prefix geometry)
      ~exact_prefix_length:(Renderer_virtual_list.Geometry.exact_prefix_length geometry)
      ~requested_scroll:(Notty_scroll_box.scroll t.pages.chat.scroll_box)
      ~viewport_height
      ~index)
;;

let relation_at_index t ~viewport_height ~index =
  classify_live_row t ~viewport_height ~index
;;

let combine_projection_damage left right =
  match left, right with
  | Unknown_damage, _ | _, Unknown_damage -> Unknown_damage
  | Above_viewport, _ | _, Above_viewport -> Above_viewport
  | Visible_damage, _ | _, Visible_damage -> Visible_damage
  | Below_viewport, _ | _, Below_viewport -> Below_viewport
  | No_damage, No_damage -> No_damage
;;

let reconcile_projected_messages_with_damage t ~viewport_height ~rows ~messages =
  let old_rows = Array.copy t.projected.rows in
  let old_geometry = Renderer_virtual_list.Geometry.snapshot t.pages.chat.geometry in
  let requested_scroll = Notty_scroll_box.scroll t.pages.chat.scroll_box in
  let old_relation index =
    if t.auto_follow
    then Unknown
    else
      classify_row_by_exact_prefix
        ~length:(Renderer_virtual_list.Geometry.Snapshot.length old_geometry)
        ~prefix:(Renderer_virtual_list.Geometry.Snapshot.prefix old_geometry)
        ~exact_prefix_length:
          (Renderer_virtual_list.Geometry.Snapshot.exact_prefix_length old_geometry)
        ~requested_scroll
        ~viewport_height
        ~index
  in
  let old_by_id = Hashtbl.create (module Projected_message.Id) in
  Array.iteri old_rows ~f:(fun index row ->
    Hashtbl.set old_by_id ~key:row.id ~data:(index, row.revision));
  reconcile_projected_messages t ~rows ~messages;
  let changed = ref [] in
  let new_ids = Hash_set.create (module Projected_message.Id) in
  Array.iteri t.projected.rows ~f:(fun index row ->
    Hash_set.add new_ids row.id;
    match Hashtbl.find old_by_id row.id with
    | Some (old_index, old_revision)
      when Int.equal old_index index && Int.equal old_revision row.revision -> ()
    | Some (old_index, _) ->
      changed := (Some (old_relation old_index), Some row.id) :: !changed
    | None -> changed := (None, Some row.id) :: !changed);
  Array.iteri old_rows ~f:(fun index row ->
    if not (Hash_set.mem new_ids row.id)
    then changed := (Some (old_relation index), None) :: !changed);
  if List.is_empty !changed
  then No_damage
  else if t.auto_follow
  then Visible_damage
  else
    List.fold !changed ~init:No_damage ~f:(fun damage (old_relation, new_id) ->
      let relation_damage = function
        | Above -> Above_viewport
        | Visible -> Visible_damage
        | Below -> Below_viewport
        | Unknown -> Unknown_damage
      in
      let damage =
        Option.value_map old_relation ~default:damage ~f:(fun relation ->
          combine_projection_damage damage (relation_damage relation))
      in
      Option.value_map new_id ~default:damage ~f:(fun id ->
        let relation =
          match render_index_by_id t ~id with
          | None -> Unknown
          | Some index -> classify_live_row t ~viewport_height ~index
        in
        combine_projection_damage damage (relation_damage relation)))
;;

let tasks t = t.tasks
let kv_store t = t.kv_store

let tool_output_by_index t =
  Hashtbl.clear t.tool_output_by_index;
  Hashtbl.iteri t.tool_output_by_id ~f:(fun ~key:id ~data ->
    Option.iter (render_index_by_id t ~id) ~f:(fun index ->
      Hashtbl.set t.tool_output_by_index ~key:index ~data));
  t.tool_output_by_index
;;

let tool_output_for_row t ~id = Hashtbl.find t.tool_output_by_id id

let invalidate_render_metadata t ~idx =
  if idx >= 0 && idx < Array.length t.message_revisions
  then (
    t.message_revisions.(idx) <- t.message_revisions.(idx) + 1;
    t.render_generation <- t.render_generation + 1;
    let chat = t.pages.chat in
    Option.iter (render_row_identity t ~idx) ~f:(fun (id, revision) ->
      Hashtbl.remove chat.msg_img_cache id;
      chat.dirty_height_rows <- (id, revision) :: chat.dirty_height_rows;
      Option.iter (Hashtbl.find chat.row_chunk_by_id id) ~f:(fun chunk_index ->
        Hash_set.add chat.dirty_history_chunks chunk_index);
      invalidate_width_preparation_row t ~id))
;;

let invalidate_render_metadata_by_id t ~id =
  Option.iter (render_index_by_id t ~id) ~f:(fun idx -> invalidate_render_metadata t ~idx)
;;

let set_tool_output_kind_for_row t ~id kind =
  let changed =
    match Hashtbl.find t.tool_output_by_id id with
    | Some existing -> not Poly.(existing = kind)
    | None -> true
  in
  if changed
  then (
    Hashtbl.set t.tool_output_by_id ~key:id ~data:kind;
    invalidate_render_metadata_by_id t ~id);
  changed
;;

let set_tool_output_kind t ~idx kind =
  match render_row_identity t ~idx with
  | None -> false
  | Some (id, _) -> set_tool_output_kind_for_row t ~id kind
;;

let mark_tool_call_finished t ~call_id ~outcome =
  let changed =
    match Hashtbl.find t.tool_call_outcome_by_call_id call_id with
    | Some existing -> not Poly.(existing = outcome)
    | None -> true
  in
  if changed
  then (
    Hashtbl.set t.tool_call_outcome_by_call_id ~key:call_id ~data:outcome;
    Hashtbl.iteri t.tool_call_id_by_id ~f:(fun ~key:id ~data ->
      if String.equal data call_id then invalidate_render_metadata_by_id t ~id));
  changed
;;

let tool_call_outcome_for_row t ~id =
  Option.bind
    (Hashtbl.find t.tool_call_id_by_id id)
    ~f:(Hashtbl.find t.tool_call_outcome_by_call_id)
;;

let tool_call_outcome_for_message t ~idx =
  Option.bind (render_row_identity t ~idx) ~f:(fun (id, _) ->
    tool_call_outcome_for_row t ~id)
;;

let clear_tool_call_outcomes t =
  let changed =
    (not (Hashtbl.is_empty t.tool_call_id_by_id))
    || not (Hashtbl.is_empty t.tool_call_outcome_by_call_id)
  in
  Hashtbl.iter_keys t.tool_call_id_by_id ~f:(fun id ->
    invalidate_render_metadata_by_id t ~id);
  if changed then t.render_generation <- t.render_generation + 1;
  Hashtbl.clear t.tool_call_id_by_id;
  Hashtbl.clear t.tool_call_outcome_by_call_id;
  let chat = t.pages.chat in
  Hashtbl.clear chat.msg_img_cache;
  Renderer_virtual_list.Geometry.clear chat.geometry;
  chat.dirty_height_rows <- []
;;

let active_page t = t.active_page
let set_active_page t page = t.active_page <- page
let shell_security_page t = t.pages.shell_security
let shell_security_snapshot t = t.pages.shell_security.snapshot
let set_shell_security_snapshot t snapshot =
  let page = t.pages.shell_security in
  page.snapshot <- snapshot;
  let ids =
    List.map snapshot.Shell_security_page_state.grants ~f:(fun grant ->
      grant.Session.Shell_state.Approval_grant.grant_id)
  in
  page.selected_grant_id
  <- (match page.selected_grant_id with
      | Some id when List.mem ids id ~equal:String.equal -> Some id
      | _ -> List.hd ids)
;;
let shell_security_tab t = t.pages.shell_security.tab
let set_shell_security_tab t tab = t.pages.shell_security.tab <- tab
let shell_security_scroll_box t = t.pages.shell_security.scroll_box
let shell_approval_modal t = t.pages.shell_security.approval_modal
let shell_grant_revoke_modal t = t.pages.shell_security.grant_revoke_modal
let moderator_modal t = t.pages.shell_security.moderator_modal
let selected_shell_grant_id t = t.pages.shell_security.selected_grant_id

let move_shell_grant_selection t delta =
  let page = t.pages.shell_security in
  let ids =
    List.map page.snapshot.grants ~f:(fun grant ->
      grant.Session.Shell_state.Approval_grant.grant_id)
  in
  match ids with
  | [] -> page.selected_grant_id <- None
  | _ ->
    let current =
      Option.bind page.selected_grant_id ~f:(fun id ->
        List.findi ids ~f:(fun _ candidate -> String.equal candidate id)
        |> Option.map ~f:fst)
      |> Option.value ~default:0
    in
    let next = (current + delta + List.length ids) mod List.length ids in
    page.selected_grant_id <- List.nth ids next
;;

let default_approval_choice (request : Shell_runtime.Approval_broker.ui_request) =
  let scopes = request.Shell_runtime.Approval_broker.scopes in
  if List.mem scopes Chatmd_shell_spec.Shell_spec.Once ~equal:Chatmd_shell_spec.Shell_spec.equal_approval_scope
  then Shell_security_page_state.Once
  else if List.mem scopes Exact_session ~equal:Chatmd_shell_spec.Shell_spec.equal_approval_scope
  then Exact_session
  else if List.mem scopes Prefix_session ~equal:Chatmd_shell_spec.Shell_spec.equal_approval_scope
  then Prefix_session
  else Durable_exact
;;

let open_shell_approval_modal
      t
      ~(request : Shell_runtime.Approval_broker.ui_request)
      ~queue_count
  =
  match t.pages.shell_security.approval_modal with
  | Some modal when String.equal modal.request.id request.id ->
    t.pages.shell_security.approval_modal <- Some { modal with queue_count }
  | None | Some _ ->
    t.pages.shell_security.approval_modal
    <- Some
         { request
         ; queue_count
         ; selected = default_approval_choice request
         ; more_options = false
         ; details_expanded = false
         ; stage = Choose
         }
;;

let close_shell_approval_modal t = t.pages.shell_security.approval_modal <- None

let open_shell_grant_revoke_modal t =
  let page = t.pages.shell_security in
  Option.bind page.selected_grant_id ~f:(fun grant_id ->
    List.find page.snapshot.grants ~f:(fun grant ->
      String.equal grant.Session.Shell_state.Approval_grant.grant_id grant_id))
  |> Option.iter ~f:(fun grant ->
    if Option.is_none grant.revoked_at_ns
    then (
      let generation = page.next_management_generation in
      page.next_management_generation <- generation + 1;
      page.grant_revoke_modal
      <- Some
           { generation
           ; grant_id = grant.grant_id
           ; runtime_id = grant.runtime_id
           ; command_sha256 = grant.command_sha256
           ; stage = Confirm_revoke
           }))
;;

let close_shell_grant_revoke_modal t =
  t.pages.shell_security.grant_revoke_modal <- None
;;

let mark_shell_grant_revoking t ~generation ~grant_id =
  Option.iter t.pages.shell_security.grant_revoke_modal ~f:(fun modal ->
    if Int.equal modal.generation generation && String.equal modal.grant_id grant_id
    then modal.stage <- Revoking)
;;

let fail_shell_grant_revoke t ~generation ~grant_id message =
  Option.iter t.pages.shell_security.grant_revoke_modal ~f:(fun modal ->
    if Int.equal modal.generation generation && String.equal modal.grant_id grant_id
    then modal.stage <- Revoke_failed message)
;;

let moderator_request_equal left right =
  match left, right with
  | ( Chat_response.In_memory_stream.Ask_text { prompt = left }
    , Chat_response.In_memory_stream.Ask_text { prompt = right } ) ->
    String.equal left right
  | ( Chat_response.In_memory_stream.Ask_choice
        { prompt = left_prompt; choices = left_choices }
    , Chat_response.In_memory_stream.Ask_choice
        { prompt = right_prompt; choices = right_choices } ) ->
    String.equal left_prompt right_prompt
    && Array.equal String.equal left_choices right_choices
  | Chat_response.In_memory_stream.Ask_text _, Ask_choice _
  | Ask_choice _, Ask_text _ -> false
;;

let open_moderator_modal t request =
  let page = t.pages.shell_security in
  match page.moderator_modal with
  | Some modal when moderator_request_equal modal.request request -> ()
  | None | Some _ ->
    let response, selected_choice =
      match request with
      | Chat_response.In_memory_stream.Ask_text _ -> "", 0
      | Chat_response.In_memory_stream.Ask_choice { choices; _ } ->
        (if Array.is_empty choices then "" else choices.(0)), 0
    in
    page.moderator_modal
    <- Some
         { request
         ; response
         ; cursor = String.length response
         ; selected_choice
         ; validation_error = None
         }
;;

let close_moderator_modal t = t.pages.shell_security.moderator_modal <- None

let set_moderator_validation_error t error =
  Option.iter t.pages.shell_security.moderator_modal ~f:(fun modal ->
    modal.validation_error <- error)
;;

let shell_audit_load_state t = t.pages.shell_security.audit_load_state
let selected_shell_audit_request_id t = t.pages.shell_security.selected_audit_request_id

let begin_shell_management_load t =
  let page = t.pages.shell_security in
  let generation = page.next_management_generation in
  page.next_management_generation <- generation + 1;
  page.audit_load_state <- Audit_loading generation;
  generation
;;

let repair_audit_selection page audit_page =
  let ids =
    List.map audit_page.Shell_security_page_state.requests ~f:(fun request ->
      request.Shell_security_page_state.request_id)
  in
  page.Shell_security_page_state.selected_audit_request_id
  <- (match page.Shell_security_page_state.selected_audit_request_id with
      | Some id when List.mem ids id ~equal:String.equal -> Some id
      | _ -> List.hd ids)
;;

let finish_shell_management_load t ~generation audit_page =
  let page = t.pages.shell_security in
  match page.audit_load_state with
  | Audit_loading current when Int.equal current generation ->
    page.audit_load_state <- Audit_loaded audit_page;
    repair_audit_selection page audit_page;
    true
  | Audit_not_loaded | Audit_loading _ | Audit_loaded _ | Audit_failed _ -> false
;;

let fail_shell_management_load t ~generation message =
  let page = t.pages.shell_security in
  match page.audit_load_state with
  | Audit_loading current when Int.equal current generation ->
    page.audit_load_state <- Audit_failed message;
    true
  | Audit_not_loaded | Audit_loading _ | Audit_loaded _ | Audit_failed _ -> false
;;

let move_shell_audit_selection t delta =
  let page = t.pages.shell_security in
  match page.audit_load_state with
  | Audit_loaded audit_page ->
    let ids = List.map audit_page.requests ~f:(fun request -> request.request_id) in
    (match ids with
     | [] -> page.selected_audit_request_id <- None
     | _ ->
       let current =
         Option.bind page.selected_audit_request_id ~f:(fun selected ->
           List.findi ids ~f:(fun _ id -> String.equal id selected) |> Option.map ~f:fst)
         |> Option.value ~default:0
       in
       let next = (current + delta + List.length ids) mod List.length ids in
       page.selected_audit_request_id <- Some (List.nth_exn ids next))
  | Audit_not_loaded | Audit_loading _ | Audit_failed _ -> ()
;;

let set_shell_approval_choice t selected =
  Option.iter t.pages.shell_security.approval_modal ~f:(fun modal ->
    modal.selected <- selected)
;;

let toggle_shell_approval_more_options t =
  Option.iter t.pages.shell_security.approval_modal ~f:(fun modal ->
    modal.more_options <- not modal.more_options)
;;

let toggle_shell_approval_details t =
  Option.iter t.pages.shell_security.approval_modal ~f:(fun modal ->
    modal.details_expanded <- not modal.details_expanded)
;;

let set_shell_approval_stage t stage =
  Option.iter t.pages.shell_security.approval_modal ~f:(fun modal ->
    modal.stage <- stage)
;;

let shell_interaction_id t =
  match t.pages.shell_security.approval_modal with
  | Some modal -> Some modal.request.id
  | None ->
    (match t.pages.shell_security.grant_revoke_modal with
     | Some modal -> Some ("grant-revoke:" ^ modal.grant_id)
     | None ->
       Option.map t.pages.shell_security.moderator_modal ~f:(fun modal ->
         match modal.request with
         | Chat_response.In_memory_stream.Ask_text _ -> "moderator-text"
         | Chat_response.In_memory_stream.Ask_choice _ -> "moderator-choice"))
;;

let shell_interaction_uses_cursor t =
  match t.pages.shell_security.approval_modal with
  | Some { stage = Deny_reason _; _ } -> true
  | Some _ -> false
  | None ->
    (match t.pages.shell_security.grant_revoke_modal with
     | Some _ -> false
     | None ->
       Option.exists t.pages.shell_security.moderator_modal ~f:(fun modal ->
         match modal.request with
         | Chat_response.In_memory_stream.Ask_text _ -> true
         | Ask_choice _ -> false))
;;
let chat_page t = t.pages.chat
let agent_page t = t.pages.agent
let scroll_box t = (chat_page t).scroll_box
let agent_scroll_box t = (agent_page t).scroll_box

let selected_msg t =
  Option.bind t.projected.selected_id ~f:(fun id -> render_index_by_id t ~id)
;;

let chat_scroll_direction t = (chat_page t).scroll_direction
let set_chat_scroll_direction t direction = (chat_page t).scroll_direction <- direction
let render_messages t = t.message_array
let transcript_generation t = t.transcript_generation

let message_revision t ~idx =
  if idx < 0 || idx >= Array.length t.message_revisions
  then None
  else Some t.message_revisions.(idx)
;;

let bump_message_revision t ~idx =
  if idx >= 0 && idx < Array.length t.message_revisions
  then (
    t.message_revisions.(idx) <- t.message_revisions.(idx) + 1;
    t.render_generation <- t.render_generation + 1)
;;

let auto_follow t = t.auto_follow

let chat_max_scroll t ~viewport_height =
  Int.max
    0
    (Renderer_virtual_list.Geometry.total_height (chat_page t).geometry
     - Int.max 0 viewport_height)
;;

let prepared_row_range t =
  Option.map (chat_page t).corridor_history_cache ~f:(fun cache -> cache.rows)
;;

let prepared_scroll_interval t ~viewport_height =
  match prepared_row_range t with
  | None -> None
  | Some rows ->
    let geometry = (chat_page t).geometry in
    let prefix = Renderer_virtual_list.Geometry.prefix geometry in
    if rows.first < 0 || rows.past < rows.first || rows.past >= Array.length prefix
    then None
    else (
      let viewport_height = Int.max 0 viewport_height in
      let first = prefix.(rows.first) in
      let past = prefix.(rows.past) in
      let last = Int.min (chat_max_scroll t ~viewport_height) (past - viewport_height) in
      if last < first then None else Some (first, last))
;;

let prepared_boundary_distances t ~viewport_height =
  Option.map (prepared_scroll_interval t ~viewport_height) ~f:(fun (first, last) ->
    let scroll = Notty_scroll_box.scroll (scroll_box t) in
    { older = Int.max 0 (scroll - first); newer = Int.max 0 (last - scroll) })
;;

let requested_scroll_is_prepared t ~viewport_height ~requested_scroll =
  match prepared_scroll_interval t ~viewport_height with
  | None -> false
  | Some (first, last) -> requested_scroll >= first && requested_scroll <= last
;;

let row_scroll t ~viewport_height ~index ~placement =
  let geometry = (chat_page t).geometry in
  let prefix = Renderer_virtual_list.Geometry.prefix geometry in
  if index < 0 || index >= Renderer_virtual_list.Geometry.length geometry
  then None
  else (
    let start = prefix.(index) in
    let past = prefix.(index + 1) in
    let requested =
      match (placement : Chat_page_state.Destination.placement) with
      | Top -> start
      | Center -> start - (Int.max 0 viewport_height / 2)
      | Bottom -> past - Int.max 0 viewport_height
    in
    Some (Int.min (chat_max_scroll t ~viewport_height) (Int.max 0 requested)))
;;

let reveal_prepared_row t ~viewport_height ~id ~placement =
  match render_index_by_id t ~id with
  | None -> false
  | Some index ->
    (match row_scroll t ~viewport_height ~index ~placement with
     | None -> false
     | Some requested_scroll ->
       if not (requested_scroll_is_prepared t ~viewport_height ~requested_scroll)
       then false
       else (
         Notty_scroll_box.scroll_to (scroll_box t) requested_scroll;
         t.auto_follow <- Int.equal requested_scroll (chat_max_scroll t ~viewport_height);
         if t.auto_follow then (chat_page t).scroll_direction <- Toward_newer;
         true))
;;

let scroll_chat t ~viewport_height delta =
  let scroll_box = scroll_box t in
  let before = Notty_scroll_box.scroll scroll_box in
  let global_max = chat_max_scroll t ~viewport_height in
  let requested =
    if delta > 0 && before > Int.max_value - delta
    then Int.max_value
    else if delta < 0 && before < Int.min_value - delta
    then Int.min_value
    else before + delta
  in
  let first, last =
    match (chat_page t).materialization, prepared_scroll_interval t ~viewport_height with
    | Chat_page_state.Corridor, Some interval -> interval
    | (Loading | Resizing | Warm), _ | Corridor, None -> 0, global_max
  in
  let scroll = Int.min last (Int.max first requested) in
  Notty_scroll_box.scroll_to scroll_box scroll;
  if delta < 0
  then (chat_page t).scroll_direction <- Toward_older
  else if delta > 0
  then (chat_page t).scroll_direction <- Toward_newer;
  let is_at_bottom = Int.equal scroll global_max in
  t.auto_follow <- is_at_bottom;
  if is_at_bottom then (chat_page t).scroll_direction <- Toward_newer;
  { changed = not (Int.equal before scroll)
  ; clamped = not (Int.equal requested scroll)
  ; distances = prepared_boundary_distances t ~viewport_height
  }
;;

let scroll_chat_by t ~viewport_height delta =
  ignore (scroll_chat t ~viewport_height delta : chat_scroll_result);
  Int.equal (Notty_scroll_box.scroll (scroll_box t)) (chat_max_scroll t ~viewport_height)
;;

let follow_chat_bottom t ~viewport_height =
  t.auto_follow <- true;
  (chat_page t).scroll_direction <- Toward_newer;
  Notty_scroll_box.scroll_to (scroll_box t) (chat_max_scroll t ~viewport_height)
;;

let cmdline t = t.cmdline
let cmdline_cursor t = t.cmdline_cursor
let set_cmdline t s = t.cmdline <- s
let set_cmdline_cursor t n = t.cmdline_cursor <- n
let search_query t = t.search_query
let search_cursor t = t.search_cursor
let set_search_query t s = t.search_query <- s
let set_search_cursor t n = t.search_cursor <- n
let last_search_query t = t.last_search_query
let last_search_dir t = t.last_search_dir

let set_last_search t ~query ~dir =
  t.last_search_query <- Some query;
  t.last_search_dir <- Some dir;
  mark_selected_history_chunk_dirty t
;;

let selected_render_revision t =
  match t.last_search_query with
  | None -> ""
  | Some query -> query
;;

(* ------------------------------------------------------------------------- *)
(*  Type-ahead completion helpers                                             *)
(* ------------------------------------------------------------------------- *)

let typeahead_completion t = t.typeahead_completion
let set_typeahead_completion t v = t.typeahead_completion <- v

let clear_typeahead t =
  t.typeahead_completion <- None;
  t.typeahead_preview_open <- false;
  t.typeahead_preview_scroll <- 0
;;

let clear_last_search t =
  t.last_search_query <- None;
  t.last_search_dir <- None;
  mark_selected_history_chunk_dirty t
;;

let typeahead_preview_open t = t.typeahead_preview_open
let set_typeahead_preview_open t v = t.typeahead_preview_open <- v
let typeahead_preview_scroll t = t.typeahead_preview_scroll
let set_typeahead_preview_scroll t v = t.typeahead_preview_scroll <- v

let bump_typeahead_generation t =
  t.typeahead_generation <- t.typeahead_generation + 1;
  t.typeahead_generation
;;

let typeahead_is_relevant t =
  match t.mode, t.typeahead_completion with
  | Insert, Some completion ->
    String.equal completion.base_input t.input_line
    && Int.equal completion.base_cursor t.cursor_pos
  | (Normal | Cmdline | Search _), _ | Insert, None -> false
;;

let agent_call_id (call : Agent_page_state.call) = call.call_id
let agent_call_name (call : Agent_page_state.call) = call.name
let agent_call_kind (call : Agent_page_state.call) = call.kind
let agent_call_payload (call : Agent_page_state.call) = call.payload
let agent_call_agent_page_kind (call : Agent_page_state.call) = call.agent_page_kind
let agent_call_start_order (call : Agent_page_state.call) = call.start_order
let agent_call_progress_entries (call : Agent_page_state.call) = call.entries
let agent_call_is_truncated (call : Agent_page_state.call) = call.is_truncated
let agent_call_outcome (call : Agent_page_state.call) = call.outcome
let agent_call_output (call : Agent_page_state.call) = call.output

let render_id_of_entry = function
  | Agent_page_state.Text entry -> entry.render_id
  | Tool tool -> tool.render_id
;;

let revision_of_entry = function
  | Agent_page_state.Text entry -> entry.revision
  | Tool tool -> tool.revision
;;

let outcome_revision = function
  | Ochat_function.Trace.Returned -> 1
  | Raised -> 2
  | Cancelled -> 3
;;

let agent_call_render_blocks (call : Agent_page_state.call) =
  let invocation =
    Agent_page_state.
      { id = -1
      ; revision = 0
      ; view =
          Invocation
            { name = call.name
            ; payload = call.payload
            ; agent_page_kind = call.agent_page_kind
            }
      }
  in
  let truncation =
    if call.is_truncated
    then [ Agent_page_state.{ id = -2; revision = 0; view = Truncation } ]
    else []
  in
  let progress =
    List.map call.entries ~f:(fun entry ->
      Agent_page_state.
        { id = render_id_of_entry entry
        ; revision = revision_of_entry entry
        ; view = Progress entry
        })
  in
  let tail =
    match call.entries, call.outcome with
    | [], None -> [ Agent_page_state.{ id = -3; revision = 0; view = Waiting } ]
    | _, None -> []
    | _, Some outcome ->
      [ Agent_page_state.
          { id = -4; revision = outcome_revision outcome; view = Status outcome }
      ]
  in
  (invocation :: truncation) @ progress @ tail
;;

let agent_render_block_id (block : Agent_page_state.render_block) = block.id
let agent_render_block_revision (block : Agent_page_state.render_block) = block.revision
let agent_render_block_view (block : Agent_page_state.render_block) = block.view

let find_agent_render_cache (call : Agent_page_state.call) block =
  match Hashtbl.find call.render_cache block.Agent_page_state.id, call.render_width with
  | Some cache, Some width
    when Int.equal cache.width width && Int.equal cache.revision block.revision ->
    Some (cache.image, cache.height)
  | (Some _ | None), _ -> None
;;

let set_agent_render_cache (call : Agent_page_state.call) block ~image =
  let width = Option.value_exn call.render_width in
  Hashtbl.set
    call.render_cache
    ~key:block.Agent_page_state.id
    ~data:
      Agent_page_state.
        { width; revision = block.revision; image; height = Notty.I.height image }
;;

let prepare_agent_render_width (call : Agent_page_state.call) ~width =
  if not (Option.equal Int.equal call.render_width (Some width))
  then (
    call.render_width <- Some width;
    Hashtbl.clear call.render_cache;
    call.render_block_ids <- [||];
    call.render_block_revisions <- [||];
    Renderer_virtual_list.Geometry.clear call.render_geometry)
;;

let prune_agent_render_cache (call : Agent_page_state.call) ~block_ids =
  let valid = Int.Hash_set.of_list block_ids in
  Hashtbl.filter_keys_inplace call.render_cache ~f:(Hash_set.mem valid)
;;

let agent_render_block_ids (call : Agent_page_state.call) = call.render_block_ids

let agent_render_block_revisions (call : Agent_page_state.call) =
  call.render_block_revisions
;;

let agent_render_geometry (call : Agent_page_state.call) = call.render_geometry

let agent_render_heights call =
  Renderer_virtual_list.Geometry.heights (agent_render_geometry call)
;;

let agent_render_prefix call =
  Renderer_virtual_list.Geometry.prefix (agent_render_geometry call)
;;

let set_agent_render_geometry call ~block_ids ~revisions ~heights ~prefix =
  call.Agent_page_state.render_block_ids <- block_ids;
  call.render_block_revisions <- revisions;
  Renderer_virtual_list.Geometry.replace call.render_geometry ~heights ~prefix
;;

let output_text = function
  | Openai.Responses.Tool_output.Output.Text text -> text
  | Content parts ->
    List.map parts ~f:(function
      | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
      | Input_image { image_url; _ } -> image_url)
    |> String.concat ~sep:"\n"
;;

let progress_entry_text_view = function
  | Agent_page_state.Text entry -> Some (entry.channel, entry.text)
  | Tool _ -> None
;;

let progress_entry_tool_view = function
  | Agent_page_state.Text _ -> None
  | Tool tool ->
    Some
      ( tool.call_id
      , tool.name
      , tool.kind
      , tool.payload
      , List.map tool.progress ~f:(fun entry -> entry.channel, entry.text)
      , tool.outcome
      , tool.output )
;;

let progress_entry_text entry =
  match progress_entry_text_view entry with
  | Some (_, text) -> text
  | None ->
    (match progress_entry_tool_view entry with
     | None -> ""
     | Some (_, _, _, payload, progress, _, output) ->
       let progress = List.map progress ~f:snd |> String.concat in
       let output = Option.value_map output ~default:"" ~f:output_text in
       payload ^ progress ^ output)
;;

let active_agent_calls t =
  let agent = agent_page t in
  List.filter_map agent.call_order ~f:(Hashtbl.find agent.calls)
;;

let selected_agent_call t =
  let agent = agent_page t in
  Option.bind agent.selected_call_id ~f:(Hashtbl.find agent.calls)
;;

let rotate_selection t direction =
  let agent = agent_page t in
  match agent.call_order, agent.selected_call_id with
  | [], _ -> agent.selected_call_id <- None
  | call_ids, None -> agent.selected_call_id <- List.hd call_ids
  | call_ids, Some selected ->
    let count = List.length call_ids in
    let current =
      Option.value
        (List.findi call_ids ~f:(fun _ id -> String.equal id selected))
        ~default:(0, List.hd_exn call_ids)
      |> fst
    in
    let next = (current + direction + count) mod count in
    agent.selected_call_id <- List.nth call_ids next
;;

let select_next_agent_call t = rotate_selection t 1
let select_previous_agent_call t = rotate_selection t (-1)
let agent_auto_follow t = (agent_page t).auto_follow
let set_agent_auto_follow t auto_follow = (agent_page t).auto_follow <- auto_follow
let text_entry_bytes (entry : Agent_page_state.text_entry) = String.length entry.text

let progress_entry_bytes = function
  | Agent_page_state.Text entry -> text_entry_bytes entry
  | Tool tool ->
    String.length tool.payload
    + List.sum (module Int) tool.progress ~f:text_entry_bytes
    + Option.value_map tool.output ~default:0 ~f:(fun output ->
      String.length (output_text output))
;;

let refresh_retained_bytes (call : Agent_page_state.call) =
  call.retained_bytes <- List.sum (module Int) call.entries ~f:progress_entry_bytes
;;

let drop_oldest_entry (call : Agent_page_state.call) =
  match call.entries with
  | [] -> false
  | entry :: rest ->
    call.entries <- rest;
    call.retained_bytes <- call.retained_bytes - progress_entry_bytes entry;
    call.is_truncated <- true;
    true
;;

let utf8_suffix text max_bytes =
  let length = String.length text in
  if length <= max_bytes
  then text
  else (
    let start = length - max_bytes in
    let rec find_boundary index =
      if index >= length
      then length
      else (
        let byte = Char.to_int text.[index] in
        if byte land 0xC0 <> 0x80 then index else find_boundary (index + 1))
    in
    let start = find_boundary start in
    String.sub text ~pos:start ~len:(length - start))
;;

let enforce_call_limit (call : Agent_page_state.call) =
  while
    call.retained_bytes > 1_000_000
    && List.length call.entries > 1
    && drop_oldest_entry call
  do
    ()
  done;
  match call.entries with
  | [] -> ()
  | _ when call.retained_bytes <= 1_000_000 -> ()
  | Agent_page_state.Text entry :: rest ->
    let text = utf8_suffix entry.text 1_000_000 in
    call.entries <- Text { entry with text; revision = entry.revision + 1 } :: rest;
    refresh_retained_bytes call;
    call.is_truncated <- true
  | Tool tool :: rest ->
    let rec trim_progress () =
      if call.retained_bytes <= 1_000_000
      then ()
      else (
        match tool.progress with
        | [] -> ()
        | _ :: tail ->
          tool.progress <- tail;
          tool.revision <- tool.revision + 1;
          refresh_retained_bytes call;
          call.is_truncated <- true;
          trim_progress ())
    in
    trim_progress ();
    if call.retained_bytes > 1_000_000
    then (
      tool.output
      <- Option.map tool.output ~f:(fun output ->
           Openai.Responses.Tool_output.Output.Text
             (utf8_suffix (output_text output) 1_000_000));
      refresh_retained_bytes call;
      call.is_truncated <- true;
      tool.revision <- tool.revision + 1);
    if call.retained_bytes > 1_000_000
    then (
      tool.payload <- utf8_suffix tool.payload 1_000_000;
      refresh_retained_bytes call;
      call.is_truncated <- true;
      tool.revision <- tool.revision + 1);
    call.entries <- Tool tool :: rest
;;

let total_agent_bytes agent =
  Hashtbl.fold agent.Agent_page_state.calls ~init:0 ~f:(fun ~key:_ ~data acc ->
    acc + data.Agent_page_state.retained_bytes)
;;

let enforce_global_limit agent =
  let rec trim () =
    if total_agent_bytes agent <= 16_000_000
    then ()
    else (
      match
        List.find_map agent.Agent_page_state.call_order ~f:(fun call_id ->
          Option.bind (Hashtbl.find agent.calls call_id) ~f:(fun call ->
            if drop_oldest_entry call then Some () else None))
      with
      | None -> ()
      | Some () -> trim ())
  in
  trim ()
;;

let agent_call_started t ~call_id ~name ~kind ~payload ~agent_page_kind =
  let agent = agent_page t in
  if Hash_set.mem agent.terminal_call_ids call_id
  then false
  else if Hashtbl.mem agent.calls call_id
  then true
  else (
    let call : Agent_page_state.call =
      { call_id
      ; name
      ; kind
      ; payload
      ; agent_page_kind
      ; start_order = agent.next_start_order
      ; entries = []
      ; retained_bytes = 0
      ; is_truncated = false
      ; outcome = None
      ; output = None
      ; next_render_id = 0
      ; render_cache = Hashtbl.create (module Int)
      ; render_width = None
      ; render_block_ids = [||]
      ; render_block_revisions = [||]
      ; render_geometry = Renderer_virtual_list.Geometry.create ()
      }
    in
    agent.next_start_order <- agent.next_start_order + 1;
    Hashtbl.set agent.calls ~key:call_id ~data:call;
    agent.call_order <- agent.call_order @ [ call_id ];
    if Option.is_none agent.selected_call_id then agent.selected_call_id <- Some call_id;
    true)
;;

let replace_latest_text entries ~channel ~text =
  let rec loop acc = function
    | [] -> None
    | entry :: rest ->
      (match loop (entry :: acc) rest with
       | Some _ as result -> result
       | None ->
         (match entry with
          | Agent_page_state.Text entry
            when entry.replaceable && Poly.(entry.channel = channel) ->
            let entry =
              Agent_page_state.Text
                { entry with text; replaceable = true; revision = entry.revision + 1 }
            in
            Some (List.rev_append acc (entry :: rest))
          | Text _ | Tool _ -> None))
  in
  loop [] entries
;;

let add_text_progress
      (call : Agent_page_state.call)
      (progress : Ochat_function.Progress.t)
  =
  let channel = progress.channel in
  let fresh_entry ~text ~replaceable =
    let render_id = call.next_render_id in
    call.next_render_id <- render_id + 1;
    Agent_page_state.Text { render_id; revision = 0; channel; text; replaceable }
  in
  let entries =
    match progress.update with
    | Append text ->
      (match List.rev call.entries with
       | Agent_page_state.Text last :: rest when Poly.(last.channel = channel) ->
         List.rev
           (Agent_page_state.Text
              { last with text = last.text ^ text; revision = last.revision + 1 }
            :: rest)
       | _ -> call.entries @ [ fresh_entry ~text ~replaceable:false ])
    | Replace text ->
      Option.value
        (replace_latest_text call.entries ~channel ~text)
        ~default:(call.entries @ [ fresh_entry ~text ~replaceable:true ])
  in
  call.entries <- entries;
  refresh_retained_bytes call;
  enforce_call_limit call
;;

let agent_call_progress t ~call_id progress =
  let agent = agent_page t in
  match Hashtbl.find agent.calls call_id with
  | None -> false
  | Some call when Option.is_some call.outcome -> false
  | Some call ->
    add_text_progress call progress;
    enforce_global_limit agent;
    true
;;

let find_nested_call (call : Agent_page_state.call) call_id =
  List.find_map call.entries ~f:(function
    | Agent_page_state.Text _ -> None
    | Tool nested when String.equal nested.call_id call_id -> Some nested
    | Tool _ -> None)
;;

let replace_nested_progress entries ~channel ~text =
  let rec loop acc = function
    | [] -> None
    | entry :: rest ->
      (match loop (entry :: acc) rest with
       | Some _ as result -> result
       | None when entry.Agent_page_state.replaceable && Poly.(entry.channel = channel) ->
         Some
           (List.rev_append
              acc
              (Agent_page_state.
                 { entry with text; replaceable = true; revision = entry.revision + 1 }
               :: rest))
       | None -> None)
  in
  loop [] entries
;;

let add_nested_progress
      (nested : Agent_page_state.nested_call)
      (progress : Ochat_function.Progress.t)
  =
  let channel = progress.channel in
  let fresh_entry ~text ~replaceable =
    Agent_page_state.{ render_id = 0; revision = 0; channel; text; replaceable }
  in
  nested.Agent_page_state.progress
  <- (match progress.update with
      | Append text ->
        (match List.rev nested.progress with
         | last :: rest when Poly.(last.channel = channel) ->
           List.rev
             (Agent_page_state.
                { last with text = last.text ^ text; revision = last.revision + 1 }
              :: rest)
         | _ -> nested.progress @ [ fresh_entry ~text ~replaceable:false ])
      | Replace text ->
        Option.value
          (replace_nested_progress nested.progress ~channel ~text)
          ~default:(nested.progress @ [ fresh_entry ~text ~replaceable:true ]))
;;

let agent_call_trace t ~call_id trace =
  let agent = agent_page t in
  match Hashtbl.find agent.calls call_id with
  | None -> false
  | Some call when Option.is_some call.outcome -> false
  | Some call ->
    let accepted =
      match trace with
      | Ochat_function.Trace.Tool_started { call_id; name; kind; payload } ->
        if Option.is_some (find_nested_call call call_id)
        then true
        else (
          call.entries
          <- call.entries
             @ [ Agent_page_state.Tool
                   { render_id = call.next_render_id
                   ; revision = 0
                   ; call_id
                   ; name
                   ; kind
                   ; payload
                   ; progress = []
                   ; outcome = None
                   ; output = None
                   }
               ];
          call.next_render_id <- call.next_render_id + 1;
          true)
      | Tool_progress { call_id; progress } ->
        (match find_nested_call call call_id with
         | None -> false
         | Some nested ->
           add_nested_progress nested progress;
           nested.revision <- nested.revision + 1;
           true)
      | Tool_finished { call_id; outcome; output } ->
        (match find_nested_call call call_id with
         | None -> false
         | Some nested ->
           nested.outcome <- Some outcome;
           nested.output <- output;
           nested.revision <- nested.revision + 1;
           true)
    in
    if accepted
    then (
      refresh_retained_bytes call;
      enforce_call_limit call;
      enforce_global_limit agent);
    accepted
;;

let agent_call_finished t ~call_id ~outcome ~output =
  let agent = agent_page t in
  match Hashtbl.find agent.calls call_id with
  | None -> false
  | Some call when Option.is_some call.outcome -> false
  | Some call ->
    call.outcome <- Some outcome;
    call.output <- output;
    Hash_set.add agent.terminal_call_ids call_id;
    true
;;

let clear_agent_calls t =
  let agent = agent_page t in
  Hashtbl.clear agent.calls;
  Hash_set.clear agent.terminal_call_ids;
  agent.call_order <- [];
  agent.selected_call_id <- None;
  agent.auto_follow <- true;
  agent.next_start_order <- 0;
  Notty_scroll_box.set_content agent.scroll_box Notty.I.empty;
  Notty_scroll_box.scroll_to_top agent.scroll_box;
  Hashtbl.iter_keys t.tool_call_id_by_id ~f:(fun id ->
    invalidate_render_metadata_by_id t ~id);
  Hashtbl.clear t.tool_call_id_by_id;
  Hashtbl.clear t.tool_call_outcome_by_call_id;
  t.active_page <- Page_id.Chat
;;

(* ------------------------------------------------------------------------- *)
(*  Command-mode helpers                                                     *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(*  Rendering cache helpers                                                   *)
(* ------------------------------------------------------------------------- *)

let active_history_width t = (chat_page t).active_history_width
let set_active_history_width t v = (chat_page t).active_history_width <- v

let width_preparation_is_current t preparation =
  not Poly.(preparation.Chat_page_state.status = Chat_page_state.Cancelled)
;;

let width_preparation t =
  let chat = chat_page t in
  match chat.preparing_width with
  | Some preparation when width_preparation_is_current t preparation -> Some preparation
  | None -> None
  | Some _ ->
    chat.preparing_width <- None;
    None
;;

let start_width_preparation
      t
      ~request_generation
      ~terminal_size
      ~layout
      ~theme_generation
      ~grammar_generation
      ~anchor
  =
  let chat = chat_page t in
  Option.iter chat.preparing_width ~f:(fun preparation ->
    preparation.status <- Chat_page_state.Cancelled);
  chat.preparing_width
  <- Some
       { request_generation
       ; terminal_size
       ; layout
       ; transcript_generation = t.transcript_generation
       ; render_generation = t.render_generation
       ; theme_generation
       ; grammar_generation
       ; anchor
       ; active_geometry = Renderer_virtual_list.Geometry.snapshot chat.geometry
       ; active_rows =
           Array.filter_mapi t.render_row_ids ~f:(fun index id ->
             Option.map (render_row_identity t ~idx:index) ~f:(fun (_, revision) ->
               id, revision))
       ; scroll_direction = chat.scroll_direction
       ; target_rows = Hashtbl.create (module Projected_message.Id)
       ; exact_rows = Hash_set.create (module Projected_message.Id)
       ; prepared_batches = Hash_set.create (module Int)
       ; prepared_chunks = Hash_set.create (module Int)
       ; partial_chunks = Hashtbl.create (module Int)
       ; desired_corridor = None
       ; published_corridor = None
       ; destination = None
       ; status = Chat_page_state.Preparing
       }
;;

let width_preparation_request_generation preparation =
  preparation.Chat_page_state.request_generation
;;

let width_preparation_target_width preparation =
  fst preparation.Chat_page_state.terminal_size
;;

let width_preparation_terminal_size preparation =
  preparation.Chat_page_state.terminal_size
;;

let width_preparation_layout preparation = preparation.Chat_page_state.layout

let width_preparation_generations preparation =
  preparation.Chat_page_state.transcript_generation, preparation.render_generation
;;

let width_preparation_highlight_generations preparation =
  preparation.Chat_page_state.theme_generation, preparation.grammar_generation
;;

let width_preparation_status preparation = preparation.Chat_page_state.status

let width_preparation_active_geometry preparation =
  preparation.Chat_page_state.active_geometry
;;

let width_preparation_scroll_direction (preparation : Chat_page_state.preparing_width) =
  preparation.Chat_page_state.scroll_direction
;;

let width_preparation_anchor preparation = preparation.Chat_page_state.anchor

let width_preparation_viewport_intent t preparation =
  let geometry = preparation.Chat_page_state.active_geometry in
  match preparation.Chat_page_state.anchor with
  | Resize_anchor.Follow_bottom -> 0, true
  | Manual_empty -> 0, false
  | Preserve_row { key; anchor; neighbors } ->
    let resolved =
      match current_resize_anchor_index t key with
      | Some index -> Some (Renderer_virtual_list.Anchor.remap_index anchor ~index)
      | None ->
        List.find_map neighbors ~f:(fun neighbor ->
          current_resize_anchor_index t neighbor.key
          |> Option.map ~f:(fun index ->
            match neighbor.side with
            | `Older -> Renderer_virtual_list.Anchor.at_end ~index ~offset:0 ~screen_row:0
            | `Newer ->
              Renderer_virtual_list.Anchor.at_start ~index ~offset:0 ~screen_row:0))
    in
    let requested_scroll =
      Option.bind resolved ~f:(fun anchor ->
        Renderer_virtual_list.Anchor.corrected_scroll_snapshot anchor ~geometry)
      |> Option.value ~default:0
    in
    requested_scroll, false
;;

let width_preparation_row_count preparation =
  Hashtbl.length preparation.Chat_page_state.target_rows
;;

let width_preparation_is_exact t preparation =
  let row_count = Array.length t.render_row_ids in
  Hashtbl.length preparation.Chat_page_state.target_rows >= row_count
  && Array.for_alli t.render_row_ids ~f:(fun index id ->
    let role, text = t.message_array.(index) in
    let _, revision = render_row_identity t ~idx:index |> Option.value_exn in
    Hashtbl.find preparation.target_rows id
    |> Option.exists ~f:(fun entry ->
      Int.equal entry.width (fst preparation.terminal_size)
      && Int.equal entry.row_revision revision
      && String.equal entry.role role
      && String.equal entry.text text))
;;

let width_preparation_exact_row_count t preparation =
  Array.counti t.render_row_ids ~f:(fun index id ->
    let _, revision = render_row_identity t ~idx:index |> Option.value_exn in
    Hashtbl.find preparation.Chat_page_state.target_rows id
    |> Option.exists ~f:(fun entry ->
      Int.equal entry.row_revision revision
      && Int.equal entry.width (fst preparation.terminal_size)))
;;

let find_width_preparation_row t ~request_generation ~id =
  Option.bind (width_preparation t) ~f:(fun preparation ->
    if Int.equal preparation.request_generation request_generation
    then (
      match Hashtbl.find preparation.target_rows id with
      | None -> None
      | Some entry ->
        (match render_index_by_id t ~id with
         | None ->
           remove_width_preparation_row_from preparation ~id;
           None
         | Some index ->
           let role, text = t.message_array.(index) in
           let _, revision = render_row_identity t ~idx:index |> Option.value_exn in
           if
             Int.equal entry.width (fst preparation.terminal_size)
             && Int.equal entry.row_revision revision
             && String.equal entry.role role
             && String.equal entry.text text
           then Some entry
           else (
             remove_width_preparation_row_from preparation ~id;
             None)))
    else None)
;;

let set_width_preparation_row t ~request_generation ~id entry =
  match width_preparation t with
  | Some preparation
    when Int.equal preparation.request_generation request_generation
         && Poly.(preparation.status = Chat_page_state.Preparing) ->
    (match render_index_by_id t ~id with
     | None -> false
     | Some index ->
       let role, text = t.message_array.(index) in
       let _, revision = render_row_identity t ~idx:index |> Option.value_exn in
       if
         Int.equal entry.width (fst preparation.terminal_size)
         && Int.equal entry.row_revision revision
         && String.equal entry.role role
         && String.equal entry.text text
       then (
         Hashtbl.set preparation.target_rows ~key:id ~data:entry;
         Hash_set.add preparation.exact_rows id;
         true)
       else false)
  | None | Some _ -> false
;;

let mark_width_preparation_complete t ~request_generation =
  match width_preparation t with
  | Some preparation
    when Int.equal preparation.request_generation request_generation
         && Poly.(preparation.status = Chat_page_state.Preparing) ->
    preparation.status <- Chat_page_state.Complete;
    true
  | None | Some _ -> false
;;

let with_current_width_preparation t ~request_generation ~f =
  match width_preparation t with
  | Some preparation
    when Int.equal preparation.request_generation request_generation
         && Poly.(preparation.status = Chat_page_state.Preparing) ->
    f preparation;
    true
  | None | Some _ -> false
;;

let mark_width_preparation_batch t ~request_generation ~batch_index =
  let batch_count =
    History_chunk.foreground_batch_count ~row_count:(Array.length t.render_row_ids)
  in
  if batch_index < 0 || batch_index >= batch_count
  then false
  else
    with_current_width_preparation t ~request_generation ~f:(fun preparation ->
      Hash_set.add preparation.prepared_batches batch_index)
;;

let mark_width_preparation_chunk t ~request_generation ~chunk_index =
  let chunk_count =
    History_chunk.canonical_count ~row_count:(Array.length t.render_row_ids)
  in
  if chunk_index < 0 || chunk_index >= chunk_count
  then false
  else
    with_current_width_preparation t ~request_generation ~f:(fun preparation ->
      Hash_set.add preparation.prepared_chunks chunk_index)
;;

let set_width_preparation_partial_chunk t ~request_generation ~chunk_index chunk =
  let chunk_count =
    History_chunk.canonical_count ~row_count:(Array.length t.render_row_ids)
  in
  if chunk_index < 0 || chunk_index >= chunk_count
  then false
  else
    with_current_width_preparation t ~request_generation ~f:(fun preparation ->
      Hashtbl.set preparation.partial_chunks ~key:chunk_index ~data:chunk)
;;

let set_width_preparation_corridors t ~request_generation ~desired ~published =
  with_current_width_preparation t ~request_generation ~f:(fun preparation ->
    preparation.desired_corridor <- desired;
    preparation.published_corridor <- published)
;;

let width_preparation_batch_is_ready t ~request_generation ~batch_index =
  Option.exists (width_preparation t) ~f:(fun preparation ->
    Int.equal preparation.request_generation request_generation
    && Hash_set.mem preparation.prepared_batches batch_index)
;;

let width_preparation_chunk_is_ready t ~request_generation ~chunk_index =
  Option.exists (width_preparation t) ~f:(fun preparation ->
    Int.equal preparation.request_generation request_generation
    && Hash_set.mem preparation.prepared_chunks chunk_index)
;;

let width_preparation_corridors preparation =
  preparation.Chat_page_state.desired_corridor, preparation.published_corridor
;;

let width_preparation_destination preparation = preparation.Chat_page_state.destination

let width_preparation_destination_is_current t preparation =
  match preparation.Chat_page_state.destination with
  | None -> true
  | Some destination ->
    Option.exists (render_index_by_id t ~id:destination.id) ~f:(fun index ->
      Option.exists (message_revision t ~idx:index) ~f:(fun revision ->
        Int.equal revision destination.revision))
;;

let destination_intent t preparation =
  match preparation.Chat_page_state.destination with
  | None -> None
  | Some destination ->
    let resolved =
      match render_index_by_id t ~id:destination.id with
      | Some index ->
        let _, revision = render_row_identity t ~idx:index |> Option.value_exn in
        if Int.equal revision destination.revision
        then Some (destination, index)
        else (
          match destination.reason with
          | Chat_page_state.Destination.Search_result -> None
          | Earlier_conversation | Latest_conversation ->
            let destination = { destination with revision } in
            preparation.destination <- Some destination;
            Some (destination, index))
      | None ->
        let length = Array.length t.render_row_ids in
        let index =
          match destination.reason with
          | Chat_page_state.Destination.Earlier_conversation ->
            if Int.equal length 0 then None else Some 0
          | Latest_conversation -> if Int.equal length 0 then None else Some (length - 1)
          | Search_result -> None
        in
        Option.bind index ~f:(fun index ->
          Option.map (render_row_identity t ~idx:index) ~f:(fun (id, revision) ->
            let destination = { destination with id; revision } in
            preparation.destination <- Some destination;
            destination, index))
    in
    (match resolved with
     | None -> None
     | Some index ->
       let destination, index = index in
       let _, revision = render_row_identity t ~idx:index |> Option.value_exn in
       if not (Int.equal revision destination.revision)
       then None
       else (
         let geometry = Renderer_virtual_list.Geometry.snapshot (chat_page t).geometry in
         let prefix = Renderer_virtual_list.Geometry.Snapshot.prefix geometry in
         let viewport_height = preparation.layout.scroll_height in
         let requested_scroll =
           match destination.placement with
           | Chat_page_state.Destination.Top -> prefix.(index)
           | Center -> prefix.(index) - (viewport_height / 2)
           | Bottom -> prefix.(index + 1) - viewport_height
         in
         Some
           ( Int.max 0 requested_scroll
           , Poly.(destination.reason = Chat_page_state.Destination.Latest_conversation)
           )))
;;

let set_width_preparation_destination t ~request_generation destination =
  with_current_width_preparation t ~request_generation ~f:(fun preparation ->
    preparation.destination <- destination)
;;

let promote_width_preparation_rows t ~request_generation =
  match width_preparation t with
  | Some preparation
    when Int.equal preparation.request_generation request_generation
         && width_preparation_is_exact t preparation ->
    let chat = chat_page t in
    Hashtbl.clear chat.msg_img_cache;
    Hashtbl.iteri preparation.target_rows ~f:(fun ~key ~data ->
      Hashtbl.set chat.msg_img_cache ~key ~data);
    let heights =
      Array.mapi t.render_row_ids ~f:(fun index id ->
        let _, revision = render_row_identity t ~idx:index |> Option.value_exn in
        Hashtbl.find preparation.target_rows id
        |> Option.filter ~f:(fun entry -> Int.equal entry.row_revision revision)
        |> Option.value_exn
        |> fun entry -> entry.height)
    in
    let prefix = Array.create ~len:(Array.length heights + 1) 0 in
    Array.iteri heights ~f:(fun index height ->
      if height < 0 || prefix.(index) > Int.max_value - height
      then invalid_arg "Model.promote_width_preparation_rows: invalid height";
      prefix.(index + 1) <- prefix.(index) + height);
    Renderer_virtual_list.Geometry.replace chat.geometry ~heights ~prefix;
    chat.active_history_width <- Some (fst preparation.terminal_size);
    preparation.status <- Chat_page_state.Complete;
    true
  | None | Some _ -> false
;;

let finish_width_preparation_promotion t ~request_generation =
  let chat = chat_page t in
  match chat.preparing_width with
  | Some preparation
    when Int.equal preparation.request_generation request_generation
         && Poly.(preparation.status = Chat_page_state.Complete)
         && Renderer_virtual_list.Geometry.all_exact chat.geometry
         && Option.exists chat.history_image_cache ~f:(fun cache ->
           Int.equal cache.width (fst preparation.terminal_size)
           && Int.equal cache.transcript_generation t.transcript_generation
           && Int.equal cache.render_generation t.render_generation) ->
    chat.corridor_history_cache <- None;
    chat.preparing_width <- None;
    chat.materialization <- Chat_page_state.Warm;
    let history_image_cache = Option.value_exn chat.history_image_cache in
    let width = Option.value_exn chat.active_history_width in
    let snapshot =
      Chat_page_state.
        { width
        ; transcript_generation = t.transcript_generation
        ; render_generation = t.render_generation
        ; entries = Hashtbl.to_alist chat.msg_img_cache
        ; heights = Array.copy (Renderer_virtual_list.Geometry.heights chat.geometry)
        ; prefix = Array.copy (Renderer_virtual_list.Geometry.prefix chat.geometry)
        ; history_image_cache
        }
    in
    chat.width_snapshots
    <- snapshot
       :: List.filter chat.width_snapshots ~f:(fun cached ->
         not (Int.equal cached.width width))
       |> Fn.flip List.take recent_width_snapshot_capacity;
    Live_scroll_trace.emit
      ~phase:"resize_full_completion_ready"
      [ "request_generation", `Number (Int.to_string request_generation)
      ; "width", `Number (Int.to_string width)
      ; "rows", `Number (Int.to_string (Array.length t.render_row_ids))
      ];
    true
  | None | Some _ -> false
;;

let publish_width_preparation_corridor t ~request_generation =
  let chat = chat_page t in
  match width_preparation t with
  | None -> false
  | Some preparation ->
    (match preparation.desired_corridor with
     | None -> false
     | Some desired ->
       let row_count = Array.length t.render_row_ids in
       let desired = History_chunk.Range.clamp desired ~row_count in
       let target_width = fst preparation.terminal_size in
       let current_entry index =
         let id, revision = render_row_identity t ~idx:index |> Option.value_exn in
         Hashtbl.find preparation.target_rows id
         |> Option.filter ~f:(fun entry ->
           let role, text = t.message_array.(index) in
           Int.equal entry.width target_width
           && Int.equal entry.row_revision revision
           && String.equal entry.role role
           && String.equal entry.text text)
         |> Option.map ~f:(fun entry -> id, entry)
       in
       let captured_heights =
         Renderer_virtual_list.Geometry.Snapshot.heights preparation.active_geometry
       in
       let captured_by_id = Hashtbl.create (module Projected_message.Id) in
       Array.iteri preparation.active_rows ~f:(fun index (id, revision) ->
         if index < Array.length captured_heights
         then Hashtbl.set captured_by_id ~key:id ~data:(revision, captured_heights.(index)));
       let entries = Array.init row_count ~f:current_entry in
       let heights =
         Array.init row_count ~f:(fun index ->
           match entries.(index) with
           | Some (_, entry) -> entry.height
           | None ->
             let id, revision = render_row_identity t ~idx:index |> Option.value_exn in
             Hashtbl.find captured_by_id id
             |> Option.bind ~f:(fun (captured_revision, height) ->
               if Int.equal captured_revision revision then Some height else None)
             |> Option.value ~default:5)
       in
       let exact = Array.map entries ~f:Option.is_some in
       let prefix = Array.create ~len:(row_count + 1) 0 in
       Array.iteri heights ~f:(fun index height ->
         if height < 0 || prefix.(index) > Int.max_value - height
         then invalid_arg "Model.publish_width_preparation_corridor: invalid height";
         prefix.(index + 1) <- prefix.(index) + height);
       let temporary_geometry = Renderer_virtual_list.Geometry.create () in
       Renderer_virtual_list.Geometry.replace_partial
         temporary_geometry
         ~heights:(Array.copy heights)
         ~prefix:(Array.copy prefix)
         ~exact:(Array.copy exact);
       let follow_current_bottom =
         t.auto_follow && Option.is_none preparation.destination
       in
       let requested_scroll, follow_bottom =
         match destination_intent t preparation with
         | Some intent -> intent
         | None ->
           if follow_current_bottom
           then 0, true
           else (
             match preparation.anchor with
             | Resize_anchor.Follow_bottom -> 0, true
             | Manual_empty -> 0, false
             | Preserve_row { key; anchor; neighbors } ->
               let resolve key anchor =
                 current_resize_anchor_index t key
                 |> Option.bind ~f:(fun index ->
                   Renderer_virtual_list.Anchor.corrected_scroll
                     (Renderer_virtual_list.Anchor.remap_index anchor ~index)
                     ~geometry:temporary_geometry)
               in
               let scroll =
                 Option.first_some
                   (resolve key anchor)
                   (List.find_map neighbors ~f:(fun neighbor ->
                      current_resize_anchor_index t neighbor.key
                      |> Option.bind ~f:(fun index ->
                        let anchor =
                          match neighbor.side with
                          | `Older ->
                            Renderer_virtual_list.Anchor.at_end
                              ~index
                              ~offset:0
                              ~screen_row:0
                          | `Newer ->
                            Renderer_virtual_list.Anchor.at_start
                              ~index
                              ~offset:0
                              ~screen_row:0
                        in
                        Renderer_virtual_list.Anchor.corrected_scroll
                          anchor
                          ~geometry:temporary_geometry)))
                 |> Option.value ~default:0
               in
               scroll, false)
       in
       let viewport =
         Renderer_virtual_list.Viewport.compute
           ~geometry:temporary_geometry
           ~requested_scroll
           ~height:preparation.layout.scroll_height
           ~follow_bottom
       in
       let visible_indices = Renderer_virtual_list.Viewport.visible_indices viewport in
       let visible_is_publishable =
         (not (List.is_empty visible_indices))
         && width_preparation_destination_is_current t preparation
         && List.for_all visible_indices ~f:(fun index ->
           index >= desired.first && index < desired.past && exact.(index))
       in
       if
         not
           (Int.equal preparation.request_generation request_generation
            && Poly.(preparation.status = Chat_page_state.Preparing)
            && visible_is_publishable)
       then false
       else (
         let first_publication = Poly.(chat.materialization = Chat_page_state.Resizing) in
         let visible_first = List.hd_exn visible_indices in
         let visible_last = List.last_exn visible_indices in
         let rec extend_older index =
           if index > desired.first && exact.(index - 1)
           then extend_older (index - 1)
           else index
         in
         let rec extend_newer index =
           if index < desired.past && exact.(index)
           then extend_newer (index + 1)
           else index
         in
         let published =
           History_chunk.Range.create_exn
             ~first:(extend_older visible_first)
             ~past:(extend_newer (visible_last + 1))
         in
         let corridor_entries =
           List.init (History_chunk.Range.length published) ~f:(fun offset ->
             entries.(published.first + offset) |> Option.value_exn)
         in
         let corridor_image =
           corridor_entries |> List.map ~f:(fun (_, entry) -> entry.image) |> Notty.I.vcat
         in
         let image =
           Notty.I.vcat
             [ Notty.I.void target_width prefix.(published.first)
             ; corridor_image
             ; Notty.I.void target_width (prefix.(row_count) - prefix.(published.past))
             ]
         in
         Hashtbl.clear chat.msg_img_cache;
         Array.iter entries ~f:(function
           | None -> ()
           | Some (id, entry) -> Hashtbl.set chat.msg_img_cache ~key:id ~data:entry);
         Renderer_virtual_list.Geometry.replace_partial
           chat.geometry
           ~heights
           ~prefix
           ~exact;
         chat.active_history_width <- Some target_width;
         chat.history_image_cache <- None;
         chat.corridor_history_cache
         <- Some { width = target_width; request_generation; rows = published; image };
         Notty_scroll_box.set_content chat.scroll_box image;
         (match preparation.destination with
          | Some destination ->
            if Poly.(destination.reason = Chat_page_state.Destination.Latest_conversation)
            then follow_chat_bottom t ~viewport_height:preparation.layout.scroll_height
            else (
              ignore
                (reveal_prepared_row
                   t
                   ~viewport_height:preparation.layout.scroll_height
                   ~id:destination.id
                   ~placement:destination.placement
                 : bool);
              t.projected.reveal_id <- None)
          | None ->
            if follow_current_bottom
            then follow_chat_bottom t ~viewport_height:preparation.layout.scroll_height
            else
              ignore
                (restore_resize_anchor
                   t
                   ~viewport_height:preparation.layout.scroll_height
                   preparation.anchor
                 : Resize_anchor.resolution));
         preparation.destination <- None;
         preparation.published_corridor <- Some published;
         chat.materialization <- Chat_page_state.Corridor;
         if first_publication
         then
           Live_scroll_trace.emit
             ~phase:"resize_first_corridor_ready"
             [ "request_generation", `Number (Int.to_string request_generation)
             ; "width", `Number (Int.to_string target_width)
             ; "first", `Number (Int.to_string published.first)
             ; "past", `Number (Int.to_string published.past)
             ; "rows", `Number (Int.to_string (History_chunk.Range.length published))
             ];
         true))
;;

let clear_width_preparation t ~request_generation =
  let chat = chat_page t in
  match width_preparation t with
  | Some preparation when Int.equal preparation.request_generation request_generation ->
    chat.preparing_width <- None;
    Some preparation
  | None | Some _ -> None
;;

let cancel_width_preparation t ~request_generation =
  let chat = chat_page t in
  match width_preparation t with
  | Some preparation when Int.equal preparation.request_generation request_generation ->
    preparation.status <- Chat_page_state.Cancelled;
    chat.preparing_width <- None;
    true
  | None | Some _ -> false
;;

let clear_all_img_caches t =
  let chat = chat_page t in
  Live_scroll_trace.emit
    ~phase:"geometry_invalidate"
    [ "kind", `String "clear_all"
    ; ( "generation_before"
      , `Number (Int.to_string (Renderer_virtual_list.Geometry.generation chat.geometry))
      )
    ; ( "length"
      , `Number (Int.to_string (Renderer_virtual_list.Geometry.length chat.geometry)) )
    ; ( "total_height"
      , `Number
          (Int.to_string (Renderer_virtual_list.Geometry.total_height chat.geometry)) )
    ];
  Hashtbl.clear chat.msg_img_cache;
  Renderer_virtual_list.Geometry.clear chat.geometry;
  chat.dirty_height_rows <- [];
  chat.history_image_cache <- None
;;

let clear_img_caches_preserving_heights t =
  let chat = chat_page t in
  Live_scroll_trace.emit
    ~phase:"geometry_invalidate"
    [ "kind", `String "preserve_heights"
    ; ( "generation_before"
      , `Number (Int.to_string (Renderer_virtual_list.Geometry.generation chat.geometry))
      )
    ; ( "length"
      , `Number (Int.to_string (Renderer_virtual_list.Geometry.length chat.geometry)) )
    ; ( "total_height"
      , `Number
          (Int.to_string (Renderer_virtual_list.Geometry.total_height chat.geometry)) )
    ];
  Hashtbl.clear chat.msg_img_cache;
  Renderer_virtual_list.Geometry.mark_all_estimated chat.geometry;
  chat.dirty_height_rows <- [];
  chat.history_image_cache <- None
;;

let invalidate_img_cache_index t ~idx =
  Option.iter (render_row_identity t ~idx) ~f:(fun (id, revision) ->
    let chat = chat_page t in
    Hashtbl.remove chat.msg_img_cache id;
    chat.dirty_height_rows <- (id, revision) :: chat.dirty_height_rows)
;;

let find_img_cache t ~id ~revision =
  Hashtbl.find (chat_page t).msg_img_cache id
  |> Option.filter ~f:(fun entry -> Int.equal entry.row_revision revision)
;;

let set_img_cache t ~id entry =
  Hashtbl.set (chat_page t).msg_img_cache ~key:id ~data:entry
;;

let cached_width_entries t =
  let chat = chat_page t in
  Hashtbl.to_alist chat.msg_img_cache
  @ List.concat_map chat.width_snapshots ~f:(fun snapshot -> snapshot.entries)
;;

let cached_entry_is_current t ~id (entry : msg_img_cache) =
  match render_index_by_id t ~id with
  | None -> false
  | Some index ->
    let role, text = t.message_array.(index) in
    let _, revision = render_row_identity t ~idx:index |> Option.value_exn in
    Int.equal revision entry.row_revision
    && String.equal role entry.role
    && String.equal text entry.text
;;

let find_cached_width_row t ~width ~id ~revision =
  cached_width_entries t
  |> List.find_map ~f:(fun (candidate_id, (entry : msg_img_cache)) ->
    if
      Projected_message.Id.equal candidate_id id
      && Int.equal entry.row_revision revision
      && Int.equal entry.width width
      && cached_entry_is_current t ~id entry
    then Some entry
    else None)
;;

let find_compatible_layout_row t ~width ~id ~revision =
  cached_width_entries t
  |> List.find_map ~f:(fun (candidate_id, (entry : msg_img_cache)) ->
    if
      Projected_message.Id.equal candidate_id id
      && Int.equal entry.row_revision revision
      && cached_entry_is_current t ~id entry
      && Chat_message_render_job.Layout_plan.allows entry.layout_plan ~width
    then Some entry
    else None)
;;

let find_semantic_cache t ~id ~revision ~role ~text ~tool_output =
  Hashtbl.find (chat_page t).msg_semantic_cache id
  |> Option.filter ~f:(fun entry ->
    Int.equal entry.row_revision revision
    && String.equal entry.role role
    && String.equal entry.text text
    && Poly.equal entry.tool_output tool_output)
;;

let set_chat_materialization_loading t =
  (chat_page t).materialization <- Chat_page_state.Loading;
  t.normal_input_enabled <- false
;;

let set_chat_materialization_resizing t =
  (chat_page t).materialization <- Chat_page_state.Resizing;
  t.normal_input_enabled <- false
;;

let set_chat_materialization_corridor t =
  (chat_page t).materialization <- Chat_page_state.Corridor
;;

let set_chat_materialization_warm t =
  (chat_page t).materialization <- Chat_page_state.Warm
;;

let chat_materialization t = (chat_page t).materialization
let normal_input_is_enabled t = t.normal_input_enabled
let set_normal_input_enabled t enabled = t.normal_input_enabled <- enabled
let history_image_cache t = (chat_page t).history_image_cache
let corridor_history_cache t = (chat_page t).corridor_history_cache
let set_history_image_cache t cache = (chat_page t).history_image_cache <- cache
let clear_corridor_history_cache t = (chat_page t).corridor_history_cache <- None

let set_corridor_history_image t image =
  Option.iter (chat_page t).corridor_history_cache ~f:(fun cache -> cache.image <- image)
;;

let mark_history_chunk_index_dirty t index =
  if index >= 0 then Hash_set.add (chat_page t).dirty_history_chunks index
;;

let mark_history_row_dirty t ~id =
  Option.iter (Hashtbl.find (chat_page t).row_chunk_by_id id) ~f:(fun index ->
    mark_history_chunk_index_dirty t index)
;;

let mark_all_history_chunks_dirty t =
  let chat = chat_page t in
  let count = History_chunk.canonical_count ~row_count:(Array.length t.message_array) in
  List.iter (List.init count ~f:Fn.id) ~f:(Hash_set.add chat.dirty_history_chunks)
;;

let take_dirty_history_chunks t =
  let dirty =
    Hash_set.to_list (chat_page t).dirty_history_chunks |> List.sort ~compare:Int.compare
  in
  Hash_set.clear (chat_page t).dirty_history_chunks;
  dirty
;;

let has_dirty_history_chunks t =
  not (Hash_set.is_empty (chat_page t).dirty_history_chunks)
;;

let defer_dirty_history_chunks t chunks =
  List.iter chunks ~f:(Hash_set.add (chat_page t).dirty_history_chunks)
;;

let rebuild_row_chunk_index t =
  let chat = chat_page t in
  Hashtbl.clear chat.row_chunk_by_id;
  Array.iteri t.render_row_ids ~f:(fun index id ->
    Hashtbl.set
      chat.row_chunk_by_id
      ~key:id
      ~data:(History_chunk.canonical_index_of_row_exn ~row_index:index))
;;

let row_viewport_relation t ~viewport_height ~id =
  match render_index_by_id t ~id with
  | None -> Unknown
  | Some index -> classify_live_row t ~viewport_height ~index
;;

let buffer_row_id t id =
  Hashtbl.find t.msg_buffers id |> Option.map ~f:(fun buffer -> buffer.row_id)
;;

let remember_current_width t =
  let chat = chat_page t in
  match chat.active_history_width, chat.history_image_cache with
  | Some width, Some history_image_cache
    when Int.equal history_image_cache.transcript_generation t.transcript_generation
         && Int.equal history_image_cache.render_generation t.render_generation
         && Renderer_virtual_list.Geometry.all_exact chat.geometry ->
    let snapshot =
      Chat_page_state.
        { width
        ; transcript_generation = t.transcript_generation
        ; render_generation = t.render_generation
        ; entries = Hashtbl.to_alist chat.msg_img_cache
        ; heights = Array.copy (Renderer_virtual_list.Geometry.heights chat.geometry)
        ; prefix = Array.copy (Renderer_virtual_list.Geometry.prefix chat.geometry)
        ; history_image_cache
        }
    in
    chat.width_snapshots
    <- snapshot
       :: List.filter chat.width_snapshots ~f:(fun cached ->
         not (Int.equal cached.width width))
       |> Fn.flip List.take recent_width_snapshot_capacity
  | None, _ | Some _, None | Some _, Some _ -> ()
;;

let restore_width t ~width =
  let chat = chat_page t in
  let matching, remaining =
    List.partition_tf chat.width_snapshots ~f:(fun snapshot ->
      Int.equal snapshot.width width
      && Int.equal snapshot.transcript_generation t.transcript_generation
      && Int.equal snapshot.render_generation t.render_generation)
  in
  match matching with
  | [] -> false
  | snapshot :: _ ->
    Hashtbl.clear chat.msg_img_cache;
    List.iter snapshot.entries ~f:(fun (id, entry) ->
      Hashtbl.set chat.msg_img_cache ~key:id ~data:entry);
    Renderer_virtual_list.Geometry.replace
      chat.geometry
      ~heights:(Array.copy snapshot.heights)
      ~prefix:(Array.copy snapshot.prefix);
    chat.history_image_cache <- Some snapshot.history_image_cache;
    chat.active_history_width <- Some width;
    chat.dirty_height_rows <- [];
    chat.width_snapshots
    <- snapshot :: remaining |> Fn.flip List.take recent_width_snapshot_capacity;
    true
;;

let reusable_layout_width t ~width =
  let chat = chat_page t in
  let entries =
    Hashtbl.to_alist chat.msg_img_cache
    @ List.concat_map chat.width_snapshots ~f:(fun snapshot -> snapshot.entries)
  in
  List.fold entries ~init:None ~f:(fun reusable (id, entry) ->
    match reusable with
    | Some _ -> reusable
    | None ->
      (match render_index_by_id t ~id with
       | None -> None
       | Some index ->
         let role, text = t.message_array.(index) in
         let _, revision = render_row_identity t ~idx:index |> Option.value_exn in
         if
           Int.equal revision entry.row_revision
           && String.equal role entry.role
           && String.equal text entry.text
           && Chat_message_render_job.Layout_plan.allows entry.layout_plan ~width
         then Some entry.width
         else None))
;;

let restore_layout_width t ~source_width ~width =
  let chat = chat_page t in
  let source_entries =
    Hashtbl.to_alist chat.msg_img_cache
    @ List.concat_map chat.width_snapshots ~f:(fun snapshot -> snapshot.entries)
    |> List.filter ~f:(fun (_, entry) -> Int.equal entry.width source_width)
  in
  let by_id = Hashtbl.create (module Projected_message.Id) in
  List.iter source_entries ~f:(fun (id, entry) -> Hashtbl.set by_id ~key:id ~data:entry);
  let restored =
    Array.filter_mapi t.render_row_ids ~f:(fun index id ->
      Hashtbl.find by_id id
      |> Option.bind ~f:(fun entry ->
        let role, text = t.message_array.(index) in
        let _, revision = render_row_identity t ~idx:index |> Option.value_exn in
        if
          Int.equal revision entry.row_revision
          && String.equal role entry.role
          && String.equal text entry.text
          && Chat_message_render_job.Layout_plan.allows entry.layout_plan ~width
        then
          Some
            ( id
            , { entry with
                width
              ; image = Notty.I.hsnap ~align:`Left width entry.image
              ; layout = { entry.layout with width }
              } )
        else None))
  in
  if Array.length restored <> Array.length t.render_row_ids
  then false
  else (
    let entries = Array.to_list restored in
    Hashtbl.clear chat.msg_img_cache;
    List.iter entries ~f:(fun (id, entry) ->
      Hashtbl.set chat.msg_img_cache ~key:id ~data:entry);
    let heights = Array.map restored ~f:(fun (_, entry) -> entry.height) in
    let prefix = Array.create ~len:(Array.length heights + 1) 0 in
    Array.iteri heights ~f:(fun index height ->
      prefix.(index + 1) <- prefix.(index) + height);
    Renderer_virtual_list.Geometry.replace chat.geometry ~heights ~prefix;
    chat.active_history_width <- Some width;
    chat.history_image_cache <- None;
    chat.dirty_height_rows <- [];
    mark_all_history_chunks_dirty t;
    true)
;;

let render_key_is_current t (key : Chat_message_render_job.Key.t) =
  match render_index_by_id t ~id:key.row_id with
  | None -> false
  | Some index ->
    let role, text =
      if index < Array.length t.projected.rows
      then t.projected.rows.(index).message
      else t.message_array.(index)
    in
    let row_revision = render_row_identity t ~idx:index |> Option.value_exn |> snd in
    let outcome =
      if String.equal role "tool"
      then tool_call_outcome_for_row t ~id:key.row_id
      else None
    in
    Int.equal row_revision key.row_revision
    && Option.equal Int.equal (active_history_width t) (Some key.width)
    && String.equal role key.role
    && String.equal text key.text
    && Poly.equal (tool_output_for_row t ~id:key.row_id) key.tool_output
    && Poly.equal outcome key.tool_call_outcome
;;

let semantic_render_key_is_current
      t
      ~theme_generation
      ~grammar_generation
      (key : Chat_message_render_job.Key.t)
  =
  match render_index_by_id t ~id:key.row_id with
  | None -> false
  | Some index ->
    let role, text =
      if index < Array.length t.projected.rows
      then t.projected.rows.(index).message
      else t.message_array.(index)
    in
    let _, row_revision = render_row_identity t ~idx:index |> Option.value_exn in
    let outcome =
      if String.equal role "tool"
      then tool_call_outcome_for_row t ~id:key.row_id
      else None
    in
    Int.equal row_revision key.row_revision
    && String.equal role key.role
    && String.equal text key.text
    && Poly.equal (tool_output_for_row t ~id:key.row_id) key.tool_output
    && Poly.equal outcome key.tool_call_outcome
    && Int.equal key.theme_generation theme_generation
    && Int.equal key.grammar_generation grammar_generation
;;

let store_semantic_result t (result : Chat_message_render_job.result) =
  Option.iter result.prepared ~f:(fun prepared ->
    let key = result.key in
    Hashtbl.set
      (chat_page t).msg_semantic_cache
      ~key:key.row_id
      ~data:
        { row_revision = key.row_revision
        ; role = key.role
        ; text = key.text
        ; tool_output = key.tool_output
        ; prepared
        ; highlights = result.highlights
        })
;;

let commit_width_preparation_result
      t
      ~theme_generation
      ~grammar_generation
      (result : Chat_message_render_job.result)
  =
  let key = result.key in
  let semantic_is_current =
    semantic_render_key_is_current t ~theme_generation ~grammar_generation key
  in
  if semantic_is_current then store_semantic_result t result;
  match width_preparation t with
  | None -> false
  | Some preparation ->
    let exact_is_current =
      semantic_is_current
      && Poly.(preparation.status = Chat_page_state.Preparing)
      && Int.equal result.request_generation preparation.request_generation
      && Int.equal key.width (fst preparation.terminal_size)
      && Int.equal key.theme_generation preparation.theme_generation
      && Int.equal key.grammar_generation preparation.grammar_generation
      && Int.equal result.layout.width key.width
      && Int.equal result.height (Notty.I.height result.image)
    in
    if not exact_is_current
    then false
    else
      set_width_preparation_row
        t
        ~request_generation:result.request_generation
        ~id:key.row_id
        { row_revision = key.row_revision
        ; width = key.width
        ; role = key.role
        ; text = key.text
        ; image = result.image
        ; height = result.height
        ; layout = result.layout
        ; layout_plan = result.layout_plan
        }
;;

let commit_render_result_internal
      ~update_geometry
      t
      (result : Chat_message_render_job.result)
  =
  let key = result.key in
  if not (render_key_is_current t key)
  then false
  else (
    let index = render_index_by_id t ~id:key.row_id |> Option.value_exn in
    let geometry = (chat_page t).geometry in
    let anchor =
      if t.auto_follow
      then None
      else
        Renderer_virtual_list.Anchor.create_at_scroll
          ~geometry
          ~scroll:(Notty_scroll_box.scroll (scroll_box t))
    in
    let entry =
      match find_img_cache t ~id:key.row_id ~revision:key.row_revision with
      | Some entry
        when Int.equal entry.width key.width
             && String.equal entry.role key.role
             && String.equal entry.text key.text -> entry
      | Some _ | None ->
        { row_revision = key.row_revision
        ; width = key.width
        ; role = key.role
        ; text = key.text
        ; image = result.image
        ; height = result.height
        ; layout = result.layout
        ; layout_plan = result.layout_plan
        }
    in
    let entry =
      { entry with
        image = result.image
      ; height = result.height
      ; layout = result.layout
      ; layout_plan = result.layout_plan
      }
    in
    set_img_cache t ~id:key.row_id entry;
    store_semantic_result t result;
    if
      update_geometry
      && index < Renderer_virtual_list.Geometry.length geometry
      && Int.equal
           result.geometry_generation
           (Renderer_virtual_list.Geometry.generation geometry)
    then Renderer_virtual_list.Geometry.mark_exact geometry ~index ~height:result.height;
    if
      update_geometry
      && not
           (Int.equal
              result.geometry_generation
              (Renderer_virtual_list.Geometry.generation geometry))
    then
      (chat_page t).dirty_height_rows
      <- (key.row_id, key.row_revision) :: (chat_page t).dirty_height_rows;
    if not t.auto_follow
    then
      Option.iter anchor ~f:(fun anchor ->
        Renderer_virtual_list.Anchor.corrected_scroll anchor ~geometry
        |> Option.iter ~f:(Notty_scroll_box.scroll_to (scroll_box t)));
    true)
;;

let commit_render_result = commit_render_result_internal ~update_geometry:true

let commit_startup_render_result t (result : Chat_message_render_job.result) =
  commit_render_result_internal ~update_geometry:false t result
;;

let take_and_clear_dirty_height_rows t =
  let chat = chat_page t in
  let lst = chat.dirty_height_rows in
  chat.dirty_height_rows <- [];
  lst
;;

let defer_dirty_height_rows t rows =
  let chat = chat_page t in
  chat.dirty_height_rows <- rows @ chat.dirty_height_rows
;;

let chat_render_geometry t = (chat_page t).geometry
let msg_heights t = Renderer_virtual_list.Geometry.heights (chat_render_geometry t)
let height_prefix t = Renderer_virtual_list.Geometry.prefix (chat_render_geometry t)

let set_chat_render_geometry t ~heights ~prefix =
  Renderer_virtual_list.Geometry.replace (chat_render_geometry t) ~heights ~prefix
;;

let toggle_mode (t : t) : unit =
  t.mode
  <- (match t.mode with
      | Insert -> Normal
      | Normal -> Insert
      | Cmdline -> Insert
      | Search _ -> Normal)
;;

let set_draft_mode (t : t) (m : draft_mode) = t.draft_mode <- m

let select_message t idx =
  select_projected
    t
    (Option.bind idx ~f:(fun idx -> render_row_identity t ~idx |> Option.map ~f:fst))
;;

(* ------------------------------------------------------------------------- *)
(*  Undo / Redo helpers                                                       *)
(* ------------------------------------------------------------------------- *)

let push_undo (t : t) : unit =
  t.undo_stack <- (t.input_line, t.cursor_pos) :: t.undo_stack;
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
(*  Type-ahead acceptance algorithms                                          *)
(* ------------------------------------------------------------------------- *)

let insert_text_at_cursor (t : t) (text : string) =
  if String.is_empty text
  then ()
  else (
    let s = t.input_line in
    let pos = t.cursor_pos in
    let before = String.sub s ~pos:0 ~len:pos in
    let after = String.sub s ~pos ~len:(String.length s - pos) in
    t.input_line <- before ^ text ^ after;
    t.cursor_pos <- pos + String.length text)
;;

let accept_typeahead_all (t : t) : bool =
  if not (typeahead_is_relevant t)
  then false
  else (
    match t.typeahead_completion with
    | None -> false
    | Some completion ->
      let text = Util.sanitize ~strip:false completion.text in
      if String.is_empty text
      then false
      else (
        push_undo t;
        clear_selection t;
        insert_text_at_cursor t text;
        clear_typeahead t;
        ignore (bump_typeahead_generation t : int);
        true))
;;

let accept_typeahead_line (t : t) : bool =
  if not (typeahead_is_relevant t)
  then false
  else (
    match t.typeahead_completion with
    | None -> false
    | Some completion ->
      let text = Util.sanitize ~strip:false completion.text in
      let inserted, remainder =
        match String.index text '\n' with
        | None -> text, ""
        | Some i ->
          let next = i + 1 in
          String.prefix text next, String.drop_prefix text next
      in
      if String.is_empty inserted
      then false
      else (
        push_undo t;
        clear_selection t;
        insert_text_at_cursor t inserted;
        let generation = bump_typeahead_generation t in
        let base_input = t.input_line in
        let base_cursor = t.cursor_pos in
        clear_typeahead t;
        if not (String.is_empty remainder)
        then
          t.typeahead_completion
          <- Some { text = remainder; base_input; base_cursor; generation };
        true))
;;

(* ------------------------------------------------------------------------- *)
(*  Internal helpers – these are largely a direct carry-over from the mutable
    implementation present before refactoring step 6.                       *)
(* ------------------------------------------------------------------------- *)

let update_message_text_by_id model ~id new_txt =
  match render_index_by_id model ~id with
  | None -> ()
  | Some index ->
    let messages = model.message_array in
    let role, _ = messages.(index) in
    messages.(index) <- role, new_txt;
    bump_message_revision model ~idx:index;
    invalidate_img_cache_index model ~idx:index;
    mark_history_row_dirty model ~id;
    invalidate_width_preparation_row model ~id
;;

let append_render_message model ~row_id message =
  model.message_array <- Array.append model.message_array [| message |];
  model.render_row_ids <- Array.append model.render_row_ids [| row_id |];
  model.transcript_generation <- model.transcript_generation + 1;
  model.render_generation <- model.render_generation + 1;
  model.message_revisions <- Array.append model.message_revisions [| 0 |];
  let index = Array.length model.message_array - 1 in
  let chat = chat_page model in
  Renderer_virtual_list.Geometry.extend_estimated
    chat.geometry
    ~length:(Array.length model.message_array)
    ~estimated_height_at_index:(fun _ -> 5);
  let chunk_index = History_chunk.canonical_index_of_row_exn ~row_index:index in
  Hashtbl.set chat.row_chunk_by_id ~key:row_id ~data:chunk_index;
  mark_history_chunk_index_dirty model chunk_index;
  reconcile_width_preparation model
;;

let row_id_of_buffer_id id =
  match History_entry.Id.of_string id with
  | Ok id -> Projected_message.Id.canonical id
  | Error _ ->
    Projected_message.Id.local ~namespace:"stream" ~local_id:id |> Result.ok_or_failwith
;;

let ensure_buffer model ~id ~role =
  match Hashtbl.find model.msg_buffers id with
  | Some b -> b
  | None ->
    let row_id = row_id_of_buffer_id id in
    let b = { buf = Buffer.create 256; row_id } in
    Hashtbl.set model.msg_buffers ~key:id ~data:b;
    let message = role, "" in
    append_render_message model ~row_id message;
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
    Buffer.add_string buf.buf text;
    update_message_text_by_id model ~id:buf.row_id (Buffer.contents buf.buf);
    model
  | Types.Set_function_name { id; name } ->
    Hashtbl.set model.function_name_by_id ~key:id ~data:name;
    model
  | Types.Associate_tool_call { item_id; call_id } ->
    let buffer = ensure_buffer model ~id:item_id ~role:"tool" in
    Hashtbl.set model.tool_call_id_by_id ~key:buffer.row_id ~data:call_id;
    if Hashtbl.mem model.tool_call_outcome_by_call_id call_id
    then invalidate_render_metadata_by_id model ~id:buffer.row_id;
    model
  | Types.Set_function_output { id; output } ->
    let role =
      match Hashtbl.find model.function_name_by_id id with
      | Some name when String.Caseless.equal name "fork" -> "fork"
      | _ -> "tool_output"
    in
    let buf =
      match Hashtbl.find model.msg_buffers id with
      | Some b -> b
      | None -> ensure_buffer model ~id ~role
    in
    let call_text = Buffer.contents buf.buf in
    let name_opt = Hashtbl.find model.function_name_by_id id in
    let path =
      match Option.map ~f:String.lowercase name_opt with
      | Some "read_file" | Some "read_directory" ->
        (match Option.join (Hashtbl.find model.tool_path_by_call_id id) with
         | Some _ as p -> p
         | None -> extract_path_from_call_text call_text)
      | _ -> None
    in
    let max_len = 10_000 in
    let txt = Util.sanitize ~strip:false output in
    let txt =
      if String.length txt > max_len
      then String.sub txt ~pos:0 ~len:max_len ^ "\n…truncated…"
      else txt
    in
    Buffer.clear buf.buf;
    Buffer.add_string buf.buf txt;
    update_message_text_by_id model ~id:buf.row_id (Buffer.contents buf.buf);
    let kind = classify_tool_output ~name_opt ~path in
    ignore (set_tool_output_kind_for_row model ~id:buf.row_id kind : bool);
    model
  | Types.Update_reasoning_idx { id; idx } ->
    (match Hashtbl.find model.reasoning_idx_by_id id with
     | Some r -> r := idx
     | None -> Hashtbl.set model.reasoning_idx_by_id ~key:id ~data:(ref idx));
    model
  | Types.Add_user_message { text } ->
    let message = "user", text in
    let row_id =
      Projected_message.Id.local
        ~namespace:"user-patch"
        ~local_id:(Int.to_string model.transcript_generation)
      |> Result.ok_or_failwith
    in
    append_render_message model ~row_id message;
    model
  | Types.Add_placeholder_message { role; text } ->
    let message = role, text in
    let row_id =
      Projected_message.Id.local
        ~namespace:"placeholder-patch"
        ~local_id:(Int.to_string model.transcript_generation)
      |> Result.ok_or_failwith
    in
    append_render_message model ~row_id message;
    model
;;

let apply_patches model patches = List.fold patches ~init:model ~f:apply_patch

let add_history_item (model : t) (entry : History_entry.t) =
  model.history_items <- model.history_items @ [ entry ];
  model
;;

let rebuild_tool_output_index_for_items (model : t) (entries : History_entry.t list)
  : unit
  =
  Hashtbl.clear model.tool_output_by_index;
  Hashtbl.clear model.tool_output_by_id;
  Hashtbl.clear model.tool_call_id_by_id;
  let call_info_by_id = Hashtbl.create (module String) in
  List.iter entries ~f:(fun entry ->
    match History_entry.item entry with
    | Res_item.Function_call fc ->
      let name = fc.name in
      let path =
        match String.lowercase name with
        | "read_file" | "read_directory" -> read_file_path_of_arguments fc.arguments
        | _ -> None
      in
      Hashtbl.set call_info_by_id ~key:fc.call_id ~data:(name, path)
    | Res_item.Custom_tool_call tc ->
      let name = tc.name in
      let path =
        match String.lowercase name with
        | "read_file" | "read_directory" -> read_file_path_of_arguments tc.input
        | _ -> None
      in
      Hashtbl.set call_info_by_id ~key:tc.call_id ~data:(name, path)
    | _ -> ());
  List.iter entries ~f:(fun entry ->
    let it = History_entry.item entry in
    let row_id = Projected_message.Id.canonical (History_entry.id entry) in
    match Conversation.pair_of_item it with
    | None -> ()
    | Some _msg ->
      (match it with
       | Res_item.Function_call fc ->
         Hashtbl.set model.tool_call_id_by_id ~key:row_id ~data:fc.call_id;
         invalidate_render_metadata_by_id model ~id:row_id
       | Res_item.Custom_tool_call tc ->
         Hashtbl.set model.tool_call_id_by_id ~key:row_id ~data:tc.call_id;
         invalidate_render_metadata_by_id model ~id:row_id
       | Res_item.Function_call_output fco ->
         let name_opt, path =
           match Hashtbl.find call_info_by_id fco.call_id with
           | None -> None, None
           | Some (name, path) -> Some name, path
         in
         let kind = classify_tool_output ~name_opt ~path in
         Hashtbl.set model.tool_output_by_id ~key:row_id ~data:kind;
         invalidate_render_metadata_by_id model ~id:row_id
       | Res_item.Custom_tool_call_output tco ->
         let name_opt, path =
           match Hashtbl.find call_info_by_id tco.call_id with
           | None -> None, None
           | Some (name, path) -> Some name, path
         in
         let kind = classify_tool_output ~name_opt ~path in
         Hashtbl.set model.tool_output_by_id ~key:row_id ~data:kind;
         invalidate_render_metadata_by_id model ~id:row_id
       | _ -> ()))
;;

let rebuild_tool_output_index model =
  rebuild_tool_output_index_for_items model model.history_items
;;

let clamp_selected_message (model : t) : unit =
  let message_count = Array.length model.message_array in
  match selected_msg model with
  | None -> ()
  | Some _ when message_count = 0 -> select_message model None
  | Some idx -> select_message model (Some (Int.min (message_count - 1) idx))
;;
