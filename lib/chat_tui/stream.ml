open Core
open Types

(* ------------------------------------------------------------------------- *)
(*  Helper                                                                    *)
(* ------------------------------------------------------------------------- *)

let role_for_event (model : Model.t) ~(default : string) : string =
  match Model.active_fork model with
  | Some _ -> "fork"
  | None -> default
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

let update_tool_output_metadata_if_present
      ~(model : Model.t)
      ~(call_id : string)
      ~(kind : Types.tool_output_kind)
  : unit
  =
  match Hashtbl.find (Model.msg_buffers model) call_id with
  | None -> ()
  | Some b ->
    if Hashtbl.mem (Model.tool_output_by_index model) b.index
    then (
      Hashtbl.set (Model.tool_output_by_index model) ~key:b.index ~data:kind;
      Model.invalidate_img_cache_index model ~idx:b.index)
;;

module Res = Openai.Responses
module Res_stream = Openai.Responses.Response_stream
module Item_stream = Openai.Responses.Response_stream.Item

let tool_output_to_string (out : Res.Tool_output.Output.t) : string =
  match out with
  | Openai.Responses.Tool_output.Output.Text text -> text
  | Content parts ->
    parts
    |> List.map ~f:(function
      | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
      | Input_image { image_url; _ } -> Printf.sprintf "<image src=\"%s\" />" image_url)
    |> String.concat ~sep:"\n"
;;

(* ------------------------------------------------------------------------- *)
(*  Main translation function                                               *)
(* ------------------------------------------------------------------------- *)
let handle_fn_out ~(model : Model.t) (out : Res.Function_call_output.t) : Types.patch list
  =
  let _mod = model in
  (match Model.active_fork model with
   | Some call_id when String.equal call_id out.call_id ->
     Model.set_active_fork model None;
     Model.set_fork_start_index model None
   | _ -> ());
  let output = tool_output_to_string out.output in
  [ Set_function_output { id = out.call_id; output } ]
;;

let handle_tool_out ~(model : Model.t) (item : Res.Item.t) : Types.patch list =
  let mk ~call_id ~output =
    (match Model.active_fork model with
     | Some active_call_id when String.equal active_call_id call_id ->
       Model.set_active_fork model None;
       Model.set_fork_start_index model None
     | _ -> ());
    [ Set_function_output { id = call_id; output = tool_output_to_string output } ]
  in
  match item with
  | Res.Item.Function_call_output out -> mk ~call_id:out.call_id ~output:out.output
  | Res.Item.Custom_tool_call_output out -> mk ~call_id:out.call_id ~output:out.output
  | _ -> []
;;

let handle_event ~(model : Model.t) (ev : Res_stream.t) : Types.patch list =
  match ev with
  (* --------------------------------------------------------------------- *)
  (* Assistant text delta                                                   *)
  (* --------------------------------------------------------------------- *)
  | Res_stream.Output_text_delta { item_id; delta; _ } ->
    let role = role_for_event model ~default:"assistant" in
    let patches = ref [] in
    if not (Hashtbl.mem (Model.msg_buffers model) item_id)
    then patches := Ensure_buffer { id = item_id; role } :: !patches;
    patches := Append_text { id = item_id; role; text = delta } :: !patches;
    List.rev !patches
  (* --------------------------------------------------------------------- *)
  (* A new item has been announced – we may need to prepare buffers.        *)
  (* --------------------------------------------------------------------- *)
  | Res_stream.Output_item_added { item; _ } ->
    (match item with
     | Item_stream.Function_call fc ->
       let patches = ref [] in
       let idx = Option.value fc.id ~default:fc.call_id in
       Hashtbl.set (Model.call_id_by_item_id model) ~key:idx ~data:fc.call_id;
       (match String.lowercase fc.name with
        | "read_file" | "read_directory" ->
          Hashtbl.set
            (Model.tool_path_by_call_id model)
            ~key:fc.call_id
            ~data:(read_file_path_of_arguments fc.arguments)
        | _ -> ());
       let is_fork = String.equal fc.name "fork" in
       let role = if is_fork then "fork" else role_for_event model ~default:"tool" in
       if not (Hashtbl.mem (Model.msg_buffers model) idx)
       then patches := Ensure_buffer { id = idx; role } :: !patches;
       patches := Set_function_name { id = fc.call_id; name = fc.name } :: !patches;
       patches := Set_function_name { id = idx; name = fc.name } :: !patches;
       patches := Append_text { id = idx; role; text = fc.name ^ "(" } :: !patches;
       (* Track active fork so subsequent deltas are coloured appropriately *)
       if is_fork
       then (
         Model.set_active_fork model (Some fc.call_id);
         Model.set_fork_start_index model (Some (List.length (Model.history_items model))));
       List.rev !patches
     | Item_stream.Custom_function tc ->
       let patches = ref [] in
       let idx = Option.value tc.id ~default:tc.call_id in
       Hashtbl.set (Model.call_id_by_item_id model) ~key:idx ~data:tc.call_id;
       (match String.lowercase tc.name with
        | "read_file" | "read_directory" ->
          Hashtbl.set
            (Model.tool_path_by_call_id model)
            ~key:tc.call_id
            ~data:(read_file_path_of_arguments tc.input)
        | _ -> ());
       let role = role_for_event model ~default:"tool" in
       if not (Hashtbl.mem (Model.msg_buffers model) idx)
       then patches := Ensure_buffer { id = idx; role } :: !patches;
       patches := Set_function_name { id = tc.call_id; name = tc.name } :: !patches;
       patches := Set_function_name { id = idx; name = tc.name } :: !patches;
       patches := Append_text { id = idx; role; text = tc.name ^ "(" } :: !patches;
       List.rev !patches
     | Item_stream.Reasoning r ->
       if Hashtbl.mem (Model.msg_buffers model) r.id
       then []
       else [ Ensure_buffer { id = r.id; role = "reasoning" } ]
     | Item_stream.Output_message om ->
       let role = role_for_event model ~default:"assistant" in
       let txt = List.map om.content ~f:(fun c -> c.text) |> String.concat ~sep:" " in
       let patches =
         if Hashtbl.mem (Model.msg_buffers model) om.id
         then []
         else [ Ensure_buffer { id = om.id; role } ]
       in
       patches @ [ Append_text { id = om.id; role; text = txt } ]
     | _ -> [])
  (* --------------------------------------------------------------------- *)
  (* Reasoning summaries                                                    *)
  (* --------------------------------------------------------------------- *)
  | Res_stream.Reasoning_summary_text_delta { item_id; delta; summary_index; _ } ->
    (* Append reasoning summaries verbatim; do not inject extra newlines between
       summary segments – the renderer handles reflow. *)
    let patches = ref [] in
    let role = role_for_event model ~default:"reasoning" in
    if not (Hashtbl.mem (Model.msg_buffers model) item_id)
    then patches := Ensure_buffer { id = item_id; role } :: !patches;
    patches := Update_reasoning_idx { id = item_id; idx = summary_index } :: !patches;
    patches := Append_text { id = item_id; role = "reasoning"; text = delta } :: !patches;
    List.rev !patches
  (* --------------------------------------------------------------------- *)
  (* Function call argument streaming                                       *)
  (* --------------------------------------------------------------------- *)
  | Res_stream.Function_call_arguments_delta { item_id; delta; _ } ->
    let _buf_empty =
      match Hashtbl.find (Model.msg_buffers model) item_id with
      | Some b -> Buffer.length b.buf = 0
      | None -> true
    in
    let _fn_name =
      Option.value
        (Hashtbl.find (Model.function_name_by_id model) item_id)
        ~default:"tool"
    in
    let patches = ref [] in
    let role = role_for_event model ~default:"tool" in
    if not (Hashtbl.mem (Model.msg_buffers model) item_id)
    then patches := Ensure_buffer { id = item_id; role } :: !patches;
    patches := Append_text { id = item_id; role; text = delta } :: !patches;
    List.rev !patches
  | Res_stream.Function_call_arguments_done { arguments; item_id; _ } ->
    let _buf_empty =
      match Hashtbl.find (Model.msg_buffers model) item_id with
      | Some b -> Buffer.length b.buf = 0
      | None -> true
    in
    let _fn_name =
      Option.value
        (Hashtbl.find (Model.function_name_by_id model) item_id)
        ~default:"tool"
    in
    let patches = ref [] in
    let role = role_for_event model ~default:"tool" in
    let call_id =
      match Hashtbl.find (Model.call_id_by_item_id model) item_id with
      | Some cid -> cid
      | None -> item_id
    in
    (match
       Option.value (Hashtbl.find (Model.function_name_by_id model) item_id) ~default:""
       |> String.lowercase
     with
     | "read_file" ->
       let path = read_file_path_of_arguments arguments in
       Hashtbl.set (Model.tool_path_by_call_id model) ~key:call_id ~data:path;
       update_tool_output_metadata_if_present
         ~model
         ~call_id
         ~kind:(Types.Read_file { path })
     | "read_directory" ->
       let path = read_file_path_of_arguments arguments in
       Hashtbl.set (Model.tool_path_by_call_id model) ~key:call_id ~data:path;
       update_tool_output_metadata_if_present
         ~model
         ~call_id
         ~kind:(Types.Read_directory { path })
     | _ -> ());
    if not (Hashtbl.mem (Model.msg_buffers model) item_id)
    then patches := Ensure_buffer { id = item_id; role } :: !patches;
    patches := Append_text { id = item_id; role; text = ")" } :: !patches;
    List.rev !patches
  | Res_stream.Custom_tool_call_input_delta { item_id; delta; _ } ->
    let _buf_empty =
      match Hashtbl.find (Model.msg_buffers model) item_id with
      | Some b -> Buffer.length b.buf = 0
      | None -> true
    in
    let _fn_name =
      Option.value
        (Hashtbl.find (Model.function_name_by_id model) item_id)
        ~default:"tool"
    in
    let patches = ref [] in
    let role = role_for_event model ~default:"tool" in
    if not (Hashtbl.mem (Model.msg_buffers model) item_id)
    then patches := Ensure_buffer { id = item_id; role } :: !patches;
    patches := Append_text { id = item_id; role; text = delta } :: !patches;
    List.rev !patches
  | Res_stream.Custom_tool_call_input_done { input; item_id; _ } ->
    let _buf_empty =
      match Hashtbl.find (Model.msg_buffers model) item_id with
      | Some b -> Buffer.length b.buf = 0
      | None -> true
    in
    let _fn_name =
      Option.value
        (Hashtbl.find (Model.function_name_by_id model) item_id)
        ~default:"tool"
    in
    let patches = ref [] in
    let role = role_for_event model ~default:"tool" in
    let call_id =
      match Hashtbl.find (Model.call_id_by_item_id model) item_id with
      | Some cid -> cid
      | None -> item_id
    in
    (match
       Option.value (Hashtbl.find (Model.function_name_by_id model) item_id) ~default:""
       |> String.lowercase
     with
     | "read_file" ->
       let path = read_file_path_of_arguments input in
       Hashtbl.set (Model.tool_path_by_call_id model) ~key:call_id ~data:path;
       update_tool_output_metadata_if_present
         ~model
         ~call_id
         ~kind:(Types.Read_file { path })
     | "read_directory" ->
       let path = read_file_path_of_arguments input in
       Hashtbl.set (Model.tool_path_by_call_id model) ~key:call_id ~data:path;
       update_tool_output_metadata_if_present
         ~model
         ~call_id
         ~kind:(Types.Read_directory { path })
     | _ -> ());
    if not (Hashtbl.mem (Model.msg_buffers model) item_id)
    then patches := Ensure_buffer { id = item_id; role } :: !patches;
    patches := Append_text { id = item_id; role; text = ")" } :: !patches;
    List.rev !patches
  (* --------------------------------------------------------------------- *)
  (* Everything else is ignored for now                                      *)
  (* --------------------------------------------------------------------- *)
  | _ -> []
;;

let handle_events ~(model : Model.t) (evs : Res_stream.t list) : Types.patch list =
  List.concat_map evs ~f:(handle_event ~model)
;;
