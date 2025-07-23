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

module Res = Openai.Responses
module Res_stream = Openai.Responses.Response_stream
module Item_stream = Openai.Responses.Response_stream.Item

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
  [ Set_function_output { id = out.call_id; output = out.output } ]
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
    patches
    := Append_text { id = item_id; role; text = Util.sanitize ~strip:false delta }
       :: !patches;
    List.rev !patches
  (* --------------------------------------------------------------------- *)
  (* A new item has been announced – we may need to prepare buffers.        *)
  (* --------------------------------------------------------------------- *)
  | Res_stream.Output_item_added { item; _ } ->
    (match item with
     | Item_stream.Function_call fc ->
       let patches = ref [] in
       let idx = Option.value fc.id ~default:fc.call_id in
       let is_fork = String.equal fc.name "fork" in
       let role = if is_fork then "fork" else role_for_event model ~default:"tool" in
       if not (Hashtbl.mem (Model.msg_buffers model) idx)
       then patches := Ensure_buffer { id = idx; role } :: !patches;
       patches := Set_function_name { id = idx; name = fc.name } :: !patches;
       (* Track active fork so subsequent deltas are coloured appropriately *)
       if is_fork
       then (
         Model.set_active_fork model (Some fc.call_id);
         Model.set_fork_start_index model (Some (List.length (Model.history_items model))));
       List.rev !patches
     | Item_stream.Reasoning r ->
       if Hashtbl.mem (Model.msg_buffers model) r.id
       then []
       else [ Ensure_buffer { id = r.id; role = "reasoning" } ]
     | Item_stream.Output_message om ->
       let role = role_for_event model ~default:"assistant" in
       let txt =
         List.map om.content ~f:(fun c -> Util.sanitize ~strip:false c.text)
         |> String.concat ~sep:" "
       in
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
    let prefix_newline =
      match Hashtbl.find (Model.reasoning_idx_by_id model) item_id with
      | Some idx_ref when !idx_ref = summary_index -> ""
      | Some _ -> "\n"
      | None -> ""
    in
    (* We'll also update the remembered summary index. *)
    let patches = ref [] in
    let role = role_for_event model ~default:"reasoning" in
    if not (Hashtbl.mem (Model.msg_buffers model) item_id)
    then patches := Ensure_buffer { id = item_id; role } :: !patches;
    patches := Update_reasoning_idx { id = item_id; idx = summary_index } :: !patches;
    let txt = prefix_newline ^ Util.sanitize ~strip:false delta in
    patches := Append_text { id = item_id; role = "reasoning"; text = txt } :: !patches;
    List.rev !patches
  (* --------------------------------------------------------------------- *)
  (* Function call argument streaming                                       *)
  (* --------------------------------------------------------------------- *)
  | Res_stream.Function_call_arguments_delta { item_id; delta; _ } ->
    let buf_empty =
      match Hashtbl.find (Model.msg_buffers model) item_id with
      | Some b -> String.is_empty !(b.text)
      | None -> true
    in
    let fn_name =
      Option.value
        (Hashtbl.find (Model.function_name_by_id model) item_id)
        ~default:"tool"
    in
    let patches = ref [] in
    let role = role_for_event model ~default:"tool" in
    if not (Hashtbl.mem (Model.msg_buffers model) item_id)
    then patches := Ensure_buffer { id = item_id; role } :: !patches;
    if buf_empty
    then patches := Append_text { id = item_id; role; text = fn_name ^ "(" } :: !patches;
    patches
    := Append_text { id = item_id; role; text = Util.sanitize ~strip:false delta }
       :: !patches;
    List.rev !patches
  | Res_stream.Function_call_arguments_done { item_id; _ } ->
    let buf_empty =
      match Hashtbl.find (Model.msg_buffers model) item_id with
      | Some b -> String.is_empty !(b.text)
      | None -> true
    in
    let fn_name =
      Option.value
        (Hashtbl.find (Model.function_name_by_id model) item_id)
        ~default:"tool"
    in
    let patches = ref [] in
    let role = role_for_event model ~default:"tool" in
    if not (Hashtbl.mem (Model.msg_buffers model) item_id)
    then patches := Ensure_buffer { id = item_id; role } :: !patches;
    if buf_empty
    then patches := Append_text { id = item_id; role; text = fn_name ^ "(" } :: !patches;
    patches
    := Append_text { id = item_id; role; text = Util.sanitize ~strip:false ")" }
       :: !patches;
    List.rev !patches
  (* --------------------------------------------------------------------- *)
  (* Everything else is ignored for now                                      *)
  (* --------------------------------------------------------------------- *)
  | _ -> []
;;

let handle_events ~(model : Model.t) (evs : Res_stream.t list) : Types.patch list =
  List.concat_map evs ~f:(handle_event ~model)
;;
