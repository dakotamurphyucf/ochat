open Core
open Types
module Res = Openai.Responses
module Res_stream = Openai.Responses.Response_stream
module Item_stream = Openai.Responses.Response_stream.Item

(* ------------------------------------------------------------------------- *)
(*  Main translation function                                               *)
(* ------------------------------------------------------------------------- *)
let handle_fn_out ~(model : Model.t) (out : Res.Function_call_output.t) : Types.patch list
  =
  let _mod = model in
  [ Set_function_output { id = out.call_id; output = out.output } ]
;;

let handle_event ~(model : Model.t) (ev : Res_stream.t) : Types.patch list =
  match ev with
  (* --------------------------------------------------------------------- *)
  (* Assistant text delta                                                   *)
  (* --------------------------------------------------------------------- *)
  | Res_stream.Output_text_delta { item_id; delta; _ } ->
    let patches = ref [] in
    if not (Hashtbl.mem model.msg_buffers item_id)
    then patches := Ensure_buffer { id = item_id; role = "assistant" } :: !patches;
    patches
    := Append_text
         { id = item_id; role = "assistant"; text = Util.sanitize ~strip:false delta }
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
       if not (Hashtbl.mem model.msg_buffers idx)
       then patches := Ensure_buffer { id = idx; role = "tool" } :: !patches;
       patches := Set_function_name { id = idx; name = fc.name } :: !patches;
       List.rev !patches
     | Item_stream.Reasoning r ->
       if Hashtbl.mem model.msg_buffers r.id
       then []
       else [ Ensure_buffer { id = r.id; role = "reasoning" } ]
     | Item_stream.Output_message om ->
       (* Already complete – insert full text immediately. *)
       let txt =
         List.map om.content ~f:(fun c -> Util.sanitize ~strip:false c.text)
         |> String.concat ~sep:" "
       in
       let patches =
         if Hashtbl.mem model.msg_buffers om.id
         then []
         else [ Ensure_buffer { id = om.id; role = "assistant" } ]
       in
       patches @ [ Append_text { id = om.id; role = "assistant"; text = txt } ]
     | _ -> [])
  (* --------------------------------------------------------------------- *)
  (* Reasoning summaries                                                    *)
  (* --------------------------------------------------------------------- *)
  | Res_stream.Reasoning_summary_text_delta { item_id; delta; summary_index; _ } ->
    let prefix_newline =
      match Hashtbl.find model.reasoning_idx_by_id item_id with
      | Some idx_ref when !idx_ref = summary_index -> ""
      | Some _ -> "\n"
      | None -> ""
    in
    (* We'll also update the remembered summary index. *)
    let patches = ref [] in
    if not (Hashtbl.mem model.msg_buffers item_id)
    then patches := Ensure_buffer { id = item_id; role = "reasoning" } :: !patches;
    patches := Update_reasoning_idx { id = item_id; idx = summary_index } :: !patches;
    let txt = prefix_newline ^ Util.sanitize ~strip:false delta in
    patches := Append_text { id = item_id; role = "reasoning"; text = txt } :: !patches;
    List.rev !patches
  (* --------------------------------------------------------------------- *)
  (* Function call argument streaming                                       *)
  (* --------------------------------------------------------------------- *)
  | Res_stream.Function_call_arguments_delta { item_id; delta; _ } ->
    let buf_empty =
      match Hashtbl.find model.msg_buffers item_id with
      | Some b -> String.is_empty !(b.text)
      | None -> true
    in
    let fn_name =
      Option.value (Hashtbl.find model.function_name_by_id item_id) ~default:"tool"
    in
    let patches = ref [] in
    if not (Hashtbl.mem model.msg_buffers item_id)
    then patches := Ensure_buffer { id = item_id; role = "tool" } :: !patches;
    if buf_empty
    then
      patches
      := Append_text { id = item_id; role = "tool"; text = fn_name ^ "(" } :: !patches;
    patches
    := Append_text
         { id = item_id; role = "tool"; text = Util.sanitize ~strip:false delta }
       :: !patches;
    List.rev !patches
  | Res_stream.Function_call_arguments_done { item_id; _ } ->
    let buf_empty =
      match Hashtbl.find model.msg_buffers item_id with
      | Some b -> String.is_empty !(b.text)
      | None -> true
    in
    let fn_name =
      Option.value (Hashtbl.find model.function_name_by_id item_id) ~default:"tool"
    in
    let patches = ref [] in
    if not (Hashtbl.mem model.msg_buffers item_id)
    then patches := Ensure_buffer { id = item_id; role = "tool" } :: !patches;
    if buf_empty
    then
      patches
      := Append_text { id = item_id; role = "tool"; text = fn_name ^ "(" } :: !patches;
    patches
    := Append_text { id = item_id; role = "tool"; text = Util.sanitize ~strip:false ")" }
       :: !patches;
    List.rev !patches
  (* --------------------------------------------------------------------- *)
  (* Everything else is ignored for now                                      *)
  (* --------------------------------------------------------------------- *)
  | _ -> []
;;
