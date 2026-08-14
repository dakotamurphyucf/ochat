open! Core
module S = Summarizer

(*------------------------------------------------------------------*)
(*  Helpers                                                          *)
(*------------------------------------------------------------------*)

let build_system_summary_message
      ?(role = Openai.Responses.Input_message.User)
      (summary : string)
  : Openai.Responses.Item.t
  =
  let open Openai.Responses in
  let open Input_message in
  let text_item text : content_item = Text { text; _type = "input_text" } in
  let msg : Input_message.t =
    { role; content = [ text_item summary ]; _type = "message" }
  in
  Item.Input_message msg
;;

let partition_history ~item history =
  let open Openai.Responses in
  let is_previous_compaction = function
    | Item.Input_message { role = User; content = Input_message.Text { text; _ } :: _; _ }
      -> String.strip text |> String.is_prefix ~prefix:"<system-reminder>"
    | _ -> false
  in
  let _, devs, comps, relevant_items =
    List.fold_right
      history
      ~init:(0, [], [], [])
      ~f:(fun entry (retained, devs, comps, items) ->
        let payload = item entry in
        if is_previous_compaction payload
        then
          if retained < 10
          then retained + 1, devs, entry :: comps, entry :: items
          else retained, devs, comps, items
        else (
          match payload with
          | Item.Input_message { role = System | Developer; _ } ->
            retained, entry :: devs, comps, entry :: items
          | _ -> retained, devs, comps, entry :: items))
  in
  devs, comps, relevant_items
;;

let compact_entries_with ~summarise ~allocator ~env ~(history : History_entry.t list) =
  try
    let devs, comps, relevant_entries =
      partition_history ~item:History_entry.item history
    in
    let open Result.Let_syntax in
    let%bind compacted =
      summarise ~relevant_items:(History_entry.items relevant_entries) ~env
    in
    let summary =
      sprintf
        "<system-reminder>This is a message from the system that we compacted the \
         conversation history from a previous session.\n\
         Here is a summary of the session that you saved:\n\
         %s\n\
         Remember this is not a message from the user, but a system reminder that you \
         should not respond to.\n\
         </system-reminder>"
        compacted
    in
    let%map reminder =
      History_entry.create ~allocator (build_system_summary_message summary)
      |> Result.map_error ~f:(fun error -> Failure error)
    in
    List.concat [ devs; comps; [ reminder ] ]
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error exn
;;

let compact_entries = compact_entries_with ~summarise:S.summarise

module For_testing = struct
  let process_current_entries history = partition_history ~item:History_entry.item history
  let compact_entries_with = compact_entries_with
end
