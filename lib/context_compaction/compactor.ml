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

let token_codec = lazy (Tikitoken.create_codec Tiktoken_data.o200k_base)

let estimated_tokens items =
  let codec = Lazy.force token_codec in
  List.sum
    (module Int)
    items
    ~f:(fun item ->
      let text = Openai.Responses.Item.jsonaf_of_t item |> Jsonaf.to_string in
      8 + List.length (Tikitoken.encode ~codec ~text))
;;

let select_relevant ~score config items =
  if not config.Config.relevance_filtering
  then items
  else (
    let groups = S.grouped_items items in
    let last = List.length groups - 1 in
    List.filteri groups ~f:(fun index group ->
      index = last
      || List.exists group ~f:(function
        | Openai.Responses.Item.Input_message { role = System | Developer; _ } -> true
        | _ -> false)
      || Float.(score (S.render_transcript group) >= config.relevance_threshold))
    |> List.concat)
;;

let compact_entries_configured
      ~config
      ~summarise
      ~allocator
      ~env
      ~(history : History_entry.t list)
  =
  try
    if not (Config.is_valid config) then invalid_arg "invalid compaction configuration";
    let devs, comps, relevant_entries =
      partition_history ~item:History_entry.item history
    in
    let open Result.Let_syntax in
    let%bind compacted =
      let relevant_items =
        select_relevant
          config
          (History_entry.items relevant_entries)
          ~score:(fun prompt -> Relevance_judge.score_relevance ?env config ~prompt)
      in
      summarise ~relevant_items ~env
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
    let item = build_system_summary_message summary in
    let retained = devs @ comps in
    let%bind () =
      if
        estimated_tokens (History_entry.items retained @ [ item ]) <= config.context_limit
      then Ok ()
      else Error (Failure "compaction exceeds context_limit; original history retained")
    in
    let%map reminder =
      History_entry.create ~allocator item
      |> Result.map_error ~f:(fun error -> Failure error)
    in
    List.concat [ devs; comps; [ reminder ] ]
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error exn
;;

let compact_entries_with ~summarise ~allocator ~env ~history =
  compact_entries_configured
    ~config:(Config.load ?env ())
    ~summarise
    ~allocator
    ~env
    ~history
;;

let compact_entries ~allocator ~env ~history =
  compact_entries_with ~summarise:S.summarise ~allocator ~env ~history
;;

module For_testing = struct
  let process_current_entries history = partition_history ~item:History_entry.item history
  let compact_entries_with = compact_entries_with
  let compact_entries_configured = compact_entries_configured
  let select_relevant = select_relevant
  let estimated_tokens = estimated_tokens
end
