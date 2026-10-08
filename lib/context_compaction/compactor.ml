open! Core
module S = Summarizer

let partition_history history =
  let _, policies, reminders, relevant =
    List.fold_right
      history
      ~init:(0, [], [], [])
      ~f:(fun entry (count, policies, reminders, relevant) ->
        if History_view.is_reminder entry
        then
          if count < 10
          then count + 1, policies, entry :: reminders, entry :: relevant
          else count, policies, reminders, relevant
        else if History_view.is_policy entry
        then count, entry :: policies, reminders, entry :: relevant
        else count, policies, reminders, entry :: relevant)
  in
  policies, reminders, relevant
;;

let token_codec = lazy (Tikitoken.create_codec Tiktoken_data.o200k_base)

let estimated_tokens items =
  let codec = Lazy.force token_codec in
  List.sum
    (module Int)
    items
    ~f:(fun item ->
      8 + List.length (Tikitoken.encode ~codec ~text:(S.render_transcript [ item ])))
;;

let select_relevant ~score config items =
  if not config.Config.relevance_filtering
  then items
  else (
    let groups = S.grouped_items items in
    let last = List.length groups - 1 in
    List.filteri groups ~f:(fun index group ->
      index = last
      || List.exists group ~f:History_view.is_policy
      || Float.(score (S.render_transcript group) >= config.relevance_threshold))
    |> List.concat)
;;

let compact_entries_configured ~config ~score ~summarise ~allocator ~env ~history =
  if not (Config.is_valid config)
  then Error (Invalid_argument "invalid compaction configuration")
  else (
    let policies, reminders, relevant = partition_history history in
    let open Result.Let_syntax in
    let%bind compacted =
      summarise ~relevant_items:(select_relevant ~score config relevant) ~env
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
    let payload = History_view.message ~role:User summary in
    (* A private preview ID is never returned or reserved; estimation reads only
       semantic text. Durable allocation happens only after the budget check. *)
    let preview_id =
      History_entry.Id.create ~namespace:"compaction-preview" ~sequence:0
      |> Result.ok_or_failwith
    in
    let preview = History_entry.create_with_id ~id:preview_id payload in
    let retained = policies @ reminders in
    let%bind () =
      if estimated_tokens (retained @ [ preview ]) <= config.context_limit
      then Ok ()
      else Error (Failure "compaction exceeds context_limit; original history retained")
    in
    let%map reminder =
      History_entry.create ~allocator payload
      |> Result.map_error ~f:(fun error -> Failure error)
    in
    retained @ [ reminder ])
;;

let compact_entries ~inference ~allocator ~env ~history =
  let config = Config.load ?env () in
  compact_entries_configured
    ~config
    ~score:(fun prompt -> Relevance_judge.score_relevance ~inference config ~prompt)
    ~summarise:(S.summarise ~inference)
    ~allocator
    ~env
    ~history
;;

module For_testing = struct
  let process_current_entries = partition_history

  let compact_entries_configured ~config =
    compact_entries_configured ~config ~score:(fun _ -> 0.5)
  ;;

  let compact_entries_with ~summarise ~allocator ~env ~history =
    compact_entries_configured
      ~config:(Config.load ?env ())
      ~summarise
      ~allocator
      ~env
      ~history
  ;;

  let select_relevant = select_relevant
  let estimated_tokens = estimated_tokens
end
