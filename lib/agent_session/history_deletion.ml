open! Core
module P = Agent_protocol
module H = P.History

type t =
  { previous : Session_state.t
  ; canonical_history : P.History.entry list
  ; retired_ids : P.History.Id.t list
  ; initial_prompt_entry_count : int
  }

let prepare state ~history_id =
  let open Result.Let_syntax in
  let history = state.Session_state.conversation.canonical_history in
  let%bind canonical = History_codec.all_of_protocol history in
  let%bind retained =
    History_entry.remove_with_tool_pair canonical ~entry_id:history_id
    |> Result.map_error ~f:P.Error.invalid_request
  in
  let ids =
    Hash_set.of_list (module P.History.Id) (List.map retained ~f:History_entry.id)
  in
  let keep entry = Hash_set.mem ids entry.P.History.id in
  let canonical_history = List.filter history ~f:keep in
  let%bind remaining = History_codec.all_of_protocol canonical_history in
  let%map () =
    History_entry.validate_relations remaining
    |> Result.map_error ~f:P.Error.invalid_request
  in
  { previous = state
  ; canonical_history
  ; retired_ids =
      List.filter_map history ~f:(fun entry -> Option.some_if (not (keep entry)) entry.id)
  ; initial_prompt_entry_count =
      List.take history state.conversation.initial_prompt_entry_count
      |> List.count ~f:keep
  }
;;

let canonical_history t = t.canonical_history
let retired_ids t = t.retired_ids
let initial_prompt_entry_count t = t.initial_prompt_entry_count

let validate_basis t state =
  let previous = t.previous.Session_state.conversation in
  let current = state.Session_state.conversation in
  if
    P.Id.Session.equal t.previous.identity.session_id state.identity.session_id
    && Int.equal t.previous.identity.generation state.identity.generation
    && Int64.equal t.previous.counters.revision state.counters.revision
    && List.equal H.equal_entry previous.canonical_history current.canonical_history
    && List.equal
         H.equal_entry
         previous.deferred_user_entries
         current.deferred_user_entries
    && Int.equal previous.initial_prompt_entry_count current.initial_prompt_entry_count
    && Int64.equal previous.next_history_sequence current.next_history_sequence
    && Int64.equal previous.reserved_history_through current.reserved_history_through
    && Option.equal Jsonaf.exactly_equal t.previous.moderator state.moderator
  then Ok ()
  else
    Error
      (P.Error.create
         Conflict
         ~message:"history deletion basis changed"
         ~retryable:false
         ())
;;
