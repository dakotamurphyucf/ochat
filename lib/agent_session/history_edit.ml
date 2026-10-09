open! Core
module P = Agent_protocol
module H = P.History
module Payload = History_entry.Payload

type t =
  { previous : Session_state.t
  ; edited_entry : H.entry
  ; retired_ids : H.Id.t list
  ; canonical_history : H.entry list
  ; initial_prompt_entry_count : int
  }

let failure code message = P.Error.create code ~message ~retryable:false ()

let unsupported id reason =
  P.Error.create
    Invalid_request
    ~message:"history target does not support text editing"
    ~retryable:false
    ~data:
      (`Object
          [ "reason", P.History_edit.Unsupported_target.to_json reason
          ; "history_id", H.Id.to_json id
          ])
    ()
;;

let overlay_allows state id =
  let open Result.Let_syntax in
  let%bind snapshot = Moderator_checkpoint.decode state.Session_state.moderator in
  match snapshot with
  | None -> Ok ()
  | Some snapshot ->
    if
      List.exists
        snapshot.replacements
        ~f:(fun (replacement : Session.Moderator_state.Identity_snapshot.Replacement.t) ->
          H.Id.equal replacement.target_id id)
      || List.exists
           snapshot.tombstones
           ~f:(fun (tombstone : Session.Moderator_state.Identity_snapshot.Tombstone.t) ->
             H.Id.equal tombstone.target_id id)
    then Error (unsupported id P.History_edit.Unsupported_target.Overlay_override)
    else Ok ()
;;

let plain_user entry =
  let open Result.Let_syntax in
  let%bind canonical = History_codec.of_protocol entry in
  let supported =
    match
      ( entry.H.provenance
      , Payload.Semantic.view (Payload.semantic (History_entry.payload canonical)) )
    with
    | Canonical, Message { form = Input; role = User; content; phase = Absent } ->
      (not (List.is_empty content))
      && List.for_all content ~f:(function
        | Payload.Content.Text { annotations = []; logprobs = Absent; _ } -> true
        | Text _ | Refusal _ | Image _ | Unknown _ -> false)
    | ( ( Canonical
        | Moderator_inserted
        | Moderator_replaced _
        | Runtime_notification _
        | Runtime_authoring _ )
      , _ ) -> false
  in
  if supported
  then Ok ()
  else Error (unsupported entry.id P.History_edit.Unsupported_target.Not_plain_user_text)
;;

let validate_pair_boundary state ~prefix_length ~target_id =
  let open Result.Let_syntax in
  let%bind entries =
    History_codec.all_of_protocol state.Session_state.conversation.canonical_history
  in
  let%bind () =
    History_entry.validate_relations entries
    |> Result.map_error ~f:P.Error.invalid_request
  in
  let prefix_ids = Hash_set.create (module H.Id) in
  List.take entries prefix_length
  |> List.iter ~f:(fun entry -> Hash_set.add prefix_ids (History_entry.id entry));
  let function_calls = Hashtbl.create (module String) in
  let custom_calls = Hashtbl.create (module String) in
  let calls = function
    | Payload.Call_kind.Function -> function_calls
    | Custom -> custom_calls
  in
  let crossing () =
    Error
      (unsupported target_id P.History_edit.Unsupported_target.Tool_pair_crosses_boundary)
  in
  List.foldi entries ~init:(Ok ()) ~f:(fun index result entry ->
    let%bind () = result in
    let semantic = Payload.semantic (History_entry.payload entry) in
    let call_id = (Payload.Semantic.metadata semantic).call_id in
    match Payload.Semantic.view semantic with
    | Call { kind; _ } ->
      (match call_id with
       | Value call_id ->
         Hashtbl.set (calls kind) ~key:call_id ~data:(index < prefix_length)
       | Absent | Null -> ());
      Ok ()
    | Result { relation = Bound id; _ }
      when index > prefix_length && Hash_set.mem prefix_ids id -> crossing ()
    | Result { relation = Unresolved; kind; _ } when index > prefix_length ->
      (match call_id with
       | Value call_id
         when Option.value (Hashtbl.find (calls kind) call_id) ~default:false ->
         crossing ()
       | Value _ | Absent | Null -> Ok ())
    | Message _ | Result _ | Reasoning _ | Unknown _ -> Ok ())
;;

let prepare state ~edit =
  let open Result.Let_syntax in
  let id = P.History_edit.history_id edit in
  let rec locate prefix = function
    | [] -> Error (failure Invalid_request "history target is not current")
    | entry :: suffix ->
      if H.Id.equal entry.H.id id
      then Ok (List.rev prefix, entry, suffix)
      else locate (entry :: prefix) suffix
  in
  let%bind prefix, entry, suffix =
    locate [] state.Session_state.conversation.canonical_history
  in
  let%bind () =
    if
      H.Content_revision.equal
        entry.content_revision
        (P.History_edit.expected_content_revision edit)
    then Ok ()
    else Error (failure Conflict "history content revision does not match")
  in
  let%bind () =
    if List.length prefix < state.conversation.initial_prompt_entry_count
    then Error (unsupported id P.History_edit.Unsupported_target.Initial_instruction)
    else Ok ()
  in
  let%bind () = plain_user entry in
  let%bind () = overlay_allows state id in
  let%bind () =
    validate_pair_boundary state ~prefix_length:(List.length prefix) ~target_id:id
  in
  let%bind content_revision = H.Content_revision.succ entry.content_revision in
  let replacement =
    History_codec.user_text ~id (P.History_edit.text edit) |> History_codec.to_protocol
  in
  let edited_entry =
    { replacement with content_revision; provenance = entry.provenance }
  in
  let canonical_history = prefix @ [ edited_entry ] in
  let%bind canonical = History_codec.all_of_protocol canonical_history in
  let%bind () =
    History_entry.validate_relations canonical
    |> Result.map_error ~f:P.Error.invalid_request
  in
  Ok
    { previous = state
    ; edited_entry
    ; retired_ids = List.map suffix ~f:(fun item -> item.H.id)
    ; canonical_history
    ; initial_prompt_entry_count =
        Int.min
          state.conversation.initial_prompt_entry_count
          (List.length canonical_history)
    }
;;

let edited_entry t = t.edited_entry
let retired_ids t = t.retired_ids
let canonical_history t = t.canonical_history
let initial_prompt_entry_count t = t.initial_prompt_entry_count

let validate_basis t state =
  let previous = t.previous.Session_state.conversation in
  let current = state.Session_state.conversation in
  if
    P.Id.Session.equal t.previous.identity.session_id state.identity.session_id
    && Int.equal t.previous.identity.generation state.identity.generation
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
  else Error (failure Conflict "history edit basis changed")
;;
