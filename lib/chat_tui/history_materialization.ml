open! Core

let create_entry ~allocator converted =
  match converted.Chat_response.Converter.history_id with
  | Some id -> Ok (History_entry.create_with_id ~id converted.item)
  | None -> History_entry.create ~allocator converted.item
;;

let prepare_allocator allocator converted =
  let namespace = History_entry.Allocator.namespace allocator in
  let next_sequence = History_entry.Allocator.next_sequence allocator in
  let required_next_sequence =
    List.fold converted ~init:next_sequence ~f:(fun next converted ->
      match converted.Chat_response.Converter.history_id with
      | Some id when String.equal namespace (History_entry.Id.namespace id) ->
        if Int.equal (History_entry.Id.sequence id) Int.max_value
        then next
        else Int.max next (History_entry.Id.sequence id + 1)
      | None | Some _ -> next)
  in
  let has_exhausted_id =
    List.exists converted ~f:(fun converted ->
      match converted.Chat_response.Converter.history_id with
      | Some id ->
        String.equal namespace (History_entry.Id.namespace id)
        && Int.equal (History_entry.Id.sequence id) Int.max_value
      | None -> false)
  in
  if has_exhausted_id
  then Error "explicit history ID exhausts the allocator namespace"
  else
    History_entry.Allocator.reserve
      allocator
      ~count:(required_next_sequence - next_sequence)
    |> Result.map ~f:(fun (_ : History_entry.Id.t list) -> ())
;;

let validate_explicit_ids converted =
  let seen = Hashtbl.create (module History_entry.Id) in
  List.fold_result converted ~init:() ~f:(fun () converted ->
    match converted.Chat_response.Converter.history_id with
    | None -> Ok ()
    | Some id ->
      (match Hashtbl.find seen id with
       | None ->
         Hashtbl.set seen ~key:id ~data:(converted.source_index, converted.source_context);
         Ok ()
       | Some (first_index, first_context) ->
         Error
           (sprintf
              "duplicate ochat-history-id %s at %s (expanded message %d) and %s \
               (expanded message %d)"
              (History_entry.Id.to_string id)
              (Option.value first_context ~default:"<unknown source>")
              first_index
              (Option.value converted.source_context ~default:"<unknown source>")
              converted.source_index)))
;;

let from_prompt ~allocator ~ctx ~run_agent elements =
  let open Result.Let_syntax in
  let converted =
    Chat_response.Converter.to_identity_bearing_items ~ctx ~run_agent elements
  in
  let%bind () = validate_explicit_ids converted in
  let%bind () = prepare_allocator allocator converted in
  let%bind entries = List.map converted ~f:(create_entry ~allocator) |> Result.all in
  let%map () = History_entry.validate ~allocator entries in
  entries
;;

let resume_or_materialize
      ~(session : Session.V4.t option)
      ~allocator
      ~ctx
      ~run_agent
      elements
  =
  match session with
  | Some session when not (List.is_empty session.history) -> Ok session.history
  | None | Some _ -> from_prompt ~allocator ~ctx ~run_agent elements
;;
