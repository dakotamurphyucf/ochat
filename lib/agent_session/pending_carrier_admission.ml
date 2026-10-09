open! Core
module D = Document_schema
module F = Agent_store.Document_fields

let admit values ~raw ~decode ~known ~limits ~name =
  let open Result.Let_syntax in
  let invalid reason =
    D.Error.Invalid_field { path = [ "conversation"; name ]; reason }
  in
  let%bind admitted = List.map raw ~f:(fun json -> decode json ~limits) |> Result.all in
  let%bind () =
    match
      List.map2 values admitted ~f:(fun projected admitted ->
        let%bind projected = known projected ~limits in
        let%bind admitted = known admitted ~limits in
        if Jsonaf.exactly_equal projected admitted
        then Ok ()
        else Error (invalid "original private carrier differs from known projection"))
    with
    | Unequal_lengths -> Error (invalid "original private carrier count differs")
    | Ok checks -> Result.all_unit checks
  in
  Ok admitted
;;

let rehydrate_transaction_prefix (state : Session_state.t) ~document ~limits =
  let open Result.Let_syntax in
  let%bind () = D.Document.validate document ~limits in
  let%bind conversation =
    F.required (D.Document.payload document) "conversation" Result.return
  in
  let%bind raw_pending = F.required conversation "deferred_user_entries" F.array in
  let%bind raw_dispositions = F.required conversation "pending_dispositions" F.array in
  let%bind deferred_user_entries =
    admit
      state.conversation.deferred_user_entries
      ~raw:raw_pending
      ~decode:Pending_input_document.of_jsonaf
      ~known:Pending_input_document.known_jsonaf
      ~limits
      ~name:"deferred_user_entries"
  in
  let%bind pending_dispositions =
    admit
      state.conversation.pending_dispositions
      ~raw:raw_dispositions
      ~decode:Pending_disposition_document.of_jsonaf
      ~known:Pending_disposition_document.known_jsonaf
      ~limits
      ~name:"pending_dispositions"
  in
  let state =
    { state with
      conversation =
        { state.conversation with deferred_user_entries; pending_dispositions }
    }
  in
  Ok state
;;

let rehydrate state ~document ~limits =
  let open Result.Let_syntax in
  let%bind state = rehydrate_transaction_prefix state ~document ~limits in
  let%map () = Session_state.validate state |> Persistence_codec.document_result in
  state
;;
