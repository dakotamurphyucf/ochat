open! Core
module D = Document_schema
module F = Document_fields

let initialize_history_revision json =
  let open Result.Let_syntax in
  let%bind _ = F.required json "id" F.string in
  match json with
  | `Object fields ->
    Ok
      (if List.Assoc.mem fields "content_revision" ~equal:String.equal
       then json
       else `Object (fields @ [ "content_revision", `String "0" ]))
  | _ -> F.invalid "history" "entry must be an object"
;;

let initialize_history_revisions json =
  let open Result.Let_syntax in
  let%bind values = F.array json in
  let%map values = List.map values ~f:initialize_history_revision |> Result.all in
  `Array values
;;

let map_history_field json name f =
  let open Result.Let_syntax in
  let%bind value = F.required json name f in
  match json with
  | `Object fields ->
    Ok
      (`Object
          (List.map fields ~f:(fun (key, old) ->
             key, if String.equal key name then value else old)))
  | _ -> F.invalid name "must be an object"
;;

let initialize_history_window json =
  map_history_field json "entries" initialize_history_revisions
;;

let initialize_snapshot_history json =
  let open Result.Let_syntax in
  let%bind json = map_history_field json "canonical_history" initialize_history_window in
  let%bind json =
    map_history_field json "deferred_entries" initialize_history_revisions
  in
  match D.Json.field json ~name:"effective_history" with
  | Absent | Null -> Ok json
  | Value _ -> map_history_field json "effective_history" initialize_history_window
;;

let initialize_event_history json =
  let open Result.Let_syntax in
  let%bind kind = F.required json "kind" F.string in
  let%bind payload = F.required json "payload" Result.return in
  let%bind payload =
    match kind with
    | "history.message_deferred" -> initialize_history_revision payload
    | "history.appended" -> initialize_history_revisions payload
    | "history.replaced" -> initialize_history_window payload
    | "moderator.overlay_changed" ->
      (match D.Json.field payload ~name:"effective_history" with
       | Absent | Null -> Ok payload
       | Value _ ->
         map_history_field payload "effective_history" initialize_history_window)
    | "session.updated" ->
      (match D.Json.field payload ~name:"replacement_snapshot" with
       | Absent | Null -> Ok payload
       | Value _ ->
         map_history_field payload "replacement_snapshot" initialize_snapshot_history)
    | _ -> Ok payload
  in
  map_history_field json "payload" (fun _ -> Ok payload)
;;

let initialize_method_result json ~method_name =
  match method_name with
  | "session.get" -> initialize_snapshot_history json
  | "session.attach" ->
    map_history_field json "replay" (fun replay ->
      let open Result.Let_syntax in
      let%bind kind = F.required replay "type" F.string in
      match kind with
      | "snapshot" -> map_history_field replay "snapshot" initialize_snapshot_history
      | "events" ->
        map_history_field replay "events" (fun events ->
          let%bind events = F.array events in
          let%map events = Result.all (List.map events ~f:initialize_event_history) in
          `Array events)
      | _ -> Ok replay)
  | _ -> Ok json
;;
