open! Core

type t = { secret : string }

let create () = { secret = Agent_protocol.Id.Transaction.(to_string (create ())) }

let invalid () =
  Agent_protocol.Error.invalid_request "pagination cursor is invalid or expired"
;;

let sign t text = Digestif.SHA256.(hmac_string ~key:t.secret text |> to_hex)

let binding t principal query items =
  sign
    t
    (Jsonaf.to_string
       (`Array [ Agent_protocol.Principal.to_json principal; query; `Array items ]))
;;

let cursor t binding offset =
  let text = binding ^ ":" ^ Int.to_string offset in
  Agent_protocol.Page.Cursor.of_string (Base64.encode_exn (text ^ ":" ^ sign t text))
;;

let offset t binding = function
  | None -> Ok 0
  | Some cursor ->
    let decoded = Base64.decode (Agent_protocol.Page.Cursor.to_string cursor) in
    (match decoded with
     | Ok value ->
       (match String.split value ~on:':' with
        | [ actual; offset; signature ]
          when String.equal actual binding
               && String.equal signature (sign t (actual ^ ":" ^ offset)) ->
          (match Int.of_string_opt offset with
           | Some value when value >= 0 -> Ok value
           | _ -> Error (invalid ()))
        | _ -> Error (invalid ()))
     | Error _ -> Error (invalid ()))
;;

let query command =
  let params =
    match Agent_protocol.Command.params command with
    | `Object fields ->
      `Object (List.filter fields ~f:(fun (name, _) -> not (String.equal name "cursor")))
    | json -> json
  in
  `Array [ `String (Agent_protocol.Command.method_name command); params ]
;;

let page t principal command request encode values =
  let open Result.Let_syntax in
  let values =
    List.sort values ~compare:(fun a b ->
      String.compare (Jsonaf.to_string (encode a)) (Jsonaf.to_string (encode b)))
  in
  let binding = binding t principal (query command) (List.map values ~f:encode) in
  let%bind offset = offset t binding request.Agent_protocol.Page.Request.cursor in
  if offset > List.length values
  then Error (invalid ())
  else (
    let items = List.take (List.drop values offset) request.limit in
    let next = offset + List.length items in
    let%map next_cursor =
      if next < List.length values
      then Result.map (cursor t binding next) ~f:Option.some
      else Ok None
    in
    Agent_protocol.Page.{ items; next_cursor })
;;

let lists t principal command result =
  let open Result.Let_syntax in
  match command, result with
  | Agent_protocol.Command.Prompt_list r, Agent_protocol.Method_result.Prompt_list p ->
    let%map p = page t principal command r.page Agent_protocol.Prompt.to_json p.items in
    Agent_protocol.Method_result.Prompt_list p
  | Workspace_list r, Workspace_list p ->
    let%map p =
      page t principal command r.page Agent_protocol.Workspace.to_json p.items
    in
    Agent_protocol.Method_result.Workspace_list p
  | Session_list r, Session_list p ->
    let%map p = page t principal command r.page Agent_protocol.Session.to_json p.items in
    Agent_protocol.Method_result.Session_list p
  | Permission_list r, Permission_list p ->
    let%map p =
      page t principal command r.page Agent_protocol.Permission.to_json p.items
    in
    Agent_protocol.Method_result.Permission_list p
  | Grant_list r, Grant_list p ->
    let%map p = page t principal command r.page Agent_protocol.Grant.to_json p.items in
    Agent_protocol.Method_result.Grant_list p
  | Job_list r, Job_list p ->
    let%map p = page t principal command r.page Agent_protocol.Job.to_json p.items in
    Agent_protocol.Method_result.Job_list p
  | Schedule_list r, Schedule_list p ->
    let%map p = page t principal command r.page Agent_protocol.Schedule.to_json p.items in
    Agent_protocol.Method_result.Schedule_list p
  | _ -> Ok result
;;

let anchor entries id =
  List.find_mapi entries ~f:(fun i entry ->
    Option.some_if (History_entry.Id.equal entry.Agent_protocol.History.id id) i)
  |> Result.of_option
       ~error:(Agent_protocol.Error.invalid_request "history anchor was not found")
;;

let history_start t binding entries request =
  match request.Agent_protocol.History.Window_request.position with
  | Tail count -> Ok (Int.max 0 (List.length entries - Int.min count request.limit))
  | After id -> Result.map (anchor entries id) ~f:(fun i -> i + 1)
  | Before id ->
    Result.map (anchor entries id) ~f:(fun i -> Int.max 0 (i - request.limit))
  | Cursor value -> offset t binding (Some value)
;;

let history_window t principal session_id request window =
  let open Result.Let_syntax in
  let entries = window.Agent_protocol.History.Window.entries in
  let query =
    `Array
      [ Agent_protocol.Id.Session.to_json session_id
      ; `String "history"
      ; `String (Bool.to_string request.Agent_protocol.History.Window_request.effective)
      ; `Number (Int.to_string request.limit)
      ]
  in
  let binding =
    binding t principal query (List.map entries ~f:Agent_protocol.History.entry_to_json)
  in
  let%bind start = history_start t binding entries request in
  let%bind stop =
    match request.position with
    | Before id -> anchor entries id
    | _ -> Ok (Int.min (List.length entries) (start + request.limit))
  in
  if start > List.length entries
  then Error (invalid ())
  else (
    let selected = List.take (List.drop entries start) (stop - start) in
    let%bind previous_cursor =
      if start = 0
      then Ok None
      else
        Result.map (cursor t binding (Int.max 0 (start - request.limit))) ~f:Option.some
    in
    let%map next_cursor =
      if stop = List.length entries
      then Ok None
      else Result.map (cursor t binding stop) ~f:Option.some
    in
    Agent_protocol.History.Window.
      { entries = selected
      ; previous_cursor
      ; next_cursor
      ; reached_start = start = 0
      ; reached_end = stop = List.length entries
      ; structurally_complete =
          window.structurally_complete && start = 0 && stop = List.length entries
      })
;;

let history t principal request snapshot =
  match request.Agent_protocol.Session.Get_request.history with
  | None -> Ok snapshot
  | Some window_request ->
    let source =
      if window_request.effective
      then
        Option.value
          snapshot.Agent_protocol.Snapshot.effective_history
          ~default:snapshot.canonical_history
      else snapshot.canonical_history
    in
    Result.map
      (history_window t principal request.session_id window_request source)
      ~f:(fun window ->
        if window_request.effective
        then
          { snapshot with
            canonical_history =
              { snapshot.canonical_history with
                entries = []
              ; reached_start = false
              ; reached_end = false
              ; structurally_complete = false
              }
          ; effective_history = Some window
          }
        else { snapshot with canonical_history = window; effective_history = None })
;;
