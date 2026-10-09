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

let ordered ?(additional_binding = []) t principal command request encode values =
  let open Result.Let_syntax in
  let authority =
    sign t (Jsonaf.to_string (Agent_protocol.Principal.to_json principal))
  in
  let query = sign t (Jsonaf.to_string (query command)) in
  let encoded = `Array (List.map values ~f:encode) in
  let encoded =
    if List.is_empty additional_binding
    then encoded
    else `Object (("items", encoded) :: additional_binding)
  in
  let data = sign t (Jsonaf.to_string encoded) in
  let%bind offset =
    match request.Agent_protocol.Page.Request.cursor with
    | None -> Ok 0
    | Some cursor ->
      (match Base64.decode (Agent_protocol.Page.Cursor.to_string cursor) with
       | Error _ -> Error (invalid ())
       | Ok text ->
         (match String.split text ~on:':' with
          | [ "catalog"; actual_authority; actual_query; actual_data; offset; signature ]
            ->
            let unsigned =
              String.concat
                ~sep:":"
                [ "catalog"; actual_authority; actual_query; actual_data; offset ]
            in
            if
              (not (String.equal signature (sign t unsigned)))
              || (not (String.equal actual_authority authority))
              || not (String.equal actual_query query)
            then Error (invalid ())
            else if not (String.equal actual_data data)
            then
              Error
                (Agent_protocol.Error.create
                   Conflict
                   ~message:"catalog changed; refresh required"
                   ~data:(`Object [ "refresh_required", `True ])
                   ~retryable:false
                   ())
            else (
              match Int.of_string_opt offset with
              | Some offset when offset >= 0 -> Ok offset
              | _ -> Error (invalid ()))
          | _ -> Error (invalid ())))
  in
  if offset > List.length values
  then Error (invalid ())
  else (
    let items = List.take (List.drop values offset) request.limit in
    let next = offset + List.length items in
    let%map next_cursor =
      if next >= List.length values
      then Ok None
      else (
        let unsigned =
          String.concat ~sep:":" [ "catalog"; authority; query; data; Int.to_string next ]
        in
        Agent_protocol.Page.Cursor.of_string
          (Base64.encode_exn (unsigned ^ ":" ^ sign t unsigned))
        |> Result.map ~f:Option.some)
    in
    Agent_protocol.Page.{ items; next_cursor })
;;

let activity
      t
      principal
      (request : Agent_protocol.Activity_query.t)
      ~organization_revision
      values
  =
  ordered
    t
    principal
    (Agent_protocol.Command.Activity_list request)
    request.catalog.page
    Agent_protocol.Session_activity.to_json
    values
    ~additional_binding:
      [ "organization_revision", `String (Int64.to_string organization_revision) ]
;;

let work t principal (request : Agent_protocol.Session_work.Query.t) values =
  ordered
    t
    principal
    (Agent_protocol.Command.Session_work request)
    request.page
    Agent_protocol.Session_work.to_json
    values
;;

let session_catalog t principal request ~host_id ~organization_revision values =
  ordered
    t
    principal
    (Agent_protocol.Command.Session_list request)
    request.Agent_protocol.Session.List_request.page
    Agent_protocol.Session_catalog.to_json
    values
    ~additional_binding:
      [ "organization_host_id", Agent_protocol.Id.Server.to_json host_id
      ; "organization_revision", `String (Int64.to_string organization_revision)
      ]
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
  | Project_list r, Project_list p ->
    let%map p =
      ordered
        t
        principal
        command
        r.page
        Agent_protocol.Organization_group.Project.to_json
        p.items
    in
    Agent_protocol.Method_result.Project_list p
  | Collection_list r, Collection_list p ->
    let%map p =
      ordered
        t
        principal
        command
        r.page
        Agent_protocol.Organization_group.Collection.to_json
        p.items
    in
    Agent_protocol.Method_result.Collection_list p
  | Activity_list _, Activity_list p -> Ok (Agent_protocol.Method_result.Activity_list p)
  | Session_work _, Session_work p -> Ok (Agent_protocol.Method_result.Session_work p)
  | Session_list _, Session_list p -> Ok (Agent_protocol.Method_result.Session_list p)
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

module Inference = struct
  module P = Agent_protocol

  type binding =
    { authority : string
    ; query : string
    ; generation : int
    ; revision : int64
    }

  let hash json =
    Jsonaf.to_string json |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
  ;;

  let binding
        ~principal
        ~(request : P.Inference_query.Request.t)
        ~generation
        ~accounting_revision
    =
    let open Result.Let_syntax in
    let%bind _ =
      P.Inference_query.Request.create
        ~session_id:request.session_id
        ~page:request.page
        ~include_configuration:request.include_configuration
        ~include_diagnostics:request.include_diagnostics
    in
    if generation < 0 || Int64.(accounting_revision < zero)
    then Error (P.Error.invalid_request "inference cursor counters must be nonnegative")
    else
      Ok
        { authority =
            hash
              (`Array
                  [ P.Id.Principal.to_json principal.P.Principal.id
                  ; P.Scope.set_to_json principal.scopes
                  ])
        ; query =
            hash
              (`Array
                  [ P.Id.Session.to_json request.session_id
                  ; `Number (Int.to_string request.page.limit)
                  ; (if request.include_configuration then `True else `False)
                  ; (if request.include_diagnostics then `True else `False)
                  ; `String "admission_ordinal_ascending.v1"
                  ])
        ; generation
        ; revision = accounting_revision
        }
  ;;

  let expired () =
    P.Error.create
      Cursor_expired
      ~message:"inference cursor is invalid or expired"
      ~retryable:false
      ()
  ;;

  let changed () =
    P.Error.create
      Conflict
      ~message:"inference accounting changed; restart pagination"
      ~retryable:false
      ~data:(`Object [ "restart_required", `True ])
      ()
  ;;

  let same_signature a b =
    if String.length a <> String.length b
    then false
    else (
      let difference = ref 0 in
      for i = 0 to String.length a - 1 do
        difference := !difference lor (Char.to_int a.[i] lxor Char.to_int b.[i])
      done;
      Int.equal !difference 0)
  ;;

  let claims binding after_ordinal =
    String.concat
      ~sep:":"
      [ "inference.v1"
      ; binding.authority
      ; binding.query
      ; Int.to_string binding.generation
      ; Int64.to_string binding.revision
      ; Int64.to_string after_ordinal
      ]
  ;;

  let cursor t binding ~after_ordinal =
    if Int64.(after_ordinal < zero)
    then Error (P.Error.invalid_request "inference cursor ordinal must be nonnegative")
    else (
      let text = claims binding after_ordinal in
      (* Fixed-size digests and bounded decimal counters fit well below 2048.
         Check before Base64 allocates the externally retained cursor. *)
      if String.length text + 65 > 1536
      then Error (expired ())
      else P.Page.Cursor.of_string (Base64.encode_exn (text ^ ":" ^ sign t text)))
  ;;

  let after t binding = function
    | None -> Ok Int64.zero
    | Some cursor ->
      let encoded = P.Page.Cursor.to_string cursor in
      if String.length encoded > 2048
      then Error (expired ())
      else (
        match Base64.decode encoded with
        | Error _ -> Error (expired ())
        | Ok raw ->
          (match String.split raw ~on:':' with
           | [ version; authority; query; generation; revision; ordinal; signature ] ->
             let text =
               String.concat
                 ~sep:":"
                 [ version; authority; query; generation; revision; ordinal ]
             in
             if not (same_signature signature (sign t text))
             then Error (expired ())
             else if
               not
                 (String.equal version "inference.v1"
                  && String.equal authority binding.authority
                  && String.equal query binding.query)
             then Error (expired ())
             else (
               match
                 ( Int.of_string_opt generation
                 , Int64.of_string_opt revision
                 , Int64.of_string_opt ordinal )
               with
               | Some generation, Some revision, Some ordinal
                 when generation >= 0 && Int64.(revision >= zero && ordinal >= zero) ->
                 if
                   not
                     (Int.equal generation binding.generation
                      && Int64.equal revision binding.revision)
                 then Error (changed ())
                 else Ok ordinal
               | _ -> Error (expired ()))
           | _ -> Error (expired ())))
  ;;
end
