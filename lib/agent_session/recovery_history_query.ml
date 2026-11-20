open! Core
module P = Agent_protocol
module Text = Recovery_item_text
module Source = Recovery_source_reader
module Work = Recovery_source_work
module Basis = Recovery_history_query_basis
module Digest = Chatml_source.Digest
module X = Persistence_codec
module Citation = P.Recovery_window.Citation
module Scope = P.Recovery_window.Scope

type family = Basis.family =
  | Canonical
  | Effective

type scope = Basis.scope =
  | Current_generation
  | Window of Scope.t

type action = P.Recovery_history_request.action =
  | List_items
  | Search of P.Search_term.t

let max_rows = P.Recovery_query.max_page_size
let max_positions = 256
let max_encoded_bytes = P.Recovery_query.max_page_bytes
let max_cursor_bytes = 2048
let max_members = Recovery_window_record.max_members
let invalid reason = Error (P.Error.invalid_request reason)
let role_name = P.Recovery_history_request.role_name
let view_name = P.Recovery_history_request.view_name

type t =
  { source : P.Session_ref.t
  ; scope : scope
  ; family : family
  ; view : Text.view
  ; role : Text.role option
  ; ordinals : (int * int) option
  ; limit : int
  ; max_reply_bytes : int
  ; action : action
  ; digest : Digest.t
  }

let source t = t.source
let scope t = t.scope
let family t = t.family
let view t = t.view
let action t = t.action
let digest t = t.digest

let wire_limits max_bytes =
  P.Recovery_wire.limits ~max_bytes ~max_fields:2048 ~max_nodes:65_536
;;

let of_request (request : P.Recovery_history_request.t) =
  let { P.Recovery_history_request.session = source
      ; window
      ; family
      ; view
      ; role
      ; ordinals
      ; page
      ; max_reply_bytes
      ; action
      }
    =
    request
  in
  let scope =
    Option.value_map window ~default:Current_generation ~f:(fun w -> Window w)
  in
  let limit = page.limit in
  let query =
    `Array
      [ `String "local_history_query.v1"
      ; P.Session_ref.to_json source
      ; (match scope with
         | Current_generation -> `Null
         | Window scope -> Scope.to_json scope)
      ; `String
          (match family with
           | Canonical -> "canonical"
           | Effective -> "effective")
      ; `String (view_name view)
      ; X.option_json (fun role -> `String (role_name role)) role
      ; X.option_json
          (fun (lo, hi) -> `Array [ X.integer_json lo; X.integer_json hi ])
          ordinals
      ; X.integer_json limit
      ; X.integer_json max_reply_bytes
      ; (match action with
         | List_items -> `Null
         | Search term -> P.Search_term.to_json term)
      ]
  in
  let%map.Result _ =
    P.Recovery_wire.measure ~limits:(wire_limits max_encoded_bytes) query
  in
  let digest = Digest.of_contents (Jsonaf.to_string query) ~checkpoint:(fun () -> ()) in
  { source; scope; family; view; role; ordinals; limit; max_reply_bytes; action; digest }
;;

let create
      ~source
      ~scope
      ~family
      ~view
      ?role
      ?ordinals
      ?(limit = max_rows)
      ?(max_reply_bytes = max_encoded_bytes)
      action
  =
  let open Result.Let_syntax in
  let%bind page = P.Page.Request.create ~limit () in
  let window =
    match scope with
    | Current_generation -> None
    | Window scope -> Some scope
  in
  let%bind request =
    P.Recovery_history_request.create
      ~session:source
      ?window
      ~family
      ~view
      ?role
      ?ordinals
      ~page
      ~max_reply_bytes
      action
  in
  of_request request
;;

module Continuation = struct
  type highwater = Basis.Highwater.t =
    { windows : int
    ; last_window_members : int
    ; last_preparation_revision : int64 option
    }

  type t =
    { query : Digest.t
    ; basis : Digest.t
    ; highwater : highwater
    ; next_position : int
    }

  let to_json t =
    `Object
      [ "query", `String (Digest.to_hex t.query)
      ; "basis", `String (Digest.to_hex t.basis)
      ; "windows", X.integer_json t.highwater.windows
      ; "last_window_members", X.integer_json t.highwater.last_window_members
      ; ( "last_preparation_revision"
        , X.option_json X.int64_json t.highwater.last_preparation_revision )
      ; "next_position", X.integer_json t.next_position
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind _ = P.Recovery_wire.measure ~limits:(wire_limits max_cursor_bytes) json in
    let%bind fields =
      P.Recovery_wire.fields
        json
        [ "query"
        ; "basis"
        ; "windows"
        ; "last_window_members"
        ; "last_preparation_revision"
        ; "next_position"
        ]
    in
    let digest = function
      | `String value ->
        Digest.of_hex value
        |> Result.map_error ~f:(fun _ ->
          P.Error.invalid_request "invalid local recovery cursor digest")
      | _ -> invalid "invalid local recovery cursor digest"
    in
    let%bind query = X.required fields "query" digest in
    let%bind basis = X.required fields "basis" digest in
    let%bind windows = X.required fields "windows" X.integer in
    let%bind last_window_members = X.required fields "last_window_members" X.integer in
    let%bind last_preparation_revision =
      X.required fields "last_preparation_revision" (X.nullable X.nonnegative_int64)
    in
    let%bind next_position = X.required fields "next_position" X.integer in
    if
      windows <= 0
      || windows > 64
      || last_window_members < 0
      || last_window_members > max_members
      || next_position <= 0
      || next_position >= max_members
    then invalid "invalid local recovery cursor position"
    else
      Ok
        { query
        ; basis
        ; highwater = { windows; last_window_members; last_preparation_revision }
        ; next_position
        }
  ;;
end

type error =
  | Source of Source.error
  | Refresh_required
  | Reply_limit
  | Invalid_cursor

type truncation =
  | Row_limit
  | Position_limit
  | Work_limit
  | Reply_bytes

let selection_json = function
  | Text.Canonical citation -> [ "citation", Citation.to_json citation ]
  | Prepared { citation; source_revision } ->
    [ "citation", Citation.to_json citation
    ; "preparation_revision", X.int64_json source_revision
    ]
;;

let item_fields (candidate : Basis.candidate) item =
  selection_json candidate.selection
  @ [ "ordinal", X.integer_json candidate.ordinal
    ; "role", `String (role_name (Text.role item))
    ]
  @
  match candidate.attempt with
  | None -> []
  | Some attempt -> [ "attempt", Basis.attempt_evidence attempt ]
;;

type projection =
  | List_parts
  | Match of P.History_text.Literal.t

let project ~checkpoint projection candidate item =
  let fields extra = Some (`Object (item_fields candidate item @ extra)) in
  let parts = Text.parts item in
  match projection with
  | List_parts ->
    Ok
      (fields
         [ ( "text_parts"
           , `Array (List.map parts ~f:(fun part -> X.integer_json part.index)) )
         ])
  | Match term ->
    let rec find = function
      | [] -> Ok None
      | (part : P.History_text.t) :: rest ->
        checkpoint ();
        let open Result.Let_syntax in
        let%bind match_ =
          P.History_text.Literal.find term part
          |> Result.map_error ~f:(fun _ -> Source Source.Corrupt)
        in
        (match match_ with
         | None -> find rest
         | Some hit ->
           Ok
             (fields
                [ "part", X.integer_json part.index
                ; "start", X.integer_json hit.start
                ; "stop", X.integer_json hit.stop
                ; "snippet", P.Search_snippet.to_json hit.snippet
                ]))
    in
    find parts
;;

type row =
  { json : Jsonaf.t
  ; position : int
  ; selection : Text.selection
  }

type page =
  { reversed : row list
  ; count : int
  ; bytes : int
  ; next : int
  ; reason : truncation option
  }

let render t basis page ~sign =
  let open Result.Let_syntax in
  let%bind next =
    match page.reason with
    | None -> Ok `Null
    | Some _ ->
      let continuation : Continuation.t =
        { query = t.digest
        ; basis = Basis.digest basis
        ; highwater = Basis.highwater basis
        ; next_position = page.next
        }
      in
      let%bind token =
        sign continuation |> Result.map_error ~f:(fun _ -> Source Source.Denied)
      in
      if String.is_empty token || String.length token > max_cursor_bytes
      then Error Reply_limit
      else Ok (`String token)
  in
  let coverage =
    match page.reason with
    | None when Basis.has_unretained_sources basis -> `String "source_unavailable"
    | None -> `String "complete"
    | Some Row_limit -> `String "row_limit"
    | Some Position_limit -> `String "position_limit"
    | Some Work_limit -> `String "work_limit"
    | Some Reply_bytes -> `String "reply_limit"
  in
  let json =
    `Object
      [ "rows", `Array (List.rev_map page.reversed ~f:(fun row -> row.json))
      ; "next", next
      ; "coverage", coverage
      ]
  in
  let limits = wire_limits t.max_reply_bytes in
  let%bind _ =
    P.Recovery_wire.measure ~limits json |> Result.map_error ~f:(fun _ -> Reply_limit)
  in
  if String.length (Jsonaf.to_string json) > t.max_reply_bytes
  then Error Reply_limit
  else Ok json
;;

let rec render_fitting t basis page ~start ~sign =
  if page.next <= start && Option.is_some page.reason
  then Error Reply_limit
  else (
    match render t basis page ~sign with
    | Ok json -> Ok (json, page.reversed)
    | Error Reply_limit ->
      (match page.reversed with
       | [] -> Error Reply_limit
       | row :: reversed ->
         let page =
           { page with reversed; next = row.position; reason = Some Reply_bytes }
         in
         render_fitting t basis page ~start ~sign)
    | Error _ as error -> error)
;;

let visit t basis ~start ~checkpoint ~read =
  let projection =
    match t.action with
    | List_items -> List_parts
    | Search term -> Match (P.History_text.Literal.create term)
  in
  let length = Basis.length basis in
  let limits = wire_limits t.max_reply_bytes in
  let rec loop page =
    let position = page.next in
    if position >= length
    then Ok page
    else if page.count >= t.limit
    then Ok { page with reason = Some Row_limit }
    else if position - start >= max_positions
    then Ok { page with reason = Some Position_limit }
    else (
      checkpoint ();
      let candidate = Basis.at basis position in
      let advance = { page with next = position + 1 } in
      let selected =
        candidate.is_selected
        &&
        match projection with
        | List_parts -> true
        | Match _ -> Basis.is_earliest basis candidate ~position
      in
      if not selected
      then loop advance
      else (
        match read candidate.selection with
        | Error Source.Budget_exhausted when position > start ->
          Ok { page with reason = Some Work_limit }
        | Error Source.Budget_exhausted -> Error (Source Source.Limit)
        | Error error -> Error (Source error)
        | Ok None -> loop advance
        | Ok (Some item) ->
          if not (Option.for_all t.role ~f:(Text.equal_role (Text.role item)))
          then loop advance
          else (
            match project ~checkpoint projection candidate item with
            | Error _ as error -> error
            | Ok None -> loop advance
            | Ok (Some json) ->
              (match P.Recovery_wire.measure ~limits json with
               | Error _ when position = start -> Error Reply_limit
               | Error _ -> Ok { page with reason = Some Reply_bytes }
               | Ok size when size > t.max_reply_bytes - page.bytes ->
                 Ok { page with reason = Some Reply_bytes }
               | Ok size ->
                 let row = { json; position; selection = candidate.selection } in
                 loop
                   { advance with
                     reversed = row :: page.reversed
                   ; count = page.count + 1
                   ; bytes = page.bytes + size + 1
                   }))))
  in
  loop { reversed = []; count = 0; bytes = 0; next = start; reason = None }
;;

module Release = struct
  type query = t

  type t =
    { query : query
    ; highwater : Basis.Highwater.t
    ; fingerprint : Digest.t
    ; selections : Text.selection list
    }

  let create query basis rows =
    { query
    ; highwater = Basis.highwater basis
    ; fingerprint = Basis.digest basis
    ; selections = List.map rows ~f:(fun row -> row.selection)
    }
  ;;

  let authorize_selections t ~state ~authorize =
    List.fold_result t.selections ~init:() ~f:(fun () selection ->
      authorize state selection t.query.view)
  ;;

  let check t ~current ~authorize ~checkpoint =
    let open Result.Let_syntax in
    let check work =
      let%bind initial = current () in
      let query = t.query in
      let%bind basis =
        Basis.create
          work
          initial
          ~owner:query.source
          ~scope:query.scope
          ~family:query.family
          ~ordinals:query.ordinals
          ~highwater:(Some t.highwater)
        |> Result.map_error ~f:(fun _ -> ())
      in
      if not (Digest.equal t.fingerprint (Basis.digest basis))
      then Error ()
      else (
        let%bind () =
          authorize_selections t ~state:initial ~authorize:(fun state selection view ->
            Work.checkpoint work;
            authorize state selection view)
        in
        let%bind fresh = current () in
        if phys_equal fresh initial then Ok () else Error ())
    in
    try Work.with_metadata_scope ~checkpoint ~f:check with
    | Work.Exhausted | Work.Inactive -> Error ()
  ;;
end

let run
      t
      store
      ~handle
      ~current
      ~authorize
      ~max_frame_bytes
      ~max_payload_bytes
      ?(checkpoint = fun () -> ())
      ?continuation
      ?on_release
      ~sign
      ()
  =
  let open Result.Let_syntax in
  let checkpoint () =
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ()
  in
  let expected_source =
    P.Session_ref.create
      ~server_id:(Agent_store.Session_store.server_id store)
      ~session_id:(Agent_store.Session_store.Handle.session_id handle)
  in
  if not (P.Session_ref.equal t.source expected_source)
  then Error (Source Source.Denied)
  else if
    Option.exists continuation ~f:(fun next ->
      not (Digest.equal t.digest next.Continuation.query))
  then Error Invalid_cursor
  else (
    let current () = current () |> Result.map_error ~f:(fun _ -> Source Source.Denied) in
    let%bind initial = current () in
    let check_basis () =
      let%bind fresh = current () in
      if not (Agent_store.Session_store.retained_lifecycle_available store handle)
      then Error (Source Source.Denied)
      else if phys_equal fresh initial
      then Ok fresh
      else Error Refresh_required
    in
    let read_current selection view =
      match check_basis () with
      | Error _ -> invalid "local recovery source changed while reading"
      | Ok state ->
        let%map () = authorize state selection view in
        state
    in
    let run allowance =
      try
        let structural work =
          Basis.create
            work
            initial
            ~owner:t.source
            ~scope:t.scope
            ~family:t.family
            ~ordinals:t.ordinals
            ~highwater:
              (Option.map continuation ~f:(fun next -> next.Continuation.highwater))
          |> Result.map_error ~f:(function
            | Basis.Source error -> Source error
            | Refresh_required -> Refresh_required)
        in
        let%bind basis =
          Work.borrow allowance store ~handle ~f:(fun () -> structural allowance)
          |> Option.value ~default:(Error (Source Source.Denied))
        in
        let%bind start =
          match continuation with
          | None -> Ok 0
          | Some cursor ->
            if not (Digest.equal cursor.basis (Basis.digest basis))
            then Error Refresh_required
            else if cursor.next_position >= Basis.length basis
            then Error Invalid_cursor
            else Ok cursor.next_position
        in
        let read selection =
          Text.resolve_selected
            ~allowance
            store
            ~handle
            ~current:read_current
            ~max_frame_bytes
            ~max_payload_bytes
            selection
            ~view:t.view
        in
        let%bind page = visit t basis ~start ~checkpoint ~read in
        let%bind json, rows = render_fitting t basis page ~start ~sign in
        checkpoint ();
        let%bind fresh = check_basis () in
        let%map () =
          List.fold_result rows ~init:() ~f:(fun () row ->
            checkpoint ();
            authorize fresh row.selection t.view
            |> Result.map_error ~f:(fun _ -> Source Source.Denied))
        in
        json, Option.map on_release ~f:(fun _ -> Release.create t basis rows)
      with
      | Work.Exhausted -> Error (Source Source.Limit)
      | Work.Inactive -> Error (Source Source.Denied)
    in
    let result =
      Source.with_allowance ~checkpoint store ~handle ~f:(fun allowance ->
        Ok (run allowance))
      |> Result.map_error ~f:(fun error -> Source error)
      |> Result.join
    in
    let%bind _ = check_basis () in
    let%map json, release = result in
    Option.iter (Option.both on_release release) ~f:(fun (register, proof) ->
      register proof);
    json)
;;
