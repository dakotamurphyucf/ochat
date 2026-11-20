open! Core
module P = Agent_protocol
module A = Agent_session
module Store = Agent_store.Session_store
module Access = A.Session_actor.Recovery_history_access
module Query = A.Recovery_history_query
module Text = A.Recovery_item_text

type authorize_view =
  P.Principal.t
  -> A.Session_state.t
  -> Text.view
  -> selection:Text.selection option
  -> (unit, P.Error.t) result

type release = unit -> bool

type staged =
  { page : Jsonaf.t
  ; check_return : release
  ; check_delivery : release option
  }

type source_result = ((staged, P.Error.t) result, A.Recovery_source_reader.error) result

let denied () =
  P.Error.create
    Permission_denied
    ~message:"recovery history is unavailable"
    ~retryable:false
    ()
;;

let expired () =
  P.Error.create
    Cursor_expired
    ~message:"recovery cursor expired; request the first page again"
    ~retryable:false
    ~data:(`Object [ "snapshot_required", `True ])
    ()
;;

let source_error = function
  | A.Recovery_source_reader.Denied -> denied ()
  | Limit | Budget_exhausted ->
    P.Error.create
      Resource_limit
      ~message:"recovery source examination limit"
      ~retryable:false
      ()
  | _ ->
    P.Error.create
      Interrupted
      ~message:"recovery history source is unavailable"
      ~retryable:true
      ()
;;

let query_error ~resuming = function
  | Query.Invalid_cursor -> expired ()
  | Refresh_required when resuming -> expired ()
  | Refresh_required ->
    P.Error.create
      Conflict
      ~message:"recovery history changed during the call"
      ~retryable:true
      ()
  | Reply_limit ->
    P.Error.create Resource_limit ~message:"recovery reply limit" ~retryable:false ()
  | Source error -> source_error error
;;

let checkpoint now =
  let start = now () in
  fun () ->
    if
      Mtime.Span.compare
        (Mtime.span start (now ()))
        (Mtime.Span.of_uint64_ns 5_000_000_000L)
      >= 0
    then raise Eio.Time.Timeout
;;

let default_view _principal _state view ~selection:_ =
  match view with
  | Text.Ordinary -> Ok ()
  | Selected_developer | Selected_tool_result -> Error (denied ())
;;

let query
      store
      ~now
      ~max_payload_bytes
      ~with_source
      ~access
      ?(authorize_view = default_view)
      ?cursor
      ?on_release
      query
  =
  let open Result.Let_syntax in
  let%bind () =
    if Access.requires_release_check access && Option.is_none on_release
    then Error (denied ())
    else Ok ()
  in
  let%bind principal = Access.principal access ~query in
  let source = Query.source query in
  let authorize (state : A.Session_state.t) =
    if
      Authorization.session_creator_visible_to
        principal
        ~creator:state.identity.creating_principal
    then authorize_view principal state (Query.view query) ~selection:None
    else Error (denied ())
  in
  let authorize_item state selection view =
    authorize_view principal state view ~selection:(Some selection)
  in
  let authorize_request state =
    let%bind _ = Access.principal access ~query in
    authorize state
  in
  let read handle ~observation ~current ~check_owner : source_result =
    Ok
      (let%bind proof = Access.capture observation access ~query in
       let retained_current () =
         let%bind () = check_owner () in
         if not (Store.retained_lifecycle_available store handle)
         then Error (denied ())
         else (
           let%bind state = Access.check_current proof in
           let%map () = authorize state in
           state)
       in
       let current_request () =
         let%bind state = current () in
         let%bind owned = retained_current () in
         if phys_equal state owned then Ok state else Error (denied ())
       in
       let%bind continuation =
         Option.value_map cursor ~default:(Ok None) ~f:(fun cursor ->
           Access.read_cursor proof cursor |> Result.map ~f:Option.some)
       in
       let registered = ref None in
       let%bind json =
         Query.run
           query
           store
           ~handle
           ~current:current_request
           ~authorize:authorize_item
           ~max_frame_bytes:max_payload_bytes
           ~max_payload_bytes
           ~checkpoint:(checkpoint now)
           ?continuation
           ~on_release:(fun proof -> registered := Some proof)
           ~sign:(Access.seal_cursor proof)
           ()
         |> Result.map_error ~f:(query_error ~resuming:(Option.is_some cursor))
       in
       let%bind accepted = current_request () in
       let%map release = Result.of_option !registered ~error:(denied ()) in
       let check_return () =
         Result.is_ok
           (let%bind _ = Access.principal access ~query in
            let%bind state = retained_current () in
            if phys_equal state accepted
            then
              Query.Release.authorize_selections release ~state ~authorize:authorize_item
            else Error (denied ()))
       in
       let check_delivery =
         Option.map on_release ~f:(fun _ () ->
           let erase result = Result.map_error result ~f:(fun _ -> ()) in
           try
             Query.Release.check
               release
               ~current:(fun () -> erase (retained_current ()))
               ~authorize:(fun state selection view -> erase (authorize_item state selection view))
               ~checkpoint:(checkpoint now)
             |> Result.is_ok
           with
           | Eio.Time.Timeout -> false)
       in
       { page = json; check_return; check_delivery })
  in
  let%bind outcome =
    with_source source ~authorize:authorize_request ~f:read
    |> Result.map_error ~f:source_error
  in
  let%bind staged = outcome in
  if not (staged.check_return ())
  then Error (denied ())
  else (
    match on_release, staged.check_delivery with
    | Some register, Some check ->
      register check;
      Ok staged.page
    | None, None -> Ok staged.page
    | Some _, None | None, Some _ -> Error (denied ()))
;;
