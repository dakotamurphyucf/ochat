open! Core
module P = Agent_protocol
module Source = Search_source
module Position = Search_cursor.Position

module Catalog = struct
  type t =
    { organization_revision : int64
    ; sessions : P.Session_catalog.t list
    }
end

type t =
  { server_id : P.Id.Server.t
  ; principal : P.Principal.t
  ; cache : Search_cache.t
  ; cursors : Search_cursor.t
  ; read_catalog : P.Session.List_request.t -> (Catalog.t, P.Error.t) result
  ; read_state : P.Id.Session.t -> (Agent_session.Session_state.t, P.Error.t) result
  }

let create ~server_id ~principal ~cache ~cursors ~read_catalog ~read_state =
  { server_id; principal; cache; cursors; read_catalog; read_state }
;;

module Candidate = struct
  type t =
    { hit : P.Search_hit.t
    ; index : int
    }
end

module Progress = struct
  type t =
    { position : Position.t
    ; reversed : Candidate.t list
    ; hit_count : int
    ; scanned_entries : int
    ; scanned_sessions : int
    ; scanned_bytes : int
    }
end

let binding t query (catalog : Catalog.t) =
  Search_cursor.bind
    t.cursors
    ~principal:t.principal
    ~query
    ~organization_revision:catalog.organization_revision
    ~catalog:catalog.sessions
;;

let read_source t session_id =
  let open Result.Let_syntax in
  let%bind state = t.read_state session_id in
  if not (P.Id.Session.equal session_id state.identity.session_id)
  then Error (Search_cursor.refresh_required ())
  else Source.create ~server_id:t.server_id state
;;

let make_hit source projected (found : Search_text.Match.t) =
  P.Search_hit.create
    ~session:(Source.session source)
    ~generation:(Source.generation source)
    ~session_revision:(Source.revision source)
    ~history_id:(Search_entry.history_id projected)
    ~content_revision:(Search_entry.content_revision projected)
    ~part_index:found.part_index
    ~snippet:found.snippet
;;

let collect source matcher offset entries (progress : Progress.t) =
  let open Result.Let_syntax in
  List.fold_result entries ~init:(offset, progress) ~f:(fun (index, progress) entry ->
    match entry with
    | None -> Ok (index + 1, progress)
    | Some projected ->
      let%bind found = Search_text.find matcher projected in
      (match found with
       | None -> Ok (index + 1, progress)
       | Some found ->
         let%map hit = make_hit source projected found in
         ( index + 1
         , { progress with
             Progress.reversed = Candidate.{ hit; index } :: progress.reversed
           ; hit_count = progress.hit_count + 1
           } )))
  |> Result.map ~f:snd
;;

let scan t query catalog initial =
  let open Result.Let_syntax in
  let matcher = Search_text.create (P.Search_query.term query) in
  let hit_limit = (P.Search_query.catalog query).page.limit in
  let scan_limit = P.Search_query.scan_limit query in
  let sessions = Array.of_list catalog.Catalog.sessions in
  let session_count = Array.length sessions in
  (* A boundary probe measures one additional payload of at most 2 MiB before
     deciding to leave it for the next page. Reserve that work explicitly. *)
  let max_admitted_bytes = 6_291_456 in
  let exhausted (progress : Progress.t) =
    progress.hit_count = hit_limit
    || progress.scanned_entries = scan_limit
    || progress.scanned_bytes = max_admitted_bytes
  in
  let rec visit (progress : Progress.t) =
    let position = progress.position in
    if
      position.session = session_count
      || exhausted progress
      || progress.scanned_sessions = 64
    then Ok progress
    else (
      let catalog = sessions.(position.session).session in
      let%bind source = read_source t catalog.id in
      if
        (not (Int.equal (Source.generation source) catalog.generation))
        || (not (Int64.equal (Source.revision source) catalog.revision))
        || position.entry > Source.length source
      then Error (Search_cursor.refresh_required ())
      else
        consume source { progress with scanned_sessions = progress.scanned_sessions + 1 })
  and consume source progress =
    if exhausted progress
    then Ok progress
    else (
      let offset = progress.position.entry in
      let%bind window =
        Source.project
          source
          ~principal:t.principal
          ~cache:t.cache
          ~offset
          ~limit:
            (Int.min
               (hit_limit - progress.hit_count)
               (scan_limit - progress.scanned_entries))
          ~max_bytes:(max_admitted_bytes - progress.scanned_bytes)
      in
      let%bind progress = collect source matcher offset window.entries progress in
      let%bind position =
        if window.reached_end
        then Position.create ~session:(progress.position.session + 1) ~entry:0
        else Position.create ~session:progress.position.session ~entry:window.next_index
      in
      let progress =
        { progress with
          position
        ; scanned_entries = progress.scanned_entries + window.scanned_entries
        ; scanned_bytes = progress.scanned_bytes + window.scanned_bytes
        }
      in
      if window.reached_end
      then visit progress
      else if window.scanned_entries = 0
      then Ok progress
      else consume source progress)
  in
  if
    initial.Position.session > session_count
    || (initial.session = session_count && initial.entry <> 0)
  then Error (Search_cursor.refresh_required ())
  else
    visit
      Progress.
        { position = initial
        ; reversed = []
        ; hit_count = 0
        ; scanned_entries = 0
        ; scanned_sessions = 0
        ; scanned_bytes = 0
        }
;;

let revalidate t matcher (candidate : Candidate.t) =
  let open Result.Let_syntax in
  let hit = candidate.hit in
  let%bind source = read_source t (P.Session_ref.session_id (P.Search_hit.session hit)) in
  if
    (not (Int.equal (Source.generation source) (P.Search_hit.generation hit)))
    || not (Int64.equal (Source.revision source) (P.Search_hit.session_revision hit))
  then Error (Search_cursor.refresh_required ())
  else (
    let%bind window =
      Source.project
        source
        ~principal:t.principal
        ~cache:t.cache
        ~offset:candidate.index
        ~limit:1
        ~max_bytes:2_097_152
    in
    match window.entries with
    | [ Some projected ]
      when P.History.Id.equal
             (Search_entry.history_id projected)
             (P.Search_hit.history_id hit)
           && P.History.Content_revision.equal
                (Search_entry.content_revision projected)
                (P.Search_hit.content_revision hit) ->
      let%bind found = Search_text.find matcher projected in
      (match found with
       | Some found -> make_hit source projected found
       | None -> Error (Search_cursor.refresh_required ()))
    | [] | None :: _ | Some _ :: _ -> Error (Search_cursor.refresh_required ()))
;;

let query t query =
  let open Result.Let_syntax in
  if not (P.Id.Server.equal t.server_id (P.Search_query.server_id query))
  then Error (P.Error.invalid_request "search query names another host")
  else if not (P.Principal.has_scope t.principal View_session_transcript)
  then
    Error
      (P.Error.create
         Permission_denied
         ~message:"conversation search requires transcript access"
         ~retryable:false
         ())
  else (
    let request = P.Search_query.catalog query in
    let%bind catalog = t.read_catalog request in
    let%bind original = binding t query catalog in
    let%bind initial = Search_cursor.resolve t.cursors original request.page.cursor in
    let%bind progress = scan t query catalog initial in
    let matcher = Search_text.create (P.Search_query.term query) in
    let%bind hits =
      List.fold_result (List.rev progress.reversed) ~init:[] ~f:(fun reversed candidate ->
        let%map hit = revalidate t matcher candidate in
        hit :: reversed)
      |> Result.map ~f:List.rev
    in
    let%bind current_catalog = t.read_catalog request in
    let%bind current = binding t query current_catalog in
    if not (Search_cursor.same_basis original current)
    then Error (Search_cursor.refresh_required ())
    else (
      let reached_end = progress.position.session = List.length catalog.sessions in
      let%bind next_cursor =
        if reached_end
        then Ok None
        else
          Search_cursor.issue t.cursors original progress.position
          |> Result.map ~f:Option.some
      in
      P.Search_page.create
        ~hits
        ~next_cursor
        ~reached_end
        ~scanned_entries:progress.scanned_entries
        ~scanned_sessions:progress.scanned_sessions))
;;

let navigation_current t query source index target =
  let open Result.Let_syntax in
  let first = Int.max 0 (index - 2) in
  let last = Int.min (Source.length source) (index + 3) in
  (* A single source entry may be 2 MiB; separate bounded probes cover the five
     positions without turning an exhausted byte budget into missing context. *)
  let%bind reversed =
    List.fold_result (List.range first last) ~init:[] ~f:(fun acc offset ->
      if offset = index
      then Ok ((offset, target) :: acc)
      else (
        let%map window =
          Source.project
            source
            ~principal:t.principal
            ~cache:t.cache
            ~offset
            ~limit:1
            ~max_bytes:2_097_152
        in
        match window.entries with
        | [ Some entry ] -> (offset, entry) :: acc
        | [ None ] | [] -> acc
        | _ :: _ :: _ -> assert false))
  in
  match List.Assoc.find reversed index ~equal:Int.equal with
  | None -> Ok P.Search_navigation.Response.unavailable
  | Some entry ->
    let%bind found =
      Search_text.find (Search_text.create (P.Search_query.term query)) entry
    in
    (match found with
     | None -> Ok P.Search_navigation.Response.unavailable
     | Some found ->
       let%bind hit = make_hit source entry found in
       let%bind context =
         List.fold_result (List.rev reversed) ~init:[] ~f:(fun acc (_, entry) ->
           let%map entry = Search_entry.navigation_context entry in
           entry :: acc)
       in
       P.Search_navigation.Response.current ~hit ~context:(List.rev context))
;;

let navigate t request =
  let open Result.Let_syntax in
  let query = P.Search_navigation.Request.query request in
  let hit = P.Search_navigation.Request.hit request in
  let session_id = P.Session_ref.session_id (P.Search_hit.session hit) in
  if not (P.Id.Server.equal t.server_id (P.Search_query.server_id query))
  then Error (P.Error.invalid_request "navigation query names another host")
  else if not (P.Principal.has_scope t.principal View_session_transcript)
  then
    Error
      (P.Error.create
         Permission_denied
         ~message:"navigation requires transcript access"
         ~retryable:false
         ())
  else (
    (* Read authorization precedes catalog membership, so a revoked target is an
       explicit denial rather than an apparent search miss. Readers never load. *)
    match read_source t session_id with
    | Error { code = Session_not_found; _ } -> Ok P.Search_navigation.Response.unavailable
    | Error _ as error -> error
    | Ok source ->
      let catalog_request = P.Search_query.catalog query in
      let%bind catalog = t.read_catalog catalog_request in
      let%bind original = binding t query catalog in
      let member =
        List.exists catalog.sessions ~f:(fun entry ->
          P.Id.Session.equal entry.P.Session_catalog.session.id session_id)
      in
      if
        (not member)
        || not (Int.equal (Source.generation source) (P.Search_hit.generation hit))
      then Ok P.Search_navigation.Response.unavailable
      else (
        let%bind response =
          match Source.index_of_id source (P.Search_hit.history_id hit) with
          | None -> Ok P.Search_navigation.Response.unavailable
          | Some index ->
            let%bind window =
              Source.project
                source
                ~principal:t.principal
                ~cache:t.cache
                ~offset:index
                ~limit:1
                ~max_bytes:2_097_152
            in
            (match window.entries with
             | [ Some entry ] ->
               if
                 not
                   (P.History.Content_revision.equal
                      (Search_entry.content_revision entry)
                      (P.Search_hit.content_revision hit))
               then
                 Ok
                   (P.Search_navigation.Response.changed
                      (Search_entry.content_revision entry))
               else navigation_current t query source index entry
             | [] | [ None ] -> Ok P.Search_navigation.Response.unavailable
             | _ :: _ :: _ -> assert false)
        in
        let%bind current = read_source t session_id in
        if
          (not (Int.equal (Source.generation source) (Source.generation current)))
          || not (Int64.equal (Source.revision source) (Source.revision current))
        then Error (Search_cursor.refresh_required ())
        else (
          let%bind catalog = t.read_catalog catalog_request in
          let%bind current = binding t query catalog in
          if not (Search_cursor.same_basis original current)
          then Error (Search_cursor.refresh_required ())
          else Ok response)))
;;
