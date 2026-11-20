open! Core
module P = Agent_protocol
module S = Session_state
module Ledger = Recovery_window_ledger
module Scope = P.Recovery_window.Scope
module Citation = P.Recovery_window.Citation
module Source = Ledger.Source
module Window = Ledger.Window
module Member = Ledger.Member
module History_id = History_entry.Id

let invalid why = Error (P.Error.invalid_request why)

let next_revision (state : S.t) =
  let revision = state.counters.revision in
  if Int64.equal revision Int64.max_value
  then invalid "recovery window publication revision exhausted"
  else Ok (Int64.succ revision)
;;

let make_member scope ~source ~source_ordinal ~provenance (entry : P.History.entry) =
  let open Result.Let_syntax in
  let%bind citation =
    Citation.create
      scope
      ~occurrence:entry.id
      ~version:(P.Recovery_item_version.canonical entry.content_revision)
  in
  Member.create citation ~source ~source_ordinal ~provenance
;;

let live_source scope ~revision =
  let%map.Result basis =
    Source.basis
      ~owner:(Scope.session scope)
      ~generation:(Scope.generation scope)
      ~revision
  in
  Source.Live basis
;;

let create_root (state : S.t) ~owner ~window =
  let open Result.Let_syntax in
  if not (P.Id.Session.equal state.identity.session_id (P.Session_ref.session_id owner))
  then invalid "recovery bootstrap owner differs from the session"
  else (
    let history = state.conversation.canonical_history in
    if Stdlib.List.compare_length_with history Window.max_members > 0
    then invalid "canonical history exceeds the recovery membership limit"
    else (
      let%bind scope =
        Scope.create ~session:owner ~generation:state.identity.generation ~window
      in
      let%bind source = live_source scope ~revision:state.counters.revision in
      let%bind members =
        List.mapi history ~f:(fun source_ordinal entry ->
          make_member scope ~source ~source_ordinal ~provenance:Observed entry)
        |> Result.all
      in
      let%bind root = Window.create scope ~origin:Root ~state:Open members in
      Ledger.create ~owner [ root ]))
;;

let bootstrap (state : S.t) ~owner ~window =
  match state.recovery_windows, state.runtime_initialization, state.active_operation with
  | Some _, _, _ -> invalid "recovery window is already enabled"
  | None, Ready, None -> create_root state ~owner ~window
  | None, (Ready | Pending _), _ -> invalid "recovery bootstrap requires settled history"
;;

let members window =
  List.init (Window.length window) ~f:(fun ordinal ->
    Option.value_exn (Window.at window ~ordinal))
;;

let with_members window members =
  Window.create
    (Window.scope window)
    ~origin:(Window.origin window)
    ~state:(Window.state window)
    members
;;

let update_active ledger ~f =
  let open Result.Let_syntax in
  let active = Ledger.active ledger in
  let%bind windows =
    List.map (Ledger.windows ledger) ~f:(fun window ->
      if P.Id.Context_window.equal (Window.id active) (Window.id window)
      then f window
      else Ok window)
    |> Result.all
  in
  Ledger.create ~owner:(Ledger.owner ledger) windows
;;

let append (state : S.t) entries =
  let open Result.Let_syntax in
  match state.recovery_windows, entries with
  | (None as unchanged), _ | (Some _ as unchanged), [] -> Ok unchanged
  | Some ledger, _ :: _ ->
    let%bind revision = next_revision state in
    let first_ordinal = List.length state.conversation.canonical_history in
    let active = Ledger.active ledger in
    if
      Stdlib.List.compare_length_with entries (Window.max_members - Window.length active)
      > 0
    then invalid "canonical append exceeds the recovery membership limit"
    else (
      let%map result =
        update_active ledger ~f:(fun window ->
          let scope = Window.scope window in
          let%bind source = live_source scope ~revision in
          let%bind added =
            List.mapi entries ~f:(fun offset entry ->
              make_member
                scope
                ~source
                ~source_ordinal:(first_ordinal + offset)
                ~provenance:Observed
                entry)
            |> Result.all
          in
          with_members window (members window @ added))
      in
      Some result)
;;

let rebind member ~source ~source_ordinal =
  Member.create
    (Member.citation member)
    ~source
    ~source_ordinal
    ~provenance:(Member.provenance member)
;;

let occurrence member = Citation.occurrence (Member.citation member)

let index_history history =
  if Stdlib.List.compare_length_with history Window.max_members > 0
  then invalid "canonical replacement exceeds the recovery membership limit"
  else (
    let entries = Hashtbl.create (module History_id) in
    let%map.Result () =
      List.foldi history ~init:(Ok ()) ~f:(fun ordinal status (entry : P.History.entry) ->
        let%bind.Result () = status in
        match Hashtbl.add entries ~key:entry.id ~data:(ordinal, entry) with
        | `Ok -> Ok ()
        | `Duplicate -> invalid "canonical replacement repeated a history occurrence")
    in
    entries)
;;

let refresh_open window ~history ~revision ~index ~retired ~archive =
  let open Result.Let_syntax in
  let captured = Hash_set.create (module History_id) in
  let scope = Window.scope window in
  let%bind live = live_source scope ~revision in
  let%bind kept =
    Window.fold_members window ~init:[] ~f:(fun kept member ->
      let id = occurrence member in
      match Member.source member with
      | _ when Member.is_withdrawn member -> Ok (member :: kept)
      | _ when Hash_set.mem retired id -> Ok (Member.withdraw member :: kept)
      | Archive _ -> Ok (member :: kept)
      | Live _ ->
        (match Hashtbl.find index id with
         | Some (ordinal, entry) when Member.matches member entry ->
           Hash_set.add captured id;
           if Int.equal ordinal (Member.source_ordinal member)
           then Ok (member :: kept)
           else (
             let%map member = rebind member ~source:live ~source_ordinal:ordinal in
             member :: kept)
         | Some _ | None ->
           (match archive with
            | None -> Ok (Member.withdraw member :: kept)
            | Some (source, _) ->
              let%map member =
                rebind member ~source ~source_ordinal:(Member.source_ordinal member)
              in
              member :: kept)))
  in
  let additions =
    List.filter_mapi history ~f:(fun ordinal (entry : P.History.entry) ->
      if Hash_set.mem captured entry.id then None else Some (ordinal, entry))
  in
  let%bind () =
    match archive, additions with
    | None, _ -> Ok ()
    | Some _, [ (ordinal, _) ] when Int.equal ordinal (List.length history - 1) -> Ok ()
    | Some _, _ -> invalid "ordinary compaction requires one new terminal summary"
  in
  let provenance =
    match archive with
    | None -> Member.Observed
    | Some (_, operation) -> Derived_summary operation
  in
  let%bind added =
    List.map additions ~f:(fun (source_ordinal, entry) ->
      make_member scope ~source:live ~source_ordinal ~provenance entry)
    |> Result.all
  in
  with_members window (List.rev_append kept added)
;;

let replace_at ledger ~previous ~history ~revision ~retired =
  let open Result.Let_syntax in
  let%bind index = index_history history in
  let retired = Hash_set.of_list (module History_id) retired in
  (* A generic same-generation replacement may also remove or edit live
     history. It must fence every archived copy of the affected occurrence. *)
  let%bind () =
    List.fold_result previous ~init:() ~f:(fun () (entry : P.History.entry) ->
      match Hashtbl.find index entry.id with
      | Some (_, next)
        when P.History.Content_revision.equal entry.content_revision next.content_revision
        ->
        if P.History.equal_entry entry next
        then Ok ()
        else invalid "canonical recovery content changed without a new version"
      | Some _ | None ->
        Hash_set.add retired entry.id;
        Ok ())
  in
  let%bind windows =
    List.map (Ledger.windows ledger) ~f:(fun window ->
      match Window.state window with
      | Open -> refresh_open window ~history ~revision ~index ~retired ~archive:None
      | Sealed when Hash_set.is_empty retired -> Ok window
      | Sealed ->
        let members =
          List.map (members window) ~f:(fun member ->
            if Hash_set.mem retired (occurrence member)
            then Member.withdraw member
            else member)
        in
        with_members window members)
    |> Result.all
  in
  Ledger.create ~owner:(Ledger.owner ledger) windows
;;

let replace (state : S.t) ~history ~retired =
  let open Result.Let_syntax in
  match state.recovery_windows with
  | None -> Ok None
  | Some ledger ->
    let%bind revision = next_revision state in
    let%map ledger =
      replace_at
        ledger
        ~previous:state.conversation.canonical_history
        ~history
        ~revision
        ~retired
    in
    Some ledger
;;

let check_summary_history (state : S.t) history =
  let open Result.Let_syntax in
  let%bind previous = index_history state.conversation.canonical_history in
  match List.rev history with
  | [] -> invalid "ordinary compaction requires a new summary"
  | summary :: retained ->
    if Hashtbl.mem previous summary.P.History.id
    then invalid "ordinary compaction summary is not a fresh occurrence"
    else
      List.fold_result retained ~init:() ~f:(fun () (entry : P.History.entry) ->
        match Hashtbl.find previous entry.id with
        | Some (_, original) when P.History.equal_entry original entry -> Ok ()
        | Some _ | None -> invalid "ordinary compaction changed a retained occurrence")
;;

let summarize (state : S.t) ~(archive : S.Compaction_archive.t) ~history =
  let open Result.Let_syntax in
  match state.recovery_windows with
  | None -> invalid "ordinary recovery compaction requires an enabled window"
  | Some ledger ->
    let%bind () =
      if
        S.Compaction_archive.equal_kind archive.kind Compaction
        && Int64.equal archive.revision state.counters.revision
        && List.is_empty archive.invocation_dispositions
        && Option.exists state.active_operation ~f:(fun operation ->
          P.Id.Operation.equal operation.id archive.operation_id
          && Int.equal operation.generation state.identity.generation
          && P.Operation.equal_kind operation.kind Compaction)
        && not
             (List.exists state.conversation.compaction_archives ~f:(fun previous ->
                P.Id.Operation.equal previous.operation_id archive.operation_id))
      then Ok ()
      else invalid "ordinary recovery compaction has no matching prestate archive"
    in
    let%bind revision = next_revision state in
    let%bind index = index_history history in
    let%bind () = check_summary_history state history in
    let%bind digest =
      Chatml_source.Digest.of_hex archive.sha256
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "invalid summary archive digest")
    in
    let owner = Ledger.owner ledger in
    let%bind basis =
      Source.basis ~owner ~generation:state.identity.generation ~revision:archive.revision
    in
    let source = Source.archive basis ~kind:Summary ~operation:archive.operation_id ~digest in
    let retired = Hash_set.create (module History_id) in
    update_active ledger ~f:(fun window ->
      refresh_open
        window
        ~history
        ~revision
        ~index
        ~retired
        ~archive:(Some (source, archive.operation_id)))
;;

let replacement (state : S.t) ~(incoming : S.t) =
  let open Result.Let_syntax in
  match state.recovery_windows with
  | None ->
    (match incoming.recovery_windows with
     | None -> Ok None
     | Some _ -> invalid "replacement cannot enable recovery window tracking")
  | Some ledger ->
    let before = state.identity.generation in
    let after = incoming.identity.generation in
    if after < before
    then invalid "recovery window source generation moved backwards"
    else if Int.equal before after
    then (
      let history = incoming.conversation.canonical_history in
      if phys_equal state.conversation.canonical_history history
      then Ok (Some ledger)
      else (
        let%map ledger =
          replace_at
            ledger
            ~previous:state.conversation.canonical_history
            ~history
            ~revision:incoming.counters.revision
            ~retired:[]
        in
        Some ledger))
    else (
      let prior = Window.id (Ledger.active ledger) |> P.Id.Context_window.to_string in
      let digest =
        sprintf "recovery-window-generation-v1:%s:%d" prior after
        |> Digestif.SHA256.digest_string
        |> Digestif.SHA256.to_hex
      in
      let%bind window = P.Id.Context_window.of_string ("win_" ^ digest) in
      let%map root = create_root incoming ~owner:(Ledger.owner ledger) ~window in
      Some root)
;;

let check_replacement_archive (state : S.t) ~(incoming : S.t) =
  if
    Option.is_none state.recovery_windows
    || Int.equal state.identity.generation incoming.identity.generation
  then Ok ()
  else if
    List.exists incoming.conversation.compaction_archives ~f:(fun archive ->
      Int64.equal archive.revision state.counters.revision
      &&
      match archive.kind with
      | Reset | Rebuild -> true
      | Compaction | Upgrade | Edit | Delete | Pending_input | Rollover -> false)
  then Ok ()
  else invalid "recovery generation replacement requires an exact administrative archive"
;;
