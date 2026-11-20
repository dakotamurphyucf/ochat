open! Core
module P = Agent_protocol
module Ledger = Recovery_window_ledger
module Window = Ledger.Window
module Source = Ledger.Source
module Member = Ledger.Member
module Scope = P.Recovery_window.Scope
module Citation = P.Recovery_window.Citation
module Archive = Session_state.Compaction_archive

let invalid message = Error (P.Error.invalid_request message)

let seal window history source =
  let open Result.Let_syntax in
  let%bind reversed =
    Window.fold_members window ~init:[] ~f:(fun reversed member ->
      match Member.source member with
      | Archive _ -> Ok (member :: reversed)
      | Live _ when Member.is_withdrawn member -> Ok (member :: reversed)
      | Live _ ->
        let ordinal = Member.source_ordinal member in
        if
          ordinal >= Array.length history || not (Member.matches member history.(ordinal))
        then invalid "rollover archive cannot supply an exact open-window member"
        else (
          let%map rebound =
            Member.create
              (Member.citation member)
              ~source
              ~source_ordinal:ordinal
              ~provenance:(Member.provenance member)
          in
          rebound :: reversed))
  in
  Window.create
    (Window.scope window)
    ~origin:(Window.origin window)
    ~state:Sealed
    (List.rev reversed)
;;

let carry
      visibility
      predecessor
      successor
      history
      source
      ~ordinal
      (entry : P.History.entry)
  =
  let open Result.Let_syntax in
  let citation scope =
    Citation.create
      scope
      ~occurrence:entry.id
      ~version:(P.Recovery_item_version.canonical entry.content_revision)
  in
  let%bind original = citation predecessor in
  match Recovery_visibility.select visibility original with
  | Eligible member ->
    let old = Member.source_ordinal member in
    (match Member.source member with
     | Live _
       when old < Array.length history
            && (not entry.redacted)
            && P.History.equal_entry history.(old) entry ->
       let%bind next = citation successor in
       Member.create
         next
         ~source
         ~source_ordinal:ordinal
         ~provenance:(Carried_from (Scope.window predecessor))
     | Live _ | Archive _ ->
       invalid "rollover carry differs from its live canonical source")
  | Unavailable | Withdrawn -> invalid "rollover carry is unavailable"
;;

let prepare
      (state : Session_state.t)
      ~predecessor
      ~successor
      ~(archive : Archive.t)
      ~retained
  =
  let open Result.Let_syntax in
  let%bind ledger =
    Result.of_option
      state.recovery_windows
      ~error:(P.Error.invalid_request "recovery windows are disabled")
  in
  let active = Ledger.active ledger in
  let owner = Ledger.owner ledger in
  let generation = state.identity.generation in
  let revision = state.counters.revision in
  if
    (not (Scope.equal (Window.scope active) predecessor))
    || (not (P.Session_ref.equal owner (Scope.session successor)))
    || (not (Int.equal generation (Scope.generation successor)))
    || (not (Archive.equal_kind archive.kind Rollover))
    || (not (Int64.equal revision archive.revision))
    || Int64.equal revision Int64.max_value
    || (not (List.is_empty archive.invocation_dispositions))
    || List.exists state.conversation.compaction_archives ~f:(fun prior ->
      P.Id.Operation.equal prior.operation_id archive.operation_id)
  then invalid "rollover has a stale window, generation, or archive prestate"
  else if
    Stdlib.List.compare_length_with retained Window.max_members > 0
    || Stdlib.List.compare_length_with
         state.conversation.canonical_history
         Window.max_members
       > 0
  then invalid "rollover exceeds canonical membership capacity"
  else (
    let%bind digest =
      Chatml_source.Digest.of_hex archive.sha256
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "invalid rollover archive digest")
    in
    let%bind basis = Source.basis ~owner ~generation ~revision in
    let history = Array.of_list state.conversation.canonical_history in
    let source =
      Source.archive basis ~kind:Rollover ~operation:archive.operation_id ~digest
    in
    let%bind sealed = seal active history source in
    let%bind visibility = Recovery_visibility.create state ~owner in
    let%bind live = Source.basis ~owner ~generation ~revision:(Int64.succ revision) in
    let%bind carried =
      List.mapi retained ~f:(fun ordinal entry ->
        carry visibility predecessor successor history (Source.Live live) ~ordinal entry)
      |> Result.all
    in
    let%bind next =
      Window.create
        successor
        ~origin:
          (Successor { previous = Window.id sealed; operation = archive.operation_id })
        ~state:Open
        carried
    in
    let earlier =
      List.filter (Ledger.windows ledger) ~f:(fun w ->
        not (P.Id.Context_window.equal (Window.id w) (Window.id active)))
    in
    Ledger.create ~owner (earlier @ [ sealed; next ]))
;;
