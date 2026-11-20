open! Core
module P = Agent_protocol
module State = Session_state
module Record = Recovery_effective_record
module Ledger = Recovery_effective_ledger
module Scope = P.Recovery_window.Scope
module Projection = Recovery_effective_projection
module Windows = Recovery_window_ledger
module Archive = Record.Archive
module Citation = P.Recovery_window.Citation
module Version = P.Recovery_item_version

let invalid reason = P.Error.invalid_request reason

let active_scope (state : State.t) =
  Option.map state.recovery_windows ~f:(fun windows ->
    Windows.Window.scope (Windows.active windows))
;;

let projected ?work (state : State.t) ~source_revision ~overlay_incarnation =
  let moderator, project =
    match work with
    | None -> Moderator_checkpoint.decode state.moderator, Projection.create
    | Some work ->
      ( Moderator_checkpoint.decode_for_recovery work state.moderator
      , Projection.create_for_read work )
  in
  let%bind.Result moderator = moderator in
  project
    ~source_revision
    ~overlay_incarnation
    ~history:state.conversation.canonical_history
    ~moderator
;;

let current_incarnation (state : State.t) =
  Option.bind state.recovery_effective ~f:Ledger.current_incarnation
;;

let incarnation state =
  Option.value (current_incarnation state) ~default:state.State.counters.revision
;;

let scope_for_operation (state : State.t) operation =
  let open Result.Let_syntax in
  let%bind () =
    match state.active_operation with
    | Some { id; generation; kind = Turn _; _ }
      when P.Id.Operation.equal operation id
           && Int.equal generation state.identity.generation -> Ok ()
    | Some _ | None -> Error (invalid "effective preparation has no current root turn")
  in
  Result.of_option (active_scope state) ~error:(invalid "recovery windows are disabled")
;;

let prepare state ~operation input =
  let open Result.Let_syntax in
  let%bind scope = scope_for_operation state operation in
  let source_revision = state.counters.revision in
  let overlay_incarnation = incarnation state in
  let%bind projection = projected state ~source_revision ~overlay_incarnation in
  match projection with
  | None -> Ok None
  | Some projection ->
    let%bind record =
      Record.capture
        input
        ~projection
        ~scope
        ~operation
        ~source_revision
        ~overlay_incarnation
    in
    let%map available = Ledger.add_if_room state.recovery_effective record in
    Option.map available ~f:(fun _ -> record)
;;

let apply state record =
  let open Result.Let_syntax in
  let%bind scope = scope_for_operation state (Record.operation record) in
  let overlay_incarnation = incarnation state in
  if
    (match Record.custody record with
     | Live -> false
     | Lost | Archived _ -> true)
    || Option.is_some (Record.attempt record)
    || (not (Scope.equal (Record.scope record) scope))
    || (not (Int64.equal state.counters.revision (Record.source_revision record)))
    || not (Int64.equal overlay_incarnation (Record.overlay_incarnation record))
  then Error (invalid "effective preparation source changed before admission")
  else (
    let%bind projection =
      projected state ~source_revision:state.counters.revision ~overlay_incarnation
    in
    let%bind projection =
      Result.of_option
        projection
        ~error:(invalid "committed preparation source exceeds the coordinate limit")
    in
    let%bind () = Record.validate_source record projection in
    Ledger.add state.recovery_effective record)
;;

let moderator_changed ledger ~previous ~next =
  match ledger with
  | None -> None
  | Some current ->
    let safe =
      match Moderator_checkpoint.decode previous, Moderator_checkpoint.decode next with
      | Ok None, Ok None -> true
      | Ok (Some before), Ok (Some after) ->
        String.equal before.script_id after.script_id
        && String.equal before.script_source_hash after.script_source_hash
        && before.next_change_id <= after.next_change_id
        && before.revision <= after.revision
      | (Error _ | Ok None | Ok (Some _)), (Error _ | Ok None | Ok (Some _)) -> false
    in
    Some (if safe then current else Ledger.rotate_incarnation current)
;;

let replacement (state : State.t) ~(incoming : State.t) =
  if not (Int.equal state.identity.generation incoming.identity.generation)
  then None
  else if
    not
      (P.Id.Prompt_revision.equal
         state.spec.prompt_revision_id
         incoming.spec.prompt_revision_id)
  then Option.map state.recovery_effective ~f:Ledger.rotate_incarnation
  else
    moderator_changed
      state.recovery_effective
      ~previous:state.moderator
      ~next:incoming.moderator
;;

let resolve_record (state : State.t) ~source_revision =
  Option.bind state.recovery_effective ~f:(fun ledger ->
    List.find (Ledger.records ledger) ~f:(fun record ->
      Int64.equal source_revision (Record.source_revision record)))
;;

let projection_revision ~revision record =
  let source_revision = Record.source_revision record in
  if Int64.equal revision (Int64.succ source_revision) then source_revision else revision
;;

let resolve_live ?work (state : State.t) ~source_revision citation =
  let open Result.Let_syntax in
  match resolve_record state ~source_revision with
  | Some record ->
    (match Record.custody record with
     | Lost | Archived _ -> Ok None
     | Live ->
       let overlay_incarnation = Record.overlay_incarnation record in
       let same_scope =
         Option.exists (active_scope state) ~f:(Scope.equal (Record.scope record))
       in
       if
         (not same_scope)
         || (not
               (Option.exists
                  (current_incarnation state)
                  ~f:(Int64.equal overlay_incarnation)))
         || Int64.(state.counters.revision <= source_revision)
       then Ok None
       else (
         let source_revision =
           projection_revision ~revision:state.counters.revision record
         in
         let%map projection =
           projected ?work state ~source_revision ~overlay_incarnation
         in
         Option.bind projection ~f:(fun projection ->
           Record.resolve ?work record projection citation)))
  | None -> Ok None
;;

let kind_of_archive = function
  | State.Compaction_archive.Compaction -> Ok Archive.Summary
  | Rollover -> Ok Archive.Rollover
  | Reset | Rebuild | Upgrade | Edit | Delete | Pending_input ->
    Error (invalid "effective source requires a compaction or rollover prestate")
;;

let same_projected_origin ~prestate_revision (prepared : Version.origin) projected =
  match prepared, projected with
  | Captured { session_revision }, Version.Captured _ ->
    Int64.equal prestate_revision (Int64.succ session_revision)
  | _ -> Version.equal_origin prepared projected
;;

let relocation_positions projection ~prestate_revision =
  let cache = Hashtbl.create (module P.History.Id) in
  let remaining_bytes = ref Inference_runtime.Recovery_input.max_projection_bytes in
  let digest row =
    Hashtbl.find_or_add cache (Projection.Row.occurrence row) ~default:(fun () ->
      let version, bytes = Projection.Row.version row ~remaining_bytes:!remaining_bytes in
      remaining_bytes := bytes;
      match version with
      | Some (Effective { digest; _ }) -> Some digest
      | Some (Canonical _) | None -> None)
  in
  fun record ->
    List.filter_map (Record.members record) ~f:(fun member ->
      let citation = Record.Member.citation member in
      match
        ( Citation.version citation
        , Projection.find projection (Citation.occurrence citation) )
      with
      | Effective { origin; digest = prepared }, Some row
        when same_projected_origin ~prestate_revision origin (Projection.Row.origin row)
             && Option.exists (digest row) ~f:(Chatml_source.Digest.equal prepared) ->
        Some
          (Archive.Position.create
             ~request_ordinal:(Record.Member.request_ordinal member)
             ~effective_ordinal:(Projection.Row.effective_ordinal row)
             ~canonical_ordinal:(Projection.Row.canonical_ordinal row))
      | _ -> None)
    |> Result.all
;;

let relocate (previous : State.t) ~(archive : State.Compaction_archive.t) =
  let open Result.Let_syntax in
  match previous.recovery_effective with
  | None -> Ok None
  | Some ledger when not (Ledger.has_live_sources ledger) ->
    Ok previous.recovery_effective
  | Some ledger ->
    let%bind kind = kind_of_archive archive.kind in
    if not (Int64.equal archive.revision previous.counters.revision)
    then Error (invalid "effective archive reference is not for the exact prestate")
    else (
      let%bind digest =
        Chatml_source.Digest.of_hex archive.sha256
        |> Result.map_error ~f:(fun _ -> invalid "effective archive digest is invalid")
      in
      let%bind present =
        match active_scope previous, Ledger.current_incarnation ledger with
        | Some scope, Some overlay_incarnation ->
          let%map projection =
            projected previous ~source_revision:archive.revision ~overlay_incarnation
          in
          Option.map projection ~f:(fun projection ->
            ( scope
            , overlay_incarnation
            , relocation_positions projection ~prestate_revision:archive.revision ))
        | _ -> Ok None
      in
      let transfer record =
        match Record.custody record, present with
        | Live, Some (scope, epoch, positions)
          when Scope.equal (Record.scope record) scope
               && Int64.equal (Record.overlay_incarnation record) epoch ->
          let%bind positions = positions record in
          if List.is_empty positions
          then Ok (Record.lose_live_source record)
          else (
            let%bind location =
              Archive.create
                ~kind
                ~operation:archive.operation_id
                ~revision:archive.revision
                ~digest
                positions
            in
            Record.archive record location)
        | Live, _ -> Ok (Record.lose_live_source record)
        | (Archived _ | Lost), _ -> Ok record
      in
      let%map next = Ledger.relocate ledger ~transfer in
      Some next)
;;

let resolve_archived ?work (archived : State.t) record citation =
  let open Result.Let_syntax in
  let source_revision = Record.source_revision record in
  let archived_record = resolve_record archived ~source_revision in
  let scope = Record.scope record in
  match Record.custody record, archived_record with
  | Archived location, Some admitted
    when Int64.equal (Archive.revision location) archived.counters.revision
         && Int.equal (Scope.generation scope) archived.identity.generation
         && P.Id.Session.equal
              archived.identity.session_id
              (P.Session_ref.session_id (Scope.session scope))
         && (match Record.custody admitted with
             | Live -> true
             | Lost | Archived _ -> false)
         && Option.exists (active_scope archived) ~f:(Scope.equal scope)
         && Option.exists
              (current_incarnation archived)
              ~f:(Int64.equal (Record.overlay_incarnation record))
         && Record.equal_preparation record admitted ->
    let source_revision =
      projection_revision ~revision:archived.counters.revision record
    in
    let overlay_incarnation = Record.overlay_incarnation record in
    let%map projection = projected ?work archived ~source_revision ~overlay_incarnation in
    Option.bind projection ~f:(fun projection ->
      Record.resolve_archived ?work record projection citation)
  | _ -> Error (invalid "verified archive does not contain the exact preparation source")
;;
