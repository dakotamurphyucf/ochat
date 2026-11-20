open! Core
module P = Agent_protocol
module State = Session_state
module Record = Recovery_effective_record
module Ledger = Recovery_effective_ledger
module Scope = P.Recovery_window.Scope
module Projection = Recovery_effective_projection
module Windows = Recovery_window_ledger

let invalid reason = P.Error.invalid_request reason

let active_scope (state : State.t) =
  Option.map state.recovery_windows ~f:(fun windows ->
    Windows.Window.scope (Windows.active windows))
;;

let projected (state : State.t) ~source_revision ~overlay_incarnation =
  let%bind.Result moderator = Moderator_checkpoint.decode state.moderator in
  Projection.create
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
    (not (Scope.equal (Record.scope record) scope))
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

let resolve_live (state : State.t) ~source_revision citation =
  let open Result.Let_syntax in
  let recorded =
    Option.bind state.recovery_effective ~f:(fun ledger ->
      List.find (Ledger.records ledger) ~f:(fun record ->
        Int64.equal source_revision (Record.source_revision record)))
  in
  match recorded with
  | None -> Ok None
  | Some record ->
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
      let projection_revision =
        match P.Recovery_window.Citation.version citation with
        | Effective { origin = Captured _; _ }
          when Int64.equal state.counters.revision (Int64.succ source_revision) ->
          source_revision
        | Canonical _ | Effective _ -> state.counters.revision
      in
      let%map projection =
        projected state ~source_revision:projection_revision ~overlay_incarnation
      in
      Option.bind projection ~f:(fun projection ->
        Record.resolve record projection citation))
;;
