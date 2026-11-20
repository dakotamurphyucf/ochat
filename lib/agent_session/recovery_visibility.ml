open! Core
module P = Agent_protocol
module Ledger = Recovery_window_ledger
module Window = Ledger.Window
module Scope = P.Recovery_window.Scope
module Citation = P.Recovery_window.Citation
module Tombstone = Session.Moderator_state.Identity_snapshot.Tombstone

type t =
  { owner : P.Session_ref.t
  ; generation : int
  ; windows : (P.Id.Context_window.t, Window.t) Hashtbl.t
  ; suppressed : History_entry.Id.t Hash_set.t
  }

type selection =
  | Unavailable
  | Withdrawn
  | Eligible of Ledger.Member.t

let create (state : Session_state.t) ~owner =
  let open Result.Let_syntax in
  if
    (not (P.Id.Session.equal state.identity.session_id (P.Session_ref.session_id owner)))
    || Option.exists state.recovery_windows ~f:(fun ledger ->
      not (P.Session_ref.equal (Ledger.owner ledger) owner))
  then Error (P.Error.invalid_request "recovery visibility owner differs from source")
  else (
    let%map overlay = Moderator_checkpoint.decode state.moderator in
    let suppressed = Hash_set.create (module History_entry.Id) in
    Option.iter overlay ~f:(fun snapshot ->
      List.iter snapshot.tombstones ~f:(fun (item : Tombstone.t) ->
        Hash_set.add suppressed item.target_id));
    let windows = Hashtbl.create (module P.Id.Context_window) in
    Option.iter state.recovery_windows ~f:(fun ledger ->
      List.iter (Ledger.windows ledger) ~f:(fun window ->
        Hashtbl.set windows ~key:(Window.id window) ~data:window));
    { owner; generation = state.identity.generation; windows; suppressed })
;;

let select t citation =
  let scope = Citation.scope citation in
  if
    (not (P.Session_ref.equal t.owner (Scope.session scope)))
    || not (Int.equal t.generation (Scope.generation scope))
  then Unavailable
  else (
    let member =
      let open Option.Let_syntax in
      let%bind window = Hashtbl.find t.windows (Scope.window scope) in
      let%bind ordinal = P.Recovery_window.ordinal (Window.manifest window) citation in
      Window.at window ~ordinal
    in
    match member with
    | None -> Unavailable
    | Some member ->
      if
        Ledger.Member.is_withdrawn member
        || Hash_set.mem t.suppressed (Citation.occurrence citation)
      then Withdrawn
      else Eligible member)
;;
