open! Core
module P = Agent_protocol
module Source = Recovery_source_reader
module Ledger = Recovery_window_ledger
module Citation = P.Recovery_window.Citation

let visible citation (state : Session_state.t) =
  not
    (Option.exists state.recovery_windows ~f:(fun ledger ->
       Ledger.is_derived_summary ledger citation))
;;

let check accepted citation =
  let owner = P.Recovery_window.Scope.session (Citation.scope citation) in
  let current_member (state : Session_state.t) =
    let open Result.Let_syntax in
    if not (visible citation state)
    then Error Source.Unavailable
    else (
      let%bind visible =
        Recovery_visibility.create state ~owner
        |> Result.map_error ~f:(fun _ -> Source.Corrupt)
      in
      match Recovery_visibility.select visible citation with
      | Eligible member -> Ok member
      | Unavailable -> Error Source.Unavailable
      | Withdrawn -> Error Source.Withdrawn
      | Expired -> Error Source.Expired)
  in
  let%map.Result selected = current_member accepted in
  let windows = accepted.Session_state.recovery_windows in
  let moderator = accepted.moderator in
  let generation = accepted.identity.generation in
  fun (latest : Session_state.t) ->
    Int.equal generation latest.identity.generation
    &&
    if
      Option.equal phys_equal latest.recovery_windows windows
      && Option.equal phys_equal latest.moderator moderator
    then true
    else
      Result.ok (current_member latest)
      |> Option.exists ~f:(fun member ->
        Ledger.Source.equal (Ledger.Member.source selected) (Ledger.Member.source member)
        && Int.equal
             (Ledger.Member.source_ordinal selected)
             (Ledger.Member.source_ordinal member))
;;
