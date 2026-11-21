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
  | Expired
  | Eligible of Ledger.Member.t

let create ?work (state : Session_state.t) ~owner =
  let open Result.Let_syntax in
  if
    (not (P.Id.Session.equal state.identity.session_id (P.Session_ref.session_id owner)))
    || Option.exists state.recovery_windows ~f:(fun ledger ->
      not (P.Session_ref.equal (Ledger.owner ledger) owner))
  then Error (P.Error.invalid_request "recovery visibility owner differs from source")
  else (
    let%map overlay =
      match work with
      | None -> Moderator_checkpoint.decode state.moderator
      | Some work -> Moderator_checkpoint.decode_for_recovery work state.moderator
    in
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
    let selected =
      let open Option.Let_syntax in
      let%bind window = Hashtbl.find t.windows (Scope.window scope) in
      let%bind ordinal = P.Recovery_window.ordinal (Window.manifest window) citation in
      let%map member = Window.at window ~ordinal in
      Window.state window, member
    in
    match selected with
    | None -> Unavailable
    | Some (Expired, _) -> Expired
    | Some ((Open | Sealed), member) ->
      if
        Ledger.Member.is_withdrawn member
        || Hash_set.mem t.suppressed (Citation.occurrence citation)
      then Withdrawn
      else Eligible member)
;;

let check_effective ?work t citation =
  let open Result.Let_syntax in
  let scope = Citation.scope citation in
  if
    (not (P.Session_ref.equal t.owner (Scope.session scope)))
    || not (Int.equal t.generation (Scope.generation scope))
  then Error `Unavailable
  else (
    let%bind window =
      Hashtbl.find t.windows (Scope.window scope) |> Result.of_option ~error:`Unavailable
    in
    let%bind () =
      match Window.state window with
      | Expired -> Error `Expired
      | Open | Sealed -> Ok ()
    in
    let occurrence = Citation.occurrence citation in
    if Hash_set.mem t.suppressed occurrence
    then Error `Withdrawn
    else (
      let canonical revision =
        let%bind citation =
          Citation.create
            scope
            ~occurrence
            ~version:(P.Recovery_item_version.canonical revision)
          |> Result.map_error ~f:(fun _ -> `Unavailable)
        in
        match select t citation with
        | Unavailable -> Error `Unavailable
        | Withdrawn -> Error `Withdrawn
        | Expired -> Error `Expired
        | Eligible _ -> Ok ()
      in
      match Citation.version citation with
      | Canonical _ -> Error `Unavailable
      | Effective { origin = Inserted _; _ } -> Ok ()
      | Effective { origin = Unchanged revision | Replaced { base = revision; _ }; _ } ->
        canonical revision
      | Effective { origin = Captured _; _ } ->
        (* The legacy witness had a canonical position, but no base revision.
           A withdrawal for any matching occurrence makes that witness unsafe. *)
        let%bind found =
          Window.fold_members window ~init:false ~f:(fun found member ->
            Option.iter work ~f:(fun work -> Recovery_source_work.charge_source work 1);
            let id = Citation.occurrence (Ledger.Member.citation member) in
            if not (P.History.Id.equal id occurrence)
            then Ok found
            else if Ledger.Member.is_withdrawn member
            then Error `Withdrawn
            else Ok true)
        in
        if found then Ok () else Error `Unavailable))
;;

let irrevocably_retired t citation =
  let scope = Citation.scope citation in
  P.Session_ref.equal t.owner (Scope.session scope)
  && (Scope.generation scope < t.generation
      ||
      match select t citation with
      | Expired -> true
      | Unavailable | Withdrawn | Eligible _ -> false)
;;
