open! Core
module L = Inference_ledger
module G = Goal_ledger_reconciliation
module C = Delegated_usage

type t =
  { ledger : L.t
  ; accounts : Goal_accounts.t option
  ; accounts_changed : bool
  ; delegated_usage : C.t option
  }

let ledger t = t.ledger
let accounts t = t.accounts
let accounts_changed t = t.accounts_changed
let delegated_usage t = t.delegated_usage

let prepare before ~after ~accounts ~catalog ~delegated_usage ~admission =
  let open Result.Let_syntax in
  let%bind goal = G.prepare ~accounts ~catalog ~before ~after ~admission in
  let%map delegated_usage =
    match delegated_usage with
    | None -> Ok None
    | Some usage -> C.reconcile usage ~before ~after |> Result.map ~f:Option.some
  in
  { ledger = after
  ; accounts = G.accounts goal
  ; accounts_changed = G.changed goal
  ; delegated_usage
  }
;;

let protected_ordinals ledger ~accounts ~delegated_usage =
  let open Result.Let_syntax in
  let%bind protected = G.protected_ordinals accounts ~ledger in
  match delegated_usage with
  | None -> Ok protected
  | Some usage ->
    let%bind () = C.validate usage ~ledger in
    let%map allowed = C.can_retire usage in
    if allowed
    then protected
    else
      List.fold (L.rows ledger) ~init:protected ~f:(fun protected row ->
        match Inference.Observation.Attempt_record.state (L.Row.record row) with
        | Prepared | Running -> protected
        | Terminal _ | Interrupted _ ->
          Set.add protected (L.Handle.ordinal (L.Row.handle row)))
;;
