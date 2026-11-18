open! Core
module L = Inference_ledger
module G = Goal_ledger_reconciliation
module C = Delegated_usage

type t =
  { ledger : L.t
  ; accounts : Goal_accounts.t option
  ; accounts_changed : bool
  ; delegated_usage : C.t option
  ; policy_usage : Policy_canonical_usage.t option
  ; policy_update : Policy_canonical_usage.Update.t option
  }

let ledger t = t.ledger
let accounts t = t.accounts
let accounts_changed t = t.accounts_changed
let delegated_usage t = t.delegated_usage
let policy_usage t = t.policy_usage
let policy_update t = t.policy_update

let prepare
      ?policy_usage
      ?policy_admission
      before
      ~after
      ~accounts
      ~catalog
      ~delegated_usage
      ~admission
  =
  let open Result.Let_syntax in
  let%bind goal = G.prepare ~accounts ~catalog ~before ~after ~admission in
  let%bind delegated_usage =
    match delegated_usage with
    | None -> Ok None
    | Some usage -> C.reconcile usage ~before ~after |> Result.map ~f:Option.some
  in
  let%map policy_update =
    match policy_usage, policy_admission with
    | None, None -> Ok None
    | None, Some _ ->
      Error
        (Agent_protocol.Error.invalid_request "policy Attempt has no canonical component")
    | Some policy, selected ->
      let%bind selection =
        Option.value_map selected ~default:(Ok None) ~f:(fun selected ->
          Policy_canonical_usage.Admission.select
            selected
            policy
            ~accounts:(G.accounts goal)
            ~native_admission:admission
            ~before
            ~after
          |> Result.map ~f:Option.some)
      in
      Policy_canonical_usage.reconcile policy ~before ~after ~selection
      |> Result.map ~f:Option.some
  in
  { ledger = after
  ; accounts = G.accounts goal
  ; accounts_changed = G.changed goal
  ; delegated_usage
  ; policy_usage = Option.map policy_update ~f:Policy_canonical_usage.Update.candidate
  ; policy_update
  }
;;

let protected_ordinals ?policy_usage ledger ~accounts ~delegated_usage =
  let open Result.Let_syntax in
  let%bind protected = G.protected_ordinals accounts ~ledger in
  let%bind protected =
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
  in
  match policy_usage with
  | None -> Ok protected
  | Some policy ->
    let%map policy = Policy_canonical_usage.protected_ordinals policy ~ledger in
    Set.union protected policy
;;
