open! Core
module P = Agent_protocol
module G = P.Goal

type t =
  { goal : G.t
  ; mutable live : bool
  }

let goal_id t = G.id t.goal
let retire t = t.live <- false
let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let same_policy before after =
  G.Budget.equal (G.budget before) (G.budget after)
  && P.Run_source.equal (G.source before) (G.source after)
  && G.Objective.equal (G.objective before) (G.objective after)
;;

let root_goal (state : Session_state.t) id =
  let open Result.Let_syntax in
  let%bind descendants =
    Result.of_option
      state.goal_descendants
      ~error:(P.Error.invalid_request "descendant root accounting is absent")
  in
  let%bind goal =
    Option.bind state.goal_catalog ~f:(fun catalog -> Goal_catalog.find catalog id)
    |> Result.of_option ~error:(P.Error.invalid_request "descendant root Goal is absent")
  in
  let%bind () =
    if
      List.mem (Goal_descendants.roots descendants) id ~equal:P.Id.Goal.equal
      && P.Session_ref.equal (Goal_descendants.session descendants) (G.session goal)
      && P.Id.Session.equal
           (P.Session_ref.session_id (G.session goal))
           state.identity.session_id
      && Int.equal (G.source goal).generation state.identity.generation
      && P.Id.Session.equal
           (Inference_ledger.session_id state.inference_ledger)
           state.identity.session_id
      && Int.equal
           (Inference_ledger.generation state.inference_ledger)
           state.identity.generation
    then Ok ()
    else denied "descendant root owner or source generation changed"
  in
  let%bind document =
    Option.bind state.goal_accounts ~f:(fun accounts -> Goal_accounts.find accounts id)
    |> Result.of_option
         ~error:(P.Error.invalid_request "descendant root accounting is absent")
  in
  let%map accounting =
    Goal_accounting_document.resolve document ~goal ~ledger:state.inference_ledger
  in
  goal, accounting
;;

let create state ~goal_id =
  let open Result.Let_syntax in
  let%bind goal, _ = root_goal state goal_id in
  match G.state goal with
  | Open Enabled -> Ok { goal; live = true }
  | Open (Held _) | Closed _ -> denied "descendant root Goal is not enabled"
;;

let current t state =
  let open Result.Let_syntax in
  let%bind () = if t.live then Ok () else denied "descendant root policy was retired" in
  let%bind goal, accounting = root_goal state (goal_id t) in
  if P.Session_ref.equal (G.session t.goal) (G.session goal) && same_policy t.goal goal
  then Ok (goal, accounting)
  else denied "descendant root source, objective or budget changed"
;;

let retain_candidate t ~previous ~candidate ~admission_hold =
  if t.live
  then (
    let retained =
      let open Result.Let_syntax in
      let%bind before, _ = current t previous in
      let%map after, _ = current t candidate in
      match G.state before, G.state after with
      | Open _, Open Enabled -> true
      | Open _, Open (Held _) ->
        Option.exists admission_hold ~f:(P.Id.Goal.equal (goal_id t))
        || G.State.equal (G.state before) (G.state after)
      | Closed _, _ | Open _, Closed _ -> false
    in
    match retained with
    | Ok true -> ()
    | Ok false | Error _ -> retire t)
;;

let fresh t ~state ~now =
  let open Result.Let_syntax in
  let%bind goal, accounting = current t state in
  Goal_charge_admission.check_activation state ~goal ~accounting ~now
;;

let admitted t ~state ~now =
  let open Result.Let_syntax in
  let%bind goal, _ = current t state in
  let%bind () =
    match G.state goal with
    | Open (Enabled | Held _) -> Ok ()
    | Closed _ -> denied "descendant root Goal is closed"
  in
  if
    Option.exists
      (G.Budget.deadline (G.budget goal))
      ~f:(fun deadline -> P.Timestamp.compare now deadline >= 0)
  then denied "descendant root Goal deadline reached"
  else Ok ()
;;
