open! Core
module P = Agent_protocol
module G = P.Goal

type t =
  { goal : G.t
  ; link : Run_goal_link.t
  }

let goal_id t = G.id t.goal
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let create ~goal ~link =
  if
    P.Id.Goal.equal (G.id goal) (Run_goal_link.goal_id link)
    && Option.exists
         (G.episode goal)
         ~f:(P.Goal_episode.equal (Run_goal_link.episode link))
  then Ok { goal; link }
  else conflict "Goal activation requires its exact episode association"
;;

let current t (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind goal =
    Option.bind state.goal_catalog ~f:(fun catalog ->
      Goal_catalog.find catalog (goal_id t))
    |> Result.of_option ~error:(P.Error.invalid_request "activation Goal is absent")
  in
  if
    Goal_admission_policy.same_allocation_policy t.goal goal
    && Int.equal (G.source goal).generation state.identity.generation
  then Ok goal
  else denied "Goal activation source, objective, budget or episode changed"
;;

let account goal (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind child =
    Option.bind state.goal_accounts ~f:(fun accounts ->
      Goal_accounts.find accounts (G.id goal))
    |> Result.of_option
         ~error:(P.Error.invalid_request "Goal activation accounting is absent")
  in
  Goal_accounting_document.resolve child ~goal ~ledger:state.inference_ledger
;;

let check_association t ~state ~(run : P.Run.t) ~admission =
  let open Result.Let_syntax in
  let%bind index =
    Result.of_option
      state.Session_state.run_state
      ~error:(P.Error.invalid_request "Goal activation Run index is absent")
  in
  let%bind actual =
    Result.of_option
      (Run_state.goal_link index run.id)
      ~error:(P.Error.invalid_request "Goal activation Run link is absent")
  in
  if
    Run_goal_link.equal actual t.link
    && P.Goal_episode.matches_run (Run_goal_link.episode actual) run
    && P.Run_receipt.equal
         (P.Goal_episode.admission (Run_goal_link.episode actual))
         admission
    && P.Run_source.equal run.source (G.source t.goal)
  then Ok ()
  else conflict "Goal activation immutable association changed"
;;

let check_new_episode_eligibility t ~state ~now =
  let open Result.Let_syntax in
  let%bind goal = current t state in
  let%bind accounting = account goal state in
  Goal_admission_policy.check_activation
    goal
    ~accounting
    ~ledger:state.inference_ledger
    ~now
;;

let check_admitted_policy t ~state =
  let open Result.Let_syntax in
  let%bind goal = current t state in
  let%bind _ = account goal state in
  match G.state goal with
  | Open (Enabled | Held _) -> Ok ()
  | Closed _ -> denied "Goal activation is closed"
;;

let check_policy t ~state =
  let open Result.Let_syntax in
  let%bind () = check_admitted_policy t ~state in
  let%bind goal = current t state in
  match G.state goal with
  | Open Enabled -> Ok ()
  | Open (Held _) | Closed _ -> denied "Goal activation is no longer enabled"
;;

let check_deadline t ~state ~now =
  let open Result.Let_syntax in
  let%bind goal = current t state in
  if
    Option.exists
      (G.Budget.deadline (G.budget goal))
      ~f:(fun deadline -> P.Timestamp.compare now deadline >= 0)
  then denied "Goal activation deadline reached"
  else Ok ()
;;
