open! Core
module P = Agent_protocol
module G = P.Goal
module A = Goal_accounting
module U = Goal_usage

type dimension =
  | Attempt
  | Host_turn
[@@deriving equal, sexp_of]

type decision =
  | Allow
  | Hold of G.Hold.t
[@@deriving equal, sexp_of]

let accounting result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (A.Error.sexp_of_t error)))
;;

let unknown metric =
  let counts = U.Metric.unknown metric in
  Int64.(U.Metric.untracked_attempts metric > 0L)
  || List.exists
       Inference.Observation.Count.
         [ Not_reported; Explicit_null; Interrupted; Not_submitted; Before_tracking ]
       ~f:(fun reason -> Int64.(U.Unknown.count counts reason > 0L))
;;

let token_decision usage (stop : G.Budget.Token_stop.t) =
  let metric = U.component usage stop.component in
  let reached =
    match U.Metric.actual metric, U.Metric.estimated metric with
    | Overflow, _ | _, Overflow -> true
    | Tokens actual, Tokens estimated ->
      (* Nonnegative sums permit comparison without overflowing their sum. *)
      Int64.(actual >= stop.ceiling || estimated >= stop.ceiling - actual)
  in
  if reached
  then Hold (Budget_gate Observed_tokens)
  else if unknown metric
  then Hold (Usage_uncertain stop.component)
  else Allow
;;

let check goal ~accounting:account ~ledger ~now ~dimension =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Id.Goal.equal (A.Scope.goal_id (A.scope account)) (G.id goal)
      && P.Session_ref.equal (A.Scope.session (A.scope account)) (G.session goal)
    then Ok ()
    else Error (P.Error.invalid_request "goal admission accounting scope differs")
  in
  let%bind usage = A.usage account ~ledger |> accounting in
  match G.state goal with
  | Closed _ -> Error (P.Error.invalid_request "closed goal cannot admit new work")
  | Open (Held hold) -> Ok (Hold hold)
  | Open Enabled ->
    let budget = G.budget goal in
    if
      Option.exists (G.Budget.deadline budget) ~f:(fun deadline ->
        P.Timestamp.compare now deadline >= 0)
    then Ok (Hold Deadline_reached)
    else (
      let count, ceiling, budget_dimension =
        match dimension with
        | Attempt ->
          ( A.admitted_attempts account
          , G.Budget.admitted_attempts budget
          , G.Budget.Dimension.Admitted_attempts )
        | Host_turn ->
          ( A.admitted_turns account
          , G.Budget.admitted_turns budget
          , G.Budget.Dimension.Admitted_turns )
      in
      if Option.exists ceiling ~f:(fun ceiling -> Int64.(count >= ceiling))
      then Ok (Hold (Budget_gate budget_dimension))
      else
        Ok
          (Option.value_map
             (G.Budget.observed_tokens budget)
             ~default:Allow
             ~f:(token_decision usage)))
;;

let same_allocation_policy before after =
  G.Budget.equal (G.budget before) (G.budget after)
  && P.Run_source.equal (G.source before) (G.source after)
  && G.Objective.equal (G.objective before) (G.objective after)
  && Option.equal P.Goal_episode.equal (G.episode before) (G.episode after)
;;
