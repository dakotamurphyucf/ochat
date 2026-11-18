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

let token_decision ?(coverage_unknown = false) usage (stop : G.Budget.Token_stop.t) =
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
  else if coverage_unknown || unknown metric
  then Hold (Usage_uncertain stop.component)
  else Allow
;;

let account_usage goal ~accounting:account ~ledger =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Id.Goal.equal (A.Scope.goal_id (A.scope account)) (G.id goal)
      && P.Session_ref.equal (A.Scope.session (A.scope account)) (G.session goal)
    then Ok ()
    else Error (P.Error.invalid_request "goal admission accounting scope differs")
  in
  A.usage account ~ledger |> accounting
;;

let control_deadline goal ~now =
  match G.state goal with
  | Closed _ -> Error (P.Error.invalid_request "closed goal cannot admit new work")
  | Open (Held hold) -> Ok (Some (Hold hold))
  | Open Enabled ->
    if
      Option.exists
        (G.Budget.deadline (G.budget goal))
        ~f:(fun deadline -> P.Timestamp.compare now deadline >= 0)
    then Ok (Some (Hold Deadline_reached))
    else Ok None
;;

let observed_tokens goal usage ~coverage_unknown =
  Option.value_map
    (G.Budget.observed_tokens (G.budget goal))
    ~default:Allow
    ~f:(token_decision ~coverage_unknown usage)
;;

let check_policy goal ~accounting:account ~ledger ~now ~dimension =
  let open Result.Let_syntax in
  (* Account integrity precedes control even for Held/Closed Goals. *)
  let%bind usage = account_usage goal ~accounting:account ~ledger in
  let%map control = control_deadline goal ~now in
  match control with
  | Some decision -> decision
  | None ->
    let budget = G.budget goal in
    let count_hold =
      Option.bind dimension ~f:(fun dimension ->
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
        then Some (G.Hold.Budget_gate budget_dimension)
        else None)
    in
    (match count_hold with
     | Some hold -> Hold hold
     | None -> observed_tokens goal usage ~coverage_unknown:false)
;;

let check goal ~accounting ~ledger ~now ~dimension =
  check_policy goal ~accounting ~ledger ~now ~dimension:(Some dimension)
;;

let check_activation goal ~accounting ~ledger ~now =
  check_policy goal ~accounting ~ledger ~now ~dimension:None
;;

let same_allocation_policy before after =
  G.Budget.equal (G.budget before) (G.budget after)
  && P.Run_source.equal (G.source before) (G.source after)
  && G.Objective.equal (G.objective before) (G.objective after)
  && Option.equal P.Goal_episode.equal (G.episode before) (G.episode after)
;;

module Descendants = struct
  type t =
    { usage : U.t
    ; coverage_unknown : bool
    }
end

module Count_availability = struct
  type t =
    | Available
    | Reserved
    | Exhausted
    | Unknown
  [@@deriving equal, sexp_of]
end

module Deferred = struct
  type t =
    | Reserved
    | Unknown
  [@@deriving equal, sexp_of]
end

type scoped_decision =
  | Decided of decision
  | Deferred of Deferred.t

let scoped_usage goal ~accounting ~ledger (descendants : Descendants.t) =
  let%bind.Result own = account_usage goal ~accounting ~ledger in
  U.add_disjoint own descendants.usage
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (U.Counter.sexp_of_error error)))
;;

let check_scoped goal ~accounting ~ledger ~descendants ~availability ~now ~dimension =
  let open Result.Let_syntax in
  let%bind usage = scoped_usage goal ~accounting ~ledger descendants in
  let%map control = control_deadline goal ~now in
  match control with
  | Some decision -> Decided decision
  | None ->
    (match (availability : Count_availability.t) with
     | Reserved -> Deferred Reserved
     | Unknown -> Deferred Unknown
     | Exhausted ->
       Decided
         (Hold
            (Budget_gate
               (match dimension with
                | Attempt -> Admitted_attempts
                | Host_turn -> Admitted_turns)))
     | Available ->
       Decided (observed_tokens goal usage ~coverage_unknown:descendants.coverage_unknown))
;;

let check_scoped_activation goal ~accounting ~ledger ~descendants ~now =
  let open Result.Let_syntax in
  let%bind usage = scoped_usage goal ~accounting ~ledger descendants in
  let%map control = control_deadline goal ~now in
  match control with
  | Some decision -> decision
  | None -> observed_tokens goal usage ~coverage_unknown:descendants.coverage_unknown
;;
