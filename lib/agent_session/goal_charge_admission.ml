open! Core
module P = Agent_protocol
module G = P.Goal

type ready = Goal_work_attribution.t

let goal_id = Goal_work_attribution.goal_id
let episode = Goal_work_attribution.episode

type deferred =
  { goal_id : P.Id.Goal.t
  ; reason : Goal_admission_policy.Deferred.t
  }

type decision =
  | Unbound
  | Ready of ready
  | Deferred of deferred
  | Held of
      { goal_id : P.Id.Goal.t
      ; reason : G.Hold.t
      ; delta : Session_delta.t option
      }

let hold_delta (state : Session_state.t) ~goal_id:id ~reason =
  let open Result.Let_syntax in
  match state.goal_catalog with
  | None -> Ok None
  | Some catalog ->
    (match Goal_catalog.find catalog id with
     | None -> Ok None
     | Some goal ->
       (match G.state goal with
        | Open (Held _) -> Ok None
        | Closed _ ->
          Error (P.Error.invalid_request "closed Goal cannot acquire a policy hold")
        | Open Enabled ->
          let%bind held = G.hold goal reason in
          let%bind document =
            Result.of_option
              (Goal_catalog.document catalog id)
              ~error:(P.Error.invalid_request "associated goal document is absent")
          in
          let%bind document =
            Goal_document.update document held
            |> Result.map_error ~f:(fun e ->
              P.Error.invalid_request
                (Sexp.to_string_hum (Document_schema.Error.sexp_of_t e)))
          in
          let%map catalog = Goal_catalog.advance_existing catalog ~goal:document in
          Some (Session_delta.Goal_catalog_changed catalog)))
;;

let descendants (state : Session_state.t) goal =
  match state.child_contributions with
  | Some children ->
    Child_contributions.aggregate_root
      children
      ~owner:(G.session goal)
      ~goal_id:(G.id goal)
  | None ->
    let%map.Result coverage =
      Child_usage_snapshot.Coverage.create
        ~historical_children_unknown:true
        ~unobserved_children:0L
    in
    (* A known empty subtotal with Unknown coverage is not historical zero. *)
    Child_usage_snapshot.Descendants.create
      ~partition:Child_usage_snapshot.Partition.empty
      ~coverage
;;

let policy_descendants descendants =
  let module S = Child_usage_snapshot in
  let coverage = S.Descendants.coverage descendants in
  Goal_admission_policy.Descendants.
    { usage = S.Partition.usage (S.Descendants.partition descendants)
    ; coverage_unknown =
        (S.Coverage.historical_children_unknown coverage
         || Int64.(S.Coverage.unobserved_children coverage > 0L))
    }
;;

let check_activation (state : Session_state.t) ~goal ~accounting ~now =
  let%bind.Result descendants = descendants state goal in
  Goal_admission_policy.check_scoped_activation
    goal
    ~accounting
    ~ledger:state.inference_ledger
    ~descendants:(policy_descendants descendants)
    ~now
;;

let count_availability (state : Session_state.t) goal account descendants dimension =
  let open Result.Let_syntax in
  let module Policy = Goal_admission_policy in
  let local_count, ceiling =
    match (dimension : Policy.dimension) with
    | Attempt ->
      ( Goal_accounting.admitted_attempts account
      , G.Budget.admitted_attempts (G.budget goal) )
    | Host_turn ->
      Goal_accounting.admitted_turns account, G.Budget.admitted_turns (G.budget goal)
  in
  match state.goal_descendants with
  | Some accounting
    when List.mem (Goal_descendants.roots accounting) (G.id goal) ~equal:P.Id.Goal.equal
    ->
    let%bind () =
      if P.Session_ref.equal (Goal_descendants.session accounting) (G.session goal)
      then Ok ()
      else Error (P.Error.invalid_request "Goal descendant owner differs from local Goal")
    in
    let%bind children =
      Result.of_option
        state.child_contributions
        ~error:(P.Error.invalid_request "Goal descendant root has no child component")
    in
    let%map availability =
      Goal_descendants.available
        accounting
        ~children
        ~goal_id:(G.id goal)
        ~local_count
        ~ceiling
        ~dimension
    in
    (match availability with
     | Available -> Policy.Count_availability.Available
     | Reserved -> Reserved
     | Exhausted -> Exhausted
     | Unknown -> Unknown)
  | None | Some _ ->
    (* An old Goal has no authenticated descendant frontier. Preserve any known
       committed lower bound, but never bootstrap a root or infer no pending. *)
    let marks =
      Child_usage_snapshot.Partition.marks
        (Child_usage_snapshot.Descendants.partition descendants)
    in
    let descendant_count =
      match dimension with
      | Attempt -> marks.attempt
      | Host_turn -> marks.turn
    in
    let module Counter = Goal_accounting_counter in
    let counter result =
      Result.map_error result ~f:(fun error ->
        P.Error.invalid_request (Sexp.to_string_hum (Counter.sexp_of_error error)))
    in
    let%bind local = Counter.of_int64 local_count |> counter in
    let%bind descendants = Counter.of_int64 descendant_count |> counter in
    let%map committed = Counter.add local descendants |> counter in
    (match ceiling with
     | None -> Policy.Count_availability.Available
     | Some ceiling ->
       if Int64.(Counter.to_int64 committed >= ceiling) then Exhausted else Unknown)
;;

let prepare (state : Session_state.t) ~attribution ~now ~dimension =
  let open Result.Let_syntax in
  match attribution with
  | None -> Ok Unbound
  | Some attribution ->
    let id = goal_id attribution in
    let%bind catalog =
      Result.of_option
        state.goal_catalog
        ~error:(P.Error.invalid_request "goal catalog is absent at charge admission")
    in
    let%bind goal =
      Result.of_option
        (Goal_catalog.find catalog id)
        ~error:(P.Error.invalid_request "associated goal is absent")
    in
    let%bind child =
      Option.bind state.goal_accounts ~f:(fun accounts -> Goal_accounts.find accounts id)
      |> Result.of_option
           ~error:
             (P.Error.invalid_request
                "governed admission requires existing pre-episode accounting")
    in
    let%bind account =
      Goal_accounting_document.resolve child ~goal ~ledger:state.inference_ledger
    in
    let%bind descendants = descendants state goal in
    let%bind availability = count_availability state goal account descendants dimension in
    let%bind decision =
      Goal_admission_policy.check_scoped
        goal
        ~accounting:account
        ~ledger:state.inference_ledger
        ~descendants:(policy_descendants descendants)
        ~availability
        ~now
        ~dimension
    in
    (match decision with
     | Decided Allow -> Ok (Ready attribution)
     | Deferred reason -> Ok (Deferred { goal_id = id; reason })
     | Decided (Hold reason) ->
       let%map delta = hold_delta state ~goal_id:id ~reason in
       Held { goal_id = id; reason; delta })
;;
