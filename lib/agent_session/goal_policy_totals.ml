open! Core
module P = Agent_protocol
module PC = Policy_canonical_usage
module G = P.Goal
module Causal = Goal_policy_contributions
module Policy = Goal_admission_policy
module Occurrence = Goal_descendant_occurrence
module Snapshot = Child_usage_snapshot
module R = Goal_policy_receipt

let invalid = R.invalid

let counter result =
  Result.map_error result ~f:(fun (_ : Goal_accounting_counter.error) ->
    P.Error.invalid_request "combined Goal usage count exhausted")
;;

let accounting result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (Goal_accounting.Error.sexp_of_t error)))
;;

module Context = struct
  type t =
    { owner : P.Session_ref.t
    ; catalog : Goal_catalog.t option
    ; accounts : Goal_accounts.t option
    ; ledger : Inference_ledger.t
    ; descendants : Goal_descendants.t option
    ; children : Child_contributions.t option
    ; causal : Causal.t option
    ; canonical : PC.t option
    ; session_revision : int64
    }

  let create
        ~owner
        ~catalog
        ~accounts
        ~ledger
        ~descendants
        ~children
        ~causal
        ~canonical
        ~session_revision
    =
    if
      Int64.(session_revision >= 0L)
      && P.Id.Session.equal
           (P.Session_ref.session_id owner)
           (Inference_ledger.session_id ledger)
      && Option.value_map descendants ~default:true ~f:(fun descendants ->
        P.Session_ref.equal owner (Goal_descendants.session descendants))
    then
      Ok
        { owner
        ; catalog
        ; accounts
        ; ledger
        ; descendants
        ; children
        ; causal
        ; canonical
        ; session_revision
        }
    else invalid "combined Goal context owner or revision differs"
  ;;
end

module View = struct
  type t =
    { goal : G.t
    ; accounting : Goal_accounting.t
    ; ledger : Inference_ledger.t
    ; local : Goal_usage.t
    ; physical : Goal_usage.t
    ; causal : Goal_usage.t
    ; total : Goal_usage.t
    ; descendants_usage : Goal_usage.t
    ; causal_unknown : bool
    ; physical_unknown_ : bool
    ; physical_count_known : bool
    ; attempts : int64
    ; turns : int64
    ; pending_attempts : int64
    ; pending_turns : int64
    }

  let local_usage t = t.local
  let physical_usage t = t.physical
  let causal_usage t = t.causal
  let total_usage t = t.total
  let historical_causal_unknown t = t.causal_unknown
  let physical_unknown t = t.physical_unknown_

  let pending t ~dimension =
    match dimension with
    | Policy.Attempt -> t.pending_attempts
    | Host_turn -> t.pending_turns
  ;;

  let committed t ~dimension =
    match dimension with
    | Policy.Attempt -> t.attempts
    | Host_turn -> t.turns
  ;;
end

let occurrence_key occurrence =
  let frame = Occurrence.frame occurrence in
  let kind =
    match Occurrence.kind occurrence with
    | Attempt { ordinal; operation_id = _; invocation_id = _ } ->
      `Object
        [ "kind", `String "attempt"; "ordinal", Persistence_codec.int64_json ordinal ]
    | Host_turn { operation_id } ->
      `Object
        [ "kind", `String "host_turn"; "operation", P.Id.Operation.to_json operation_id ]
  in
  Jsonaf.to_string
    (`Object
        [ "session", P.Session_ref.to_json (Occurrence.Frame.session frame)
        ; ( "generation"
          , Persistence_codec.host_counter_to_json (Occurrence.Frame.generation frame) )
        ; "kind", kind
        ])
;;

let pending_counts occurrences =
  let open Result.Let_syntax in
  let%bind unique =
    List.fold_result occurrences ~init:String.Map.empty ~f:(fun map occurrence ->
      let key = occurrence_key occurrence in
      match Map.find map key with
      | Some previous when Occurrence.equal previous occurrence -> Ok map
      | Some _ ->
        invalid
          "same governing Goal pending canonical identity has contradictory occurrence"
      | None -> Ok (Map.set map ~key ~data:occurrence))
  in
  Map.fold
    unique
    ~init:(Ok (0L, 0L))
    ~f:(fun ~key:_ ~data:occurrence result ->
      let%bind attempts, turns = result in
      match Occurrence.dimension occurrence with
      | Attempt ->
        let%map attempts = R.add attempts 1L in
        attempts, turns
      | Host_turn ->
        let%map turns = R.add turns 1L in
        attempts, turns)
;;

let physical (context : Context.t) goal =
  match context.children with
  | Some children ->
    Child_contributions.aggregate_root children ~owner:context.owner ~goal_id:(G.id goal)
  | None ->
    let%map.Result coverage =
      Snapshot.Coverage.create ~historical_children_unknown:true ~unobserved_children:0L
    in
    Snapshot.Descendants.create ~partition:Snapshot.Partition.empty ~coverage
;;

let capture (context : Context.t) ~goal_id =
  let open Result.Let_syntax in
  let%bind goal =
    Option.bind context.catalog ~f:(fun catalog -> Goal_catalog.find catalog goal_id)
    |> Result.of_option
         ~error:(P.Error.invalid_request "combined Goal view has no actual Goal")
  in
  let%bind document =
    Option.bind context.accounts ~f:(fun accounts -> Goal_accounts.find accounts goal_id)
    |> Result.of_option
         ~error:(P.Error.invalid_request "combined Goal view has no actual account")
  in
  let%bind () =
    if P.Session_ref.equal context.owner (G.session goal)
    then Ok ()
    else invalid "combined Goal has another full owner"
  in
  let%bind account =
    Goal_accounting_document.resolve document ~goal ~ledger:context.ledger
  in
  let%bind local = Goal_accounting.usage account ~ledger:context.ledger |> accounting in
  let%bind descendants = physical context goal in
  let partition = Snapshot.Descendants.partition descendants in
  let physical = Snapshot.Partition.usage partition in
  let physical_marks = Snapshot.Partition.marks partition in
  let coverage = Snapshot.Descendants.coverage descendants in
  let physical_unknown_ =
    Snapshot.Coverage.historical_children_unknown coverage
    || Int64.(Snapshot.Coverage.unobserved_children coverage > 0L)
  in
  let%bind physical_count_known, physical_pending =
    match context.descendants, context.children with
    | Some descendants, Some children
      when List.mem (Goal_descendants.roots descendants) goal_id ~equal:P.Id.Goal.equal ->
      let%map (_ : Snapshot.Coverage.t) =
        Goal_descendants.accounting_coverage descendants ~children ~goal_id
      in
      ( true
      , List.filter_map (Goal_descendants.branches descendants ~goal_id) ~f:(fun branch ->
          Option.map
            (Goal_descendant_branch.pending branch)
            ~f:Goal_descendant_pending.occurrence) )
    | (None | Some _), (None | Some _) -> Ok (false, [])
  in
  let%bind subtotal =
    Causal.subtotal
      context.causal
      ~owner:context.owner
      ~goal_id
      ~catalog:context.catalog
      ~accounts:context.accounts
      ~canonical:context.canonical
      ~ledger:context.ledger
      ~session_revision:context.session_revision
  in
  let causal = Causal.Subtotal.usage subtotal in
  let causal_unknown =
    Causal.History.equal (Causal.Subtotal.history subtotal) Historical_unknown
  in
  let%bind descendants_usage = Goal_usage.add_disjoint physical causal |> counter in
  let%bind total = Goal_usage.add_disjoint local descendants_usage |> counter in
  let%bind attempts =
    R.add (Goal_accounting.admitted_attempts account) physical_marks.attempt
  in
  let%bind attempts = R.add attempts (Goal_usage.attempts causal) in
  let%bind turns = R.add (Goal_accounting.admitted_turns account) physical_marks.turn in
  let%bind pending_attempts, pending_turns =
    pending_counts (physical_pending @ Causal.Subtotal.pending subtotal)
  in
  Ok
    { View.goal
    ; accounting = account
    ; ledger = context.ledger
    ; local
    ; physical
    ; causal
    ; total
    ; descendants_usage
    ; causal_unknown
    ; physical_unknown_
    ; physical_count_known
    ; attempts
    ; turns
    ; pending_attempts
    ; pending_turns
    }
;;

let check_goal (view : View.t) goal =
  if G.equal view.goal goal
  then Ok ()
  else invalid "combined Goal view differs from supplied current Goal policy"
;;

let available (view : View.t) ~context ~goal ~dimension =
  let open Result.Let_syntax in
  let%bind () = check_goal view goal in
  let ceiling =
    match dimension with
    | Policy.Attempt -> G.Budget.admitted_attempts (G.budget goal)
    | Host_turn -> G.Budget.admitted_turns (G.budget goal)
  in
  match ceiling with
  | None -> Ok Policy.Count_availability.Available
  | Some ceiling ->
    let committed = View.committed view ~dimension in
    let%map occupied = R.add committed (View.pending view ~dimension) in
    let causal_unknown =
      match context, dimension with
      | Causal.Admission_context.Delegated_policy_causal, Policy.Attempt ->
        view.causal_unknown
      | Ordinary_existing, (Attempt | Host_turn) | Delegated_policy_causal, Host_turn ->
        false
    in
    if Int64.(committed >= ceiling)
    then Policy.Count_availability.Exhausted
    else if Int64.(occupied >= ceiling)
    then Reserved
    else if view.physical_unknown_ || (not view.physical_count_known) || causal_unknown
    then Unknown
    else Available
;;

let policy_descendants (view : View.t) context =
  let causal_unknown =
    match context with
    | Causal.Admission_context.Ordinary_existing -> false
    | Delegated_policy_causal -> view.causal_unknown
  in
  Policy.Descendants.
    { usage = view.descendants_usage
    ; coverage_unknown = view.physical_unknown_ || causal_unknown
    }
;;

let check context ~goal ~context:admission_context ~now ~dimension =
  let open Result.Let_syntax in
  let%bind view = capture context ~goal_id:(G.id goal) in
  let%bind availability = available view ~context:admission_context ~goal ~dimension in
  Policy.check_scoped
    goal
    ~accounting:view.accounting
    ~ledger:view.ledger
    ~descendants:(policy_descendants view admission_context)
    ~availability
    ~now
    ~dimension
;;

let activation (view : View.t) ~goal ~context ~now =
  Policy.check_scoped_activation
    goal
    ~accounting:view.accounting
    ~ledger:view.ledger
    ~descendants:(policy_descendants view context)
    ~now
;;

let check_activation context ~goal ~context:admission_context ~now =
  let open Result.Let_syntax in
  let%bind view = capture context ~goal_id:(G.id goal) in
  let%bind () = check_goal view goal in
  activation view ~goal ~context:admission_context ~now
;;

let observe_activation (view : View.t) ~now =
  let open Result.Let_syntax in
  let%bind goal =
    match G.state view.goal with
    | Open Enabled -> Ok view.goal
    | Open (Held _) | Closed _ -> G.resume view.goal
  in
  activation view ~goal ~context:Ordinary_existing ~now
;;
