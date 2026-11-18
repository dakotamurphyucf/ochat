open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module Data = Policy_canonical_usage_data
module Cause = Policy_cause_data
module Member = Cause.Member
module Work = Cause.Allocation_work
module Target = Cause.Goal_target
module Coverage = Cause.Coverage
module Context = Cause.Context
module L = Inference_ledger
module F = Canonical_usage_fold
module Canonical = Goal_descendants.Canonical
module Occurrence = Goal_descendant_occurrence
module U = Goal_usage
open Policy_canonical_usage_carrier

type t = Policy_canonical_usage_carrier.t

module Choice = Policy_canonical_usage_data.Choice

module Local_attempt = struct
  type t =
    { goal_id : P.Id.Goal.t
    ; episode : P.Goal_episode.t
    }

  let equal left right =
    P.Id.Goal.equal left.goal_id right.goal_id
    && P.Goal_episode.equal left.episode right.episode
  ;;

  let to_json t =
    `Object
      [ "goal_id", P.Id.Goal.to_json t.goal_id
      ; "episode", P.Goal_episode.to_json t.episode
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let%bind goal_id = X.required fields "goal_id" P.Id.Goal.of_json in
    let%map episode = X.required fields "episode" P.Goal_episode.of_json in
    { goal_id; episode }
  ;;

  let shape =
    X.shape_exn
      [ "goal_id", D.Shape.value
      ; ( "episode"
        , X.shape_exn
            [ "session", X.fields_shape [ "server_id"; "session_id" ]
            ; "admission", Run_record_shapes.receipt
            ] )
      ]
  ;;
end

module Basis = struct
  type t =
    { accounts : string option
    ; descendants : string option
    ; catalog : string option
    }
  [@@deriving equal]

  let capture (context : Context.t) =
    let open Result.Let_syntax in
    let digest encode value =
      Option.value_map value ~default:(Ok None) ~f:(fun value ->
        let%map raw = encode value in
        Some (Data.digest raw))
    in
    let%bind accounts = digest Goal_accounts.to_jsonaf context.accounts in
    let%bind descendants =
      digest
        (fun value -> Goal_descendants.to_document value |> Result.map ~f:D.Document.json)
        context.descendants
    in
    let%map catalog = digest Goal_catalog.to_jsonaf context.catalog in
    { accounts; descendants; catalog }
  ;;

  let to_json t =
    `Object
      [ "accounts", X.option_json (fun value -> `String value) t.accounts
      ; "descendants", X.option_json (fun value -> `String value) t.descendants
      ; "catalog", X.option_json (fun value -> `String value) t.catalog
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let digest raw = Cause.Key.of_json raw |> Result.map ~f:Cause.Key.to_string in
    let%bind accounts = X.required fields "accounts" (X.nullable digest) in
    let%bind descendants = X.required fields "descendants" (X.nullable digest) in
    let%map catalog = X.required fields "catalog" (X.nullable digest) in
    { accounts; descendants; catalog }
  ;;

  let shape = X.fields_shape [ "accounts"; "descendants"; "catalog" ]
end

let account_child (context : Context.t) goal_id =
  Option.bind context.accounts ~f:(fun accounts -> Goal_accounts.find accounts goal_id)
;;

(* This is the unique actual native Attempt advance, not a source-wide test that
   a Goal exists. Native validation below independently reconstructs retirement
   and Unknown settlement and enforces global one-increment conservation. *)
let native_attempt_advance
      ~(before : Context.t)
      ~(after : Context.t)
      ~before_ledger
      ~after_ledger
  =
  let open Result.Let_syntax in
  let%bind () =
    Goal_accounts.validate_transition
      ~previous:before.accounts
      ~incoming:after.accounts
      ~before:before_ledger
      ~after:after_ledger
      ~previous_catalog:before.catalog
      ~catalog:after.catalog
  in
  let children = Option.value_map after.accounts ~default:[] ~f:Goal_accounts.documents in
  List.fold_result children ~init:None ~f:(fun selected child ->
    let stored = Goal_accounting_document.stored child in
    let goal_id = Goal_accounting.Stored.goal_id stored in
    let old =
      Option.map (account_child before goal_id) ~f:Goal_accounting_document.stored
    in
    let attempts =
      Option.value_map old ~default:0L ~f:Goal_accounting.Stored.admitted_attempts
    in
    let delta = Int64.(Goal_accounting.Stored.admitted_attempts stored - attempts) in
    if Int64.equal delta 0L
    then Ok selected
    else if Int64.equal delta 1L && Option.is_none selected && Option.is_some old
    then Ok (Some goal_id)
    else invalid "policy native Attempt association is not a unique actual advance")
;;

let check_local_attempt
      association
      ~fact
      ~(before : Context.t)
      ~(after : Context.t)
      ~before_ledger
      ~after_ledger
  =
  let open Result.Let_syntax in
  let%bind actual = native_attempt_advance ~before ~after ~before_ledger ~after_ledger in
  match association, actual with
  | None, None -> Ok ()
  | None, Some _ | Some _, None ->
    invalid "policy local association presence differs from actual native Attempt"
  | Some local, Some actual ->
    let%bind () =
      if P.Id.Goal.equal local.Local_attempt.goal_id actual
      then Ok ()
      else invalid "policy local association names another native partition"
    in
    let%bind goal =
      Result.of_option
        (Option.bind before.catalog ~f:(fun catalog -> Goal_catalog.find catalog actual))
        ~error:(P.Error.invalid_request "policy local Goal absent at admission prefix")
    in
    let source = (P.Goal_episode.admission local.episode).source in
    let%bind () =
      if
        Option.exists (P.Goal.episode goal) ~f:(P.Goal_episode.equal local.episode)
        && P.Session_ref.equal
             (P.Goal.session goal)
             (P.Goal_episode.session local.episode)
        && P.Run_source.equal (P.Goal.source goal) source
        && Int.equal source.generation (L.generation after_ledger)
      then Ok ()
      else invalid "policy local Attempt episode/source differs"
    in
    let%bind child =
      Result.of_option
        (account_child after actual)
        ~error:(P.Error.invalid_request "policy local after-account absent")
    in
    let%bind previous =
      Result.of_option
        (account_child before actual)
        ~error:(P.Error.invalid_request "policy local before-account absent")
    in
    let old = Goal_accounting_document.stored previous in
    let current = Goal_accounting_document.stored child in
    let%bind ordinal = Data.ordinal fact in
    (match Canonical.Fact.tracking fact with
     | L.Tracked ->
       let%bind resolved =
         Goal_accounting_document.resolve child ~goal ~ledger:after_ledger
       in
       let admission = P.Goal_episode.admission local.episode in
       if
         List.exists (Goal_accounting.bindings resolved) ~f:(fun binding ->
           Int64.equal ordinal (L.Handle.ordinal (Goal_accounting.Binding.handle binding))
           && P.Id.Run.equal admission.run_id (Goal_accounting.Binding.run_id binding)
           && P.Run_source.equal source (Goal_accounting.Binding.source binding))
       then Ok ()
       else invalid "policy tracked local Attempt lacks same-H native binding"
     | Untracked (Attempt_count | Retained_bytes | Protected_future_data) ->
       let%bind expected =
         Data.increment (Goal_accounting.Stored.untracked_attempts old)
       in
       if
         Int64.equal expected (Goal_accounting.Stored.untracked_attempts current)
         && not
              (List.mem
                 (Goal_accounting.Stored.ordinals current)
                 ordinal
                 ~equal:Int64.equal)
       then Ok ()
       else invalid "policy Untracked local Attempt lacks exact native Unknown advance"
     | Untracked Turn_count -> invalid "policy Attempt cannot use Turn tracking limit")
;;

let physical_coverage (context : Context.t) target occurrence =
  match Target.view target, context.descendants with
  | Local _, _ | Inherited _, None -> false
  | Inherited scope, Some descendants ->
    Option.exists (Goal_descendants.origin descendants) ~f:(fun origin ->
      List.exists
        (Agent_store.Delegation_goal_origin.scopes origin)
        ~f:(Agent_store.Delegation_goal_origin.Scope.equal scope))
    && List.exists (Goal_descendants.local_included descendants) ~f:(fun included ->
      let pending = Goal_descendant_included.pending included in
      Agent_store.Delegation_goal_origin.Scope.equal
        scope
        (Goal_descendant_pending.scope pending)
      && Occurrence.equal occurrence (Goal_descendant_pending.occurrence pending))
;;

(* Native validation proves the actual current episode and same-H admission.
   Coverage belongs to the stable owner/Goal partition: the incoming cause's
   frozen creator episode must not charge that native Handle a second time. *)
let coverage_choices cause association context fact =
  List.map (Cause.targets cause) ~f:(fun target ->
    let covered_local =
      Option.exists association ~f:(fun local ->
        P.Id.Goal.equal local.Local_attempt.goal_id (Target.goal_id target)
        && P.Session_ref.equal
             (P.Goal_episode.session local.episode)
             (Target.owner target))
    in
    let coverage =
      if covered_local
      then Coverage.Covered_local
      else if physical_coverage context target (Canonical.Fact.occurrence fact)
      then Covered_physical
      else Causal_overlay
    in
    { Data.Choice.target; coverage })
;;

module Selection = struct
  type accounting = t

  type t =
    { cause : Cause.Key.t
    ; work : Work.t
    ; fact : Canonical.Fact.t
    ; local_attempt : Local_attempt.t option
    ; basis : Basis.t
    ; choices : Choice.t list
    }

  let choices t = t.choices

  let matches_subject t ~cause ~work ~occurrence =
    Cause.Key.equal t.cause cause
    && Work.equal t.work work
    && Occurrence.equal (Canonical.Fact.occurrence t.fact) occurrence
  ;;

  let check_work accounting ~cause ~work ~fact (context : Context.t) =
    let open Result.Let_syntax in
    let%bind () =
      if
        Option.exists
          context.current_source
          ~f:(P.Run_source.equal (Cause.callee_source cause))
      then Ok ()
      else invalid "policy allocation source is not the actual current installation"
    in
    let%bind member =
      Work.member work ~generation:(Cause.callee_source cause).generation
    in
    let%bind retained =
      Result.of_option
        (Data.find_member accounting member)
        ~error:(P.Error.invalid_request "policy allocation work has no birth membership")
    in
    let%bind current =
      Data.Source_work.capture ~memberships:accounting.memberships context cause member
    in
    let%bind current =
      Result.of_option
        current
        ~error:(P.Error.invalid_request "policy allocation work is absent")
    in
    let%bind () =
      if
        Cause.Key.equal retained.cause (Cause.key cause)
        && Data.Source_work.matches retained current
        && current.ready
      then Ok ()
      else invalid "policy allocation work is not this live caused prefix"
    in
    let%bind () =
      match work, current.attempt with
      | ( (Work.Model_job_attempt { attempt; _ } | Async_job_attempt { attempt; _ })
        , Some actual )
        when Int.equal attempt actual -> Ok ()
      | (Event _ | Invocation _), None -> Ok ()
      | (Model_job_attempt _ | Async_job_attempt _), (None | Some _)
      | (Event _ | Invocation _), Some _ ->
        invalid "policy allocation Job attempt differs"
    in
    match Occurrence.kind (Canonical.Fact.occurrence fact) with
    | Host_turn _ -> invalid "policy allocation requires Attempt H"
    | Attempt { operation_id; invocation_id; _ } ->
      if
        Option.equal P.Id.Operation.equal operation_id current.operation_id
        && Option.equal P.Id.Invocation.equal invocation_id current.invocation_id
      then Ok ()
      else invalid "policy allocation native operation/Invocation relation differs"
  ;;

  let verify t accounting ~live_prefix ~coverage_after ~before_ledger ~after_ledger =
    let open Result.Let_syntax in
    let%bind () =
      Canonical.validate_fact ~before:before_ledger ~after:after_ledger ~fact:t.fact
    in
    let%bind cause =
      Result.of_option
        (Data.find_cause (value accounting) t.cause)
        ~error:(P.Error.invalid_request "policy allocation cause absent")
    in
    let%bind () =
      check_work (value accounting) ~cause ~work:t.work ~fact:t.fact live_prefix
    in
    let%bind () =
      check_local_attempt
        t.local_attempt
        ~fact:t.fact
        ~before:live_prefix
        ~after:coverage_after
        ~before_ledger
        ~after_ledger
    in
    let%bind basis = Basis.capture coverage_after in
    if
      Basis.equal t.basis basis
      && List.equal
           Choice.equal
           t.choices
           (coverage_choices cause t.local_attempt coverage_after t.fact)
    then Ok ()
    else invalid "policy allocation coverage differs from actual native prefix"
  ;;

  let prepare
        accounting
        ~cause
        ~work
        ~live_prefix
        ~coverage_after
        ~native_admission
        ~before_ledger
        ~after_ledger
        ~canonical
        ~occurrence
    =
    let open Result.Let_syntax in
    let%bind () = exact_current accounting ~context:live_prefix ~ledger:before_ledger in
    let%bind fact =
      Canonical.check ~before:before_ledger ~after:after_ledger ~canonical ~occurrence
    in
    let%bind (_ : int64) = Data.ordinal fact in
    let%bind local_attempt =
      match native_admission, canonical with
      | None, _ -> Ok None
      | ( Some (goal_id, Goal_accounting.Admission.Attempt { episode; handle; tracking })
        , Canonical.Attempt (actual, actual_tracking) ) ->
        if L.Handle.equal handle actual && L.equal_tracking tracking actual_tracking
        then Ok (Some { Local_attempt.goal_id; episode })
        else invalid "policy native admission is not the same canonical Attempt"
      | Some (_, Turn _), _ | Some (_, Attempt _), Host_turn _ ->
        invalid "policy native admission dimension differs"
    in
    let%bind actual_cause =
      Result.of_option
        (Data.find_cause (value accounting) cause)
        ~error:(P.Error.invalid_request "policy allocation cause absent")
    in
    let%bind basis = Basis.capture coverage_after in
    let t =
      { cause
      ; work
      ; fact
      ; local_attempt
      ; basis
      ; choices = coverage_choices actual_cause local_attempt coverage_after fact
      }
    in
    let%map () =
      verify t accounting ~live_prefix ~coverage_after ~before_ledger ~after_ledger
    in
    t
  ;;

  let to_json t =
    `Object
      [ "cause", Cause.Key.to_json t.cause
      ; "work", Work.to_json t.work
      ; "fact", Canonical.Fact.to_json t.fact
      ; "local_attempt", X.option_json Local_attempt.to_json t.local_attempt
      ; "basis", Basis.to_json t.basis
      ; "choices", `Array (List.map t.choices ~f:Choice.to_json)
      ]
  ;;

  let equal left right = D.Json.equal (to_json left) (to_json right)

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let%bind cause = X.required fields "cause" Cause.Key.of_json in
    let%bind work = X.required fields "work" Work.of_json in
    let%bind fact = X.required fields "fact" Canonical.Fact.of_json in
    let%bind (_ : int64) = Data.ordinal fact in
    let%bind local_attempt =
      X.required fields "local_attempt" (X.nullable Local_attempt.of_json)
    in
    let%bind basis = X.required fields "basis" Basis.of_json in
    let%map choices = X.required fields "choices" (X.list Choice.of_json) in
    { cause; work; fact; local_attempt; basis; choices }
  ;;

  let shape =
    X.shape_exn
      [ "cause", D.Shape.value
      ; "work", Work.shape
      ; "fact", Canonical.Fact.shape
      ; "local_attempt", D.Shape.nullable Local_attempt.shape
      ; "basis", Basis.shape
      ; "choices", X.array_shape_exn Choice.shape
      ]
  ;;
end

module Admission = struct
  type accounting = t

  type t =
    { cause : Cause.Key.t
    ; work : Work.t
    ; live_prefix : Context.t
    ; descendants_after : Goal_descendants.t option
    ; canonical : Canonical.t
    ; occurrence : Occurrence.t
    }

  let create ~cause ~work ~live_prefix ~descendants_after ~canonical ~occurrence =
    match canonical, Occurrence.kind occurrence with
    | Canonical.Attempt _, Occurrence.Kind.Attempt _ ->
      Ok { cause; work; live_prefix; descendants_after; canonical; occurrence }
    | Host_turn _, _ | Attempt _, Host_turn _ ->
      invalid "policy admission requires Attempt dimension"
  ;;

  let select t accounting ~accounts ~native_admission ~before ~after =
    let open Result.Let_syntax in
    let prefix = t.live_prefix in
    let%bind coverage_after =
      Context.create
        ~session_id:prefix.session_id
        ~generation:prefix.generation
        ~current_source:prefix.current_source
        ~events:prefix.events
        ~invocations:prefix.invocations
        ~jobs:prefix.jobs
        ~runs:prefix.runs
        ~accounts
        ~catalog:prefix.catalog
        ~descendants:t.descendants_after
    in
    Selection.prepare
      accounting
      ~cause:t.cause
      ~work:t.work
      ~live_prefix:prefix
      ~coverage_after
      ~native_admission
      ~before_ledger:before
      ~after_ledger:after
      ~canonical:t.canonical
      ~occurrence:t.occurrence
  ;;
end
