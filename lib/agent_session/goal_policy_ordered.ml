open! Core
module P = Agent_protocol
module G = P.Goal
module Book = Goal_policy_contributions
module PC = Policy_canonical_usage
module Totals = Goal_policy_totals
module L = Inference_ledger
module Accounts = Goal_accounts
module Bootstrap = Accounts.Ordered_bootstrap
module Stored = Goal_accounting.Stored
module D = Document_schema
module Scope = Agent_store.Delegation_goal_origin.Scope

let invalid message = Error (P.Error.invalid_request message)

let equal_optional left right ~encode =
  match left, right with
  | None, None -> Ok true
  | None, Some _ | Some _, None -> Ok false
  | Some left, Some right ->
    let%bind.Result left = encode left in
    let%map.Result right = encode right in
    D.Json.equal left right
;;

let book_json book = Book.to_document book |> Result.map ~f:D.Document.json
let canonical_json canonical = PC.to_document canonical |> Result.map ~f:D.Document.json

let descendants_json descendants =
  Goal_descendants.to_document descendants |> Result.map ~f:D.Document.json
;;

let ledger_json ledger =
  L.to_document ledger
  |> Result.map ~f:D.Document.json
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (L.Error.sexp_of_t error)))
;;

module Projection = struct
  type t =
    { session_id : P.Id.Session.t
    ; session_revision : int64
    ; owner : P.Session_ref.t option
    ; catalog : Goal_catalog.t option
    ; accounts : Accounts.t option
    ; ledger : L.t
    ; descendants : Goal_descendants.t option
    ; children : Child_contributions.t option
    ; causal : Book.t option
    ; canonical : PC.t option
    }

  let owner t = t.owner

  let create
        ~session_id
        ~session_revision
        ~catalog
        ~accounts
        ~ledger
        ~descendants
        ~children
        ~causal
        ~canonical
    =
    let open Result.Let_syntax in
    let facts =
      Option.value_map catalog ~default:[] ~f:(fun catalog ->
        List.map (Goal_catalog.goals catalog) ~f:G.session)
      @ Option.to_list (Option.map descendants ~f:Goal_descendants.session)
      @ Option.to_list (Option.map causal ~f:Book.owner)
      @ Option.to_list (Option.map canonical ~f:PC.callee)
    in
    let%bind owner =
      List.fold_result facts ~init:None ~f:(fun previous fact ->
        if not (P.Id.Session.equal session_id (P.Session_ref.session_id fact))
        then invalid "Goal policy owner fact belongs to another session"
        else (
          match previous with
          | None -> Ok (Some fact)
          | Some previous when P.Session_ref.equal previous fact -> Ok (Some previous)
          | Some _ -> invalid "Goal policy full-owner facts conflict"))
    in
    if
      Int64.(session_revision >= 0L)
      && P.Id.Session.equal session_id (L.session_id ledger)
    then
      Ok
        { session_id
        ; session_revision
        ; owner
        ; catalog
        ; accounts
        ; ledger
        ; descendants
        ; children
        ; causal
        ; canonical
        }
    else invalid "Goal policy projection ledger or revision differs"
  ;;

  let rebuild t ~catalog ~accounts ~ledger ~descendants ~children ~causal ~canonical =
    create
      ~session_id:t.session_id
      ~session_revision:t.session_revision
      ~catalog
      ~accounts
      ~ledger
      ~descendants
      ~children
      ~causal
      ~canonical
  ;;

  let totals t ~revision =
    match t.owner with
    | None -> invalid "Goal policy view has no actual full owner"
    | Some owner ->
      Totals.Context.create
        ~owner
        ~catalog:t.catalog
        ~accounts:t.accounts
        ~ledger:t.ledger
        ~descendants:t.descendants
        ~children:t.children
        ~causal:t.causal
        ~canonical:t.canonical
        ~session_revision:revision
  ;;
end

type edge =
  { prefix : Projection.t
  ; after : L.t
  ; dimension : Goal_admission_policy.dimension option
  ; causal_targets : P.Id.Goal.t list
  ; consumed : bool
  }

type t =
  { original : Projection.t
  ; current : Projection.t
  ; owner : P.Session_ref.t option
  ; bootstrap : Bootstrap.t
  ; edge : edge option
  }

let begin_ projection =
  Ok
    { original = projection
    ; current = projection
    ; owner = projection.Projection.owner
    ; bootstrap =
        Bootstrap.begin_
          ~previous:projection.accounts
          ~before:projection.ledger
          ~catalog:projection.catalog
    ; edge = None
    }
;;

let publication_revision t =
  if Int64.equal t.original.session_revision Int64.max_value
  then invalid "Goal policy publication revision exhausted"
  else Ok (Int64.succ t.original.session_revision)
;;

let retain_projection t current =
  match t.owner, current.Projection.owner with
  | Some before, Some after when not (P.Session_ref.equal before after) ->
    invalid "Goal policy full owner changed within transaction"
  | Some _, None -> invalid "Goal policy full owner disappeared within transaction"
  | None, (None | Some _) | Some _, Some _ ->
    Ok
      { t with
        current
      ; owner =
          (match t.owner with
           | Some _ -> t.owner
           | None -> current.owner)
      }
;;

let check_capacity ?before t =
  let open Result.Let_syntax in
  let%bind revision = publication_revision t in
  match t.current.owner with
  | None -> Ok ()
  | Some owner ->
    let module Carrier = Goal_policy_contributions_carrier in
    let ledger = t.current.ledger in
    (match t.current.causal with
     | Some causal ->
       Carrier.validate_canonical_edge
         causal
         ~owner
         ~catalog:t.current.catalog
         ~accounts:t.current.accounts
         ~canonical:t.current.canonical
         ~before:(Option.value before ~default:ledger)
         ~after:ledger
         ~session_revision:revision
     | None ->
       let%bind inline =
         Carrier.inline
           ~owner
           ~canonical:t.current.canonical
           ~ledger
           ~session_revision:revision
       in
       if List.length inline <= Book.max_streams
       then Ok ()
       else invalid "inline Goal stream capacity exhausted")
;;

let update_projection
      t
      ~catalog
      ~accounts
      ~ledger
      ~descendants
      ~children
      ~causal
      ~canonical
  =
  let%bind.Result current =
    Projection.rebuild
      t.current
      ~catalog
      ~accounts
      ~ledger
      ~descendants
      ~children
      ~causal
      ~canonical
  in
  retain_projection t current
;;

let count_available t prefix ~goal_id ~dimension ~context =
  let open Result.Let_syntax in
  let%bind revision = publication_revision t in
  let%bind context_view = Projection.totals prefix ~revision in
  let%bind goal =
    Option.bind prefix.Projection.catalog ~f:(fun catalog ->
      Goal_catalog.find catalog goal_id)
    |> Result.of_option
         ~error:(P.Error.invalid_request "ordered count has no preceding actual Goal")
  in
  let%bind view = Totals.capture context_view ~goal_id in
  let%bind availability = Totals.available view ~context ~goal ~dimension in
  match availability with
  | Available -> Ok ()
  | Reserved | Exhausted | Unknown ->
    invalid "ordered Goal admission has no combined count capacity"
;;

let dimension before after =
  let%bind.Result before =
    Canonical_usage_fold.Marks.of_ledger before
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request
        (Sexp.to_string_hum (Canonical_usage_fold.Error.sexp_of_t error)))
  in
  let%map.Result after =
    Canonical_usage_fold.Marks.of_ledger after
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request
        (Sexp.to_string_hum (Canonical_usage_fold.Error.sexp_of_t error)))
  in
  if Int64.(after.attempt - before.attempt = 1L && after.turn = before.turn)
  then Some Goal_admission_policy.Attempt
  else if Int64.(after.turn - before.turn = 1L && after.attempt = before.attempt)
  then Some Host_turn
  else None
;;

let canonical t ~before ~after ~policy =
  let open Result.Let_syntax in
  let%bind actual = ledger_json t.current.ledger in
  let%bind supplied = ledger_json before in
  let%bind () =
    if D.Json.equal actual supplied
    then Ok ()
    else invalid "Goal policy canonical edge has a different actual basis"
  in
  let%bind dimension = dimension before after in
  let targets =
    Option.bind policy ~f:PC.Update.selection
    |> Option.value_map ~default:[] ~f:(fun selection ->
      List.filter_map (PC.Selection.choices selection) ~f:(fun choice ->
        let target = PC.Choice.target choice in
        if
          P.Id.Session.equal
            (Policy_cause_data.Goal_target.owner target |> P.Session_ref.session_id)
            t.current.session_id
        then Some target
        else None))
  in
  let%bind () =
    List.fold_result targets ~init:() ~f:(fun () target ->
      let%bind () =
        match t.current.owner with
        | Some owner
          when P.Session_ref.equal owner (Policy_cause_data.Goal_target.owner target) ->
          Ok ()
        | None | Some _ -> invalid "selected Goal target differs from actual full owner"
      in
      count_available
        t
        t.current
        ~goal_id:(Policy_cause_data.Goal_target.goal_id target)
        ~dimension:Attempt
        ~context:Delegated_policy_causal)
  in
  let edge =
    Some
      { prefix = t.current
      ; after
      ; dimension
      ; causal_targets = List.map targets ~f:Policy_cause_data.Goal_target.goal_id
      ; consumed = false
      }
  in
  let canonical =
    match policy with
    | None -> t.current.canonical
    | Some update -> Some (PC.Update.candidate update)
  in
  let%bind t =
    update_projection
      { t with edge }
      ~catalog:t.current.catalog
      ~accounts:t.current.accounts
      ~ledger:after
      ~descendants:t.current.descendants
      ~children:t.current.children
      ~causal:t.current.causal
      ~canonical
  in
  (* A canonical retirement precedes its exact Accounts fold in the same
     transaction. Check the retained bindings on their preceding ledger. *)
  check_capacity ~before t |> Result.map ~f:(fun () -> t)
;;

let account_advances t incoming =
  let open Result.Let_syntax in
  let documents = Option.value_map incoming ~default:[] ~f:Accounts.documents in
  List.fold_result documents ~init:t.edge ~f:(fun edge incoming ->
    let next = Goal_accounting_document.stored incoming in
    match
      Option.bind t.current.accounts ~f:(fun accounts ->
        Accounts.find accounts (Stored.goal_id next))
    with
    | None -> Ok edge (* Existing bootstrap checker requires exact zero creation. *)
    | Some previous ->
      let previous = Goal_accounting_document.stored previous in
      let attempts =
        Int64.(Stored.admitted_attempts next - Stored.admitted_attempts previous)
      and turns = Int64.(Stored.admitted_turns next - Stored.admitted_turns previous) in
      if Int64.equal attempts 0L && Int64.equal turns 0L
      then Ok edge
      else (
        let%bind edge =
          match edge with
          | Some edge when not edge.consumed -> Ok edge
          | None | Some _ ->
            invalid "native Goal count advance has no unconsumed exact canonical prefix"
        in
        let%bind dimension =
          match edge.dimension with
          | Some Goal_admission_policy.Attempt
            when Int64.equal attempts 1L && Int64.equal turns 0L ->
            Ok Goal_admission_policy.Attempt
          | Some Host_turn when Int64.equal turns 1L && Int64.equal attempts 0L ->
            Ok Host_turn
          | None | Some (Attempt | Host_turn) ->
            invalid "native Goal counter advance differs from its exact canonical edge"
        in
        let%bind () =
          Stored.validate_transition
            previous
            ~incoming:next
            ~before:edge.prefix.ledger
            ~after:edge.after
        in
        let context =
          if List.mem edge.causal_targets (Stored.goal_id next) ~equal:P.Id.Goal.equal
          then Book.Admission_context.Delegated_policy_causal
          else Ordinary_existing
        in
        let%map () =
          count_available t edge.prefix ~goal_id:(Stored.goal_id next) ~dimension ~context
        in
        Some { edge with consumed = true }))
;;

let exact_candidate expected incoming =
  let open Result.Let_syntax in
  let%bind merged = Book.adopt expected ~incoming in
  let%bind expected = book_json merged in
  let%bind actual = book_json incoming in
  if D.Json.equal expected actual
  then Ok ()
  else invalid "Goal book Update omitted preserved fields"
;;

let book_update t update =
  let open Result.Let_syntax in
  let%bind owner =
    t.current.owner
    |> Result.of_option
         ~error:(P.Error.invalid_request "Goal book mutation has no actual full owner")
  in
  let previous = t.current.causal in
  let%bind expected, new_quota =
    match Book.Update.reason update with
    | Initialize_goal goal_id ->
      let%bind goal, account =
        Bootstrap.creation t.bootstrap ~goal_id
        |> Result.of_option
             ~error:
               (P.Error.invalid_request
                  "known Goal history requires actual ordered Goal/account creation")
      in
      let%map expected =
        Book.initialize_goal
          previous
          ~owner
          ~goal
          ~account
          ~before_ledger:t.original.ledger
      in
      Book.Update.candidate expected, false
    | Reserve request ->
      let%bind result = Book.reserve previous ~owner ~request in
      (match result with
       | Reserved (expected, _) ->
         let goal_id =
           Policy_cause_data.Goal_target.goal_id
             (PC.Stream.target (Book.Request.stream request))
         in
         let%map () =
           count_available
             t
             t.current
             ~goal_id
             ~dimension:Attempt
             ~context:Delegated_policy_causal
         in
         Book.Update.candidate expected, true
       | Already_pending _ ->
         let%map previous =
           previous
           |> Result.of_option
                ~error:(P.Error.invalid_request "exact pending replay lost its book")
         in
         previous, false
       | Busy _ -> invalid "different causal request cannot commit a busy receipt")
    | Import projection ->
      let%bind previous =
        previous
        |> Result.of_option
             ~error:(P.Error.invalid_request "causal import has no retained book")
      in
      let%map expected, _ = Book.import previous ~projection in
      Book.Update.candidate expected, false
    | Close_before_writer pending ->
      let%bind previous =
        previous
        |> Result.of_option
             ~error:(P.Error.invalid_request "causal close has no retained book")
      in
      let%map expected = Book.close_before_writer previous ~pending in
      Book.Update.candidate expected, false
  in
  let%bind () = exact_candidate expected (Book.Update.candidate update) in
  let t = if new_quota then { t with edge = None } else t in
  let%bind t =
    update_projection
      t
      ~catalog:t.current.catalog
      ~accounts:t.current.accounts
      ~ledger:t.current.ledger
      ~descendants:t.current.descendants
      ~children:t.current.children
      ~causal:(Some (Book.Update.candidate update))
      ~canonical:t.current.canonical
  in
  check_capacity t |> Result.map ~f:(fun () -> t)
;;

let descendants t update =
  let open Result.Let_syntax in
  let%bind new_quota =
    match Goal_descendants.Update.reason update with
    | Reserve { scope; occurrence; local_count; ceiling } ->
      let%bind current =
        t.current.descendants
        |> Result.of_option
             ~error:(P.Error.invalid_request "descendant Reserve has no actual owner")
      in
      let%bind children =
        t.current.children
        |> Result.of_option
             ~error:(P.Error.invalid_request "descendant Reserve has no actual children")
      in
      let%bind result =
        Goal_descendants.reserve
          current
          ~children
          ~scope
          ~occurrence
          ~local_count
          ~ceiling
      in
      (match result with
       | Already_pending _ -> Ok false
       | Reserved _ ->
         let%map () =
           count_available
             t
             t.current
             ~goal_id:(Scope.goal_id scope)
             ~dimension:(Goal_descendant_occurrence.dimension occurrence)
             ~context:Ordinary_existing
         in
         true
       | Blocked _ -> invalid "descendant quota Update is not a new eligible reservation")
    | Register_new_root _
    | Register_first_unrelated_child _
    | Register_child _
    | Register_branch _
    | Register_retained_branch _
    | Record_included _
    | Import _
    | Close_before_writer _ -> Ok false
  in
  let t = if new_quota then { t with edge = None } else t in
  let children =
    match Goal_descendants.Update.children update with
    | Preserve -> t.current.children
    | Replace children -> Some children
  in
  update_projection
    t
    ~catalog:t.current.catalog
    ~accounts:t.current.accounts
    ~ledger:t.current.ledger
    ~descendants:(Some (Goal_descendants.Update.accounting update))
    ~children
    ~causal:t.current.causal
    ~canonical:t.current.canonical
;;

type step =
  | Catalog of Goal_catalog.t option
  | Accounts of Accounts.t option
  | Canonical of
      { before : L.t
      ; after : L.t
      ; policy : PC.Update.t option
      }
  | Descendants of Goal_descendants.Update.t
  | Children of Child_contributions.t option
  | Canonical_policy of PC.Update.t
  | Update of Book.Update.t
  | Replacement of Projection.t

let advance t = function
  | Catalog catalog ->
    let%bind.Result bootstrap =
      match catalog with
      | Some catalog -> Bootstrap.advance t.bootstrap (Catalog catalog)
      | None ->
        Bootstrap.advance
          t.bootstrap
          (Replacement { catalog; accounts = t.current.accounts })
    in
    update_projection
      { t with bootstrap }
      ~catalog
      ~accounts:t.current.accounts
      ~ledger:t.current.ledger
      ~descendants:t.current.descendants
      ~children:t.current.children
      ~causal:t.current.causal
      ~canonical:t.current.canonical
  | Accounts accounts ->
    let open Result.Let_syntax in
    let%bind edge = account_advances t accounts in
    let%bind bootstrap =
      match accounts with
      | Some accounts -> Bootstrap.advance t.bootstrap (Accounts accounts)
      | None ->
        Bootstrap.advance
          t.bootstrap
          (Replacement { catalog = t.current.catalog; accounts })
    in
    update_projection
      { t with bootstrap; edge }
      ~catalog:t.current.catalog
      ~accounts
      ~ledger:t.current.ledger
      ~descendants:t.current.descendants
      ~children:t.current.children
      ~causal:t.current.causal
      ~canonical:t.current.canonical
  | Canonical { before; after; policy } -> canonical t ~before ~after ~policy
  | Descendants update -> descendants t update
  | Children children ->
    update_projection
      t
      ~catalog:t.current.catalog
      ~accounts:t.current.accounts
      ~ledger:t.current.ledger
      ~descendants:t.current.descendants
      ~children
      ~causal:t.current.causal
      ~canonical:t.current.canonical
  | Canonical_policy update ->
    let%bind.Result t =
      update_projection
        t
        ~catalog:t.current.catalog
        ~accounts:t.current.accounts
        ~ledger:t.current.ledger
        ~descendants:t.current.descendants
        ~children:t.current.children
        ~causal:t.current.causal
        ~canonical:(Some (PC.Update.candidate update))
    in
    check_capacity t |> Result.map ~f:(fun () -> t)
  | Update update -> book_update t update
  | Replacement incoming ->
    let open Result.Let_syntax in
    let%bind () =
      match t.current.causal, incoming.causal with
      | None, None -> Ok ()
      | Some previous, Some incoming -> exact_candidate previous incoming
      | None, Some _ | Some _, None ->
        invalid "replacement cannot initialize or discard causal Goal books"
    in
    let%bind bootstrap =
      Bootstrap.advance
        t.bootstrap
        (Replacement { catalog = incoming.catalog; accounts = incoming.accounts })
    in
    let%bind t = retain_projection { t with bootstrap; edge = None } incoming in
    check_capacity t |> Result.map ~f:(fun () -> t)
;;

let check t incoming =
  let open Result.Let_syntax in
  let%bind same_ledger =
    equal_optional
      (Some t.current.ledger)
      (Some incoming.Projection.ledger)
      ~encode:ledger_json
  in
  let%bind same_catalog =
    equal_optional t.current.catalog incoming.catalog ~encode:Goal_catalog.to_jsonaf
  in
  let%bind same_accounts =
    equal_optional t.current.accounts incoming.accounts ~encode:Accounts.to_jsonaf
  in
  let%bind same_descendants =
    equal_optional t.current.descendants incoming.descendants ~encode:descendants_json
  in
  let%bind same_children =
    equal_optional
      t.current.children
      incoming.children
      ~encode:Child_contributions.to_jsonaf
  in
  let%bind same_causal =
    equal_optional t.current.causal incoming.causal ~encode:book_json
  in
  let%bind same_canonical =
    equal_optional t.current.canonical incoming.canonical ~encode:canonical_json
  in
  let%bind () =
    if
      P.Id.Session.equal t.current.session_id incoming.session_id
      && Option.equal P.Session_ref.equal t.current.owner incoming.owner
      && same_ledger
      && same_catalog
      && same_accounts
      && same_descendants
      && same_children
      && same_causal
      && same_canonical
    then Ok ()
    else invalid "ordered Goal accounting projection differs from actual candidate"
  in
  check_capacity t
;;
