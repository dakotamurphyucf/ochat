open! Core
module P = Agent_protocol
module D = Document_schema
module C = D.Extension_carrier
module X = Persistence_codec
module A = Goal_accounting
module S = A.Stored
module Child = Goal_accounting_document

let invalid s = Error (P.Error.invalid_request s)

let domain result =
  Result.map_error result ~f:(fun e ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t e)))
;;

let limits =
  D.Limits.create
    ~max_bytes:(4 * 1024 * 1024)
    ~max_depth:72
    ~max_fields:280000
    ~max_nodes:560000
  |> domain
  |> Result.map_error ~f:(fun e -> e.P.Error.message)
  |> Result.ok_or_failwith
;;

let id child = S.goal_id (Child.stored child)
let same a b = P.Id.Goal.equal (id a) (id b)

let child_json child =
  let%map.Result doc = Child.to_document child in
  `Object [ "id", P.Id.Goal.to_json (id child); "document", D.Document.json doc ]
;;

let decode_child json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind expected = X.required f "id" P.Id.Goal.of_json in
  let%bind raw = X.required f "document" X.raw in
  let%bind doc = D.Document.inspect raw ~limits:Child.limits |> domain in
  let%bind child = Child.of_document doc in
  if P.Id.Goal.equal expected (id child)
  then Ok child
  else invalid "account wrapper identity differs"
;;

let validate_projection entries =
  let open Result.Let_syntax in
  let%bind () =
    if List.length entries <= 16
    then Ok ()
    else invalid "account catalog exceeds sixteen goals"
  in
  let%map _ =
    List.fold_result
      entries
      ~init:(None, Int64.Set.empty)
      ~f:(fun (previous, ordinals) child ->
        let%bind () =
          if
            Option.exists previous ~f:(fun previous ->
              P.Id.Goal.compare previous (id child) >= 0)
          then invalid "account goal identities must be unique and ordered"
          else Ok ()
        in
        let%map ordinals =
          List.fold_result
            (S.ordinals (Child.stored child))
            ~init:ordinals
            ~f:(fun ordinals ordinal ->
              if Set.mem ordinals ordinal
              then invalid "canonical attempt attributed to more than one goal"
              else if Set.length ordinals >= 256
              then invalid "account catalog exceeds256 canonical bindings"
              else Ok (Set.add ordinals ordinal))
        in
        Some (id child), ordinals)
  in
  ()
;;

let encode entries =
  let%map.Result entries = Result.all (List.map entries ~f:child_json) in
  `Object [ "accounts", `Array entries ]
;;

let decode json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind entries =
    X.required f "accounts" (function
      | `Array entries when List.length entries <= 16 ->
        Result.all (List.map entries ~f:decode_child)
      | _ -> invalid "account catalog exceeds sixteen goals")
  in
  let%map () = validate_projection entries in
  entries
;;

let projection_shape =
  X.shape_exn
    [ ( "accounts"
      , X.array_shape_exn
          ~identity_field:"id"
          (X.shape_exn [ "id", D.Shape.value; "document", D.Shape.value ]) )
    ]
;;

let codec =
  match
    D.Domain_codec.create_validated
      ~version:1
      ~limits
      ~kind:"ochat.goal_accounts"
      ~shape:projection_shape
      ~supported_semantics:[]
      ~validate:(fun entries -> validate_projection entries |> X.document_result)
      ~decode:(fun raw -> decode raw |> X.document_result)
      ~encode:(fun entries -> encode entries |> X.document_result)
  with
  | Ok codec -> codec
  | Error error -> raise_s [%sexp "invalid accounting catalog codec", (error : D.Error.t)]
;;

(* The parent owns this complete child as one value. This codec, not the parent,
   alone preserves the child envelope and unknown paths. *)
let shape = D.Shape.value

type t = Child.t list C.t

let empty = C.of_authored_value []
let documents = C.value

let find t goal_id =
  List.find (documents t) ~f:(fun child -> P.Id.Goal.equal goal_id (id child))
;;

let reserved_growth t =
  List.fold_result (documents t) ~init:Capacity_growth.zero ~f:(fun growth child ->
    let open Result.Let_syntax in
    let%bind next = Child.reserved_growth child in
    Capacity_growth.add growth next)
;;

let validate_encoded_capacity t raw =
  let open Result.Let_syntax in
  let%bind growth = reserved_growth t in
  let%bind remaining = Capacity_growth.remaining_limits growth limits in
  D.Json.validate ~limits:remaining raw |> domain
;;

let to_jsonaf t =
  let open Result.Let_syntax in
  let%bind doc = D.Domain_codec.encode codec t |> domain in
  let raw = D.Document.json doc in
  let%map () = validate_encoded_capacity t raw in
  raw
;;

let of_jsonaf raw =
  let open Result.Let_syntax in
  let%bind doc = D.Document.inspect raw ~limits |> domain in
  let%bind t = D.Domain_codec.decode codec doc |> domain in
  let%map () = validate_encoded_capacity t raw in
  t
;;

let admit t entries =
  let incoming = C.with_value t entries in
  let%map.Result _ = to_jsonaf incoming in
  incoming
;;

let bootstrap t ~goal ~ledger =
  let open Result.Let_syntax in
  let%bind () =
    if Option.is_none (find t (P.Goal.id goal))
    then Ok ()
    else invalid "goal accounting already exists; bootstrap is not reset"
  in
  let%bind child = Child.create goal ~ledger in
  admit
    t
    (List.sort (child :: documents t) ~compare:(fun a b ->
       P.Id.Goal.compare (id a) (id b)))
;;

let apply t plan =
  let open Result.Let_syntax in
  let goal_id = S.goal_id (S.capture (A.Plan.before_accounting plan)) in
  let%bind previous =
    Result.of_option
      (find t goal_id)
      ~error:(P.Error.invalid_request "missing goal accounting partition")
  in
  let%bind child = Child.apply previous plan in
  admit t (List.map (documents t) ~f:(fun old -> if same old child then child else old))
;;

let adopt previous ~incoming =
  let open Result.Let_syntax in
  let%bind entries =
    List.map (documents incoming) ~f:(fun child ->
      match find previous (id child) with
      | Some old -> Child.adopt old ~incoming:child
      | None ->
        let s = Child.stored child in
        if Int64.equal (S.revision s) 0L && List.is_empty (S.ordinals s)
        then Ok child
        else invalid "new accounting partition lacks bootstrap frontier")
    |> Result.all
  in
  let%bind () =
    List.fold_result (documents previous) ~init:() ~f:(fun () old ->
      if Option.is_some (find incoming (id old))
      then Ok ()
      else invalid "accounting partition cannot be deleted")
  in
  D.Domain_codec.adopt codec ~previous ~incoming:(C.with_value incoming entries) |> domain
;;

let admission_totals t =
  let add a b =
    if Int64.(b > max_value - a)
    then invalid "account catalog admission total overflow"
    else Ok Int64.(a + b)
  in
  List.fold_result
    (documents t)
    ~init:(0L, 0L, 0L, 0L)
    ~f:(fun (attempts, turns, untracked_attempts, untracked_turns) child ->
      let open Result.Let_syntax in
      let child = Child.stored child in
      let%bind attempts = add attempts (S.admitted_attempts child) in
      let%bind turns = add turns (S.admitted_turns child) in
      let%bind untracked_attempts = add untracked_attempts (S.untracked_attempts child) in
      let%map untracked_turns = add untracked_turns (S.untracked_turns child) in
      attempts, turns, untracked_attempts, untracked_turns)
;;

let validate t ~catalog ~ledger ~runs =
  let open Result.Let_syntax in
  let%bind _ = to_jsonaf t in
  let%bind _ =
    List.fold_result (documents t) ~init:Int64.Set.empty ~f:(fun seen child ->
      let%bind goal =
        Result.of_option
          (Option.bind catalog ~f:(fun c -> Goal_catalog.find c (id child)))
          ~error:(P.Error.invalid_request "account has no retained goal")
      in
      let%bind _ = Child.resolve child ~goal ~ledger in
      let%bind seen =
        List.fold_result
          (S.ordinals (Child.stored child))
          ~init:seen
          ~f:(fun seen ordinal ->
            if Set.mem seen ordinal
            then invalid "canonical attempt attributed to more than one goal"
            else Ok (Set.add seen ordinal))
      in
      let%map () =
        List.fold_result
          (S.run_bindings (Child.stored child))
          ~init:()
          ~f:(fun () (run_id, source) ->
            match Option.bind runs ~f:(fun index -> Run_state.goal_link index run_id) with
            | Some link
              when P.Id.Goal.equal (Run_goal_link.goal_id link) (id child)
                   && P.Run_source.equal
                        (P.Goal.Episode.admission (Run_goal_link.episode link)).source
                        source -> Ok ()
            | Some _ | None ->
              invalid "account binding has no matching immutable Run goal association")
      in
      seen)
  in
  let%bind attempts, turns, untracked_attempts, untracked_turns = admission_totals t in
  let%bind marks =
    Goal_accounting_state.marks ledger
    |> Result.map_error ~f:(fun e ->
      P.Error.invalid_request
        (Sexp.to_string_hum (Goal_accounting_state.Error.sexp_of_t e)))
  in
  if
    Int64.(
      attempts <= marks.attempt
      && turns <= marks.turn
      && untracked_attempts <= marks.untracked_attempts
      && untracked_turns <= marks.untracked_turns)
  then Ok ()
  else invalid "stored accounting totals exceed canonical coverage"
;;

let can_retire t ~ordinal =
  List.fold_result (documents t) ~init:true ~f:(fun allowed child ->
    let%map.Result eligible = Child.can_retire child ~ordinal in
    allowed && eligible)
;;

let validate_global_admissions previous incoming ~before ~after =
  let open Result.Let_syntax in
  let ledger_marks current =
    Goal_accounting_state.marks current
    |> Result.map_error ~f:(fun e ->
      P.Error.invalid_request
        (Sexp.to_string_hum (Goal_accounting_state.Error.sexp_of_t e)))
  in
  let%bind a = ledger_marks before in
  let%bind b = ledger_marks after in
  let%bind pa, pt, pu, pv = admission_totals previous in
  let%bind na, nt, nu, nv = admission_totals incoming in
  let fits previous current before after =
    Int64.(current >= previous && after >= before && current - previous <= after - before)
  in
  if
    fits pa na a.attempt b.attempt
    && fits pt nt a.turn b.turn
    && fits pu nu a.untracked_attempts b.untracked_attempts
    && fits pv nv a.untracked_turns b.untracked_turns
  then Ok ()
  else invalid "attributed account admissions exceed canonical progress"
;;

let validate_transition_with_bootstrap
      ~bootstrap_goal
      ~previous
      ~incoming
      ~before
      ~after
      ~previous_catalog
      ~catalog
  =
  let open Result.Let_syntax in
  match previous, incoming with
  | None, None -> Ok ()
  | Some _, None -> invalid "durable accounting cannot be discarded"
  | (None | Some _), Some incoming ->
    let previous = Option.value previous ~default:empty in
    let%bind _ = adopt previous ~incoming in
    let%bind () = validate_global_admissions previous incoming ~before ~after in
    List.fold_result (documents incoming) ~init:() ~f:(fun () child ->
      match find previous (id child) with
      | Some old ->
        S.validate_transition
          (Child.stored old)
          ~incoming:(Child.stored child)
          ~before
          ~after
      | None ->
        let%bind current_goal =
          Result.of_option
            (Option.bind catalog ~f:(fun goals -> Goal_catalog.find goals (id child)))
            ~error:(P.Error.invalid_request "new accounting has no goal")
        in
        let bootstrap_goal =
          match bootstrap_goal (id child) with
          | Some captured -> captured
          | None ->
            Option.value
              (Option.bind previous_catalog ~f:(fun goals ->
                 Goal_catalog.find goals (id child)))
              ~default:current_goal
        in
        (* Check the Goal's durable episode, not a bounded retained link list.
           A missing previous catalog is not evidence that the goal is new. *)
        let%bind bootstrap = Child.create bootstrap_goal ~ledger:before in
        if S.equal (Child.stored bootstrap) (Child.stored child)
        then Ok ()
        else invalid "new partition differs from actual before-ledger bootstrap")
;;

let validate_transition ~previous ~incoming ~before ~after ~previous_catalog ~catalog =
  validate_transition_with_bootstrap
    ~bootstrap_goal:(fun _ -> None)
    ~previous
    ~incoming
    ~before
    ~after
    ~previous_catalog
    ~catalog
;;

module Ordered_bootstrap = struct
  type accounts = t

  type t =
    { previous : accounts option
    ; before : Inference_ledger.t
    ; previous_catalog : Goal_catalog.t option
    ; catalog : Goal_catalog.t option
    ; accounts : accounts option
    ; captured : P.Goal.t list
    }

  let begin_ ~previous ~before ~catalog =
    { previous
    ; before
    ; previous_catalog = catalog
    ; catalog
    ; accounts = previous
    ; captured = []
    }
  ;;

  type step =
    | Catalog of Goal_catalog.t
    | Accounts of accounts
    | Replacement of
        { catalog : Goal_catalog.t option
        ; accounts : accounts option
        }

  let catalog_edge previous incoming =
    match previous, incoming with
    | None, None -> Ok ()
    | None, Some incoming -> Goal_catalog.validate_transition Goal_catalog.empty ~incoming
    | Some previous, Some incoming -> Goal_catalog.validate_transition previous ~incoming
    | Some _, None -> invalid "ordered replacement discarded durable Goals"
  ;;

  let account_edge previous incoming ~adopt_incoming =
    match previous, incoming with
    | None, None -> Ok None
    | Some _, None -> invalid "ordered replacement discarded durable accounting"
    | (None | Some _), Some incoming ->
      let%map.Result merged = adopt (Option.value previous ~default:empty) ~incoming in
      Some (if adopt_incoming then merged else incoming)
  ;;

  let capture_new t accounts =
    List.fold_result
      (Option.value_map accounts ~default:[] ~f:documents)
      ~init:t.captured
      ~f:(fun captured child ->
        if
          Option.exists t.previous ~f:(fun previous ->
            Option.is_some (find previous (id child)))
          || List.exists captured ~f:(fun goal ->
            P.Id.Goal.equal (P.Goal.id goal) (id child))
        then Ok captured
        else
          let open Result.Let_syntax in
          let%bind goal =
            Result.of_option
              (Option.bind t.catalog ~f:(fun catalog ->
                 Goal_catalog.find catalog (id child)))
              ~error:
                (P.Error.invalid_request
                   "new accounting lacks its actual ordered Goal prefix")
          in
          let%bind bootstrap = Child.create goal ~ledger:t.before in
          if S.equal (Child.stored bootstrap) (Child.stored child)
          then Ok (goal :: captured)
          else invalid "new partition differs from actual before-ledger bootstrap")
  ;;

  let advance t = function
    | Catalog catalog ->
      let%map.Result () = catalog_edge t.catalog (Some catalog) in
      { t with catalog = Some catalog }
    | Accounts incoming ->
      let open Result.Let_syntax in
      let%bind accounts = account_edge t.accounts (Some incoming) ~adopt_incoming:true in
      let%map captured = capture_new t accounts in
      { t with accounts; captured }
    | Replacement { catalog; accounts } ->
      let open Result.Let_syntax in
      let%bind () = catalog_edge t.catalog catalog in
      let%bind accounts = account_edge t.accounts accounts ~adopt_incoming:false in
      let candidate = { t with catalog; accounts } in
      let%map captured = capture_new candidate accounts in
      { candidate with captured }
  ;;

  let equal_projection previous incoming ~to_json =
    match previous, incoming with
    | None, None -> Ok true
    | None, Some _ | Some _, None -> Ok false
    | Some previous, Some incoming ->
      let open Result.Let_syntax in
      let%bind previous = to_json previous in
      let%map incoming = to_json incoming in
      D.Json.equal previous incoming
  ;;

  let check t ~incoming ~after ~catalog =
    let open Result.Let_syntax in
    let%bind same_accounts = equal_projection t.accounts incoming ~to_json:to_jsonaf in
    let%bind same_catalog =
      equal_projection t.catalog catalog ~to_json:Goal_catalog.to_jsonaf
    in
    let%bind () =
      if same_accounts && same_catalog
      then Ok ()
      else invalid "ordered Goal accounting projection differs from actual final state"
    in
    validate_transition_with_bootstrap
      ~bootstrap_goal:(fun id ->
        List.find t.captured ~f:(fun goal -> P.Id.Goal.equal (P.Goal.id goal) id))
      ~previous:t.previous
      ~incoming
      ~before:t.before
      ~after
      ~previous_catalog:t.previous_catalog
      ~catalog
  ;;
end
