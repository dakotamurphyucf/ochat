open! Core
module P = Agent_protocol
module A = Goal_accounting
module C = Goal_accounts
module Child = Goal_accounting_document
module L = Inference_ledger

type t =
  { accounts : C.t option
  ; changed : bool
  }

let accounts t = t.accounts
let changed t = t.changed
let invalid message = Error (P.Error.invalid_request message)

let accounting result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (A.Error.sexp_of_t error)))
;;

let goal catalog id =
  Result.of_option
    (Option.bind catalog ~f:(fun catalog -> Goal_catalog.find catalog id))
    ~error:(P.Error.invalid_request "accounting partition goal is absent")
;;

let prepare ~accounts ~catalog ~before ~after ~admission =
  let open Result.Let_syntax in
  match accounts with
  | None ->
    (match admission with
     | None -> Ok { accounts = None; changed = false }
     | Some _ -> invalid "goal admission requires existing pre-episode accounting")
  | Some original ->
    let%bind () =
      match admission with
      | None -> Ok ()
      | Some (id, _) ->
        if Option.is_some (C.find original id)
        then Ok ()
        else invalid "goal admission requires its existing accounting partition"
    in
    let%map candidate, changed =
      List.fold_result
        (C.documents original)
        ~init:(original, false)
        ~f:(fun (candidate, changed) child ->
          let id = A.Stored.goal_id (Child.stored child) in
          let%bind goal = goal catalog id in
          let%bind account = Child.resolve child ~goal ~ledger:before in
          let admission =
            Option.bind admission ~f:(fun (target, admission) ->
              Option.some_if (P.Id.Goal.equal target id) admission)
          in
          let%bind plan = A.plan account ~goal ~before ~after ~admission |> accounting in
          let%map candidate = C.apply candidate plan in
          candidate, changed || not (A.equal account (A.Plan.candidate plan)))
    in
    { accounts = Some candidate; changed }
;;

let protected_ordinals accounts ~ledger =
  let open Result.Let_syntax in
  match accounts with
  | None -> Ok Int64.Set.empty
  | Some accounts ->
    List.fold_result
      (C.documents accounts)
      ~init:Int64.Set.empty
      ~f:(fun protected child ->
        List.fold_result
          (A.Stored.ordinals (Child.stored child))
          ~init:protected
          ~f:(fun protected ordinal ->
            let%bind row =
              Result.of_option
                (L.find ledger ~ordinal)
                ~error:(P.Error.invalid_request "bound canonical row is absent")
            in
            match Inference.Observation.Attempt_record.state (L.Row.record row) with
            | Prepared | Running -> Ok protected
            | Terminal _ | Interrupted _ ->
              let%map allowed = Child.can_retire child ~ordinal in
              if allowed then protected else Set.add protected ordinal))
;;
