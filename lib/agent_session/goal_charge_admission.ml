open! Core
module P = Agent_protocol
module G = P.Goal

type ready = Goal_work_attribution.t

let goal_id = Goal_work_attribution.goal_id
let episode = Goal_work_attribution.episode

type decision =
  | Unbound
  | Ready of ready
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
    let%bind decision =
      Goal_admission_policy.check
        goal
        ~accounting:account
        ~ledger:state.inference_ledger
        ~now
        ~dimension
    in
    (match decision with
     | Allow -> Ok (Ready attribution)
     | Hold reason ->
       let%map delta = hold_delta state ~goal_id:id ~reason in
       Held { goal_id = id; reason; delta })
;;
