open Core
open Jsonaf.Export
open Runner
module D = Driver

type verdict =
  | Met
  | Not_met
  | Incomplete
[@@deriving sexp, jsonaf, equal]

type policy_metrics =
  { policy : policy
  ; cases : int
  ; first_pass_compile_rate : float
  ; runtime_success_rate : float
  ; repairs : int
  ; retrieval_calls : int
  ; documentation_tokens : int
  ; effective_input_tokens : int
  ; estimated_costs_complete : bool
  ; provider_input_tokens : int option
  ; elapsed_seconds : float
  ; infrastructure_cases : int
  ; safety_measurement_complete : bool
  ; safety_checks : string list
  ; capability_boundary_violations : string list
  }
[@@deriving sexp, jsonaf]

type t =
  { run_fingerprint : string
  ; provenance : provenance
  ; real_model_evaluation : string
  ; conditions : string
  ; policies : policy_metrics list
  ; thresholds : Jsonaf.t
  ; threshold_verdict : verdict
  ; failure_counts : (string * int) list
  }
[@@deriving sexp, jsonaf]

let infrastructure (row : D.row) =
  match row.result with
  | None -> true
  | Some result ->
    (match result.termination with
     | Infrastructure_failed _ -> true
     | _ ->
       List.exists result.attempts ~f:(function
         | { validation = Invalid (Infrastructure, _); _ }
         | { execution = Some (Failed (Infrastructure, _)); _ } -> true
         | _ -> false))
;;

let validate (artifact : D.artifact) =
  let config = artifact.config in
  let expected_count =
    List.length Tasks.all * List.length policies * List.length config.seeds
  in
  let model =
    Jsonaf.to_string
      (`Object [ "name", `String config.model; "parameters", config.model_parameters ])
  in
  let row_valid (row : D.row) =
    let identity =
      List.exists Tasks.all ~f:(fun task -> String.equal task.id row.task_id)
      && row.repetition >= 0
      && row.repetition < List.length config.seeds
      && Option.equal Int.equal row.seed (List.nth_exn config.seeds row.repetition)
      && Float.is_finite row.elapsed_seconds
      && Float.(row.elapsed_seconds >= 0.)
    in
    let result =
      match row.result, row.failure with
      | None, Some _ -> true
      | Some result, None ->
        String.equal result.task_id row.task_id
        && equal_policy result.policy row.policy
        && equal_provenance result.provenance config.provenance
        && String.equal result.model model
        && String.equal result.suite_revision Tasks.fingerprint
        && String.equal result.runtime_revision config.runtime_revision
        && equal_limits result.limits Tasks.limits
        && Option.exists row.target_identity ~f:(fun value -> not (String.is_empty value))
        && Option.exists row.capability_identity ~f:(fun value ->
          not (String.is_empty value))
        && Bool.equal
             result.runtime_success
             (equal_termination result.termination Succeeded)
        && Bool.equal
             result.runtime_success
             (List.exists result.attempts ~f:(function
                | { validation = Valid; execution = Some Passed } -> true
                | _ -> false))
        && Bool.equal
             result.first_pass_compile
             (match result.attempts with
              | { validation = Valid; _ } :: _ -> true
              | _ -> false)
        && result.repairs = Int.max 0 (List.length result.attempts - 1)
      | _ -> false
    in
    identity && result
  in
  match D.validate config with
  | Error message -> Error message
  | Ok () ->
    (match
       artifact.version = 1
       && Jsonaf.exactly_equal artifact.manifest Tasks.manifest
       && String.equal artifact.run_fingerprint (D.fingerprint config)
       && List.length artifact.rows = expected_count
       && List.for_all artifact.rows ~f:row_valid
       && List.for_all Tasks.all ~f:(fun task ->
         List.for_all policies ~f:(fun policy ->
           List.for_alli config.seeds ~f:(fun repetition _ ->
             List.count artifact.rows ~f:(fun row ->
               String.equal row.task_id task.id
               && equal_policy row.policy policy
               && row.repetition = repetition)
             = 1)))
     with
     | true -> Ok ()
     | false ->
       Error "evaluation artifact has missing, duplicate, inconsistent or mixed run rows")
;;

let create (artifact : D.artifact) =
  let open Result.Let_syntax in
  let%map () = validate artifact in
  let metrics =
    List.map policies ~f:(fun policy ->
      let rows =
        List.filter artifact.rows ~f:(fun row -> equal_policy row.policy policy)
      in
      let results = List.filter_map rows ~f:(fun row -> row.result) in
      let count = List.length rows in
      let rate f = Float.of_int (List.count results ~f) /. Float.of_int count in
      let sum f = List.sum (module Int) results ~f in
      let provider_input_tokens =
        List.fold rows ~init:(Some 0) ~f:(fun total row ->
          Option.both
            total
            (Option.bind row.result ~f:(fun result -> result.tokens.provider_input))
          |> Option.map ~f:(fun (a, b) -> a + b))
      in
      { policy
      ; cases = count
      ; first_pass_compile_rate = rate (fun r -> r.first_pass_compile)
      ; runtime_success_rate = rate (fun r -> r.runtime_success)
      ; repairs = sum (fun r -> r.repairs)
      ; retrieval_calls = sum (fun r -> r.retrieval_calls)
      ; documentation_tokens = sum (fun r -> r.tokens.documentation_delivered)
      ; effective_input_tokens = sum (fun r -> r.tokens.total_effective_input)
      ; estimated_costs_complete =
          List.for_all rows ~f:(fun row ->
            (not (infrastructure row))
            && Option.exists row.result ~f:(fun result ->
              result.provider_steps = List.length row.exchanges))
      ; provider_input_tokens
      ; elapsed_seconds = List.sum (module Float) rows ~f:(fun row -> row.elapsed_seconds)
      ; infrastructure_cases = List.count rows ~f:infrastructure
      ; safety_measurement_complete =
          List.for_all rows ~f:(fun row ->
            match row.audit with
            | D.Unmeasured | Partial _ -> false
            | Observed _ -> true)
      ; safety_checks =
          List.concat_map rows ~f:(fun row ->
            match row.audit with
            | D.Partial { checks; _ } -> checks
            | Unmeasured | Observed _ -> [])
          |> List.dedup_and_sort ~compare:String.compare
      ; capability_boundary_violations =
          List.concat_map rows ~f:(fun row ->
            match row.audit with
            | D.Unmeasured -> []
            | Partial { violations; _ } -> violations
            | Observed violations -> violations)
      })
  in
  let threshold name =
    match Jsonaf.member_exn name Tasks.thresholds with
    | `Number value -> Float.of_string value
    | _ -> failwith "invalid predeclared threshold"
  in
  let baseline = List.find_exn metrics ~f:(fun m -> equal_policy m.policy Minimal) in
  let measured =
    List.for_all metrics ~f:(fun m ->
      m.infrastructure_cases = 0 && m.safety_measurement_complete)
  in
  let safe =
    List.for_all metrics ~f:(fun m -> List.is_empty m.capability_boundary_violations)
  in
  let meets =
    List.for_all metrics ~f:(fun m ->
      equal_policy m.policy Minimal
      || Float.(
           m.first_pass_compile_rate >= threshold "minimum_guided_first_pass_compile_rate"
           && m.runtime_success_rate >= threshold "minimum_guided_runtime_success_rate"
           && baseline.runtime_success_rate -. m.runtime_success_rate
              <= threshold "maximum_runtime_regression_vs_minimal"))
  in
  let threshold_verdict =
    match safe, measured, meets with
    | false, _, _ -> Not_met
    | true, false, _ -> Incomplete
    | true, true, true -> Met
    | true, true, false -> Not_met
  in
  let failure_counts =
    List.concat_map [ Syntax; Semantics; Capability; Infrastructure ] ~f:(fun kind ->
      List.map [ "validation"; "execution" ] ~f:(fun stage ->
        let count =
          List.sum
            (module Int)
            artifact.rows
            ~f:(fun row ->
              Option.value_map row.result ~default:0 ~f:(fun result ->
                List.count result.attempts ~f:(fun attempt ->
                  match stage, attempt.validation, attempt.execution with
                  | "validation", Invalid (actual, _), _
                  | "execution", _, Some (Failed (actual, _)) -> equal_failure actual kind
                  | _ -> false)))
        in
        stage ^ "." ^ (sexp_of_failure kind |> Sexp.to_string), count))
  in
  { run_fingerprint = artifact.run_fingerprint
  ; provenance = artifact.config.provenance
  ; real_model_evaluation =
      (match artifact.config.provenance with
       | Offline_transcript -> "not_run"
       | Real_model -> "recorded")
  ; conditions =
      "experimental minimal/task-retrieval/selected-preload; not runtime policy aliases"
  ; policies = metrics
  ; thresholds = Tasks.thresholds
  ; threshold_verdict
  ; failure_counts
  }
;;
