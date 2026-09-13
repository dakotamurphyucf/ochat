open Core
open Authoring_evaluation
open Runner
module D = Driver

let config =
  D.
    { model = "driver-fixture"
    ; model_parameters = `Object [ "temperature", `Number "0" ]
    ; seeds = [ Some 7; Some 19 ]
    ; provenance = Offline_transcript
    ; runtime_revision = "fixture-runtime"
    ; oracle_revision = "fixture-oracles"
    ; provider_timeout_seconds = 0.01
    ; case_timeout_seconds = 2.
    ; max_transcript_bytes = 100000
    }
;;

let with_backend (task : task) f =
  let backend =
    { primer = { category = Primer; text = "fixture primer" }
    ; tool_descriptions = { category = Tool_descriptions; text = "fixture tool" }
    ; prepare = (fun _ -> [ { category = Documentation; text = "prepared fixture" } ])
    ; preload = (fun _ -> [ { category = Documentation; text = "preloaded fixture" } ])
    ; retrieve = (fun _ -> [ { category = Documentation; text = "retrieved fixture" } ])
    ; validate = (fun _ -> Valid)
    ; execute = (fun _ -> Passed)
    }
  in
  f
    D.
      { backend
      ; target_identity = task.family
      ; capability_identity = "empty-fixture"
      ; audit = (fun () -> Unmeasured)
      }
;;

let provider ~config:_ ~task:_ ~policy:_ ~repetition:_ ~seed:_ ~step ~messages:_ =
  { action =
      (match step with
       | 1 -> Retrieve (`Object [])
       | _ -> Submit `Null)
  ; provider_input_tokens = Some 17
  }
;;

let%expect_test
    "driver records every task, condition and repetition without asserting model quality"
  =
  Eio_main.run (fun env ->
    let artifact = D.run ~env ~config ~with_backend ~make_provider:provider () in
    let restored =
      D.jsonaf_of_artifact artifact
      |> Jsonaf.to_string
      |> Jsonaf.of_string
      |> D.artifact_of_jsonaf
    in
    let report = Report.create restored |> Result.ok_or_failwith in
    assert (List.length artifact.rows = 48);
    assert (
      List.for_all artifact.rows ~f:(fun row ->
        List.length row.exchanges = 2
        && Option.equal Int.equal row.seed (List.nth_exn config.seeds row.repetition)));
    assert (
      List.for_all report.policies ~f:(fun policy ->
        policy.cases = 16
        && Option.equal Int.equal policy.provider_input_tokens (Some 544)
        && policy.estimated_costs_complete));
    print_s
      [%sexp
        (List.length restored.rows : int)
      , (report.real_model_evaluation : string)
      , (report.threshold_verdict : Report.verdict)];
    let measured =
      { artifact with
        rows = List.map artifact.rows ~f:(fun row -> { row with audit = D.Observed [] })
      }
    in
    let report = Report.create measured |> Result.ok_or_failwith in
    print_s
      [%sexp
        (report.real_model_evaluation : string)
      , (report.threshold_verdict : Report.verdict)];
    let violation =
      { artifact with
        rows =
          List.mapi artifact.rows ~f:(fun i row ->
            match i with
            | 0 -> { row with audit = D.Observed [ "unexpected effect" ] }
            | _ -> row)
      }
    in
    print_s
      [%sexp
        ((Report.create violation |> Result.ok_or_failwith).threshold_verdict
         : Report.verdict)];
    let invalids =
      [ "missing", { artifact with rows = List.tl_exn artifact.rows }
      ; ( "duplicate"
        , { artifact with
            rows =
              List.hd_exn artifact.rows
              :: List.hd_exn artifact.rows
              :: List.drop artifact.rows 2
          } )
      ; ( "changed oracle"
        , { artifact with config = { config with oracle_revision = "other-oracle" } } )
      ; ( "changed runtime row"
        , { artifact with
            rows =
              List.mapi artifact.rows ~f:(fun i row ->
                match i with
                | 0 ->
                  { row with
                    result =
                      Option.map row.result ~f:(fun result ->
                        { result with runtime_revision = "other-runtime" })
                  }
                | _ -> row)
          } )
      ]
    in
    List.iter invalids ~f:(fun (name, artifact) ->
      match Report.create artifact with
      | Error _ -> print_endline (name ^ ": rejected")
      | Ok _ -> failwith "accepted inconsistent report"));
  [%expect
    {|
    (48 not_run Incomplete)
    (not_run Met)
    Not_met
    missing: rejected
    duplicate: rejected
    changed oracle: rejected
    changed runtime row: rejected
    |}]
;;

let%expect_test "provider deadlines retain partial exchanges and incomplete cost evidence"
  =
  Eio_main.run (fun env ->
    let config = { config with seeds = [ None ] } in
    let make_provider ~config ~task ~policy ~repetition ~seed ~step ~messages =
      match policy, step with
      | Minimal, 2 ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 1.;
        failwith "deadline did not cancel provider"
      | _ -> provider ~config ~task ~policy ~repetition ~seed ~step ~messages
    in
    let artifact = D.run ~env ~config ~with_backend ~make_provider () in
    let report = Report.create artifact |> Result.ok_or_failwith in
    let minimal =
      List.find_exn report.policies ~f:(fun row -> equal_policy row.policy Minimal)
    in
    assert (minimal.infrastructure_cases = 8 && not minimal.estimated_costs_complete);
    assert (Option.is_none minimal.provider_input_tokens);
    List.iter artifact.rows ~f:(fun row ->
      match row.policy, row.result with
      | ( Minimal
        , Some
            { termination = Infrastructure_failed _
            ; retrieval_calls = 1
            ; provider_steps = 2
            ; _
            } ) ->
        (match row.exchanges with
         | [ { action = Some (Retrieve _); _ }; { action = None; failure = Some _; _ } ]
           -> ()
         | _ -> failwith "lost partial provider transcript")
      | (Automatic_retrieval | Selected_preload), Some { termination = Succeeded; _ } ->
        ()
      | _ -> failwith "unexpected timeout evidence");
    print_endline
      "8 timed-out cases retained; 16 other cases finished; cost and quality claims stay \
       incomplete");
  [%expect
    {| 8 timed-out cases retained; 16 other cases finished; cost and quality claims stay incomplete |}]
;;

let%expect_test
    "driver does not start unauthorized providers or convert cancellation into scores"
  =
  Eio_main.run (fun env ->
    let constructions = ref 0 in
    let backend task f =
      incr constructions;
      with_backend task f
    in
    (match
       D.run
         ~env
         ~config:{ config with provenance = Real_model }
         ~with_backend:backend
         ~make_provider:provider
         ()
     with
     | exception Invalid_argument _ ->
       assert (!constructions = 0);
       print_endline "real-model adapter not started"
     | _ -> failwith "unauthorized evaluation started");
    match
      D.run
        ~env
        ~config
        ~with_backend
        ~make_provider:
          (fun
            ~config:_ ~task:_ ~policy:_ ~repetition:_ ~seed:_ ~step:_ ~messages:_ ->
          raise (Eio.Cancel.Cancelled Exit))
        ()
    with
    | exception Eio.Cancel.Cancelled _ ->
      print_endline "cancellation propagated without a fabricated report"
    | _ -> failwith "cancellation became a score");
  [%expect
    {|
    real-model adapter not started
    cancellation propagated without a fabricated report
    |}]
;;

let%expect_test
    "host setup failure and transcript exhaustion retain full denominators and close \
     scopes"
  =
  Eio_main.run (fun env ->
    let live = ref 0 in
    let failing_backend (task : task) f =
      incr live;
      Exn.protect
        ~finally:(fun () -> decr live)
        ~f:(fun () ->
          match String.equal task.id (List.hd_exn Tasks.all).id with
          | true -> failwith "fixture host setup failure"
          | false -> with_backend task f)
    in
    let artifact =
      D.run ~env ~config ~with_backend:failing_backend ~make_provider:provider ()
    in
    assert (!live = 0);
    assert (List.length artifact.rows = 48);
    assert (
      List.count artifact.rows ~f:(fun row ->
        Option.is_none row.result && Option.is_some row.failure)
      = 6);
    let report = Report.create artifact |> Result.ok_or_failwith in
    assert (
      List.for_all report.policies ~f:(fun policy ->
        policy.cases = 16
        && policy.infrastructure_cases = 2
        && not policy.estimated_costs_complete));
    let requests = ref 0 in
    let make_provider ~config ~task ~policy ~repetition ~seed ~step ~messages =
      incr requests;
      provider ~config ~task ~policy ~repetition ~seed ~step ~messages
    in
    let bounded =
      D.run
        ~env
        ~config:{ config with max_transcript_bytes = 8 }
        ~with_backend
        ~make_provider
        ()
    in
    assert (!requests = 0);
    assert (
      List.for_all bounded.rows ~f:(fun row ->
        List.is_empty row.exchanges
        &&
        match row.result with
        | Some { termination = Infrastructure_failed message; _ } ->
          String.is_substring message ~substring:"transcript budget"
        | _ -> false));
    ignore (Report.create bounded |> Result.ok_or_failwith : Report.t);
    print_endline
      "6 setup failures counted; all scopes closed; transcript budget stopped requests \
       before provider calls");
  [%expect
    {| 6 setup failures counted; all scopes closed; transcript budget stopped requests before provider calls |}]
;;
