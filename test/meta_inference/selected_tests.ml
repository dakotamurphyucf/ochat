open! Core
module M = Meta_prompting
module R = Inference.Request
module O = Inference.Observation
module E = Inference.Event
module P = History_entry.Payload
module Runtime = Inference_runtime

let ok result =
  Result.map_error result ~f:(fun _ -> "fixture admission") |> Result.ok_or_failwith
;;

let limits = Transcript.Admission.default

let target =
  R.Target.create
    ~adapter:"synthetic"
    ~profile:"selected"
    ~profile_revision:None
    ~account:(Some "selected-account")
    ~endpoint:"local"
    ~model:"selected-model"
    ~settings:
      (List.map
         [ "tool_choice", `String "required"; "parallel_tool_calls", `False ]
         ~f:(fun (name, value) ->
           R.Setting.create ~name ~value:(Value value) ~provenance:Captured_prompt ~limits
           |> ok))
    ~limits
  |> ok
;;

let execution ~target ~answer ~on_observation =
  let requests = ref [] in
  let attempts = ref 0 in
  let context =
    Runtime.Adapter.create
      ~id:"synthetic"
      ~limits:Runtime.Limits.default
      ~bind:(fun selected ->
        assert (String.equal (R.Target.adapter selected) (R.Target.adapter target));
        assert (String.equal (R.Target.profile selected) (R.Target.profile target));
        assert (
          Option.equal String.equal (R.Target.account selected) (R.Target.account target));
        assert (String.equal (R.Target.endpoint selected) (R.Target.endpoint target));
        Ok ())
      ~prepare:(fun ~preparation_id request ->
        requests := request :: !requests;
        let configuration =
          O.Configuration.of_target
            (R.target request)
            ~preparation_id
            ~transport:In_process
            ~capabilities:[]
            ~limits:O.Admission.observation
          |> Result.map_error ~f:(fun error ->
            Sexp.to_string_hum (O.Error.sexp_of_t error))
          |> Result.ok_or_failwith
        in
        Runtime.Plan.create
          ~request
          ~configuration
          ~fingerprint:"prepared-meta"
          ~run:
            (fun
              ~sw:_
              ~scope
              ~accounting_id
              ~note_delivery:_
              ~on_event:_
              ~on_observation:_
            ->
            let text = answer () in
            let semantic =
              P.Semantic.create
                (Message
                   { form = Output
                   ; role = Assistant
                   ; content = [ Text { text; annotations = []; logprobs = Absent } ]
                   ; phase = Absent
                   })
                ~metadata:P.Metadata.empty
              |> ok
            in
            let payload = P.authored semantic in
            let item =
              Transcript.Item.create
                ~scope
                ~id:(Transcript.Item_id.of_string "answer" |> ok)
                ~entry_id:None
                ~header:(Some (Transcript.Header.of_semantic semantic))
                ~call_name:None
              |> ok
            in
            let candidate =
              E.create
                (Candidate_ready { item; payload; local_execution = Not_eligible })
                ~limits
              |> ok
            in
            let unknown = O.Count.create (Unknown Not_reported) |> ok in
            let usage =
              O.Usage.create
                ~counts:
                  { input = unknown
                  ; output = unknown
                  ; reported_total = unknown
                  ; cached_input = unknown
                  ; cache_write_input = unknown
                  ; reasoning_output = unknown
                  }
                ~inclusions:[]
              |> ok
            in
            let usage =
              O.create
                ~scope
                ~id:accounting_id
                ~revision:0L
                ~payload:(Usage usage)
                ~limits:O.Admission.observation
              |> ok
            in
            Runtime.Receipt.create
              ~terminal:
                (E.Terminal.create ~scope ~delivery:Response_started ~outcome:Completed
                 |> ok)
              ~usage
              ~output:[ candidate ]
              ~output_coverage:Response_output
              ~limits:Runtime.Limits.default
            |> ok))
    |> ok
    |> Runtime.Context.create ~target
    |> ok
  in
  let identity =
    Inference_client.Identity.
      { new_preparation_id = (fun () -> "prepare-" ^ Int.to_string (!attempts + 1))
      ; new_attempt =
          (fun _ ~relation ->
            Int.incr attempts;
            let scope =
              Transcript.Scope.create
                ~source:(Transcript.Source_id.of_string "meta-owner" |> ok)
                ~attempt:(Transcript.Attempt_id.of_string (Int.to_string !attempts) |> ok)
                ~relation
              |> ok
            in
            scope, O.Observation_id.of_string "usage" |> ok)
      }
  in
  let execution =
    Inference_client.Execution.create
      ~context
      ~identity
      ~relation:Root
      ~before_dispatch:ignore
      ~on_attempt:ignore
      ~on_completion:ignore
      ~on_observation
  in
  execution, requests, attempts
;;

let%expect_test
    "reward judges inherit actual selected target and do not skip actual attempts"
  =
  Eio_main.run (fun _ ->
    let responses = ref [ "0.4"; "0.8" ] in
    let usages = ref [] in
    let inference, requests, attempts =
      execution
        ~target
        ~answer:(fun () ->
          let text = List.hd_exn !responses in
          responses := List.tl_exn !responses;
          text)
        ~on_observation:(fun observation ->
          match O.payload observation with
          | Usage _ -> usages := O.scope observation :: !usages
          | Context_estimate _ | Configuration _ | Diagnostic _ -> ())
    in
    let module J = (val M.Evaluator.prompt_reward_model_judge : M.Evaluator.Judge) in
    let evaluator = M.Evaluator.create ~judges:[ Judge (module J) ] () in
    let first = M.Evaluator.evaluate ~inference evaluator "same-candidate" in
    let second = M.Evaluator.evaluate ~inference evaluator "same-candidate" in
    assert (!attempts = 2 && List.length !requests = 2 && List.length !usages = 2);
    assert (not (Transcript.Scope.equal (List.hd_exn !usages) (List.nth_exn !usages 1)));
    List.iter !requests ~f:(fun request ->
      assert (String.equal (R.Target.model (R.target request)) "selected-model");
      assert (
        Option.equal
          String.equal
          (R.Target.account (R.target request))
          (Some "selected-account"));
      assert (List.is_empty (R.tools request));
      let setting name =
        List.find_exn
          (R.Target.settings (R.target request))
          ~f:(fun setting -> String.equal (R.Setting.name setting) name)
      in
      assert (
        P.Presence.equal
          Jsonaf.exactly_equal
          (R.Setting.value (setting "tool_choice"))
          Absent);
      assert (
        P.Presence.equal
          Jsonaf.exactly_equal
          (R.Setting.value (setting "parallel_tool_calls"))
          (Value `False));
      assert (List.length (R.history request) = 2));
    printf "%.1f %.1f; two actual observed attempts\n" first second);
  [%expect {| 0.4 0.8; two actual observed attempts |}]
;;

let%expect_test "rubric retains 0 to 10 normalization and rejects malformed completion" =
  Eio_main.run (fun _ ->
    let responses =
      ref
        [ {|{"correctness":10,"completeness":10,"depth":10,"style":10,"safety":10}|}
        ; {|{"correctness":11,"completeness":10,"depth":10,"style":10,"safety":10}|}
        ]
    in
    let inference, _, _ =
      execution ~target ~on_observation:ignore ~answer:(fun () ->
        let text = List.hd_exn !responses in
        responses := List.tl_exn !responses;
        text)
    in
    let module J = (val M.Evaluator.rubric_critic_judge : M.Evaluator.Judge) in
    let valid = J.evaluate ~inference "valid" in
    let invalid = J.evaluate ~inference "invalid" in
    printf "%.1f %.1f\n" valid invalid);
  [%expect {| 1.0 0.5 |}]
;;

let%expect_test "selected grading overrides structured format without changing its target"
  =
  Eio_main.run (fun _ ->
    let text =
      `Object
        [ "verbosity", `String "low"
        ; ( "format"
          , `Object
              [ "type", `String "json_schema"
              ; "name", `String "main_output"
              ; "schema", `Object [ "type", `String "object" ]
              ] )
        ; "future_text_option", `Number "1e+00"
        ]
    in
    let selected =
      R.Target.with_setting
        target
        ~name:"text"
        ~value:(Value text)
        ~provenance:Captured_prompt
        ~limits
      |> ok
    in
    let original = R.Target.to_json selected in
    let inference, requests, _ =
      execution ~target:selected ~answer:(fun () -> "0.8") ~on_observation:ignore
    in
    let module J = (val M.Evaluator.prompt_reward_model_judge : M.Evaluator.Judge) in
    assert (Float.equal (J.evaluate ~inference "candidate") 0.8);
    let effective = R.target (List.hd_exn !requests) in
    assert (String.equal (R.Target.model effective) (R.Target.model selected));
    assert (
      Option.equal String.equal (R.Target.account effective) (R.Target.account selected));
    let setting =
      List.find_exn (R.Target.settings effective) ~f:(fun setting ->
        String.equal (R.Setting.name setting) "text")
    in
    assert (R.Setting.equal_provenance (R.Setting.provenance setting) Execution_override);
    assert (
      P.Presence.equal
        Jsonaf.exactly_equal
        (R.Setting.value setting)
        (Value
           (`Object
               [ "verbosity", `String "low"
               ; "format", `Object [ "type", `String "text" ]
               ; "future_text_option", `Number "1e+00"
               ])));
    assert (Jsonaf.exactly_equal original (R.Target.to_json selected));
    print_endline
      "plain effective format; model, account, verbosity and original preserved");
  [%expect {| plain effective format; model, account, verbosity and original preserved |}]
;;

let%expect_test
    "strict observation cancellation survives model judge and consistency guard"
  =
  Eio_main.run (fun _ ->
    let inference, _, _ =
      execution
        ~target
        ~answer:(fun () -> "0.5")
        ~on_observation:(fun _ -> raise (Eio.Cancel.Cancelled Exit))
    in
    let judge =
      M.Evaluator.wrap_self_consistency_judge
        ~k:2
        ~strategy:Mean
        M.Evaluator.prompt_reward_model_judge
    in
    let evaluator = M.Evaluator.create ~judges:[ Judge judge ] () in
    let cancelled =
      try
        ignore (M.Evaluator.evaluate ~inference evaluator "candidate" : float);
        false
      with
      | Eio.Cancel.Cancelled _ -> true
    in
    assert cancelled;
    print_endline "original cancellation propagated");
  [%expect {| original cancellation propagated |}]
;;

let%expect_test "strict non-cancellation observer exceptions retain exact identity" =
  Eio_main.run (fun _ ->
    let inference, _, _ =
      execution ~target ~answer:(fun () -> "0.5") ~on_observation:(fun _ -> raise Exit)
    in
    let judge =
      M.Evaluator.wrap_self_consistency_judge
        ~k:2
        ~strategy:Mean
        M.Evaluator.prompt_reward_model_judge
    in
    let evaluator = M.Evaluator.create ~judges:[ Judge judge ] () in
    let original =
      try
        ignore (M.Evaluator.evaluate ~inference evaluator "candidate" : float);
        false
      with
      | Exit -> true
    in
    assert original;
    print_endline "original observer exception propagated");
  [%expect {| original observer exception propagated |}]
;;
