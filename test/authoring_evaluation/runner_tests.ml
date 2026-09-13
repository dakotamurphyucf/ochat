open Core
open Authoring_evaluation
open Runner

let message category text = { category; text }

let task =
  { id = "repair"
  ; family = "one_off_script"
  ; prompt = "compute a result"
  ; preload_topics = [ "contract" ]
  ; compaction_after_step = Some 2
  }
;;

let fixture ?(execution = Passed) policy =
  let validated = ref [] in
  let executed = ref [] in
  let requests = ref [] in
  let backend =
    { primer = message Primer "primer"
    ; tool_descriptions = message Tool_descriptions "tools"
    ; prepare = (fun _ -> [ message Documentation "prepared contract" ])
    ; preload = (fun _ -> [ message Documentation "preloaded contract" ])
    ; retrieve = (fun _ -> [ message Documentation "retrieved exact contract" ])
    ; validate =
        (fun json ->
          validated := json :: !validated;
          match json with
          | `String "bad" -> Invalid (Syntax, "use parenthesized calls")
          | _ -> Valid)
    ; execute =
        (fun json ->
          executed := json :: !executed;
          execution)
    }
  in
  let actions =
    [ Retrieve (`String "contract")
    ; Submit (`String "bad")
    ; Retrieve (`String "contract")
    ; Submit (`String "good")
    ]
  in
  let provider ~step ~messages =
    requests := messages :: !requests;
    { action = List.nth_exn actions (step - 1)
    ; provider_input_tokens =
        (match step with
         | 3 -> None
         | _ -> Some 100)
    }
  in
  let ticks = ref 0. in
  let now () =
    let n = !ticks in
    ticks := n +. 0.25;
    n
  in
  let result =
    run
      ~now
      ~provenance:Offline_transcript
      ~model:"scripted-v1"
      ~suite_revision:"fixture-v1"
      ~runtime_revision:"runtime-v1"
      ~limits:{ max_steps = 4; max_attempts = 2 }
      ~policy
      ~backend
      ~provider
      task
  in
  assert (
    List.equal
      Jsonaf.exactly_equal
      (List.rev !validated)
      [ `String "bad"; `String "good" ]);
  assert (List.equal Jsonaf.exactly_equal !executed [ `String "good" ]);
  let requests = List.rev !requests in
  assert (documentation_tokens (List.nth_exn requests 1) > 0);
  assert (documentation_tokens (List.nth_exn requests 2) = 0);
  assert (documentation_tokens (List.nth_exn requests 3) > 0);
  assert (Option.is_none result.tokens.provider_input);
  result
;;

let%expect_test "retrieval, repair and context loss have distinct measured costs" =
  let results = List.map policies ~f:fixture in
  List.iter results ~f:(fun r ->
    print_s
      [%sexp
        (r.policy : policy)
      , (r.termination : termination)
      , (r.first_pass_compile : bool)
      , (r.runtime_success : bool)
      , (r.repairs : int)
      , (r.retrieval_calls : int)
      , (r.compactions : int)
      , (r.tokens : tokens)]);
  let scores = compare ~task_ids:[ "repair" ] results |> Result.ok_or_failwith in
  assert (List.for_all scores ~f:(fun s -> Float.equal s.runtime_success_rate 1.));
  [%expect
    {|
    (Minimal Succeeded false true 1 2 1
     ((primer 2) (tool_descriptions 2) (documentation_delivered 16)
      (documentation_effective_input 16) (total_effective_input 212)
      (provider_input ())))
    (Automatic_retrieval Succeeded false true 1 2 1
     ((primer 2) (tool_descriptions 2) (documentation_delivered 22)
      (documentation_effective_input 28) (total_effective_input 224)
      (provider_input ())))
    (Selected_preload Succeeded false true 1 2 1
     ((primer 2) (tool_descriptions 2) (documentation_delivered 22)
      (documentation_effective_input 28) (total_effective_input 224)
      (provider_input ())))
    |}]
;;

let%expect_test "validation success cannot fabricate runtime success or quality evidence" =
  let result = fixture ~execution:(Not_run "backend unavailable") Minimal in
  print_s
    [%sexp
      (result.termination : termination)
    , (result.runtime_success : bool)
    , (result.provenance : provenance)
    , (result.attempts : attempt list)];
  let rows = List.map policies ~f:fixture in
  let report rows =
    match compare ~task_ids:[ "repair" ] rows with
    | Ok _ -> failwith "accepted incomplete or mixed evaluation"
    | Error message -> print_endline message
  in
  report (List.tl_exn rows);
  report (List.hd_exn rows :: rows);
  report
    (List.mapi rows ~f:(fun i row ->
       match i with
       | 0 -> { row with provenance = Real_model }
       | _ -> row));
  report
    (List.mapi rows ~f:(fun i row ->
       match i with
       | 0 -> { row with limits = { row.limits with max_attempts = 9 } }
       | _ -> row));
  [%expect
    {|
    (Attempts_exhausted false Offline_transcript
     (((validation (Invalid Syntax "use parenthesized calls")) (execution ()))
      ((validation Valid) (execution ((Not_run "backend unavailable"))))))
    incomparable evaluation: each policy must have exactly the full task set
    incomparable evaluation: each policy must have exactly the full task set
    incomparable evaluation: provenance, model, revisions or limits differ
    incomparable evaluation: provenance, model, revisions or limits differ
    |}]
;;

let%expect_test "a retrieval-only provider cannot evade the model-step bound" =
  let backend =
    { primer = message Primer "primer"
    ; tool_descriptions = message Tool_descriptions "tools"
    ; prepare = (fun _ -> [])
    ; preload = (fun _ -> [])
    ; retrieve = (fun _ -> [ message Documentation "contract" ])
    ; validate = (fun _ -> failwith "unexpected submission")
    ; execute = (fun _ -> failwith "unexpected execution")
    }
  in
  let provider ~step:_ ~messages:_ =
    { action = Retrieve `Null; provider_input_tokens = Some 10 }
  in
  let result =
    run
      ~now:(fun () -> 0.)
      ~provenance:Offline_transcript
      ~model:"loop"
      ~suite_revision:"fixture-v1"
      ~runtime_revision:"runtime-v1"
      ~limits:{ max_steps = 3; max_attempts = 2 }
      ~policy:Minimal
      ~backend
      ~provider
      { task with compaction_after_step = None }
  in
  print_s
    [%sexp
      (result.termination : termination)
    , (result.provider_steps : int)
    , (result.retrieval_calls : int)
    , (result.attempts : attempt list)
    , (result.tokens.documentation_delivered : int)
    , (result.tokens.documentation_effective_input : int)
    , (result.tokens.provider_input : int option)];
  [%expect {| (Steps_exhausted 3 3 () 9 9 (30)) |}]
;;
