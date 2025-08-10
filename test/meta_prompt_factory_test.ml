open Core
open Expect_test_helpers_core

let%expect_test "create_pack renders required sections" =
  let open Meta_prompting in
  let p : Prompt_factory.create_params =
    { agent_name = "Test Agent"
    ; goal = "Summarise a PR"
    ; success_criteria = [ "Accurate"; "Safe" ]
    ; audience = Some "engineers"
    ; tone = Some "neutral"
    ; domain = Some "coding"
    ; use_responses_api = true
    ; markdown_allowed = true
    ; eagerness = Prompt_factory.Medium
    ; reasoning_effort = `Low
    ; verbosity_target = `Low
    }
  in
  let out = Prompt_factory.create_pack p ~prompt:"Rewrite the diff in plain English." in
  let required =
    [ "<system_prompt>"
    ; "</system_prompt>"
    ; "<assistant_rules>"
    ; "</assistant_rules>"
    ; "<tool_preambles>"
    ; "</tool_preambles>"
    ; "<agentic_controls>"
    ; "</agentic_controls>"
    ; "<context_gathering>"
    ; "</context_gathering>"
    ; "<formatting_and_verbosity>"
    ; "</formatting_and_verbosity>"
    ; "<domain_module>"
    ; "</domain_module>"
    ; "<safety_and_handback>"
    ; "</safety_and_handback>"
    ; "Recommended_API_Parameters"
    ]
  in
  let missing =
    List.filter required ~f:(fun t -> not (String.is_substring out ~substring:t))
  in
  (match missing with
   | [] -> print_endline "ok"
   | xs -> print_s (List.sexp_of_t String.sexp_of_t xs));
  [%expect {| ok |}]
;;

let%expect_test "iterate_pack renders required sections" =
  let open Meta_prompting in
  let p : Prompt_factory.iterate_params =
    { goal = "Improve the prompt"
    ; desired_behaviors = []
    ; undesired_behaviors = []
    ; safety_boundaries = []
    ; stop_conditions = []
    ; reasoning_effort = `Minimal
    ; verbosity_target = `Low
    ; use_responses_api = true
    }
  in
  let out = Prompt_factory.iterate_pack p ~current_prompt:"CURRENT_PROMPT_TEXT" in
  let required =
    [ "Overview"
    ; "Issues_Found"
    ; "Minimal_Edit_List"
    ; "Revised_Prompt"
    ; "Optional_Toggles"
    ; "API_Parameter_Suggestions"
    ; "Test_Plan"
    ; "Telemetry"
    ]
  in
  let missing =
    List.filter required ~f:(fun t -> not (String.is_substring out ~substring:t))
  in
  (match missing with
   | [] -> print_endline "ok"
   | xs -> print_s (List.sexp_of_t String.sexp_of_t xs));
  [%expect {| ok |}]
;;
