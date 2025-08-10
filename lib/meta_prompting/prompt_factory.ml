open Core

[@@@warning "-16-27-32-39"]

type eagerness =
  | Low
  | Medium
  | High

type create_params =
  { agent_name : string
  ; goal : string
  ; success_criteria : string list
  ; audience : string option
  ; tone : string option
  ; domain : string option
  ; use_responses_api : bool
  ; markdown_allowed : bool
  ; eagerness : eagerness
  ; reasoning_effort : [ `Minimal | `Low | `Medium | `High ]
  ; verbosity_target : [ `Low | `Medium | `High ]
  }

type iterate_params =
  { goal : string
  ; desired_behaviors : string list
  ; undesired_behaviors : string list
  ; safety_boundaries : string list
  ; stop_conditions : string list
  ; reasoning_effort : [ `Minimal | `Low | `Medium | `High ]
  ; verbosity_target : [ `Low | `Medium | `High ]
  ; use_responses_api : bool
  }

let string_of_eagerness = function
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"
;;

let string_of_re = function
  | `Minimal -> "minimal"
  | `Low -> "low"
  | `Medium -> "medium"
  | `High -> "high"
;;

let string_of_v = function
  | `Low -> "low"
  | `Medium -> "medium"
  | `High -> "high"
;;

let guardrail_lines =
  [ "After apply_patch, consider files saved; do not instruct users to save or copy code."
  ; "Avoid displaying large file contents unless explicitly requested."
  ; "When emitting a tool call, output ONLY the function-call object; preambles are used \
     only when not emitting a tool call."
  ]
;;

let guardrails_block () =
  let header = "Integration Guardrails" in
  let body =
    String.concat
      ~sep:"\n"
      ([ "- Safety, permissions, and repository constraints take precedence"
       ; "- Follow plan workflow strictly (pending → in_progress → completed)"
       ; "- Prefer low-risk tools first; apply write tools deliberately"
       ; "- Do not mix preambles with tool-call JSON outputs"
       ]
       @ List.map guardrail_lines ~f:(fun l -> "- " ^ l))
  in
  Printf.sprintf "\n## %s\n%s\n" header body
;;

let mk_system_prompt ~(p : create_params) ~prompt : string =
  let use_prev =
    if p.use_responses_api then "Enable reuse of previous_response_id." else ""
  in
  let role = p.agent_name in
  let audience = Option.value p.audience ~default:"technical users" in
  let tone = Option.value p.tone ~default:"neutral" in
  let sc =
    match p.success_criteria with
    | [] -> "- Adhere to repository policies; - Produce correct, safe results."
    | xs -> String.concat ~sep:"\n- " ("- " :: xs)
  in
  Printf.sprintf
    "<system_prompt>\n\
     Role: %s. Objective: %s. Success criteria:\n\
     %s\n\
     Audience/Tone: %s / %s.\n\
     Instruction hierarchy: system > developer > tools > user. Resolve conflicts by \
     prioritising safety and stop conditions. %s\n\
     %s\n\
     </system_prompt>"
    role
    p.goal
    sc
    audience
    tone
    use_prev
    (guardrails_block ())
;;

let mk_assistant_rules ~stop_conditions =
  let stops =
    if List.is_empty stop_conditions
    then "(none supplied)"
    else String.concat ~sep:"; " stop_conditions
  in
  Printf.sprintf
    "<assistant_rules>\n\
     Do:\n\
     - Follow the OUTPUT_CONTRACT and markdown policy.\n\
     - Respect knowledge boundaries; verify via tools when required.\n\
     - Prefer clear, concise reasoning; avoid redundant tool use.\n\
     Don’t:\n\
     - Perform unsafe/irreversible actions without explicit confirmation.\n\
     - Ask for confirmation when reversible and safe progress can be made.\n\
     Stop conditions: %s.\n\
     </assistant_rules>"
    stops
;;

let mk_tool_preambles () =
  "<tool_preambles>\n\
   Frequency: upfront plan once; brief progress updates at each meaningful step (or \
   after N tool calls, default N=1–2); final completion summary.\n\
   Content:\n\
   - Rephrase the user goal succinctly.\n\
   - Outline a short, ordered plan of steps.\n\
   - Provide step‑tied progress markers during execution.\n\
   - Summarise completed work vs plan before ending turn.\n\
   Style: concise for user‑facing text; allow higher verbosity in code/diff tools if \
   configured.\n\
   </tool_preambles>"
;;

let mk_agentic_controls eagerness =
  let p_active, b_active =
    match eagerness with
    | Low -> false, true
    | Medium -> false, true
    | High -> true, false
  in
  Printf.sprintf
    "<agentic_controls>\n\
     <persistence active=\"%b\">\n\
     - Keep going until the user’s query is completely resolved before ending your turn.\n\
     - Don’t hand back on uncertainty; research or deduce a reasonable approach; \
     document assumptions afterward.\n\
     - Prefer safe, proactive progress over asking confirmatory questions for reversible \
     steps.\n\
     </persistence>\n\
     <bounded_exploration active=\"%b\">\n\
     - Minimise tool calls and latency; apply early-stop criteria.\n\
     - Tool‑call budget based on eagerness.\n\
     - Proceed under acceptable uncertainty; document assumptions.\n\
     </bounded_exploration>\n\
     </agentic_controls>"
    p_active
    b_active
;;

let mk_context_gathering ~eagerness ~reasoning_effort =
  let budget =
    match eagerness with
    | Low -> 2
    | Medium -> 4
    | High -> 6
  in
  let helper =
    match reasoning_effort with
    | `Minimal | `Low ->
      "\n\
       Minimal/Low reasoning helper: Begin with a 3–5 bullet plan; ensure all \
       sub‑requests are completed before finishing."
    | `Medium | `High -> ""
  in
  Printf.sprintf
    "<context_gathering>\n\
     Goal: gather enough context fast.\n\
     Method:\n\
     - Start broad, then focus. Batch parallel queries; dedupe; cache; avoid repeats.\n\
     Early‑stop criteria:\n\
     - You can name exact actions to take, and top sources converge (~70%%).\n\
     Escalation:\n\
     - If signals conflict or scope is fuzzy, run one refined parallel batch, then act.\n\
     Depth:\n\
     - Trace only symbols/contracts you’ll modify or rely on; avoid unnecessary \
     transitive expansion.\n\
     Tool budgets: %d.\n\
     %s\n\
     </context_gathering>"
    budget
    helper
;;

let mk_formatting_and_verbosity ~markdown_allowed ~verbosity_target =
  Printf.sprintf
    "<formatting_and_verbosity>\n\
     Markdown: %b. If true, use Markdown only when semantically appropriate.\n\
     Verbosity: %s.\n\
     Reassert formatting instructions every 3–5 user turns in long conversations.\n\
     </formatting_and_verbosity>"
    markdown_allowed
    (string_of_v verbosity_target)
;;

let mk_domain_module = function
  | Some d when String.Caseless.equal d "coding" ->
    "<domain_module>\n\
     Coding clarity:\n\
     - Write code for clarity first; readable names; helpful comments where appropriate; \
     straightforward control flow.\n\
     - Make well‑structured, reviewable diffs; prefer small, focused changes unless a \
     large refactor is requested.\n\
     Codebase alignment:\n\
     - Match existing style and conventions; blend in with project patterns.\n\
     </domain_module>"
  | _ -> ""
;;

let mk_safety_and_handback ~safety_boundaries ~stop_conditions =
  let allowed = "[derive from environment tools]" in
  let denied = String.concat ~sep:"; " safety_boundaries in
  let stops = String.concat ~sep:"; " stop_conditions in
  Printf.sprintf
    "<safety_and_handback>\n\
     Allowed vs denied operations: %s / %s.\n\
     High‑risk actions require explicit user confirmation (irreversible, destructive, \
     financial, or privacy‑impacting operations).\n\
     Handback when: %s.\n\
     Failure recovery: on failure, attempt one alternate path; if still blocked, \
     summarise blockers and present options.\n\
     </safety_and_handback>"
    allowed
    denied
    (if String.is_empty stops then "per repository policy" else stops)
;;

let mk_recommended_api ~model ~reasoning_effort ~verbosity_target ~use_responses_api =
  Printf.sprintf
    "Recommended_API_Parameters\n\
     - model: %s\n\
     - reasoning_effort: %s\n\
     - verbosity: %s\n\
     - Responses API: %b. If true, reuse previous_response_id to carry reasoning context.\n"
    model
    (string_of_re reasoning_effort)
    (string_of_v verbosity_target)
    use_responses_api
;;

let create_pack (p : create_params) ~prompt : string =
  let system_prompt = mk_system_prompt ~p ~prompt in
  let assistant_rules = mk_assistant_rules ~stop_conditions:[] in
  let tool_preambles = mk_tool_preambles () in
  let agentic_controls = mk_agentic_controls p.eagerness in
  let context_gathering =
    mk_context_gathering ~eagerness:p.eagerness ~reasoning_effort:p.reasoning_effort
  in
  let fav =
    mk_formatting_and_verbosity
      ~markdown_allowed:p.markdown_allowed
      ~verbosity_target:p.verbosity_target
  in
  let domain_mod = mk_domain_module p.domain in
  let safety = mk_safety_and_handback ~safety_boundaries:[] ~stop_conditions:[] in
  let api =
    mk_recommended_api
      ~model:"gpt-5"
      ~reasoning_effort:p.reasoning_effort
      ~verbosity_target:p.verbosity_target
      ~use_responses_api:p.use_responses_api
  in
  let smoke =
    "Smoke_Test_Checklist\n\
     - No internal contradictions; stop conditions and safety gates are explicit.\n\
     - Tool preambles present; upfront plan, progress updates, final summary.\n\
     - Eagerness behaviour matches profile; respects tool-call budgets and early-stop \
     criteria.\n\
     - Formatting matches OUTPUT_CONTRACT and markdown policy.\n\
     - Minimal/low reasoning emits an upfront plan and completes all sub‑requests.\n\
     - Responses API reuse confirmed (if enabled).\n"
  in
  let telem =
    "Telemetry\n\
     - Resolution rate; average turns to completion.\n\
     - Avg tool calls per task vs budget; unnecessary handbacks rate.\n\
     - Latency per step; token usage split (reasoning vs output).\n\
     - Instruction violations: formatting, safety, stop‑conditions.\n"
  in
  String.concat
    ~sep:"\n\n"
    [ "Assumptions"
    ; "- Filled defaults for unspecified inputs per repository policy"
    ; "Prompt Pack"
    ; system_prompt
    ; assistant_rules
    ; tool_preambles
    ; agentic_controls
    ; context_gathering
    ; fav
    ; domain_mod
    ; safety
    ; api
    ; smoke
    ; telem
    ]
;;

let iterate_pack (p : iterate_params) ~current_prompt : string =
  let overview = Printf.sprintf "Overview\nGoal: %s\n" p.goal in
  let issues = "Issues_Found\n- Placeholder: detect contradictions and vagueness.\n" in
  let edits =
    "Minimal_Edit_List\n\
    \  - Add: \"<tool_preambles>…</tool_preambles>\" (after system prompt)\n\
    \  - Delete: \"redundant guidance\"\n\
    \  - Replace: \"weak phrasing\" -> \"explicit rule\"\n"
  in
  let revised = Printf.sprintf "Revised_Prompt\n%s\n" current_prompt in
  let toggles =
    "Optional_Toggles\n\
     <persistence>…</persistence>\n\
     <bounded_exploration>…</bounded_exploration>\n\
     Per_tool_verbosity_override: \"code: high\"\n"
  in
  let api =
    Printf.sprintf
      "API_Parameter_Suggestions\n\
       - model: gpt-5\n\
       - reasoning_effort: %s\n\
       - verbosity: %s\n\
       - Responses API: %b\n"
      (string_of_re p.reasoning_effort)
      (string_of_v p.verbosity_target)
      p.use_responses_api
  in
  let test_plan =
    "Test_Plan\n\
     - Success: minimal/medium tasks complete without unnecessary handback.\n\
     - Edge: ambiguous inputs resolved with one escalation cycle.\n\
     - Adversarial: inject conflicts; ensure contradiction_check resolves them.\n"
  in
  let telemetry =
    "Telemetry\n\
     - Resolution rate; average turns; tool-call count vs budget.\n\
     - Instruction violations (formatting, safety, stop-conditions).\n\
     - Latency and token split; unnecessary handbacks.\n"
  in
  String.concat
    ~sep:"\n\n"
    [ overview; issues; edits; revised; toggles; api; test_plan; telemetry ]
;;
