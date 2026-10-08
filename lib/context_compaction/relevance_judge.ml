open! Core

let system_prompt =
  {|# Role & Objective
You are an impartial grader of the importance of individual chat messages so another assistant can compress the conversation while still being able to resume it seamlessly.

# Instructions

## Evaluation Criteria
• Judge how indispensable the MESSAGE is for preserving the conversation’s meaning and allowing the assistant to pick up where it left off.
• Highest-importance messages contain crucial information the assistant could not easily infer or replace.
• Lowest-importance messages are redundant, predictable, off-topic, or purely social.
• Consider the message’s role within the evolving dialogue; importance may change as the conversation progresses.
• Clarifications, acknowledgments, greetings, or side chatter are generally low importance.
• Messages that introduce new topics, pivot the discussion, provide key data, instructions, or essential function-call output are high importance.
• Messages with factual errors are low importance unless their correction is vital to future steps.
• Rate necessity, not writing quality.
• Adopt a strict standard: keep only what is truly necessary.
• Assume the user recalls nothing from earlier; retained messages must supply all needed context.

## Scoring
Return a single floating-point number in the closed interval [0, 1]:
0 = irrelevant, safely droppable
0.5 = somewhat important; dropping causes only minor loss
1 = crucial, must keep

## Response Rules
• Output the bare number only—no extra text, labels, or formatting.
• Do not provide explanations or reasoning.

# Examples
Example-A
Message: “The database password is env var DB_PASS, set to ‘moonRiver42’.”
Your output:
1

Example-B
Message: “Thanks for clarifying!”
Your output:
0|}
;;

let score_samples ~sample =
  List.init 3 ~f:(fun _ ->
    match sample () with
    | Error _ -> 0.5
    | Ok text ->
      Option.value (Meta_prompting.Evaluator.Score.of_string text ~max:1.) ~default:0.5)
  |> List.sum (module Float) ~f:Fn.id
  |> fun sum -> sum /. 3.
;;

let score_relevance ~inference (_cfg : Config.t) ~prompt =
  let setting =
    Inference.Request.Setting.create
      ~name:"reasoning"
      ~value:(Value (`Object [ "effort", `String "low" ]))
      ~provenance:Execution_override
      ~limits:Transcript.Admission.default
    |> Result.map_error ~f:(fun _ -> "invalid fixed relevance setting")
    |> Result.ok_or_failwith
  in
  score_samples ~sample:(fun () ->
    Eio.Switch.run (fun sw ->
      Inference_client.Execution.complete_text
        inference
        ~sw
        ~settings:[ setting ]
        ~messages:[ System, system_prompt; User, prompt ]
        ()))
;;

let is_relevant ~inference cfg ~prompt =
  Float.(score_relevance ~inference cfg ~prompt >= cfg.Config.relevance_threshold)
;;

module For_testing = struct
  let score_samples = score_samples
end
