open Core
open Meta_prompting

(*------------------------------------------------------------------*)
(*  Relevance scoring                                               *)
(*------------------------------------------------------------------*)

module E = Evaluator

(*------------------------------------------------------------------*)
(*  Domain-specific judge – message importance                       *)
(*------------------------------------------------------------------*)

module Importance_judge : E.Judge = struct
  let name = "importance"

  (* Memoised presence check to stay offline-friendly in CI *)
  let api_key_present = lazy (Option.is_some (Sys.getenv "OPENAI_API_KEY"))

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

  let call_openai ~(env : Eio_unix.Stdenv.base) (candidate : string) : float option =
    if not (Stdlib.Lazy.force api_key_present)
    then None
    else (
      try
        let dir = Eio.Stdenv.fs env in
        let net = Eio.Stdenv.net env in
        let open Openai.Grader in
        let sampling_params =
          Sampling_params.
            { temperature = None
            ; top_p = None
            ; seed = None
            ; reasoning_effort = Some "low"
            }
        in
        let reward =
          run_score_model_or_stub
            ~prompt:system_prompt
            ~sampling_params
            ~candidate
            ~dir
            ~net
            ()
        in
        Some (Float.max 0.0 (Float.min 1.0 reward))
      with
      | _ -> None)
  ;;

  let evaluate ?env candidate =
    match env with
    | None -> 0.5 (* offline default *)
    | Some env ->
      (match call_openai ~env candidate with
       | Some s -> s
       | None -> 0.5)
  ;;
end

(*------------------------------------------------------------------*)
(*  Shared evaluator instance                                       *)
(*------------------------------------------------------------------*)

(* The relevance judge should be fast and reasonably accurate while
   remaining offline-friendly when no [OPENAI_API_KEY] is present.  We
   therefore combine three complementary judges:

   • [Reward_model_judge] – high-fidelity scalar reward (needs network)
   • [Guidelines_judge]   – heuristic / optional LLM call
   • [Logprob_judge]      – cheap length-penalty proxy (always local)

   The ensemble is aggregated via the default arithmetic mean. *)

let evaluator : E.t Lazy.t =
  lazy
    (let importance_judge : (module E.Judge) =
       E.wrap_self_consistency_judge ~k:3 ~strategy:Mean (module Importance_judge)
     in
     E.create ~judges:[ E.Judge importance_judge ] ())
;;

(*------------------------------------------------------------------*)
(*  Public API                                                      *)
(*------------------------------------------------------------------*)

let score_relevance ?env (_cfg : Config.t) ~prompt =
  (* Delegate to the evaluator; guard against unexpected exceptions so
     that the compaction pipeline never crashes the host application. *)
  try E.evaluate ?env (Lazy.force evaluator) prompt with
  | _ -> 0.5
;;

let is_relevant ?env (cfg : Config.t) ~prompt =
  Float.(score_relevance ?env cfg ~prompt >= cfg.Config.relevance_threshold)
;;
