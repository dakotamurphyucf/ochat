open Core

(** Evaluator – flexible scoring framework for prompt quality.  An
    [Evaluator.t] combines one or more {e judges}.  Each judge maps a
    candidate answer (here represented simply as [string]) to a
    numeric score.  The evaluator caches results and can parallelise
    judgements in the future via a task-pool (left TODO – the current
    implementation executes sequentially).

    Pure/injected judges execute offline. Model judges require selected
    inference and preserve cancellation and strict host callback failures. *)

type score = float

exception Configuration_required = Inference_support.Configuration_required
exception Expected_model_failure

module Score = struct
  type t = score

  let of_string = Inference_support.score
end

(*********************************************************************
 *  Judge interface and helpers                                      *
 ********************************************************************)

open Eio

module type Judge = sig
  val name : string

  (** Optional [env] supplies Eio capabilities (net, fs …).  Judges that do
      not rely on external IO simply ignore it. *)
  val evaluate
    :  ?env:Eio_unix.Stdenv.base
    -> ?inference:Inference_client.Execution.t
    -> string
    -> score
end

(*--------------------------------------------------------------------
 *  Guidelines compliance judge                                       *
 *--------------------------------------------------------------------*)

module Guidelines_judge : Judge = struct
  let name = "guidelines"

  let system_prompt =
    {|
  You are an impartial judge evaluating how closely a prompt follows
  best practices for prompting openai o-series and gpt4.1 models.
  Your task is to score how well a prompt adheres to the guidelines in the Prompting Guide appended to the model response.
  The score should be a floating-point number in the range [0,1], where
  0.0 means "not at all" and 1.0 means "perfectly".
  |}
  ;;

  let evaluate ?env:_ ?inference candidate =
    let inference = Inference_support.require inference in
    let settings =
      [ Inference_support.setting "temperature" (`Number "1.0")
      ; Inference_support.setting "reasoning" (`Object [ "effort", `String "high" ])
      ]
    in
    match
      Inference_support.complete
        inference
        ~settings
        ~messages:[ System, system_prompt; User, candidate ]
        ()
    with
    | Ok text -> Option.value (Inference_support.score text ~max:1.0) ~default:0.5
    | Error _ -> 0.5
  ;;
end

(*--------------------------------------------------------------------
 *  Pairwise judge interface                                          *
 *--------------------------------------------------------------------*)

module type Pairwise_judge = sig
  val name : string

  (** [evaluate ~incumbent ~challenger ?env] compares two answers and
      returns a score in [0,1] where 1.0 means the [challenger] wins,
      0.0 means the [incumbent] wins, and 0.5 indicates a tie. *)
  val evaluate
    :  incumbent:string
    -> challenger:string
    -> ?env:Eio_unix.Stdenv.base
    -> ?inference:Inference_client.Execution.t
    -> unit
    -> score
end

type judge =
  | Judge of (module Judge)
  | Pairwise_judge of (module Pairwise_judge)

(*--------------------------------------------------------------------
 *  Pairwise arena judge                                              *
 *--------------------------------------------------------------------*)

module Pairwise_arena_judge : Pairwise_judge = struct
  let name = "pairwise_arena"

  (* ----------------------------------------------------------------- *)
  (*  Elo utilities                                                    *)
  (* ----------------------------------------------------------------- *)

  (* Default initial rating recommended by most LLM arena
     implementations.  The absolute number is arbitrary as only
     differences matter. *)
  let default_rating = 1200.0

  (* How aggressively to update the rating after each comparison.  We
     follow common practise and expose [K] via an environment variable
     for easy tuning. *)
  let k_factor () : float =
    match Sys.getenv "ARENA_ELO_K" with
    | Some s ->
      (try Float.of_string s with
       | _ -> 32.)
    | None -> 32.
  ;;

  (* Internal store mapping the {i entire answer string} to its current
     Elo rating.  This is {b intentionally} keyed on the full string
     rather than a hash to keep the implementation simple and avoid
     collisions; callers are expected to use deduplicated candidate
     pools. *)
  let ratings : (string, float) Hashtbl.t = Hashtbl.create (module String)

  let get_rating candidate =
    Hashtbl.find_or_add ratings candidate ~default:(fun () -> default_rating)
  ;;

  let set_rating candidate rating = Hashtbl.set ratings ~key:candidate ~data:rating

  (* Compute the expected score of [ra] against [rb] using the standard
     Elo logistic model. *)
  let expected ra rb = 1. /. (1. +. Stdlib.Float.pow 10. ((rb -. ra) /. 400.))

  (* Update Elo ratings given outcome [sa] (1.0 win, 0.5 tie, 0.0 loss)
     for player A (incumbent) and [sb] for player B (challenger).  The
     function returns the updated pair. *)
  let elo_update ~ra ~rb ~sa ~sb =
    let k = k_factor () in
    let ra' = ra +. (k *. (sa -. expected ra rb)) in
    let rb' = rb +. (k *. (sb -. expected rb ra)) in
    ra', rb'
  ;;

  type result =
    | Incumbent_win
    | Challenger_win
    | Tie

  let parse_llm_response text =
    match String.lowercase (String.strip text) with
    | "a" -> Some Incumbent_win
    | "b" -> Some Challenger_win
    | "tie" | "draw" -> Some Tie
    | _ -> None
  ;;

  let evaluate ~incumbent ~challenger ?env:_ ?inference () =
    let inference = Inference_support.require inference in
    let settings =
      [ Inference_support.setting
          "reasoning"
          (`Object [ "effort", `String "high"; "summary", `String "detailed" ])
      ; Inference_support.setting "max_output_tokens" (`Number "10000")
      ]
    in
    let result =
      match
        Inference_support.complete
          inference
          ~settings
          ~messages:
            [ ( System
              , "You are an impartial referee in a head-to-head LLM arena.\n\
                 Two Prompts (A and B) for the same Task are provided.\n\
                 Decide which prompt best follows the Task and guidlines in terms of\n\
                 correctness, completeness, depth, style.\n\
                 Respond with *only* the single character 'A', 'B', or 'Tie'." )
            ; ( User
              , Printf.sprintf
                  "Prompt A:\n%s\n\nPrompt B:\n\n%s\n\nWhich answer is better?"
                  incumbent
                  challenger )
            ]
          ()
      with
      | Ok text -> Option.value (parse_llm_response text) ~default:Tie
      | Error _ -> Tie
    in
    let ra = get_rating incumbent in
    let rb = get_rating challenger in
    let sa, sb =
      match result with
      | Incumbent_win -> 1., 0.
      | Challenger_win -> 0., 1.
      | Tie -> 0.5, 0.5
    in
    let ra', rb' = elo_update ~ra ~rb ~sa ~sb in
    set_rating incumbent ra';
    set_rating challenger rb';
    expected rb' ra'
  ;;
end

let pairwise_arena_judge : (module Pairwise_judge) = (module Pairwise_arena_judge)

(* [with_exception_guard j f] runs [f] and traps any exception,
   returning [None] on failure and logging an error with the judge
   name.  This prevents a single crashing judge from aborting the
   whole evaluation. *)
(*--------------------------------------------------------------------
 *  Execution helpers                                                 *
 *-------------------------------------------------------------------*)

let judge_timeout_seconds () : float =
  match Sys.getenv "EVAL_JUDGE_TIMEOUT" with
  | Some s ->
    (try Float.of_string s with
     | _ -> 400.0)
  | None -> 400.0
;;

let pool_size () : int =
  match Sys.getenv "EVAL_POOL_SIZE" with
  | Some s ->
    (try Int.of_string s with
     | _ -> 20)
  | None -> 20
;;

let with_exception_guard
      ~(clock : _ Eio.Time.clock)
      ~(timeout_s : float)
      (j : judge)
      ?env
      ?inference
      ?best
      (candidate : string)
  : score option
  =
  let incumbent =
    match best with
    | Some b -> b
    | None -> candidate
  in
  let name =
    match j with
    | Judge (module J) -> J.name
    | Pairwise_judge (module J) -> J.name
  in
  Log.emit `Debug (sprintf "Evaluator.with_exception_guard: %s" name);
  (* -----------------------------------------------------------------
     Wrap the judge evaluation in a fiber to allow cancellation. *)
  (* ----------------------------------------------------------------- *)
  let wrap () =
    match env, j with
    | None, Judge (module J) -> J.evaluate ?inference candidate
    | Some env', Judge (module J) -> J.evaluate ~env:env' ?inference candidate
    | None, Pairwise_judge (module J) ->
      J.evaluate ~incumbent ~challenger:candidate ?inference ()
    | Some env', Pairwise_judge (module J) ->
      J.evaluate ~env:env' ~incumbent ~challenger:candidate ?inference ()
  in
  try
    let res =
      (* Cancel the evaluation fiber if it exceeds [timeout_s] seconds. *)
      match Eio.Time.with_timeout clock timeout_s (fun () -> Ok (wrap ())) with
      | Ok score -> Some score
      | Error `Timeout ->
        Log.emit `Debug (sprintf "%s judge timed out after %.1fs" name timeout_s);
        eprintf "%s judge timed out after %.1fs\n%!" name timeout_s;
        None
    in
    res
  with
  | (Eio.Cancel.Cancelled _ | Configuration_required) as exn -> raise exn
  | Expected_model_failure -> None
  | exn ->
    if Option.is_some inference then raise exn;
    Log.emit `Debug (sprintf "%s judge error: %s" name (Core.Exn.to_string exn));
    (* Log the error but do not crash the caller. *)
    (* This is useful in unit tests that expect some judges to fail. *)
    eprintf "%s judge error: %s\n%!" name (Core.Exn.to_string exn);
    None
;;

(*--------------------------------------------------------------------
 *  Sequential guard (no timeout, no concurrency)                     *
 *-------------------------------------------------------------------*)

let with_exception_guard_sequential (j : judge) ?env ?inference ?best (candidate : string)
  : score option
  =
  try
    Log.emit
      `Debug
      (sprintf
         "Evaluator.with_exception_guard_sequential: %s"
         (match j with
          | Judge (module J) -> J.name
          | Pairwise_judge (module J) -> J.name));
    (* No timeout, no concurrency – just run the judge and return the score. *)
    let incumbent =
      match best with
      | Some b -> b
      | None -> candidate
    in
    let score =
      match env, j with
      | None, Judge (module J) -> J.evaluate ?inference candidate
      | Some env', Judge (module J) -> J.evaluate ~env:env' ?inference candidate
      | None, Pairwise_judge (module J) ->
        J.evaluate ~incumbent ~challenger:candidate ?inference ()
      | Some env', Pairwise_judge (module J) ->
        J.evaluate ~env:env' ~incumbent ~challenger:candidate ?inference ()
    in
    Some score
  with
  | (Eio.Cancel.Cancelled _ | Configuration_required) as exn -> raise exn
  | Expected_model_failure -> None
  | exn ->
    if Option.is_some inference then raise exn;
    Log.emit
      `Debug
      (sprintf "Evaluator.with_exception_guard_sequential: %s" (Core.Exn.to_string exn));
    (* Log the error but do not crash the caller. *)
    (* This is useful in unit tests that expect some judges to fail. *)
    eprintf
      "%s judge error: %s\n%!"
      (match j with
       | Judge (module J) -> J.name
       | Pairwise_judge (module J) -> J.name)
      (Core.Exn.to_string exn);
    None
;;

(*********************************************************************
 *  Built-in judges                                                  *
 ********************************************************************)

module Mock_judge : Judge = struct
  let name = "mock"
  let evaluate ?env:_ ?inference:_ _candidate = 0.5
end

(** {2 Regex-based judge}

    Accepts a POSIX regular expression and returns 1.0 when the pattern
    matches, else 0.0.  Useful in unit tests. *)

module Answer_regex_judge (P : sig
    val regex : string
  end) : Judge = struct
  let name = "answer_regex"
  let re = Re.Pcre.regexp ~flags:[ `DOTALL ] P.regex

  let evaluate ?env:_ ?inference:_ candidate =
    if Re.Pcre.pmatch ~rex:re candidate then 1.0 else 0.0
  ;;
end

(** {2 Log-probability judge}

    Very naive proxy: longer answers get lower scores.  A proper
    implementation would call the model with [logprobs=True] and sum
    the token log-probs. *)

module Logprob_judge : Judge = struct
  let name = "length_penalty"

  let evaluate ?env:_ ?inference:_ candidate =
    let len = String.length candidate |> float_of_int in
    (* Map length to (0,1] with a simple exponential decay. *)
    Float.exp (-.len /. 1000.)
  ;;
end

(** {2 LLM-powered judge}

    Selected inference grades under the existing 0–10 prompt; expected
    completion/parse failures return 0.5. Missing configuration is an error. *)

module Llm_judge : Judge = struct
  let name = "llm_judge"

  let evaluate ?env:_ ?inference candidate =
    let inference = Inference_support.require inference in
    match
      Inference_support.complete
        inference
        ~settings:[ Inference_support.setting "max_output_tokens" (`Number "5") ]
        ~messages:
          [ ( System
            , "You are an impartial grader.  Given the \n\
               candidate response enclosed below, return a\n\
               single floating-point score in the range 0–10\n\
               (inclusive) that reflects its overall quality.\n\
               Reply with the bare number only – no text, no\n\
               punctuation." )
          ; User, candidate
          ]
        ()
    with
    | Ok text ->
      Option.value_map
        (Inference_support.score text ~max:10.)
        ~default:0.5
        ~f:(fun value -> value /. 10.)
    | Error _ -> 0.5
  ;;
end

module Rubric_critic_judge : Judge = struct
  let name = "rubric_critic"
  let aspects = [ "correctness"; "completeness"; "depth"; "style"; "safety" ]

  let system_prompt =
    "You are an impartial grader. Given the candidate answer "
    ^ "enclosed below, evaluate it along the following five aspects: "
    ^ "correctness, completeness, depth, style, and safety.\n"
    ^ "Return ONLY a single JSON object with these keys and a numeric "
    ^ "score between 0 and 10 for each. No additional text."
  ;;

  let extract_scores text =
    if String.length text > 4096
    then None
    else (
      match Jsonaf.parse text with
      | Ok (`Object fields)
        when List.equal
               String.equal
               (List.sort (List.map fields ~f:fst) ~compare:String.compare)
               (List.sort aspects ~compare:String.compare) ->
        Option.all
          (List.map aspects ~f:(fun name ->
             match List.Assoc.find fields name ~equal:String.equal with
             | Some (`Number number) -> Inference_support.score number ~max:10.
             | Some _ | None -> None))
      | Ok _ | Error _ -> None)
  ;;

  let evaluate ?env:_ ?inference candidate =
    let inference = Inference_support.require inference in
    match
      Inference_support.complete
        inference
        ~settings:
          [ Inference_support.setting "temperature" (`Number "0.0")
          ; Inference_support.setting "max_output_tokens" (`Number "120")
          ]
        ~messages:[ System, system_prompt; User, candidate ]
        ()
    with
    | Ok text ->
      Option.value_map (extract_scores text) ~default:0.5 ~f:(fun values ->
        List.fold values ~init:0. ~f:( +. ) /. Float.of_int (List.length values) /. 10.)
    | Error _ -> 0.5
  ;;
end

let rubric_critic_judge : (module Judge) = (module Rubric_critic_judge)

(*--------------------------------------------------------------------
 *  Reward model judge                                                *
 *--------------------------------------------------------------------*)

module Generate_Reward_model_judge (P : sig
    val name : string
    val system_prompt : string
  end) : Judge = struct
  let name = P.name

  let evaluate ?env:_ ?inference candidate =
    let inference = Inference_support.require inference in
    match
      Inference_support.complete
        inference
        ~settings:
          [ Inference_support.setting "temperature" (`Number "1.0")
          ; Inference_support.setting "reasoning" (`Object [ "effort", `String "low" ])
          ]
        ~messages:[ System, P.system_prompt; User, candidate ]
        ()
    with
    | Ok text ->
      (match Inference_support.score text ~max:1. with
       | Some score -> score
       | None -> raise Expected_model_failure)
    | Error _ -> raise Expected_model_failure
  ;;
end

(** {2 Reward model judge}

    Requests a strict finite scalar in [0,1] under the existing rubric
    through selected inference.
    This preserves grading intent and range, not statistical equivalence to a
    dedicated provider grader. Expected failures use the outer drop-judge policy;
    missing configuration and cancellation propagate. *)

module Reward_model_judge : Judge = Generate_Reward_model_judge (struct
    let name = "reward_model"

    let system_prompt =
      {|
Evaluate a prompt outputted by a model for how effectively and thoroughly it guides a language model to complete a user-defined task given its system prompt.

Assess the prompt according to these criteria:
- Clarity: Is the prompt clear, unambiguous, and easy to understand?
- Task Coverage: Does the prompt fully capture all necessary goals, requirements, and constraints of the user-defined task?
- Output Specification: Does the prompt precisely define the required output format (including syntax, length, format details)?
- Example Quality: Are examples (if included) thorough, relevant, and use placeholders when helpful? Are reasoning and conclusion steps ordered correctly per guidelines?
- Completeness: Does the prompt include all important constants/guidelines supplied by the user?
- Alignment: Does it preserve and integrate any specific user instructions or constraints?
- Context Awareness: Does the prompt consider guidelines and context provided by the user?

After thoroughly analyzing all aspects, provide:
- An overall numerical score on a scale from 0 to 1 (where 0 means "really bad," and 1 means "as good as it gets" regarding prompt quality and effectiveness).

# Notes

- Always reason through each criterion before determining your score
- If no prompt is provided by the model then score how well the current prompt captures the user-defined task and matched the models system prompt.
  |}
    ;;
  end)

module Tool_description_reward_model_judge : Judge = Generate_Reward_model_judge (struct
    let name = "tool_description_reward_model"

    let system_prompt =
      {|
      Evaluate a tool description outputted by a model for how effectively and thoroughly it guides a language model to complete a user-defined task given its system prompt.
      Assess the tool description according to these criteria:
- Clarity: Is the tool description clear, unambiguous, and easy to understand?
- Task Coverage: Does the tool description fully capture all necessary goals, requirements, and constraints of the user-defined task?
- Output Specification: Does the tool description precisely define the required output format (including syntax, length, format details)?
- Example Quality: Are examples (if included) thorough, relevant, and use placeholders when helpful? Are reasoning and conclusion steps ordered correctly per guidelines?
- Completeness: Does the tool description include all important constants/guidelines supplied by the user?
- Alignment: Does it preserve and integrate any specific user instructions or constraints?
- Context Awareness: Does the tool description consider guidelines and context provided by the user?

After thoroughly analyzing all aspects, provide:
- An overall numerical score on a scale from 0 to 1 (where 0 means "really bad," and 1 means "as good as it gets" regarding tool description quality and effectiveness).
- Always reason through each criterion before determining your score
- If no tool description is provided by the model then score how well the current tool description captures the user-defined task and matched the models system prompt.
      |}
    ;;
  end)

(*--------------------------------------------------------------------
 *  Reward model judge                                                *
 *--------------------------------------------------------------------*)

let prompt_reward_model_judge : (module Judge) = (module Reward_model_judge)

(* Public first-class wrapper for the specialised tool-description rubric. *)
let tool_description_reward_model_judge : (module Judge) =
  (module Tool_description_reward_model_judge)
;;

(*--------------------------------------------------------------------
 *  Self-consistency / ensemble strategy                              *
 *--------------------------------------------------------------------*)

(** Strategy used to aggregate the [k] individual scores produced by a
    judge when running in self-consistency mode. *)
type sc_strategy =
  | Mean (** Arithmetic mean of the [k] scores *)
  | Majority
  (** Majority vote interpreting a score > 0.5 as “success”.
                Ties resolve to [0.0]. *)

let string_of_sc_strategy = function
  | Mean -> "mean"
  | Majority -> "majority"
;;

(** {2 Self-consistency wrapper}

    [Self_consistency_judge] is a functor that wraps an existing
    {!module:Judge} and evaluates it [k] times on the same candidate
    string.  The resulting list of scores is then aggregated using
    the user-selected {!type:sc_strategy}.  This implements the
    so-called *self-consistency* evaluator described in the research
    literature, where multiple stochastic executions are combined to
    obtain a more robust signal. *)

module Self_consistency_judge
    (Config : sig
       val k : int
       val strategy : sc_strategy
     end)
    (Base : Judge) : Judge = struct
  let name =
    Printf.sprintf
      "self_consistency[%s,k=%d](%s)"
      (string_of_sc_strategy Config.strategy)
      Config.k
      Base.name
  ;;

  let evaluate ?env ?inference candidate =
    (* Run the base judge [k] times.  We do not attempt any explicit
       random-seed handling here – stochasticity must come from the
       underlying judge (e.g. an LLM call with temperature > 0). *)
    let k = Int.max 1 Config.k in
    let run () =
      try
        let s = Base.evaluate ?env ?inference candidate in
        Log.emit
          `Debug
          (Printf.sprintf
             "Self_consistency_judge.evaluate: %f\n%s"
             s
             (String.prefix candidate (Int.min 200 (String.length candidate))));
        let positive = if Float.(s > 0.5) then 1 else 0 in
        Some (s, positive)
      with
      | (Eio.Cancel.Cancelled _ | Configuration_required) as exn -> raise exn
      | Expected_model_failure -> None
      | exn ->
        if Option.is_some inference then raise exn;
        Log.emit
          `Debug
          (Printf.sprintf
             "Self_consistency_judge.evaluate: %s\n%s"
             (Core.Exn.to_string exn)
             (String.prefix candidate (Int.min 200 (String.length candidate))));
        (* Log the error but do not crash the caller. *)
        (* This is useful in unit tests that expect some judges to fail. *)
        None
    in
    (* Execute the [k] runs sequentially when no Eio environment is
       available to avoid the [Effect.Unhandled] runtime error in
       offline unit tests.  We fall back to [Eio.Fiber] only when the
       caller provided a live [env]. *)
    let run_indices = List.init (Int.max 1 Config.k) ~f:Fn.id in
    let results : (float * int) list =
      match env with
      | None -> List.filter_map run_indices ~f:(fun _ -> run ())
      | Some _ -> Eio.Fiber.List.filter_map (fun _ -> run ()) run_indices
    in
    let sum =
      List.fold results ~init:0.0 ~f:(fun acc (s, positive) ->
        match Config.strategy with
        | Mean -> acc +. s
        | Majority -> acc +. Float.of_int positive)
    in
    match Config.strategy with
    | Mean -> sum /. Float.of_int k
    | Majority -> if Float.(sum > 0.5) then 1.0 else 0.0
  ;;
end

(** Convenience wrapper to create a self-consistency judge without
    resorting to first-class modules on the caller side. *)
let wrap_self_consistency_judge ~(k : int) ~(strategy : sc_strategy) (module Base : Judge)
  : (module Judge)
  =
  (module Self_consistency_judge
            (struct
              let k = k
              let strategy = strategy
            end)
            (Base) : Judge)
;;

(*********************************************************************
 *  Evaluator container                                              *
 ********************************************************************)

(** [Evaluator.t] is a container for multiple judges and an optional
    aggregation strategy.  It caches results to avoid redundant
    evaluations.  The [aggregate] function combines the raw scores
    from all judges into a single score, which is then cached per
    candidate string. *)

type t =
  { judges : judge list
  ; aggregate : Aggregator.t
  ; cache : (string, score) Hashtbl.t (* candidate → aggregated score *)
  }

let create
      ?(judges = [ Judge (module Mock_judge : Judge) ])
      ?(aggregate = Aggregator.mean)
      ()
  : t
  =
  { judges; aggregate; cache = Hashtbl.create (module String) }
;;

let default = create ()

(* Retain the historic [aggregate] helper for backwards-compatibility; it
   simply delegates to [Aggregator.mean].  New code should use
   [Evaluator.create ~aggregate] instead. *)
let aggregate (scores : score list) : score = Aggregator.mean scores

let evaluate ?env ?inference (t : t) ?best (candidate : string) : score =
  match if Option.is_some inference then None else Hashtbl.find t.cache candidate with
  | Some s -> s
  | None ->
    (match env with
     | None ->
       (* -----------------------------------------------------------------
           Sequential fallback when no Eio environment is provided.          *)
       let raw_scores =
         t.judges
         |> List.filter_map ~f:(fun j ->
           with_exception_guard_sequential j ?env ?inference ?best candidate)
       in
       let agg = t.aggregate raw_scores in
       if Option.is_none inference then Hashtbl.set t.cache ~key:candidate ~data:agg;
       agg
     | Some env ->
       Log.emit `Debug (sprintf "Evaluator.evaluate: %s" candidate);
       (* -----------------------------------------------------------------
           Parallel execution using Eio fibers under the provided environment. *)
       (* -----------------------------------------------------------------
           Parallel execution using fibers under the provided environment.   *)
       let clock = Eio.Stdenv.clock env in
       let timeout_s = judge_timeout_seconds () in
       let max_fibers = pool_size () in
       let raw_scores =
         Eio.Fiber.List.filter_map
           ~max_fibers
           (fun judge ->
              with_exception_guard ~clock ~timeout_s judge ~env ?inference ?best candidate)
           t.judges
       in
       let agg = t.aggregate raw_scores in
       if Option.is_none inference then Hashtbl.set t.cache ~key:candidate ~data:agg;
       agg)
;;

(*********************************************************************
 *  Convenience API                                                  *
 ********************************************************************)

let evaluate_default candidate = evaluate default candidate
