open Core

[@@@warning "-16-27-32-39"]

(** Recursive Meta-Prompting: a lightweight monadic wrapper that
    repeatedly refines an existing prompt with the help of an
    {!module:Evaluator}.  The current implementation demonstrates the
    control-flow and monotonicity guard without performing any fancy
    prompt transformation – it merely appends a metadata field to
    indicate the iteration number.  Replacing the transformation logic
    with an LLM-based agent can be done without changing the outward
    API.  *)

module P = Prompt_intf
module E = Evaluator
module Vdb = Vector_db

(*********************************************************************
 *  Transformation strategies                                       *
 ********************************************************************)

(* A transformation strategy takes a prompt and produces a refined
   prompt.  Including [~iteration] allows strategies to inject the
   iteration counter if desired. *)

type transform_strategy =
  { name : string
  ; apply : P.t -> ?env:Eio_unix.Stdenv.base -> iteration:int -> context:Context.t -> P.t
  }

(* Forward declaration of [transform_prompt] – defined later after the
   OpenAI helper functions.  We use a normal [let rec] to enable the
   earlier definition of the default strategy without resorting to
   mutable references. *)

(* Transformation logic – defined later after the OpenAI helper
   [ask_llm] to avoid forward-reference issues.  *)

(*********************************************************************
 *  Monad skeleton                                                    *
 ********************************************************************)

type 'a t =
  | Return of 'a
  | Bind of 'a t * ('a -> 'a t)

let return x = Return x

let rec bind m f =
  match m with
  | Return x -> f x
  | Bind (m', g) -> Bind (m', fun x -> bind (g x) f)
;;

let rec join (m : 'a t t) : 'a t =
  match m with
  | Return x -> x
  | Bind (m', f) -> join (bind m' f)
;;

(*********************************************************************
 *  Refinement loop                                                  *
 ********************************************************************)

type refine_params =
  { evaluator : E.t
  ; max_iters : int
  ; score_epsilon : float (** Stop when improvement over window ≤ [score_epsilon] *)
  ; plateau_window : int
    (** Deprecated: kept for backward-compatibility; no longer used now that Bayesian convergence is implemented. *)
  ; bayes_alpha : float
    (** Credible interval significance level. The refinement loop stops when the
        posterior probability that the improvement Δ exceeds [score_epsilon] drops
        below [1. -. bayes_alpha] (e.g. 0.05 for a 95 % credible bound). *)
  ; bandit_enabled : bool
  ; strategies : transform_strategy list
  ; proposer_model : Openai.Responses.Request.model option
  ; executor_model : Openai.Responses.Request.model option
  }

(* [default_params] and [make_params] will be redefined further below,
   once the default strategies have been declared. *)

(*------------------------------------------------------------------
   Prompt transformation powered by an LLM
  ------------------------------------------------------------------*)

(* Environment variable toggles ------------------------------------------------*)

(* Fallback-compatible environment variable lookup. *)
let getenv_opt var = Core.Sys.getenv var
let api_key_opt = lazy (getenv_opt "OPENAI_API_KEY")

(* Optional soft token-budget (maximum output tokens) – set via
   [META_PROMPT_BUDGET] environment variable.  When the variable is
   absent or malformed we leave the parameter to the OpenAI default. *)
let budget_opt =
  lazy
    (match getenv_opt "META_PROMPT_BUDGET" with
     | None -> None
     | Some s ->
       (try Some (Int.of_string s) with
        | _ -> None))
;;

(*------------------------------------------------------------------*)
(*                        OpenAI API helpers                         *)
(*------------------------------------------------------------------*)
let remove_reasoning s =
  let len = String.length s in
  let open Int in
  (* constant tag strings *)
  let open_tag = "<reasoning>" in
  let close_tag = "</reasoning>" in
  let open_len = String.length open_tag in
  let close_len = String.length close_tag in
  (* buffer for the result *)
  let buf = Buffer.create len in
  (* tail-recursive scan *)
  let rec loop pos =
    match String.substr_index s ~pattern:open_tag ~pos with
    | None ->
      (* no more <reasoning>: copy the tail and finish *)
      Buffer.add_substring buf s ~pos ~len:(len - pos);
      Buffer.contents buf
    | Some open_at ->
      (* copy bytes before the tag *)
      Buffer.add_substring buf s ~pos ~len:(open_at - pos);
      let after_open = open_at + open_len in
      (* look for matching </reasoning> *)
      let resume_at =
        match String.substr_index s ~pattern:close_tag ~pos:after_open with
        | None ->
          (* unbalanced tag: drop the rest of the string *)
          len
        | Some close_at -> close_at + close_len
      in
      loop resume_at
  in
  loop 0
;;

(* [ask_llm prompt] sends [prompt] to the completions endpoint and
   returns the assistant reply, or [None] if the call failed for any
   reason (missing API key, network error, unexpected JSON …).  The
   helper runs its IO inside a fresh [Eio_main.run] to avoid threading
   capabilities through the entire call-chain – acceptable here given
   the short synchronous workload and the low iteration counts used by
   recursive meta-prompting. *)
let ask_llm ~env ~user_content ~context ?model ?guidelines : string option =
  match Lazy.force api_key_opt with
  | None -> None
  | Some _key ->
    (try
       let dir = Eio.Stdenv.fs env in
       let net = Eio.Stdenv.net env in
       let open Openai.Responses in
       let open Input_message in
       (* Construct the system prompt asking the model to improve the
          user-supplied prompt.  Optionally append additional guidelines
          supplied via the [?guidelines] parameter when their injection
          is enabled through the [META_PROMPT_GUIDELINES] environment
          variable (defaults to enabled). *)
       let guidelines_enabled =
         match getenv_opt "META_PROMPT_GUIDELINES" with
         | None -> true
         | Some v ->
           (match String.lowercase (String.strip v) with
            | "0" | "false" | "off" -> false
            | _ -> true)
       in
       let is_edit =
         match context.Context.action with
         | Update -> true
         | Generate -> false
       in
       let system_prompt =
         let open Prompts in
         match context.model_to_optimize, context.prompt_type, is_edit with
         | Some O3, General, true -> openai_system_edit_instructions_prompt_o3
         | Some O3, General, false -> openai_system_instructions_prompt_o3
         | _, General, true -> openai_system_edit_instructions_prompt
         | _, General, false -> openai_system_instructions_prompt
         | _, Tool, true -> openai_tool_description_prompt
         | _, Tool, false -> openai_tool_description_prompt
       in
       let system_text =
         match guidelines, guidelines_enabled with
         | Some g, true when not (String.is_empty (String.strip g)) ->
           system_prompt
           ^ Printf.sprintf "\n\n<additional-guidelines>\n%s\n</additional-guidelines>" g
         | _ -> system_prompt
       in
       let system_msg : Input_message.t =
         { role = Developer
         ; content = [ Text { text = system_text; _type = "input_text" } ]
         ; _type = "message"
         }
       in
       (* ----------------------------------------------------------- *)
       (* Optional Vector-DB context retrieval                       *)
       (* ----------------------------------------------------------- *)
       let context_k =
         match getenv_opt "META_PROMPT_CTX_K" with
         | Some s ->
           (try Int.of_string s with
            | _ -> 0)
         | None -> 0
       in
       let vector_ctx_opt =
         if context_k <= 0
         then None
         else (
           let vector_db_folder =
             Option.value (getenv_opt "VECTOR_DB_FOLDER") ~default:".md_index"
           in
           let vec_file = vector_db_folder ^ "/vectors.ml.binio" in
           let vec_path = Eio.Path.(dir / vec_file) in
           let vecs =
             try Vdb.Vec.read_vectors_from_disk vec_path with
             | _ -> [||]
           in
           if Array.length vecs = 0
           then None
           else (
             let corpus = Vdb.create_corpus vecs in
             let bm25_file = Eio.Path.(dir / (vector_db_folder ^ "/bm25.ml.binio")) in
             let bm25 =
               try Bm25.read_from_disk bm25_file with
               | _ -> Bm25.create []
             in
             (* obtain embedding for the user prompt text *)
             let embed_resp =
               try
                 Some
                   (Openai.Embeddings.post_openai_embeddings
                      net
                      ~input:[ user_content.P.body ])
               with
               | _ -> None
             in
             match embed_resp with
             | None -> None
             | Some resp ->
               (match resp.data with
                | [] -> None
                | first :: _ ->
                  let arr = Array.of_list first.embedding in
                  let emb =
                    Owl.Mat.of_array arr (Array.length arr) 1 |> Owl.Mat.transpose
                  in
                  let indices =
                    Vdb.query_hybrid
                      corpus
                      ~bm25
                      ~beta:0.4
                      ~embedding:emb
                      ~text:user_content.P.body
                      ~k:context_k
                  in
                  let docs =
                    Vdb.get_docs Eio.Path.(dir / vector_db_folder) corpus indices
                  in
                  (match docs with
                   | [] -> None
                   | _ ->
                     Some
                       (List.mapi docs ~f:(fun i d ->
                          Printf.sprintf "### Context %d\n%s" (i + 1) d)
                        |> String.concat ~sep:"\n\n---\n")))))
       in
       let tag =
         match context.prompt_type with
         | Context.General -> "current-prompt"
         | Context.Tool -> "current-tool-description"
       in
       let user_msg : Input_message.t =
         { role = User
         ; content =
             [ Text
                 { text =
                     sprintf
                       "%s\n<%s>\n%s\n</%s>"
                       (Option.value user_content.header ~default:"")
                       tag
                       user_content.body
                       tag
                 ; _type = "input_text"
                 }
             ]
         ; _type = "message"
         }
       in
       let inputs : Item.t list =
         match vector_ctx_opt with
         | None -> [ Item.Input_message system_msg; Item.Input_message user_msg ]
         | Some ctx ->
           let ctx_msg : Input_message.t =
             { role = User
             ; content =
                 [ Text
                     { text = "Relevant context snippets:\n" ^ ctx; _type = "input_text" }
                 ]
             ; _type = "message"
             }
           in
           [ Item.Input_message system_msg
           ; Item.Input_message ctx_msg
           ; Item.Input_message user_msg
           ]
       in
       let max_output_tokens = 1000000 in
       let chosen_model = Option.value model ~default:Request.O3 in
       let ({ Response.output; _ } : Response.t) =
         post_response
           Default
           ~reasoning:{ effort = Some High; summary = Some Detailed }
           ~max_output_tokens
           ~model:chosen_model
           ~dir
           net
           ~inputs
       in
       Log.emit `Debug (Sexp.to_string_hum [%sexp (output : Item.t list)]);
       (* Extract the assistant text from the first [Output_message]
                 item.  We ignore any additional items (reasoning blocks,
                 annotations …) – the transformation agent is expected to
                 return a single improved prompt. *)
       let rec first_text = function
         | [] -> None
         | Item.Output_message om :: _ ->
           (match om.Output_message.content with
            | [] -> None
            | { text; _ } :: _ -> Some text)
         | _ :: tl -> first_text tl
       in
       let res = first_text output in
       match is_edit, res with
       | true, Some res ->
         (* remove raw reasoning xml tags and content <reasoning> ....</reasoning> from the output *)
         let clean_res = remove_reasoning res in
         Some clean_res
       | false, _ -> res
       | _, None ->
         Log.emit `Debug "ask_llm: no response from LLM";
         None
     with
     | exn ->
       Log.emit `Debug (Printf.sprintf "ask_llm: %s" (Core.Exn.to_string exn));
       None)
;;

(*********************************************************************
 *  LLM-powered prompt transformation                                *
 ********************************************************************)

(* We can now safely define the LLM helper, transformation function
   and default strategies, as the OpenAI plumbing has already been
   declared above. *)

let rec transform_prompt ?env ~(context : Context.t) (p : P.t) ~(iteration : int) : P.t =
  (* Choose proposer model – explicit context takes precedence over
     environment variable fallbacks. *)
  let proposer_model_opt : Openai.Responses.Request.model option =
    match context.proposer_model with
    | Some _ as m -> m
    | None ->
      (match Core.Sys.getenv "META_PROPOSER_MODEL" with
       | None -> None
       | Some s ->
         (try Some (Openai.Responses.Request.model_of_str_exn s) with
          | _ -> None))
  in
  match env with
  | Some e ->
    (match
       ask_llm
         ?model:proposer_model_opt
         ?guidelines:context.guidelines
         ~context
         ~env:e
         ~user_content:p
     with
     | Some improved when not (String.is_empty (String.strip improved)) ->
       let p' = { p with body = improved } in
       (* P.add_metadata p' ~key:"iteration" ~value:(Int.to_string iteration) *)
       p'
     | _ -> P.add_metadata p ~key:"iteration" ~value:(Int.to_string iteration))
  | None -> P.add_metadata p ~key:"iteration" ~value:(Int.to_string iteration)
;;

let default_llm_strategy : transform_strategy =
  { name = "llm"
  ; apply =
      (fun p ?env ~iteration ~context -> transform_prompt ?env ~context p ~iteration)
  }
;;

let heuristic_strategy : transform_strategy =
  { name = "heuristic"
  ; apply =
      (fun p ?env:_ ~iteration ~context:_ ->
        P.add_metadata p ~key:"heuristic_iter" ~value:(Int.to_string iteration))
  }
;;

(*------------------------------------------------------------------
  Default parameter helpers                                           *)

let default_params () : refine_params =
  { evaluator = E.default
  ; max_iters = 3
  ; score_epsilon = 1e-6
  ; plateau_window = 0
  ; bayes_alpha = 0.05
  ; bandit_enabled = false
  ; strategies = [ default_llm_strategy ]
  ; proposer_model = None
  ; executor_model = None
  }
;;

let make_params
      ?judges
      ?(max_iters = 3)
      ?(score_epsilon = 1e-6)
      ?(plateau_window = 0)
      ?(bayes_alpha = 0.05)
      ?(bandit_enabled = false)
      ?strategies
      ?proposer_model
      ?executor_model
      ()
  : refine_params
  =
  let evaluator =
    match judges with
    | None -> E.default
    | Some js -> E.create ~judges:js ()
  in
  let strategies =
    match strategies with
    | Some s when not (List.is_empty s) -> s
    | _ -> [ default_llm_strategy ]
  in
  { evaluator
  ; max_iters
  ; score_epsilon
  ; plateau_window
  ; bayes_alpha
  ; bandit_enabled
  ; strategies
  ; proposer_model
  ; executor_model
  }
;;

let refine
      ?context
      ?params
      ?judges
      ?max_iters
      ?score_epsilon
      ?plateau_window
      ?bayes_alpha
      ?bandit_enabled
      ?strategies
      ?proposer_model
      ?executor_model
      (prompt : P.t)
  : P.t
  =
  (* ------------------------------------------------------------------ *)
  (* Resolve parameters                                                  *)
  (* ------------------------------------------------------------------ *)
  let params =
    match params with
    | Some p -> p
    | None ->
      make_params
        ?judges
        ?max_iters
        ?score_epsilon
        ?plateau_window
        ?bayes_alpha
        ?bandit_enabled
        ?strategies
        ?proposer_model
        ?executor_model
        ()
  in
  (* ------------------------------------------------------------------ *)
  (* Resolve effective context                                           *)
  (* ------------------------------------------------------------------ *)
  let context =
    let base_ctx = Option.value context ~default:(Context.default ()) in
    match params.proposer_model with
    | None -> base_ctx
    | Some m -> Context.with_proposer_model base_ctx ~model:(Some m)
  in
  let evaluate ?best p =
    let footnotes =
      match context.guidelines with
      | Some g when not (String.is_empty (String.strip g)) ->
        [ Printf.sprintf "<guidelines>\n%s\n</guidelines>" g ]
      | _ -> []
    in
    let task =
      let open Prompts in
      match context.model_to_optimize, context.action, context.prompt_type with
      | Some O3, Update, General -> openai_system_edit_instructions_prompt_o3
      | Some O3, Generate, General -> openai_system_instructions_prompt_o3
      | _, Update, General -> openai_system_edit_instructions_prompt
      | _, Generate, General -> openai_system_instructions_prompt
      | _, Update, Tool -> openai_tool_description_prompt
      | _, Generate, Tool -> openai_tool_description_prompt
    in
    let tag =
      match context.prompt_type with
      | Context.General -> "output-prompt"
      | Context.Tool -> "output-tool-description"
    in
    let p =
      P.
        { p with
          footnotes
        ; body =
            sprintf
              "<model-system-prompt>%s</model-system-prompt>\n\n<%s>%s</%s>"
              task
              tag
              p.body
              tag
        }
    in
    E.evaluate ?env:context.env params.evaluator ?best (P.to_string p)
  in
  (* ------------------------------------------------------------------ *)
  (* Bandit state                                                       *)
  (* ------------------------------------------------------------------ *)
  let n_strategies = List.length params.strategies in
  let successes = Array.create ~len:n_strategies 0 in
  let failures = Array.create ~len:n_strategies 0 in
  let sample_thompson idx =
    let a = Float.of_int successes.(idx) +. 1.0 in
    let b = Float.of_int failures.(idx) +. 1.0 in
    Owl_stats.beta_rvs ~a ~b
  in
  let choose_strategy () : int * transform_strategy =
    if (not params.bandit_enabled) || n_strategies = 1
    then 0, List.hd_exn params.strategies
    else (
      (* Thompson sampling: draw one beta sample per arm and pick the best. *)
      let best_idx =
        let max_sample = ref (-1.0) in
        let best_idx = ref 0 in
        for i = 0 to n_strategies - 1 do
          let s = sample_thompson i in
          if Float.(s > !max_sample)
          then (
            max_sample := s;
            best_idx := i)
        done;
        !best_idx
      in
      best_idx, List.nth_exn params.strategies best_idx)
  in
  (* ------------------------------------------------------------------ *)
  (* Recursive refinement loop with Bayesian convergence & bandit update *)
  (* ------------------------------------------------------------------ *)
  let rec loop current iter best_score best_prompt success_count failure_count =
    (* Termination by explicit iteration bound *)
    if iter >= params.max_iters
    then best_prompt
    else (
      (* Select a transformation strategy via the bandit (if enabled)   *)
      let strat_idx, strat = choose_strategy () in
      (* Produce refined candidate and score it                         *)
      let candidate = strat.apply current ?env:context.env ~iteration:iter ~context in
      let candidate =
        match current.P.header with
        | None -> { candidate with P.header = Some current.body }
        | Some _ -> candidate
      in
      let score = evaluate ~best:best_prompt.P.body candidate in
      Log.emit
        `Debug
        (Printf.sprintf
           "[%d] %s: score = %f, best = %f, strategy = %s"
           iter
           (P.to_string candidate)
           score
           best_score
           strat.name);
      (* Update bandit state and convergence statistics *)
      let improvement = score -. best_score in
      let improved = Float.(score > best_score) in
      Log.emit
        `Debug
        (Printf.sprintf
           "[%d] %s: improvement = %f, improved = %b"
           iter
           (P.to_string candidate)
           improvement
           improved);
      (* Update best prompt and score if the candidate is better *)
      (* Bandit feedback update *)
      if params.bandit_enabled && n_strategies > 1
      then
        if improved
        then successes.(strat_idx) <- successes.(strat_idx) + 1
        else failures.(strat_idx) <- failures.(strat_idx) + 1;
      (* Update best prompt/score if new candidate is better *)
      let best_score, best_prompt, current =
        if improved then score, candidate, current else best_score, best_prompt, current
      in
      (* Bayesian update of convergence statistics *)
      let success_count, failure_count =
        if Float.(improvement > params.score_epsilon)
        then success_count + 1, failure_count
        else success_count, failure_count + 1
      in
      let a = Float.of_int (success_count + 1) in
      let b = Float.of_int (failure_count + 1) in
      (* Posterior mean of success probability p = E[p | data] *)
      let expected_success_prob = a /. (a +. b) in
      let bayes_plateau_detected = Float.(expected_success_prob <= params.bayes_alpha) in
      (* Termination conditions *)
      (* Float.(improvement <= params.score_epsilon) *)
      if bayes_plateau_detected
      then best_prompt
      else loop current (iter + 1) best_score best_prompt success_count failure_count)
  in
  let initial_score =
    match String.is_empty prompt.body with
    | true -> 0.0
    | false -> evaluate prompt
  in
  let result = loop prompt 1 initial_score prompt 0 0 in
  result
;;

(*********************************************************************
 *  Infix helper                                                      *
 ********************************************************************)

module Let_syntax = struct
  let return = return
  let bind = bind
end
