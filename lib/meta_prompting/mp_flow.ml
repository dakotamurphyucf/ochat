(** High-level orchestration helpers for *recursive meta-prompting*.

    The two entry-points {!val:first_flow} and {!val:tool_flow} take an initial
    draft prompt and iterate a refinement loop powered by
    {!module:Recursive_mp}.  Each iteration calls a *proposer* model (GPT-4o by
    default) to transform the prompt and then evaluates the candidate with an
    OpenAI reward-model based {!module:Evaluator}.  The best-scoring candidate
    is kept and the process stops once the score plateaus or the fixed maximum
    number of iterations is reached.

    {!val:first_flow} targets *general* prompts whereas {!val:tool_flow}
    specialises on *tool descriptions*.  Other than the evaluator rubric the
    control-flow and defaults are identical.

    Both helpers require an {!module:Eio_unix.Stdenv.base} capability record so
    that all IO (HTTP requests to the OpenAI API, vector-DB look-ups, …) is
    executed inside the caller-supplied event loop.

    Neither function raises; any network or evaluator error degrades
    gracefully by falling back to conservative defaults provided by the
    underlying modules.
*)

open Core
module RMP = Recursive_mp
module E = Evaluator

(** {1 API} *)

(** Refine a general prompt using recursive meta-prompting.

    [first_flow ~env ~task ~prompt ?action ()] constructs an initial
    {!module:Prompt_intf.t} from [task] and [prompt], spawns a
    {!module:Recursive_mp} loop with a reward-model evaluator and returns the
    improved [body] string.

    Parameter description:
    • [env] – Eio capability value obtained from [Eio_main.run].
    • [task] – Natural-language description of the user task. It is wrapped in
      a [`<user-task>`] XML-like tag so that the LLM transformation agent
      always receives the full specification.
    • [prompt] – Initial draft prompt to refine.
    • [?action] – Whether to [Context.Generate] a new prompt or
      [Context.Update] an existing one. Defaults to [Generate].

    Return value: the refined prompt text ready to be sent to the language
    model (header and footnotes are stripped).

    Example refining a prompt:
    {[
      let refined =
        Mp_flow.first_flow
          ~env
          ~task:"Summarise the linked article"
          ~prompt:"Read the article and summarise the key points."
          ()
      in
      print_endline refined
    ]}
*)

let first_flow
      ~(env : Eio_unix.Stdenv.base)
      ~(task : string)
      ~prompt
      ?(action = Context.Generate)
      ?(use_meta_factory_online = true)
      ()
  =
  Log.emit `Info "mp_flow: start first_flow";
  let prompt : Prompt_intf.t =
    Prompt_intf.make
      ~header:(Printf.sprintf "<user-task>\n%s\n</user-task>" task)
      ~body:prompt
      ()
  in
  Log.emit `Debug "mp_flow: judges configured";
  (* ----------------------------------------------------------------
     Judges                                                           *)
  Log.emit `Debug "mp_flow: configuring judges";
  let judges : E.judge list =
    let open E in
    [ Judge (wrap_self_consistency_judge ~k:6 ~strategy:Mean (module Reward_model_judge))
    ]
  in
  Log.emit `Debug "mp_flow: building refine parameters";
  let strategies =
    if use_meta_factory_online
    then [ RMP.meta_factory_online_strategy ]
    else [ RMP.default_llm_strategy ]
  in
  let params : RMP.refine_params =
    RMP.make_params
      ~judges
      ~bandit_enabled:true
      ~max_iters:5
      ~proposer_model:Openai.Responses.Request.Gpt5
      ~executor_model:Openai.Responses.Request.Gpt5
      ~strategies
      ~score_epsilon:1e-8
      ~bayes_alpha:0.05
      ()
  in
  Log.emit `Debug "mp_flow: refine parameters built";
  Log.emit `Debug "mp_flow: preparing context";
  let base_ctx = Context.default () in
  let ctx_with_env = { base_ctx with env = Some env; action } in
  let context : Context.t = Context.with_guidelines ctx_with_env ~guidelines:None in
  Log.emit `Debug "mp_flow: context prepared";
  (* ----------------------------------------------------------------
     Run refinement                                                   *)
  Log.emit `Info "mp_flow: starting refinement";
  let refined_prompt =
    Log.with_span "recursive_refine" (fun () ->
      RMP.refine ~context ~params prompt ~proposer_model:Openai.Responses.Request.Gpt5)
  in
  Log.emit `Info "mp_flow: refinement completed";
  refined_prompt.body
;;

(** Refine a *tool description* using recursive meta-prompting.

    The only differences to {!val:first_flow} are:

    – The evaluator judge is switched to
      {!module:Evaluator.Tool_description_reward_model_judge}, which is trained
      specifically on tool description quality.
    – The context field [prompt_type] is set to [Tool] so that the proposer
      and executor instruction templates match the OpenAI “function calling”
      guidelines.

    All parameters share the same meaning as in {!val:first_flow}.

    Example:
    {[
      let refined_tool =
        Mp_flow.tool_flow
          ~env
          ~task:"Expose a translate function"
          ~prompt:"Translate between English and French."
          ()
      in
      print_endline refined_tool
    ]}
*)

let tool_flow
      ~(env : Eio_unix.Stdenv.base)
      ~(task : string)
      ~prompt
      ?(action = Context.Generate)
      ?(use_meta_factory_online = true)
      ()
  =
  Log.emit `Info "mp_flow: start tool_flow";
  let prompt : Prompt_intf.t =
    Prompt_intf.make
      ~header:(Printf.sprintf "<user-task>\n%s\n</user-task>" task)
      ~body:prompt
      ()
  in
  Log.emit `Debug "mp_flow: judges configured";
  Log.emit `Debug "mp_flow: configuring judges";
  let judges : E.judge list =
    let open E in
    [ Judge
        (wrap_self_consistency_judge
           ~k:6
           ~strategy:Mean
           (module Tool_description_reward_model_judge))
    ]
  in
  Log.emit `Debug "mp_flow: building refine parameters";
  let strategies =
    if use_meta_factory_online
    then [ RMP.meta_factory_online_strategy ]
    else [ RMP.default_llm_strategy ]
  in
  let params : RMP.refine_params =
    RMP.make_params
      ~judges
      ~bandit_enabled:true
      ~max_iters:3
      ~proposer_model:Openai.Responses.Request.Gpt5
      ~executor_model:Openai.Responses.Request.Gpt5
      ~strategies
      ~score_epsilon:1e-8
      ~bayes_alpha:0.05
      ()
  in
  Log.emit `Debug "mp_flow: refine parameters built";
  Log.emit `Debug "mp_flow: preparing context";
  let base_ctx = Context.default () in
  let ctx_with_env = { base_ctx with env = Some env; action; prompt_type = Tool } in
  let context : Context.t = Context.with_guidelines ctx_with_env ~guidelines:None in
  Log.emit `Debug "mp_flow: context prepared";
  Log.emit `Info "mp_flow: starting refinement";
  let refined_prompt =
    Log.with_span "recursive_refine" (fun () ->
      RMP.refine ~context ~params prompt ~proposer_model:Openai.Responses.Request.Gpt5)
  in
  Log.emit `Info "mp_flow: refinement completed";
  refined_prompt.body
;;
