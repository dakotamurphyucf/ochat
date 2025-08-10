(** Recursive meta-prompting orchestration helpers.

    The module exposes two convenience functions that wrap the lower-level
    {!module:Recursive_mp} API with sensible defaults so that callers can
    refine an initial prompt in only a few lines of code:

    • {!val:first_flow} – refines a *general* prompt.
    • {!val:tool_flow} – refines a *tool description* prompt that adheres to
      the OpenAI JSON-tool-calling guidelines.

    Both helpers run several refinement iterations in which they
    1. ask a *proposer* LLM to transform the prompt,
    2. score the candidate with a *reward-model evaluator*, and
    3. keep the best-scoring version.

    The loop terminates once the score plateaus or the maximum number of
    iterations (currently 5) is reached.  All network IO is executed inside
    the caller-supplied {!Eio_unix.Stdenv.base} environment so that the
    library can be embedded into any existing Eio event loop.

    Neither function raises; transient errors (e.g. API rate limits) are
    handled internally by falling back to conservative defaults. *)

(** {1 API} *)

(** {1 API} *)

(** [first_flow ~env ~task ~prompt ?action ()] refines [prompt] for the user
    task [task] and returns the improved prompt text.

    Parameters:
    • [env] – capability record obtained from {!Eio_main.run}; required for
      talking to external services.
    • [task] – natural-language description of the task the refined prompt
      should solve.
    • [prompt] – initial draft prompt.
    • [?action] – whether the agent should [Generate] a new prompt or
      [Update] an existing one.  Defaults to {!Context.Generate}.

    Return value: the refined prompt **body** (header and footnotes are
    stripped) ready to be sent to a language model.

    Toggle:
    • [?use_meta_factory_online] – when [true], use the online meta-prompt
      factory iteration strategy to propose edits (LLM-backed). Defaults to
      [true].

    Example:
    {[ let refined =
         Mp_flow.first_flow
           ~env
           ~task:"Summarise the linked article"
           ~prompt:"Read the article and summarise the key points."
           ()
       in
       print_endline refined ]} *)
val first_flow
  :  env:Eio_unix.Stdenv.base
  -> task:string
  -> prompt:string
  -> ?action:Context.action
  -> ?use_meta_factory_online:bool
  -> unit
  -> string

(** Same as {!val:first_flow} but specialised for prompts that describe a
    JSON-serialised *tool*.  Internally the evaluator rubric switches to
    {!module:Evaluator.Tool_description_reward_model_judge} and the context
    field [prompt_type] is set to {!Context.Tool} so that the proposer and
    executor templates match OpenAI’s function-calling guidelines.

    Optional parameters mirror {!val:first_flow}:
    • [?action] – choose between prompt generation or in-place update.
    • [?use_meta_factory_online] – enable the online multi-candidate proposer
      strategy.

    Example:
    {[ let refined_tool =
         Mp_flow.tool_flow
           ~env
           ~task:"Expose a translate function"
           ~prompt:"Translate between English and French."
           ()
       in
       print_endline refined_tool ]} *)
val tool_flow
  :  env:Eio_unix.Stdenv.base
  -> task:string
  -> prompt:string
  -> ?action:Context.action
  -> ?use_meta_factory_online:bool
  -> unit
  -> string
