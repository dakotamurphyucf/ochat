(** Command-line tool that refines prompts using *recursive meta-prompting*.

    {1 Overview}

    [`mp-refine-run`] is a very thin wrapper around {!module:Mp_flow}.  It
    exposes the two high-level helpers {!Mp_flow.first_flow} (general prompts)
    and {!Mp_flow.tool_flow} (tool descriptions) on the command line.  The
    executable itself merely

    • loads the user-supplied Markdown files from disk via {!Io.load_doc};
    • parses a handful of flags with {!Core.Command};
    • converts the textual flag values to the corresponding {!module:Context}
      variants; and
    • delegates to the appropriate [*_flow] function inside
      {!Io.run_main} so that all IO (OpenAI calls, vector-DB look-ups, …)
      runs inside an {!Eio_main.run} event loop.

    The heavy-lifting – proposer model, reward-model evaluators, Thompson
    bandit, convergence checks, … – is handled internally by {!Mp_flow}.  On
    success the refined prompt is either printed to *stdout* or appended to a
    user-specified file.

    {1 Usage}

{[ mp-refine-run
     -task-file   TASK.md            (* mandatory *)
     -input-file  DRAFT.md           (* optional *)
     -output-file RESULT.md          (* optional *)
     -action      generate|update    (* default: generate *)
     -prompt-type general|tool       (* default: general *) ]}

    All flags are optional except [\-task-file].  When [\-output-file] is
    omitted the result is written to *stdout*.

    {2 Flags}

    • [`-task-file FILE`] – Markdown file containing the natural-language task
      description.  The contents are wrapped in a [`<user-task>`] XML-like tag
      before being sent to the LLM so that the specification is always
      visible to transformation agents.

    • [`-input-file FILE`] – Initial draft prompt to refine.  If missing a new
      prompt is generated from scratch.

    • [`-output-file FILE`] – Destination path.  The refined prompt is
      *appended* to the file, mimicking the historical in-place behaviour of
      the old positional interface.  When absent the prompt is printed to
      *stdout*.

    • [`-action ACTION`] – Either {b generate} (create a brand-new prompt) or
      {b update} (mutate an existing one).  Maps to
      {!Context.Generate}/{!Context.Update}.  Defaults to {b generate}.

    • [`-prompt-type TYPE`] – {b general} for assistant prompts or {b tool}
      for tool descriptions, which swaps in a tool-specific evaluator rubric
      and templates.  Defaults to {b general}.

    {2 Exit codes}

    • 0 – success
    • 1 – invalid flag value (unknown [action] or [prompt-type])

    {1 Examples}

    Generate a fresh assistant prompt and print it to *stdout*:

{[
  mp-refine-run -task-file summarise.md
]}

    Refine an existing tool description and append the result:

{[
  mp-refine-run \
    -task-file translator_task.md \
    -input-file  draft_tool.md \
    -output-file refined_tool.md \
    -action      update \
    -prompt-type tool
]}

    {1 Limitations}

    * Requires a valid [OPENAI_API_KEY] in the environment so that the
      underlying OpenAI API calls succeed.
    * The reward-model evaluator is called synchronously and may take several
      seconds per iteration, especially for large prompts.
*)

open Core
open Command.Let_syntax

let command : Command.t =
  Command.basic
    ~summary:"Run a meta-prompt refinement flow"
    (let%map_open task_file =
       flag
         "-task-file"
         (optional string)
         ~doc:"FILE Path to the meta-prompt task file (required)"
     and input_file =
       flag "-input-file" (optional string) ~doc:"FILE Initial prompt file (optional)"
     and output_file_opt =
       flag
         "-output-file"
         (optional string)
         ~doc:"FILE Destination file; result is appended if provided (optional)"
     and action =
       flag
         "-action"
         (optional_with_default "generate" string)
         ~doc:"ACTION Action to perform (default: generate)"
     and prompt_type =
       flag
         "-prompt-type"
         (optional_with_default "general" string)
         ~doc:"TYPE Type of prompt (default: general)"
     and meta_factory =
       flag
         "-meta-factory"
         (optional_with_default false bool)
         ~doc:
           "BOOL When true, generate/iterate using the meta-prompt factory \
            (non-destructive)"
     and meta_factory_online =
       flag
         "-meta-factory-online"
         (optional_with_default true bool)
         ~doc:
           "BOOL When true, use the online meta-prompt factory strategy inside the \
            recursive refinement loop (LLM-backed). Precedence: -meta-factory (offline) \
            > -meta-factory-online > default RMP"
     and classic_rmp =
       flag
         "-classic-rmp"
         (optional_with_default false bool)
         ~doc:
           "BOOL When true, force the classic recursive meta-prompting strategy \
            (disables meta-factory-online)."
     in
     fun () ->
       Log.emit `Info "mp_refine_run: starting";
       Io.run_main
       @@ fun env ->
       let fs = Eio.Stdenv.fs env in
       let task_contents =
         Option.map ~f:(Io.load_doc ~dir:fs) task_file |> Option.value ~default:""
       in
       let open Meta_prompting in
       let action =
         match String.lowercase action with
         | "generate" -> Context.Generate
         | "update" -> Context.Update
         | _ ->
           Log.emit `Error (Printf.sprintf "Unknown action: %s" action);
           exit 1
       in
       let prompt =
         match input_file with
         | Some file -> Io.load_doc ~dir:fs file
         | None -> ""
       in
       let prompt_type =
         match String.lowercase prompt_type with
         | "general" -> Context.General
         | "tool" -> Context.Tool
         | _ ->
           Log.emit `Error (Printf.sprintf "Unknown prompt type: %s" prompt_type);
           exit 1
       in
       let online_enabled = (not classic_rmp) && meta_factory_online in
       let result =
         if meta_factory
         then (
           match input_file with
           | None ->
             let p : Prompt_factory.create_params =
               { agent_name = "Meta-Prompt Agent"
               ; goal = task_contents
               ; success_criteria = [ "Adheres to safety and output constraints" ]
               ; audience = Some "technical"
               ; tone = Some "neutral"
               ; domain =
                   (match prompt_type with
                    | Context.General -> Some "general"
                    | Context.Tool -> Some "coding")
               ; use_responses_api = true
               ; markdown_allowed = true
               ; eagerness = Prompt_factory.Medium
               ; reasoning_effort = `Medium
               ; verbosity_target = `Low
               }
             in
             Prompt_factory.create_pack p ~prompt
           | Some _ ->
             let ip : Prompt_factory.iterate_params =
               { goal = task_contents
               ; desired_behaviors = []
               ; undesired_behaviors = []
               ; safety_boundaries = []
               ; stop_conditions = []
               ; reasoning_effort = `Low
               ; verbosity_target = `Low
               ; use_responses_api = true
               }
             in
             Prompt_factory.iterate_pack ip ~current_prompt:prompt)
         else (
           match input_file with
           | None when online_enabled ->
             (match
                Prompt_factory_online.create_pack_online
                  ~env
                  ~agent_name:"Meta-Prompt Agent"
                  ~goal:task_contents
                  ~proposer_model:(Some Openai.Responses.Request.O3)
              with
              | Some txt -> txt
              | None ->
                Mp_flow.first_flow
                  ~env
                  ~task:task_contents
                  ~prompt
                  ~action
                  ~use_meta_factory_online:false
                  ())
           | _ ->
             (match prompt_type with
              | Context.General ->
                Mp_flow.first_flow
                  ~env
                  ~task:task_contents
                  ~prompt
                  ~action
                  ~use_meta_factory_online:online_enabled
                  ()
              | Context.Tool ->
                Mp_flow.tool_flow
                  ~env
                  ~task:task_contents
                  ~prompt
                  ~action
                  ~use_meta_factory_online:online_enabled
                  ()))
       in
       (match output_file_opt with
        | Some path ->
          Log.emit `Info (Printf.sprintf "mp_refine_run: writing output to %s" path);
          Io.append_doc ~dir:fs path result
        | None -> print_endline result);
       Log.emit `Info "mp_refine_run: finished")
;;

let () = Command_unix.run command
