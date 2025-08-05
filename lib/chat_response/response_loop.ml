open Core
module Res = Openai.Responses

(** Automated completion loop.

    [Response_loop] keeps calling the OpenAI *chat/completions* endpoint
    until the conversation is *quiescent* – i.e. the assistant’s most
    recent reply no longer contains any {!Res.Item.Function_call}
    requests.

    At each iteration the algorithm:

    1. Sends the current [history] to the API.
    2. Appends all returned items to the conversation.
    3. Executes every [`Function_call`] with the implementation found in
       [tool_tbl], turning the textual result into a synthetic
       {!Res.Item.Function_call_output} item.
    4. Repeats from step 1 until no pending calls remain.

    The helper is **synchronous** and therefore primarily used by
    non-streaming code paths such as the CLI or unit-tests.  A streaming
    variant with real-time callbacks lives in {!Fork.run_stream}.

    {1 Example}

    Executing a simple loop that has access to a custom [grep] tool and
    to {!Fork.execute} for nested assistants:

    {[{
      let tool_tbl = String.Table.create () in
      Hashtbl.set tool_tbl ~key:"grep" ~data:grep_tool;
      Hashtbl.set tool_tbl ~key:"fork" ~data:Fork.execute;

      let final_history =
        Response_loop.run
          ~ctx
          ~model:Res.Request.Gpt4
          ~tool_tbl
          initial_history
      in
      (* [final_history] now contains assistant replies and tool outputs *)
    }]}
*)

(*********************************************************************
  Response_loop – keep going until no pending function calls
  ----------------------------------------------------------

  High-level algorithm:

  1. Send current history to the OpenAI chat/completions endpoint.
  2. Collect the new items (messages, function calls, reasoning).
  3. If *no* `Function_call` item has been returned, we are done – just
     append the items to the history and return.
  4. Otherwise execute every requested tool (using the supplied
     [tool_tbl]) and turn each result into a `Function_call_output`
     placeholder, then recurse.

  The helper is side-effect free except for the calls it makes to the
  tool functions.  Callers (notably {!Driver} and nested forks) can plug
  their own configuration (temperature, reasoning, etc.) via optional
  labelled arguments.
**********************************************************************)

(** Response_loop – repeat until no pending function calls

    A generic helper that keeps calling the OpenAI model until the
    conversation reaches a *quiescent* state – i.e. the last response
    contains no [`Function_call`] items.  At each iteration the newly
    requested calls are resolved through the [`tool_tbl`] mapping and the
    resulting [`Function_call_output`] items are appended to the
    history.

    The algorithm is synchronous and therefore used by non-streaming
    code paths (CLI, tests).  The streaming variant lives in
    {!Fork.run_stream}.
*)

(*********************************************************************
    Generic response-loop used by both the public helper
    [execute_response_loop] (used by CLI / driver code) and the
    private recursion inside [run_agent].  The two former versions
    had virtually identical bodies – only the captured configuration
    (temperature, tools …) differed.

    We factor the common algorithm here: keep issuing completion
    requests until no pending function-calls remain, resolving any
    function-call items with the provided [tool_tbl].
**********************************************************************)

(* Shared execution loop that keeps calling the OpenAI model until there are
    no pending function calls.  The former implementation threaded [dir],
    [net] and [cache] explicitly; after the step-8 refactor these live in the
    immutable context record [Ctx.t]. *)

(** [run ~ctx ?temperature ?max_output_tokens ?tools ?reasoning ~model ~tool_tbl history]
    expands [history] until the last assistant message contains **no**
    [`Function_call`] item.

    Parameters:
    • [ctx] – immutable context that provides network access, current
      directory and a shared cache.
    • [?temperature] – sampling temperature forwarded verbatim to the
      model (defaults to the server-side value).
    • [?max_output_tokens] – per-request upper bound on generated
      tokens.
    • [?tools] – flat list of available tools, forwarded unchanged so
      the model can call them.
    • [?reasoning] – request whether the model should emit
      [`Reasoning`] blocks.
    • [model] – OpenAI model used for **every** iteration.
    • [tool_tbl] – mapping from tool names to implementations.  The
      table **must** hold a ["fork"] entry pointing at {!Fork.execute}
      so the built-in [fork] tool works recursively.
    • [history] – full conversation to date (user messages, assistant
      replies, previous tool outputs …).

    Returns: the extended conversation made of the original [history]
    followed by every newly generated item.

    Complexity: O(k·m) API round-trips where *k* is the maximum nesting
    depth of function calls and *m* the size of the largest reply.

    @raise Not_found  if a function name produced by the model is **not**
            present in [tool_tbl]. *)

let rec run
          ~(ctx : _ Ctx.t)
          ?temperature
          ?max_output_tokens
          ?tools
          ?reasoning
          ?(fork_depth = 0)
          ?(history_compaction = false)
          ~model
          ~tool_tbl
          (history : Res.Item.t list)
  : Res.Item.t list
  =
  let inputs =
    if history_compaction
    then Compact_history.collapse_read_file_history history
    else history
  in
  (* 1.  Send current history to OpenAI and gather fresh items. *)
  match
    Res.post_response
      Res.Default
      ~dir:(Ctx.dir ctx)
      ~model
      ~parallel_tool_calls:true
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      (Ctx.net ctx)
      ~inputs
  with
  | exception Res.Response_stream_parsing_error (_, _) ->
    run
      ~ctx
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      ~model
      ~history_compaction
      ~fork_depth
      ~tool_tbl
      history
  | response ->
    let new_items = response.output in
    (* 2.  Extract any function-call requests from the newly returned
           items. *)
    let function_calls =
      List.filter_map new_items ~f:(function
        | Res.Item.Function_call fc -> Some fc
        | _ -> None)
    in
    (* 3.  If no calls – we're done.  Append the new items and return. *)
    if List.is_empty function_calls
    then history @ new_items
    else (
      (* 4.  Otherwise, run each requested tool, wrap the output into
             a Function_call_output item, and recurse with the extended
             history. *)
      let outputs =
        List.map function_calls ~f:(fun fc ->
          let fn = Hashtbl.find_exn tool_tbl fc.name in
          let res =
            if String.equal fc.name "fork"
            then (
              (* We do not have streaming callbacks in this synchronous path;
               pass in dummies.  History so far is [history @ new_items]
               (but we are still computing [outputs] so the current history
               is adequate). *)
              (* let env = Ctx.env ctx in *)
              (* Fork.execute
              ~env
              ~history:(history @ new_items)
              ~call_id:fc.call_id
              ~arguments:fc.arguments
              ~tools:(Option.value tools ~default:[])
              ~tool_tbl
              ~on_event:(fun _ -> ())
              ~on_fn_out:(fun _ -> ())
              ?temperature
              ?max_output_tokens
              ?reasoning
              () *)
              let inputs =
                if history_compaction
                then Compact_history.collapse_read_file_history (history @ new_items)
                else history @ new_items
              in
              match fork_depth with
              | 0 | 1 ->
                let res =
                  run
                    ~ctx
                    ?temperature
                    ?max_output_tokens
                    ?tools
                    ?reasoning
                    ~history_compaction
                    ~fork_depth:(fork_depth + 1)
                    ~model
                    ~tool_tbl
                  @@ Fork.history ~history:inputs ~arguments:fc.arguments fc.call_id
                in
                [ List.last_exn res ]
                |> List.filter_map ~f:(function
                  | Res.Item.Output_message o ->
                    Some
                      (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" ")
                  | _ -> None)
                |> String.concat ~sep:"\n"
              | _ ->
                "Error: Called the [fork] tool in a forked process! Remember that if you \
                 are running in a forked process that you must Respond with a message in \
                 the required Format when finished with the task.")
            else fn fc.arguments
          in
          let data =
            Res.Item.Function_call_output
              { output = res
              ; call_id = fc.call_id
              ; _type = "function_call_output"
              ; id = None
              ; status = None
              }
          in
          Io.log
            ~dir:(Ctx.dir ctx)
            ~file:"raw-openai-response.txt"
            ((Jsonaf.to_string @@ Res.Item.jsonaf_of_t @@ data) ^ "\n");
          data)
      in
      run
        ~ctx
        ?temperature
        ?max_output_tokens
        ?tools
        ?reasoning
        ~history_compaction
        ~fork_depth
        ~model
        ~tool_tbl
        (history @ new_items @ outputs))
;;
