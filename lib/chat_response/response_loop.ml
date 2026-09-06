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
module Output = Res.Tool_output.Output

type post =
  sw:Eio.Switch.t
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> inputs:Res.Item.t list
  -> Res.Response.t

let compatibility_namespace =
  let next = Atomic.make 0 in
  fun () ->
    let sequence = Atomic.fetch_and_add next 1 in
    Printf.sprintf "response-loop-%d" sequence
;;

let create_entries ~allocator items =
  List.map items ~f:(History_entry.create ~allocator)
  |> Result.all
  |> Result.ok_or_failwith
;;

let max_response_retries = 5
let retry_delay retry_number = Float.of_int retry_number

let retry_request ~sleep ~f =
  let rec loop retries =
    match f () with
    | response -> response
    | (exception Res.Response_stream_parsing_error (_, cause))
    | (exception Res.Response_parsing_error (_, cause)) ->
      if retries >= max_response_retries
      then
        failwithf
          "OpenAI response parsing failed after %d retries: %s"
          max_response_retries
          (Exn.to_string cause)
          ()
      else (
        let retry_number = retries + 1 in
        sleep (retry_delay retry_number);
        loop retry_number)
  in
  loop 0
;;

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

let rec run_entries
          ~(ctx : < clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > Ctx.t)
          ~allocator
          ?temperature
          ?max_output_tokens
          ?tools
          ?reasoning
          ?(fork_depth = 0)
          ?(history_compaction = false)
          ?response_dir
          ?post
          ~model
          ~tool_tbl
          (history : History_entry.t list)
  : History_entry.t list
  =
  let request_entries =
    if history_compaction
    then Compact_history.collapse_read_file_entries history
    else history
  in
  let inputs = History_entry.items request_entries in
  let response_dir = Option.value response_dir ~default:(Ctx.dir ctx) in
  let post =
    Option.value post ~default:(fun ~sw ~dir ~inputs ->
      Res.post_response
        Res.Default
        ~dir
        ~model
        ~parallel_tool_calls:true
        ?temperature
        ?max_output_tokens
        ?tools
        ?reasoning
        ~sw
        (Ctx.net ctx)
        ~inputs)
  in
  (* 1.  Send current history to OpenAI and gather fresh items. *)
  let response =
    retry_request
      ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock (Ctx.env ctx)))
      ~f:(fun () -> Eio.Switch.run (fun sw -> post ~sw ~dir:response_dir ~inputs))
  in
  let new_entries = create_entries ~allocator response.output in
  let new_items = History_entry.items new_entries in
  (* 2.  Extract any tool-call requests from the newly returned items. *)
  let tool_calls =
    List.filter_map new_items ~f:(function
      | Res.Item.Function_call fc -> Some (`Function fc)
      | Res.Item.Custom_tool_call tc -> Some (`Custom tc)
      | _ -> None)
  in
  (* 3.  If no calls – we're done.  Append the new items and return. *)
  if List.is_empty tool_calls
  then history @ new_entries
  else (
    (* 4.  Otherwise, run each requested tool, wrap the output into
             a Function_call_output item, and recurse with the extended
             history. *)
    let outputs =
      List.map tool_calls ~f:(fun call ->
        let name, call_id, payload =
          match call with
          | `Function fc -> fc.name, fc.call_id, fc.arguments
          | `Custom tc -> tc.name, tc.call_id, tc.input
        in
        let res =
          match call with
          | `Function _ when String.equal name "fork" ->
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
            let fork_entries =
              if history_compaction
              then Compact_history.collapse_read_file_entries (history @ new_entries)
              else history @ new_entries
            in
            (match fork_depth with
             | 0 | 1 ->
               let invocation_id = Fork.Invocation_id.create () in
               let child_allocator =
                 Fork.allocator
                   ~parent_namespace:(History_entry.Allocator.namespace allocator)
                   invocation_id
               in
               let fork_history =
                 Fork.history_entries
                   ~allocator:child_allocator
                   ~history:fork_entries
                   ~arguments:payload
                   ~call_id
               in
               let res =
                 run_entries
                   ~ctx
                   ~allocator:child_allocator
                   ?temperature
                   ?max_output_tokens
                   ?tools
                   ?reasoning
                   ~history_compaction
                   ~fork_depth:(fork_depth + 1)
                   ~response_dir
                   ~post
                   ~model
                   ~tool_tbl
                   fork_history
               in
               let result =
                 [ History_entry.item (List.last_exn res) ]
                 |> List.filter_map ~f:(function
                   | Res.Item.Output_message o ->
                     Some
                       (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" ")
                   | _ -> None)
                 |> String.concat ~sep:"\n"
               in
               Output.Text result
             | _ ->
               Output.Text
                 "Error: Called the [fork] tool in a forked process! Remember that if \
                  you are running in a forked process that you must Respond with a \
                  message in the required Format when finished with the task.")
          | `Custom _ when String.equal name "fork" ->
            Output.Text "Error: [fork] cannot be invoked as a custom tool call."
          | _ ->
            let runner = Hashtbl.find_exn tool_tbl name in
            let kind =
              match call with
              | `Function _ -> `Function
              | `Custom _ -> `Custom
            in
            Tool_executor.run ~kind ~call_id ~name ~payload ~runner ()
        in
        let data : Res.Item.t =
          match call with
          | `Function _ ->
            Tool_call.output_item ~kind:Tool_call.Kind.Function ~call_id ~output:res
          | `Custom _ ->
            Tool_call.output_item ~kind:Tool_call.Kind.Custom ~call_id ~output:res
        in
        Io.log
          ~dir:response_dir
          ~file:"raw-openai-response.txt"
          (Jsonaf.to_string (Res.Item.jsonaf_of_t data) ^ "\n");
        data)
    in
    let output_entries = create_entries ~allocator outputs in
    run_entries
      ~ctx
      ~allocator
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      ~history_compaction
      ~fork_depth
      ~response_dir
      ~model
      ~tool_tbl
      ~post
      (history @ new_entries @ output_entries))
;;

module For_testing = struct
  let retry_request = retry_request
end
