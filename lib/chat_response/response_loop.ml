open Core
module Res = Openai.Responses

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

let rec run
          ~(ctx : _ Ctx.t)
          ?temperature
          ?max_output_tokens
          ?tools
          ?reasoning
          ~model
          ~tool_tbl
          (history : Res.Item.t list)
  : Res.Item.t list
  =
  (* 1.  Send current history to OpenAI and gather fresh items. *)
  let response =
    Res.post_response
      Res.Default
      ~dir:(Ctx.dir ctx)
      ~model
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      (Ctx.net ctx)
      ~inputs:history
  in
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
        let res = fn fc.arguments in
        Res.Item.Function_call_output
          { output = res
          ; call_id = fc.call_id
          ; _type = "function_call_output"
          ; id = None
          ; status = None
          })
    in
    run
      ~ctx
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      ~model
      ~tool_tbl
      (history @ new_items @ outputs))
;;
