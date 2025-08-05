open Core
module Res = Openai.Responses

(** Nested *fork* execution

    The fork tool allows a conversation to spawn an **auxiliary agent**
    that runs in isolation and eventually reports back using a special
    `PERSIST` block.  This OCaml module contains the runtime support to
    drive that nested conversation without depending on {!Driver} – a
    lighter subset is required to keep recursion small.

    There are two entry points:

    {ul
    {- {!run_stream} – streaming version used when the parent
       conversation itself is streamed.}
    {- {!execute}     – synchronous helper used by the non-streaming
       response loop.}}

    Both functions guarantee that the *outer* assistant receives a
    function-call output as soon as text becomes available so that user
    interfaces can render fork progress in real time.
*)

(* -------------------------------------------------------------------- *)
(*  Helper: build a system/input message instructing the forked agent.   *)
(* -------------------------------------------------------------------- *)

(* -------------------------------------------------------------------- *)
(*  Minimal nested streaming driver (avoids dependency on [Driver]).     *)
(* -------------------------------------------------------------------- *)

let rec run_stream
          ~(env : Eio_unix.Stdenv.base)
          ~(initial_history : Res.Item.t list)
          ~(tools : Res.Request.Tool.t list)
          ~(tool_tbl : (string, string -> string) Base.Hashtbl.t)
          ~(on_event : Res.Response_stream.t -> unit)
          ~(on_fn_out : Res.Function_call_output.t -> unit)
          ~(call_id_parent : string)
          ~(output_buffer : Buffer.t)
          ?temperature
          ?max_output_tokens
          ?reasoning
          ()
  : Res.Item.t list
  =
  let net = env#net in
  let cwd = Eio.Stdenv.cwd env in
  let datadir = Io.ensure_chatmd_dir ~cwd in
  (* A small cache is sufficient. *)
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1_000 () in
  (* ------------------------------------------------------------------ *)
  (* Recursive turn function                                             *)
  (* ------------------------------------------------------------------ *)
  let rec turn (hist : Res.Item.t list) : Res.Item.t list =
    (* Tables for tracking function calls and reasoning items. *)
    let func_info : (string, string * string) Hashtbl.t =
      Hashtbl.create (module String)
    in
    let reasoning_state : (string, int) Hashtbl.t = Hashtbl.create (module String) in
    let new_items : Res.Item.t list ref = ref [] in
    let add_item it = new_items := it :: !new_items in
    let run_again = ref false in
    (* Execute a tool once its arguments have been streamed. *)
    let handle_function_done ~item_id ~arguments =
      match Hashtbl.find func_info item_id with
      | None ->
        (* Should not happen, return dummy output *)
        { Res.Function_call_output.output = ""
        ; call_id = ""
        ; _type = "function_call_output"
        ; id = None
        ; status = None
        }
      | Some (name, call_id) ->
        let fn = Hashtbl.find_exn tool_tbl name in
        let result =
          if String.equal name "fork"
          then
            (* Recursive fork call – propagate streaming further. *)
            execute
              ~env
              ~history:hist
              ~call_id
              ~arguments
              ~tools
              ~tool_tbl
              ~on_event
              ~on_fn_out
              ?temperature
              ?max_output_tokens
              ?reasoning
              ()
          else fn arguments
        in
        let fn_call_item : Res.Item.t =
          Res.Item.Function_call
            { name
            ; arguments
            ; call_id
            ; _type = "function_call"
            ; id = Some item_id
            ; status = None
            }
        in
        let fn_out : Res.Function_call_output.t =
          { output = result
          ; call_id
          ; _type = "function_call_output"
          ; id = None
          ; status = None
          }
        in
        let fn_out_item = Res.Item.Function_call_output fn_out in
        new_items := fn_out_item :: fn_call_item :: !new_items;
        run_again := true;
        fn_out
    in
    (* Streaming callback – forwards to caller while building history. *)
    let stream_cb (ev : Res.Response_stream.t) =
      (match ev with
       (* Assistant text progress – accumulate and propagate *)
       | Res.Response_stream.Output_text_delta { delta; _ } ->
         Stdlib.Buffer.add_string output_buffer delta;
         let fn_out : Res.Function_call_output.t =
           { output = Stdlib.Buffer.contents output_buffer
           ; call_id = call_id_parent
           ; _type = "function_call_output"
           ; id = None
           ; status = None
           }
         in
         on_fn_out fn_out
       (* New item announced – track pending function calls *)
       | Res.Response_stream.Output_item_added { item; _ } ->
         (match item with
          | Res.Response_stream.Item.Function_call fc ->
            let idx = Option.value fc.id ~default:fc.call_id in
            Hashtbl.set func_info ~key:idx ~data:(fc.name, fc.call_id)
          | Res.Response_stream.Item.Reasoning r ->
            Hashtbl.set reasoning_state ~key:r.id ~data:0
          | _ -> ())
       (* Completed item – append to history list *)
       | Res.Response_stream.Output_item_done { item; _ } ->
         (match item with
          | Res.Response_stream.Item.Output_message om ->
            add_item (Res.Item.Output_message om)
          | Res.Response_stream.Item.Reasoning r -> add_item (Res.Item.Reasoning r)
          | _ -> ())
       (* Function-call argument streaming finished – run tool *)
       | Res.Response_stream.Function_call_arguments_done { item_id; arguments; _ } ->
         let fn_out = handle_function_done ~item_id ~arguments in
         (* also propagate result if this was nested call *)
         on_fn_out fn_out
       | _ -> ());
      (* Forward every raw event upward so the parent UI can, if desired,
         display fork activity.  The TUI will distinguish forked events by
         monitoring whether a fork tool call with [call_id_parent] is
         currently outstanding. *)
      on_event ev
    in
    (* Fire request. *)
    Res.post_response
      (Res.Stream stream_cb)
      ~dir:datadir
      ?temperature
      ?max_output_tokens
      ?reasoning
      ~parallel_tool_calls:true
      net
      ~inputs:hist
      ~tools
      ~model:Res.Request.O3;
    let next_hist = hist @ List.rev !new_items in
    if !run_again then turn next_hist else next_hist
  in
  let full = turn initial_history in
  Cache.save ~file:cache_file cache;
  full

(* -------------------------------------------------------------------- *)
(*  Main [execute] entry point                                           *)
(* -------------------------------------------------------------------- *)

(* ------------------------------------------------------------------ *)
(*  Implementation of [execute] – relies on [run_stream].               *)
(* ------------------------------------------------------------------ *)

(** [execute ~env ~history ~call_id ~arguments ~tools ~tool_tbl …] runs a
    *fork* session to completion and returns the assistant’s final text
    reply.

    Under the hood the helper:

    1. Adds a synthetic [`Function_call_output`] that explains the fork
       protocol to the clone.
    2. Delegates the actual conversation to {!run_stream} so that nested
       function calls keep working.
    3. Concatenates the assistant messages produced after the initial
       history and returns the merged string to the parent loop.

    The function is synchronous – callers will block until the forked
    agent finishes or the outer OpenAI request reaches its token limit.
  *)
and execute
      ~(env : Eio_unix.Stdenv.base)
      ~(history : Res.Item.t list)
      ~(call_id : string)
      ~(arguments : string)
      ~(tools : Res.Request.Tool.t list)
      ~(tool_tbl : (string, string -> string) Base.Hashtbl.t)
      ~(on_event : Res.Response_stream.t -> unit)
      ~(on_fn_out : Res.Function_call_output.t -> unit)
      ?temperature
      ?max_output_tokens
      ?reasoning
      ()
  : string
  =
  let input = Definitions.Fork.input_of_string arguments in
  let command = input.command in
  let argv = input.arguments in
  let arg_str = String.concat ~sep:" " argv in
  let instruction_text =
    Printf.sprintf
      {|SYSTEM MESSAGE – Forked Agent

You are an **isolated clone** of the main assistant.  Your internal state will be *discarded* once you hand control back and merge with the parent.  Only the information you explicitly place in the *PERSIST* section will survive.

Primary task inside the fork
• Execute: 
  command - `%s`  
  arguments - `%s`

You may leverage every available tool (except the [fork] tool), read/write files if capable, and generate extensive output.  Work **thoroughly**; token limits are not a concern in this fork.

Return exactly **one** assistant message in this template:

```
===RESULT===
<Exhaustive narrative of EVERYTHING you did – reasoning, obstacles, fixes, validation, code patches (use fenced blocks), logs, etc.>

===PERSIST===
<Consise ≤20 bullet points capturing facts, artefacts, follow-ups, or warnings the parent must retain. Bullets can be as detailed as needed, but should be succinct>
```

Best-practice reminders (GPT-4.1 / O3):
• Think step-by-step internally; *write* that reasoning in RESULT for auditability.
• Perform a quick self-check before replying; note unresolved issues in PERSIST.
• Avoid filler phrases like “let’s think step-by-step”.  Just reason and write.

Call-ID: %s
|}
      command
      arg_str
      call_id
  in
  (* Inject the instruction as a Function_call_output so the clone is aware
     it is a forked agent and what command to run. *)
  let instruction_fn_out : Res.Function_call_output.t =
    { output = instruction_text
    ; call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = None
    }
  in
  let instruction_item = Res.Item.Function_call_output instruction_fn_out in
  let clone_history = history @ [ instruction_item ] in
  (* Buffer to accumulate and stream progress back to parent *)
  let output_buffer = Stdlib.Buffer.create 256 in
  let full_history =
    run_stream
      ~env
      ~initial_history:clone_history
      ~tools
      ~tool_tbl
      ~on_event
      ~on_fn_out
      ~call_id_parent:call_id
      ~output_buffer
      ?temperature
      ?max_output_tokens
      ?reasoning
      ()
  in
  let new_items = List.drop full_history (List.length clone_history) in
  let assistant_msgs =
    List.filter_map new_items ~f:(function
      | Res.Item.Output_message om ->
        Some (List.map om.content ~f:(fun c -> c.text) |> String.concat ~sep:" ")
      | _ -> None)
  in
  let final_reply = String.concat ~sep:"\n" assistant_msgs in
  (* Return the final reply; the parent driver will create the definitive
     Function_call_output once [execute] returns. *)
  final_reply
;;

let history ~history ~arguments call_id =
  let input = Definitions.Fork.input_of_string arguments in
  let command = input.command in
  let argv = input.arguments in
  let arg_str = String.concat ~sep:" " argv in
  let instruction_text =
    Printf.sprintf
      {|SYSTEM MESSAGE – Forked Agent

You are an **isolated clone** of the main assistant.  Your internal state will be *discarded* once you hand control back and merge with the parent.  Only the information you explicitly place in the *PERSIST* section will survive.

Primary task inside the fork
• Execute: 
  command - `%s`  
  arguments - `%s`

You may leverage every available tool (except the [fork] tool), read/write files if capable, and generate extensive output.  Work **thoroughly**; token limits are not a concern in this fork.

Return exactly **one** assistant message in this template:

```
===RESULT===
<Exhaustive narrative of EVERYTHING you did – reasoning, obstacles, fixes, validation, code patches (use fenced blocks), logs, etc.>

===PERSIST===
<Consise ≤20 bullet points capturing facts, artefacts, follow-ups, or warnings the parent must retain. Bullets can be as detailed as needed, but should be succinct>
```

Best-practice reminders (GPT-4.1 / O3):
• Think step-by-step internally; *write* that reasoning in RESULT for auditability.
• Perform a quick self-check before replying; note unresolved issues in PERSIST.
• Avoid filler phrases like “let’s think step-by-step”.  Just reason and write.

Call-ID: %s
|}
      command
      arg_str
      call_id
  in
  (* Inject the instruction as a Function_call_output so the clone is aware
     it is a forked agent and what command to run. *)
  let instruction_fn_out : Res.Function_call_output.t =
    { output = instruction_text
    ; call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = None
    }
  in
  let instruction_item = Res.Item.Function_call_output instruction_fn_out in
  history @ [ instruction_item ]
;;
