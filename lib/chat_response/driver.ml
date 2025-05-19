open Core
module CM = Prompt_template.Chat_markdown
module Res = Openai.Responses

let rec run_agent ~(ctx : _ Ctx.t) (prompt_xml : string) (items : CM.content_item list)
  : string
  =
  (* 1.  Extract individual components from the shared context *)
  let dir = Ctx.dir ctx in
  (* 1.  Build the full agent XML by adding any inline user items. *)
  let user_suffix =
    if List.is_empty items
    then ""
    else
      Printf.sprintf
        "<msg role=\"user\">\n%s\n</msg>"
        (Converter.string_of_items ~ctx ~run_agent items)
  in
  let full_xml = prompt_xml ^ "\n" ^ user_suffix in
  (* 2.  Parse the merged document into structured elements. *)
  let elements = CM.parse_chat_inputs ~dir full_xml in
  (* 3.  Configuration (max_tokens, model, …) *)
  let cfg = Config.of_elements elements in
  let CM.{ max_tokens; model; reasoning_effort; temperature; show_tool_call = _; id = _ } =
    cfg
  in
  let model =
    Option.value_map model ~default:Res.Request.Gpt4 ~f:Res.Request.model_of_str_exn
  in
  let reasoning =
    Option.map reasoning_effort ~f:(fun eff ->
      Res.Request.Reasoning.
        { effort = Some (Effort.of_str_exn eff); summary = Some Summary.Detailed })
  in
  (* 4.  Collect tool declarations inside the agent prompt. *)
  let declared_tools =
    List.filter_map elements ~f:(function
      | CM.Tool t -> Some t
      | _ -> None)
  in
  let tools : Gpt_function.t list =
    List.map declared_tools ~f:(fun decl ->
      (* For nested agent tools we want them to run with a filesystem
         root identical to the one used by the current agent – hence we
         ~ctx:{ ctx with dir = Eio.Stdenv.cwd (Ctx.env ctx) }
         reuse [ctx] directly. *)
      let ctx_for_tool =
        match decl with
        | CM.Agent _ -> { ctx with dir = Eio.Stdenv.cwd (Ctx.env ctx) }
        | _ -> { ctx with dir = Eio.Stdenv.cwd (Ctx.env ctx) }
      in
      Tool.of_declaration ~ctx:ctx_for_tool ~run_agent decl)
  in
  let comp_tools, tool_tbl = Gpt_function.functions tools in
  let tools_req = Tool.convert_tools comp_tools in
  (* 5.  Convert XML ‑> API items and enter the execute loop to handle function calls. *)
  let init_items = Converter.to_items ~ctx ~run_agent elements in
  let all_items =
    Response_loop.run
      ~ctx
      ?temperature
      ?max_output_tokens:max_tokens
      ~tools:tools_req
      ?reasoning
      ~model
      ~tool_tbl
      init_items
  in
  (* 6.  Extract assistant messages and concatenate them. *)
  List.drop all_items (List.length init_items)
  |> List.filter_map ~f:(function
    | Res.Item.Output_message o ->
      Some (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" ")
    | _ -> None)
  |> String.concat ~sep:"\n"
;;

(*──────────────────────── 6.  Main driver  ───────────────────────────────*)

(*──────────────────────── 7.  Public helper  ─────────────────────────────*)
let run_completion
      ~env
      ?prompt_file (* optional template to prepend *)
      ~output_file (* evolving conversation buffer *)
      ()
  =
  let dir = Eio.Stdenv.fs env in
  let cwd = Eio.Stdenv.cwd env in
  (* Ensure the hidden data directory exists and get its path. *)
  let datadir = Io.ensure_chatmd_dir ~cwd in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1000 () in
  (* 1 • append initial prompt file if provided *)
  Option.iter prompt_file ~f:(fun file ->
    Io.append_doc ~dir output_file (Io.load_doc ~dir file));
  (* 2 • main loop *)
  let rec loop () =
    let xml = Io.load_doc ~dir output_file in
    let elements = CM.parse_chat_inputs ~dir xml in
    (* gather config *)
    let cfg = Config.of_elements elements in
    let CM.
          { max_tokens = model_tokens
          ; model = model_opt
          ; reasoning_effort
          ; temperature
          ; _
          }
      =
      cfg
    in
    let reasoning =
      Option.map reasoning_effort ~f:(fun eff ->
        { Res.Request.Reasoning.effort =
            Some (Res.Request.Reasoning.Effort.of_str_exn eff)
        ; summary = Some Detailed
        })
    in
    (* tools / function mapping *)
    let comp_tools, tool_tbl = Gpt_function.functions [] in
    let tools = Tool.convert_tools comp_tools in
    (* convert xml → items and fire first request *)
    let ctx = Ctx.create ~env ~dir ~cache in
    let init_items = Converter.to_items ~ctx ~run_agent elements in
    (* For the response loop we use a context bound to the .chatmd data folder so
       that any tool-generated artefacts land in that directory. *)
    let ctx_loop = Ctx.create ~env ~dir:datadir ~cache in
    let all_items =
      Response_loop.run
        ~ctx:ctx_loop
        ?temperature
        ?max_output_tokens:model_tokens
        ~tools
        ?reasoning
        ~tool_tbl
        ~model:
          (Option.value_map
             model_opt
             ~default:Res.Request.Gpt4
             ~f:Res.Request.model_of_str_exn)
        init_items
    in
    (* 3 • render assistant text back into XML buffer *)
    let append = Io.append_doc ~dir output_file in
    List.iter
      (List.drop all_items (List.length init_items))
      ~f:(function
        | Res.Item.Output_message o ->
          append
            (Printf.sprintf
               "<msg role=\"assistant\" id=\"%s\">\n\t%s|\n\t\t%s\n\t|%s\n</msg>\n"
               o.id
               "RAW"
               (Fetch.tab_on_newline
                  (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" "))
               "RAW")
        | Res.Item.Reasoning r ->
          let summaries =
            List.map r.summary ~f:(fun s ->
              Printf.sprintf
                "\n<summary>\n\t\t%s\n</summary>\n"
                (Fetch.tab_on_newline s.text))
            |> String.concat ~sep:""
          in
          print_endline "summaries";
          print_endline summaries;
          append
            (Printf.sprintf
               "\n<reasoning id=\"%s\">\n%s\n</reasoning>\n"
               r.id
               (Fetch.tab_on_newline summaries))
        | Res.Item.Function_call fc ->
          append
            (Printf.sprintf
               "\n\
                <msg role=\"tool\" function_call function_name=\"%s\" call_id=\"%s\">\n\
                %s\n\
                </msg>\n"
               fc.name
               fc.call_id
               fc.arguments)
        | Res.Item.Function_call_output fco ->
          append
            (Printf.sprintf
               "\n<msg role=\"tool\" call_id=\"%s\">%s</msg>\n"
               fco.call_id
               fco.output)
        | Res.Item.Input_message _
        | Res.Item.Web_search_call _
        | Res.Item.File_search_call _ -> ());
    (* stop if no new function calls were produced *)
    if
      List.exists all_items ~f:(function
        | Res.Item.Function_call _ -> true
        | _ -> false)
    then loop ()
    else append "\n<msg role=\"user\">\n\n</msg>"
  in
  loop ();
  Cache.save ~file:cache_file cache
;;

let run_completion_stream
      ~env
      ?prompt_file (* optional template to prepend once          *)
      ?(on_event : Openai.Responses.Response_stream.t -> unit = fun _ -> ())
      ~output_file (* evolving conversation buffer               *)
      ()
  =
  (* ─────────────────────── 0.  setup & helpers ───────────────────────── *)
  let dir = Eio.Stdenv.fs env in
  let cwd = Eio.Stdenv.cwd env in
  let datadir = Io.ensure_chatmd_dir ~cwd in
  let net = env#net in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1_000 () in
  let append_doc = Io.append_doc ~dir output_file in
  Option.iter prompt_file ~f:(fun file -> append_doc (Io.load_doc ~dir file));
  (* Pretty logger: every event – even if we do not act on it *)
  let log_event _ev =
    (* print_endline "STREAM EVENT:";
    print_endline (Jsonaf.to_string_hum (Res.Response_stream.jsonaf_of_t ev)) *)
    ()
  in
  let fn_id = ref 0 in
  (* ─────────────────────── 1.  main recursive turn ────────────────────── *)
  let rec turn () =
    (* 1‑A • read current prompt XML and parse *)
    let xml = Io.load_doc ~dir output_file in
    let elements = CM.parse_chat_inputs ~dir xml in
    (* 1‑B • current config (max_tokens, model, …) *)
    let cfg = Config.of_elements elements in
    let CM.{ max_tokens; model; reasoning_effort; temperature; show_tool_call; id = _ } =
      cfg
    in
    let model =
      Option.value_map model ~f:Res.Request.model_of_str_exn ~default:Res.Request.Gpt4
    in
    let reasoning =
      Option.map reasoning_effort ~f:(fun eff ->
        Res.Request.Reasoning.
          { effort = Some (Effort.of_str_exn eff); summary = Some Summary.Detailed })
    in
    let tools =
      List.filter_map elements ~f:(function
        | CM.Tool t -> Some t
        | _ -> None)
      |> List.map ~f:(fun decl ->
        let ctx_for_tool =
          match decl with
          | CM.Agent _ -> Ctx.create ~env ~dir:(Eio.Stdenv.cwd env) ~cache
          | _ -> Ctx.create ~env ~dir:(Eio.Stdenv.cwd env) ~cache
        in
        Tool.of_declaration ~ctx:ctx_for_tool ~run_agent decl)
    in
    (* 1‑C • tools / functions *)
    let comp_tools, tool_tbl = Gpt_function.functions tools in
    let tools = Tool.convert_tools comp_tools in
    (* 1‑D • initial request items *)
    let ctx = Ctx.create ~env ~dir ~cache in
    let inputs = Converter.to_items ~ctx ~run_agent elements in
    (* ────────────────── 2.  streaming callback state ─────────────────── *)
    (* existing tables … *)
    let opened_msgs : (string, unit) Hashtbl.t = Hashtbl.create (module String)
    and func_info : (string, string * string) Hashtbl.t = Hashtbl.create (module String)
    (* NEW – track currently open reasoning‑blocks, mapping id → current summary_index *)
    and reasoning_state : (string, int) Hashtbl.t = Hashtbl.create (module String) in
    let run_again = ref false in
    let output_text_delta ~id txt =
      if not (Hashtbl.mem opened_msgs id)
      then (
        append_doc
          (Printf.sprintf "\n<msg role=\"assistant\" id=\"%s\">\n\t%s|\n\t\t" id "RAW");
        Hashtbl.set opened_msgs ~key:id ~data:());
      append_doc (Fetch.tab_on_newline txt)
    in
    let close_message id =
      if Hashtbl.mem opened_msgs id
      then (
        append_doc (Printf.sprintf "\n\t|%s\n</msg>\n" "RAW");
        (* remove the message from the opened list *)
        Hashtbl.remove opened_msgs id)
    in
    let lt, gt = "<", ">" in
    (* avoid raw “<tag>” in the output *)
    let open_reasoning id =
      append_doc
        (Printf.sprintf "\n%sreasoning id=\"%s\"%s\n\t%ssummary%s\n\t\t" lt id gt lt gt)
    in
    let open_new_summary () = append_doc (Printf.sprintf "\n\t%ssummary%s\n\t\t" lt gt) in
    let close_summary () = append_doc (Printf.sprintf "\n\t%s/summary%s" lt gt) in
    let close_reasoning () = append_doc (Printf.sprintf "\n%s/reasoning%s\n" lt gt) in
    let handle_function_done ~item_id ~arguments =
      match Hashtbl.find func_info item_id with
      | None -> () (* should not happen *)
      | Some (name, call_id) ->
        let i = !fn_id in
        let tool_call_url id = Printf.sprintf "%i.tool-call.%s.json" i id in
        (* assistant’s tool‑call request *)
        (* if show_tool_call is true, then we will show the tool call in the output *)
        (* otherwise, we will save the tool call to a file and not show it in the output *)
        if show_tool_call
        then
          append_doc
            (Printf.sprintf
               "\n\
                <msg role=\"tool\" tool_call tool_call_id=\"%s\" function_name=\"%s\" \
                id=\"%s\">\n\
                \t%s|\n\
                \t\t%s\n\
                \t|%s\n\
                </msg>\n"
               call_id
               name
               item_id
               "RAW"
               (Fetch.tab_on_newline arguments)
               "RAW")
        else (
          let content =
            Printf.sprintf "<doc src=\"./.chatmd/%s\" local>" (tool_call_url call_id)
          in
          append_doc
            (Printf.sprintf
               "\n\
                <msg role=\"tool\" tool_call tool_call_id=\"%s\" function_name=\"%s\" \
                id=\"%s\">\n\
                \t%s\n\
                </msg>\n"
               call_id
               name
               item_id
               (Fetch.tab_on_newline content));
          (* save the tool call to a file *)
          Io.save_doc ~dir:datadir (tool_call_url call_id) arguments);
        (* run the tool *)
        let fn = Hashtbl.find_exn tool_tbl name in
        let result = fn arguments in
        let tool_call_result_url id = Printf.sprintf "%i.tool-call-result.%s.json" i id in
        if show_tool_call
        then
          append_doc
            (Printf.sprintf
               "\n\
                <msg role=\"tool\" tool_call_id=\"%s\" id=\"%s\">\n\
                \t%s|\n\
                \t\t%s\n\
                \t|%s\n\
                </msg>\n"
               call_id
               item_id
               "RAW"
               (Fetch.tab_on_newline result)
               "RAW")
        else (
          let content =
            Printf.sprintf
              "<doc src=\"./.chatmd/%s\" local>"
              (tool_call_result_url call_id)
          in
          append_doc
            (Printf.sprintf
               "\n<msg role=\"tool\" tool_call_id=\"%s\" id=\"%s\">\n\t%s\n</msg>\n"
               call_id
               item_id
               (Fetch.tab_on_newline content));
          Io.save_doc ~dir:datadir (tool_call_result_url call_id) result);
        Int.incr fn_id;
        run_again := true
    in
    let callback (ev : Res.Response_stream.t) =
      (* For debugging purposes we still log every event. *)
      log_event ev;
      (* Internal book-keeping for writing the streamed response back into the
         conversation buffer and executing tool calls. *)
      (match ev with
       (* ───────────────────────── assistant text ────────────────────── *)
       | Res.Response_stream.Output_text_delta { item_id; delta; _ } ->
         output_text_delta ~id:item_id delta
       | Res.Response_stream.Output_item_done { item; _ } ->
         (match item with
          | Res.Response_stream.Item.Output_message om -> close_message om.id
          | Res.Response_stream.Item.Reasoning r ->
            (* close an open reasoning block, if any *)
            (match Hashtbl.find reasoning_state r.id with
             | Some _ ->
               close_summary ();
               close_reasoning ();
               Hashtbl.remove reasoning_state r.id
             | None -> ())
          | _ -> ())
       (* ─────────────────────── reasoning deltas ────────────────────── *)
       | Res.Response_stream.Reasoning_summary_text_delta
           { item_id; delta; summary_index; _ } ->
         (match Hashtbl.find reasoning_state item_id with
          | None ->
            (* first chunk for this reasoning item *)
            open_reasoning item_id;
            Hashtbl.set reasoning_state ~key:item_id ~data:summary_index
          | Some current when current = summary_index -> () (* same summary → continue *)
          | Some _ ->
            (* moved to the next summary *)
            close_summary ();
            open_new_summary ();
            Hashtbl.set reasoning_state ~key:item_id ~data:summary_index);
         append_doc (Fetch.tab_on_newline delta)
       (* ────────────────────── function calls etc. ──────────────────── *)
       | Res.Response_stream.Output_item_added { item; _ } ->
         (match item with
          | Res.Response_stream.Item.Function_call fc ->
            let idx = Option.value fc.id ~default:fc.call_id in
            Hashtbl.set func_info ~key:idx ~data:(fc.name, fc.call_id)
          | Res.Response_stream.Item.Reasoning r ->
            (* first chunk for this reasoning item *)
            open_reasoning r.id;
            Hashtbl.set reasoning_state ~key:r.id ~data:0
          | _ -> ())
       | Res.Response_stream.Function_call_arguments_done { item_id; arguments; _ } ->
         handle_function_done ~item_id ~arguments
       | _ -> ());
      on_event ev
    in
    (* ────────────────── 3.  fire request in stream mode ──────────────── *)
    Res.post_response
      (Res.Stream callback)
      ?max_output_tokens:max_tokens
      ?temperature
      ~tools
      ?reasoning
      ~model
      ~dir:datadir
      net
      ~inputs;
    (* make sure any dangling assistant block is closed *)
    Hashtbl.iter_keys opened_msgs ~f:(fun id -> close_message id);
    (match !run_again with
     | true -> print_endline "run again"
     | false -> print_endline "no run again");
    (* 4 • If no function call just happened, append empty user message.   *)
    (* 4 • If a function call just happened, recurse for the next turn.   *)
    if !run_again then turn () else append_doc "\n<msg role=\"user\">\n\n</msg>"
  in
  turn ();
  Cache.save ~file:cache_file cache
;;
