open Core
module CM = Prompt_template.Chat_markdown
module Res = Openai.Responses

(*──────────────────────── 1.  Agent response cache ─────────────────────*)
module Agent_key = struct
  type t = CM.agent_content [@@deriving sexp, bin_io, hash, compare]

  let invariant (_ : t) = ()
end

module Agent_LRU = Ttl_lru_cache.Make (Agent_key)

type persistent_form =
  { max_size : int
  ; items : (Agent_key.t * string Agent_LRU.entry) list
  }
[@@deriving bin_io]

let to_persistent lru =
  { max_size = Agent_LRU.max_size lru; items = Agent_LRU.to_alist lru }
;;

let of_persistent pf =
  let cache = Agent_LRU.create ~max_size:pf.max_size () in
  List.iter pf.items ~f:(fun (k, v) -> Agent_LRU.set cache ~key:k ~data:v);
  cache
;;

let write_cache ~file cache =
  Bin_prot_utils_eio.write_bin_prot'
    file
    [%bin_writer: persistent_form]
    (to_persistent cache)
;;

let read_cache ~file =
  of_persistent (Bin_prot_utils_eio.read_bin_prot' file [%bin_reader: persistent_form])
;;

(*──────────────────────── 2.  Helpers ───────────────────────────────────*)
let clean_html raw =
  let decompressed = Option.value ~default:raw (Result.ok (Ezgzip.decompress raw)) in
  let soup = Soup.parse decompressed in
  soup
  |> Soup.texts
  |> List.map ~f:String.strip
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:"\n"
;;

let tab_on_newline (input : string) : string =
  let buffer = Buffer.create (String.length input) in
  String.iter
    ~f:(fun c ->
      let open Char in
      Buffer.add_char buffer c;
      if c = '\n'
      then (
        Buffer.add_char buffer '\t';
        Buffer.add_char buffer '\t'))
    input;
  Buffer.contents buffer
;;

let get_remote ?(gzip = false) ~net url =
  let host = Io.Net.get_host url
  and path = Io.Net.get_path url in
  let headers =
    Http.Header.of_list
      (if gzip
       then [ "Accept", "*/*"; "Accept-Encoding", "gzip" ]
       else [ "Accept", "*/*" ])
  in
  Io.Net.get Io.Net.Default ~net ~host ~headers path
;;

let get_content ~dir ~net url is_local ~cleanup_html =
  if is_local
  then Io.load_doc ~dir url
  else (
    let raw = get_remote ~net url in
    if cleanup_html then clean_html raw else raw)
;;

(*──────────────────────── 3.  Converting CM → Res.Item.t ────────────────*)
let rec string_of_items ~dir ~net ~cache (items : CM.content_item list) : string =
  items
  |> List.map ~f:(function
    | CM.Basic b ->
      (match b.image_url, b.document_url with
       | Some { url }, _ ->
         if b.is_local
         then Printf.sprintf "<img src=\"%s\" local=\"true\"/>" url
         else Printf.sprintf "<img src=\"%s\"/>" url
       | _, Some doc_url ->
         get_content ~dir ~net doc_url b.is_local ~cleanup_html:b.cleanup_html
       | _, _ -> Option.value ~default:"" b.text)
    | CM.Agent ({ url; is_local; items } as agent) ->
      Agent_LRU.find_or_add cache agent ~ttl:Time_ns.Span.day ~default:(fun () ->
        let prompt = get_content ~dir ~net url is_local ~cleanup_html:false in
        run_agent ~dir ~net ~cache prompt items))
  |> String.concat ~sep:"\n"

and convert_basic_item ~dir ~net ~cache:_ (b : CM.basic_content_item)
  : Res.Input_message.content_item
  =
  match b.image_url, b.document_url with
  | Some { url }, _ ->
    let final = if b.is_local then Io.Base64.file_to_data_uri ~dir url else url in
    Image { image_url = final; detail = "auto"; _type = "input_image" }
  | _, Some doc ->
    let txt = get_content ~dir ~net doc b.is_local ~cleanup_html:b.cleanup_html in
    Text { text = txt; _type = "input_text" }
  | _ -> Text { text = Option.value ~default:"" b.text; _type = "input_text" }

and convert_content_item ~dir ~net ~cache (ci : CM.content_item)
  : Res.Input_message.content_item
  =
  match ci with
  | CM.Basic b -> convert_basic_item ~dir ~net ~cache b
  | CM.Agent ({ url; is_local; items } as agent) ->
    let txt =
      Agent_LRU.find_or_add cache agent ~ttl:Time_ns.Span.day ~default:(fun () ->
        let prompt = get_content ~dir ~net url is_local ~cleanup_html:false in
        run_agent ~dir ~net ~cache prompt items)
    in
    Text { text = txt; _type = "input_text" }

and convert_msg ~dir ~net ~cache (m : CM.msg) : Res.Item.t =
  let role =
    match String.lowercase m.role with
    | "assistant" -> `Assistant
    | "user" -> `User
    | "system" -> `System
    | "developer" -> `Developer
    | "tool" -> `Tool
    | other -> failwithf "unknown role %s" other ()
  in
  match role with
  | `Assistant ->
    let text =
      match m.content with
      | None -> ""
      | Some (CM.Text t) -> t
      | Some (CM.Items items) -> string_of_items ~dir ~net ~cache items
    in
    Res.Item.Output_message
      { role = Assistant
      ; id = Option.value ~default:"" m.id
      ; status = Option.value m.status ~default:"completed"
      ; _type = "message"
      ; content = [ { annotations = []; text; _type = "output_text" } ]
      }
  | `Tool ->
    let tool_call_id = Option.value_exn m.tool_call_id in
    (match m.content with
     | Some (CM.Text t) ->
       (* if tool_call is present, then this is the function_call requested by model *)
       (match m.tool_call with
        | Some { id; function_ = { name; arguments } } ->
          Res.Item.Function_call
            { name
            ; arguments
            ; call_id = id
            ; _type = "function_call"
            ; id = m.id
            ; status = None
            }
        | None ->
          (* if tool_call is not present, then this is the function_call_output *)
          (* that was returned by the function *)
          Res.Item.Function_call_output
            { call_id = tool_call_id
            ; _type = "function_call_output"
            ; id = None
            ; status = None
            ; output = t
            })
     | Some (CM.Items items) ->
       (* if tool_call is present, then this is the function_call requested by model *)
       (match m.tool_call with
        | Some { id; function_ = { name; _ } } ->
          Res.Item.Function_call
            { name
            ; arguments = string_of_items ~dir ~net ~cache items
            ; call_id = id
            ; _type = "function_call"
            ; id = m.id
            ; status = None
            }
        | None ->
          (* if tool_call is not present, then this is the function_call_output *)
          (* that was returned by the function *)
          Res.Item.Function_call_output
            { call_id = tool_call_id
            ; _type = "function_call_output"
            ; id = None
            ; status = None
            ; output = string_of_items ~dir ~net ~cache items
            })
     | _ ->
       failwith
         "Expected function_call to be raw text arguments; found structured content.")
  | (`User | `System | `Developer) as r ->
    let role_val =
      match r with
      | `User -> Res.Input_message.User
      | `System -> Res.Input_message.System
      | `Developer -> Res.Input_message.Developer
    in
    let content_items =
      match m.content with
      | None -> []
      | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "input_text" } ]
      | Some (CM.Items lst) -> List.map lst ~f:(convert_content_item ~dir ~net ~cache)
    in
    Res.Item.Input_message { role = role_val; content = content_items; _type = "message" }

and convert_reasoning (r : CM.reasoning) : Res.Item.t =
  let summ =
    List.map r.summary ~f:(fun s -> { Res.Reasoning.text = s.text; _type = s._type })
  in
  Res.Item.Reasoning { id = r.id; status = r.status; _type = "reasoning"; summary = summ }

and elements_to_items ~dir ~net ~cache (els : CM.top_level_elements list)
  : Res.Item.t list
  =
  List.filter_map els ~f:(function
    | CM.Msg m -> Some (convert_msg ~dir ~net ~cache m)
    | CM.Reasoning r -> Some (convert_reasoning r)
    | CM.Config _ -> None
    | CM.Tool _ -> None)

(*──────────────────────── 4.  Tools conversion  ─────────────────────────*)
and convert_tools (ts : Openai.Completions.tool list) : Res.Request.Tool.t list =
  List.map ts ~f:(fun { type_; function_ = { name; description; parameters; strict } } ->
    Res.Request.Tool.Function { name; description; parameters; strict; type_ })

(*──────────────────────── 5.  Agent recursion using Responses API ───────*)
and run_agent ~dir ~net ~cache (prompt_xml : string) (items : CM.content_item list)
  : string
  =
  let user_suffix =
    if List.is_empty items
    then ""
    else "<msg role=\"user\">\n" ^ string_of_items ~dir ~net ~cache items ^ "\n</msg>"
  in
  let full_xml = prompt_xml ^ "\n" ^ user_suffix in
  let parsed = CM.parse_chat_inputs ~dir full_xml in
  let req_items = elements_to_items ~dir ~net ~cache parsed in
  let resp = Res.post_response Res.Default ~dir net ~inputs:req_items in
  resp.output
  |> List.filter_map ~f:(function
    | Res.Item.Output_message o ->
      Some (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" ")
    | _ -> None)
  |> String.concat ~sep:"\n"
;;

(*──────────────────────── 6.  Main driver  ───────────────────────────────*)
let rec execute_response_loop
          ~dir
          ~net
          ~cache
          ?temperature
          ?max_output_tokens
          ?tools
          ?reasoning
          ~model
          ~tool_tbl
          (history : Res.Item.t list)
  : Res.Item.t list
  =
  let response =
    Res.post_response
      Res.Default
      ~dir
      ~model
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      net
      ~inputs:history
  in
  let new_items = response.output in
  let function_calls =
    List.filter_map new_items ~f:(function
      | Res.Item.Function_call fc -> Some fc
      | _ -> None)
  in
  if List.is_empty function_calls
  then history @ new_items
  else (
    (* run every function, create corresponding Function_call_output items *)

    (* adapt to your function set *)
    let outputs =
      List.map function_calls ~f:(fun fc ->
        let f = Hashtbl.find_exn tool_tbl fc.name in
        let res = f fc.arguments in
        Res.Item.Function_call_output
          { output = res
          ; call_id = fc.call_id
          ; _type = "function_call_output"
          ; id = None
          ; status = None
          })
    in
    execute_response_loop
      ~dir
      ~net
      ~cache
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      ~model
      ~tool_tbl
      (history @ new_items @ outputs))
;;

(*──────────────────────── 7.  Public helper  ─────────────────────────────*)
let run_completion
      ~env
      ?prompt_file (* optional template to prepend *)
      ~output_file (* evolving conversation buffer *)
      ()
  =
  let dir = Eio.Stdenv.fs env in
  let cwd = Eio.Stdenv.cwd env in
  let datadir = Eio.Path.(cwd / ".chatmd") in
  (match Io.is_dir ~dir:cwd ".chatmd" with
   | true -> ()
   | false -> Io.mkdir ~exists_ok:true ~dir:cwd ".chatmd");
  let net = env#net in
  let cache =
    if Eio.Path.is_file Eio.Path.(datadir / "cache.bin")
    then read_cache ~file:Eio.Path.(datadir / "cache.bin")
    else Agent_LRU.create ~max_size:1000 ()
  in
  (* 1 • append initial prompt file if provided *)
  Option.iter prompt_file ~f:(fun file ->
    Io.append_doc ~dir output_file (Io.load_doc ~dir file));
  (* 2 • main loop *)
  let rec loop () =
    let xml = Io.load_doc ~dir output_file in
    let elements = CM.parse_chat_inputs ~dir xml in
    (* gather config *)
    let cfg =
      List.filter_map elements ~f:(function
        | CM.Config c -> Some c
        | _ -> None)
      |> List.hd
    in
    let CM.
          { max_tokens = model_tokens
          ; model = model_opt
          ; reasoning_effort
          ; temperature
          ; _
          }
      =
      Option.value
        cfg
        ~default:
          { max_tokens = None
          ; model = None
          ; reasoning_effort = None
          ; temperature = None
          ; show_tool_call = false
          }
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
    let tools = convert_tools comp_tools in
    (* convert xml → items and fire first request *)
    let init_items = elements_to_items ~dir ~net ~cache elements in
    let all_items =
      execute_response_loop
        ~dir:datadir
        ~net
        ~cache
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
               (tab_on_newline
                  (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" "))
               "RAW")
        | Res.Item.Reasoning r ->
          let summaries =
            List.map r.summary ~f:(fun s ->
              Printf.sprintf "\n<summary>\n\t\t%s\n</summary>\n" (tab_on_newline s.text))
            |> String.concat ~sep:""
          in
          print_endline "summaries";
          print_endline summaries;
          append
            (Printf.sprintf
               "\n<reasoning id=\"%s\">\n%s\n</reasoning>\n"
               r.id
               (tab_on_newline summaries))
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
  write_cache ~file:Eio.Path.(datadir / "cache.bin") cache
;;

let custom_fn ~env CM.{ name; description; command } =
  let module M : Gpt_function.Def with type input = string list = struct
    type input = string list

    let name = name

    let description =
      match description with
      | Some desc ->
        Some
          (String.concat
             [ "Run a "
             ; command
             ; " shell command with arguments, and returns its output.\n"
             ; desc
             ])
      | None ->
        Some
          (String.concat
             [ "Run a "
             ; command
             ; " shell command with arguments, and returns its output"
             ])
    ;;

    let parameters : Jsonaf.t =
      `Object
        [ "type", `String "object"
        ; ( "properties"
          , `Object
              [ ( "arguments"
                , `Object
                    [ "type", `String "array"
                    ; "items", `Object [ "type", `String "string" ]
                    ] )
              ] )
        ; "required", `Array [ `String "arguments" ]
        ; "additionalProperties", `False
        ]
    ;;

    let input_of_string s =
      let j = Jsonaf.of_string s in
      List.map ~f:Jsonaf.string_exn @@ Jsonaf.list_exn @@ Jsonaf.member_exn "arguments" j
    ;;
  end
  in
  let gpt_fun : Gpt_function.t =
    let fp params =
      let proc_mgr = Eio.Stdenv.process_mgr env in
      Eio.Switch.run
      @@ fun sw ->
      (* 1.  Make a pipe that we can read. *)
      let r, w = Eio.Process.pipe ~sw proc_mgr in
      (* 2.  Start the child, sending both stdout and stderr to [w]. *)
      let _child =
        Eio.Process.spawn ~sw proc_mgr ~stdout:w ~stderr:w (command :: params)
      in
      Eio.Flow.close w;
      (* nobody else will write now *)
      match Eio.Buf_read.parse_exn ~max_size:1_000_000 Eio.Buf_read.take_all r with
      | res -> res
      | exception ex -> Fmt.str "error running %s command: %a" command Eio.Exn.pp ex
    in
    Gpt_function.create_function (module M) fp
  in
  gpt_fun
;;

(*──────────────────────── helper: agent tool → Gpt_function.t ──────────*)
let agent_fn ~dir ~net ~cache CM.{ name; description; agent; is_local } : Gpt_function.t =
  (* Module describing the function interface presented to the model. The
     agent tool simply expects an object with a single `input` string that
     will become the user message for the sub-agent. *)
  let module M : Gpt_function.Def with type input = string = struct
    type input = string

    let name = name

    let description =
      Option.first_some
        description
        (Some
           (Printf.sprintf
              "Run agent prompt located at %s and return its final answer."
              agent))
    ;;

    let parameters : Jsonaf.t =
      `Object
        [ "type", `String "object"
        ; "properties", `Object [ "input", `Object [ "type", `String "string" ] ]
        ; "required", `Array [ `String "input" ]
        ; "additionalProperties", `False
        ]
    ;;

    let input_of_string s =
      let j = Jsonaf.of_string s in
      match Jsonaf.member_exn "input" j with
      | `String str -> str
      | _ -> failwith "Expected {\"input\": string} for agent tool input"
    ;;
  end
  in
  let run (user_msg : string) : string =
    let basic_item : CM.basic_content_item =
      { type_ = "text"
      ; text = Some user_msg
      ; image_url = None
      ; document_url = None
      ; is_local = false
      ; cleanup_html = false
      }
    in
    let prompt_xml =
      (* Re-use helper from above to fetch either local or remote prompt *)
      let fetch =
        (* get_content requires cleanup_html flag, which we set to false since
           we want raw chatmd. *)
        get_content ~dir ~net agent is_local ~cleanup_html:false
      in
      fetch
    in
    run_agent ~dir ~net ~cache prompt_xml [ CM.Basic basic_item ]
  in
  Gpt_function.create_function (module M) run
;;

let run_completion_stream
      ~env
      ?prompt_file (* optional template to prepend once          *)
      ~output_file (* evolving conversation buffer               *)
      ()
  =
  (* ─────────────────────── 0.  setup & helpers ───────────────────────── *)
  let dir = Eio.Stdenv.fs env in
  let cwd = Eio.Stdenv.cwd env in
  let datadir = Eio.Path.(cwd / ".chatmd") in
  (match Io.is_dir ~dir:cwd ".chatmd" with
   | true -> ()
   | false -> Io.mkdir ~exists_ok:true ~dir:cwd ".chatmd");
  let net = env#net in
  let cache =
    if Eio.Path.is_file Eio.Path.(datadir / "cache.bin")
    then read_cache ~file:Eio.Path.(datadir / "cache.bin")
    else Agent_LRU.create ~max_size:1_000 ()
  in
  let append_doc = Io.append_doc ~dir output_file in
  Option.iter prompt_file ~f:(fun file -> append_doc (Io.load_doc ~dir file));
  (* Pretty logger: every event – even if we do not act on it *)
  let log_event ev =
    print_endline "STREAM EVENT:";
    print_endline (Jsonaf.to_string_hum (Res.Response_stream.jsonaf_of_t ev))
  in
  (* ─────────────────────── 1.  main recursive turn ────────────────────── *)
  let rec turn () =
    (* 1‑A • read current prompt XML and parse *)
    let xml = Io.load_doc ~dir output_file in
    let elements = CM.parse_chat_inputs ~dir xml in
    (* 1‑B • current config (max_tokens, model, …) *)
    let cfg =
      List.filter_map elements ~f:(function
        | CM.Config c -> Some c
        | _ -> None)
      |> List.hd
    in
    let CM.{ max_tokens; model; reasoning_effort; temperature; show_tool_call } =
      Option.value
        cfg
        ~default:
          { max_tokens = None
          ; model = None
          ; reasoning_effort = None
          ; temperature = None
          ; show_tool_call = false
          }
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
      |> List.map ~f:(function
        | CM.Builtin name ->
          (match name with
           | "apply_patch" -> Functions.apply_patch ~dir:(Eio.Stdenv.cwd env)
           | "read_dir" -> Functions.read_dir ~dir:(Eio.Stdenv.cwd env)
           | "get_contents" -> Functions.get_contents ~dir:(Eio.Stdenv.cwd env)
           | _ -> failwithf "Unknown built-in tool: %s" name ())
        | CM.Custom c -> custom_fn ~env c
        | CM.Agent a -> agent_fn ~dir:(Eio.Stdenv.cwd env) ~net ~cache a)
    in
    (* 1‑C • tools / functions *)
    let comp_tools, tool_tbl = Gpt_function.functions tools in
    let tools = convert_tools comp_tools in
    (* 1‑D • initial request items *)
    let inputs = elements_to_items ~dir ~net ~cache elements in
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
      append_doc (tab_on_newline txt)
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
        let tool_call_url id = Printf.sprintf "tool-call.%s.json" id in
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
               (tab_on_newline arguments)
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
               (tab_on_newline content));
          (* save the tool call to a file *)
          Io.save_doc ~dir:datadir (tool_call_url call_id) arguments);
        (* run the tool *)
        let fn = Hashtbl.find_exn tool_tbl name in
        let result = fn arguments in
        let tool_call_result_url id = Printf.sprintf "tool-call-result.%s.json" id in
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
               (tab_on_newline result)
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
               (tab_on_newline content));
          Io.save_doc ~dir:datadir (tool_call_result_url call_id) result);
        run_again := true
    in
    let callback (ev : Res.Response_stream.t) =
      log_event ev;
      match ev with
      (* ─────────────────────────── assistant text ─────────────────────── *)
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
      (* ─────────────────────────── reasoning deltas ───────────────────── *)
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
        append_doc (tab_on_newline delta)
      (* ─────────────────────────── function calls etc. ────────────────── *)
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
      | _ -> ()
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
  write_cache ~file:Eio.Path.(datadir / "cache.bin") cache
;;
