open Core
module CM = Prompt_template.Chat_markdown
module Res = Openai.Responses

(*──────────────────────── 0.  Agent cache  ─────────────────────────────*)

module Cache = struct
  (*--- 0-a. Key + underlying LRU implementation -----------------------*)
  module Key = struct
    type t = CM.agent_content [@@deriving sexp, bin_io, hash, compare]

    let invariant (_ : t) = ()
  end

  module LRU = Ttl_lru_cache.Make (Key)

  (* Abstract type exposed to callers.  Values are backed by a TTL-LRU
     storing the textual response of an agent prompt keyed by the
     agent specification (its url + inline user items). *)
  type t = string LRU.t

  (*--- 0-b. Persistence helpers ---------------------------------------*)
  type persistent_form =
    { max_size : int
    ; items : (Key.t * string LRU.entry) list
    }
  [@@deriving bin_io]

  let create ~max_size () = LRU.create ~max_size ()
  let to_persistent lru = { max_size = LRU.max_size lru; items = LRU.to_alist lru }

  let of_persistent pf =
    let cache = create ~max_size:pf.max_size () in
    List.iter pf.items ~f:(fun (k, v) -> LRU.set cache ~key:k ~data:v);
    cache
  ;;

  (* Binary serialisation on disk – deterministic and compact. *)
  let write_file ~file cache =
    Bin_prot_utils_eio.write_bin_prot'
      file
      [%bin_writer: persistent_form]
      (to_persistent cache)
  ;;

  let read_file ~file =
    of_persistent (Bin_prot_utils_eio.read_bin_prot' file [%bin_reader: persistent_form])
  ;;

  (*--- 0-c. Public API --------------------------------------------------*)
  let find_or_add t key ~ttl ~default = LRU.find_or_add t key ~ttl ~default

  (* Convenience wrappers for loading / saving the cache on disk that are
     aware of a fallback [~max_size] when no cache file is present.  This
     removes boiler-plate from callers such as the driver functions. *)

  let load ~file ~max_size () =
    if Eio.Path.is_file file then read_file ~file else create ~max_size ()
  ;;

  let save ~file t = write_file ~file t
end

(*──────────────────────── 1.  Shared runtime context  ──────────────────*)

module Ctx = struct
  (* We keep the type abstract enough so that we don't have to pin the
     exact polymorphic object type exposed by [Eio.Stdenv.t].  The few
     things we require for now are [#net] and that we can recover a
     filesystem root from it. *)

  type 'env t =
    { env : 'env
    ; dir : Eio.Fs.dir_ty Eio.Path.t
    ; cache : Cache.t
    }

  let create ~env ~dir ~cache = { env; dir; cache }

  (* Convenience constructor when [dir] is the process' filesystem root. *)
  let of_env ~env ~cache = { env; dir = Eio.Stdenv.fs env; cache }
  let net t = t.env#net
  let env t = t.env
  let dir t = t.dir
  let cache t = t.cache
end

(*──────────────────────── 2.5.  Config helper  ─────────────────────────*)

module Config = struct
  (*********************************************************************
     Small utility around the <config/> element that appears at most
     once in a ChatMarkdown document.  Most call-sites only need the
     *first* occurrence or a sensible default when the element is
     missing, leading to the same boiler-plate in three different
     places.

     We expose a single helper [of_elements] that collapses that logic
     into one reusable function.
  *********************************************************************)

  type t = CM.config

  let default : t =
    { max_tokens = None
    ; model = None
    ; reasoning_effort = None
    ; temperature = None
    ; show_tool_call = false
    ; id = None
    }
  ;;

  (* [of_elements els] returns either the first <config/> element found
     in [els] or a default record when none is present. *)
  let of_elements (els : CM.top_level_elements list) : t =
    List.find_map els ~f:(function
      | CM.Config c -> Some c
      | _ -> None)
    |> Option.value ~default
  ;;
end

(*──────────────────────── 2.  Helpers ───────────────────────────────────*)
(*──────────────────────── 2.  Fetch helpers  ───────────────────────────*)

module Fetch = struct
  (* Internal helpers --------------------------------------------------- *)
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

  (* Shared implementation --------------------------------------------- *)
  let get_impl ~(ctx : _ Ctx.t) url ~is_local ~cleanup_html =
    if is_local
    then Io.load_doc ~dir:(Ctx.dir ctx) url
    else (
      let net = Ctx.net ctx in
      let raw = get_remote ~net url in
      if cleanup_html then clean_html raw else raw)
  ;;

  (* Public helpers ----------------------------------------------------- *)
  let get ~ctx url ~is_local = get_impl ~ctx url ~is_local ~cleanup_html:false
  let get_html ~ctx url ~is_local = get_impl ~ctx url ~is_local ~cleanup_html:true
end

(*──────────────────────── 4.  Response loop  ───────────────────────────*)

module Response_loop = struct
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

  (* [run] defined above; legacy alias kept for a transition period. *)
  (* Deprecated: kept for transitional compatibility; will be removed in future *)
  (* let execute_response_loop = run *)
end

(*──────────────────────── 3.  Converter  ───────────────────────────────*)

module Converter = struct
  (*********************************************************************
     Conversion of parsed ChatMarkdown (CM) structures into the          
     OpenAI Responses (Res) typed representation.  All helpers share a   
     single immutable context record [Ctx.t] so that long parameter      
     lists disappear, and accept an explicit [~run_agent] callback to    
     resolve nested <agent/> inclusions without creating a circular      
     compile-time dependency between this module and the [run_agent]     
     function that in turn relies on [Converter].                        
  *********************************************************************)

  type 'env ctx = 'env Ctx.t

  (* Forward declarations *)

  let rec string_of_items ~ctx ~run_agent (items : CM.content_item list) : string =
    let cache = Ctx.cache ctx in
    items
    |> List.map ~f:(function
      | CM.Basic b ->
        (match b.image_url, b.document_url with
         | Some { url }, _ ->
           if b.is_local
           then Printf.sprintf "<img src=\"%s\" local=\"true\"/>" url
           else Printf.sprintf "<img src=\"%s\"/>" url
         | _, Some doc_url ->
           if b.cleanup_html
           then Fetch.get_html ~ctx doc_url ~is_local:b.is_local
           else Fetch.get ~ctx doc_url ~is_local:b.is_local
         | _, _ -> Option.value ~default:"" b.text)
      | CM.Agent ({ url; is_local; items } as agent) ->
        Cache.find_or_add cache agent ~ttl:Time_ns.Span.day ~default:(fun () ->
          let prompt = Fetch.get ~ctx url ~is_local in
          (* delegate to the shared agent runner *)
          run_agent ~ctx prompt items))
    |> String.concat ~sep:"\n"

  (** Convert a basic_content_item to an OpenAI input content item. *)
  and convert_basic_item ~ctx (b : CM.basic_content_item) : Res.Input_message.content_item
    =
    let dir = Ctx.dir ctx in
    match b.image_url, b.document_url with
    | Some { url }, _ ->
      let final = if b.is_local then Io.Base64.file_to_data_uri ~dir url else url in
      Image { image_url = final; detail = "auto"; _type = "input_image" }
    | _, Some doc ->
      let txt =
        if b.cleanup_html
        then Fetch.get_html ~ctx doc ~is_local:b.is_local
        else Fetch.get ~ctx doc ~is_local:b.is_local
      in
      Text { text = txt; _type = "input_text" }
    | _ -> Text { text = Option.value ~default:"" b.text; _type = "input_text" }

  and convert_content_item ~ctx ~run_agent (ci : CM.content_item)
    : Res.Input_message.content_item
    =
    let cache = Ctx.cache ctx in
    match ci with
    | CM.Basic b -> convert_basic_item ~ctx b
    | CM.Agent ({ url; is_local; items } as agent) ->
      let txt =
        Cache.find_or_add cache agent ~ttl:Time_ns.Span.day ~default:(fun () ->
          let prompt = Fetch.get ~ctx url ~is_local in
          run_agent ~ctx prompt items)
      in
      Text { text = txt; _type = "input_text" }

  and convert_msg ~ctx ~run_agent (m : CM.msg) : Res.Item.t =
    let _ = Ctx.cache ctx in
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
        | Some (CM.Items items) -> string_of_items ~ctx ~run_agent items
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
         (* function_call vs function_call_output discrimination *)
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
            Res.Item.Function_call_output
              { call_id = tool_call_id
              ; _type = "function_call_output"
              ; id = None
              ; status = None
              ; output = t
              })
       | Some (CM.Items items) ->
         (match m.tool_call with
          | Some { id; function_ = { name; _ } } ->
            Res.Item.Function_call
              { name
              ; arguments = string_of_items ~ctx ~run_agent items
              ; call_id = id
              ; _type = "function_call"
              ; id = m.id
              ; status = None
              }
          | None ->
            Res.Item.Function_call_output
              { call_id = tool_call_id
              ; _type = "function_call_output"
              ; id = None
              ; status = None
              ; output = string_of_items ~ctx ~run_agent items
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
        | Some (CM.Text t) ->
          [ Res.Input_message.Text { text = t; _type = "input_text" } ]
        | Some (CM.Items lst) -> List.map lst ~f:(convert_content_item ~ctx ~run_agent)
      in
      Res.Item.Input_message
        { role = role_val; content = content_items; _type = "message" }

  and convert_reasoning (r : CM.reasoning) : Res.Item.t =
    let summ =
      List.map r.summary ~f:(fun s -> { Res.Reasoning.text = s.text; _type = s._type })
    in
    Res.Item.Reasoning
      { id = r.id; status = r.status; _type = "reasoning"; summary = summ }
  ;;

  let to_items ~ctx ~run_agent (els : CM.top_level_elements list) : Res.Item.t list =
    List.filter_map els ~f:(function
      | CM.Msg m -> Some (convert_msg ~ctx ~run_agent m)
      | CM.Reasoning r -> Some (convert_reasoning r)
      | CM.Config _ -> None
      | CM.Tool _ -> None)
  ;;
end

(*──────────────────────── 4.  Tools helper module  ─────────────────────*)

module Tool = struct
  (*********************************************************************
     Helpers for tool creation and conversion.

     This gathers the previously scattered [convert_tools], [custom_fn]
     and [agent_fn] helpers into one cohesive namespace.  The module is
     intentionally kept local to this file for now; future work will
     move it to its own compilation unit.
  *********************************************************************)

  (*--- 4-a.  OpenAI → Responses tool conversion ----------------------*)

  let convert_tools (ts : Openai.Completions.tool list) : Res.Request.Tool.t list =
    List.map
      ts
      ~f:(fun { type_; function_ = { name; description; parameters; strict } } ->
        Res.Request.Tool.Function { name; description; parameters; strict; type_ })
  ;;

  (*--- 4-b.  Custom shell command tool --------------------------------*)

  let custom_fn ~env (c : CM.custom_tool) : Gpt_function.t =
    let CM.{ name; description; command } = c in
    let module M : Gpt_function.Def with type input = string list = struct
      type input = string list

      let name = name

      let description : string option =
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

      let input_of_string s : input =
        let j = Jsonaf.of_string s in
        j
        |> Jsonaf.member_exn "arguments"
        |> Jsonaf.list_exn
        |> List.map ~f:Jsonaf.string_exn
      ;;
    end
    in
    let fp (params : string list) : string =
      let proc_mgr = Eio.Stdenv.process_mgr env in
      Eio.Switch.run
      @@ fun sw ->
      (* 1.  Pipe for capturing stdout & stderr. *)
      let r, w = Eio.Process.pipe ~sw proc_mgr in
      match Eio.Process.spawn ~sw proc_mgr ~stdout:w ~stderr:w (command :: params) with
      | exception ex ->
        let err_msg = Fmt.str "error running %s command: %a" command Eio.Exn.pp ex in
        Eio.Flow.close w;
        err_msg
      | _child ->
        Eio.Flow.close w;
        (match Eio.Buf_read.parse_exn ~max_size:1_000_000 Eio.Buf_read.take_all r with
         | res -> res
         | exception ex -> Fmt.str "error running %s command: %a" command Eio.Exn.pp ex)
    in
    Gpt_function.create_function (module M) fp
  ;;

  (*--- 4-c.  Agent tool → Gpt_function.t ------------------------------*)

  let agent_fn ~(ctx : _ Ctx.t) ~run_agent (agent_spec : CM.agent_tool) : Gpt_function.t =
    let CM.{ name; description; agent; is_local } = agent_spec in
    (* pull components from the shared context *)
    let _net_unused = Ctx.net ctx in
    (* Interface definition for the agent tool – expects an object with a
       single string field "input". *)
    let module M : Gpt_function.Def with type input = string = struct
      type input = string

      let name = name

      let description : string option =
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

      let input_of_string s : input =
        match Jsonaf.(of_string s |> member_exn "input") with
        | `String str -> str
        | _ -> failwith "Expected {\"input\": string} for agent tool input"
      ;;
    end
    in
    let run (user_msg : string) : string =
      (* Build a basic content item from the provided user input. *)
      let basic_item : CM.basic_content_item =
        { type_ = "text"
        ; text = Some user_msg
        ; image_url = None
        ; document_url = None
        ; is_local = false
        ; cleanup_html = false
        }
      in
      (* Fetch the agent prompt (local or remote) *)
      let prompt_xml = Fetch.get ~ctx agent ~is_local in
      (* Delegate the heavy lifting to the provided [run_agent] callback. *)
      run_agent ~ctx prompt_xml [ CM.Basic basic_item ]
    in
    Gpt_function.create_function (module M) run
  ;;

  (*--- 4-d.  Unified declaration → function mapping ------------------*)

  let of_declaration ~(ctx : _ Ctx.t) ~run_agent (decl : CM.tool) : Gpt_function.t =
    match decl with
    | CM.Builtin name ->
      (match name with
       | "apply_patch" -> Functions.apply_patch ~dir:(Ctx.dir ctx)
       | "read_dir" -> Functions.read_dir ~dir:(Ctx.dir ctx)
       | "get_contents" -> Functions.get_contents ~dir:(Ctx.dir ctx)
       | other -> failwithf "Unknown built-in tool: %s" other ())
    | CM.Custom c -> custom_fn ~env:(Ctx.env ctx) c
    | CM.Agent agent_spec -> agent_fn ~ctx ~run_agent agent_spec
  ;;
end

(*──────────────────────── 4.  Tools conversion  ─────────────────────────*)
(*──────────────────────── 5.  Agent recursion using Responses API ───────*)
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
         reuse [ctx] directly. *)
      Tool.of_declaration ~ctx ~run_agent decl)
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
  let log_event ev =
    print_endline "STREAM EVENT:";
    print_endline (Jsonaf.to_string_hum (Res.Response_stream.jsonaf_of_t ev))
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
          | _ -> Ctx.create ~env ~dir:(Eio.Stdenv.fs env) ~cache
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
        append_doc (Fetch.tab_on_newline delta)
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
  Cache.save ~file:cache_file cache
;;
