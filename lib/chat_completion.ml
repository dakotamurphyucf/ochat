open Core

module Agent_key = struct
  type t = Prompt.Chat_markdown.agent_content [@@deriving sexp, bin_io, hash, compare]

  let invariant (_ : t) = ()
end

module Agent_res_LRU = Ttl_lru_cache.Make (Agent_key)

type persistent_form =
  { max_size : int
  ; items : (Prompt.Chat_markdown.agent_content * string Agent_res_LRU.entry) list
    (* in LRU order *)
  }
[@@deriving bin_io]

let to_persistent (t : string Agent_res_LRU.t) : persistent_form =
  { max_size = Agent_res_LRU.max_size t
  ; items = Agent_res_LRU.to_alist t (* least -> most recently used *)
  }
;;

(* Rebuild a new LRU in the original order. *)
let of_persistent (pf : persistent_form) : string Agent_res_LRU.t =
  let t = Agent_res_LRU.create ~max_size:pf.max_size () in
  (* Insert pairs from least- to most-recently used order. *)
  List.iter pf.items ~f:(fun (key, data) -> Agent_res_LRU.set t ~key ~data);
  t
;;

(* Cstruct.of_bigarray  to convet bigarray to cstruct and then we can use eio to write using Path.with_open_out to get flow then we can use Writer to write the cstruct *)
let write_file ~file cache =
  Bin_prot_utils_eio.write_bin_prot'
    file
    [%bin_writer: persistent_form]
    (to_persistent cache)
;;

let read_file ~file =
  of_persistent (Bin_prot_utils_eio.read_bin_prot' file [%bin_reader: persistent_form])
;;

let clean_html raw_html =
  let decompressed =
    Option.value ~default:raw_html @@ Result.ok (Ezgzip.decompress raw_html)
  in
  let soup = Soup.parse decompressed in
  String.concat ~sep:"\n"
  @@ List.filter ~f:(fun s -> not @@ String.equal "" s)
  @@ List.map ~f:(fun s -> String.strip s)
  @@ Soup.texts soup
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

let get_messages top_elements =
  List.filter_map top_elements ~f:(function
    | Prompt.Chat_markdown.Msg s -> Some s
    | _ -> None)
;;

let get_config top_elements =
  List.filter_map top_elements ~f:(function
    | Prompt.Chat_markdown.Config s -> Some s
    | _ -> None)
  |> List.hd_exn
;;

let get_content ~dir ~net url is_local =
  let host = Io.Net.get_host url in
  let path = Io.Net.get_path url in
  match is_local with
  | true -> Io.load_doc ~dir path
  | false ->
    let headers = Http.Header.of_list [ "Accept", "*/*"; "Accept-Encoding", "gzip" ] in
    let doc = Io.Net.get Io.Net.Default ~net ~host ~headers path in
    doc
;;

let rec get_user_msg ~dir ~net ~cache items =
  List.map
    ~f:(fun item ->
      match item with
      | Prompt.Chat_markdown.Basic item ->
        (match item.image_url with
         | Some url ->
           (match item.is_local with
            | true ->
              print_endline "local";
              let a = sprintf "<img src=\"%s\" %s />" url.url "local" in
              print_endline a;
              a
            | false ->
              print_endline "not local";
              sprintf "<img src=\"%s\" />" url.url)
         | None ->
           (match item.document_url with
            | Some url ->
              (match item.is_local with
               | true -> sprintf "<doc src=\"%s\" %s />" url "local"
               | false ->
                 (match item.cleanup_html with
                  | true -> sprintf "<doc src=\"%s\" %s />" url "strip"
                  | false -> sprintf "<doc src=\"%s\" />" url))
            | None -> Option.value ~default:"" item.text))
      | Agent ({ url; is_local; items } as agent) ->
        let a =
          Agent_res_LRU.find_or_add cache agent ~ttl:Time_ns.Span.day ~default:(fun () ->
            let prompt = get_content ~dir ~net url is_local in
            run_agent ~dir ~net ~cache prompt items)
        in
        a
      (* let prompt = get_content ~dir ~net url is_local in
        let contents = run_agent ~dir ~net prompt items in
        contents) *))
    items
  |> String.concat ~sep:"\n"

and convert ~dir ~net ~cache msg =
  let { Prompt.Chat_markdown.role
      ; content
      ; name
      ; function_call
      ; tool_call
      ; tool_call_id
      ; id = _
      ; status = _
      }
    =
    msg
  in
  let function_call =
    Option.map function_call ~f:(fun function_call ->
      { Openai.Completions.name = function_call.name
      ; arguments = function_call.arguments
      })
  in
  let tool_calls =
    Option.map tool_call ~f:(fun tool_call ->
      [ { Openai.Completions.id = Some tool_call.id
        ; function_ =
            Some
              { arguments = tool_call.function_.arguments
              ; name = tool_call.function_.name
              }
        ; type_ = Some "function"
        }
      ])
  in
  let content =
    Option.map content ~f:(fun s ->
      match s with
      | Prompt.Chat_markdown.Text t -> Openai.Completions.Text t
      | Items items ->
        Openai.Completions.Items
          (List.map items ~f:(fun item ->
             match item with
             | Basic item ->
               let image_url =
                 match item.image_url with
                 | Some url ->
                   (match item.is_local with
                    | true ->
                      Some
                        { Openai.Completions.url = Io.Base64.file_to_data_uri ~dir url.url
                        }
                    | false -> Some { Openai.Completions.url = url.url })
                 | None -> None
               in
               let text =
                 match item.document_url with
                 | Some url ->
                   (match item.is_local with
                    | true ->
                      let doc = Io.load_doc ~dir url in
                      Some doc
                    | false ->
                      let host = Io.Net.get_host url in
                      let path = Io.Net.get_path url in
                      (match item.cleanup_html with
                       | true ->
                         let headers =
                           Http.Header.of_list
                             [ "Accept", "*/*"; "Accept-Encoding", "gzip" ]
                         in
                         let doc = Io.Net.get Io.Net.Default ~net ~host ~headers path in
                         Some (clean_html doc)
                       | false ->
                         let doc = Io.Net.get Io.Net.Default ~net ~host path in
                         Some doc))
                 | None -> item.text
               in
               Openai.Completions.{ type_ = item.type_; text; image_url }
             | Agent ({ url; is_local; items } as agent) ->
               print_endline "agent hit rate";
               Agent_res_LRU.hit_rate cache |> Float.to_string_hum |> print_endline;
               let contents =
                 Agent_res_LRU.find_or_add
                   cache
                   agent
                   ~ttl:Time_ns.Span.day
                   ~default:(fun () ->
                     let prompt = get_content ~dir ~net url is_local in
                     run_agent ~dir ~net ~cache prompt items)
               in
               Agent_res_LRU.hit_rate cache |> Float.to_string_hum |> print_endline;
               Openai.Completions.
                 { type_ = "text"; text = Some contents; image_url = None })))
  in
  (* (match content with
       | None -> ()
       | Some content ->
         print_endline
         @@ Jsonaf.to_string_hum
         @@ Openai.jsonaf_of_chat_message_content content); *)
  (match tool_call_id with
   | None -> ()
   | Some tool_call_id -> print_endline tool_call_id);
  { Openai.Completions.role; content; name; function_call; tool_calls; tool_call_id }

and run_agent prompt items ~dir ~net ~cache =
  let funcs, tbl =
    (* add replace_in_file *)
    Gpt_function.functions
      [ (* Functions.create_file ~dir *)
        Functions.get_contents ~dir
        (* ; Functions.insert_code ~dir *)
        (* ; Functions.update_file_lines ~dir
           ; Functions.append_to_file ~dir *)
        (* ; Functions.summarize_file ~net:env#net ~dir *)
        (* ; Functions.generate_interface ~net:env#net ~dir
           (* ; Functions.generate_code_context_from_query ~net:env#net ~dir *)
           ; Functions.get_url_content ~net:env#net
           ; Functions.index_ocaml_code ~net:env#net ~dir ~dm
           ; Functions.query_vector_db ~net:env#net ~dir *)
      ]
  in
  let content =
    match items with
    | [] -> ""
    | items ->
      let c = get_user_msg ~dir ~net ~cache items in
      sprintf "<user>\n%s\n</user>" c
  in
  let prompt = prompt ^ "\n" ^ content in
  let elements = Prompt.Chat_markdown.parse_chat_inputs ~dir prompt in
  let config = get_config elements in
  let Prompt.Chat_markdown.{ max_tokens; model; reasoning_effort; temperature; _ } =
    config
  in
  (* print_endline
    @@ Jsonaf.to_string_hum
    @@ Prompt.Chat_markdown.jsonaf_of_config config; *)
  let model =
    match model with
    | Some m ->
      print_endline m;
      Openai.Completions.model_of_str_exn m
    | None -> Gpt4
  in
  let messages = get_messages @@ elements in
  List.iter messages ~f:(fun m ->
    print_endline @@ Jsonaf.to_string_hum @@ Prompt.Chat_markdown.jsonaf_of_msg m);
  let inputs = List.map ~f:(convert ~dir ~net ~cache) @@ get_messages @@ elements in
  List.iter inputs ~f:(fun i ->
    print_endline @@ Jsonaf.to_string_hum @@ Openai.Completions.jsonaf_of_chat_message i);
  let choice =
    Openai.Completions.post_chat_completion
      Openai.Completions.Default
      ?max_tokens
      ~dir
      net
      ~tools:funcs
      ~model
      ?reasoning_effort
      ?temperature
      ~inputs
  in
  match choice.message.tool_calls with
  | None -> choice.message.content |> Option.value ~default:""
  | Some calls ->
    let call = List.hd_exn calls in
    let id = Option.value_exn call.id in
    let func = Option.value_exn call.function_ in
    let name = func.name in
    let f = Hashtbl.find_exn tbl name in
    let res = f func.arguments in
    let call =
      sprintf
        "\n\
         <msg role=\"assistant\" tool_call tool_call_id=\"%s\" function_name=\"%s\">\n\
         <raw>\n\
         %s\n\
         </raw>\n\
         </msg>\n\n"
        id
        name
        func.arguments
    in
    let call_result =
      sprintf
        "\n<msg role=\"tool\" tool_call_id=\"%s\">\n<raw>\n%s\n</raw>\n</msg>\n\n"
        id
        res
    in
    run_agent (prompt ^ call ^ call_result) [] ~dir ~net ~cache
;;

let run_completion ~env ~output_file ~prompt_file =
  let dir = Eio.Stdenv.fs env in
  let cache =
    match Eio.Path.is_file Eio.Path.(dir / "./cache.bin") with
    | true -> read_file ~file:Eio.Path.(dir / "./cache.bin")
    | false -> Agent_res_LRU.create ~max_size:1000 ()
  in
  (* let dm = Eio.Stdenv.domain_mgr env in *)
  let append_doc s = Io.append_doc ~dir:(Eio.Stdenv.fs env) output_file s in
  let () =
    match prompt_file with
    | Some file ->
      let p = Io.load_doc ~dir:(Eio.Stdenv.fs env) file in
      append_doc p
    | None -> ()
  in
  let rec start () =
    let prompt = Io.load_doc ~dir:(Eio.Stdenv.fs env) output_file in
    let run = ref false in
    (* append_doc "\n<msg role=\"assistant\">\n"; *)
    let content = Option.value ~default:"" in
    let _funcs, tbl =
      (* add replace_in_file *)
      Gpt_function.functions
        [ (* Functions.create_file ~dir *)
          Functions.get_contents ~dir
          (* ; Functions.insert_code ~dir *)
          (* ; Functions.update_file_lines ~dir
             ; Functions.append_to_file ~dir *)
          (* ; Functions.summarize_file ~net:env#net ~dir *)
          (* ; Functions.generate_interface ~net:env#net ~dir
             (* ; Functions.generate_code_context_from_query ~net:env#net ~dir *)
             ; Functions.get_url_content ~net:env#net
             ; Functions.index_ocaml_code ~net:env#net ~dir ~dm
             ; Functions.query_vector_db ~net:env#net ~dir *)
        ]
    in
    (* so args needs to be a table with id -> args, toolid use for getting current tool and knowinf when to switch, func name same as args *)
    let f () =
      let contents = ref false in
      let function_call = ref false in
      let func_name = Hashtbl.create (module String) in
      let tool_id = ref "" in
      let args = Hashtbl.create (module String) in
      fun choice ->
        match choice.Openai.Completions.delta.role with
        | Some _ | None ->
          if
            String.length (choice.delta.content |> content) > 0
            && (not !contents)
            && not !function_call
          then (
            append_doc "\n<msg role=\"assistant\">\n\t<raw>\n\t\t";
            contents := true);
          (match choice.delta.tool_calls with
           | Some (call :: _) ->
             if not @@ String.is_empty call.id
             then (
               function_call := true;
               Hashtbl.add_exn func_name ~key:call.id ~data:call.function_.name;
               tool_id := call.id);
             Hashtbl.update args !tool_id ~f:(function
               | None -> call.function_.arguments
               | Some a -> String.concat [ a; call.function_.arguments ])
           | Some [] -> ()
           | None ->
             if String.length (choice.delta.content |> content) > 0
             then append_doc @@ tab_on_newline (choice.delta.content |> content));
          (match choice.finish_reason with
           | Some _ ->
             if !function_call
             then (
               (* add assistant msg with function_call function_name function_arguments*)
               Hashtbl.iteri args ~f:(fun ~key ~data ->
                 print_endline key;
                 (* call function and add results to doc in funtion role type with name*)
                 let name = Hashtbl.find_exn func_name key in
                 let f = Hashtbl.find_exn tbl name in
                 let res = f data in
                 append_doc
                   (sprintf
                      "\n\
                       <msg role=\"assistant\" tool_call tool_call_id=\"%s\" \
                       function_name=\"%s\">\n\
                       <raw>\n"
                      key
                      name);
                 append_doc data;
                 append_doc "\n</raw>\n</msg>\n\n";
                 append_doc
                 @@ sprintf
                      "\n\
                       <msg role=\"tool\" tool_call_id=\"%s\">\n\
                       <raw>\n\
                       %s\n\
                       </raw>\n\
                       </msg>\n\n"
                      key
                      res);
               (* this is a function_call so we need to run again with value *)
               run := true)
             else (
               append_doc "\n\t</raw>\n</msg>\n";
               append_doc "\n<msg role=\"user\">\n\n</msg>")
           | None -> ())
    in
    (* print_endline "prompt";
       print_endline prompt; *)
    let elements = Prompt.Chat_markdown.parse_chat_inputs ~dir prompt in
    let config = get_config elements in
    let Prompt.Chat_markdown.{ max_tokens; model; reasoning_effort; temperature; _ } =
      config
    in
    (* print_endline
    @@ Jsonaf.to_string_hum
    @@ Prompt.Chat_markdown.jsonaf_of_config config; *)
    let model =
      match model with
      | Some m ->
        print_endline m;
        Openai.Completions.model_of_str_exn m
      | None -> Gpt4
    in
    let messages = get_messages @@ elements in
    print_endline "messages";
    List.iter messages ~f:(fun m ->
      print_endline @@ Jsonaf.to_string_hum @@ Prompt.Chat_markdown.jsonaf_of_msg m);
    let text =
      {|
    Before performing any tool calls ask the user for permission.
    |}
    in
    let func_tool_system_msg =
      { Openai.Completions.role = "developer"
      ; content = Some (Openai.Completions.Text text)
      ; name = None
      ; function_call = None
      ; tool_calls = None
      ; tool_call_id = None
      }
    in
    let inputs =
      func_tool_system_msg
      :: (List.map ~f:(convert ~dir ~net:env#net ~cache) @@ get_messages @@ elements)
    in
    print_endline "inputs";
    List.iter inputs ~f:(fun i ->
      print_endline @@ Jsonaf.to_string_hum @@ Openai.Completions.jsonaf_of_chat_message i);
    Openai.Completions.post_chat_completion
      (Openai.Completions.Stream (f ()))
      ?max_tokens
      ~dir
      env#net (* ~tools:funcs *)
      ~model
      ?reasoning_effort
      ?temperature
      ~inputs;
    if !run then start () else ()
  in
  start ();
  let cwd = Eio.Stdenv.cwd env in
  let datadir = Io.ensure_chatmd_dir ~cwd in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  write_file ~file:cache_file cache
;;
