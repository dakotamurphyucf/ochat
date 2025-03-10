open Core

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
    | Prompt_template.Chat_markdown.Msg s -> Some s
    | _ -> None)
;;

let get_config top_elements =
  List.filter_map top_elements ~f:(function
    | Prompt_template.Chat_markdown.Config s -> Some s
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

let rec get_user_msg ~dir ~net items =
  List.map
    ~f:(fun item ->
      match item with
      | Prompt_template.Chat_markdown.Basic item ->
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
      | Agent { url; is_local; items } ->
        let prompt = get_content ~dir ~net url is_local in
        let contents = run_agent ~dir ~net prompt items in
        contents)
    items
  |> String.concat ~sep:"\n"

and convert ~dir ~net msg =
  let { Prompt_template.Chat_markdown.role
      ; content
      ; name
      ; function_call
      ; tool_call
      ; tool_call_id
      }
    =
    msg
  in
  let function_call =
    Option.map function_call ~f:(fun function_call ->
      { Openai.name = function_call.name; arguments = function_call.arguments })
  in
  let tool_calls =
    Option.map tool_call ~f:(fun tool_call ->
      [ { Openai.id = Some tool_call.id
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
      | Prompt_template.Chat_markdown.Text t -> Openai.Text t
      | Items items ->
        Openai.Items
          (List.map items ~f:(fun item ->
             match item with
             | Basic item ->
               let image_url =
                 match item.image_url with
                 | Some url ->
                   (match item.is_local with
                    | true ->
                      Some { Openai.url = Io.Base64.file_to_data_uri ~dir url.url }
                    | false -> Some { Openai.url = url.url })
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
               Openai.{ type_ = item.type_; text; image_url }
             | Agent { url; is_local; items } ->
               let prompt = get_content ~dir ~net url is_local in
               let contents = run_agent ~dir ~net prompt items in
               Openai.{ type_ = "text"; text = Some contents; image_url = None })))
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
  { Openai.role; content; name; function_call; tool_calls; tool_call_id }

and run_agent prompt items ~dir ~net =
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
      let c = get_user_msg ~dir ~net items in
      sprintf "<msg role=\"user\">\n%s\n</msg>" c
  in
  let prompt = prompt ^ "\n" ^ content in
  let elements = Prompt_template.Chat_markdown.parse_chat_inputs ~dir prompt in
  let config = get_config elements in
  let Prompt_template.Chat_markdown.{ max_tokens; model; reasoning_effort; temperature } =
    config
  in
  (* print_endline
    @@ Jsonaf.to_string_hum
    @@ Prompt_template.Chat_markdown.jsonaf_of_config config; *)
  let model =
    match model with
    | Some m ->
      print_endline m;
      Openai.model_of_str_exn m
    | None -> Gpt4
  in
  let messages = get_messages @@ elements in
  List.iter messages ~f:(fun m ->
    print_endline @@ Jsonaf.to_string_hum @@ Prompt_template.Chat_markdown.jsonaf_of_msg m);
  let inputs = List.map ~f:(convert ~dir ~net) @@ get_messages @@ elements in
  print_endline "inputs";
  List.iter inputs ~f:(fun i ->
    print_endline @@ Jsonaf.to_string_hum @@ Openai.jsonaf_of_chat_message i);
  let choice =
    Openai.post_chat_completion
      Openai.Default
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
    run_agent (prompt ^ call ^ call_result) [] ~dir ~net
;;

let run_completion ~env ~output_file ~prompt_file =
  let dir = Eio.Stdenv.fs env in
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
    (* so args needs to be a table with id -> args, toolid use for getting current tool and knowinf when to switch, func name same as args *)
    let f () =
      let contents = ref false in
      let function_call = ref false in
      let func_name = Hashtbl.create (module String) in
      let tool_id = ref "" in
      let args = Hashtbl.create (module String) in
      fun choice ->
        match choice.Openai.delta.role with
        | Some _ | None ->
          if String.length (choice.delta.content |> content) > 0
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
    let elements = Prompt_template.Chat_markdown.parse_chat_inputs ~dir prompt in
    let config = get_config elements in
    let Prompt_template.Chat_markdown.{ max_tokens; model; reasoning_effort; temperature }
      =
      config
    in
    (* print_endline
    @@ Jsonaf.to_string_hum
    @@ Prompt_template.Chat_markdown.jsonaf_of_config config; *)
    let model =
      match model with
      | Some m ->
        print_endline m;
        Openai.model_of_str_exn m
      | None -> Gpt4
    in
    let messages = get_messages @@ elements in
    print_endline "messages";
    List.iter messages ~f:(fun m ->
      print_endline
      @@ Jsonaf.to_string_hum
      @@ Prompt_template.Chat_markdown.jsonaf_of_msg m);
    let text =
      {|
    Before performing any tool calls ask the user for permission.
    |}
    in
    let func_tool_system_msg =
      { Openai.role = "developer"
      ; content = Some (Openai.Text text)
      ; name = None
      ; function_call = None
      ; tool_calls = None
      ; tool_call_id = None
      }
    in
    let inputs =
      func_tool_system_msg
      :: (List.map ~f:(convert ~dir ~net:env#net) @@ get_messages @@ elements)
    in
    print_endline "inputs";
    List.iter inputs ~f:(fun i ->
      print_endline @@ Jsonaf.to_string_hum @@ Openai.jsonaf_of_chat_message i);
    Openai.post_chat_completion
      (Openai.Stream (f ()))
      ?max_tokens
      ~dir
      env#net
      ~tools:funcs
      ~model
      ?reasoning_effort
      ?temperature
      ~inputs;
    if !run then start () else ()
  in
  start ()
;;
