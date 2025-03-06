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

let run_completion ~env ~output_file ~prompt_file =
  let dir = Eio.Stdenv.fs env in
  let dm = Eio.Stdenv.domain_mgr env in
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
        ; Functions.generate_interface ~net:env#net ~dir
          (* ; Functions.generate_code_context_from_query ~net:env#net ~dir *)
        ; Functions.get_url_content ~net:env#net
        ; Functions.index_ocaml_code ~net:env#net ~dir ~dm
        ; Functions.query_vector_db ~net:env#net ~dir
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
            append_doc "\n<msg role=\"assistant\"><raw>\n";
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
             then append_doc (choice.delta.content |> content));
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
                       <msg role=\"assistant\" tool_call tool_id=\"%s\" \
                       function_name=\"%s\">"
                      key
                      name);
                 append_doc data;
                 append_doc "\n</msg>\n";
                 append_doc
                 @@ sprintf "\n<msg role=\"tool\" tool_call_id=\"%s\">%s</msg>" key res);
               (* this is a function_call so we need to run again with value *)
               run := true)
             else (
               append_doc "\n</raw></msg>\n";
               append_doc "\n<msg role=\"user\">\n\n</msg>")
           | None -> ())
    in
    let convert
      { Prompt_template.Chat_markdown.role
      ; content
      ; name
      ; function_call
      ; tool_call
      ; tool_call_id
      }
      =
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
                 let image_url =
                   match item.image_url with
                   | Some url ->
                     (match item.is_local with
                      | true ->
                        Some
                          { Openai.url =
                              Io.Base64.file_to_data_uri ~dir:(Eio.Stdenv.fs env) url.url
                          }
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
                           let doc =
                             Io.Net.get Io.Net.Default ~net:env#net ~host ~headers path
                           in
                           Some (clean_html doc)
                         | false ->
                           let doc = Io.Net.get Io.Net.Default ~net:env#net ~host path in
                           Some doc))
                   | None -> item.text
                 in
                 Openai.{ type_ = item.type_; text; image_url })))
      in
      (* (match content with
       | None -> ()
       | Some content ->
         print_endline
         @@ Jsonaf.to_string_hum
         @@ Openai.jsonaf_of_chat_message_content content); *)
      { Openai.role; content; name; function_call; tool_calls; tool_call_id }
    in
    let elements = Prompt_template.Chat_markdown.parse_chat_inputs ~dir prompt in
    let config = get_config elements in
    let Prompt_template.Chat_markdown.{ max_tokens; model; reasoning_effort; temperature }
      =
      config
    in
    print_endline
    @@ Jsonaf.to_string_hum
    @@ Prompt_template.Chat_markdown.jsonaf_of_config config;
    let model =
      match model with
      | Some m ->
        print_endline m;
        Openai.model_of_str_exn m
      | None -> Gpt4
    in
    Openai.post_chat_completion
      (Openai.Stream (f ()))
      ?max_tokens
      env#net (* ~tools:funcs *)
      ~model
      ?reasoning_effort
      ?temperature
      ~inputs:(List.map ~f:convert @@ get_messages @@ elements);
    if !run then start () else ()
  in
  start ()
;;

let run_completion_default ~env ~output_file ~max_tokens ~prompt_file =
  let append_doc s = Io.append_doc ~dir:(Eio.Stdenv.fs env) output_file s in
  let () =
    match prompt_file with
    | Some file ->
      let p = Io.load_doc ~dir:(Eio.Stdenv.fs env) file in
      append_doc p
    | None -> ()
  in
  let start () =
    let prompt = Io.load_doc ~dir:(Eio.Stdenv.fs env) output_file in
    (* append_doc "\n<msg role=\"assistant\">\n"; *)
    let convert
      { Prompt_template.Chat_markdown.role
      ; content
      ; name
      ; function_call
      ; tool_call
      ; tool_call_id
      }
      =
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
                 let image_url =
                   match item.image_url with
                   | Some url ->
                     (match item.is_local with
                      | true ->
                        Some
                          { Openai.url =
                              Io.Base64.file_to_data_uri ~dir:(Eio.Stdenv.fs env) url.url
                          }
                      | false -> Some { Openai.url = url.url })
                   | None -> None
                 in
                 Openai.{ type_ = item.type_; text = item.text; image_url })))
      in
      (match content with
       | None -> ()
       | Some content ->
         print_endline
         @@ Jsonaf.to_string_hum
         @@ Openai.jsonaf_of_chat_message_content content);
      { Openai.role; content; name; function_call; tool_calls; tool_call_id }
    in
    let res =
      Openai.post_chat_completion
        Openai.Default
        ~max_tokens
        env#net (* ~tools:funcs *)
        ~model:O3_Mini (* ~temperature:0.0 *)
        ~reasoning_effort:"high"
        ~inputs:
          (List.map ~f:convert
           @@ get_messages
           @@ Prompt_template.Chat_markdown.parse_chat_inputs
                ~dir:(Eio.Stdenv.fs env)
                prompt)
    in
    append_doc "\n<msg role=\"assistant\">\n";
    (match res.message.content with
     | Some content -> append_doc content
     | None -> ());
    append_doc "\n</msg>\n";
    append_doc "\n<msg role=\"user\">\n\n</msg>"
  in
  start ()
;;
