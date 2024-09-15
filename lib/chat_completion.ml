open Core

let run_completion ~env ~output_file ~max_tokens ~prompt_file =
  let dir = Eio.Stdenv.fs env in
  let dm = Eio.Stdenv.domain_mgr env in
  let append_doc = Io.append_doc ~dir:(Eio.Stdenv.fs env) output_file in
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
        [ 
          (* Functions.create_file ~dir *)
        Functions.get_contents ~dir (* change to load contents *)
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
    let f () =
      let contents = ref false in
      let function_call = ref false in
      let func_name = ref "" in
      let args = ref "" in
      fun choice ->
        match choice.Openai.delta.role with
        | Some _ | None ->
          if String.length (choice.delta.content |> content) > 0
             && (not !contents)
             && not !function_call
          then (
            append_doc "\n<msg role=\"assistant\">\n";
            contents := true);
          (match choice.delta.function_call with
           | Some call ->
             print_endline call.name;
             if not !function_call
             then (
               function_call := true;
               append_doc
                 (sprintf
                    "\n<msg role=\"assistant\" function_call function_name=\"%s\">"
                    call.name);
               func_name := call.name);
             append_doc call.arguments;
             args := String.concat [ !args; call.arguments ]
           | None ->
             if String.length (choice.delta.content |> content) > 0
             then append_doc (choice.delta.content |> content));
          (match choice.finish_reason with
           | Some _ ->
             if !function_call
             then (
               (* add assistant msg with function_call function_name function_arguments*)
               append_doc "\n</msg>\n";
               (* call function and add results to doc in funtion role type with name*)
               let f = Hashtbl.find_exn tbl !func_name in
               let res = f !args in
               append_doc
               @@ sprintf "\n<msg role=\"function\" name=\"%s\">%s</msg>" !func_name res;
               (* this is a function_call so we need to run again with value *)
               run := true)
             else (
               append_doc "\n</msg>\n";
               append_doc "\n<msg role=\"user\">\n\n</msg>")
           | None -> ())
    in
    let convert { Prompt_template.Chat_markdown.role; content; name; function_call;} =
      let function_call =
        Option.map function_call ~f:(fun function_call ->
          { Openai.name = function_call.name; arguments = function_call.arguments })
      in
      { Openai.role; content; name; function_call }
    in
    Openai.post_chat_completion
      (Openai.Stream (f ()))
      ~max_tokens
      env#net
      ~functions:funcs
      ~model:Gpt4
      ~temperature:0.0
      ~inputs:
        (List.map ~f:convert @@ Prompt_template.Chat_markdown.parse_chat_inputs prompt);
    if !run then start () else ()
  in
  start ()
;;
