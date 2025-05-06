open Core
open Io

let create_file ~(dir : Eio.Fs.dir_ty Eio.Path.t) : Gpt_function.t =
  let f (s, c) =
    Io.save_doc ~dir s c;
    sprintf "%s created" s
  in
  Gpt_function.create_function (module Definitions.Create_file) f
;;

let update_lines (s : string) (edits : (int * string) list) : string =
  let lines = String.split_lines s in
  let edits_map = Int.Map.of_alist_exn edits in
  List.mapi lines ~f:(fun i line ->
    match Map.find edits_map (i + 1) with
    | Some new_line -> new_line
    | None -> line)
  |> String.concat ~sep:"\n"
;;

let insert_at s (input : string) l_num : string =
  let lines = String.split_lines s in
  List.mapi lines ~f:(fun i line ->
    match i + 1 = l_num with
    | true -> String.concat [ input; line ] ~sep:"\n"
    | false -> line)
  |> String.concat ~sep:"\n"
;;

let update_file_lines ~dir : Gpt_function.t =
  let f (f, e) =
    let s = Io.load_doc ~dir f in
    let c = update_lines s e in
    Io.save_doc ~dir f c;
    sprintf "%s updated" f
  in
  Gpt_function.create_function (module Definitions.Update_file_lines) f
;;

let add_line_numbers str =
  let lines = String.split_lines str in
  let numbered_lines =
    List.mapi ~f:(fun i line -> Printf.sprintf "%d. %s" (i + 1) line) lines
  in
  String.concat ~sep:"\n" numbered_lines
;;

let get_contents ~dir : Gpt_function.t =
  let f path =
    match Io.load_doc ~dir path with
    | res -> res
    | exception ex -> Fmt.str "error running read_file: %a" Eio.Exn.pp ex
  in
  Gpt_function.create_function (module Definitions.Get_contents) f
;;

let edit_code ~net ~dir =
  let f (instruction, code) =
    let content =
      Openai.Completions.Text
        {|\nYou are an expert software engineer that edits code given a user instruction.  Only return the lines that have been edited from original output i.e \n\nUpdate The following ocaml function sub so that it decreases the return value by 2.\n1. \n2. let sub a d = \n3.  let c = a * d in\n4.  let r = c - d in\n5.  c + r \noutput\n5.  c + r - 2|}
    in
    let system =
      { Openai.Completions.role = "system"
      ; content = Some content
      ; name = None
      ; function_call = None
      ; tool_calls = None
      ; tool_call_id = None
      }
    in
    let content =
      Openai.Completions.Text
        (sprintf "instruction: %s\n%s" instruction (add_line_numbers code))
    in
    let user =
      { Openai.Completions.role = "user"
      ; content = Some content
      ; name = None
      ; function_call = None
      ; tool_calls = None
      ; tool_call_id = None
      }
    in
    let msg =
      Openai.Completions.post_chat_completion
        Openai.Completions.Default
        ~max_tokens:2000
        net
        ~dir
        ~inputs:[ system; user ]
    in
    Option.value_exn msg.message.content
  in
  Gpt_function.create_function (module Definitions.Edit_code) f
;;

let insert_code ~dir : Gpt_function.t =
  let f (file, line, code) =
    let s = Io.load_doc ~dir file in
    let updated_content = insert_at s code line in
    Io.save_doc ~dir file updated_content;
    sprintf "%s updated" file
  in
  Gpt_function.create_function (module Definitions.Insert_code) f
;;

let append_to_file ~dir : Gpt_function.t =
  let f (file, content) =
    let s = Io.load_doc ~dir file in
    let updated_content = String.concat [ s; content ] ~sep:"\n" in
    Io.save_doc ~dir file updated_content;
    sprintf "%s updated" file
  in
  Gpt_function.create_function (module Definitions.Append_to_file) f
;;

let summarize_file ~dir ~net : Gpt_function.t =
  let f file =
    let text = Io.load_doc ~dir file in
    let content =
      Openai.Completions.Text
        "You are an AI language model. Summarize the following text:"
    in
    let system =
      { Openai.Completions.role = "system"
      ; content = Some content
      ; name = None
      ; function_call = None
      ; tool_calls = None
      ; tool_call_id = None
      }
    in
    let content = Openai.Completions.Text text in
    let user =
      { Openai.Completions.role = "user"
      ; content = Some content
      ; name = None
      ; function_call = None
      ; tool_calls = None
      ; tool_call_id = None
      }
    in
    let msg =
      Openai.Completions.post_chat_completion
        Openai.Completions.Default
        ~max_tokens:8000
        ~model:Gpt3_16k
        net
        ~dir
        ~inputs:[ system; user ]
    in
    Option.value_exn msg.message.content
  in
  Gpt_function.create_function (module Definitions.Summarize_file) f
;;

let generate_interface ~dir ~net : Gpt_function.t =
  let f file =
    let code = Io.load_doc ~dir file in
    let content =
      Openai.Completions.Text
        "You are an AI language model. You are an expert Ocaml developer, and always \
         include comments that follow odoc standards. Generate an interface file for the \
         following OCaml file:"
    in
    let system =
      { Openai.Completions.role = "system"
      ; content = Some content
      ; name = None
      ; function_call = None
      ; tool_calls = None
      ; tool_call_id = None
      }
    in
    let content = Openai.Completions.Text (Printf.sprintf "```ocaml\n%s\n```" code) in
    let user =
      { Openai.Completions.role = "user"
      ; content = Some content
      ; name = None
      ; function_call = None
      ; tool_calls = None
      ; tool_call_id = None
      }
    in
    let msg =
      Openai.Completions.post_chat_completion
        Openai.Completions.Default
        ~max_tokens:2000
        ~model:Gpt4
        net
        ~dir
        ~inputs:[ system; user ]
    in
    Option.value_exn msg.message.content
  in
  Gpt_function.create_function (module Definitions.Generate_interface) f
;;

let generate_code_context_from_query ~dir ~net : Gpt_function.t =
  let f (file, query, context) =
    let code = Io.load_doc ~dir file in
    let content =
      Openai.Completions.Text
        "You are an AI language model and an expert Ocaml developer. You are being given \
         a user query for generating ocaml code, code from an ocaml file, and an \
         aggregated context. Use the query and the aggregated context to determine what \
         the most relevant information (code snippets/ functions / examples / ect) is \
         from the provided ocaml code that will aid in generating the code described in \
         the query. respond with only the MOST relevant information, make sure you are \
         not duplicating any info that is already in the aggregated context. Begin \
         response with  'Relevant info from <filename>:'"
    in
    let system =
      { Openai.Completions.role = "system"
      ; content = Some content
      ; name = None
      ; function_call = None
      ; tool_calls = None
      ; tool_call_id = None
      }
    in
    let content =
      Openai.Completions.Text
        (Printf.sprintf
           "Context: %s\n Code for %s:\n```ocaml\n%s\n```\nQuery: %s"
           query
           file
           context
           code)
    in
    let user =
      { Openai.Completions.role = "user"
      ; content = Some content
      ; name = None
      ; function_call = None
      ; tool_calls = None
      ; tool_call_id = None
      }
    in
    let msg =
      Openai.Completions.post_chat_completion
        Openai.Completions.Default
        ~max_tokens:3000
        ~model:Gpt4
        ~dir
        net
        ~inputs:[ system; user ]
    in
    Option.value_exn msg.message.content
  in
  Gpt_function.create_function (module Definitions.Generate_code_context_from_query) f
;;

let get_url_content ~net : Gpt_function.t =
  let f url =
    let host = Net.get_host url in
    let path = Net.get_path url in
    print_endline host;
    print_endline path;
    let headers = Http.Header.of_list [ "Accept", "*/*"; "Accept-Encoding", "gzip" ] in
    let res = Net.get Net.Default ~net ~host path ~headers in
    let decompressed = Option.value ~default:res @@ Result.ok (Ezgzip.decompress res) in
    let soup = Soup.parse decompressed in
    String.concat ~sep:"\n"
    @@ List.filter ~f:(fun s -> not @@ String.equal "" s)
    @@ List.map ~f:(fun s -> String.strip s)
    @@ Soup.texts soup
  in
  Gpt_function.create_function (module Definitions.Get_url_content) f
;;

let index_ocaml_code ~dir ~dm ~net : Gpt_function.t =
  let f (folder_to_index, vector_db_folder) =
    Eio.Switch.run
    @@ fun sw ->
    Indexer.index ~sw ~dir ~dm ~net ~vector_db_folder ~folder_to_index;
    "code has been indexed"
  in
  Gpt_function.create_function (module Definitions.Index_ocaml_code) f
;;

let query_vector_db ~dir ~net : Gpt_function.t =
  let f (vector_db_folder, query, num_results, index) =
    let vf = dir / vector_db_folder in
    let index =
      Option.value ~default:"" @@ Option.map ~f:(fun index -> "." ^ index) index
    in
    let file = String.concat [ "vectors"; index; ".binio" ] in
    let vec_file = String.concat [ vector_db_folder; "/"; file ] in
    let vecs = Vector_db.Vec.read_vectors_from_disk vec_file in
    let corpus = Vector_db.create_corpus vecs in
    let response = Openai.Embeddings.post_openai_embeddings net ~input:[ query ] in
    let query_vector =
      Owl.Mat.of_arrays [| Array.of_list (List.hd_exn response.data).embedding |]
      |> Owl.Mat.transpose
    in
    let top_indices = Vector_db.query corpus query_vector num_results in
    let docs = Vector_db.get_docs vf corpus top_indices in
    let results =
      List.map ~f:(fun doc -> sprintf "\n**Result:**\n```ocaml\n%s\n```\n" doc) docs
    in
    String.concat ~sep:"\n" results
  in
  Gpt_function.create_function (module Definitions.Query_vector_db) f
;;

let replace_lines_in_file ~dir : Gpt_function.t =
  let f (file, start_line, end_line, text) =
    let s = Io.load_doc ~dir file in
    let lines = String.split_lines s in
    let before = List.take lines (start_line - 1) in
    let after = List.drop lines end_line in
    let updated_content = String.concat ~sep:"\n" (before @ [ text ] @ after) in
    Io.save_doc ~dir file updated_content;
    sprintf "%s updated with new text from line %d to %d" file start_line end_line
  in
  Gpt_function.create_function (module Definitions.Replace_lines) f
;;

let apply_patch ~dir : Gpt_function.t =
  let split path =
    Eio.Path.split (dir / path)
    |> Option.map ~f:(fun ((_, dirname), basename) -> dirname, basename)
  in
  let f patch =
    let open_fn path = Io.load_doc ~dir path in
    let write_fn path s =
      match split path with
      | Some (dirname, _basename) ->
        (match Io.is_dir ~dir dirname with
         | true -> Io.save_doc ~dir path s
         | false ->
           Io.mkdir ~exists_ok:true ~dir dirname;
           Io.save_doc ~dir path s)
      | None -> Io.save_doc ~dir path s
    in
    let remove_fn path = Io.delete_doc ~dir path in
    match Apply_patch.process_patch ~text:patch ~open_fn ~write_fn ~remove_fn with
    | _ -> sprintf "git patch successful"
    | exception ex -> Fmt.str "error running apply_patch: %a" Eio.Exn.pp ex
  in
  Gpt_function.create_function (module Definitions.Apply_patch) f
;;

let read_dir ~dir : Gpt_function.t =
  let f path =
    match Io.directory ~dir path with
    | res -> String.concat ~sep:"\n" res
    | exception ex -> Fmt.str "error running read_directory: %a" Eio.Exn.pp ex
  in
  Gpt_function.create_function (module Definitions.Read_directory) f
;;

let mkdir ~dir : Gpt_function.t =
  let f path =
    match Io.mkdir ~exists_ok:true ~dir path with
    | () -> sprintf "Directory %s created successfully." path
    | exception ex -> Fmt.str "error running mkdir: %a" Eio.Exn.pp ex
  in
  Gpt_function.create_function (module Definitions.Make_dir) f
;;
