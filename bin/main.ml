open Core
open Eio
open Command.Let_syntax
open Io

let index_command =
  Command.basic
    ~summary:
      "Index OCaml code in the specified folder for a code vector search database using \
       OpenAI embeddings."
    (let%map_open folder_to_index =
       flag
         "-folder-to-index"
         (optional_with_default "./lib" string)
         ~doc:"FOLDER Path to the folder containing OCaml code to index (default: ./)"
     and vector_db_folder =
       flag
         "-vector-db-folder"
         (optional_with_default "./vector" string)
         ~doc:
           "FOLDER Path to the folder to store vector database data (default: ./vector)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       let dm = Eio.Stdenv.domain_mgr env in
       log ~dir @@ sprintf "Indexing OCaml code in folder: %s\n" folder_to_index;
       log ~dir @@ sprintf "Storing vector database data in folder: %s\n" vector_db_folder;
       Switch.run
       @@ fun sw ->
       Indexer.index ~sw ~dir ~dm ~net:env#net ~vector_db_folder ~folder_to_index)
;;

let query_command =
  Command.basic
    ~summary:"Query the indexed OCaml code using natural language."
    (let%map_open vector_db_folder =
       flag
         "-vector-db-folder"
         (optional_with_default "./vector" string)
         ~doc:
           "FOLDER Path to the folder containing vector database data (default: ./vector)"
     and query_text =
       flag
         "-query-text"
         (required string)
         ~doc:"TEXT Natural language query text to search the indexed OCaml code"
     and num_results =
       flag
         "-num-results"
         (optional_with_default 5 int)
         ~doc:"NUM Number of top results to return (default: 5)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       log ~dir @@ sprintf "Querying indexed OCaml code with text: **%s**\n" query_text;
       log ~dir
       @@ sprintf "Using vector database data from folder: **%s**\n" vector_db_folder;
       log ~dir @@ sprintf "Returning top **%d** results\n" num_results;
       let vf = Eio.Stdenv.fs env / vector_db_folder in
       let vec_file = String.concat [ vector_db_folder; "/"; "vectors.binio" ] in
       let vecs = Vector_db.Vec.read_vectors_from_disk vec_file in
       let corpus = Vector_db.create_corpus vecs in
       let response =
         Openai.Embeddings.post_openai_embeddings env#net ~input:[ query_text ]
       in
       let query_vector =
         Owl.Mat.of_arrays [| Array.of_list (List.hd_exn response.data).embedding |]
         |> Owl.Mat.transpose
       in
       let top_indices = Vector_db.query corpus query_vector num_results in
       let docs = Vector_db.get_docs vf corpus top_indices in
       List.iteri
         ~f:(fun i doc ->
           log ~dir @@ sprintf "\n**Result %d:**\n" (i + 1);
           log ~dir @@ sprintf "```ocaml\n%s\n```\n" doc)
         docs)
;;

let chat_completion_command =
  Command.basic
    ~summary:
      "Call OpenAI API to provide chat completion based on the content of a prompt file ."
    (let%map_open prompt_file =
       flag
         "-prompt-file"
         (optional string)
         ~doc:
           "FILE Path to the file containing inital prompt (optional). Think of this as \
            a  prompt template, and output-file as a conversation instance using the \
            prompt-template. If you are trying to continue a previous conversation do \
            not include this flag and only provide the output file"
     and output_file =
       flag
         "-output-file"
         (optional_with_default "./prompts/default.md" string)
         ~doc:
           "FILE Path to the file to save the chat completion output (default: \
            /prompts/default.md). If prompt-file is provided contents of prompt file \
            will be appended to the output file. "
     in
     fun () ->
       run_main
       @@ fun env ->
       (* let dir = Eio.Stdenv.fs env in *)
       (* Prompt_template.run (); *)

       (* log ~dir @@ sprintf "Saving chat completion output to: %s\n" output_file; *)
       (* Chat_completion.run_completion ~env ~output_file ~prompt_file *)
       Chat_response.run_completion_stream ~env ?prompt_file ~output_file ())
;;

let apply_patch_command =
  Command.basic
    ~summary:"Apply a patch to the codebase."
    (let%map_open patch_file =
       flag
         "-patch-file"
         (optional_with_default "patch.txt" string)
         ~doc:"FILE Path to the patch file to apply"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       log ~dir @@ sprintf "Applying patch from file: %s\n" patch_file;
       let main =
         {|#### Fix an issue

\`\`\`sh
# First, copy an error
# Then, start codex with interactive mode
codex

# Or you can pass in via command line argument
codex "Fix this issue: $(pbpaste)"

# Or even as a task (it should use your current repo and branch)
codex -t "Fix this issue: $(pbpaste)"
\`\`\`|}
       in
       let files = Map.of_alist_exn (module String) [ "README.md", main ] in
       let remove_fn str = print_endline str in
       let write_fn path str =
         print_endline "write: ";
         print_endline path;
         print_endline str
       in
       let open_fn str =
         print_endline str;
         Map.find_exn files str
       in
       let patch =
         {|*** Begin Patch
*** Update File: README.md
@@
  codex -t "Fix this issue: $(pbpaste)"
  \`\`\`
+
+hello
*** End Patch|}
       in
       let _res =
         Apply_patch.process_patch
           ~text:patch (* Placeholder for the patch text *)
           ~open_fn
           ~write_fn
           ~remove_fn
       in
       ())
;;

let describe_command =
  Command.basic
    ~summary:"get dune describe output."
    (let%map_open _ =
       flag
         "-file"
         (optional_with_default "bin/main.ml" string)
         ~doc:"FILE Path to the file to tokenize (default: bin/main.ml)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let output = Dune_describe.run env in
       let json = Dune_describe.jsonaf_of_project_details output in
       Io.console_log ~stdout:env#stdout @@ Jsonaf.to_string json)
;;

let tokenize_command =
  Command.basic
    ~summary:"Tokenize the provided file using the OpenAI Tikitoken spec"
    (let%map_open file =
       flag
         "-file"
         (optional_with_default "bin/main.ml" string)
         ~doc:"FILE Path to the file to tokenize (default: bin/main.ml)"
     in
     fun () ->
       run_main
       @@ fun env ->
       let dir = Eio.Stdenv.fs env in
       let tiki_token_bpe =
         load_doc ~dir:(Eio.Stdenv.fs env) "./out-cl100k_base.tikitoken.txt"
       in
       let text = load_doc ~dir:(Eio.Stdenv.fs env) file in
       let codec = Tikitoken.create_codec tiki_token_bpe in
       log ~dir @@ sprintf "Tokenizing file: %s\n" file;
       let encoded = Tikitoken.encode ~codec ~text in
       Io.console_log ~stdout:env#stdout @@ sprintf "tokens: %i\n" (List.length encoded))
;;

let main_command =
  Command.group
    ~summary:
      "A command-line app for indexing OCaml code and serving queries to a code vector \
       search database using OpenAI embeddings."
    [ "index", index_command
    ; "query", query_command
    ; "chat-completion", chat_completion_command
    ; "tokenize", tokenize_command
    ; "dune-describe", describe_command
    ; "apply-patch", apply_patch_command
    ]
;;

let () = Command_unix.run main_command
