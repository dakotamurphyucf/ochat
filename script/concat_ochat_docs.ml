(** concat_ochat_docs.ml
    ----------------------------
    Simple helper that concatenates all Markdown files found under
    [markdown/ochat] (flat directory) into a single artefact
    [out/ochat_docs/ALL_OCHAT_DOCS.md].  Each file content is preceded by a
    level-2 Markdown header with the original filename (without extension)
    and a HTML comment that records the source path.  The target directory is
    created on demand.

    The script mirrors the *concatenation* portion of [script/doc_mining.ml]
    but without any coverage checks – it only aggregates the files.

    Usage:

    ```sh
    dune exec concat_ochat_docs
    ```
    The command produces (or overwrites) the output file and prints the
    location when done.
*)

open Printf

let src_dir = Filename.concat "markdown" "ochat"

let out_root = Filename.concat (Sys.getcwd ()) "out"
let out_dir  = Filename.concat out_root "ochat_docs"
let out_file = Filename.concat out_dir "ALL_OCHAT_DOCS.md"

let ensure_dir path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o755

let list_markdown_files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun fname -> Filename.check_suffix fname ".md")
  |> List.sort String.compare

let () =
  if not (Sys.file_exists src_dir) then begin
    eprintf "Error: source directory '%s' not found.\n" src_dir;
    exit 1
  end;

  ensure_dir out_root;
  ensure_dir out_dir;

  let oc = open_out out_file in

  list_markdown_files src_dir
  |> List.iter (fun fname ->
         let path = Filename.concat src_dir fname in
         let header_name = Filename.remove_extension fname in
         fprintf oc "\n\n## %s\n\n<!-- source: %s -->\n\n" header_name path;
         (* copy file content verbatim *)
         (try
            let ic = open_in path in
            (try
               while true do
                 let line = input_line ic in
                 fprintf oc "%s\n" line
               done
             with End_of_file -> ());
            close_in ic
          with Sys_error err ->
            eprintf "Warning: failed to read %s (%s)\n" path err));

  close_out oc;
  printf "Concatenation complete – output written to %s\n" out_file

