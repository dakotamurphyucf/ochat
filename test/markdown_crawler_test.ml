open Core

(* Helper to create files with content *)
let write_file path contents =
  Core_unix.mkdir_p (Filename.dirname path);
  Out_channel.write_all path ~data:contents

let%expect_test "markdown_crawler.basic_ignore_and_extension" =
  (* Create a temporary working directory *)
  let tmp_root =
    Filename.concat Filename.temp_dir_name
      ("md_crawler_" ^ Int.to_string (Random.int 1_000_000))
  in
  Core_unix.mkdir_p tmp_root;
  (* Populate files ----------------------------------------------------- *)
  (* 1. A top-level markdown file that should be picked up *)
  write_file (Filename.concat tmp_root "README.md") "# Readme\n\nHello";
  (* 2. Non-markdown file – should be ignored *)
  write_file (Filename.concat tmp_root "ignore_me.txt") "ignore";
  (* 3. File ignored via fallback block-list (_build/) *)
  let build_dir = Filename.concat tmp_root "_build" in
  write_file (Filename.concat build_dir "skip.md") "# Build artefact";
  (* 4. File inside sub-directory – should be discovered *)
  let doc_dir = Filename.concat tmp_root "doc" in
  write_file (Filename.concat doc_dir "Doc1.md") "# Doc1";
  (* 5. Large file > 10 MiB – should be skipped *)
  let big_contents = String.init (11 * 1024 * 1024) ~f:(fun _ -> 'x') in
  write_file (Filename.concat tmp_root "large.md") big_contents;
  (* 6. File excluded by .gitignore *)
  write_file (Filename.concat tmp_root ".gitignore") "ignored.md\n";
  write_file (Filename.concat tmp_root "ignored.md") "# Should be ignored";

  (* Run crawler -------------------------------------------------------- *)
  let collected = ref [] in
  Eio_main.run
  @@ fun env ->
  let root = Eio.Path.(env#fs / tmp_root) in
  Markdown_crawler.crawl ~root ~f:(fun ~doc_path ~markdown:_ ->
    collected := doc_path :: !collected);
  (* Verify ------------------------------------------------------------- *)
  let files = List.sort ~compare:String.compare !collected in
  List.iter files ~f:print_endline;
  [%expect {|
README.md
doc/Doc1.md
|}]

