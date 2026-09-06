open! Core

let excerpts =
  [ "environment", "docs-src/lib/environment.doc.md"
  ; "source", "docs-src/lib/source.doc.md"
  ; "webpage", "docs-src/lib/webpage_markdown/tool.doc.md"
  ; "completions", "docs-src/lib/openai/completions.doc.md"
  ; "crawler", "docs-src/lib/odoc_crawler.doc.md"
  ]
;;

let run env root =
  let load file = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / file) in
  List.iter excerpts ~f:(fun (name, page) ->
    let source = load (sprintf "test/agent_docs/docs_example_%s.ml" name) in
    let expected = "```ocaml\n" ^ String.rstrip source ^ "\n```" in
    if not (String.is_substring (load page) ~substring:expected)
    then failwith (page ^ ": compiled example differs from documentation"));
  Docs_example_environment.example ();
  Docs_example_source.example ();
  let metadata, _fetch = Docs_example_webpage.register env in
  assert (List.length metadata = 1);
  assert (List.length Docs_example_completions.tools = 1);
  assert (String.equal (Docs_example_completions.user "hello").role "user");
  let _crawl = Docs_example_crawler.crawl in
  Eio.Flow.copy_string
    "Selected documentation examples PASS (offline)\n"
    (Eio.Stdenv.stdout env)
;;
