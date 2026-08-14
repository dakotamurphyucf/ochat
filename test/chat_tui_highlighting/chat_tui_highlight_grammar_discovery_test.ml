open Core

let grammar ~name ~scope ~filetype =
  sprintf
    {|{
  "name": %S,
  "scopeName": %S,
  "fileTypes": [%S],
  "patterns": []
}|}
    name
    scope
    filetype
;;

let save path contents = Eio.Path.save ~create:(`Or_truncate 0o644) path contents

let with_env values f =
  let previous = List.map values ~f:(fun (key, _) -> key, Sys.getenv key) in
  List.iter values ~f:(fun (key, value) -> Core_unix.putenv ~key ~data:value);
  Exn.protect ~f ~finally:(fun () ->
    List.iter previous ~f:(fun (key, value) ->
      Core_unix.putenv ~key ~data:(Option.value value ~default:"")))
;;

let is_registered registry lang =
  Chat_tui.Highlight_tm_loader.find_grammar_by_lang_tag registry lang |> Option.is_some
;;

let%expect_test "discovers sorted regular JSON files from environment and XDG" =
  Eio_main.run
  @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let env_dir = Eio.Path.(cwd / "grammar-env") in
  let xdg_dir = Eio.Path.(cwd / "grammar-xdg/ochat/grammars") in
  Eio.Path.mkdirs ~perm:0o755 env_dir;
  Eio.Path.mkdirs ~perm:0o755 xdg_dir;
  save Eio.Path.(env_dir / "b.json") "{";
  save Eio.Path.(env_dir / "a.json") "{";
  save
    Eio.Path.(env_dir / "env.tmLanguage.json")
    (grammar ~name:"Env grammar" ~scope:"source.env-test" ~filetype:"env-test");
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(env_dir / "ignored.json");
  save
    Eio.Path.(xdg_dir / "xdg.json")
    (grammar ~name:"XDG grammar" ~scope:"source.xdg-test" ~filetype:"xdg-test");
  let registry = Chat_tui.Highlight_tm_loader.create_registry () in
  with_env
    [ "OCHAT_GRAMMAR_DIR", Eio.Path.native_exn env_dir
    ; "XDG_CONFIG_HOME", Eio.Path.native_exn Eio.Path.(cwd / "grammar-xdg")
    ]
    (fun () ->
       Chat_tui.Highlight_grammar_discovery.load
         ~fs:(Eio.Stdenv.fs env)
         ~cwd
         ~registry
         ~explicit_files:[]
         ~warn:print_endline
         ()
       |> Or_error.ok_exn);
  printf
    "env=%b xdg=%b\n"
    (is_registered registry "env-test")
    (is_registered registry "xdg-test");
  [%expect
    {|
    Failed to load discovered TextMate grammar "./grammar-env/a.json": json > object: not enough input
    Failed to load discovered TextMate grammar "./grammar-env/b.json": json > object: not enough input
    env=true xdg=true
    |}]
;;

let%expect_test "an invalid explicit file stops automatic discovery" =
  Eio_main.run
  @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let env_dir = Eio.Path.(cwd / "grammar-explicit-env") in
  Eio.Path.mkdir ~perm:0o755 env_dir;
  save Eio.Path.(cwd / "invalid-explicit.json") "{";
  save
    Eio.Path.(env_dir / "automatic.json")
    (grammar
       ~name:"Automatic grammar"
       ~scope:"source.automatic-test"
       ~filetype:"automatic-test");
  let registry = Chat_tui.Highlight_tm_loader.create_registry () in
  let result =
    with_env
      [ "OCHAT_GRAMMAR_DIR", Eio.Path.native_exn env_dir
      ; "XDG_CONFIG_HOME", Eio.Path.native_exn Eio.Path.(cwd / "missing-xdg")
      ]
      (fun () ->
         Chat_tui.Highlight_grammar_discovery.load
           ~fs:(Eio.Stdenv.fs env)
           ~cwd
           ~registry
           ~explicit_files:[ "invalid-explicit.json" ]
           ~warn:print_endline
           ())
  in
  printf
    "error=%b automatic=%b\n"
    (Result.is_error result)
    (is_registered registry "automatic-test");
  [%expect {| error=true automatic=false |}]
;;

let source_names sources =
  List.map sources ~f:Chat_tui.Highlight_grammar_discovery.Source.name
  |> List.map ~f:Filename.basename
;;

let%expect_test "immutable source collection preserves explicit and discovered policy" =
  Eio_main.run
  @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  let discovered = Eio.Path.(cwd / "grammar-source-discovered") in
  Eio.Path.mkdir ~perm:0o755 discovered;
  save
    Eio.Path.(cwd / "explicit-a.json")
    (grammar ~name:"Explicit A" ~scope:"source.explicit-a" ~filetype:"explicit-a");
  save
    Eio.Path.(cwd / "explicit-b.json")
    (grammar ~name:"Explicit B" ~scope:"source.explicit-b" ~filetype:"explicit-b");
  save Eio.Path.(cwd / "explicit-invalid.json") "{";
  save Eio.Path.(discovered / "a-invalid.json") "{";
  save
    Eio.Path.(discovered / "b-valid.json")
    (grammar ~name:"Discovered B" ~scope:"source.discovered-b" ~filetype:"discovered-b");
  save
    Eio.Path.(discovered / "c-valid.json")
    (grammar ~name:"Discovered C" ~scope:"source.discovered-c" ~filetype:"discovered-c");
  let explicit =
    Chat_tui.Highlight_grammar_discovery.load_explicit_sources
      ~fs
      ~cwd
      [ "explicit-b.json"; "explicit-a.json" ]
    |> Or_error.ok_exn
  in
  let invalid =
    Chat_tui.Highlight_grammar_discovery.load_explicit_sources
      ~fs
      ~cwd
      [ "explicit-a.json"; "explicit-invalid.json"; "explicit-b.json" ]
  in
  let warnings = ref [] in
  let loaded =
    with_env
      [ "OCHAT_GRAMMAR_DIR", Eio.Path.native_exn discovered
      ; "XDG_CONFIG_HOME", Eio.Path.native_exn Eio.Path.(cwd / "missing-sources")
      ]
      (fun () ->
         Chat_tui.Highlight_grammar_discovery.read_discovered_sources
           ~fs
           ~cwd
           ~warn:(fun warning -> warnings := warning :: !warnings)
           ())
  in
  let discovered_sources, parse_warnings =
    Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
      Chat_tui.Highlight_grammar_discovery.parse_discovered_sources loaded)
  in
  List.iter parse_warnings ~f:(fun warning -> warnings := warning :: !warnings);
  print_s
    [%sexp
      (source_names explicit : string list)
    , (Result.is_error invalid : bool)
    , (source_names discovered_sources : string list)
    , (List.length !warnings : int)];
  [%expect {| ((explicit-b.json explicit-a.json) true (b-valid.json c-valid.json) 1) |}]
;;
