open! Core

let%expect_test "default config" =
  let cfg = Context_compaction.Config.default in
  print_s
    [%sexp
      { context_limit : int = cfg.context_limit
      ; relevance_threshold : float = cfg.relevance_threshold
      }];
  [%expect {| ((context_limit 20000) (relevance_threshold 0.5)) |}]
;;

let%expect_test "Eio config loading skips malformed settings and respects path precedence"
  =
  Eio_main.run (fun env ->
    let name = "ochat-compaction-config-" ^ Int.to_string (Random.bits ()) in
    let directory = Eio.Path.(Eio.Stdenv.fs env / "/tmp" / name) in
    Eio.Path.mkdir ~perm:0o700 directory;
    let first = Eio.Path.(directory / "first.json") in
    let second = Eio.Path.(directory / "second.json") in
    Exn.protect
      ~finally:(fun () ->
        List.iter [ first; second ] ~f:Eio.Path.unlink;
        Eio.Path.rmdir directory)
      ~f:(fun () ->
        Eio.Path.save ~create:(`Exclusive 0o600) first {|{"context_limit":"wrong"}|};
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          second
          {|{"context_limit":1234,"relevance_threshold":0.8,"relevance_filtering":true}|};
        let paths =
          List.map [ "first.json"; "second.json" ] ~f:(fun file ->
            "/tmp/" ^ name ^ "/" ^ file)
        in
        let config = Context_compaction.Config.load_paths ~env paths in
        printf
          "limit=%d threshold=%.1f filtering=%b\n"
          config.context_limit
          config.relevance_threshold
          config.relevance_filtering;
        Eio.Path.save ~create:(`Or_truncate 0o600) first {|{"context_limit":321}|};
        printf
          "first=%d\n"
          (Context_compaction.Config.load_paths ~env paths).context_limit));
  [%expect
    {|
    limit=1234 threshold=0.8 filtering=true
    first=321
    |}]
;;
