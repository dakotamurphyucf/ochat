open! Core
module Int_file = Bin_prot_utils_eio.With_file_methods (Int)
module String_file = Bin_prot_utils_eio.With_file_methods (String)

let check condition = if not condition then failwith "contract assertion failed"
let fails f = check (Result.is_error (Result.try_with f))
let save path contents = Eio.Path.save ~create:(`Or_truncate 0o600) path contents

let binary_order root =
  let path = Eio.Path.(root / "order.bin") in
  Int_file.File.write_all path [ 1; 2; 3 ];
  check (List.equal Int.equal (Int_file.File.read_all path) [ 1; 2; 3 ]);
  check (List.equal Int.equal (Int_file.File.map path ~f:succ) [ 2; 3; 4 ]);
  let seen = ref [] in
  Int_file.File.iter path ~f:(fun value -> seen := value :: !seen);
  check (List.equal Int.equal (List.rev !seen) [ 1; 2; 3 ]);
  Int_file.File.write_all path [ 4 ];
  check (List.equal Int.equal (Int_file.File.read_all path) [ 1; 2; 3; 4 ]);
  fails (fun () -> Int_file.File.read path);
  fails (fun () -> Int_file.File.fold path ~init:() ~f:(fun () _ -> raise End_of_file))
;;

let binary_truncation root =
  let path = Eio.Path.(root / "truncated.bin") in
  Int_file.File.write path 12345;
  let encoded = Eio.Path.load path in
  for length = 1 to String.length encoded - 1 do
    save path (String.prefix encoded length);
    fails (fun () -> Int_file.File.read_all path);
    fails (fun () -> Int_file.File.read path)
  done;
  save path "";
  check (List.is_empty (Int_file.File.read_all path));
  fails (fun () -> Int_file.File.read path);
  save path (encoded ^ "x");
  fails (fun () -> Int_file.File.read path);
  fails (fun () -> Int_file.File.read_all path);
  save path encoded;
  check (Int_file.File.read path = 12345);
  let large = String.make 10000 'x' in
  String_file.File.write path large;
  check (List.equal String.equal (String_file.File.read_all path) [ large ])
;;

let spans () =
  let point offset = Source.{ line = 1; column = offset; offset } in
  let span a b = Source.{ left = point a; right = point b } in
  let source = Source.make "hello" in
  List.iter [ Int.min_value; -1; 0; 1; 5; 6; Int.max_value ] ~f:(fun a ->
    List.iter [ Int.min_value; -1; 0; 1; 5; 6; Int.max_value ] ~f:(fun b ->
      let text = Source.read source (span a b) in
      check (String.length text <= 5)));
  check (String.is_empty (Source.read source (span 4 2)));
  check (String.equal (Source.read source (span (-1) 10)) "hello");
  let merged = Source.merge (span 2 5) (span 0 3) in
  check (merged.left.offset = 0 && merged.right.offset = 5);
  check (String.equal (Source.read source merged) "hello")
;;

let catalogue name =
  [| Md_index_catalog.Entry.{ name; description = name; vector = [| 1. |] } |]
;;

let catalogue_atomic root =
  let dir = Eio.Path.(root / "catalogue") in
  Eio.Path.mkdir ~perm:0o700 dir;
  let path = Eio.Path.(dir / "md_index_catalog.binio") in
  Md_index_catalog.save ~dir (catalogue "old");
  let old = Eio.Path.load path in
  Eio.Switch.run (fun sw ->
    let reader = Eio.Path.open_in ~sw path in
    Md_index_catalog.save ~dir (catalogue "new");
    let contents = Eio.Buf_read.parse_exn ~max_size:10000 Eio.Buf_read.take_all reader in
    check (String.equal old contents));
  let loaded = Option.value_exn (Md_index_catalog.load ~dir) in
  check (String.equal loaded.(0).name "new");
  check ((Eio.Path.stat ~follow:true path).perm land 0o077 = 0);
  check (List.equal String.equal (Eio.Path.read_dir dir) [ "md_index_catalog.binio" ])
;;

let catalogue_failure root =
  let dir = Eio.Path.(root / "failure") in
  Eio.Path.mkdir ~perm:0o700 dir;
  let path = Eio.Path.(dir / "md_index_catalog.binio") in
  Eio.Path.mkdir ~perm:0o700 path;
  save Eio.Path.(path / "keep") "preserved";
  fails (fun () -> Md_index_catalog.save ~dir (catalogue "failed"));
  check (String.equal (Eio.Path.load Eio.Path.(path / "keep")) "preserved");
  check (List.equal String.equal (Eio.Path.read_dir dir) [ "md_index_catalog.binio" ])
;;

let catalogue_concurrent root =
  let dir = Eio.Path.(root / "concurrent") in
  Eio.Path.mkdir ~perm:0o700 dir;
  Eio.Fiber.List.iter
    (fun i -> Md_index_catalog.save ~dir (catalogue (Int.to_string i)))
    (List.init 20 ~f:Fn.id);
  let loaded = Option.value_exn (Md_index_catalog.load ~dir) in
  check (Array.length loaded = 1);
  check (Int.of_string loaded.(0).name >= 0);
  check (List.equal String.equal (Eio.Path.read_dir dir) [ "md_index_catalog.binio" ])
;;

let compatibility_probes env =
  let root = Eio.Stdenv.cwd env in
  let log = Eio.Path.(root / "run.log") in
  Eio.Path.mkdir ~perm:0o700 log;
  fails (fun () -> Log.emit `Info "blocked");
  let called = ref false in
  fails (fun () -> Log.with_span "blocked" (fun () -> called := true));
  check (not !called);
  Eio.Path.rmtree log;
  Log.emit `Info "recovered";
  check (String.is_substring (Eio.Path.load log) ~substring:"recovered");
  Eio.Switch.run (fun sw ->
    let first, resolve = Eio.Promise.create () in
    Log.heartbeat
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~interval:60.
      ~probe:(fun () ->
        Eio.Promise.resolve resolve ();
        [])
      ();
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 1. (fun () ->
      Eio.Promise.await first));
  let missing = Eio.Path.(root / "missing") in
  fails (fun () ->
    Markdown_crawler.crawl ~root:missing ~f:(fun ~doc_path:_ ~markdown:_ -> ()));
  fails (fun () ->
    Odoc_crawler.crawl ~root:missing (fun ~pkg:_ ~doc_path:_ ~markdown:_ -> ()))
;;

let crawler_callbacks env =
  let root = Eio.Stdenv.cwd env in
  let md = Eio.Path.(root / "md") in
  Eio.Path.mkdir ~perm:0o700 md;
  save Eio.Path.(md / "README.md") "hello";
  fails (fun () ->
    Markdown_crawler.crawl ~root:md ~f:(fun ~doc_path:_ ~markdown:_ ->
      failwith "callback"));
  let odoc = Eio.Path.(root / "odoc") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(odoc / "pkg" / "_doc-dir");
  save Eio.Path.(odoc / "pkg" / "_doc-dir" / "README.md") "hello";
  let visited = ref false in
  Odoc_crawler.crawl ~root:odoc (fun ~pkg:_ ~doc_path:_ ~markdown:_ ->
    visited := true;
    failwith "callback swallowed by compatibility crawler");
  check !visited
;;

let run env =
  let temporary =
    Eio.Process.parse_out
      (Eio.Stdenv.process_mgr env)
      Eio.Buf_read.line
      [ "mktemp"; "-d"; "/tmp/ochat-library-contracts.XXXXXX" ]
  in
  let root = Eio.Path.(Eio.Stdenv.fs env / temporary) in
  Fun.protect
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root))
    (fun () ->
       binary_order root;
       binary_truncation root;
       spans ();
       catalogue_atomic root;
       catalogue_failure root;
       catalogue_concurrent root;
       let executable =
         Eio.Path.native_exn Eio.Path.(Eio.Stdenv.cwd env / (Sys.get_argv ()).(0))
         |> Eio_posix.Low_level.realpath
       in
       Eio.Process.run
         ~cwd:root
         (Eio.Stdenv.process_mgr env)
         [ executable; "--compatibility-probes" ];
       Eio.Flow.copy_string
         "Library contract regressions PASS (8 groups)\n"
         (Eio.Stdenv.stdout env))
;;

let () =
  Eio_main.run (fun env ->
    if Array.mem (Sys.get_argv ()) "--compatibility-probes" ~equal:String.equal
    then (
      compatibility_probes env;
      crawler_callbacks env)
    else run env)
;;
