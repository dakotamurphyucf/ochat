open Core
module P = Chatml_project
module S = Chatml_source
module Local = Chatml_local_capture

let source path contents =
  let id = S.Id.create ~package_namespace:"test" ~relative_path:path |> Or_error.ok_exn in
  S.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let project bytes =
  match
    P.decode
      (source "chatml.project.json" bytes)
      ~limits:Document_schema.Limits.default
      ~checkpoint:ignore
  with
  | Ok value -> value
  | Error error -> raise_s [%sexp (P.Error.details error : Document_schema.Error.t)]
;;

let limits
      ?(entries = 100)
      ?(sources = 100)
      ?(bytes = 4096)
      ?(total = 65_536)
      ?(components = 8)
      ?(work = 1_000_000)
      ()
  =
  Local.Limits.create
    ~max_directory_entries:entries
    ~max_sources:sources
    ~max_source_bytes:bytes
    ~max_total_source_bytes:total
    ~max_path_components:components
    ~max_work_steps:work
  |> Or_error.ok_exn
;;

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (Local.Error.problem error : Local.Error.problem)]
;;

let save path contents = Eio.Path.save ~create:(`Or_truncate 0o644) path contents

let with_root f =
  Eio_main.run (fun env ->
    let temporary =
      Eio_unix.run_in_systhread (fun () ->
        Core_unix.mkdtemp "/tmp/ochat-local-capture-XXXXXX")
    in
    let anchor = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    Fun.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      (fun () ->
         let workspace = Eio.Path.(anchor / "workspace") in
         Eio.Path.mkdir ~perm:0o755 workspace;
         Eio.Path.with_open_dir workspace (fun root -> f env anchor workspace root)))
;;

let run ?(limits = limits ()) ?(checkpoint = ignore) p root =
  Local.capture
    p
    ~root
    ~request:(P.Selection.Entry { name = "main"; environment = None })
    ~limits
    ~checkpoint
;;

let captured_paths t =
  List.map (Local.sources t) ~f:(fun value -> S.Id.relative_path (S.id value))
;;

let find t path =
  Local.find_source t (S.id (source path "")) |> Option.value_exn |> S.contents
;;

let%expect_test
    "all catalog members, aliases and entry overlap; fresh launch is independent"
  =
  with_root (fun _ _ workspace root ->
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "src");
    let p =
      project
        {|{"schema_version":1,"namespace":"test","source_dirs":["src"],"modules":{"Alias":{"implementation":"src/a.chatml"}},"entries":{"main":{"source":"src/a.chatml","target":"one_off_v1"},"later":{"source":"missing.chatml","target":"one_off_v1"}}}|}
    in
    save Eio.Path.(workspace / "src" / "a.chatml") "old raw bytes: not parsed";
    save Eio.Path.(workspace / "src" / "a.chatmli") "signature bytes";
    save Eio.Path.(workspace / "src" / "unused.chatml") "unused";
    save Eio.Path.(workspace / "src" / "irrelevant.txt") "ignored";
    let old = run p root |> ok in
    [%test_eq: string list]
      (captured_paths old)
      [ "chatml.project.json"; "src/a.chatml"; "src/a.chatmli"; "src/unused.chatml" ];
    [%test_eq: int] (Local.Accounting.directory_entries (Local.accounting old)) 4;
    [%test_eq: int] (Local.Accounting.source_count (Local.accounting old)) 4;
    [%test_eq: int] (List.length (Local.definitions old)) 2;
    [%test_eq: int] (List.length (Local.bindings old)) 3;
    let total = List.sum (module Int) (Local.sources old) ~f:S.byte_length in
    [%test_eq: int] (Local.Accounting.source_bytes (Local.accounting old)) total;
    save Eio.Path.(workspace / "src" / "a.chatml") "new";
    let fresh = run p root |> ok in
    [%test_eq: string] (find old "src/a.chatml") "old raw bytes: not parsed";
    [%test_eq: string] (find fresh "src/a.chatml") "new";
    print_endline ("captured: " ^ String.concat ~sep:", " (captured_paths old));
    print_endline ("old: " ^ find old "src/a.chatml");
    print_endline ("fresh: " ^ find fresh "src/a.chatml"));
  [%expect
    {|
    captured: chatml.project.json, src/a.chatml, src/a.chatmli, src/unused.chatml
    old: old raw bytes: not parsed
    fresh: new
  |}]
;;

let%test_unit
    "explicit override replaces ambiguous candidates; all enumeration is charged"
  =
  with_root (fun _ _ workspace root ->
    let p =
      project
        {|{"schema_version":1,"namespace":"test","source_dirs":[".","one","two"],"modules":{"A":{"implementation":"chosen.raw","interface":"chosen.signature"}},"entries":{"main":{"source":"chosen.raw","target":"one_off_v1"}}}|}
    in
    List.iter [ "one"; "two" ] ~f:(fun directory ->
      Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / directory);
      Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / directory / "a.chatml"));
    save Eio.Path.(workspace / "chosen.raw") "selected";
    save Eio.Path.(workspace / "chosen.signature") "selected interface";
    let value = run p root |> ok in
    [%test_eq: int] (Local.Accounting.directory_entries (Local.accounting value)) 6;
    [%test_eq: string list]
      (captured_paths value)
      [ "chatml.project.json"; "chosen.raw"; "chosen.signature" ];
    match run ~limits:(limits ~entries:5 ()) p root with
    | Error e ->
      (match Local.Error.problem e with
       | Limit_exceeded { limit = Directory_entries; _ } -> ()
       | _ -> failwith "entry cap")
    | Ok _ -> failwith "discarded candidates escaped entry cap")
;;

let%expect_test "exact missing leaf only; explicit interface and missing parent fail" =
  with_root (fun _ _ workspace root ->
    let p =
      project
        {|{"schema_version":1,"namespace":"test","modules":{"A":{"implementation":"code.chatml"}},"entries":{"main":{"source":"code.chatml","target":"one_off_v1"}}}|}
    in
    save Eio.Path.(workspace / "code.chatml") "code";
    let value = run ~limits:(limits ~sources:2 ()) p root |> ok in
    [%test_eq: int] (Local.Accounting.source_count (Local.accounting value)) 2;
    print_endline ("optional absent: " ^ String.concat ~sep:", " (captured_paths value));
    let explicit =
      project
        {|{"schema_version":1,"namespace":"test","modules":{"A":{"implementation":"code.chatml","interface":"absent.chatmli"}},"entries":{"main":{"source":"code.chatml","target":"one_off_v1"}}}|}
    in
    let parent =
      project
        {|{"schema_version":1,"namespace":"test","modules":{"A":{"implementation":"code.chatml","interface":"missing/absent.chatmli"}},"entries":{"main":{"source":"code.chatml","target":"one_off_v1"}}}|}
    in
    List.iter [ explicit; parent ] ~f:(fun p ->
      match run p root with
      | Error e ->
        (match Local.Error.problem e with
         | Filesystem { failure; member = _ } ->
           let operation =
             Chatml_local_source.Error.sexp_of_operation
               (Chatml_local_source.Error.operation failure)
             |> Sexp.to_string
           in
           let code =
             Chatml_local_source.Error.sexp_of_code
               (Chatml_local_source.Error.code failure)
             |> Sexp.to_string
           in
           let field =
             P.Declared_at.field_path (Local.Error.declared_at e)
             |> String.concat ~sep:"/"
           in
           printf "required interface: %s/%s at %s\n" operation code field
         | _ -> failwith "wrong missing error")
      | Ok _ -> failwith "missing required interface admitted");
    save Eio.Path.(workspace / "code.chatmli") "";
    match run ~limits:(limits ~sources:2 ()) p root with
    | Error e ->
      (match Local.Error.problem e with
       | Limit_exceeded { limit = Source_count; maximum } ->
         printf "present optional: Source_count (maximum %d)\n" maximum
       | _ -> failwith "present optional")
    | Ok _ -> failwith "present optional exceeded source count");
  [%expect
    {|
    optional absent: chatml.project.json, code.chatml
    required interface: Open_file/Missing at modules/A
    required interface: Open_parent/Missing at modules/A
    present optional: Source_count (maximum 2)
  |}]
;;

let%test_unit "independent exact byte/work bounds and interruption cleanup" =
  with_root (fun env _ workspace root ->
    let p =
      project
        {|{"schema_version":1,"namespace":"test","entries":{"main":{"source":"code.raw","target":"one_off_v1"}}}|}
    in
    save Eio.Path.(workspace / "code.raw") "abc";
    let total = S.byte_length (P.source p) + 3 in
    let value = run ~limits:(limits ~total ()) p root |> ok in
    (* Hand-calculated from the documented v1 stage weights: declaration
       chunk=2; metadata=B; empty lookup/directory stages=0; root=6;
       planning=32; read allowance=3 plus depth/overhead=8; hash=2;
       validation (2 captures + 1 member + 1 planning item)=384; revision=2. *)
    let work = S.byte_length (P.source p) + 439 in
    [%test_eq: int] (Local.Accounting.work_steps (Local.accounting value)) work;
    ignore (run ~limits:(limits ~total ~work ()) p root |> ok : Local.t);
    List.iter
      [ limits ~total:(total - 1) (); limits ~total ~work:(work - 1) () ]
      ~f:(fun limits ->
        match run ~limits p root with
        | Error _ -> ()
        | Ok _ -> failwith "budget overflow admitted");
    let descriptors () =
      Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / "/dev/fd") |> List.length
    in
    let polls = ref 0 in
    ignore (run ~checkpoint:(fun () -> incr polls) p root |> ok : Local.t);
    let before = descriptors () in
    for stop = 1 to !polls do
      let visited = ref 0 in
      let token = Failure "capture interruption" in
      (match
         run
           ~checkpoint:(fun () ->
             incr visited;
             if !visited = stop then raise token)
           p
           root
       with
       | _ -> failwith "checkpoint swallowed"
       | exception actual -> assert (phys_equal token actual));
      [%test_eq: int] (descriptors ()) before;
      ignore (run p root |> ok : Local.t)
    done;
    [%test_eq: string] (find value "code.raw") "abc")
;;
