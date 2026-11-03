open! Core
open Support

(* Independent oracle: filter a known finite file inventory by directory ancestry,
   then rank by component count. It does not construct the scanner's walk. *)
let guidance_oracle files target =
  let components name = String.split name ~on:'/' in
  let target = components target in
  List.filter files ~f:(fun name ->
    String.equal (Filename.basename name) "AGENTS.md"
    &&
    let parent = Filename.dirname name in
    String.equal parent "."
    || List.is_prefix target ~prefix:(components parent) ~equal:String.equal)
  |> List.sort ~compare:(fun a b ->
    Int.compare (List.length (components a)) (List.length (components b)))
;;

let%expect_test "real admitted sources preserve root-to-target order and shallow skills" =
  with_roots (fun _ _ left right _ _ left_root right_root actor denied _ ->
    let files = [ "a/b/AGENTS.md"; "sibling/AGENTS.md"; "AGENTS.md"; "a/AGENTS.md" ] in
    List.iter files ~f:(fun member -> save left member member);
    let scope = C.Scope.of_path (path "a/b") in
    let discovery =
      I.discover_project left_root ~actor ~scope ~limits ~checkpoint:ignore |> io_ok
    in
    [%test_eq: string list] (candidate_names discovery) (guidance_oracle files "a/b");
    print_s [%sexp (candidate_names discovery : string list)];
    assert (
      Result.is_error
        (I.discover_project left_root ~actor:denied ~scope ~limits ~checkpoint:ignore));
    save right "demo/SKILL.md" (skill "demo");
    save left "demo/SKILL.md" (skill "demo");
    save right "nested/deep/SKILL.md" (skill "deep");
    save right "invalid-name/SKILL.md" "---\nname: other\ndescription: no\n---\n";
    let first = discover_skills right_root actor |> io_ok in
    [%test_eq: string list] (candidate_names first) [ "demo/SKILL.md" ];
    let a = List.hd_exn (I.Discovery.candidates first) in
    let b =
      discover_skills left_root actor
      |> io_ok
      |> I.Discovery.candidates
      |> List.find_exn ~f:(fun value ->
        String.equal
          (Source.Portable_path.to_string
             (C.Id.member (C.Observed.id (I.Candidate.observed value))))
          "demo/SKILL.md")
    in
    assert (
      not
        (C.Id.equal
           (C.Observed.id (I.Candidate.observed a))
           (C.Observed.id (I.Candidate.observed b))));
    assert (Result.is_error (load left_root actor a));
    print_s
      [%sexp
        (candidate_names first : string list)
      , (List.length (I.Discovery.issues first) : int)];
    [%test_eq: string]
      (C.Loaded.contents (load right_root actor a |> io_ok))
      (skill "demo"));
  [%expect
    {|
    (AGENTS.md a/AGENTS.md a/b/AGENTS.md)
    ((demo/SKILL.md) 1)
    |}]
;;

let%test_unit "creation order cannot affect skill observations" =
  let names = [ "charlie"; "alpha"; "bravo" ] in
  List.iter
    [ names; List.rev names; [ "bravo"; "charlie"; "alpha" ] ]
    ~f:(fun order ->
      with_roots (fun _ _ left _ _ _ root _ actor _ _ ->
        List.iter order ~f:(fun name -> save left (name ^ "/SKILL.md") (skill name));
        let observed = discover_skills root actor |> io_ok |> candidate_names in
        let expected =
          List.map names ~f:(fun name -> name ^ "/SKILL.md")
          |> List.sort ~compare:String.compare
        in
        [%test_eq: string list] observed expected))
;;

let%expect_test "load is exact, refresh is explicit, captured restore never rereads" =
  with_roots (fun _ _ left _ host raw root _ actor _ _ ->
    let original = skill "demo" in
    save left "demo/SKILL.md" original;
    let candidate =
      discover_skills root actor |> io_ok |> I.Discovery.candidates |> List.hd_exn
    in
    let retained = load root actor candidate |> io_ok in
    let stored =
      C.Snapshot.capture [ retained ] ~limits ~checkpoint:ignore
      |> content_ok
      |> C.Snapshot.to_storage
    in
    save left "demo/SKILL.md" (skill ~description:"Updated" "demo");
    (match load root actor candidate with
     | Error (Content (Stale { expected; observed })) ->
       assert (not (Chatml_source.Digest.equal expected observed))
     | Error error -> raise_s (I.Error.sexp_of_t error)
     | Ok _ -> failwith "stale load substituted current bytes");
    let refreshed =
      discover_skills root actor |> io_ok |> I.Discovery.candidates |> List.hd_exn
    in
    assert (
      not
        (Chatml_source.Digest.equal
           (C.Observed.digest (I.Candidate.observed candidate))
           (C.Observed.digest (I.Candidate.observed refreshed))));
    [%test_eq: string] (C.Loaded.contents retained) original;
    let rebound = I.bind_root host ~actor ~root:raw ~id:(root_id "left") |> io_ok in
    (match load rebound actor candidate with
     | Error Foreign_candidate -> ()
     | _ -> failwith "receipt rebound through equal label");
    Eio.Path.rmtree Eio.Path.(left / "demo");
    (match load root actor refreshed with
     | Error (Source _) -> ()
     | Error Missing -> ()
     | _ -> failwith "deleted source substituted");
    let replayed =
      C.Snapshot.of_storage stored ~limits ~checkpoint:ignore |> content_ok
    in
    [%test_eq: string list]
      (List.map (C.Snapshot.loaded replayed) ~f:C.Loaded.contents)
      [ original ];
    print_endline
      "stale rejected; refresh preserves retained bytes; deleted sources restore from \
       exact capture");
  [%expect
    {| stale rejected; refresh preserves retained bytes; deleted sources restore from exact capture |}]
;;

let%test_unit "real root grants and narrowing caps remain authoritative around yields" =
  with_roots (fun _ _ left _ host raw root _ actor denied admitted ->
    save left "demo/SKILL.md" (skill "demo");
    [%test_eq: int]
      (Sources.roots host ~actor:denied ~checkpoint:ignore |> source_ok |> List.length)
      0;
    let member = path "demo/SKILL.md" in
    assert (
      Result.is_error
        (Sources.read_root_member
           ~max_bytes:1
           host
           ~actor
           ~root:raw
           ~member
           ~checkpoint:ignore));
    assert (
      Result.is_error
        (Sources.read_root_member
           ~max_bytes:0
           host
           ~actor
           ~root:raw
           ~member
           ~checkpoint:ignore));
    assert (
      Result.is_error
        (Sources.list_root_directory
           ~max_entries:(-1)
           host
           ~actor
           ~root:raw
           ~directory:None
           ~checkpoint:ignore));
    assert (
      Result.is_error
        (Sources.list_root_directory
           ~max_entries:0
           host
           ~actor
           ~root:raw
           ~directory:None
           ~checkpoint:ignore));
    let narrowed =
      C.Limits.create
        ~max_files:1
        ~max_file_bytes:16
        ~max_total_bytes:16
        ~max_work_steps:100_000
      |> Or_error.ok_exn
    in
    let issues =
      discover_skills ~limits:narrowed root actor |> io_ok |> I.Discovery.issues
    in
    [%test_eq: int] (List.length issues) 1;
    let checkpoint () = admitted := false in
    (match discover_skills ~checkpoint root actor with
     | Error (Source Unauthorized) -> ()
     | Error (Source Expired_actor) -> ()
     | Error error -> raise_s (I.Error.sexp_of_t error)
     | Ok _ -> failwith "revoked discovery published partial result");
    admitted := true;
    [%test_eq: string list]
      (discover_skills root actor |> io_ok |> candidate_names)
      [ "demo/SKILL.md" ])
;;

let%test_unit "symlink members and governing parent escapes never become instructions" =
  with_roots (fun _ home left _ _ _ root _ actor _ _ ->
    save home "outside/SKILL.md" (skill "escape");
    Eio.Path.symlink
      ~link_to:(Eio.Path.native_exn Eio.Path.(home / "outside"))
      Eio.Path.(left / "escape");
    save left "leaf/SKILL.md" (skill "leaf");
    Eio.Path.unlink Eio.Path.(left / "leaf/SKILL.md");
    Eio.Path.symlink
      ~link_to:(Eio.Path.native_exn Eio.Path.(home / "outside/SKILL.md"))
      Eio.Path.(left / "leaf/SKILL.md");
    let discovery = discover_skills root actor |> io_ok in
    [%test_eq: int] (List.length (I.Discovery.candidates discovery)) 0;
    [%test_eq: int] (List.length (I.Discovery.issues discovery)) 2;
    assert (
      Result.is_error
        (I.discover_project
           root
           ~actor
           ~scope:(C.Scope.of_path (path "escape"))
           ~limits
           ~checkpoint:ignore));
    Eio.Path.symlink
      ~link_to:(Eio.Path.native_exn Eio.Path.(home / "outside/SKILL.md"))
      Eio.Path.(left / "AGENTS.md");
    assert (
      Result.is_error
        (I.discover_project root ~actor ~scope:C.Scope.root ~limits ~checkpoint:ignore)))
;;

let%test_unit "aggregate exhaustion and actual cancellation publish no partial discovery" =
  with_roots (fun _ _ left _ _ _ root _ actor _ _ ->
    save left "alpha/SKILL.md" "12345";
    save left "bravo/SKILL.md" "12345";
    let aggregate =
      C.Limits.create
        ~max_files:2
        ~max_file_bytes:8
        ~max_total_bytes:8
        ~max_work_steps:100_000
      |> Or_error.ok_exn
    in
    (match discover_skills ~limits:aggregate root actor with
     | Error Limit_exceeded -> ()
     | Error error -> raise_s (I.Error.sexp_of_t error)
     | Ok _ -> failwith "partial aggregate discovery published");
    let cancelled = ref false in
    (try
       Eio.Cancel.sub (fun cancel ->
         let checkpoint () =
           Eio.Cancel.cancel cancel Exit;
           Eio.Fiber.check ()
         in
         ignore (discover_skills ~checkpoint root actor : (_, _) Result.t))
     with
     | Eio.Cancel.Cancelled _ -> cancelled := true);
    assert !cancelled;
    [%test_eq: string list]
      (discover_skills root actor |> io_ok |> candidate_names)
      [ "alpha/SKILL.md"; "bravo/SKILL.md" ])
;;

let%test_unit "a discovered regular file replaced by a symlink cannot be loaded" =
  with_roots (fun _ home left _ _ _ root _ actor _ _ ->
    save left "demo/SKILL.md" (skill "demo");
    save home "outside.md" (skill ~description:"Outside secret" "demo");
    let candidate =
      discover_skills root actor |> io_ok |> I.Discovery.candidates |> List.hd_exn
    in
    let replaced = ref false in
    let checkpoint () =
      if not !replaced
      then (
        replaced := true;
        Eio.Path.unlink Eio.Path.(left / "demo/SKILL.md");
        Eio.Path.symlink
          ~link_to:(Eio.Path.native_exn Eio.Path.(home / "outside.md"))
          Eio.Path.(left / "demo/SKILL.md"))
    in
    (match I.load root ~actor candidate ~limits ~checkpoint with
     | Error (Source (Local _)) -> ()
     | Error error -> raise_s (I.Error.sexp_of_t error)
     | Ok _ -> failwith "replacement symlink content loaded");
    assert !replaced)
;;

let%test_unit
    "guidance absence is optional only for the exact leaf and scan work stays bounded"
  =
  with_roots (fun _ _ left _ _ _ root _ actor _ _ ->
    [%test_eq: string list]
      (I.discover_project root ~actor ~scope:C.Scope.root ~limits ~checkpoint:ignore
       |> io_ok
       |> candidate_names)
      [];
    assert (
      Result.is_error
        (I.discover_project
           root
           ~actor
           ~scope:(C.Scope.of_path (path "missing"))
           ~limits
           ~checkpoint:ignore));
    save left "demo/SKILL.md" (skill "demo");
    let tiny_work =
      C.Limits.create
        ~max_files:8
        ~max_file_bytes:1024
        ~max_total_bytes:4096
        ~max_work_steps:1
      |> Or_error.ok_exn
    in
    (match discover_skills ~limits:tiny_work root actor with
     | Error Limit_exceeded -> ()
     | _ -> failwith "work limit ignored");
    let tiny_count =
      C.Limits.create
        ~max_files:1
        ~max_file_bytes:1024
        ~max_total_bytes:4096
        ~max_work_steps:100_000
      |> Or_error.ok_exn
    in
    save left "other/SKILL.md" (skill "other");
    assert (Result.is_error (discover_skills ~limits:tiny_count root actor)))
;;

let%test_unit "optional source caps cannot widen existing host limits" =
  with_roots
    ~max_read_bytes:8
    ~max_browse_entries:1
    (fun _ _ left _ host raw _ _ actor _ _ ->
       save left "data.md" "123456789";
       let member = path "data.md" in
       List.iter [ None; Some 100 ] ~f:(fun maximum ->
         assert (
           Result.is_error
             (Sources.read_root_member
                ?max_bytes:maximum
                host
                ~actor
                ~root:raw
                ~member
                ~checkpoint:ignore)));
       save left "other.md" "more";
       List.iter [ None; Some 100 ] ~f:(fun maximum ->
         assert (
           Result.is_error
             (Sources.list_root_directory
                ?max_entries:maximum
                host
                ~actor
                ~root:raw
                ~directory:None
                ~checkpoint:ignore))))
;;

let%test_unit "post-read revocation suppresses stale revision disclosure" =
  with_roots (fun _ _ left _ host raw root _ actor _ admitted ->
    let original = skill "demo" in
    save left "demo/SKILL.md" original;
    let candidate =
      discover_skills root actor |> io_ok |> I.Discovery.candidates |> List.hd_exn
    in
    [%test_eq: string] (C.Loaded.contents (load root actor candidate |> io_ok)) original;
    let changed = skill ~description:"Changed while still readable" "demo" in
    save left "demo/SKILL.md" changed;
    (match load root actor candidate with
     | Error (Content (Stale _)) -> ()
     | Error error -> raise_s (I.Error.sexp_of_t error)
     | Ok _ -> failwith "expected an authorized stale-revision control");
    (* Measure the actual source owner's complete read path for the SAME current
       bytes/member/cap. The adapter adds its first pure Content.load checkpoint
       only after that read has returned and its source-grant checks completed. *)
    let read_calls = ref 0 in
    let member = path "demo/SKILL.md" in
    ignore
      (Sources.read_root_member
         ~max_bytes:(Int.min (C.Limits.file_bytes limits) (C.Limits.total_bytes limits))
         host
         ~actor
         ~root:raw
         ~member
         ~checkpoint:(fun () -> Int.incr read_calls)
       |> source_ok
       : Sources.Document_capture.t);
    assert (!read_calls > 0);
    let calls = ref 0 in
    let checkpoint () =
      Int.incr calls;
      if !calls = !read_calls + 1 then admitted := false
    in
    (match I.load root ~actor candidate ~limits ~checkpoint with
     | Error (Source Unauthorized) -> ()
     | Error error -> raise_s (I.Error.sexp_of_t error)
     | Ok _ -> failwith "revoked post-read load disclosed content");
    [%test_eq: int] !calls (!read_calls + 1);
    assert (not !admitted);
    admitted := true;
    let current =
      discover_skills root actor |> io_ok |> I.Discovery.candidates |> List.hd_exn
    in
    [%test_eq: string] (C.Loaded.contents (load root actor current |> io_ok)) changed)
;;
