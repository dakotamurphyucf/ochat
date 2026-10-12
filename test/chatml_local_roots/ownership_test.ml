open Core
open Support
module S = Chatml_source
module Capture = Chatml_local_capture
module Project = Chatml_project

let local_code result operation code =
  match result with
  | Ok _ -> failwith "expected native owner failure"
  | Error e ->
    assert (Local.Error.equal_operation (Local.Error.operation e) operation);
    assert (Local.Error.equal_code (Local.Error.code e) code)
;;

let%test_unit "independent subroot lifetime, clone identity and repeated cursors" =
  with_workspace (fun _ workspace root ->
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "child");
    save Eio.Path.(workspace / "child" / "marker") "child";
    Eio.Switch.run (fun child_sw ->
      let parent_seen = ref None in
      let child =
        Local.with_root root ~max_path_components:16 ~checkpoint:ignore ~f:(fun parent ->
          parent_seen := Some parent;
          let clone =
            Local.open_subroot parent ~sw:child_sw ~directory:(path []) ~checkpoint:ignore
            |> ok_local
          in
          assert (
            Local.Directory_identity.equal
              (Local.identity clone ~checkpoint:ignore |> ok_local)
              (Local.identity parent ~checkpoint:ignore |> ok_local));
          Local.open_subroot
            parent
            ~sw:child_sw
            ~directory:(path [ "child" ])
            ~checkpoint:ignore
          |> Result.map_error ~f:(fun _ -> ()))
      in
      let child =
        match child with
        | Ok value -> value
        | Error _ -> failwith "subroot rejected"
      in
      local_code
        (Local.identity (Option.value_exn !parent_seen) ~checkpoint:ignore)
        Observe_directory
        Closed;
      Eio.Path.rename Eio.Path.(workspace / "child") Eio.Path.(workspace / "moved");
      assert (String.equal (read child) "child");
      let names () =
        Local.fold_directory
          child
          ~directory:None
          ~max_entries:1
          ~init:[]
          ~checkpoint:ignore
          ~f:(fun names ~name -> Ok (name :: names))
      in
      List.iter
        [ names (); names () ]
        ~f:(function
          | Ok (values, count) ->
            assert (count = 1 && List.equal String.equal values [ "marker" ])
          | Error _ -> failwith "cursor failed"));
    let retained = ref None in
    Eio.Switch.run (fun sw ->
      retained
      := Some
           (Local.open_root root ~sw ~max_path_components:16 ~checkpoint:ignore
            |> ok_local));
    local_code
      (Local.identity (Option.value_exn !retained) ~checkpoint:ignore)
      Observe_directory
      Closed)
;;

let%test_unit "switch close joins admitted child operation" =
  with_workspace (fun _ workspace root ->
    save Eio.Path.(workspace / "marker") "value";
    Eio.Switch.run (fun caller_sw ->
      let entered, signal_entered = Eio.Promise.create () in
      let release, signal_release = Eio.Promise.create () in
      let returning, signal_returning = Eio.Promise.create () in
      let finished, signal_finished = Eio.Promise.create () in
      let closed, signal_closed = Eio.Promise.create () in
      Eio.Fiber.fork ~sw:caller_sw (fun () ->
        Eio.Switch.run (fun child_sw ->
          let owner =
            Local.open_root root ~sw:child_sw ~max_path_components:16 ~checkpoint:ignore
            |> ok_local
          in
          Eio.Fiber.fork ~sw:caller_sw (fun () ->
            (match
               Local.fold_directory
                 owner
                 ~directory:None
                 ~max_entries:1
                 ~init:()
                 ~checkpoint:ignore
                 ~f:(fun () ~name ->
                   assert (String.equal name "marker");
                   Eio.Promise.resolve signal_entered owner;
                   Eio.Promise.await release;
                   Ok ())
             with
             | Ok ((), count) -> assert (count = 1)
             | Error _ -> failwith "active fold failed");
            Eio.Promise.resolve signal_finished ());
          ignore (Eio.Promise.await entered : Local.t);
          Eio.Promise.resolve signal_returning ());
        assert (Eio.Promise.is_resolved finished);
        Eio.Promise.resolve signal_closed ());
      let owner = Eio.Promise.await entered in
      Eio.Promise.await returning;
      Eio.Fiber.yield ();
      Fun.protect
        ~finally:(fun () -> Eio.Promise.resolve signal_release ())
        (fun () ->
           assert (not (Eio.Promise.is_resolved closed));
           local_code
             (Local.read owner ~member:(member "marker") ~max_bytes:10 ~checkpoint:ignore)
             Read
             Closed);
      Eio.Promise.await closed))
;;

let%test_unit "native path validation, cancellation cleanup and explicit Eio cancellation"
  =
  assert (
    Result.is_error
      (Local.Directory_path.create
         [ ".." ]
         ~max_bytes:10
         ~max_components:2
         ~checkpoint:ignore));
  assert (
    Result.is_error
      (Local.Directory_path.create
         [ "a/b" ]
         ~max_bytes:10
         ~max_components:2
         ~checkpoint:ignore));
  assert (
    Result.is_error
      (Local.Directory_path.create
         [ String.make 1025 'a' ]
         ~max_bytes:2000
         ~max_components:2
         ~checkpoint:ignore));
  ignore (path [ "native:directory"; "back\\slash" ] : Local.Directory_path.t);
  with_workspace (fun env workspace root ->
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "child");
    save Eio.Path.(workspace / "child" / "marker") "value";
    let run checkpoint =
      Eio.Switch.run (fun sw ->
        let owner =
          Local.open_root root ~sw ~max_path_components:16 ~checkpoint |> ok_local
        in
        let child =
          Local.open_subroot owner ~sw ~directory:(path [ "child" ]) ~checkpoint
          |> ok_local
        in
        ignore (Local.identity child ~checkpoint |> ok_local : Local.Directory_identity.t))
    in
    interrupt_each ~env run;
    let entered, signal_entered = Eio.Promise.create () in
    let cancelled = ref false in
    let cancellation = Failure "cancel-native" in
    (try
       Eio.Switch.run (fun sw ->
         Eio.Fiber.fork ~sw (fun () ->
           Eio.Promise.await entered;
           Eio.Switch.fail sw cancellation);
         let owner =
           Local.open_root root ~sw ~max_path_components:16 ~checkpoint:ignore |> ok_local
         in
         ignore
           (Local.open_subroot
              owner
              ~sw
              ~directory:(path [ "child" ])
              ~checkpoint:(fun () ->
                if not (Eio.Promise.is_resolved entered)
                then Eio.Promise.resolve signal_entered ();
                Eio.Fiber.yield ())
            : (Local.t, Local.Error.t) Result.t))
     with
     | Eio.Cancel.Cancelled reason ->
       assert (phys_equal reason cancellation);
       cancelled := true
     | exception_ ->
       assert (phys_equal exception_ cancellation);
       cancelled := true);
    assert !cancelled;
    run ignore)
;;

let source namespace path contents =
  let id =
    S.Id.create ~package_namespace:namespace ~relative_path:path |> Or_error.ok_exn
  in
  S.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let%test_unit "reidentification preserves bytes and revision spans without forging owners"
  =
  let old = source "acquired" "chatml.project.json" "a\r\nb\n\255" in
  let renamed_id =
    S.Id.create ~package_namespace:"declared" ~relative_path:"chatml.project.json"
    |> Or_error.ok_exn
  in
  let renamed = S.reidentify old ~id:renamed_id in
  assert (String.equal (S.contents old) (S.contents renamed));
  assert (S.Digest.equal (S.digest old) (S.digest renamed));
  assert (S.Id.equal (S.id old) (S.id (S.reidentify old ~id:(S.id old))));
  for left = 0 to S.byte_length old do
    for right = left to S.byte_length old do
      let previous =
        S.locate old ~start_byte:left ~end_byte:right ~checkpoint:ignore
        |> Or_error.ok_exn
      in
      let current =
        S.locate renamed ~start_byte:left ~end_byte:right ~checkpoint:ignore
        |> Or_error.ok_exn
      in
      assert (Result.is_error (S.read renamed previous));
      assert (
        String.equal
          (S.read renamed current |> Or_error.ok_exn)
          (S.read old previous |> Or_error.ok_exn));
      assert (S.Id.equal (S.Location.source current) renamed_id)
    done
  done
;;

let capture_limits components =
  Capture.Limits.create
    ~max_directory_entries:10
    ~max_sources:10
    ~max_source_bytes:4096
    ~max_total_source_bytes:16384
    ~max_path_components:components
    ~max_work_steps:1_000_000
  |> Or_error.ok_exn
;;

let capture_ok = function
  | Ok value -> value
  | Error e -> raise_s [%sexp (Capture.Error.problem e : Capture.Error.problem)]
;;

let%expect_test
    "legacy capture and acquired-owner capture agree; fresh bytes stay independent"
  =
  with_workspace (fun _ workspace root ->
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "src");
    save Eio.Path.(workspace / "src" / "a.chatml") "old";
    save Eio.Path.(workspace / "entry.chatml") "entry";
    let declaration =
      source
        "pkg"
        "chatml.project.json"
        {|{"schema_version":1,"namespace":"pkg","source_dirs":["src"],"entries":{"main":{"source":"entry.chatml","target":"one_off_v1"}}}|}
    in
    let project =
      match
        Project.decode
          declaration
          ~limits:Document_schema.Limits.default
          ~checkpoint:ignore
      with
      | Ok value -> value
      | Error _ -> failwith "fixture decode"
    in
    let request = Project.Selection.Entry { name = "main"; environment = None } in
    let legacy =
      Capture.capture project ~root ~request ~limits:(capture_limits 8) ~checkpoint:ignore
      |> capture_ok
    in
    Eio.Switch.run (fun sw ->
      let owner =
        Local.open_root root ~sw ~max_path_components:8 ~checkpoint:ignore |> ok_local
      in
      let captured =
        Capture.capture_from_owner
          project
          ~owner
          ~request
          ~limits:(capture_limits 8)
          ~checkpoint:ignore
        |> capture_ok
      in
      let facts result =
        List.map (Capture.sources result) ~f:(fun s ->
          S.Id.relative_path (S.id s), S.contents s)
      in
      assert (
        List.equal
          (fun (pa, ba) (pb, bb) -> String.equal pa pb && String.equal ba bb)
          (facts legacy)
          (facts captured));
      assert (
        Capture.Accounting.work_steps (Capture.accounting legacy)
        = Capture.Accounting.work_steps (Capture.accounting captured) + 6);
      save Eio.Path.(workspace / "src" / "a.chatml") "fresh";
      let fresh =
        Capture.capture_from_owner
          project
          ~owner
          ~request
          ~limits:(capture_limits 8)
          ~checkpoint:ignore
        |> capture_ok
      in
      let find result =
        List.find_exn (Capture.sources result) ~f:(fun s ->
          String.equal (S.Id.relative_path (S.id s)) "src/a.chatml")
        |> S.contents
      in
      print_endline ("old: " ^ find captured);
      print_endline ("fresh: " ^ find fresh);
      assert (
        Result.is_error
          (Capture.capture_from_owner
             project
             ~owner
             ~request
             ~limits:(capture_limits 9)
             ~checkpoint:ignore))));
  [%expect
    {|
    old: old
    fresh: fresh
    |}]
;;
