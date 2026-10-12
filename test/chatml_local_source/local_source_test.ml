open Core
open Support

let%expect_test "bounded bytes and current local revision" =
  with_workspace (fun _ _ workspace root ->
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "nested");
    save Eio.Path.(workspace / "empty") "";
    save Eio.Path.(workspace / "nested" / "code.chatml") "old";
    with_owner root (fun owner ->
      assert (String.equal (read owner "empty" ~max_bytes:0 |> ok) "");
      let captured = read owner "nested/code.chatml" ~max_bytes:3 |> ok in
      assert (String.equal captured "old");
      error (read owner "nested/code.chatml" ~max_bytes:2) Read Too_large;
      error (read owner "nested/code.chatml" ~max_bytes:0) Read Too_large;
      error (read owner "empty" ~max_bytes:(-1)) Read Invalid_limit;
      save Eio.Path.(workspace / "nested" / "code.chatml") "new";
      assert (String.equal (read owner "nested/code.chatml" |> ok) "new");
      assert (String.equal captured "old");
      print_endline "exact caps; fresh read; prior bytes immutable"));
  [%expect {| exact caps; fresh read; prior bytes immutable |}]
;;

let%expect_test "fresh nonrecursive cursors, bounded entries and callback reentrancy" =
  with_workspace (fun _ _ workspace root ->
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "empty");
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "nested");
    save Eio.Path.(workspace / "nested" / "hidden.chatml") "hidden";
    save Eio.Path.(workspace / "code.chatml") "code";
    let expected =
      [ "code.chatml"; "empty"; "nested" ]
      @ List.init 260 ~f:(fun i -> sprintf "irrelevant-%03d.txt" i)
      |> List.sort ~compare:String.compare
    in
    List.iter expected ~f:(fun name ->
      if String.is_prefix name ~prefix:"irrelevant-"
      then save Eio.Path.(workspace / name) "");
    with_owner root (fun owner ->
      let count = List.length expected in
      assert (List.equal String.equal (fst (names owner ~max_entries:count)) expected);
      assert (snd (names owner ~max_entries:count) = count);
      assert (snd (names owner ~directory:"empty" ~max_entries:0) = 0);
      let limited max_entries =
        Local.fold_directory
          owner
          ~directory:None
          ~max_entries
          ~init:()
          ~checkpoint:ignore
          ~f:(fun () ~name:_ -> Ok ())
      in
      filesystem_error (limited (count - 1)) Enumerate Entry_limit;
      filesystem_error (limited 0) Enumerate Entry_limit;
      filesystem_error (limited (-1)) Enumerate Invalid_limit;
      assert (List.equal String.equal (fst (names owner)) expected);
      let visited = ref 0 in
      let _, delivered =
        Local.fold_directory
          owner
          ~directory:None
          ~max_entries:count
          ~init:()
          ~checkpoint:ignore
          ~f:(fun () ~name:_ ->
            if !visited = 0
            then (
              assert (String.equal (read owner "code.chatml" |> ok) "code");
              assert (List.equal String.equal (fst (names owner)) expected));
            incr visited;
            Ok ())
        |> admitted
      in
      assert (delivered = count && !visited = count);
      print_endline "all entries charged; repeated and reentrant cursors independent"));
  [%expect {| all entries charged; repeated and reentrant cursors independent |}]
;;

let%expect_test "exact missing leaf and kernel no-follow regular-file admission" =
  with_workspace (fun _ anchor workspace root ->
    save Eio.Path.(anchor / "outside") "outside authority";
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "directory");
    Eio.Path.symlink ~link_to:"../outside" Eio.Path.(workspace / "leaf-link");
    Eio.Path.symlink ~link_to:".." Eio.Path.(workspace / "parent-link");
    let native = Eio.Path.native_exn workspace in
    Eio_unix.run_in_systhread (fun () ->
      Core_unix.mkfifo (Filename.concat native "fifo") ~perm:0o644);
    with_owner root (fun owner ->
      error (read owner "absent.chatmli") Open_file Missing;
      error (read owner "absent/child.chatmli") Open_parent Missing;
      error (read owner "directory") Open_file Unsupported_kind;
      error (read owner "fifo") Open_file Unsupported_kind;
      error (read owner "leaf-link") Open_file Symlink_or_non_directory;
      error (read owner "parent-link/outside") Open_parent Symlink_or_non_directory;
      print_endline "only exact leaf absence is optional; links and non-files rejected"));
  [%expect {| only exact leaf absence is optional; links and non-files rejected |}]
;;

let%expect_test "replacement at every read boundary never follows outside authority" =
  with_workspace (fun _ anchor workspace root ->
    let inside = String.make 9000 'i' in
    save Eio.Path.(anchor / "outside") (String.make 9000 'o');
    let leaf = Eio.Path.(workspace / "code.chatml") in
    save leaf inside;
    with_owner root (fun owner ->
      let polls = ref 0 in
      assert (
        String.equal
          (read owner "code.chatml" ~checkpoint:(fun () -> incr polls) |> ok)
          inside);
      for stop = 1 to !polls do
        let visited = ref 0 in
        let replaced = ref false in
        let outcome =
          read owner "code.chatml" ~checkpoint:(fun () ->
            incr visited;
            if !visited = stop
            then (
              Eio.Path.unlink leaf;
              Eio.Path.symlink ~link_to:"../outside" leaf;
              replaced := true))
        in
        assert !replaced;
        retained_or_rejected outcome ~inside;
        Eio.Path.unlink leaf;
        save leaf inside
      done;
      assert (String.equal (read owner "code.chatml" |> ok) inside);
      print_endline
        "replacement rejects or retains the opened object; never outside bytes"));
  [%expect {| replacement rejects or retains the opened object; never outside bytes |}]
;;

let%expect_test "all checkpoint exceptions join cleanup before return and owner reuse" =
  with_workspace (fun env _ workspace root ->
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "nested");
    let bytes = String.make 9000 'c' in
    save Eio.Path.(workspace / "nested" / "code.chatml") bytes;
    save Eio.Path.(workspace / "plain") "plain";
    with_owner root (fun owner ->
      let expected_names, expected_count = names owner in
      let reuse () =
        assert (String.equal (read owner "nested/code.chatml" |> ok) bytes);
        let actual, count = names owner in
        assert (List.equal String.equal actual expected_names && count = expected_count)
      in
      reuse ();
      interrupt_each ~env ~reuse ~run:(fun checkpoint ->
        assert (String.equal (read owner "nested/code.chatml" ~checkpoint |> ok) bytes));
      interrupt_each ~env ~reuse ~run:(fun checkpoint ->
        let actual, count = names owner ~checkpoint in
        assert (List.equal String.equal actual expected_names && count = expected_count));
      print_endline "exception identity; no live descriptor leak; same owner reusable"));
  [%expect {| exception identity; no live descriptor leak; same owner reusable |}]
;;

let%expect_test "whole root acquisition interruption releases before caller reuse" =
  with_workspace (fun env _ workspace root ->
    save Eio.Path.(workspace / "code.chatml") "code";
    let reuse () =
      with_owner root (fun owner ->
        assert (String.equal (read owner "code.chatml" |> ok) "code"))
    in
    reuse ();
    interrupt_each ~env ~reuse ~run:(fun checkpoint ->
      Local.with_root root ~max_path_components:8 ~checkpoint ~f:(fun owner ->
        assert (String.equal (read owner "code.chatml" |> ok) "code");
        Ok ())
      |> admitted);
    assert (String.equal (Eio.Path.load Eio.Path.(root / "code.chatml")) "code");
    print_endline
      "root interruption preserves identity; no leak; caller capability reusable");
  [%expect
    {| root interruption preserves identity; no leak; caller capability reusable |}]
;;

let%expect_test "parent replacement at every boundary never widens acquired authority" =
  with_workspace (fun _ anchor workspace root ->
    let inside = String.make 9000 'i' in
    let parent = Eio.Path.(workspace / "parent") in
    let retained = Eio.Path.(workspace / "retained-parent") in
    let outside = Eio.Path.(anchor / "outside-parent") in
    Eio.Path.mkdir ~perm:0o755 parent;
    Eio.Path.mkdir ~perm:0o755 outside;
    save Eio.Path.(parent / "code.chatml") inside;
    save Eio.Path.(outside / "code.chatml") (String.make 9000 'o');
    with_owner root (fun owner ->
      let polls = ref 0 in
      assert (
        String.equal
          (read owner "parent/code.chatml" ~checkpoint:(fun () -> incr polls) |> ok)
          inside);
      for stop = 1 to !polls do
        let visited = ref 0 in
        let replaced = ref false in
        Fun.protect
          ~finally:(fun () ->
            if !replaced
            then (
              Eio.Path.unlink parent;
              Eio.Path.rename retained parent))
          (fun () ->
             let outcome =
               read owner "parent/code.chatml" ~checkpoint:(fun () ->
                 incr visited;
                 if !visited = stop
                 then (
                   Eio.Path.rename parent retained;
                   Eio.Path.symlink ~link_to:"../outside-parent" parent;
                   replaced := true))
             in
             assert !replaced;
             retained_or_rejected outcome ~inside)
      done;
      assert (String.equal (read owner "parent/code.chatml" |> ok) inside);
      print_endline "parent swap rejects or keeps retained directory; never outside bytes"));
  [%expect {| parent swap rejects or keeps retained directory; never outside bytes |}]
;;

let%expect_test "bracket close waits for admitted work and rejects new operations" =
  with_workspace (fun env _ workspace root ->
    save Eio.Path.(workspace / "code.chatml") "code";
    (* Warm acquisition before comparing descriptor counts; no GC is forced. *)
    with_owner root (fun owner -> assert (snd (names owner) = 1));
    let before = descriptor_count env in
    Eio.Switch.run (fun sw ->
      let entered, signal_entered = Eio.Promise.create () in
      let release, signal_release = Eio.Promise.create () in
      let returning, signal_returning = Eio.Promise.create () in
      let finished, signal_finished = Eio.Promise.create () in
      let closed, signal_closed = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Local.with_root root ~max_path_components:8 ~checkpoint:ignore ~f:(fun owner ->
          (* This worker belongs to the caller's switch, so it can stay active
             while the acquisition bracket's callback returns. *)
          Eio.Fiber.fork ~sw (fun () ->
            let (), count =
              Local.fold_directory
                owner
                ~directory:None
                ~max_entries:1
                ~init:()
                ~checkpoint:ignore
                ~f:(fun () ~name ->
                  assert (String.equal name "code.chatml");
                  Eio.Promise.resolve signal_entered owner;
                  Eio.Promise.await release;
                  Ok ())
              |> admitted
            in
            assert (count = 1);
            Eio.Promise.resolve signal_finished ());
          ignore (Eio.Promise.await entered : Local.t);
          Eio.Promise.resolve signal_returning ();
          Ok ())
        |> admitted;
        assert (Eio.Promise.is_resolved finished);
        Eio.Promise.resolve signal_closed ());
      let owner = Eio.Promise.await entered in
      Eio.Promise.await returning;
      (* Give the returning fiber its turn to enter bracket cleanup. The
         visitor stays blocked on release: no sleeps or wall-clock deadline. *)
      Eio.Fiber.yield ();
      Fun.protect
        ~finally:(fun () -> Eio.Promise.resolve signal_release ())
        (fun () ->
           assert (not (Eio.Promise.is_resolved finished));
           assert (not (Eio.Promise.is_resolved closed));
           error (read owner "code.chatml") Read Closed);
      Eio.Promise.await finished;
      Eio.Promise.await closed;
      error (read owner "code.chatml") Read Closed);
    assert (descriptor_count env = before);
    assert (String.equal (Eio.Path.load Eio.Path.(root / "code.chatml")) "code");
    print_endline
      "closing rejects admission; active callback and cleanup finish before release");
  [%expect
    {| closing rejects admission; active callback and cleanup finish before release |}]
;;

let%expect_test
    "callback failures preserve identity and no-op checkpoint preserves Eio cancellation"
  =
  with_workspace (fun env _ workspace root ->
    save Eio.Path.(workspace / "code.chatml") "code";
    with_owner root (fun owner ->
      ignore (names owner : string list * int);
      let before = descriptor_count env in
      let rejection = ref 7 in
      (match
         Local.fold_directory
           owner
           ~directory:None
           ~max_entries:1
           ~init:()
           ~checkpoint:ignore
           ~f:(fun () ~name:_ -> Error rejection)
       with
       | Error (`Callback value) -> assert (phys_equal rejection value)
       | Ok _ | Error (`Filesystem _) -> failwith "callback result was replaced");
      let token = Failure "directory callback" in
      (try
         ignore
           (Local.fold_directory
              owner
              ~directory:None
              ~max_entries:1
              ~init:()
              ~checkpoint:ignore
              ~f:(fun () ~name:_ -> raise token)
            : (unit * int, [ `Filesystem of Local.Error.t | `Callback of unit ]) Result.t);
         failwith "callback exception was swallowed"
       with
       | exception_ -> assert (phys_equal token exception_));
      let published = ref false in
      (try
         Eio.Switch.run (fun sw ->
           Eio.Switch.fail sw Exit;
           match read owner "code.chatml" ~checkpoint:ignore with
           | Error _ -> ()
           | Ok _ -> published := true)
       with
       | Exit | Eio.Cancel.Cancelled Exit -> ());
      assert (not !published);
      assert (descriptor_count env = before);
      assert (String.equal (read owner "code.chatml" |> ok) "code");
      assert (snd (names owner) = 1);
      print_endline
        "callback unchanged; cancellation before publication; cleanup and reuse"));
  [%expect {| callback unchanged; cancellation before publication; cleanup and reuse |}]
;;

let%expect_test "bracket release closes escaped owner but retains caller root authority" =
  with_workspace (fun _ _ workspace root ->
    save Eio.Path.(workspace / "code.chatml") "code";
    let owner = with_owner root Fn.id in
    error (read owner "code.chatml") Read Closed;
    assert (String.equal (Eio.Path.load Eio.Path.(root / "code.chatml")) "code");
    filesystem_error
      (Local.with_root
         Eio.Path.(root / "directory")
         ~max_path_components:8
         ~checkpoint:ignore
         ~f:(fun _ -> Ok ()))
      Borrow_root
      Not_retained_directory;
    filesystem_error
      (Local.with_root root ~max_path_components:0 ~checkpoint:ignore ~f:(fun _ -> Ok ()))
      Borrow_root
      Invalid_limit;
    print_endline "escaped owner closed; input capability remains live");
  [%expect {| escaped owner closed; input capability remains live |}]
;;
