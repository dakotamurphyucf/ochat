open Core
open Support

let%expect_test "declaring package siblings and explicit installed anchor" =
  with_workspace (fun _ workspace root ->
    List.iter [ "app"; "shared"; "installed" ] ~f:(fun dir ->
      Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / dir);
      save Eio.Path.(workspace / dir / "marker") dir);
    Eio.Path.with_open_dir
      Eio.Path.(workspace / "installed")
      (fun installed ->
         let anchors = [ anchor root "/work"; anchor installed "/opt/lib" ] in
         Roots.with_anchors anchors ~limits:(limits ()) ~checkpoint:ignore ~f:(fun t ->
           let app = resolve t "/work/app" |> ok_roots in
           let sibling = resolve ~base:app t "../shared" |> ok_roots in
           let installed = resolve ~base:app t "/opt/lib" |> ok_roots in
           print_endline ("sibling: " ^ read (Roots.Cursor.owner sibling));
           print_endline ("installed: " ^ read (Roots.Cursor.owner installed));
           code (resolve ~base:app t "../../private") Outside_grants;
           code (resolve t "/work-other") Outside_grants;
           code (resolve t "relative") Invalid_locator;
           let before = Roots.Accounting.retained_roots (Roots.accounting t) in
           ignore (resolve t "/work/shared" |> ok_roots : Roots.Cursor.t);
           assert (Roots.Accounting.retained_roots (Roots.accounting t) = before);
           Ok ())
         |> admitted));
  [%expect
    {|
    sibling: shared
    installed: installed
    |}]
;;

(* Handwritten normalization model: stack operations over split component lists,
   independent of the production byte scanner and grant-prefix implementation. *)
let model input =
  let rec fold stack = function
    | [] -> Some (List.rev stack)
    | "" :: tail | "." :: tail -> fold stack tail
    | ".." :: tail ->
      (match stack with
       | [] -> None
       | _ :: rest -> fold rest tail)
    | part :: tail -> fold (part :: stack) tail
  in
  fold [] (String.split input ~on:'/')
;;

let%test_unit "finite locator reference model and grant permutations" =
  with_workspace (fun _ workspace root ->
    let rec make prefix depth =
      save
        Eio.Path.(workspace / String.concat ~sep:"/" (prefix @ [ "marker" ]))
        ("/" ^ String.concat ~sep:"/" prefix);
      if depth > 0
      then
        List.iter [ "a"; "b" ] ~f:(fun part ->
          let next = prefix @ [ part ] in
          Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / String.concat ~sep:"/" next);
          make next (depth - 1))
    in
    make [] 2;
    let rec words n =
      if n = 0
      then [ [] ]
      else
        List.concat_map [ "a"; "b"; "."; ".."; "" ] ~f:(fun part ->
          List.map (words (n - 1)) ~f:(fun tail -> part :: tail))
    in
    Roots.with_anchors
      [ anchor root "/" ]
      ~limits:(limits ())
      ~checkpoint:ignore
      ~f:(fun t ->
        List.iter (words 4) ~f:(fun parts ->
          let input = "/" ^ String.concat ~sep:"/" parts in
          match model input, resolve t input with
          | None, result -> code result Invalid_locator
          | Some normalized, Ok cursor ->
            assert (List.length normalized <= 2);
            assert (
              String.equal
                (read (Roots.Cursor.owner cursor))
                ("/" ^ String.concat ~sep:"/" normalized))
          | Some normalized, Error failure ->
            assert (List.length normalized > 2);
            assert (Roots.Error.equal_code (Roots.Error.code failure) Filesystem));
        Ok ())
    |> admitted;
    Eio.Path.with_open_dir
      Eio.Path.(workspace / "b")
      (fun nested ->
         (* The nested grant deliberately maps to a different physical subtree.
            Choosing the broader grant would return /a/b instead of /b/b. *)
         List.iter
           [ [ anchor root "/work"; anchor nested "/work/a" ]
           ; [ anchor nested "/work/a"; anchor root "/work" ]
           ]
           ~f:(fun anchors ->
             Roots.with_anchors
               anchors
               ~limits:(limits ())
               ~checkpoint:ignore
               ~f:(fun t ->
                 assert (
                   String.equal
                     (read (Roots.Cursor.owner (resolve t "/work/a/b" |> ok_roots)))
                     "/b/b");
                 Ok ())
             |> admitted)))
;;

let%test_unit "different grants same live object and no implicit ancestor grant" =
  with_workspace (fun _ workspace root ->
    List.iter [ "app"; "shared" ] ~f:(fun dir ->
      Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / dir);
      save Eio.Path.(workspace / dir / "marker") dir);
    Eio.Path.with_open_dir
      Eio.Path.(workspace / "app")
      (fun app ->
         Eio.Path.with_open_dir
           Eio.Path.(workspace / "shared")
           (fun shared ->
              Roots.with_anchors
                [ anchor app "/work/app"
                ; anchor shared "/work/shared"
                ; anchor shared "/installed"
                ]
                ~limits:(limits ())
                ~checkpoint:ignore
                ~f:(fun t ->
                  let app = resolve t "/work/app" |> ok_roots in
                  let one = resolve ~base:app t "../shared" |> ok_roots in
                  let two = resolve t "/installed" |> ok_roots in
                  assert (
                    Local.Directory_identity.equal
                      (Roots.Cursor.identity one)
                      (Roots.Cursor.identity two));
                  assert (
                    not
                      (Local.Directory_identity.equal
                         (Roots.Cursor.identity one)
                         (Roots.Cursor.identity app)));
                  code (resolve ~base:app t "..") Outside_grants;
                  Ok ())
              |> admitted)))
;;

let%test_unit "root limits, duplicate grants, foreign cursors and reentrancy" =
  with_workspace (fun _ workspace root ->
    save Eio.Path.(workspace / "marker") "root";
    (match
       Roots.with_anchors
         [ anchor root "/a"; anchor root "/a/." ]
         ~limits:(limits ())
         ~checkpoint:ignore
         ~f:(fun _ -> failwith "duplicate admitted")
     with
     | Error (`Roots e) ->
       assert (Roots.Error.equal_code (Roots.Error.code e) Duplicate_anchor)
     | _ -> failwith "expected duplicate");
    let retained = ref None in
    Roots.with_anchors
      [ anchor root "/a" ]
      ~limits:(limits ~roots:1 ())
      ~checkpoint:ignore
      ~f:(fun t ->
        let a = resolve t "/a" |> ok_roots in
        retained := Some (t, a);
        code (resolve t "/a/missing") Limit_exceeded;
        let recursive = ref false in
        ignore
          (resolve t "/a" ~checkpoint:(fun () ->
             recursive := true;
             code (resolve t "/a") Busy)
           |> ok_roots
           : Roots.Cursor.t);
        assert !recursive;
        code (resolve t (String.make 2049 'x')) Limit_exceeded;
        Ok ())
    |> admitted;
    let old, cursor = Option.value_exn !retained in
    code (resolve old "/a") Closed;
    Roots.with_anchors
      [ anchor root "/a" ]
      ~limits:(limits ())
      ~checkpoint:ignore
      ~f:(fun t ->
        code (resolve ~base:cursor t "/a") Foreign_cursor;
        Ok ())
    |> admitted;
    List.iter
      [ limits ~anchors:0 (); limits ~roots:0 (); limits ~work:0 () ]
      ~f:(fun limits ->
        match
          Roots.with_anchors
            [ anchor root "/a" ]
            ~limits
            ~checkpoint:ignore
            ~f:(fun _ -> failwith "zero allowance admitted")
        with
        | Error (`Roots e) ->
          assert (Roots.Error.equal_code (Roots.Error.code e) Limit_exceeded)
        | _ -> failwith "expected limit"))
;;

let%test_unit "whole-attempt interruption and replacement confinement" =
  with_workspace (fun env workspace root ->
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "package");
    save Eio.Path.(workspace / "package" / "marker") "inside";
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "outside");
    save Eio.Path.(workspace / "outside" / "marker") "outside";
    let run checkpoint =
      Roots.with_anchors
        [ anchor root "/work" ]
        ~limits:(limits ())
        ~checkpoint
        ~f:(fun t ->
          ignore (resolve t "/work/package" ~checkpoint |> ok_roots : Roots.Cursor.t);
          Ok ())
      |> admitted
    in
    interrupt_each ~env run;
    let polls = ref 0 in
    run (fun () -> incr polls);
    for stop = 1 to !polls do
      let seen = ref 0 in
      let swapped = ref false in
      Roots.with_anchors
        [ anchor root "/work" ]
        ~limits:(limits ())
        ~checkpoint:(fun () -> incr seen)
        ~f:(fun t ->
          let result =
            resolve t "/work/package" ~checkpoint:(fun () ->
              incr seen;
              if !seen = stop
              then (
                swapped := true;
                Eio.Path.rename
                  Eio.Path.(workspace / "package")
                  Eio.Path.(workspace / "saved");
                Eio.Path.symlink ~link_to:"outside" Eio.Path.(workspace / "package")))
          in
          (match result with
           | Ok cursor ->
             assert (String.equal (read (Roots.Cursor.owner cursor)) "inside")
           | Error _ -> ());
          Ok ())
      |> admitted;
      if !swapped
      then (
        Eio.Path.unlink Eio.Path.(workspace / "package");
        Eio.Path.rename Eio.Path.(workspace / "saved") Eio.Path.(workspace / "package"))
    done)
;;

let%test_unit "hand-computed reservation and locator admission boundaries" =
  with_workspace (fun _ workspace root ->
    save Eio.Path.(workspace / "marker") "root";
    (* One '/' anchor: (1+1)*8 +16 =32 units. Resolving '/' adds
       8*1+8 =16 and 4*1 +1*(4*1+8) +(1+1) =18: total66.
       These handwritten numbers never use measured implementation counters. *)
    List.iter [ 32; 31 ] ~f:(fun work ->
      let called = ref false in
      let outcome =
        Roots.with_anchors
          [ anchor root "/" ]
          ~limits:(limits ~work ())
          ~checkpoint:ignore
          ~f:(fun _ ->
            called := true;
            Ok ())
      in
      if work = 32
      then (
        ignore (admitted outcome : unit);
        assert !called)
      else (
        assert (not !called);
        match outcome with
        | Error (`Roots e) ->
          assert (Roots.Error.equal_code (Roots.Error.code e) Limit_exceeded)
        | _ -> failwith "expected pre-acquisition limit"));
    List.iter [ 66; 65 ] ~f:(fun work ->
      Roots.with_anchors
        [ anchor root "/" ]
        ~limits:(limits ~work ~bytes:1 ~components:0 ~anchors:1 ~roots:1 ())
        ~checkpoint:ignore
        ~f:(fun t ->
          let outcome = resolve t "/" in
          if work = 66
          then assert (String.equal (read (Roots.Cursor.owner (ok_roots outcome))) "root")
          else code outcome Limit_exceeded;
          code (resolve t "//") Limit_exceeded;
          Ok ())
      |> admitted))
;;

let%test_unit "fresh attempt observes replaced route while retained cursor stays old" =
  with_workspace (fun _ workspace root ->
    Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "package");
    save Eio.Path.(workspace / "package" / "marker") "old";
    let anchors = [ anchor root "/work" ] in
    Roots.with_anchors anchors ~limits:(limits ()) ~checkpoint:ignore ~f:(fun old ->
      let retained = resolve old "/work/package" |> ok_roots in
      Eio.Path.rename Eio.Path.(workspace / "package") Eio.Path.(workspace / "saved");
      Eio.Path.mkdir ~perm:0o755 Eio.Path.(workspace / "package");
      save Eio.Path.(workspace / "package" / "marker") "new";
      Roots.with_anchors anchors ~limits:(limits ()) ~checkpoint:ignore ~f:(fun fresh ->
        let current = resolve fresh "/work/package" |> ok_roots in
        assert (String.equal (read (Roots.Cursor.owner current)) "new");
        assert (String.equal (read (Roots.Cursor.owner retained)) "old");
        assert (
          String.equal
            (read (Roots.Cursor.owner (resolve old "/work/package" |> ok_roots)))
            "old");
        Ok ())
      |> admitted;
      Ok ())
    |> admitted)
;;
