open Core
module G = Chatml_git_catalog
module H = Chatml_git_host
open Fixture

let capture_one r =
  let open Result.Let_syntax in
  let%bind d = declaration r "pkg" in
  G.Reader.capture r d ~request
;;

let limit_error expected = function
  | Error (`Callback e) ->
    (match G.Error.problem e with
     | Limit_exceeded actual -> assert (G.Error.equal_limit expected actual)
     | _ -> failwith "wrong semantic limit")
  | Ok _ | Error (`Host _) -> failwith "semantic cap did not reject"
;;

let%test_unit "exact source, object, tree and work bounds admit before capture" =
  Support.with_workspace (fun env anchor _ _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env) in
    let bytes =
      String.length (project ~namespace:"alpha")
      + String.length old_body
      + String.length "val meaning : int\n"
    in
    let capture limits =
      Support.acquire
        store
        (repository fixture)
        (old_pin fixture)
        ~limits:(Support.limits ())
        ~f:(fun repo -> reader ~limits repo ~f:capture_one)
    in
    let (_, accounting), _ =
      capture (limits ~objects:6 ~entries:10 ~sources:3 ~total_source_bytes:bytes ())
      |> native_ok
    in
    assert (G.Accounting.object_count accounting = 6);
    assert (G.Accounting.tree_entries accounting = 10);
    assert (G.Accounting.source_count accounting = 3);
    assert (G.Accounting.source_bytes accounting = bytes);
    List.iter
      [ limits ~objects:5 (), G.Error.Object_count
      ; limits ~entries:9 (), Tree_entries
      ; limits ~sources:2 (), Source_count
      ; limits ~total_source_bytes:(bytes - 1) (), Total_source_bytes
      ; ( limits ~source_bytes:(String.length (project ~namespace:"alpha") - 1) ()
        , Source_bytes )
      ; limits ~work:1 (), Work_steps
      ]
      ~f:(fun (limits, expected) -> limit_error expected (capture limits));
    assert (
      Result.is_error
        (capture
           (limits ~total_object_bytes:(G.Accounting.object_bytes accounting - 1) ())));
    assert (Result.is_error (capture (limits ~object_bytes:1 ()))))
;;

let%test_unit "translated over-component paths issue zero raw queries" =
  Support.with_workspace (fun env anchor _ _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env) in
    ignore
      (Support.acquire
         store
         (repository fixture)
         (old_pin fixture)
         ~limits:(Support.limits ())
         ~f:(fun repo -> reader repo ~f:capture_one)
       |> native_ok);
    let rejected, receipt =
      H.with_cached_repository
        store
        ~repository:(Support.native (repository fixture))
        ~pin:(old_pin fixture)
        ~limits:(Support.limits ~payload:0 ())
        ~checkpoint:ignore
        ~f:(fun repo ->
          match
            reader ~limits:(limits ~components:1 ()) repo ~f:(fun r ->
              declaration r "pkg")
          with
          | Error e ->
            (match G.Error.problem e with
             | Limit_exceeded Path_components -> Ok true
             | _ -> failwith "wrong path bound")
          | Ok _ -> failwith "over-component member admitted")
      |> native_ok
    in
    assert rejected;
    assert (H.Receipt.output_bytes receipt = 0))
;;

let%test_unit
    "selected or traversed links reject while conventional absent interface stays absent"
  =
  List.iter
    [ `Source Mode.Symlink
    ; `Source Mode.Gitlink
    ; `Ancestor Mode.Symlink
    ; `Ancestor Mode.Gitlink
    ; `Interface
    ]
    ~f:(fun variant ->
      Support.with_workspace (fun env anchor _ _ _ store ->
        let source_mode, ancestor_mode, interface =
          match variant with
          | `Source source -> source, Mode.Regular, Interface.Present
          | `Ancestor ancestor -> Mode.Regular, ancestor, Interface.Present
          | `Interface -> Mode.Regular, Mode.Regular, Interface.Symlink
        in
        let fixture =
          create
            ~source_mode
            ~ancestor_mode
            ~interface
            anchor
            ~format:H.Pin.Sha1
            ~git:(Support.run_git env)
        in
        let result =
          Support.acquire
            store
            (repository fixture)
            (old_pin fixture)
            ~limits:(Support.limits ())
            ~f:(fun repo -> reader repo ~f:capture_one)
        in
        match result with
        | Error (`Callback e) ->
          (match G.Error.problem e with
           | Unsupported_member_mode _ -> ()
           | _ -> failwith "selected link wrong failure")
        | Ok _ | Error (`Host _) ->
          failwith "selected link did not reach semantic rejection"));
  Support.with_workspace (fun env anchor _ _ _ store ->
    let fixture =
      create
        ~interface:Interface.Absent
        anchor
        ~format:H.Pin.Sha256
        ~git:(Support.run_git env)
    in
    let (catalog, accounting), _ =
      Support.acquire
        store
        (repository fixture)
        (old_pin fixture)
        ~limits:(Support.limits ())
        ~f:(fun repo -> reader repo ~f:capture_one)
      |> native_ok
    in
    assert (G.Accounting.source_count accounting = 2);
    assert (List.length (Chatml_catalog.sources catalog) = 2);
    let definition = List.hd_exn (Chatml_catalog.definitions catalog) in
    assert (Option.is_none (Chatml_prepared_program.Definition.interface definition)))
;;

let%test_unit
    "unavailable full pin fails before Reader without HEAD fallback or cache publication"
  =
  Support.with_workspace (fun env anchor cache _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env) in
    let missing =
      H.Pin.create ~object_format:Sha1 ~commit:(String.make 40 '0') |> Or_error.ok_exn
    in
    let entered = ref false in
    let result =
      Support.acquire
        store
        (repository fixture)
        missing
        ~limits:(Support.limits ())
        ~f:(fun _ ->
          entered := true;
          Ok ())
    in
    (match result with
     | Error (`Host e) -> assert (H.Error.equal_code (H.Error.code e) Process_failed)
     | Ok _ | Error (`Callback _) ->
       failwith "missing full pin did not reject acquisition");
    assert (not !entered);
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages"))))
;;

let%expect_test "pure catalog work observes the repository deadline" =
  Support.with_workspace (fun env anchor _ _ deployment _ ->
    let fixture = create anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env) in
    let path = Eio.Path.(anchor / "deadline-store") in
    Eio.Path.mkdir ~perm:0o700 path;
    Eio.Path.with_open_dir path (fun root ->
      Eio.Switch.run (fun sw ->
        let clock = Eio_mock.Clock.Mono.make () in
        let store =
          H.Store.open_root
            root
            ~sw
            ~deployment
            ~clock
            ~limits:Support.store_limits
            ~checkpoint:ignore
          |> Support.ok
        in
        let armed = ref false in
        let observed = ref false in
        let checkpoint () =
          if !armed
          then (
            armed := false;
            Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 20_000_000_000L))
        in
        let result =
          Support.acquire
            store
            (repository fixture)
            (old_pin fixture)
            ~limits:(Support.limits ())
            ~f:(fun repo ->
              reader ~checkpoint repo ~f:(fun r ->
                let open Result.Let_syntax in
                let%bind d = declaration r "pkg" in
                armed := true;
                G.Reader.capture r d ~request))
        in
        (match result with
         | Error (`Host failure) ->
           observed := H.Error.equal_code (H.Error.code failure) Timed_out
         | Error (`Callback error) ->
           (match G.Error.problem error with
            | Host failure ->
              observed := H.Error.equal_code (H.Error.code failure) Timed_out
            | _ -> failwith "deadline lost native ownership")
         | Ok _ -> failwith "expired catalog admitted");
        assert !observed;
        assert (Result.is_error result);
        let published =
          not (List.is_empty (Eio.Path.read_dir Eio.Path.(path / "cache")))
        in
        assert (not published);
        print_s [%sexp { timed_out = (!observed : bool); published : bool }])));
  [%expect
    {|
    +mock time is now 20
    ((timed_out true) (published false))
    |}]
;;

let%test_unit
    "effective namespace overrides preserve authored validation and once-only identities"
  =
  Support.with_workspace (fun env anchor _ _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env) in
    let (_, accounting), _ =
      Support.acquire
        store
        (repository fixture)
        (old_pin fixture)
        ~limits:(Support.limits ())
        ~f:(fun repo ->
          reader repo ~f:(fun r ->
            let open Result.Let_syntax in
            let%bind default = capture_one r in
            let namespace = "explicit/opaque\000label" in
            let%bind d =
              G.Reader.read_declaration
                r
                ~package_root:(Some (path "pkg"))
                ~project:(path "project.chatml.json")
                ~namespace_override:(Some namespace)
            in
            assert (String.equal (Chatml_project.namespace (G.Reader.project d)) "alpha");
            let%map overridden = G.Reader.capture r d ~request in
            List.iter (Chatml_catalog.sources overridden) ~f:(fun source ->
              assert (
                String.equal
                  (Chatml_source.Id.package_namespace (Chatml_source.id source))
                  namespace));
            List.iter2_exn
              (Chatml_catalog.sources default)
              (Chatml_catalog.sources overridden)
              ~f:(fun first second ->
                assert (
                  String.equal
                    (Chatml_source.contents first)
                    (Chatml_source.contents second));
                assert (
                  not
                    (Chatml_source.Id.equal
                       (Chatml_source.id first)
                       (Chatml_source.id second))))))
      |> native_ok
    in
    assert (G.Accounting.object_count accounting = 6);
    assert (G.Accounting.source_count accounting = 6));
  Support.with_workspace (fun env anchor cache _ _ store ->
    let fixture =
      create ~namespace:"" anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env)
    in
    let result =
      Support.acquire
        store
        (repository fixture)
        (old_pin fixture)
        ~limits:(Support.limits ())
        ~f:(fun repo ->
          reader repo ~f:(fun r ->
            G.Reader.read_declaration
              r
              ~package_root:(Some (path "pkg"))
              ~project:(path "project.chatml.json")
              ~namespace_override:(Some "cannot-repair-authored-invalid")))
    in
    (match result with
     | Error (`Callback e) ->
       assert (Option.is_none (G.Error.declared_at e));
       (match G.Error.problem e with
        | Bootstrap _ -> ()
        | _ -> failwith "override bypassed authored validity")
     | Ok _ | Error (`Host _) ->
       failwith "invalid authored namespace did not reach bootstrap rejection");
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache"))))
;;
