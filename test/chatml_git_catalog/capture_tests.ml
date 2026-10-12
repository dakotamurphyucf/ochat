open Core
module G = Chatml_git_catalog
module C = Chatml_catalog
module P = Chatml_project
module S = Chatml_source
module H = Chatml_git_host
open Fixture

let assert_catalog catalog namespace body =
  let id path =
    S.Id.create ~package_namespace:namespace ~relative_path:path |> Or_error.ok_exn
  in
  let code = C.find_source catalog (id "code.chatml") |> Option.value_exn in
  assert (String.equal (S.contents code) body);
  [%test_eq: string list]
    (C.sources catalog
     |> List.map ~f:(fun source -> S.Id.relative_path (S.id source))
     |> List.sort ~compare:String.compare)
    [ "code.chatml"; "code.chatmli"; "project.chatml.json" ];
  assert (List.length (C.definitions catalog) = 1);
  assert (List.length (C.bindings catalog) = 2);
  assert (
    Chatml_prepared_program.Module_id.equal
      (C.Binding.module_id (List.hd_exn (C.bindings catalog)))
      (C.Binding.module_id (List.last_exn (C.bindings catalog))));
  assert (S.Id.equal (Option.value_exn (C.entry catalog)) (S.id code));
  assert (
    String.equal
      (P.Scope_spec.name
         (C.scope_for catalog ~source:(id "code.chatml") |> Option.value_exn))
      "own");
  assert (
    String.equal
      (P.Scope_spec.name
         (C.scope_for catalog ~source:(id "code.chatmli") |> Option.value_exn))
      "public");
  S.digest code
;;

let both reader =
  let open Result.Let_syntax in
  let%bind a = declaration reader "pkg" in
  let%bind b = declaration reader "other" in
  let%bind a = G.Reader.capture reader a ~request in
  let%map b = G.Reader.capture reader b ~request in
  a, b
;;

let%expect_test "full pins own exact source bytes after HEAD moves and origin disappears" =
  Support.with_workspace (fun env anchor _ _ _ store ->
    let digests = ref [] in
    List.iter [ H.Pin.Sha1; Sha256 ] ~f:(fun format ->
      let fixture = create anchor ~format ~git:(Support.run_git env) in
      let ((a, b), accounting), first =
        Support.acquire
          store
          (repository fixture)
          (old_pin fixture)
          ~limits:(Support.limits ())
          ~f:(fun repo -> reader repo ~f:both)
        |> native_ok
      in
      digests := assert_catalog a "alpha" old_body :: !digests;
      ignore (assert_catalog b "beta" old_body);
      [%test_eq: int] (G.Accounting.object_count accounting) 8;
      [%test_eq: int] (G.Accounting.tree_entries accounting) 18;
      [%test_eq: int] (G.Accounting.source_count accounting) 6;
      let expected_bytes =
        String.length (project ~namespace:"alpha")
        + String.length (project ~namespace:"beta")
        + (2 * String.length old_body)
        + (2 * String.length "val meaning : int\n")
      in
      [%test_eq: int] (G.Accounting.source_bytes accounting) expected_bytes;
      move_to_new fixture ~git:(Support.run_git env);
      let ((still_old, _), _), cached =
        Support.acquire
          store
          (repository fixture)
          (old_pin fixture)
          ~limits:(Support.limits ())
          ~f:(fun repo -> reader repo ~f:both)
        |> native_ok
      in
      ignore (assert_catalog still_old "alpha" old_body);
      assert (H.Receipt.equal_disposition (H.Receipt.disposition cached) Cached);
      let ((changed, _), _), _ =
        Support.acquire
          store
          (repository fixture)
          (new_pin fixture)
          ~limits:(Support.limits ())
          ~f:(fun repo -> reader repo ~f:both)
        |> native_ok
      in
      ignore (assert_catalog changed "alpha" new_body);
      assert (
        match
          Eio.Path.kind ~follow:false Eio.Path.(repository fixture / "filter-ran")
        with
        | `Not_found -> true
        | _ -> false);
      let route = Support.native (repository fixture) in
      Eio.Path.rmtree (repository fixture);
      let ((restored, _), _), offline =
        H.with_cached_repository
          store
          ~repository:route
          ~pin:(old_pin fixture)
          ~limits:(Support.limits ~payload:0 ())
          ~checkpoint:ignore
          ~f:(fun repo -> reader repo ~f:both)
        |> native_ok
      in
      ignore (assert_catalog restored "alpha" old_body);
      print_s
        [%sexp
          (format : H.Pin.object_format)
        , (H.Receipt.disposition first : H.Receipt.disposition)
        , (H.Receipt.disposition offline : H.Receipt.disposition)
        , (G.Accounting.source_count accounting : int)]);
    assert (S.Digest.equal (List.hd_exn !digests) (List.last_exn !digests)));
  [%expect
    {|
    (Sha1 Fetched Cached 6)
    (Sha256 Fetched Cached 6)
    |}]
;;

let%test_unit "HTTPS produces the same exact catalog without filtering" =
  Support.with_workspace ~trust_fixture_certificate:true (fun env anchor _ _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha256 ~git:(Support.run_git env) in
    move_to_new fixture ~git:(Support.run_git env);
    let observed = ref 0 in
    Eio.Switch.run (fun sw ->
      let endpoint =
        Https_fixture.start (Eio.Stdenv.net env) ~sw ~handle:(fun incoming ->
          incr observed;
          assert (
            Option.equal
              String.equal
              (Https_fixture.Request.header incoming "authorization")
              (Some "Bearer catalog-only"));
          Support.smart_git_response env (repository fixture) incoming)
      in
      let grant =
        H.Grant.https
          ~repository:(endpoint ^ "/repo")
          ~authorization:(Some "Authorization: Bearer catalog-only")
          ~max_input_bytes:4096
        |> Or_error.ok_exn
      in
      let request_ =
        H.Request.prepare store ~grant ~pin:(new_pin fixture) ~limits:(Support.limits ())
        |> Support.ok
      in
      let ((a, _), _), _ =
        H.with_acquisition request_ ~checkpoint:ignore ~f:(fun repo ->
          reader repo ~f:both)
        |> native_ok
      in
      ignore (assert_catalog a "alpha" new_body);
      assert (!observed > 0)))
;;

let%test_unit "repeated declaration/capture consumes source and object facts once" =
  Support.with_workspace (fun env anchor _ _ _ store ->
    let fixture = create anchor ~format:H.Pin.Sha1 ~git:(Support.run_git env) in
    let (_, receipt), _ =
      Support.acquire
        store
        (repository fixture)
        (old_pin fixture)
        ~limits:(Support.limits ())
        ~f:(fun repo ->
          reader repo ~f:(fun r ->
            let open Result.Let_syntax in
            let%bind d = declaration r "pkg" in
            let%bind _ = G.Reader.capture r d ~request in
            let%bind before = G.Reader.accounting r in
            let%bind repeated = declaration r "pkg" in
            let%bind _ = G.Reader.capture r repeated ~request in
            let%map after = G.Reader.accounting r in
            assert (G.Accounting.object_count before = G.Accounting.object_count after);
            assert (G.Accounting.object_bytes before = G.Accounting.object_bytes after);
            assert (G.Accounting.tree_entries before = G.Accounting.tree_entries after);
            assert (G.Accounting.source_count before = G.Accounting.source_count after);
            assert (G.Accounting.source_bytes before = G.Accounting.source_bytes after);
            assert (G.Accounting.work_steps before < G.Accounting.work_steps after)))
      |> native_ok
    in
    assert (G.Accounting.source_count receipt = 3))
;;
