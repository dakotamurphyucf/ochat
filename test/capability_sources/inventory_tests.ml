open! Core
open Support

let%expect_test "installed discovery derives source identities and real requirements" =
  Eio_main.run (fun _ ->
    let resource = resource () in
    [%test_eq: string] (Sources.Id.to_string (Sources.Resource.id resource)) "lifecycle";
    let pure =
      Sources.Resource.member
        resource
        (path "examples/libraries/lifecycle/src/lifecycle.chatml")
      |> source_ok
    in
    [%test_eq: string]
      (Sources.Member.contents pure)
      [%blob "../../docs-src/examples/libraries/lifecycle/src/lifecycle.chatml"];
    Chatml_source.Digest.of_contents (Sources.Member.contents pure) ~checkpoint:ignore
    |> fun expected ->
    assert (Chatml_source.Digest.equal expected (Sources.Member.digest pure));
    let projects = Sources.Resource.projects resource in
    [%test_eq: int] (List.length projects) 5;
    let adapter =
      List.find_exn projects ~f:(fun p ->
        String.equal
          (Sources.Project_summary.namespace p)
          "ochat.capabilities.lifecycle-run")
    in
    let requirement =
      List.Assoc.find_exn
        (Sources.Project_summary.environments adapter)
        "moderator"
        ~equal:String.equal
    in
    [%test_eq: string list option]
      (Chatml_project.Compiler_requirement.allowed_targets requirement)
      (Some [ "moderator_v1"; "delegated_moderator_v1" ]);
    print_endline
      "lifecycle: exact installed bytes, five decoded projects, moderator-only adapter";
    let installed =
      Sources.installed ~limits:Sources.Limits.default ~checkpoint:ignore |> source_ok
    in
    (match Sources.list installed ~max_items:0 ~checkpoint:ignore with
     | Error error ->
       assert (Sources.Error.equal_code (Sources.Error.code error) Limit_exceeded)
     | Ok _ -> failwith "enumeration ceiling ignored");
    match Sources.find installed (Sources.Id.of_string "missing" |> source_ok) with
    | Error error ->
      assert (Sources.Error.equal_code (Sources.Error.code error) Unknown_resource)
    | Ok _ -> failwith "uninstalled resource substituted");
  [%expect
    {| lifecycle: exact installed bytes, five decoded projects, moderator-only adapter |}]
;;
