open! Core
open Support

let%expect_test "installed discovery derives source identities and real requirements" =
  Eio_main.run (fun _ ->
    let resource = resource () in
    (* The exact installed identity includes the member bytes, including guides. *)
    [%test_eq: string]
      (Chatml_source.Digest.to_hex (Sources.Resource.digest resource))
      "4123fcc2149b52545c9a8fb8c95c5c3edd51ba6d61d1c78f5caa66c266c85311";
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

let%expect_test "installed instruction helper closure reuses canonical pure libraries" =
  Eio_main.run (fun _ ->
    let selected = resource ~id:"instruction-selection" () in
    [%test_eq: int] 11 (List.length (Sources.Resource.members selected));
    [%test_eq: int] 3 (List.length (Sources.Resource.projects selected));
    let helper =
      Sources.Resource.member
        selected
        (path "examples/libraries/instruction-requests/src/instruction_requests.chatmli")
      |> source_ok
    in
    [%test_eq: string]
      (Sources.Member.contents helper)
      [%blob
        "../../docs-src/examples/libraries/instruction-requests/src/instruction_requests.chatmli"];
    let lifecycle_path = path "examples/libraries/lifecycle/src/lifecycle.chatml" in
    let original = Sources.Resource.member (resource ()) lifecycle_path |> source_ok in
    let shared = Sources.Resource.member selected lifecycle_path |> source_ok in
    assert (
      Chatml_source.Digest.equal
        (Sources.Member.digest original)
        (Sources.Member.digest shared));
    print_endline
      "instruction-selection: exact helper, three projects, shared canonical Lifecycle \
       bytes");
  [%expect
    {| instruction-selection: exact helper, three projects, shared canonical Lifecycle bytes |}]
;;

let%expect_test "installed request helpers are pure ordinary projects" =
  Eio_main.run (fun _ ->
    List.iter [ "goal-requests"; "recurrence-requests" ] ~f:(fun id ->
      let selected = resource ~id () in
      [%test_eq: int] (List.length (Sources.Resource.members selected)) 4;
      match Sources.Resource.projects selected with
      | [ project ] ->
        [%test_eq: string]
          (Sources.Project_summary.namespace project)
          ("ochat.capabilities." ^ id);
        [%test_eq: string list] (Sources.Project_summary.required_tools project) [];
        [%test_eq: string list]
          (List.map (Sources.Project_summary.environments project) ~f:fst)
          [ "pure" ];
        printf "%s: one pure project with no required tools\n" id
      | _ -> failwithf "%s helper inventory does not match its actual project" id ());
    let selected = resource ~id:"goal-requests" () in
    let entry = "examples/libraries/goal-requests/chatml.project.json" in
    [%test_eq: string]
      (Source.Portable_path.to_string (Sources.Resource.entry selected))
      entry;
    let canonical =
      [ ( entry
        , [%blob "../../docs-src/examples/libraries/goal-requests/chatml.project.json"] )
      ; ( "examples/libraries/goal-requests/src/goal_requests.chatml"
        , [%blob
            "../../docs-src/examples/libraries/goal-requests/src/goal_requests.chatml"] )
      ; ( "examples/libraries/goal-requests/src/goal_requests.chatmli"
        , [%blob
            "../../docs-src/examples/libraries/goal-requests/src/goal_requests.chatmli"] )
      ; ( "guide/native-goal-metadata.md"
        , [%blob "../../docs-src/guide/native-goal-metadata.md"] )
      ]
    in
    List.iter canonical ~f:(fun (name, expected) ->
      let member = Sources.Resource.member selected (path name) |> source_ok in
      [%test_eq: string] (Sources.Member.contents member) expected);
    print_endline
      "goal-requests: exact canonical project, implementation, interface and guide");
  [%expect
    {|
    goal-requests: one pure project with no required tools
    recurrence-requests: one pure project with no required tools
    goal-requests: exact canonical project, implementation, interface and guide
    |}]
;;
