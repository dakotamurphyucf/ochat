let%test_module "host graph acquisition" =
  (module struct
    open Core
    module Graph = Project_prepare_graph
    module Attempt = Project_prepare_attempt
    module Deadline = Project_prepare_deadline
    module A = Project_prepare_accounting
    module Error = Project_prepare_error
    module Policy = Project_prepare_policy
    module Limits = Project_prepare_limits
    module Host = Project_prepare_host
    module Request = Project_prepare_request
    module B = Chatml_project_build
    module P = Chatml_project
    module S = Chatml_source
    module R = Chatml_prepared_program
    module Roots = Chatml_local_roots
    module Git = Chatml_git_host

    let admit = function
      | Ok value -> value
      | Error _ -> failwith "fixture admission rejected"
    ;;

    let path value = Source.Portable_path.create value |> Or_error.ok_exn

    let id namespace member =
      S.Id.create ~package_namespace:namespace ~relative_path:member |> Or_error.ok_exn
    ;;

    let contract () =
      Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
    ;;

    let tools () =
      Chat_response.Tool_capability.create
        ~owner:"graph-test"
        ~resource_fingerprint:(String.make 64 '0')
        []
      |> admit
    ;;

    let policy () =
      Policy.create
        ~roots:
          (Roots.Limits.create
             ~max_locator_bytes:1024
             ~max_locator_components:32
             ~max_anchors:16
             ~max_retained_roots:128
             ~max_work_steps:1_000_000
           |> Or_error.ok_exn)
        ~local:
          (Chatml_local_capture.Limits.create
             ~max_directory_entries:128
             ~max_sources:128
             ~max_source_bytes:8192
             ~max_total_source_bytes:262_144
             ~max_path_components:32
             ~max_work_steps:2_000_000
           |> Or_error.ok_exn)
        ~git_objects:
          (Chatml_git_catalog.Limits.create
             ~max_object_bytes:65_536
             ~max_total_object_bytes:1_048_576
             ~max_objects:256
             ~max_tree_entries:1024
             ~max_sources:128
             ~max_source_bytes:8192
             ~max_total_source_bytes:262_144
             ~max_path_components:32
             ~max_work_steps:2_000_000
             ~document:Document_schema.Limits.default
           |> Or_error.ok_exn)
        ~git_native:(Support.limits ())
        ~composition:
          (B.Limits.create
             ~max_work_steps:2_000_000
             ~max_input_bytes:(Some 1_048_576)
             ~document:Document_schema.Limits.default
           |> Or_error.ok_exn)
    ;;

    let limits ?(packages = 128) ?(visits = 1024) () =
      Limits.create
        ~max_packages:packages
        ~max_dependency_visits:visits
        ~max_source_count:1024
        ~max_source_bytes:4_194_304
        ~max_local_directory_entries:4096
        ~max_git_objects:1024
        ~max_git_object_bytes:4_194_304
        ~max_git_tree_entries:4096
        ~max_native_request_bytes:1_048_576
        ~max_native_output_bytes:1_048_576
        ~max_acquisition_payload_reserved:4_194_304
        ~max_work_steps:100_000_000
        ~max_prepared_input_bytes:1_048_576
        ~wall_time:(Time_ns.Span.of_sec 60.)
      |> Or_error.ok_exn
    ;;

    let request ?(operation = B.Operation.Initial_prepare) root =
      Request.create
        root
        ~operation
        ~selection:(P.Selection.Entry { name = "main"; environment = None })
        ~entry:Declared
        ~contract:(contract ())
        ~lock_id:(id "selected" "selection.lock.json")
        ~max_request_bytes:4096
      |> Or_error.ok_exn
    ;;

    let run env ?(checkpoint = ignore) ?(limits = limits ()) host request =
      let clock = Eio.Stdenv.mono_clock env in
      Attempt.run
        ~limits
        ~policy:(policy ())
        ~deadline:
          (Deadline.create ~clock ~wall_time:(Time_ns.Span.of_sec 60.) |> Or_error.ok_exn)
        ~clock
        ~checkpoint
        ~f:(fun attempt -> Graph.prepare attempt ~host ~request ~tools:(tools ()))
    ;;

    let ok = function
      | Ok value -> value
      | Error error ->
        (match Error.problem error with
         | Roots failure ->
           raise_s [%sexp "roots", (Roots.Error.code failure : Roots.Error.code)]
         | Local failure ->
           raise_s
             [%sexp
               (Chatml_local_capture.Error.problem failure
                : Chatml_local_capture.Error.problem)]
         | Git_host failure -> raise_s [%sexp (failure : Git.Error.t)]
         | Git_catalog _ -> failwith "Git catalog rejected"
         | Declaration failure ->
           raise_s [%sexp (P.Error.details failure : Document_schema.Error.t)]
         | Composition _ -> failwith "composition rejected"
         | Limit_exceeded -> failwith "aggregate limit exceeded"
         | _ -> failwith "unexpected graph rejection")
    ;;

    let with_directory f =
      Eio_main.run (fun env ->
        let random = Cstruct.create 16 in
        Eio.Flow.read_exact (Eio.Stdenv.secure_random env) random;
        let suffix =
          Cstruct.to_string random
          |> String.to_list
          |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
          |> String.concat
        in
        let directory =
          Eio.Path.(Eio.Stdenv.fs env / "/tmp" / ("ochat-graph-" ^ suffix))
        in
        Eio.Path.mkdir ~perm:0o700 directory;
        Fun.protect
          ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory))
          (fun () -> Eio.Path.with_open_dir directory (fun root -> f env directory root)))
    ;;

    let save directory name contents =
      Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(directory / name) contents
    ;;

    let object_ fields = `Object fields
    let string value = `String value

    let manifest namespace dependencies =
      let selected =
        ("Own", string "self.Code")
        :: List.map dependencies ~f:(fun (name, _, _) ->
          String.capitalize name, string (name ^ ".Code"))
      in
      Jsonaf.to_string
        (object_
           [ "schema_version", `Number "1"
           ; "namespace", string namespace
           ; ( "modules"
             , object_ [ "Code", object_ [ "implementation", string "code.chatml" ] ] )
           ; ( "entries"
             , object_
                 [ ( "main"
                   , object_
                       [ "source", string "code.chatml"; "target", string "one_off_v1" ] )
                 ] )
           ; ( "environments"
             , object_ [ "default", object_ [ "modules", object_ selected ] ] )
           ; ( "dependencies"
             , object_
                 (List.map dependencies ~f:(fun (name, directory, project) ->
                    ( name
                    , object_
                        [ ( "source"
                          , object_ [ "kind", string "local"; "path", string directory ] )
                        ; "project", string project
                        ] ))) )
           ])
    ;;

    let host root ?(store = None) ?(grants = []) () =
      let anchor =
        Roots.Anchor.create
          root
          ~host_prefix:"/workspace"
          ~max_path_components:32
          ~limits:(policy ()).roots
          ~checkpoint:ignore
        |> admit
      in
      Host.create
        [ anchor ]
        ~git_store:store
        ~git_grants:grants
        ~max_grants:16
        ~max_grant_bytes:65_536
      |> Or_error.ok_exn
    ;;

    let create_package directory name text =
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(directory / name);
      save directory (name ^ "/chatml.project.json") text;
      save directory (name ^ "/code.chatml") "let main = 0\n"
    ;;

    let reached edges =
      let rec walk seen = function
        | [] -> List.sort seen ~compare:Int.compare
        | node :: rest ->
          if List.mem seen node ~equal:Int.equal
          then walk seen rest
          else
            walk
              (node :: seen)
              (List.filter_map edges ~f:(fun (a, b) -> if a = node then Some b else None)
               @ rest)
      in
      walk [] [ 0 ]
    ;;

    let materialize directory edges =
      for n = 0 to 3 do
        let dependencies =
          List.filter_map edges ~f:(fun (a, b) ->
            if a = n
            then
              Some ("d" ^ Int.to_string b, "../p" ^ Int.to_string b, "chatml.project.json")
            else None)
        in
        create_package
          directory
          ("p" ^ Int.to_string n)
          (manifest ("p" ^ Int.to_string n) dependencies)
      done
    ;;

    let%expect_test "diamonds and declaration cycles capture one inventory per package" =
      List.iter
        [ [ 0, 1; 0, 2; 1, 3; 2, 3 ]; [ 0, 1; 1, 0 ] ]
        ~f:(fun edges ->
          with_directory (fun env directory root ->
            materialize directory edges;
            let build, receipt =
              run
                env
                (host root ())
                (request
                   (Local
                      { locator = "/workspace/p0"; project = path "chatml.project.json" }))
              |> ok
            in
            let expected =
              reached edges |> List.map ~f:(fun n -> "p" ^ Int.to_string n)
            in
            let actual =
              P.Lock.catalogs (B.lock build)
              |> List.map ~f:P.Lock.Catalog.namespace
              |> List.sort ~compare:String.compare
            in
            [%test_eq: string list] actual expected;
            [%test_eq: int] (A.packages receipt) (List.length expected);
            [%test_eq: int] (A.source_count receipt) (2 * List.length expected);
            [%test_eq: int] (A.dependency_visits receipt) (List.length edges);
            print_s
              [%sexp
                (actual : string list)
              , (A.packages receipt : int)
              , (A.dependency_visits receipt : int)]));
      [%expect
        {|
        ((p0 p1 p2 p3) 4 4)
        ((p0 p1) 2 2)
        |}]
    ;;

    let%test_unit
        "finite graph reachability and package/visit admission match an independent model"
      =
      List.iter
        [ []; [ 0, 1 ]; [ 0, 1; 1, 2; 2, 0 ]; [ 0, 1; 0, 2; 1, 2; 2, 3 ] ]
        ~f:(fun edges ->
          with_directory (fun env directory root ->
            materialize directory edges;
            let host = host root () in
            let request =
              request
                (Local { locator = "/workspace/p0"; project = path "chatml.project.json" })
            in
            let count = List.length (reached edges) in
            List.iter
              [ Int.max 0 (count - 1); count; count + 1 ]
              ~f:(fun cap ->
                let result = run env ~limits:(limits ~packages:cap ()) host request in
                assert (Bool.equal (Result.is_ok result) (cap >= count)));
            let visits = List.length edges in
            List.iter
              [ Int.max 0 (visits - 1); visits; visits + 1 ]
              ~f:(fun cap ->
                let result = run env ~limits:(limits ~visits:cap ()) host request in
                assert (Bool.equal (Result.is_ok result) (cap >= visits)))))
    ;;

    let%test_unit "nested declaration local locators start at its actual directory" =
      with_directory (fun env directory root ->
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(directory / "p0");
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(directory / "p0" / "config");
        save
          directory
          "p0/config/chatml.project.json"
          (manifest "root" [ "dep", "../../p1", "chatml.project.json" ]);
        save directory "p0/code.chatml" "let main = 0\n";
        create_package directory "p1" (manifest "dep" []);
        let build, _ =
          run
            env
            (host root ())
            (request
               (Local
                  { locator = "/workspace/p0"
                  ; project = path "config/chatml.project.json"
                  }))
          |> ok
        in
        [%test_eq: int] (List.length (P.Lock.catalogs (B.lock build))) 2)
    ;;

    let%test_unit
        "each lexical alias of one native package resolves its own relative edges"
      =
      with_directory (fun env directory root ->
        create_package
          directory
          "root"
          (manifest
             "root"
             [ "a", "/one/owner", "chatml.project.json"
             ; "b", "/two/owner", "chatml.project.json"
             ]);
        create_package
          directory
          "shared"
          (manifest "shared" [ "child", "../dep", "chatml.project.json" ]);
        create_package directory "dep1" (manifest "dep1" []);
        create_package directory "dep2" (manifest "dep2" []);
        Eio.Switch.run (fun sw ->
          let anchors =
            List.map
              [ "/workspace", "."
              ; "/one/owner", "shared"
              ; "/two/owner", "shared"
              ; "/one/dep", "dep1"
              ; "/two/dep", "dep2"
              ]
              ~f:(fun (prefix, relative) ->
                let directory = Eio.Path.open_dir ~sw Eio.Path.(root / relative) in
                Roots.Anchor.create
                  directory
                  ~host_prefix:prefix
                  ~max_path_components:32
                  ~limits:(policy ()).roots
                  ~checkpoint:ignore
                |> admit)
          in
          let host =
            Host.create
              anchors
              ~git_store:None
              ~git_grants:[]
              ~max_grants:0
              ~max_grant_bytes:0
            |> Or_error.ok_exn
          in
          List.iter [ false; true ] ~f:(fun reverse ->
            let a, b =
              if reverse then "/two/owner", "/one/owner" else "/one/owner", "/two/owner"
            in
            save
              directory
              "root/chatml.project.json"
              (manifest
                 "root"
                 [ "a", a, "chatml.project.json"; "b", b, "chatml.project.json" ]);
            match
              run
                env
                host
                (request
                   (Local
                      { locator = "/workspace/root"
                      ; project = path "chatml.project.json"
                      }))
            with
            | Ok _ -> failwith "divergent lexical alias admitted"
            | Error error ->
              (match Error.problem error with
               | Conflicting_selection -> ()
               | _ -> failwith "wrong alias failure");
              let owner =
                Error.declared_at error
                |> Option.value_exn
                |> P.Declared_at.source
                |> S.id
              in
              [%test_eq: string] (S.Id.package_namespace owner) "shared")))
    ;;

    let%test_unit
        "capture sharing admits equal effective selectors and rejects divergent ones"
      =
      with_directory (fun env directory root ->
        create_package directory "root" (manifest "root" []);
        let dependency namespace environment =
          object_
            ([ "source", object_ [ "kind", string "local"; "path", string "../lib" ]
             ; "project", string "chatml.project.json"
             ]
             @ Option.to_list
                 (Option.map namespace ~f:(fun value -> "namespace", string value))
             @ Option.to_list
                 (Option.map environment ~f:(fun value -> "environment", string value)))
        in
        let declaration a b =
          match Jsonaf.of_string (manifest "root" []) with
          | `Object fields ->
            Jsonaf.to_string
              (`Object
                  (("dependencies", object_ [ "a", a; "b", b ])
                   :: ( "environments"
                      , object_
                          [ ( "default"
                            , object_
                                [ ( "modules"
                                  , object_ [ "A", string "a.Code"; "B", string "b.Code" ]
                                  )
                                ] )
                          ] )
                   :: List.filter fields ~f:(fun (name, _) ->
                     not
                       (List.mem
                          [ "dependencies"; "environments" ]
                          name
                          ~equal:String.equal))))
          | _ -> assert false
        in
        let lib =
          String.substr_replace_all
            (manifest "lib" [])
            ~pattern:"\"environments\":{\"default\":"
            ~with_:"\"environments\":{\"empty\":{\"modules\":{}},\"default\":"
        in
        create_package directory "lib" lib;
        List.iter
          [ Some "shared", None, true
          ; Some "other", None, false
          ; Some "shared", Some "empty", false
          ]
          ~f:(fun (namespace, environment, expected) ->
            save
              directory
              "root/chatml.project.json"
              (declaration
                 (dependency (Some "shared") None)
                 (dependency namespace environment));
            let result =
              run
                env
                (host root ())
                (request
                   (Local
                      { locator = "/workspace/root"
                      ; project = path "chatml.project.json"
                      }))
            in
            assert (Bool.equal (Result.is_ok result) expected);
            match result with
            | Ok (build, receipt) ->
              [%test_eq: int] (A.packages receipt) 2;
              [%test_eq: int] (A.source_count receipt) 4;
              let root_catalog =
                P.Lock.catalogs (B.lock build)
                |> List.find_exn ~f:(fun catalog ->
                  String.equal (P.Lock.Catalog.namespace catalog) "root")
              in
              let edges = P.Lock.Catalog.dependencies root_catalog in
              assert (
                S.Id.equal
                  (P.Lock.Dependency_selection.declaration (List.hd_exn edges))
                  (P.Lock.Dependency_selection.declaration (List.last_exn edges)))
            | Error error ->
              (match Error.problem error with
               | Conflicting_selection -> ()
               | _ -> failwith "wrong selector rejection")))
    ;;

    let%test_unit "graph checkpoint exceptions preserve identity and joined reuse" =
      with_directory (fun env directory root ->
        materialize directory [ 0, 1; 1, 0 ];
        let host = host root () in
        let request =
          request
            (Local { locator = "/workspace/p0"; project = path "chatml.project.json" })
        in
        let count = ref 0 in
        ignore (run env ~checkpoint:(fun () -> incr count) host request |> ok);
        let exception Stop of unit ref in
        List.iter
          [ 1; !count / 2; !count - 1 ]
          ~f:(fun stop ->
            let witness = ref () in
            let polls = ref 0 in
            (match
               run
                 env
                 ~checkpoint:(fun () ->
                   incr polls;
                   if !polls = stop then raise (Stop witness))
                 host
                 request
             with
             | _ -> failwith "checkpoint interruption swallowed"
             | exception Stop actual -> assert (phys_equal witness actual));
            [%test_eq: int] !polls stop);
        ignore (run env host request |> ok))
    ;;

    let rich_manifest namespace implementation dependencies =
      Jsonaf.to_string
        (object_
           [ "schema_version", `Number "1"
           ; "namespace", string namespace
           ; ( "modules"
             , object_ [ "Code", object_ [ "implementation", string implementation ] ] )
           ; ( "entries"
             , object_
                 [ ( "main"
                   , object_
                       [ "source", string implementation; "target", string "one_off_v1" ]
                   )
                 ] )
           ; ( "environments"
             , object_
                 [ ( "default"
                   , object_
                       [ ( "modules"
                         , object_
                             (("Own", string "self.Code")
                              :: List.map dependencies ~f:(fun (name, _) ->
                                String.capitalize name, string (name ^ ".Code"))) )
                       ] )
                 ] )
           ; "dependencies", object_ dependencies
           ])
    ;;

    let local_dependency ?namespace directory =
      object_
        ([ "source", object_ [ "kind", string "local"; "path", string directory ]
         ; "project", string "chatml.project.json"
         ]
         @ Option.to_list (Option.map namespace ~f:(fun name -> "namespace", string name))
        )
    ;;

    let git_dependency ?namespace repository pin project =
      let format =
        match P.Git_pin.object_format pin with
        | Sha1 -> "sha1"
        | Sha256 -> "sha256"
      in
      object_
        ([ ( "source"
           , object_
               [ "kind", string "git"
               ; "repository", string repository
               ; "object_format", string format
               ; "commit", string (P.Git_pin.commit pin)
               ] )
         ; "project", string project
         ]
         @ Option.to_list (Option.map namespace ~f:(fun name -> "namespace", string name))
        )
    ;;

    let git_repository env anchor name format packages =
      let source = Eio.Path.(anchor / name) in
      Eio.Path.mkdir ~perm:0o700 source;
      let git args body =
        Support.run_git env source ~arguments:args ~stdin:body |> String.strip
      in
      let object_format =
        match format with
        | P.Git_pin.Sha1 -> "sha1"
        | Sha256 -> "sha256"
      in
      ignore (git [ "init"; "--bare"; "--quiet"; "--object-format=" ^ object_format ] "");
      let blob body = git [ "hash-object"; "-w"; "--stdin" ] body in
      let tree entries =
        git
          [ "mktree" ]
          (String.concat
             ~sep:""
             (List.map entries ~f:(fun (mode, kind, oid, name) ->
                mode ^ " " ^ kind ^ " " ^ oid ^ "\t" ^ name ^ "\n")))
      in
      let entries =
        List.map packages ~f:(fun (name, manifest) ->
          let tree =
            tree
              [ "100644", "blob", blob manifest, "chatml.project.json"
              ; "100644", "blob", blob "let main = 0\n", "code.chatml"
              ]
          in
          "040000", "tree", tree, name)
      in
      let commit = git [ "commit-tree"; tree entries ] "fixture\n" in
      ignore (git [ "update-ref"; "refs/heads/main"; commit ] "");
      ignore (git [ "symbolic-ref"; "HEAD"; "refs/heads/main" ] "");
      source, P.Git_pin.create ~object_format:format ~commit |> Or_error.ok_exn
    ;;

    let%expect_test
        "Git to Local to another package at the same pin reopens with aggregate receipts"
      =
      Support.with_workspace (fun env anchor cache _ _ store ->
        List.iter [ P.Git_pin.Sha1; Sha256 ] ~f:(fun format ->
          let suffix =
            match format with
            | Sha1 -> "one"
            | Sha256 -> "two"
          in
          let local_name = "local-" ^ suffix in
          let local_locator = "/workspace/" ^ local_name in
          let source, pin =
            git_repository
              env
              anchor
              ("origin-" ^ suffix)
              format
              [ ( "pkg"
                , rich_manifest
                    "alpha"
                    "code.chatml"
                    [ "local", local_dependency local_locator ] )
              ; "other", rich_manifest "beta" "other/code.chatml" []
              ]
          in
          let repository = Support.native source in
          create_package
            anchor
            local_name
            (rich_manifest
               "local"
               "code.chatml"
               [ "back", git_dependency repository pin "other/chatml.project.json" ]);
          Eio.Path.with_open_dir anchor (fun root ->
            let grant =
              Git.Grant.local ~repository ~max_input_bytes:4096 |> Or_error.ok_exn
            in
            let host = host root ~store:(Some store) ~grants:[ grant ] () in
            let request =
              request
                (Git
                   { repository
                   ; pin
                   ; package_root = Some (path "pkg")
                   ; project = path "chatml.project.json"
                   })
            in
            let saw_workspace = ref false in
            let checkpoint () =
              if not (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages")))
              then saw_workspace := true
            in
            let rejected =
              run env ~limits:(limits ~packages:0 ()) ~checkpoint host request
            in
            (match rejected with
             | Error error ->
               (match Error.problem error with
                | Limit_exceeded -> ()
                | _ -> failwith "wrong zero-package rejection")
             | Ok _ -> failwith "zero packages admitted");
            assert (not !saw_workspace);
            let build, receipt = run env host request |> ok in
            let names =
              P.Lock.catalogs (B.lock build)
              |> List.map ~f:P.Lock.Catalog.namespace
              |> List.sort ~compare:String.compare
            in
            [%test_eq: string list] names [ "alpha"; "beta"; "local" ];
            [%test_eq: int] (A.packages receipt) 3;
            [%test_eq: int] (A.source_count receipt) 6;
            [%test_eq: int] (A.dependency_visits receipt) 2;
            assert (A.native_request_bytes receipt > 0);
            assert (A.native_output_bytes receipt > 0);
            (* Each scoped Reader verifies commit and root tree again on reopening,
               plus its selected package tree and two blobs: 5 OIDs per bracket. *)
            [%test_eq: int] (A.git_objects receipt) 10;
            let captured =
              R.sources (B.prepared build)
              |> List.find_exn ~f:(fun source ->
                String.equal (S.Id.package_namespace (S.id source)) "beta"
                && String.equal (S.Id.relative_path (S.id source)) "other/code.chatml")
            in
            [%test_eq: string] (S.contents captured) "let main = 0\n";
            let sources =
              R.sources (B.prepared build)
              |> List.filter ~f:(fun source ->
                not (S.Id.equal (S.id source) (S.id (B.lock_source build))))
            in
            let restored =
              B.restore
                (B.lock_source build)
                ~sources
                ~contract:(contract ())
                ~limits:(policy ()).composition
                ~checkpoint:ignore
              |> admit
            in
            assert (
              S.Digest.equal
                (R.fingerprint (B.prepared build))
                (R.fingerprint (B.prepared restored)));
            print_s
              [%sexp
                (format : P.Git_pin.object_format)
              , (names : string list)
              , (A.source_count receipt : int)
              , (A.git_objects receipt : int)])));
      [%expect
        {|
        (Sha1 (alpha beta local) 6 10)
        (Sha256 (alpha beta local) 6 10)
        |}]
    ;;

    let%test_unit
        "a converged native package preserves every incoming prior-owner route before Git"
      =
      Support.with_workspace (fun env anchor _ _ _ store ->
        let source1, pin1 =
          git_repository
            env
            anchor
            "old-one"
            P.Git_pin.Sha1
            [ "pkg", rich_manifest "git-one" "pkg/code.chatml" [] ]
        in
        let source2, pin2 =
          git_repository
            env
            anchor
            "old-two"
            P.Git_pin.Sha1
            [ "pkg", rich_manifest "git-two" "pkg/code.chatml" [] ]
        in
        let repo1 = Support.native source1
        and repo2 = Support.native source2 in
        create_package
          anchor
          "root"
          (rich_manifest
             "root"
             "code.chatml"
             [ "a", local_dependency "../left"; "b", local_dependency "../right" ]);
        create_package
          anchor
          "left"
          (rich_manifest
             "left"
             "code.chatml"
             [ "back", git_dependency repo1 pin1 "pkg/chatml.project.json" ]);
        create_package
          anchor
          "right"
          (rich_manifest
             "right"
             "code.chatml"
             [ "back", git_dependency repo2 pin2 "pkg/chatml.project.json" ]);
        Eio.Path.with_open_dir anchor (fun root ->
          let grants =
            List.map [ repo1; repo2 ] ~f:(fun repository ->
              Git.Grant.local ~repository ~max_input_bytes:4096 |> Or_error.ok_exn)
          in
          let initial, _ =
            run
              env
              (host root ~store:(Some store) ~grants ())
              (request
                 (Local
                    { locator = "/workspace/root"; project = path "chatml.project.json" }))
            |> ok
          in
          List.iter [ false; true ] ~f:(fun reverse ->
            let left = local_dependency ~namespace:"merged" "../left"
            and right = local_dependency ~namespace:"merged" "../left" in
            let deps =
              if reverse then [ "b", right; "a", left ] else [ "a", left; "b", right ]
            in
            save
              anchor
              "root/chatml.project.json"
              (rich_manifest "root" "code.chatml" deps);
            let request =
              request
                ~operation:(B.Operation.Build { previous_lock = B.lock_source initial })
                (Local
                   { locator = "/workspace/root"; project = path "chatml.project.json" })
            in
            match run env (host root ()) request with
            | Ok _ -> failwith "changed converging Git route admitted"
            | Error error ->
              (match Error.problem error with
               | Composition failure ->
                 (match B.Error.problem failure with
                  | Needs_update -> ()
                  | _ -> failwith "wrong gate rejection")
               | _ -> failwith "Git backend entered before second route check");
              let at = Error.declared_at error |> Option.value_exn in
              [%test_eq: string]
                (S.Id.package_namespace (S.id (P.Declared_at.source at)))
                "merged")))
    ;;
  end)
;;
