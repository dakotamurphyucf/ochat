let%test_module "host preparation public boundary" =
  (module struct
    open Core
    module P = Chatml_project_prepare
    module B = Chatml_project_build
    module S = Chatml_source
    module R = Chatml_prepared_program
    module Contract = Chatml.Chatml_compiler_contract

    let ok = function
      | Ok value -> value
      | Error _ -> failwith "unexpected preparation fixture rejection"
    ;;

    let path text = Source.Portable_path.create text |> Or_error.ok_exn

    let id name =
      S.Id.create ~package_namespace:"example" ~relative_path:name |> Or_error.ok_exn
    ;;

    let root_limits () =
      Chatml_local_roots.Limits.create
        ~max_locator_bytes:4096
        ~max_locator_components:64
        ~max_anchors:8
        ~max_retained_roots:32
        ~max_work_steps:10_000
      |> Or_error.ok_exn
    ;;

    let composition_limits () =
      B.Limits.create
        ~max_work_steps:1_000_000
        ~max_input_bytes:(Some 1_000_000)
        ~document:Document_schema.Limits.default
      |> Or_error.ok_exn
    ;;

    let policy () =
      P.Policy.create
        ~roots:(root_limits ())
        ~local:
          (Chatml_local_capture.Limits.create
             ~max_directory_entries:100
             ~max_sources:100
             ~max_source_bytes:100_000
             ~max_total_source_bytes:1_000_000
             ~max_path_components:64
             ~max_work_steps:1_000_000
           |> Or_error.ok_exn)
        ~git_objects:
          (Chatml_git_catalog.Limits.create
             ~max_object_bytes:100_000
             ~max_total_object_bytes:1_000_000
             ~max_objects:100
             ~max_tree_entries:100
             ~max_sources:100
             ~max_source_bytes:100_000
             ~max_total_source_bytes:1_000_000
             ~max_path_components:64
             ~max_work_steps:1_000_000
             ~document:Document_schema.Limits.default
           |> Or_error.ok_exn)
        ~git_native:
          (Chatml_git_host.Limits.create
             ~metadata_file_bytes:100_000
             ~pack_file_bytes:1_000_000
             ~max_acquisition_payload_bytes:10_000_000
             ~max_request_bytes:100_000
             ~max_output_bytes:1_000_000
             ~max_work_steps:100_000
             ~wall_time:(Time_ns.Span.of_sec 60.)
             ~cpu_seconds_per_process:10
             ~memory_bytes_per_process:100_000_000
             ~open_files_per_process:64
           |> Or_error.ok_exn)
        ~composition:(composition_limits ())
    ;;

    let limits ?(prepared = 1_000_000) ?(sources = 100) ?(bytes = 1_000_000) () =
      P.Limits.create
        ~max_packages:100
        ~max_dependency_visits:100
        ~max_source_count:sources
        ~max_source_bytes:bytes
        ~max_local_directory_entries:100
        ~max_git_objects:100
        ~max_git_object_bytes:1_000_000
        ~max_git_tree_entries:100
        ~max_native_request_bytes:100_000
        ~max_native_output_bytes:1_000_000
        ~max_acquisition_payload_reserved:10_000_000
        ~max_work_steps:10_000_000
        ~max_prepared_input_bytes:prepared
        ~wall_time:(Time_ns.Span.of_sec 60.)
      |> Or_error.ok_exn
    ;;

    let declaration ?(requires_tool = false) () =
      sprintf
        {|{"schema_version":1,"namespace":"example","modules":{"Main":{"implementation":"main.chatml"}},"entries":{"main":{"source":"main.chatml","target":"one_off_v1"}},"capabilities":%s}|}
        (if requires_tool then {|["missing.tool"]|} else "[]")
    ;;

    let save directory name contents =
      Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(directory / name) contents
    ;;

    let with_host f =
      Eio_main.run (fun env ->
        let bytes = Cstruct.create 16 in
        Eio.Flow.read_exact env#secure_random bytes;
        let suffix =
          Cstruct.to_string bytes
          |> String.to_list
          |> List.map ~f:(fun byte -> sprintf "%02x" (Char.to_int byte))
          |> String.concat
        in
        let directory = Eio.Path.(env#fs / "/tmp" / ("och204-public-" ^ suffix)) in
        Eio.Path.mkdir ~perm:0o700 directory;
        Fun.protect
          ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory))
          (fun () ->
             Eio.Path.with_open_dir directory (fun directory ->
               let anchor =
                 Chatml_local_roots.Anchor.create
                   directory
                   ~host_prefix:"/project"
                   ~max_path_components:64
                   ~limits:(root_limits ())
                   ~checkpoint:ignore
                 |> ok
               in
               let host =
                 P.Host.create
                   [ anchor ]
                   ~git_store:None
                   ~git_grants:[]
                   ~max_grants:0
                   ~max_grant_bytes:0
                 |> Or_error.ok_exn
               in
               save directory "chatml.project.json" (declaration ());
               save directory "main.chatml" "let main = 1";
               f env directory host)))
    ;;

    let contract () = Contract.for_target One_off_v1 ~checkpoint:ignore

    let request operation =
      P.Request.create
        (Local { locator = "/project"; project = path "chatml.project.json" })
        ~operation
        ~selection:(Entry { name = "main"; environment = None })
        ~entry:Declared
        ~contract:(contract ())
        ~lock_id:(id "selected.json")
        ~max_request_bytes:4096
      |> Or_error.ok_exn
    ;;

    let tools () =
      Chat_response.Tool_capability.create
        ~owner:"preparation-fixture"
        ~resource_fingerprint:
          (S.Digest.of_contents "empty" ~checkpoint:ignore |> S.Digest.to_hex)
        []
      |> ok
    ;;

    let publish ?(checkpoint = ignore) host publication clock operation =
      P.prepare_and_publish
        host
        (request operation)
        ~publication
        ~tools:(tools ())
        ~policy:(policy ())
        ~limits:(limits ())
        ~clock
        ~checkpoint
    ;;

    let selected directory = Eio.Path.load Eio.Path.(directory / "selected.json")
    let lock result = B.lock_source (P.build result)

    let main result =
      R.sources (B.prepared (P.build result))
      |> List.find_exn ~f:(fun source -> S.Id.equal (S.id source) (id "main.chatml"))
      |> S.contents
    ;;

    let open_publication directory sw =
      P.Publication.open_owned
        directory
        ~sw
        ~basename:"selected.json"
        ~expected:Absent
        ~max_lock_bytes:1_000_000
      |> ok
    ;;

    let%expect_test "new launches capture edits; retained programs restore without files" =
      with_host (fun env directory host ->
        Eio.Switch.run (fun sw ->
          let publication = open_publication directory sw in
          let old = publish host publication env#mono_clock Initial_prepare |> ok in
          [%test_eq: string] (selected directory) (S.contents (lock old));
          save directory "main.chatml" "let main = 2";
          let fresh =
            publish host publication env#mono_clock (Build { previous_lock = lock old })
            |> ok
          in
          [%test_eq: string] (selected directory) (S.contents (lock fresh));
          [%test_eq: string] (main old) "let main = 1";
          [%test_eq: string] (main fresh) "let main = 2";
          [%test_eq: int] (P.Accounting.packages (P.accounting fresh)) 1;
          [%test_eq: int] (P.Accounting.source_count (P.accounting fresh)) 2;
          printf "retained main: 1; independently captured main: 2\n";
          Eio.Path.unlink Eio.Path.(directory / "main.chatml");
          Eio.Path.unlink Eio.Path.(directory / "chatml.project.json");
          let captured = B.prepared (P.build old) in
          let restored =
            B.restore
              (lock old)
              ~sources:
                (List.filter (R.sources captured) ~f:(fun source ->
                   not (S.Id.equal (S.id source) (S.id (lock old)))))
              ~contract:(contract ())
              ~limits:(composition_limits ())
              ~checkpoint:ignore
            |> ok
          in
          assert (
            S.Digest.equal (R.fingerprint captured) (R.fingerprint (B.prepared restored)));
          assert (P.Publication.equal_state (P.Publication.state publication) Available);
          printf
            "old program restored from exact captures after source files were removed\n";
          printf "selected lock: exact completed fresh capture; owner Available\n"));
      [%expect
        {| 
        retained main: 1; independently captured main: 2
        old program restored from exact captures after source files were removed
        selected lock: exact completed fresh capture; owner Available
        |}]
    ;;

    let%test_unit "prepublication admission failures preserve the selected lock" =
      with_host (fun env directory host ->
        Eio.Switch.run (fun sw ->
          let publication = open_publication directory sw in
          let old = publish host publication env#mono_clock Initial_prepare |> ok in
          let old_bytes = S.contents (lock old) in
          let operation = B.Operation.Build { previous_lock = lock old } in
          save directory "chatml.project.json" (declaration ~requires_tool:true ());
          (match publish host publication env#mono_clock operation with
           | Error error ->
             (match P.Error.problem error with
              | Tool_admission _ -> ()
              | _ -> failwith "tool failure was misclassified")
           | Ok _ -> failwith "missing native tool admitted");
          [%test_eq: string] (selected directory) old_bytes;
          assert (P.Publication.equal_state (P.Publication.state publication) Available);
          save directory "chatml.project.json" (declaration ());
          let exception Interrupted in
          let polls = ref 0 in
          (match
             publish host publication env#mono_clock operation ~checkpoint:(fun () ->
               incr polls;
               if !polls = 25 then raise Interrupted)
           with
           | exception Interrupted -> ()
           | _ -> failwith "checkpoint exception identity changed");
          [%test_eq: string] (selected directory) old_bytes;
          assert (P.Publication.equal_state (P.Publication.state publication) Available);
          save directory "selected.json" "changed by external owner";
          save directory "chatml.project.json" "invalid declaration must not be read";
          (match publish host publication env#mono_clock operation with
           | Error error ->
             (match P.Error.problem error with
              | Stale_selection -> ()
              | _ -> failwith "stale selected lock did not stop preparation")
           | Ok _ -> failwith "stale selected lock replaced");
          [%test_eq: string] (selected directory) "changed by external owner"))
    ;;

    let source_request ?(name = "source.lock.json") entry =
      P.Request.create_with_entry
        (Local { locator = "/project"; project = path "chatml.project.json" })
        ~operation:Initial_prepare
        ~selection:(Library { environment = None })
        ~entry
        ~contract:(contract ())
        ~lock_id:(id name)
        ~max_request_bytes:4096
      |> Or_error.ok_exn
    ;;

    let supplied contents =
      let id =
        S.Id.create ~package_namespace:"prompt" ~relative_path:"script.chatml"
        |> Or_error.ok_exn
      in
      S.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint:ignore
      |> Or_error.ok_exn
    ;;

    let capture ?(checkpoint = ignore) ?(limits = limits ()) host clock requests =
      P.prepare_sources host requests ~policy:(policy ()) ~limits ~clock ~checkpoint
    ;;

    let source_build batch =
      P.Source_batch.preparations batch |> List.hd_exn |> P.Source_preparation.build
    ;;

    let%expect_test "source capture retains requirements before live tool admission" =
      with_host (fun env directory host ->
        save directory "chatml.project.json" (declaration ~requires_tool:true ());
        let batch = capture host env#mono_clock [ request Initial_prepare ] |> ok in
        let preparation = List.hd_exn (P.Source_batch.preparations batch) in
        [%test_eq: string list]
          (P.Source_preparation.required_tools preparation)
          [ "missing.tool" ];
        assert (
          S.Id.equal
            (R.entry (B.prepared (P.Source_preparation.build preparation)))
            (id "main.chatml"));
        printf "source-selected graph retained; required tools: missing.tool\n";
        match
          P.prepare
            host
            (request Initial_prepare)
            ~tools:(tools ())
            ~policy:(policy ())
            ~limits:(limits ())
            ~clock:env#mono_clock
            ~checkpoint:ignore
        with
        | Ok _ -> failwith "strict preparation admitted missing tool"
        | Error error ->
          (match P.Error.problem error with
           | Tool_admission _ ->
             printf "strict preparation rejects unavailable live tool\n"
           | _ -> failwith "wrong strict admission error"));
      [%expect
        {|
        source-selected graph retained; required tools: missing.tool
        strict preparation rejects unavailable live tool
        |}]
    ;;

    let%expect_test "an admitted file entry shares catalog identity and initializes once" =
      with_host (fun env directory host ->
        let contents =
          "let initialized = to_string(\"entry\") let main args = \
           Task.pure(`String(\"ok\"))"
        in
        save directory "main.chatml" contents;
        let source = supplied contents in
        let batch =
          capture
            host
            env#mono_clock
            [ source_request
                (Captured_file { source; root_member = Some (path "main.chatml") })
            ]
          |> ok
        in
        let build = source_build batch in
        let input = B.prepared build in
        assert (S.Id.equal (R.entry input) (id "main.chatml"));
        assert (Option.is_none (R.find_source input (S.id source)));
        [%test_eq: int] (P.Accounting.source_count (P.Source_batch.accounting batch)) 2;
        [%test_eq: int]
          (List.count (R.definitions input) ~f:(fun definition ->
             S.Id.equal (R.Definition.implementation definition) (R.entry input)))
          1;
        let program =
          Chatml_compilation.compile_prepared_with_policy
            input
            ~policy:(Bounded Chatml_compilation.default_limits)
            ~max_dependency_depth:32
            ~env
            ~target:One_off_v1
            ()
          |> function
          | Ok program -> program
          | Error (error : Chatml_compilation.error) -> failwith error.message
        in
        let calls = ref 0 in
        let module Builtin = Chatml.Chatml_builtin_spec in
        let surface = Chatml.Chatml_extension_surface.one_off_v1 in
        let surface =
          { surface with
            globals =
              List.map surface.globals ~f:(fun (builtin : Builtin.builtin) ->
                if String.equal builtin.name "to_string"
                then
                  { builtin with
                    impl =
                      (fun arguments ->
                        incr calls;
                        builtin.impl arguments)
                  }
                else builtin)
          }
        in
        let value =
          Chatml_execution.run_prepared
            program
            ~env
            ~config:(Chatml_host_runtime.default_runtime_config ~surface ())
            ~entrypoint:"main"
            ~arguments:[ Chatml.Chatml_lang.VVariant ("Null", []) ]
            ()
          |> ok
        in
        assert (
          Chatml.Chatml_lang.equal_value value (VVariant ("String", [ VString "ok" ])));
        [%test_eq: int] !calls 1;
        printf
          "entry = example/main.chatml; catalog/body captured once per selected graph\n";
        printf "successful graph compilation; run: one entry initializer, result ok\n";
        let divergent =
          source_request
            (Captured_file
               { source = supplied (contents ^ " ")
               ; root_member = Some (path "main.chatml")
               })
        in
        (match capture host env#mono_clock [ divergent ] with
         | Error error ->
           (match P.Error.problem error with
            | Entry_revision_conflict actual ->
              assert (S.Id.equal actual (id "main.chatml"))
            | _ -> failwith "wrong revision conflict")
         | Ok _ -> failwith "divergent entry ran beside catalog revision");
        let unrelated =
          capture
            host
            env#mono_clock
            [ source_request
                (Captured_file { source; root_member = Some (path "outside.chatml") })
            ]
          |> ok
          |> source_build
          |> B.prepared
        in
        assert (S.Id.equal (R.entry unrelated) (S.id source));
        let fragment =
          capture host env#mono_clock [ source_request (Captured_fragment source) ]
          |> ok
          |> source_build
          |> B.prepared
        in
        assert (S.Id.equal (R.entry fragment) (S.id source));
        printf
          "changed member rejects; absent member and equal-text fragment keep prompt \
           identity\n");
      [%expect
        {|
        entry = example/main.chatml; catalog/body captured once per selected graph
        successful graph compilation; run: one entry initializer, result ok
        changed member rejects; absent member and equal-text fragment keep prompt identity
        |}]
    ;;

    let%test_unit
        "batch source and canonical-input ceilings follow a finite independent ledger"
      =
      with_host (fun env _directory host ->
        let requests = [ request Initial_prepare; request Initial_prepare ] in
        let single = capture host env#mono_clock [ List.hd_exn requests ] |> ok in
        let source_bytes =
          String.length (declaration ()) + String.length "let main = 1"
        in
        [%test_eq: int]
          (P.Accounting.source_bytes (P.Source_batch.accounting single))
          source_bytes;
        let canonical_bytes =
          R.Accounting.input_bytes (R.accounting (B.prepared (source_build single)))
        in
        let ceilings =
          [ ( limits
                ~sources:4
                ~bytes:(2 * source_bytes)
                ~prepared:(2 * canonical_bytes)
                ()
            , true )
          ; limits ~sources:3 (), false
          ; limits ~bytes:((2 * source_bytes) - 1) (), false
          ; limits ~prepared:((2 * canonical_bytes) - 1) (), false
          ]
        in
        List.iter ceilings ~f:(fun (limits, expected) ->
          let result = capture ~limits host env#mono_clock requests in
          assert (Bool.equal (Result.is_ok result) expected);
          match result with
          | Error error ->
            (match P.Error.problem error with
             | Limit_exceeded -> ()
             | Local failure ->
               (match Chatml_local_capture.Error.problem failure with
                | Limit_exceeded _ -> ()
                | _ -> failwith "batch source ceiling failed for another reason")
             | Composition failure ->
               (match B.Error.problem failure with
                | Limit_exceeded -> ()
                | Prepared failure ->
                  (match failure.R.Error.problem with
                   | Input_limit _ -> ()
                   | _ -> failwith "wrong prepared-input failure")
                | _ -> failwith "wrong composition limit failure")
             | _ -> failwith "batch failed outside its bounded ledgers")
          | Ok batch ->
            [%test_eq: int] (List.length (P.Source_batch.preparations batch)) 2;
            [%test_eq: int]
              (P.Accounting.source_bytes (P.Source_batch.accounting batch))
              (2 * source_bytes);
            [%test_eq: int]
              (P.Accounting.prepared_input_bytes (P.Source_batch.accounting batch))
              (2 * canonical_bytes));
        let empty = capture host env#mono_clock [] |> ok in
        assert (List.is_empty (P.Source_batch.preparations empty));
        [%test_eq: int] (P.Accounting.packages (P.Source_batch.accounting empty)) 0;
        [%test_eq: int]
          (P.Accounting.prepared_input_bytes (P.Source_batch.accounting empty))
          0)
    ;;

    let%test_unit
        "every observed source-batch checkpoint preserves interruption and host reuse"
      =
      with_host (fun env _directory host ->
        let requests =
          [ source_request
              (Captured_file
                 { source = supplied "let main = 1"
                 ; root_member = Some (path "main.chatml")
                 })
          ]
        in
        let observed = ref 0 in
        ignore
          (capture ~checkpoint:(fun () -> incr observed) host env#mono_clock requests
           |> ok
           : P.Source_batch.t);
        assert (!observed > 0);
        let marker = Failure "source-batch interrupted" in
        for stop = 1 to !observed do
          let calls = ref 0 in
          match
            capture host env#mono_clock requests ~checkpoint:(fun () ->
              incr calls;
              if !calls = stop then raise marker)
          with
          | exception exn -> assert (phys_equal exn marker)
          | Ok _ | Error _ -> failwith "source-batch interruption became a result"
        done;
        let reused = capture host env#mono_clock requests |> ok in
        [%test_eq: int] (List.length (P.Source_batch.preparations reused)) 1)
    ;;
  end)
;;
