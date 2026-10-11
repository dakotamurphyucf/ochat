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

    let limits () =
      P.Limits.create
        ~max_packages:100
        ~max_dependency_visits:100
        ~max_source_count:100
        ~max_source_bytes:1_000_000
        ~max_local_directory_entries:100
        ~max_git_objects:100
        ~max_git_object_bytes:1_000_000
        ~max_git_tree_entries:100
        ~max_native_request_bytes:100_000
        ~max_native_output_bytes:1_000_000
        ~max_acquisition_payload_reserved:10_000_000
        ~max_work_steps:10_000_000
        ~max_prepared_input_bytes:1_000_000
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
  end)
;;
