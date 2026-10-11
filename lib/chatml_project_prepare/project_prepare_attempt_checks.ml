let%test_module "project preparation accounting" =
  (module struct
    open Core
    module A = Project_prepare_accounting
    module Attempt = Project_prepare_attempt
    module Error = Project_prepare_error
    module Limits = Project_prepare_limits
    module Policy = Project_prepare_policy

    let limits ~output ~work =
      Limits.create
        ~max_packages:100
        ~max_dependency_visits:100
        ~max_source_count:100
        ~max_source_bytes:1000
        ~max_local_directory_entries:100
        ~max_git_objects:100
        ~max_git_object_bytes:1000
        ~max_git_tree_entries:100
        ~max_native_request_bytes:1000
        ~max_native_output_bytes:output
        ~max_acquisition_payload_reserved:1000
        ~max_work_steps:work
        ~max_prepared_input_bytes:1000
        ~wall_time:(Time_ns.Span.of_sec 10.)
      |> Or_error.ok_exn
    ;;

    let policy ?(local_work = 100) ?(roots_work = 100) () =
      let document = Document_schema.Limits.default in
      Policy.create
        ~roots:
          (Chatml_local_roots.Limits.create
             ~max_locator_bytes:1024
             ~max_locator_components:16
             ~max_anchors:16
             ~max_retained_roots:32
             ~max_work_steps:roots_work
           |> Or_error.ok_exn)
        ~local:
          (Chatml_local_capture.Limits.create
             ~max_directory_entries:100
             ~max_sources:100
             ~max_source_bytes:1000
             ~max_total_source_bytes:1000
             ~max_path_components:16
             ~max_work_steps:local_work
           |> Or_error.ok_exn)
        ~git_objects:
          (Chatml_git_catalog.Limits.create
             ~max_object_bytes:1000
             ~max_total_object_bytes:1000
             ~max_objects:100
             ~max_tree_entries:100
             ~max_sources:100
             ~max_source_bytes:1000
             ~max_total_source_bytes:1000
             ~max_path_components:16
             ~max_work_steps:100
             ~document
           |> Or_error.ok_exn)
        ~git_native:
          (Chatml_git_host.Limits.create
             ~metadata_file_bytes:1000
             ~pack_file_bytes:1000
             ~max_acquisition_payload_bytes:1000
             ~max_request_bytes:1000
             ~max_output_bytes:1000
             ~max_work_steps:100
             ~wall_time:(Time_ns.Span.of_sec 10.)
             ~cpu_seconds_per_process:10
             ~memory_bytes_per_process:100_000_000
             ~open_files_per_process:64
           |> Or_error.ok_exn)
        ~composition:
          (Chatml_project_build.Limits.create
             ~max_work_steps:100
             ~max_input_bytes:(Some 1000)
             ~document
           |> Or_error.ok_exn)
    ;;

    let output bytes = { A.zero with native_output_bytes = bytes }

    let ok = function
      | Ok result -> result
      | Error _ -> failwith "unexpected admission failure"
    ;;

    let run ?(checkpoint = fun () -> ()) ~output f =
      Attempt.run
        ~limits:(limits ~output ~work:1000)
        ~policy:(policy ())
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~checkpoint
        ~f
    ;;

    let with_local_owner f =
      Eio_main.run (fun env ->
        let random = Cstruct.create 16 in
        Eio.Flow.read_exact (Eio.Stdenv.secure_random env) random;
        let suffix =
          Cstruct.to_string random
          |> String.to_list
          |> List.map ~f:(fun byte -> sprintf "%02x" (Char.to_int byte))
          |> String.concat
        in
        let path = Eio.Path.(Eio.Stdenv.fs env / "/tmp" / ("ochat-prepare-" ^ suffix)) in
        Eio.Path.mkdir ~perm:0o700 path;
        Fun.protect
          ~finally:(fun () -> Eio.Path.rmtree path)
          (fun () ->
             Eio.Path.with_open_dir path (fun root ->
               Chatml_local_source.with_root
                 root
                 ~max_path_components:16
                 ~checkpoint:ignore
                 ~f:(fun owner -> Ok (f path owner))
               |> ok)))
    ;;

    let local_attempt ?(roots_work = 100) f =
      Attempt.run
        ~limits:(limits ~output:1000 ~work:1_000_000)
        ~policy:(policy ~local_work:1_000_000 ~roots_work ())
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~checkpoint:ignore
        ~f
    ;;

    let capture_local attempt owner =
      Project_prepare_local.capture
        attempt
        ~owner
        ~project:(Source.Portable_path.create "chatml.project.json" |> Or_error.ok_exn)
        ~namespace_override:(Some "selected")
        ~selection:(Chatml_project.Selection.Library { environment = None })
    ;;

    let%expect_test
        "local bootstrap retains exact authored bytes under the selected identity"
      =
      with_local_owner (fun path owner ->
        let save name contents =
          Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(path / name) contents
        in
        let declaration namespace =
          sprintf
            {|{"schema_version":1,"namespace":"%s","modules":{"Code":{"implementation":"code.raw"}}}|}
            namespace
        in
        let old_declaration = declaration "authored" in
        save "chatml.project.json" old_declaration;
        save "code.raw" "first version";
        let (old, fresh), receipt =
          local_attempt (fun attempt ->
            let open Result.Let_syntax in
            let%bind old = capture_local attempt owner in
            save "chatml.project.json" (declaration "edited");
            save "code.raw" "second version";
            let%map fresh = capture_local attempt owner in
            old, fresh)
          |> ok
        in
        let module Local = Chatml_local_capture in
        let module Captured = Chatml_source in
        let old_source = Chatml_project.source (Local.project old) in
        [%test_eq: string] (Captured.contents old_source) old_declaration;
        [%test_eq: string]
          (Captured.Id.package_namespace (Captured.id old_source))
          "selected";
        let inventory = Local.sources old @ Local.sources fresh in
        [%test_eq: int] (A.source_count receipt) (List.length inventory);
        [%test_eq: int]
          (A.source_bytes receipt)
          (List.sum (module Int) inventory ~f:Captured.byte_length);
        let code value =
          Local.sources value
          |> List.find_exn ~f:(fun source ->
            String.equal (Captured.Id.relative_path (Captured.id source)) "code.raw")
          |> Captured.contents
        in
        printf
          "old authored namespace: %s\n"
          (Chatml_project.namespace (Local.project old));
        printf
          "new authored namespace: %s\n"
          (Chatml_project.namespace (Local.project fresh));
        printf "retained / fresh: %s / %s\n" (code old) (code fresh);
        printf "captured sources: %d\n" (A.source_count receipt));
      [%expect
        {|
        old authored namespace: authored
        new authored namespace: edited
        retained / fresh: first version / second version
        captured sources: 4
      |}]
    ;;

    let%test_unit "caught local bootstrap and catalog failures cannot complete an attempt"
      =
      with_local_owner (fun path owner ->
        List.iter
          [ {|{"schema_version":1,"namespace":""}|}, `Bootstrap
          ; ( {|{"schema_version":1,"namespace":"valid","modules":{"Missing":{"implementation":"missing.raw"}}}|}
            , `Catalog )
          ]
          ~f:(fun (declaration, expected_phase) ->
            Eio.Path.save
              ~create:(`Or_truncate 0o600)
              Eio.Path.(path / "chatml.project.json")
              declaration;
            let result =
              local_attempt (fun attempt ->
                (match capture_local attempt owner with
                 | Error error ->
                   (match expected_phase, Error.problem error with
                    | `Bootstrap, Bootstrap _ | `Catalog, Local _ -> ()
                    | _ -> assert false)
                 | Ok _ -> assert false);
                Ok ())
            in
            assert (Result.is_error result)))
    ;;

    let%test_unit "real root and pure stages reconcile their final receipts" =
      with_local_owner (fun path _ ->
        let declaration = {|{"schema_version":1,"namespace":"authored"}|} in
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          Eio.Path.(path / "chatml.project.json")
          declaration;
        Eio.Path.with_open_dir path (fun directory ->
          let module Roots = Chatml_local_roots in
          let module Stage = Project_prepare_stage in
          let policy = policy ~local_work:1_000_000 ~roots_work:1000 () in
          let anchor =
            Roots.Anchor.create
              directory
              ~host_prefix:"/workspace"
              ~max_path_components:16
              ~limits:policy.roots
              ~checkpoint:ignore
            |> ok
          in
          let (catalog, root_work, inner_work), receipt =
            local_attempt ~roots_work:1000 (fun attempt ->
              Stage.with_roots attempt ~anchors:[ anchor ] ~f:(fun roots ->
                let open Result.Let_syntax in
                let before = A.work_steps (Attempt.remaining attempt) in
                let native =
                  Roots.resolve roots ~base:None ~locator:"/workspace" ~checkpoint:ignore
                  |> ok
                  |> Roots.Cursor.owner
                in
                let%bind catalog = capture_local attempt native in
                let after_capture = A.work_steps (Attempt.accounting attempt) in
                let%map pure_work =
                  Stage.pure attempt ~f:(fun ~limits ~checkpoint ->
                    assert (A.work_steps (Attempt.remaining attempt) < before);
                    Chatml_project_build.Selection_gate.create
                      Initial_prepare
                      ~limits
                      ~checkpoint
                    |> Result.map ~f:(fun (_, receipt) ->
                      Chatml_project_build.Accounting.work_steps receipt, receipt))
                in
                [%test_eq: int]
                  (A.work_steps (Attempt.accounting attempt))
                  (after_capture + pure_work);
                let root_work = Roots.Accounting.work_steps (Roots.accounting roots) in
                catalog, root_work, A.work_steps (Attempt.accounting attempt)))
            |> Result.map_error ~f:(fun error ->
              match Error.problem error with
              | Roots failure ->
                raise_s [%sexp (Roots.Error.code failure : Roots.Error.code)]
              | Local failure ->
                raise_s
                  [%sexp
                    (Chatml_local_capture.Error.problem failure
                     : Chatml_local_capture.Error.problem)]
              | Composition _ -> failwith "pure stage failed"
              | Limit_exceeded -> failwith "aggregate stage allowance failed"
              | _ -> failwith "unexpected stage failure")
            |> ok
          in
          assert (root_work > 0);
          [%test_eq: int] (A.work_steps receipt) (root_work + inner_work);
          [%test_eq: int] (A.source_count receipt) 1;
          [%test_eq: int] (A.source_bytes receipt) (String.length declaration);
          [%test_eq: string]
            (Chatml_source.contents
               (Chatml_project.source (Chatml_local_capture.project catalog)))
            declaration))
    ;;

    let%test_unit "caught authority rejection prevents a subsequent stage from entering" =
      Eio_main.run (fun _ ->
        let host =
          Project_prepare_host.create
            []
            ~git_store:None
            ~git_grants:[]
            ~max_grants:0
            ~max_grant_bytes:0
          |> Or_error.ok_exn
        in
        let pin =
          Chatml_project.Git_pin.create ~object_format:Sha1 ~commit:(String.make 40 'a')
          |> Or_error.ok_exn
        in
        let rejected = ref None in
        let entered = ref false in
        let result =
          run ~output:10 (fun attempt ->
            (match
               Project_prepare_stage.with_git
                 attempt
                 ~host
                 ~repository:"https://example.invalid/repo"
                 ~pin
                 ~f:(fun _ ->
                   entered := true;
                   Ok ())
             with
             | Error error -> rejected := Some error
             | Ok _ -> assert false);
            ignore
              (Project_prepare_stage.pure attempt ~f:(fun ~limits ~checkpoint ->
                 entered := true;
                 Chatml_project_build.Selection_gate.create
                   Initial_prepare
                   ~limits
                   ~checkpoint)
               : (_, Error.t) Result.t);
            Ok ())
        in
        assert (not !entered);
        match result with
        | Error error ->
          assert (phys_equal error (Option.value_exn !rejected));
          (match Error.problem error with
           | Authority_denied -> ()
           | _ -> assert false)
        | Ok _ -> assert false)
    ;;

    let%expect_test "in-flight capacity is held until measured completion" =
      Eio_main.run (fun _ ->
        let _, receipt =
          run ~output:10 (fun attempt ->
            let reservation = Attempt.reserve attempt (output 8) |> ok in
            Attempt.charge attempt (output 2) |> ok;
            Attempt.finish attempt reservation ~actual:(output 3) |> ok;
            Attempt.charge attempt (output 5) |> ok;
            Ok ())
          |> ok
        in
        printf
          "reserved 8, observed 3; other stages used 7; total = %d\n"
          (A.native_output_bytes receipt));
      [%expect {| reserved 8, observed 3; other stages used 7; total = 10 |}]
    ;;

    let%test_unit "bounded schedules match an independent capacity equation" =
      Eio_main.run (fun _ ->
        for capacity = 0 to 7 do
          for reserved = 0 to capacity do
            for observed = 0 to reserved do
              for concurrent = 0 to capacity + 1 do
                for later = 0 to capacity + 1 do
                  let result =
                    run ~output:capacity (fun attempt ->
                      let open Result.Let_syntax in
                      let%bind token = Attempt.reserve attempt (output reserved) in
                      let%bind () = Attempt.charge attempt (output concurrent) in
                      let%bind () =
                        Attempt.finish attempt token ~actual:(output observed)
                      in
                      Attempt.charge attempt (output later))
                  in
                  let admitted =
                    reserved + concurrent <= capacity
                    && observed + concurrent + later <= capacity
                  in
                  assert (Bool.equal (Result.is_ok result) admitted);
                  match result with
                  | Error error ->
                    (match Error.problem error with
                     | Limit_exceeded -> ()
                     | _ -> assert false)
                  | Ok ((), receipt) ->
                    assert (A.native_output_bytes receipt = observed + concurrent + later)
                done
              done
            done
          done
        done)
    ;;

    let%test_unit "uncompleted, reused and foreign reservations cannot publish" =
      Eio_main.run (fun _ ->
        let invalid result =
          match result with
          | Error error ->
            (match Error.problem error with
             | Invalid_accounting -> ()
             | _ -> assert false)
          | Ok _ -> assert false
        in
        invalid
          (run ~output:10 (fun attempt ->
             ignore (Attempt.reserve attempt (output 1) |> ok : Attempt.Reservation.t);
             Ok ()));
        invalid
          (run ~output:10 (fun attempt ->
             let token = Attempt.reserve attempt (output 1) |> ok in
             Attempt.finish attempt token ~actual:(output 1) |> ok;
             Attempt.finish attempt token ~actual:A.zero));
        let result =
          run ~output:10 (fun outer ->
            let token = Attempt.reserve outer (output 1) |> ok in
            invalid
              (run ~output:10 (fun inner -> Attempt.finish inner token ~actual:A.zero));
            Attempt.finish outer token ~actual:A.zero)
        in
        ignore (ok result : unit * A.t))
    ;;

    let%test_unit "deadline and caught checkpoint failure poison the same invocation" =
      Eio_main.run (fun _ ->
        let clock = Eio_mock.Clock.Mono.make () in
        let expired =
          Attempt.run
            ~limits:(limits ~output:10 ~work:1000)
            ~policy:(policy ())
            ~clock
            ~checkpoint:(fun () -> ())
            ~f:(fun attempt ->
              Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 11_000_000_000L);
              (try Attempt.poll attempt with
               | _ -> ());
              Ok ())
        in
        (match expired with
         | Error error ->
           (match Error.problem error with
            | Timed_out -> ()
            | _ -> assert false)
         | Ok _ -> assert false);
        let callback_clock = Eio_mock.Clock.Mono.make () in
        let entered = ref false in
        let callback_expired =
          Attempt.run
            ~limits:(limits ~output:10 ~work:1000)
            ~policy:(policy ())
            ~clock:callback_clock
            ~checkpoint:(fun () ->
              Eio_mock.Clock.Mono.set_time
                callback_clock
                (Mtime.of_uint64_ns 11_000_000_000L))
            ~f:(fun _ ->
              entered := true;
              Ok ())
        in
        assert (not !entered);
        (match callback_expired with
         | Error error ->
           (match Error.problem error with
            | Timed_out -> ()
            | _ -> assert false)
         | Ok _ -> assert false);
        let cancellation =
          run ~output:10 (fun attempt ->
            (try
               Eio.Cancel.sub (fun context ->
                 Eio.Cancel.cancel context (Failure "cancel current stage");
                 Attempt.poll attempt)
             with
             | Eio.Cancel.Cancelled _ -> ());
            Ok ())
        in
        (match cancellation with
         | Error error ->
           (match Error.problem error with
            | Invalid_accounting -> ()
            | _ -> assert false)
         | Ok _ -> assert false);
        let callback_cancellation_entered = ref false in
        let callback_cancellation =
          try
            Eio.Cancel.sub (fun context ->
              ignore
                (run
                   ~output:10
                   ~checkpoint:(fun () ->
                     Eio.Cancel.cancel context (Failure "cancel in checkpoint"))
                   (fun _ ->
                      callback_cancellation_entered := true;
                      Ok ())
                 : (unit * A.t, Error.t) Result.t));
            false
          with
          | Eio.Cancel.Cancelled _ -> true
        in
        assert callback_cancellation;
        assert (not !callback_cancellation_entered);
        let exception Sentinel of int ref in
        let witness = ref 0 in
        let armed = ref false in
        let result =
          run
            ~output:10
            ~checkpoint:(fun () -> if !armed then raise (Sentinel witness))
            (fun attempt ->
               armed := true;
               (try
                  Attempt.poll attempt;
                  assert false
                with
                | Sentinel actual -> assert (phys_equal witness actual));
               armed := false;
               Ok ())
        in
        match result with
        | Error error ->
          (match Error.problem error with
           | Invalid_accounting -> ()
           | _ -> assert false)
        | Ok _ -> assert false)
    ;;

    let%test_unit "receipt admission is atomic across dimensions and overflow" =
      Eio_main.run (fun _ ->
        let invalid =
          run ~output:10 (fun attempt ->
            let token =
              Attempt.reserve
                attempt
                { A.zero with work_steps = 2; native_output_bytes = 8 }
              |> ok
            in
            let result =
              Attempt.finish
                attempt
                token
                ~actual:{ A.zero with work_steps = 3; native_output_bytes = 1 }
            in
            assert (A.work_steps (Attempt.accounting attempt) = 0);
            assert (A.native_output_bytes (Attempt.accounting attempt) = 0);
            result)
        in
        (match invalid with
         | Error error ->
           (match Error.problem error with
            | Invalid_accounting -> ()
            | _ -> assert false)
         | Ok _ -> assert false);
        let overflow =
          Attempt.run
            ~limits:(limits ~output:10 ~work:Int.max_value)
            ~policy:(policy ())
            ~clock:(Eio_mock.Clock.Mono.make ())
            ~checkpoint:(fun () -> ())
            ~f:(fun attempt ->
              Attempt.charge attempt { A.zero with work_steps = Int.max_value } |> ok;
              let result = Attempt.charge attempt { A.zero with work_steps = 1 } in
              assert (A.work_steps (Attempt.accounting attempt) = Int.max_value);
              result)
        in
        match overflow with
        | Error error ->
          (match Error.problem error with
           | Accounting_overflow -> ()
           | _ -> assert false)
        | Ok _ -> assert false)
    ;;
  end)
;;
