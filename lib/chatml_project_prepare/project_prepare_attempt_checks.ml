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

    let policy () =
      let document = Document_schema.Limits.default in
      Policy.create
        ~roots:
          (Chatml_local_roots.Limits.create
             ~max_locator_bytes:1024
             ~max_locator_components:16
             ~max_anchors:16
             ~max_retained_roots:32
             ~max_work_steps:100
           |> Or_error.ok_exn)
        ~local:
          (Chatml_local_capture.Limits.create
             ~max_directory_entries:100
             ~max_sources:100
             ~max_source_bytes:1000
             ~max_total_source_bytes:1000
             ~max_path_components:16
             ~max_work_steps:100
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
