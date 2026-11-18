open! Core
module Switch = Eio.Switch
module Report = Switch.Release_report
module Outcome = Switch.Outcome

let complete outcome ~attempted =
  let report = Outcome.release outcome in
  (match Report.status report with
   | Complete -> ()
   | Not_entered | Incomplete -> failwith "actual release did not complete");
  assert (Int.equal attempted (Report.attempted report));
  report
;;

let primary_case ~hook_failure =
  let original = Failure "original callback" in
  let hooks = ref 0 in
  let outcome =
    Switch.run_reported (fun sw ->
      Switch.on_release sw (fun () ->
        Int.incr hooks;
        if hook_failure then raise original);
      raise original)
  in
  assert (Int.equal 1 !hooks);
  (match Outcome.result outcome with
   | Error (actual, _) when phys_equal actual original -> ()
   | Error (actual, trace) -> Exn.raise_with_original_backtrace actual trace
   | Ok _ -> failwith "original callback exception was lost");
  let report = complete outcome ~attempted:1 in
  (match hook_failure, Report.failures report with
   | false, [] -> ()
   | true, [ (actual, _) ] when phys_equal actual original -> ()
   | _ -> failwith "same-object release failure was lost");
  let ordinary =
    try
      Ok
        (Switch.run (fun sw ->
           Switch.on_release sw (fun () -> if hook_failure then raise original);
           raise original))
    with
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  (match ordinary with
   | Error (actual, _) when phys_equal actual original -> ()
   | Error (actual, trace) -> Exn.raise_with_original_backtrace actual trace
   | Ok _ -> failwith "ordinary Switch.run changed its original exception");
  Printf.printf
    "callback: hook_failure=%b; original outcome and actual release report\n"
    hook_failure
;;

let cancellation_case ~hook_failure =
  let hooks = ref 0 in
  let actual_hook_failure = Eio.Cancel.Cancelled Exit in
  let outcome =
    Eio.Cancel.sub (fun cancel ->
      Switch.run_reported (fun sw ->
        Switch.on_release sw (fun () ->
          Int.incr hooks;
          if hook_failure then raise actual_hook_failure);
        Eio.Cancel.cancel cancel Exit;
        Eio.Fiber.yield ()))
  in
  assert (Int.equal 1 !hooks);
  (match Outcome.result outcome with
   | Error (Eio.Cancel.Cancelled Exit, _) -> ()
   | Error (actual, trace) -> Exn.raise_with_original_backtrace actual trace
   | Ok _ -> failwith "requested cancellation was lost");
  let report = complete outcome ~attempted:1 in
  (match hook_failure, Report.failures report with
   | false, [] -> ()
   | true, [ (actual, _) ] when phys_equal actual actual_hook_failure -> ()
   | _ -> failwith "cancelled release hook was hidden by normal cancellation");
  let ordinary =
    try
      Ok
        (Eio.Cancel.sub (fun cancel ->
           Switch.run (fun sw ->
             Switch.on_release sw (fun () ->
               if hook_failure then raise actual_hook_failure);
             Eio.Cancel.cancel cancel Exit;
             Eio.Fiber.yield ())))
    with
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  (match ordinary with
   | Error (Eio.Cancel.Cancelled Exit, _) -> ()
   | Error (actual, trace) -> Exn.raise_with_original_backtrace actual trace
   | Ok _ -> failwith "ordinary Switch.run changed requested cancellation");
  Printf.printf
    "cancellation: hook_failure=%b; original outcome and actual release report\n"
    hook_failure
;;

let child_case () =
  let child_failure = Failure "actual child failure" in
  let child_exited = ref false in
  let hook_called = ref false in
  let outcome =
    Switch.run_reported (fun sw ->
      Switch.on_release sw (fun () ->
        assert !child_exited;
        hook_called := true);
      Eio.Fiber.fork ~sw (fun () ->
        Exn.protect
          ~finally:(fun () -> child_exited := true)
          ~f:(fun () -> raise child_failure));
      let never, _ = Eio.Promise.create () in
      Eio.Promise.await never)
  in
  assert !child_exited;
  assert !hook_called;
  (match Outcome.result outcome with
   | Error (actual, _) when phys_equal actual child_failure -> ()
   | Error (actual, trace) -> Exn.raise_with_original_backtrace actual trace
   | Ok _ -> failwith "child failure disappeared");
  let report = complete outcome ~attempted:1 in
  assert (List.is_empty (Report.failures report));
  print_endline "child failure cancelled and joined; hook report remains independent"
;;

let early_and_preentry_cases () =
  let hook_called = ref false in
  let healthy =
    Switch.run_reported (fun sw ->
      Switch.on_release sw (fun () -> hook_called := true);
      31)
  in
  assert !hook_called;
  (match Outcome.result healthy with
   | Ok value -> [%test_eq: int] 31 value
   | Error (actual, trace) -> Exn.raise_with_original_backtrace actual trace);
  let healthy_report = complete healthy ~attempted:1 in
  assert (List.is_empty (Report.failures healthy_report));
  let original = Failure "before any release registration" in
  let early = Switch.run_reported (fun _ -> raise original) in
  let report = complete early ~attempted:0 in
  assert (List.is_empty (Report.failures report));
  let called = ref false in
  let before =
    Eio.Cancel.sub (fun cancel ->
      Eio.Cancel.cancel cancel Exit;
      Switch.run_reported (fun _ -> called := true))
  in
  assert (not !called);
  let report = Outcome.release before in
  (match Report.status report, Outcome.result before with
   | Not_entered, Error (Eio.Cancel.Cancelled Exit, _) -> ()
   | _ -> failwith "cancelled entry fabricated an acquired release report");
  assert (Int.equal 0 (Report.attempted report));
  assert (List.is_empty (Report.failures report));
  print_endline
    "healthy return and early body error released normally; cancelled entry never entered"
;;

let ordered_same_object_hooks () =
  let original = Failure "same actual hook exception" in
  let order = ref [] in
  let saved_trace = ref None in
  let outcome =
    Switch.run_reported (fun sw ->
      Switch.on_release sw (fun () ->
        order := 1 :: !order;
        raise original);
      Switch.on_release sw (fun () -> order := 2 :: !order);
      Switch.on_release sw (fun () ->
        order := 3 :: !order;
        try raise original with
        | exn ->
          let trace = Stdlib.Printexc.get_raw_backtrace () in
          saved_trace := Some (Stdlib.Printexc.raw_backtrace_to_string trace);
          Exn.raise_with_original_backtrace exn trace))
  in
  assert (List.equal Int.equal [ 1; 2; 3 ] !order);
  let report = complete outcome ~attempted:3 in
  (match Report.failures report with
   | [ (first, trace); (second, _) ] ->
     (* Identity and duplicate occurrence are explicit public report contracts. *)
     assert (phys_equal first original);
     assert (phys_equal second original);
     let original_trace = Option.value_exn !saved_trace in
     let observed_trace = Stdlib.Printexc.raw_backtrace_to_string trace in
     assert (not (String.is_empty original_trace));
     assert (String.is_substring original_trace ~substring:"ordered_same_object_hooks");
     (* Reraising through the hook's protected scope legitimately appends frames.
        The actual original raise site and all its frames must remain intact. *)
     assert (String.is_prefix observed_trace ~prefix:original_trace)
   | _ -> failwith "actual hook failures deduplicated or hooks skipped");
  print_endline "all LIFO hooks attempted; duplicate exceptions and raw trace retained"
;;

let removed_and_late_hooks () =
  let removed_calls = ref 0 in
  let queued_calls = ref 0 in
  let late_calls = ref 0 in
  let after_calls = ref 0 in
  let original = Failure "actual late release failure" in
  let propagated = ref None in
  let captured_switch = ref None in
  let outcome =
    Switch.run_reported (fun sw ->
      captured_switch := Some sw;
      let removed =
        Switch.on_release_cancellable sw (fun () ->
          Int.incr removed_calls;
          failwith "removed hook ran")
      in
      assert (Switch.try_remove_hook removed);
      assert (not (Switch.try_remove_hook removed));
      Switch.on_release sw (fun () -> Int.incr queued_calls);
      Switch.on_release sw (fun () ->
        Int.incr queued_calls;
        try
          (* The queue is already closed. This runs immediately in this queued
             caller; its propagated exception is that caller's actual failure. *)
          Switch.on_release sw (fun () ->
            Int.incr late_calls;
            raise original)
        with
        | exn ->
          let trace = Stdlib.Printexc.get_raw_backtrace () in
          propagated := Some exn;
          Exn.raise_with_original_backtrace exn trace))
  in
  assert (Int.equal 0 !removed_calls);
  assert (Int.equal 2 !queued_calls);
  assert (Int.equal 1 !late_calls);
  let observed = Option.value_exn !propagated in
  (match Outcome.result outcome with
   | Error (actual, _) when phys_equal actual observed -> ()
   | Error (actual, trace) -> Exn.raise_with_original_backtrace actual trace
   | Ok _ -> failwith "late exception did not propagate through actual queued caller");
  let report = complete outcome ~attempted:2 in
  let exact_failure () =
    match Report.failures report with
    | [ (actual, _) ] when phys_equal actual observed -> ()
    | _ -> failwith "late immediate callback fabricated a separate queued observation"
  in
  exact_failure ();
  let sw = Option.value_exn !captured_switch in
  (match Switch.on_release sw (fun () -> Int.incr after_calls) with
   | () -> failwith "late registration did not reject the finished switch"
   | exception Invalid_argument _ -> ());
  assert (Int.equal 1 !after_calls);
  ignore (complete outcome ~attempted:2 : Report.t);
  exact_failure ();
  print_endline
    "removed hook excluded; late error propagates through queued caller; terminal report \
     immutable"
;;

type Eio.Exn.err += Release_reporting_probe of string

let io_combination () =
  let original = Eio.Exn.create (Release_reporting_probe "original callback IO") in
  let hook_failure = Eio.Exn.create (Release_reporting_probe "actual release IO") in
  let outcome =
    Switch.run_reported (fun sw ->
      Switch.on_release sw (fun () -> raise hook_failure);
      raise original)
  in
  (match Outcome.result outcome with
   | Error (Eio.Io (Eio.Exn.Multiple_io errors, _), _) ->
     [%test_eq: int] 2 (List.length errors)
   | Error (actual, trace) -> Exn.raise_with_original_backtrace actual trace
   | Ok _ -> failwith "ordinary IO aggregation disappeared");
  let report = complete outcome ~attempted:1 in
  (match Report.failures report with
   | [ (actual, _) ] when phys_equal actual hook_failure -> ()
   | _ -> failwith "IO combination lost the actual hook exception object");
  print_endline "ordinary IO errors combine; actual hook IO identity remains exact"
;;

let with_runtime f =
  Eio_main.run (fun env -> Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. f)
;;

let%expect_test "actual hook failures remain independent of ordinary switch outcomes" =
  with_runtime (fun () ->
    List.iter [ false; true ] ~f:(fun hook_failure -> primary_case ~hook_failure);
    List.iter [ false; true ] ~f:(fun hook_failure -> cancellation_case ~hook_failure);
    io_combination ());
  [%expect
    {|
    callback: hook_failure=false; original outcome and actual release report
    callback: hook_failure=true; original outcome and actual release report
    cancellation: hook_failure=false; original outcome and actual release report
    cancellation: hook_failure=true; original outcome and actual release report
    ordinary IO errors combine; actual hook IO identity remains exact
    |}]
;;

let%expect_test "actual child joining and switch entry keep existing cancellation laws" =
  with_runtime (fun () ->
    child_case ();
    early_and_preentry_cases ();
    Eio.Cancel.sub (fun cancel ->
      Eio.Cancel.cancel cancel Exit;
      let called = ref false in
      let value =
        Switch.run_protected (fun _ ->
          called := true;
          Eio.Fiber.yield ();
          17)
      in
      assert !called;
      [%test_eq: int] 17 value);
    print_endline "ordinary protected switch still enters under cancelled parent");
  [%expect
    {|
    child failure cancelled and joined; hook report remains independent
    healthy return and early body error released normally; cancelled entry never entered
    ordinary protected switch still enters under cancelled parent
    |}]
;;

let%expect_test "release observations preserve order, origin and registration boundaries" =
  let previous = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous)
    ~f:(fun () ->
      Stdlib.Printexc.record_backtrace true;
      with_runtime (fun () ->
        ordered_same_object_hooks ();
        removed_and_late_hooks ()));
  [%expect
    {|
    all LIFO hooks attempted; duplicate exceptions and raw trace retained
    removed hook excluded; late error propagates through queued caller; terminal report immutable
    |}]
;;
