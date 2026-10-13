open Core
module Trace = Chatml_trace
module Coordinates = Source
module Source = Chatml_source
module Lang = Chatml.Chatml_lang
module Host = Chatml_host_runtime
module Execution = Chatml_execution
open Trace.Event.Kind
open Trace.Event.Operation
open Trace.Event.Phase
open Trace.Event.Failure
open Trace.Recorder.Mode
open Trace.Recorder.Snapshot
open Trace.Terminal.Disposition

let capture ~name contents =
  let id =
    Source.Id.create ~package_namespace:"trace-test" ~relative_path:name
    |> Or_error.ok_exn
  in
  Source.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint:(fun () ->
    ())
  |> Or_error.ok_exn
;;

let header source =
  let execution_id = Trace.Execution_id.of_opaque_string "run-1" |> Or_error.ok_exn in
  Trace.Header.of_submitted_capture
    ~execution_id
    ~submission_fingerprint:(Source.digest source)
    ~sources:[ source ]
    ~selection_fingerprint:(Source.digest source)
    ~max_inventory_entries:1
    ~max_header_bytes:4096
    ~checkpoint:(fun () -> ())
  |> Or_error.ok_exn
;;

let recorder source ~mode ~max_events ~max_normal_bytes =
  let limits =
    Trace.Recorder.Limits.create ~max_events ~max_normal_bytes ~max_terminal_bytes:128
    |> Or_error.ok_exn
  in
  Trace.Recorder.create (header source) ~mode ~limits
;;

let observe kind = Trace.Scope.observe_unlocated ~kind ~correlation:None
let prefix recorder = Trace.Recorder.snapshot recorder |> Trace.Recorder.Snapshot.prefix

let drops recorder =
  Trace.Recorder.snapshot recorder
  |> Trace.Recorder.Snapshot.omitted_after_prefix
  |> Option.value_map ~default:0L ~f:Trace.Recorder.Drop_count.at_least
;;

(* A reference byte writer for the documented canonical accounting layout.
   It is deliberately separate from the recorder's arithmetic admission path. *)
let reference_word buffer = Buffer.add_string buffer "\000\000\000\000\000\000\000\000"

let reference_id buffer id =
  reference_word buffer;
  Buffer.add_string buffer (Source.Id.package_namespace id);
  reference_word buffer;
  Buffer.add_string buffer (Source.Id.relative_path id)
;;

let reference_submitted_header_bytes source =
  let buffer = Buffer.create 256 in
  (* Version, execution-token length, admission and three option flags, count. *)
  for _ = 1 to 7 do
    reference_word buffer
  done;
  Buffer.add_string buffer "run-1";
  Buffer.add_string buffer (Source.Digest.to_hex (Source.digest source));
  Buffer.add_string buffer (Source.Digest.to_hex (Source.digest source));
  reference_id buffer (Source.id source);
  Buffer.add_string buffer (Source.Digest.to_hex (Source.digest source));
  reference_word buffer;
  Buffer.length buffer
;;

let reference_site_event_bytes source =
  let buffer = Buffer.create 256 in
  (* Version, sequence, kind, kind parameter, site flag, correlation flag. *)
  for _ = 1 to 6 do
    reference_word buffer
  done;
  reference_id buffer (Source.id source);
  Buffer.add_string buffer (Source.Digest.to_hex (Source.digest source));
  (* Two endpoints, each with line, column and byte offset. *)
  for _ = 1 to 6 do
    reference_word buffer
  done;
  Buffer.length buffer
;;

let%expect_test "canonical binary byte oracle sets exact admission boundaries" =
  let original = capture ~name:"binary.chatml" "x" in
  let id =
    Source.Id.create ~package_namespace:"non-utf8-\255\000" ~relative_path:"binary.chatml"
    |> Or_error.ok_exn
  in
  let source = Source.reidentify original ~id in
  let execution_id = Trace.Execution_id.of_opaque_string "run-1" |> Or_error.ok_exn in
  let make_header max_header_bytes =
    Trace.Header.of_submitted_capture
      ~execution_id
      ~submission_fingerprint:(Source.digest source)
      ~sources:[ source ]
      ~selection_fingerprint:(Source.digest source)
      ~max_inventory_entries:1
      ~max_header_bytes
      ~checkpoint:(fun () -> ())
  in
  let header_bytes = reference_submitted_header_bytes source in
  assert (Result.is_error (make_header (header_bytes - 1)));
  ignore (make_header header_bytes |> Or_error.ok_exn : Trace.Header.t);
  assert (
    Result.is_error
      (Trace.Header.of_submitted_capture
         ~execution_id
         ~submission_fingerprint:(Source.digest source)
         ~sources:[ source; source ]
         ~selection_fingerprint:(Source.digest source)
         ~max_inventory_entries:2
         ~max_header_bytes:(2 * header_bytes)
         ~checkpoint:(fun () -> ())));
  let colliding_id =
    Source.Id.create ~package_namespace:"non-utf8-\255\000" ~relative_path:"BINARY.chatml"
    |> Or_error.ok_exn
  in
  assert (
    Result.is_error
      (Trace.Header.of_submitted_capture
         ~execution_id
         ~submission_fingerprint:(Source.digest source)
         ~sources:[ source; Source.reidentify source ~id:colliding_id ]
         ~selection_fingerprint:(Source.digest source)
         ~max_inventory_entries:2
         ~max_header_bytes:(2 * header_bytes)
         ~checkpoint:(fun () -> ())));
  let location =
    Source.locate source ~start_byte:0 ~end_byte:1 ~checkpoint:(fun () -> ())
    |> Or_error.ok_exn
  in
  let span = Source.Location.span location in
  let event_bytes = reference_site_event_bytes source in
  Eio_main.run (fun _ ->
    List.iter
      [ event_bytes - 1, 0; event_bytes, 1 ]
      ~f:(fun (budget, expected) ->
        let recorder =
          recorder source ~mode:Full ~max_events:1 ~max_normal_bytes:budget
        in
        Trace.Scope.within (Some recorder) (fun () ->
          Trace.Scope.observe_span
            ~kind:Statement_entered
            ~source:(Some source)
            ~span
            ~correlation:None);
        [%test_eq: int] (List.length (prefix recorder)) expected));
  let terminal_limit bytes =
    Trace.Recorder.Limits.create
      ~max_events:0
      ~max_normal_bytes:0
      ~max_terminal_bytes:bytes
  in
  assert (Result.is_error (terminal_limit 63));
  ignore (terminal_limit 64 |> Or_error.ok_exn : Trace.Recorder.Limits.t);
  [%expect {| |}]
;;

let%expect_test "bounded prefix follows a small independent accounting model" =
  let source = capture ~name:"model.chatml" "" in
  Eio_main.run (fun _ ->
    for max_events = 0 to 5 do
      for byte_quanta = 0 to 5 do
        let recorder =
          recorder source ~mode:Full ~max_events ~max_normal_bytes:(48 * byte_quanta)
        in
        Trace.Scope.within (Some recorder) (fun () ->
          for _ = 1 to 7 do
            observe (Phase_started Evaluation)
          done);
        let expected_count = Int.min 7 (Int.min max_events byte_quanta) in
        [%test_eq: int] (List.length (prefix recorder)) expected_count;
        [%test_eq: int64] (drops recorder) (Int64.of_int (7 - expected_count));
        List.iteri (prefix recorder) ~f:(fun index event ->
          [%test_eq: int64] (Trace.Event.sequence event) (Int64.of_int (index + 1)))
      done
    done);
  [%expect {| |}]
;;

let%expect_test "oversized source identity saturates before site conversion" =
  let original = capture ~name:"wide.chatml" "x" in
  let id =
    Source.Id.create ~package_namespace:(String.make 512 'n') ~relative_path:"wide.chatml"
    |> Or_error.ok_exn
  in
  let source = Source.reidentify original ~id in
  let recorder = recorder source ~mode:Full ~max_events:2 ~max_normal_bytes:100 in
  let invalid_span =
    let out_of_range = Coordinates.{ line = 1; column = 100; offset = 100 } in
    Coordinates.{ left = out_of_range; right = out_of_range }
  in
  Eio_main.run (fun _ ->
    Trace.Scope.within (Some recorder) (fun () ->
      Trace.Scope.observe_span
        ~kind:Statement_entered
        ~source:(Some source)
        ~span:invalid_span
        ~correlation:None;
      Trace.Scope.observe_span
        ~kind:Call_entered
        ~source:(Some source)
        ~span:invalid_span
        ~correlation:None));
  [%test_eq: int] (List.length (prefix recorder)) 0;
  [%test_eq: int64] (drops recorder) 2L;
  [%expect {| |}]
;;

let%expect_test "summary omission is not a drop, and terminal survives saturation" =
  let source = capture ~name:"summary.chatml" "" in
  Eio_main.run (fun _ ->
    let recorder = recorder source ~mode:Summary ~max_events:1 ~max_normal_bytes:48 in
    Trace.Scope.within (Some recorder) (fun () ->
      observe Statement_entered;
      observe Call_entered;
      observe (Phase_started Evaluation);
      observe (Phase_finished Evaluation));
    [%test_eq: int] (List.length (prefix recorder)) 1;
    [%test_eq: int64] (drops recorder) 1L;
    let terminal =
      Trace.Terminal.create ~receipt_version:7 ~disposition:Succeeded |> Or_error.ok_exn
    in
    Trace.Recorder.finish recorder terminal |> Or_error.ok_exn;
    assert (Result.is_error (Trace.Recorder.finish recorder terminal));
    match Trace.Recorder.Snapshot.state (Trace.Recorder.snapshot recorder) with
    | Complete actual -> [%test_eq: int] (Trace.Terminal.receipt_version actual) 7
    | Incomplete -> failwith "terminal lost after saturation");
  [%expect {| |}]
;;

let%expect_test "nested traced and untraced scopes do not mix" =
  let source = capture ~name:"nested.chatml" "" in
  Eio_main.run (fun _ ->
    let outer = recorder source ~mode:Full ~max_events:8 ~max_normal_bytes:512 in
    let inner = recorder source ~mode:Full ~max_events:8 ~max_normal_bytes:512 in
    Trace.Scope.within (Some outer) (fun () ->
      observe (Phase_started Evaluation);
      Trace.Scope.within None (fun () -> observe (Phase_started Compilation));
      Trace.Scope.within (Some inner) (fun () -> observe (Phase_started Task));
      observe (Phase_finished Evaluation));
    [%test_eq: int] (List.length (prefix outer)) 2;
    [%test_eq: int] (List.length (prefix inner)) 1);
  [%expect {| |}]
;;

let%expect_test "independent fibers and late inherited child stay isolated" =
  let source = capture ~name:"fibers.chatml" "" in
  Eio_main.run (fun _ ->
    let first = recorder source ~mode:Full ~max_events:8 ~max_normal_bytes:512 in
    let second = recorder source ~mode:Full ~max_events:8 ~max_normal_bytes:512 in
    Eio.Fiber.both
      (fun () ->
         Trace.Scope.within (Some first) (fun () ->
           observe (Phase_started Evaluation);
           Eio.Fiber.yield ();
           observe (Phase_finished Evaluation)))
      (fun () ->
         Trace.Scope.within (Some second) (fun () ->
           observe (Phase_started Task);
           Eio.Fiber.yield ();
           observe (Phase_finished Task)));
    [%test_eq: int] (List.length (prefix first)) 2;
    [%test_eq: int] (List.length (prefix second)) 2;
    let release, release_u = Eio.Promise.create () in
    Eio.Switch.run (fun sw ->
      Trace.Scope.within (Some first) (fun () ->
        Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.await release;
          observe (Phase_started Compilation)));
      Eio.Promise.resolve release_u ());
    [%test_eq: int] (List.length (prefix first)) 2);
  [%expect {| |}]
;;

let%expect_test "recorder rejects a foreign domain before binding or reading" =
  let source = capture ~name:"domain.chatml" "" in
  let recorder = recorder source ~mode:Full ~max_events:2 ~max_normal_bytes:64 in
  let result =
    Domain.spawn (fun () ->
      let binding_rejected =
        match Trace.Scope.within (Some recorder) (fun () -> ()) with
        | () -> false
        | exception Invalid_argument _ -> true
      in
      let read_rejected =
        match Trace.Recorder.snapshot recorder with
        | _ -> false
        | exception Invalid_argument _ -> true
      in
      binding_rejected, read_rejected)
    |> Domain.join
  in
  [%test_eq: bool * bool] result (true, true);
  [%expect {| |}]
;;

let%expect_test "trusted host wait resumes only after a real Eio wait returns" =
  let source = capture ~name:"wait.chatml" "" in
  Eio_main.run (fun _ ->
    let active = recorder source ~mode:Summary ~max_events:8 ~max_normal_bytes:512 in
    let released, release = Eio.Promise.create () in
    Eio.Fiber.both
      (fun () ->
         Trace.Scope.within (Some active) (fun () ->
           Trace.Scope.with_host_wait ~site:None (fun () -> Eio.Promise.await released)))
      (fun () ->
         Eio.Fiber.yield ();
         Eio.Promise.resolve release ());
    let kinds = List.map (prefix active) ~f:Trace.Event.kind in
    assert (
      List.equal Trace.Event.Kind.equal kinds [ Host_wait_entered; Host_wait_resumed ]);
    let cancelled = recorder source ~mode:Summary ~max_events:8 ~max_normal_bytes:512 in
    let external_effects = ref 0 in
    let reason = Failure "cancel-wait" in
    (match
       Eio.Cancel.sub (fun cancellation ->
         Trace.Scope.within (Some cancelled) (fun () ->
           incr external_effects;
           Trace.Scope.with_host_wait ~site:None (fun () ->
             Eio.Cancel.cancel cancellation reason;
             Eio.Fiber.await_cancel ())))
     with
     | _ -> failwith "cancelled wait returned"
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual reason));
    [%test_eq: int] !external_effects 1;
    let kinds = List.map (prefix cancelled) ~f:Trace.Event.kind in
    assert (List.equal Trace.Event.Kind.equal kinds [ Host_wait_entered ]));
  [%expect {| |}]
;;

let%expect_test "plain evaluator remains valid outside Eio" =
  let env = Lang.create_env () in
  let program : Lang.resolved_program = { stmts = []; source_text = "" } in
  Chatml.Chatml_eval.eval_program env program;
  [%expect {| |}]
;;

let%expect_test "compile failures are observed on caller domain with owned source" =
  Eio_main.run (fun env ->
    let source = capture ~name:"broken.chatml" "let main input =" in
    let recorder = recorder source ~mode:Summary ~max_events:8 ~max_normal_bytes:4096 in
    Trace.Scope.within (Some recorder) (fun () ->
      assert (
        Result.is_error
          (Chatml_compilation.compile_captured
             source
             ~env
             ~target:Chatml_compilation.One_off_v1
             ())));
    match prefix recorder with
    | [ start; failure ] ->
      assert (Trace.Event.Kind.equal (Trace.Event.kind start) (Phase_started Compilation));
      assert (
        Trace.Event.Kind.equal (Trace.Event.kind failure) (Unrecovered_failure Parse));
      let site = Option.value_exn (Trace.Event.site failure) in
      assert (Source.Id.equal (Source.Location.source site) (Source.id source));
      assert (
        Source.Digest.equal (Source.Location.source_digest site) (Source.digest source))
    | _ -> failwith "unexpected compile trace");
  [%expect {| |}]
;;

let%expect_test "compile cancellation propagates without a false terminal" =
  Eio_main.run (fun env ->
    let source = capture ~name:"cancel.chatml" "let main input = Task.pure(input)" in
    let recorder = recorder source ~mode:Summary ~max_events:8 ~max_normal_bytes:1024 in
    let reason = Failure "trace-cancel" in
    (match
       Eio.Cancel.sub (fun cancellation ->
         Trace.Scope.within (Some recorder) (fun () ->
           Eio.Cancel.cancel cancellation reason;
           ignore
             (Chatml_compilation.compile_captured
                source
                ~env
                ~target:Chatml_compilation.One_off_v1
                ()
              : (Host.compiled_script, Chatml_compilation.error) result)))
     with
     | _ -> failwith "cancelled compile returned"
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual reason));
    (match Trace.Recorder.Snapshot.state (Trace.Recorder.snapshot recorder) with
     | Incomplete -> ()
     | Complete _ -> failwith "cancellation published terminal");
    [%test_eq: int] (List.length (prefix recorder)) 1);
  [%expect {| |}]
;;

let%expect_test "later execution observes an old closure at its defining source" =
  let defining_source = capture ~name:"old.chatml" "42" in
  let current_source = capture ~name:"new.chatml" "0" in
  let defining_site =
    Source.locate defining_source ~start_byte:0 ~end_byte:2 ~checkpoint:(fun () -> ())
    |> Or_error.ok_exn
  in
  let body : Lang.resolved_expr Lang.node =
    { value = REInt 42; span = Source.Location.span defining_site }
  in
  let closure =
    Lang.VClosure
      { params = []
      ; body
      ; env = Lang.create_env ~source:defining_source ()
      ; frames = []
      ; param_slots = []
      }
  in
  Eio_main.run (fun _ ->
    let recorder =
      recorder current_source ~mode:Full ~max_events:4 ~max_normal_bytes:4096
    in
    Trace.Scope.within (Some recorder) (fun () ->
      match Chatml.Chatml_eval.apply_value_result closure [] with
      | Ok (VInt 42) -> ()
      | Ok _ | Error _ -> failwith "old closure outcome changed");
    match prefix recorder with
    | [ event ] ->
      assert (Trace.Event.Kind.equal (Trace.Event.kind event) Call_entered);
      let site = Option.value_exn (Trace.Event.site event) in
      assert (Source.Id.equal (Source.Location.source site) (Source.id defining_source));
      assert (
        Source.Digest.equal
          (Source.Location.source_digest site)
          (Source.digest defining_source))
    | _ -> failwith "expected one call event");
  [%expect {| |}]
;;

let%expect_test "tracing modes preserve task success and caught or uncaught failure" =
  Eio_main.run (fun env ->
    List.iter
      [ "let main input = Task.pure(input)", true, false
      ; "let main input = Task.fail(\"boom\")", false, false
      ; ( {|let main input = Task.catch(
  Task.fail("boom"),
  fun error -> Task.pure(input))|}
        , true
        , true )
      ]
      ~f:(fun (text, should_succeed, should_recover) ->
        let source = capture ~name:"tasks.chatml" text in
        let program =
          Chatml_compilation.compile_captured
            source
            ~env
            ~target:Chatml_compilation.One_off_v1
            ()
          |> Result.map_error ~f:(fun error -> error.Chatml_compilation.message)
          |> Result.ok_or_failwith
        in
        let run () =
          let config =
            Host.default_runtime_config ~surface:(Host.compiled_surface program) ()
          in
          Host.run_entrypoint_detailed
            config
            program
            ~entrypoint:"main"
            ~arguments:[ Lang.VVariant ("Null", []) ]
            ()
        in
        let baseline = run () in
        [%test_eq: bool] (Result.is_ok baseline) should_succeed;
        List.iter [ Summary; Full ] ~f:(fun mode ->
          let recorder = recorder source ~mode ~max_events:128 ~max_normal_bytes:32768 in
          let observed = Trace.Scope.within (Some recorder) run in
          (match baseline, observed with
           | Ok before, Ok after -> assert (Lang.equal_value before after)
           | Error before, Error after -> [%test_eq: string] before.message after.message
           | Ok _, Error _ | Error _, Ok _ -> failwith "trace changed task outcome");
          let events = List.map (prefix recorder) ~f:Trace.Event.kind in
          [%test_eq: int]
            (List.count events ~f:(function
               | Recovered_failure Runtime -> true
               | _ -> false))
            (if should_recover then 1 else 0);
          [%test_eq: int]
            (List.count events ~f:(function
               | Unrecovered_failure Runtime -> true
               | _ -> false))
            (if should_succeed then 0 else 1);
          assert (
            List.exists events ~f:(function
              | Phase_started Evaluation -> true
              | _ -> false));
          if should_succeed
          then
            assert (
              List.exists events ~f:(function
                | Phase_finished Evaluation -> true
                | _ -> false))
          else
            assert (
              List.exists events ~f:(function
                | Unrecovered_failure Runtime -> true
                | _ -> false)))));
  [%expect {| |}]
;;

let%expect_test "initializer failure retains one source-owned final observation" =
  Eio_main.run (fun env ->
    let source =
      capture
        ~name:"initializer.chatml"
        "let poison = 1 / 0\nlet main input = Task.pure(input)"
    in
    let program =
      Chatml_compilation.compile_captured
        source
        ~env
        ~target:Chatml_compilation.One_off_v1
        ()
      |> Result.map_error ~f:(fun error -> error.Chatml_compilation.message)
      |> Result.ok_or_failwith
    in
    let recorder = recorder source ~mode:Summary ~max_events:8 ~max_normal_bytes:4096 in
    let result =
      Trace.Scope.within (Some recorder) (fun () ->
        let config =
          Host.default_runtime_config ~surface:(Host.compiled_surface program) ()
        in
        Host.run_entrypoint_detailed
          config
          program
          ~entrypoint:"main"
          ~arguments:[ Lang.VVariant ("Null", []) ]
          ())
    in
    assert (Result.is_error result);
    let failures =
      List.filter (prefix recorder) ~f:(fun event ->
        Trace.Event.Kind.equal (Trace.Event.kind event) (Unrecovered_failure Runtime))
    in
    match failures with
    | [ event ] ->
      let site = Option.value_exn (Trace.Event.site event) in
      assert (Source.Id.equal (Source.Location.source site) (Source.id source))
    | _ -> failwith "expected one initializer failure");
  [%expect {| |}]
;;

let%expect_test "persistent event and approval resume failures retain one final site" =
  Eio_main.run (fun _ ->
    let programs =
      [ ( {|let initial_state = 0
let on_event ctx state event = Task.pure(1 / state)|}
        , false )
      ; ( {|let initial_state = 0
let on_event ctx state event =
  Task.bind(Approval.ask_text("continue?"), fun answer -> Task.pure(1 / state))|}
        , true )
      ]
    in
    List.iter programs ~f:(fun (text, resume) ->
      let source = capture ~name:"persistent.chatml" text in
      let surface = Chatml.Chatml_builtin_surface.ui_moderator_surface in
      let compiled =
        Host.compile_captured_script source ~surface ()
        |> Result.map_error ~f:(fun error -> error.Host.formatted)
        |> Result.ok_or_failwith
      in
      let config = Host.default_runtime_config ~surface () in
      let session =
        match
          Host.instantiate_session_detailed
            config
            compiled
            ~entrypoints:
              { initial_state_name = "initial_state"; on_event_name = "on_event" }
        with
        | Ok session -> session
        | Error diagnostic -> failwith diagnostic.message
      in
      let recorder =
        recorder source ~mode:Summary ~max_events:32 ~max_normal_bytes:8192
      in
      let context =
        Lang.VRecord (String.Map.singleton "phase" (Lang.VString "turn_start"))
      in
      let event () =
        Host.handle_event_detailed
          session
          ~context
          ~event:(Lang.VVariant ("Turn_start", []))
      in
      let result =
        if resume
        then (
          (match Trace.Scope.within (Some recorder) event with
           | Ok () -> ()
           | Error diagnostic -> failwith diagnostic.message);
          Trace.Scope.within (Some recorder) (fun () ->
            Host.resume_ui_request_detailed session ~response:"continue"))
        else Trace.Scope.within (Some recorder) event
      in
      assert (Result.is_error result);
      let failures =
        List.filter (prefix recorder) ~f:(fun event ->
          Trace.Event.Kind.equal (Trace.Event.kind event) (Unrecovered_failure Runtime))
      in
      match failures with
      | [ event ] ->
        let site = Option.value_exn (Trace.Event.site event) in
        assert (Source.Id.equal (Source.Location.source site) (Source.id source))
      | _ -> failwith "expected one persistent failure"));
  [%expect {| |}]
;;

let%expect_test "effect observations preserve host-call count and order" =
  Eio_main.run (fun env ->
    let source =
      capture
        ~name:"effect.chatml"
        {|let main input = Task.bind(Tool.call("fixture", input), fun ignored -> Task.pure(input))|}
    in
    let program =
      Chatml_compilation.compile_captured
        source
        ~env
        ~target:Chatml_compilation.One_off_v1
        ()
      |> Result.map_error ~f:(fun error -> error.Chatml_compilation.message)
      |> Result.ok_or_failwith
    in
    let calls = ref 0 in
    let handlers =
      { Host.default_handlers with
        on_tool_call =
          (fun _ ~name:_ ~args:_ ->
            incr calls;
            Ok (Lang.VVariant ("Ok", [ Lang.VVariant ("Null", []) ])))
      }
    in
    let run () =
      let config =
        Host.default_runtime_config ~surface:(Host.compiled_surface program) ~handlers ()
      in
      Host.run_entrypoint_detailed
        config
        program
        ~entrypoint:"main"
        ~arguments:[ Lang.VVariant ("Null", []) ]
        ()
    in
    let baseline = run () in
    [%test_eq: int] !calls 1;
    let recorder = recorder source ~mode:Summary ~max_events:16 ~max_normal_bytes:4096 in
    let observed = Trace.Scope.within (Some recorder) run in
    [%test_eq: int] !calls 2;
    (match baseline, observed with
     | Ok before, Ok after -> assert (Lang.equal_value before after)
     | _ -> failwith "trace changed effect outcome");
    let effect_kinds =
      List.filter_map (prefix recorder) ~f:(fun event ->
        match Trace.Event.kind event with
        | (Effect_attempted _ | Effect_host_returned _) as kind -> Some kind
        | _ -> None)
    in
    assert (
      List.equal
        Trace.Event.Kind.equal
        effect_kinds
        [ Effect_attempted Tool_call; Effect_host_returned Tool_call ]));
  [%expect {| |}]
;;

let%expect_test "bounded execution is invariant across mode and trace capacity" =
  Eio_main.run (fun env ->
    let source =
      capture
        ~name:"budget.chatml"
        {|let main input = Task.bind(Tool.call("first", input), fun ignored ->
  Task.bind(Tool.call("second", input), fun ignored -> Task.pure(input)))|}
    in
    let program =
      Chatml_compilation.compile_captured
        source
        ~env
        ~target:Chatml_compilation.One_off_v1
        ()
      |> Result.map_error ~f:(fun error -> error.Chatml_compilation.message)
      |> Result.ok_or_failwith
    in
    let run ~max_calls ~mode ~capacity =
      let calls = ref 0 in
      let checkpoints = ref 0 in
      let handlers =
        { Host.default_handlers with
          on_tool_call =
            (fun _ ~name:_ ~args:_ ->
              incr calls;
              Ok (Lang.VVariant ("Ok", [ Lang.VVariant ("Null", []) ])))
        }
      in
      let config =
        Host.default_runtime_config ~surface:(Host.compiled_surface program) ~handlers ()
      in
      let runner =
        Execution.create_runner
          ~env
          ~policy:
            (Execution.Bounded { Execution.default_limits with fuel = 1000; max_calls })
          ()
      in
      let raw_control = Execution.runner_control runner in
      let control =
        { raw_control with
          checkpoint =
            (fun () ->
              incr checkpoints;
              raw_control.checkpoint ())
        }
      in
      let trace =
        Option.map mode ~f:(fun mode ->
          recorder source ~mode ~max_events:(capacity / 48) ~max_normal_bytes:capacity)
      in
      let outcome =
        Trace.Scope.within trace (fun () ->
          Execution.run_scoped runner (fun () ->
            Host.run_entrypoint_detailed
              ~control
              config
              program
              ~entrypoint:"main"
              ~arguments:[ Lang.VVariant ("Null", []) ]
              ()))
      in
      let disposition =
        match outcome with
        | Ok (Ok (Lang.VVariant ("Null", []))) -> "success"
        | Ok (Ok _) -> "unexpected-value"
        | Ok (Error diagnostic) -> "runtime:" ^ diagnostic.message
        | Error error -> "budget:" ^ error.code
      in
      Option.iter trace ~f:(fun recorder ->
        if Int.equal capacity 0
        then
          assert (
            Option.is_some
              (Trace.Recorder.Snapshot.omitted_after_prefix
                 (Trace.Recorder.snapshot recorder))));
      disposition, !calls, !checkpoints
    in
    List.iter [ 1; 2 ] ~f:(fun max_calls ->
      let baseline = run ~max_calls ~mode:None ~capacity:4096 in
      let disposition, calls, _ = baseline in
      [%test_eq: int] calls max_calls;
      if Int.equal max_calls 1
      then assert (String.is_prefix disposition ~prefix:"budget:")
      else [%test_eq: string] disposition "success";
      List.iter [ None; Some Summary; Some Full ] ~f:(fun mode ->
        List.iter [ 0; 48; 4096 ] ~f:(fun capacity ->
          [%test_eq: string * int * int] (run ~max_calls ~mode ~capacity) baseline))));
  [%expect {| |}]
;;

let%expect_test "cancellation after a completed host effect preserves partial effects" =
  Eio_main.run (fun env ->
    let source =
      capture
        ~name:"cancel-effect.chatml"
        {|let main input = Task.bind(Tool.call("first", input), fun ignored ->
  Task.bind(Tool.call("second", input), fun ignored -> Task.pure(input)))|}
    in
    let program =
      Chatml_compilation.compile_captured
        source
        ~env
        ~target:Chatml_compilation.One_off_v1
        ()
      |> Result.map_error ~f:(fun error -> error.Chatml_compilation.message)
      |> Result.ok_or_failwith
    in
    let reason = Failure "cancel-after-host-effect" in
    let run mode capacity =
      let calls = ref 0 in
      let checkpoints = ref 0 in
      let trace =
        Option.map mode ~f:(fun mode ->
          recorder source ~mode ~max_events:(capacity / 48) ~max_normal_bytes:capacity)
      in
      (match
         Eio.Cancel.sub (fun cancellation ->
           let handlers =
             { Host.default_handlers with
               on_tool_call =
                 (fun _ ~name:_ ~args:_ ->
                   incr calls;
                   Ok (Lang.VVariant ("Ok", [ Lang.VVariant ("Null", []) ])))
             }
           in
           let config =
             Host.default_runtime_config
               ~surface:(Host.compiled_surface program)
               ~handlers
               ()
           in
           let runner =
             Execution.create_runner
               ~env
               ~policy:
                 (Execution.Bounded
                    { Execution.default_limits with fuel = 1000; max_calls = 2 })
               ()
           in
           let raw_control = Execution.runner_control runner in
           let control =
             { raw_control with
               checkpoint =
                 (fun () ->
                   incr checkpoints;
                   raw_control.checkpoint ())
             ; after_effect =
                 (fun value ->
                   raw_control.after_effect value;
                   if Int.equal !calls 1
                   then (
                     Eio.Cancel.cancel cancellation reason;
                     Eio.Fiber.yield ()))
             }
           in
           Trace.Scope.within trace (fun () ->
             Execution.run_scoped runner (fun () ->
               Host.run_entrypoint_detailed
                 ~control
                 config
                 program
                 ~entrypoint:"main"
                 ~arguments:[ Lang.VVariant ("Null", []) ]
                 ())))
       with
       | _ -> failwith "cancelled run returned"
       | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual reason));
      !calls, !checkpoints
    in
    let baseline = run None 4096 in
    [%test_eq: int] (fst baseline) 1;
    List.iter [ None; Some Summary; Some Full ] ~f:(fun mode ->
      List.iter [ 0; 48; 4096 ] ~f:(fun capacity ->
        [%test_eq: int * int] (run mode capacity) baseline)));
  [%expect {| |}]
;;
