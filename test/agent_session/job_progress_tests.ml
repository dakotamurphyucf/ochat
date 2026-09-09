open Core
open Fixtures
open Job_fixtures
module Progress = Agent_protocol.Job_progress

let%expect_test
    "native job progress is bounded, transient and cannot outlive its invocation"
  =
  with_actor (fun env sw actor writer backend ->
    let started, started_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let finished, finished_u = Eio.Promise.create () in
    let calls = ref 0 in
    let escaped = ref None in
    let parent = add_claimed_job actor in
    let base = native_registry calls ~raises:false in
    let native =
      C.find base ~name:"read_file"
      |> Background_execution_tests.cap
      |> C.native_implementation
      |> Option.value_exn
    in
    let chunk = String.concat (List.init 2048 ~f:(fun _ -> "é")) in
    let implementation =
      { native with
        run_with_progress =
          (fun ~invocation input ->
            assert (Ochat_function.Invocation.is_observed invocation);
            escaped := Some invocation;
            for _index = 1 to 6 do
              Ochat_function.Invocation.emit
                invocation
                { channel = `Stdout; update = Append chunk };
              A.read_job actor ~job_id:parent.id |> protocol_ok |> ignore
            done;
            Ochat_function.Invocation.emit
              invocation
              { channel = `Stderr; update = Replace "warning" };
            A.read_job actor ~job_id:parent.id |> protocol_ok |> ignore;
            Eio.Promise.resolve started_u ();
            (* Race live reads with another domain's producer. Display traffic
               must not consume command capacity or await actor responses. *)
            Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
              for _index = 1 to 10_000 do
                Ochat_function.Invocation.emit
                  invocation
                  { channel = `Stdout; update = Append chunk }
              done);
            Ochat_function.Invocation.emit
              invocation
              { channel = `Stderr; update = Replace (String.make 4097 'x') };
            Ochat_function.Invocation.emit
              invocation
              { channel = `Stderr; update = Replace "\255" };
            Eio.Promise.await release;
            native.run_with_progress ~invocation input)
      }
    in
    let registry =
      C.create
        ~owner:"progress-fixture"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
        [ Chatmd_shell_spec.Source_ref.digest "progress-v1", implementation ]
      |> Background_execution_tests.cap
    in
    let request =
      Background_execution_tests.capture_tool
        registry
        Chat_response.One_off_request.default_policy
    in
    (* The owned parent request must match the request actually executed. *)
    let parent =
      { parent with payload = Chat_response.Background_request.to_json request }
    in
    A.change_job actor ~attachment_id:writer.id parent |> protocol_ok |> ignore;
    let tools =
      Background_execution_tests.tools (fun () -> registry)
      |> fun tools ->
      Agent_session.Script_tool_calls.with_progress
        tools
        ~emit:(fun invocation progress ->
          A.publish_job_progress actor ~invocation_id:invocation.context.id progress)
    in
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve
        finished_u
        (Background_execution_tests.run env actor parent request tools));
    Eio.Promise.await started;
    let rec await_progress () =
      let live = A.read_job actor ~job_id:parent.id |> protocol_ok in
      match live.progress with
      | Some progress when List.length progress.channels = 2 -> live
      | _ ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.001;
        await_progress ()
    in
    let live = await_progress () in
    let progress = Option.value_exn live.progress in
    assert (progress.sequence <= Progress.max_updates);
    let decoded = Progress.of_json (Progress.to_json progress) |> protocol_ok in
    List.iter decoded.channels ~f:(fun item ->
      assert (Stdlib.String.is_valid_utf_8 item.text);
      [%test_eq: bool] true (String.length item.text <= Progress.max_channel_bytes));
    print_s
      [%sexp
        (List.map decoded.channels ~f:(fun item ->
           item.channel, String.length item.text, item.truncated)
         : (Progress.channel * int * bool) list)];
    let stored = Agent_session.Memory_backend.state backend in
    assert (List.for_all stored.jobs ~f:(fun job -> Option.is_none job.progress));
    let corrupt =
      { stored with
        jobs =
          List.map stored.jobs ~f:(fun job ->
            if Agent_protocol.Id.Job.equal job.id live.id then live else job)
      }
    in
    assert (
      Result.is_error
        (Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t corrupt))));
    Eio.Promise.resolve release_u ();
    let result = Eio.Promise.await finished |> protocol_ok in
    (match result.resolved.status with
     | Resolved (Complete (`String "disclosed")) -> ()
     | status -> raise_s [%sexp (status : I.status)]);
    A.complete_background_job
      actor
      ~job_id:parent.id
      ~generation:parent.generation
      ~attempt:parent.attempt
      (Succeeded (`String "disclosed"))
    |> protocol_ok
    |> ignore;
    Ochat_function.Invocation.emit
      (Option.value_exn !escaped)
      { channel = `Stdout; update = Replace "escaped" };
    A.publish_job_progress
      actor
      ~invocation_id:result.resolved.context.id
      { channel = `Stdout; update = Replace "forged late update" };
    let final = A.read_job actor ~job_id:parent.id |> protocol_ok in
    assert (Option.is_none final.progress);
    [%test_eq: int] 1 !calls;
    assert (Option.is_some (J.terminal_completion final |> protocol_ok));
    print_endline "result retained; progress absent from storage and expired callbacks");
  [%expect
    {|
    ((Stdout 8192 true) (Stderr 7 false))
    result retained; progress absent from storage and expired callbacks
    |}]
;;
