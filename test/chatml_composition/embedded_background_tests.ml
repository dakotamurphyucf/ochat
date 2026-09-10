open Core
open Agent_server_test_support
module P = Agent_protocol
module Embedded = Agent_server.Embedded
module F = Embedded_extension_tests

type finish =
  | Complete_after_disconnect
  | Close_host
[@@deriving sexp_of]

let%expect_test
    "embedded moderator jobs survive client disconnect but are reaped when their host \
     closes"
  =
  List.iter [ false; true ] ~f:(fun durable ->
    List.iter [ Complete_after_disconnect; Close_host ] ~f:(fun finish ->
      let calls = ref 0 in
      let post_stream ~sw:_ ~inputs =
        Int.incr calls;
        match !calls, finish with
        | 1, _ -> Fixtures.call_events [ "begin", "begin_work", `Object [] ]
        | 2, _ -> Stdlib.Seq.empty
        | 3, Complete_after_disconnect ->
          [%test_eq: int]
            1
            (List.count inputs ~f:(function
               | Openai.Responses.Item.Input_message
                   { role = User; content = Text { text; _ } :: _; _ } ->
                 String.is_prefix text ~prefix:"Ochat runtime notification."
               | _ -> false));
          Stdlib.Seq.empty
        | _ ->
          failwith "embedded background fixture requested an unexpected provider turn"
      in
      F.with_host
        ~durable
        ~sources:Background_shell_tests.sources
        ~daemon_options:
          { Agent_server.Daemon.default_options with
            qualify_chatml_extensions = true
          ; model_post_stream = Some post_stream
          }
        (fun env workspace embedded ->
           let file name = Eio.Path.(Eio.Stdenv.fs env / workspace / name) in
           let wait = Background_shell_tests.wait env in
           F.send embedded "Start the background shell.";
           wait (fun () ->
             !calls = 2
             && Option.is_none (F.snapshot embedded).session.active_operation
             && Eio.Path.is_file (file "fixture-work.started")
             && Eio.Path.is_file (file "fixture-work.pid"));
           let pid =
             Eio.Path.load (file "fixture-work.pid") |> String.strip |> Pid.of_string
           in
           let process_alive () =
             match
               Eio_unix.run_in_systhread (fun () ->
                 Signal_unix.send Signal.zero (`Pid pid))
             with
             | `Ok -> true
             | `No_such_process -> false
           in
           assert (process_alive ());
           let initial = F.snapshot embedded in
           let job_id =
             match F.initial_outcome initial "begin" with
             | Pending (Job id, _) -> id
             | _ -> failwith "embedded moderator did not publish Pending acknowledgement"
           in
           (match finish with
            | Close_host ->
              Embedded.close embedded;
              wait (fun () -> not (process_alive ()));
              [%test_eq: int] 2 !calls
            | Complete_after_disconnect ->
              let observer = Embedded.connect embedded in
              Exn.protect
                ~finally:(fun () -> Agent_client.Connection.close observer)
                ~f:(fun () ->
                  Agent_client.Session_handle.initialize
                    observer
                    ~implementation_name:"embedded-observer"
                    ~implementation_version:"test"
                  |> protocol_ok
                  |> ignore;
                  Agent_client.Connection.close (Embedded.connection embedded);
                  let snapshot () =
                    match
                      Agent_client.Connection.request
                        observer
                        (Session_get
                           { session_id = Embedded.session_id embedded; history = None })
                      |> protocol_ok
                    with
                    | Session_get value -> value
                    | _ -> failwith "unexpected observer response"
                  in
                  let running = snapshot () in
                  assert (process_alive ());
                  (match (List.hd_exn running.jobs).status with
                   | Running -> ()
                   | _ -> failwith "client disconnect stopped process-bound work");
                  Eio.Path.save
                    ~create:(`Exclusive 0o600)
                    (file "fixture-work.release")
                    "finish";
                  wait (fun () ->
                    let state = snapshot () in
                    !calls = 3
                    && Option.is_none state.session.active_operation
                    && List.for_all state.jobs ~f:(fun job ->
                      match job.P.Job.delivery with
                      | Delivered _ -> true
                      | _ -> false));
                  let completed = snapshot () in
                  [%test_eq: int] 1 (List.length completed.jobs);
                  let job = List.hd_exn completed.jobs in
                  assert (P.Id.Job.equal job_id job.id);
                  (match P.Job.terminal_completion job |> protocol_ok with
                   | Some (Succeeded _) -> ()
                   | _ -> failwith "embedded background shell failed");
                  [%test_eq: int]
                    1
                    (List.count completed.canonical_history.entries ~f:(fun entry ->
                       match entry.P.History.provenance with
                       | Runtime_notification _ -> true
                       | _ -> false));
                  wait (fun () -> not (process_alive ()));
                  [%test_eq: int] 3 !calls));
           [%test_eq: string] "started\n" (Eio.Path.load (file "fixture-work.started"));
           print_s
             [%sexp
               (durable : bool)
             , (finish : finish)
             , "one effect; process lifetime enforced"])));
  [%expect
    {|
    (false Complete_after_disconnect "one effect; process lifetime enforced")
    (false Close_host "one effect; process lifetime enforced")
    (true Complete_after_disconnect "one effect; process lifetime enforced")
    (true Close_host "one effect; process lifetime enforced")
    |}]
;;
