open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module A = Managed_completion_command

let with_application f =
  Eio_main.run (fun actual_env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root actual_env in
    let directory = Eio.Path.(Eio.Stdenv.fs actual_env / root) in
    Exn.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory))
      ~f:(fun () ->
        Eio.Switch.run (fun output_sw ->
          let capture_output () =
            let source, sink = Eio_unix.pipe output_sw in
            let bytes = Buffer.create 1024 in
            let joined, finish = Eio.Promise.create () in
            Eio.Fiber.fork ~sw:output_sw (fun () ->
              Exn.protect
                ~finally:(fun () -> Eio.Promise.resolve finish ())
                ~f:(fun () -> Eio.Flow.copy source (Eio.Flow.buffer_sink bytes)));
            let closed = ref false in
            let drain () =
              if not !closed
              then (
                closed := true;
                Eio.Flow.close sink);
              Eio.Promise.await joined;
              Buffer.contents bytes
            in
            sink, drain
          in
          let stdout, take_stdout = capture_output () in
          let stderr, take_stderr = capture_output () in
          let env =
            object
              method fs = actual_env#fs
              method cwd = directory
              method stdin = actual_env#stdin
              method stdout = stdout
              method stderr = stderr
              method net = actual_env#net
              method domain_mgr = actual_env#domain_mgr
              method process_mgr = actual_env#process_mgr
              method clock = actual_env#clock
              method mono_clock = actual_env#mono_clock
              method secure_random = actual_env#secure_random
              method debug = actual_env#debug
              method backend_id = actual_env#backend_id
            end
          in
          let requests = ref 0 in
          let post_stream ~sw:_ ~inputs:_ =
            Int.incr requests;
            Stdlib.Seq.empty
          in
          let fixture =
            Inference_fixture.create
              ~namespace:root
              ~default_model:"fixture-model"
              ~post_stream
          in
          let ctx =
            Inference_fixture.ctx
              fixture
              ~env
              ~dir:directory
              ~tool_dir:directory
              ~cache:(Chat_response.Cache.create ~max_size:1 ())
              ()
          in
          let daemon_options =
            { Agent_server.Daemon.default_options with
              inference_policy =
                inference_policy ~default_model:"fixture-model" ~post_stream
            }
          in
          let save file source =
            Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(directory / file) source
          in
          let definition = Filename.concat root "definition.chatmd" in
          let storage =
            Agent_server.Local_storage.Root.create ~path:(Filename.concat root "store") ()
            |> protocol_ok
            |> fun root -> Agent_server.Local_storage.Durable root
          in
          let run
                ?session_id
                ?run_id
                ?import_transcript
                ?submission
                ?project_sources
                ?(rebuild = false)
                mode
                output
            =
            let%bind.Result options =
              A.Options.create
                ~definition_file:definition
                ~output_file:
                  (if String.equal output "/dev/stdout"
                   then output
                   else Filename.concat root output)
                ~storage
                ~session_id
                ~run_id
                ~rebuild
                ~import_transcript
                ~submission
                ~mode
            in
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
              A.run options ~env ~daemon_options ~project_sources ~transcript_context:ctx)
          in
          let stop session_id =
            let options : Agent_server.Embedded.options =
              { prompt_file = definition
              ; workspace = root
              ; tool_dir = root
              ; home = Some root
              ; storage
              ; start_immediately = false
              ; permission_profile = Agent_server.Embedded.default_permission_profile
              ; attachment_mode = Read_write
              ; event_capacity = 256
              }
            in
            Eio.Switch.run (fun sw ->
              Agent_server.Embedded.with_local_host
                ~sw
                ~env
                ~daemon_options
                ~root_invocation:Headless
                options
                ~f:(fun host ->
                  let connection = Agent_server.Embedded.host_connection host in
                  let%bind.Result snapshot = C.Admin.get_session connection session_id in
                  let lifecycle =
                    (P.Public.Snapshot.fields snapshot).lifecycle |> Option.value_exn
                  in
                  let%bind.Result () =
                    Agent_server.Embedded.select_session
                      host
                      (P.Session_lifecycle.Observation.expected lifecycle)
                  in
                  Eio.Switch.run (fun handle_sw ->
                    let%bind.Result handle =
                      C.Session_handle.attach
                        ~sw:handle_sw
                        ~clock:(Eio.Stdenv.clock env)
                        ~connection
                        ~session_id
                        ~mode:Read_write
                        ~subscribe:false
                        ()
                    in
                    Exn.protect
                      ~finally:(fun () ->
                        Eio.Cancel.protect (fun () -> C.Session_handle.close handle))
                      ~f:(fun () ->
                        let%map.Result session =
                          C.Session_handle.stop handle ~mode:Graceful
                        in
                        assert (
                          P.Session.equal_desired_state session.desired_state Stopped);
                        match session.observed_state with
                        | Stopped -> ()
                        | _ -> assert false)))
              |> protocol_ok)
          in
          Exn.protect
            ~finally:(fun () ->
              Eio.Cancel.protect (fun () ->
                ignore (take_stdout () : string);
                ignore (take_stderr () : string)))
            ~f:(fun () ->
              f ~env ~root ~save ~run ~stop ~requests ~take_stdout ~take_stderr))))
;;

let selected completed =
  (C.Canonical_export.snapshot completed.Managed_completion_cli.Completed.export
   |> P.Public.Snapshot.fields)
    .session
;;

let%expect_test
    "actual executable owner admits both modes and exports exact canonical bytes"
  =
  with_application
    (fun ~env ~root ~save ~run ~stop:_ ~requests ~take_stdout ~take_stderr ->
       save "definition.chatmd" Completion_tests.finished_prompt;
       List.iteri [ P.Run.Mode.Single_turn; Workflow ] ~f:(fun index mode ->
         let output = sprintf "transcript-%d.chatmd" index in
         let completed = run mode output |> protocol_ok in
         [%test_eq: string]
           (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / output))
           completed.chatmd;
         [%test_eq: string]
           (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / "definition.chatmd"))
           Completion_tests.finished_prompt;
         match completed.evidence with
         | Managed_run { terminal; _ } -> assert (P.Run.Mode.equal terminal.run.mode mode)
         | Foreground _ -> assert false);
       let streamed = run P.Run.Mode.Single_turn "/dev/stdout" |> protocol_ok in
       [%test_eq: string] (take_stdout ()) streamed.chatmd;
       let observations = take_stderr () in
       assert (String.is_substring observations ~substring:"[managed conversation ");
       assert (String.is_substring observations ~substring:"[managed run ");
       assert (
         String.is_substring observations ~substring:"whole-session live observation");
       assert (
         not
           (String.is_substring
              streamed.chatmd
              ~substring:"whole-session live observation"));
       [%test_eq: int] !requests 0);
  print_endline
    "Both authored modes use real managed runs; canonical export preserves definition.";
  [%expect
    {| Both authored modes use real managed runs; canonical export preserves definition. |}]
;;

let%test_unit
    "edited transcript is a new identity and retained original run survives source \
     removal"
  =
  with_application
    (fun ~env ~root ~save ~run ~stop ~requests ~take_stdout:_ ~take_stderr:_ ->
       save "definition.chatmd" Completion_tests.finished_prompt;
       save "edited.chatmd" "<user>explicit edited transcript</user>";
       let imported =
         run
           ~import_transcript:(Filename.concat root "edited.chatmd")
           Workflow
           "imported.chatmd"
         |> protocol_ok
       in
       let session = selected imported in
       let admission =
         match imported.evidence with
         | Managed_run { admission; _ } -> admission
         | Foreground _ -> assert false
       in
       assert (String.is_substring imported.chatmd ~substring:"explicit edited transcript");
       Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / root / "definition.chatmd");
       let observed =
         run ~session_id:session.id ~run_id:admission.run_id Workflow "retained.chatmd"
         |> protocol_ok
       in
       [%test_eq: string] imported.chatmd observed.chatmd;
       assert (P.Id.Session.equal (selected observed).id session.id);
       (match observed.evidence with
        | Managed_run actual -> assert (P.Run_receipt.equal actual.admission admission)
        | Foreground _ -> assert false);
       [%test_eq: int] !requests 0;
       let rejected =
         run
           ~session_id:session.id
           ~run_id:admission.run_id
           Workflow
           "./definition.chatmd"
       in
       assert (Result.is_error rejected);
       assert (
         not (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / root / "definition.chatmd")));
       stop session.id;
       let submitted =
         run
           ~session_id:session.id
           ~submission:"Actual retained new input."
           Single_turn
           "submitted.chatmd"
         |> protocol_ok
       in
       assert (P.Id.Session.equal (selected submitted).id session.id);
       assert (
         String.is_substring submitted.chatmd ~substring:"Actual retained new input.");
       [%test_eq: int] !requests 1;
       save
         "definition.chatmd"
         (String.substr_replace_all
            Completion_tests.finished_prompt
            ~pattern:"Managed completion definition."
            ~with_:"Explicitly rebuilt definition.");
       stop session.id;
       let rebuilt =
         run ~session_id:session.id ~rebuild:true Workflow "rebuilt.chatmd" |> protocol_ok
       in
       let next = selected rebuilt in
       assert (P.Id.Session.equal next.id session.id);
       assert (next.generation > session.generation);
       assert (
         not
           (Option.equal
              P.Id.Prompt_revision.equal
              next.prompt_revision
              session.prompt_revision));
       assert (
         String.is_substring rebuilt.chatmd ~substring:"Explicitly rebuilt definition.");
       [%test_eq: int] !requests 1)
;;

let%test_unit "ordinary Workflow rejects without provider effects or output replacement" =
  with_application
    (fun ~env ~root ~save ~run ~stop:_ ~requests ~take_stdout:_ ~take_stderr:_ ->
       save "definition.chatmd" "<user>ordinary authored input</user>";
       save "output.chatmd" "existing target";
       (match run Workflow "output.chatmd" with
        | Error { code = Invalid_state; _ } -> ()
        | Ok _ | Error _ -> assert false);
       [%test_eq: int] !requests 0;
       [%test_eq: string]
         (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / "output.chatmd"))
         "existing target";
       let completed = run Single_turn "ordinary.chatmd" |> protocol_ok in
       (match completed.evidence with
        | Foreground _ -> ()
        | Managed_run _ -> assert false);
       [%test_eq: int] !requests 1;
       save
         "definition.chatmd"
         {|<developer>Failed workflow canonical evidence.</developer>
<script id="failure" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event ctx state event = Task.fail("actual application callback failure")
</script>|};
       (match run Workflow "failed.chatmd" with
        | Error { code = Internal_error; _ } -> ()
        | Ok _ | Error _ -> assert false);
       assert (
         String.is_substring
           (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / "failed.chatmd"))
           ~substring:"Failed workflow canonical evidence.");
       [%test_eq: int] !requests 1)
;;

let%expect_test "configured external entry uses its selected environment and companion" =
  with_application
    (fun ~env ~root ~save ~run ~stop:_ ~requests ~take_stdout:_ ~take_stderr:_ ->
       let project_root = root ^ "-project" in
       let directory = Eio.Path.(Eio.Stdenv.fs env / project_root) in
       Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(directory / "lib");
       Exn.protect
         ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory))
         ~f:(fun () ->
           let save_external member contents =
             Eio.Path.save
               ~create:(`Or_truncate 0o600)
               Eio.Path.(directory / member)
               contents
           in
           let route = "../" ^ Filename.basename project_root in
           save
             "main.chatml"
             {|let initial_state = [Counter.seed]
let on_event = fun ctx state event -> match event with
| `Session_start -> Task.bind(Run.finish(`Object([
  { key = "kind"; value = `String("finish") },
  { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
  { key = "relinquish"; value = `Array([]) }])), fun ignored -> Task.pure(state))
| _ -> Task.pure(state)|};
           save_external "lib/original.chatml" "let seed = 40 let hidden = false";
           save_external "lib/original.chatmli" "val seed : int";
           save_external
             "chatml.project.json"
             {|{"schema_version":1,"namespace":"cli.project","source_dirs":["lib"],"environments":{"empty":{"modules":{}},"selected":{"modules":{"Counter":"self.Original"}}},"default_environment":"empty"}|};
           save
             "definition.chatmd"
             (sprintf
                {|<developer>Configured executable project.</developer>
<script id="complete" language="chatml" kind="moderator" api="extensibility-v1" src="main.chatml" project="%s/chatml.project.json" environment="selected"/>|}
                route);
           save "configured.chatmd" "existing output";
           (match run Workflow "configured.chatmd" with
            | Error _ -> ()
            | Ok _ -> failwith "external source without an explicit grant was admitted");
           [%test_eq: int] !requests 0;
           [%test_eq: string]
             (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / "configured.chatmd"))
             "existing output";
           let project_sources : Agent_server.Project_source_configuration.t =
             { external_roots = [ project_root ]; git = None }
           in
           let completed =
             run ~project_sources Workflow "configured.chatmd" |> protocol_ok
           in
           assert (
             String.is_substring
               completed.chatmd
               ~substring:"Configured executable project.");
           (match completed.evidence with
            | Managed_run { terminal; _ } ->
              assert (P.Run.Mode.equal terminal.run.mode Workflow)
            | Foreground _ -> assert false);
           [%test_eq: int] !requests 0));
  print_endline
    "External source requires its real grant; selected module and companion compile.";
  [%expect
    {| External source requires its real grant; selected module and companion compile. |}]
;;
