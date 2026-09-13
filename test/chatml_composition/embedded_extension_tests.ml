open Core
open Agent_server_test_support
module P = Agent_protocol
module Embedded = Agent_server.Embedded

let request embedded command =
  Agent_client.Connection.request (Embedded.connection embedded) command |> protocol_ok
;;

let snapshot embedded =
  match
    request
      embedded
      (Session_get { session_id = Embedded.session_id embedded; history = None })
  with
  | Session_get snapshot -> snapshot
  | _ -> failwith "embedded session.get returned another method"
;;

let send embedded text =
  request
    embedded
    (Session_send_message
       { session_id = Embedded.session_id embedded
       ; attachment_id = (Embedded.attachment embedded).id
       ; content = { kind = Plain_text; text; attachments = [] }
       ; idempotency_key = P.Idempotency_key.of_string "embedded:send" |> protocol_ok
       })
  |> ignore
;;

let with_host ?(package_files = []) ?authoring_budget ~durable ~sources ~daemon_options f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdirs
          ~exists_ok:true
          ~perm:0o700
          Eio.Path.(Eio.Stdenv.fs env / workspace / "reports");
        let save path source =
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / path)
            source
        in
        List.iter sources ~f:(fun (name, source) ->
          save (Filename.concat root name) source);
        save (Filename.concat workspace "reports/report-a.json") Fixtures.report_a;
        save (Filename.concat workspace "reports/report-b.json") Fixtures.report_b;
        let options : Embedded.options =
          { prompt_file = Filename.concat root "agent.chatmd"
          ; workspace
          ; tool_dir = root
          ; home = root
          ; data_root = Option.some_if durable (Filename.concat root "data")
          ; start_immediately = true
          ; permission_profile =
              { Embedded.default_permission_profile with
                tool_default = Allow
              ; manifest_authorization = Assume_authorized
              }
          ; attachment_mode = Read_write
          ; event_capacity = 512
          }
        in
        Eio.Switch.run (fun sw ->
          let authoring_package_files =
            List.map package_files ~f:(Filename.concat root)
          in
          let embedded =
            Embedded.start
              ~sw
              ~env
              ~daemon_options
              ~authoring_package_files
              ?authoring_budget
              options
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> Embedded.close embedded)
            ~f:(fun () -> f env workspace embedded))))
;;

let initial_outcome (snapshot : P.Snapshot.t) call_id =
  List.find_map_exn snapshot.canonical_history.entries ~f:(fun entry ->
    match
      Agent_session.History_codec.of_protocol entry |> protocol_ok |> History_entry.item
    with
    | Openai.Responses.Item.Function_call_output
        { call_id = actual; output = Text text; _ }
      when String.equal actual call_id ->
      Some (P.Invocation.outcome_of_json (Jsonaf.of_string text) |> protocol_ok)
    | _ -> None)
;;

type recipe =
  | One_off
  | Standalone
  | Background
[@@deriving sexp_of]

let%expect_test
    "embedded transient and durable hosts execute compiled X01 and X04 through shared \
     runtime"
  =
  List.iter [ false; true ] ~f:(fun durable ->
    List.iter [ One_off; Standalone; Background ] ~f:(fun recipe ->
      let sources, name, input =
        match recipe with
        | One_off ->
          ( [ "agent.chatmd", One_off_tests.agent ]
          , "run_chatml"
          , One_off_tests.request [ "report-a.json"; "report-b.json" ] )
        | Standalone ->
          ( Standalone_tests.sources ~bad_output:false
          , "compare_reports"
          , Standalone_tests.input "report-a.json" "report-b.json" )
        | Background ->
          ( Standalone_pending_tests.sources
          , "compare_reports_async"
          , Standalone_tests.input "report-a.json" "report-b.json" )
      in
      let requests = ref 0 in
      let post_stream ~sw:_ ~inputs:_ =
        Int.incr requests;
        match !requests with
        | 1 -> Fixtures.call_events [ "work", name, input ]
        | 2 -> Stdlib.Seq.empty
        | _ -> failwith "embedded script unexpectedly called another model"
      in
      let daemon_options =
        { Agent_server.Daemon.default_options with
          model_post_stream = Some post_stream
        ; chatml_runtime_policy =
            { Chat_response.Runtime_semantics.default_policy with
              honor_request_turn = false
            }
        }
      in
      with_host ~durable ~sources ~daemon_options (fun env _ embedded ->
        let probe = Embedded.connect embedded in
        let initialized =
          Agent_client.Session_handle.initialize
            probe
            ~implementation_name:"embedded-extensions"
            ~implementation_version:"test"
          |> protocol_ok
        in
        let metadata = Option.value_exn initialized.extensions in
        assert (
          P.Extension_capabilities.equal_host
            metadata.host
            (if durable then Embedded_durable else Embedded_transient));
        assert (P.Extension_capabilities.equal_journal_flush metadata.journal_flush Synced);
        let expected =
          List.filter P.Extension_capabilities.known_features ~f:(fun name ->
            durable || not (String.equal name "agent.delegation.v1"))
          |> List.sort ~compare:String.compare
        in
        [%test_eq: string list] expected metadata.available_features;
        Agent_client.Connection.close probe;
        send embedded "Run the report workflow.";
        Background_shell_tests.wait env (fun () ->
          let current = snapshot embedded in
          !requests = 2
          && Option.is_none current.session.active_operation
          && List.for_all current.jobs ~f:(fun job ->
            match job.P.Job.delivery with
            | Delivered _ -> true
            | _ -> false));
        let current = snapshot embedded in
        assert (
          P.Session.equal_execution_host current.session.spec.execution_host Embedded);
        assert (P.Session.equal_liveness current.session.spec.liveness Process_bound);
        let value =
          match recipe, initial_outcome current "work" with
          | (One_off | Standalone), Complete value ->
            assert (List.is_empty current.jobs);
            value
          | Background, Pending (Job id, acknowledgement) ->
            [%test_eq: int] 1 (List.length current.jobs);
            let job = List.hd_exn current.jobs in
            assert (P.Id.Job.equal id job.id);
            [%test_eq: string]
              (P.Id.Job.to_string id)
              (Jsonaf.member_exn "job_id" acknowledgement |> Jsonaf.string_exn);
            [%test_eq: int]
              1
              (List.count current.canonical_history.entries ~f:(fun entry ->
                 match entry.P.History.provenance with
                 | Runtime_notification _ -> true
                 | _ -> false));
            (match P.Job.terminal_completion job |> protocol_ok with
             | Some (Succeeded value) -> value
             | _ -> failwith "embedded job failed")
          | _, outcome -> raise_s [%sexp (outcome : P.Invocation.outcome)]
        in
        (match recipe with
         | One_off ->
           let expected =
             [%blob "../chatml_extensibility_fixtures/x01-report/expected.json"]
             |> Jsonaf.of_string
           in
           assert (Jsonaf.exactly_equal expected value)
         | Standalone | Background ->
           [%test_eq: float]
             1.
             (Jsonaf.member_exn "invocation_count" value |> Jsonaf.float_exn);
           List.iter
             [ "left", Fixtures.report_a; "right", Fixtures.report_b ]
             ~f:(fun (field, expected) ->
               assert (
                 String.is_substring
                   (Jsonaf.member_exn field value |> Jsonaf.string_exn)
                   ~substring:(String.strip expected))));
        [%test_eq: int] 2 !requests;
        print_s
          [%sexp
            (durable : bool)
          , (recipe : recipe)
          , "compiled tools; process-bound; no extra provider"])));
  [%expect
    {|
    (false One_off "compiled tools; process-bound; no extra provider")
    (false Standalone "compiled tools; process-bound; no extra provider")
    (false Background "compiled tools; process-bound; no extra provider")
    (true One_off "compiled tools; process-bound; no extra provider")
    (true Standalone "compiled tools; process-bound; no extra provider")
    (true Background "compiled tools; process-bound; no extra provider")
    |}]
;;
