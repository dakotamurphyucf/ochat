open Core
open Agent_server_test_support
module Host = Embedded_extension_tests
module E = Agent_server.Embedded
module P = Agent_protocol

let ledger_sources =
  [ ( "agent.chatmd"
    , [%blob "../../docs-src/examples/learning/stateful-workflow/agent.chatmd"] )
  ; ( "scripts/ledger.chatml"
    , [%blob "../../docs-src/examples/learning/stateful-workflow/scripts/ledger.chatml"] )
  ; ( "schemas/ledger-input.json"
    , [%blob
        "../../docs-src/examples/learning/stateful-workflow/schemas/ledger-input.json"] )
  ; ( "schemas/ledger-output.json"
    , [%blob
        "../../docs-src/examples/learning/stateful-workflow/schemas/ledger-output.json"] )
  ]
;;

let background_sources =
  [ ( "agent.chatmd"
    , [%blob "../../docs-src/examples/learning/background-results/agent.chatmd"] )
  ; ( "scripts/coordinator.chatml"
    , [%blob
        "../../docs-src/examples/learning/background-results/scripts/coordinator.chatml"]
    )
  ; ( "runtimes/checks.chatmd"
    , [%blob "../../docs-src/examples/learning/background-results/runtimes/checks.chatmd"]
    )
  ; ( "schemas/empty.json"
    , [%blob "../../docs-src/examples/learning/background-results/schemas/empty.json"] )
  ; ( "schemas/progress.json"
    , [%blob "../../docs-src/examples/learning/background-results/schemas/progress.json"]
    )
  ]
;;

let workspace_files =
  Documentation_shell_tests.workspace_files
  @ [ "sample-project/expected-report.json", Documentation_shell_tests.expected_report ]
;;

let send host key text =
  Host.request
    host
    (Session_send_message
       { session_id = E.session_id host
       ; attachment_id = (E.attachment host).id
       ; content = { kind = Plain_text; text; attachments = [] }
       ; idempotency_key = P.Idempotency_key.of_string key |> protocol_ok
       })
  |> ignore
;;

let complete = function
  | P.Invocation.Complete json -> json
  | outcome -> raise_s [%sexp (outcome : P.Invocation.outcome)]
;;

let finish_call env host call_id =
  try
    Background_shell_tests.wait env (fun () ->
      let snapshot = Host.snapshot host in
      List.iter snapshot.permissions ~f:(fun permission ->
        if
          P.Permission.equal_state permission.state Pending
          && not (String.is_prefix permission.tool_name ~prefix:"shell:")
        then
          Host.request
            host
            (Permission_respond
               { session_id = E.session_id host
               ; attachment_id = (E.attachment host).id
               ; permission_id = permission.id
               ; permission_generation = permission.generation
               ; choice = Approve_once
               ; reason =
                   Some
                     "Allow this declared workflow tool; shell decisions stay separate."
               ; idempotency_key =
                   P.Idempotency_key.of_string (P.Id.Permission.to_string permission.id)
                   |> protocol_ok
               })
          |> ignore);
      Option.is_none snapshot.session.active_operation
      && List.exists snapshot.canonical_history.entries ~f:(fun entry ->
        match
          Agent_session.History_codec.of_protocol entry
          |> protocol_ok
          |> History_entry.item
        with
        | Openai.Responses.Item.Function_call_output { call_id = actual; _ } ->
          String.equal actual call_id
        | _ -> false))
  with
  | Eio.Time.Timeout ->
    let snapshot = Host.snapshot host in
    raise_s
      [%sexp
        "workflow call did not finish"
      , (call_id : string)
      , (snapshot.session.observed_state : P.Session.observed_state)
      , (snapshot.session.active_operation : P.Operation.t option)
      , (List.map snapshot.permissions ~f:(fun p -> p.P.Permission.tool_name, p.state)
         : (string * P.Permission.state) list)
      , (List.map snapshot.jobs ~f:(fun j -> j.P.Job.status) : P.Job.status list)]
;;

let%expect_test
    "public review ledger retains and revises findings across turns without editing files"
  =
  let queued = ref [] in
  let post_stream ~sw:_ ~inputs:_ =
    let calls = !queued in
    queued := [];
    Fixtures.call_events calls
  in
  Host.with_host
    ~durable:false
    ~sources:ledger_sources
    ~workspace_files
    ~daemon_options:
      { Agent_server.Daemon.default_options with model_post_stream = Some post_stream }
    (fun env workspace host ->
       let call id action file note =
         queued
         := [ ( id
              , "review_ledger"
              , `Object [ "action", `String action; "file", file; "note", note ] )
            ];
         send host id "Update the retained review ledger.";
         finish_call env host id;
         Host.initial_outcome (Host.snapshot host) id
       in
       let record id file note =
         call id "record" (`String file) (`String note) |> complete
       in
       record "setup" "docs/setup.md" "Add verification instructions." |> ignore;
       record "reference" "docs/reference.md" "Explain the expected failing check."
       |> ignore;
       let updated =
         record "refine" "docs/setup.md" "Add an expected result and explain exit 1."
       in
       (match call "empty" "record" (`String "docs/setup.md") (`String "  ") with
        | Fail error -> [%test_eq: string] "ledger.invalid_request" error.code
        | outcome ->
          raise_s [%sexp "empty note accepted", (outcome : P.Invocation.outcome)]);
       let retained = call "summary" "summary" `Null `Null |> complete in
       assert (Jsonaf.exactly_equal updated retained);
       print_endline (Jsonaf.to_string retained);
       [%test_eq: string]
         Documentation_shell_tests.setup
         (Eio.Path.load
            Eio.Path.(Eio.Stdenv.fs env / workspace / "sample-project/docs/setup.md"));
       print_endline
         "five distinct turns; one note replaced; other note retained; rejected edit \
          leaves ledger and source unchanged");
  [%expect
    {|
    {"findings":[{"file":"docs/setup.md","note":"Add an expected result and explain exit 1."},{"file":"docs/reference.md","note":"Explain the expected failing check."}]}
    five distinct turns; one note replaced; other note retained; rejected edit leaves ledger and source unchanged
    |}]
;;

type background_case =
  | Check_failure
  | Missing_input
  | Cancel_running
[@@deriving sexp_of]

let%expect_test
    "public background checker separates acknowledgement, evidence and cancellation"
  =
  List.iter [ Check_failure; Missing_input; Cancel_running ] ~f:(fun scenario ->
    let queued = ref [ "begin", "begin_checks", `Object [] ] in
    let notified = ref false in
    let post_stream ~sw:_ ~inputs =
      notified
      := !notified
         || List.exists inputs ~f:(function
           | Openai.Responses.Item.Input_message
               { role = User; content = Text { text; _ } :: _; _ } ->
             String.is_prefix text ~prefix:"Ochat runtime notification."
           | _ -> false);
      let calls = !queued in
      queued := [];
      Fixtures.call_events calls
    in
    let sources = background_sources in
    let workspace_files =
      match scenario with
      | Check_failure -> workspace_files
      | Cancel_running ->
        (* Hold the sample checker behind a filesystem condition so cancellation
           does not race its tiny workload. The public coordinator, tool, schemas
           and required shell runtime are unchanged. No elapsed delay is success. *)
        List.map workspace_files ~f:(fun (name, source) ->
          ( name
          , if String.equal name "sample-project/scripts/check-docs.sh"
            then "while [ ! -f fixture.release ]; do sleep 0.01; done\n" ^ source
            else source ))
      | Missing_input ->
        List.filter workspace_files ~f:(fun (name, _) ->
          not (String.equal name "sample-project/docs/setup.md"))
    in
    Host.with_host
      ~durable:false
      ~sources
      ~workspace_files
      ~permission_profile:
        (E.interactive_permission_profile ~authorize_shell_manifest:true)
      ~daemon_options:
        { Agent_server.Daemon.default_options with model_post_stream = Some post_stream }
      (fun env _ host ->
         send
           host
           "begin"
           "Check the documentation and notify me when its evidence is ready.";
         finish_call env host "begin";
         let initial = Host.initial_outcome (Host.snapshot host) "begin" in
         let id =
           match initial with
           | Pending (Job id, acknowledgement) ->
             [%test_eq: string]
               "accepted"
               (Jsonaf.member_exn "status" acknowledgement |> Jsonaf.string_exn);
             id
           | outcome -> raise_s [%sexp (outcome : P.Invocation.outcome)]
         in
         (match scenario with
          | Check_failure | Missing_input -> ()
          | Cancel_running ->
            Background_shell_tests.wait env (fun () ->
              List.exists (Host.snapshot host).jobs ~f:(fun job ->
                P.Id.Job.equal job.id id
                &&
                match job.status with
                | Running -> true
                | _ -> false));
            queued := [ "progress", "check_progress", `Object [] ];
            send host "progress" "Show progress while the background check is running.";
            finish_call env host "progress";
            let status =
              Host.initial_outcome (Host.snapshot host) "progress" |> complete
            in
            [%test_eq: string]
              "running"
              (Jsonaf.member_exn "status" status |> Jsonaf.string_exn);
            queued := [ "busy", "begin_checks", `Object [] ];
            send host "busy" "Try starting another check before this one has finished.";
            finish_call env host "busy";
            (match Host.initial_outcome (Host.snapshot host) "busy" with
             | Fail error -> [%test_eq: string] "checks.busy" error.code
             | outcome -> raise_s [%sexp (outcome : P.Invocation.outcome)]);
            queued := [ "cancel", "cancel_checks", `Object [] ];
            send host "cancel" "Cancel that pending check.";
            finish_call env host "cancel";
            Host.initial_outcome (Host.snapshot host) "cancel" |> complete |> ignore);
         Background_shell_tests.wait env (fun () ->
           let snapshot = Host.snapshot host in
           !notified
           && Option.is_none snapshot.session.active_operation
           && List.exists snapshot.jobs ~f:(fun job ->
             P.Id.Job.equal id job.id
             &&
             match job.delivery with
             | Delivered _ -> true
             | _ -> false));
         let snapshot = Host.snapshot host in
         let job = List.find_exn snapshot.jobs ~f:(fun job -> P.Id.Job.equal id job.id) in
         (match scenario, P.Job.terminal_completion job |> protocol_ok with
          | (Check_failure | Missing_input), Some (Succeeded (`String text)) ->
            let result = Shell_runtime.Result.t_of_jsonaf (Jsonaf.of_string text) in
            (match scenario with
             | Check_failure ->
               assert (Shell_runtime.Result.equal_status result.status (Exited 1));
               assert (
                 Jsonaf.exactly_equal
                   (Jsonaf.of_string result.stdout)
                   (Jsonaf.of_string Documentation_shell_tests.expected_report))
             | Missing_input ->
               assert (Shell_runtime.Result.equal_status result.status (Exited 2));
               assert (
                 String.is_substring result.stderr ~substring:"Missing docs/setup.md")
             | Cancel_running -> assert false)
          | Cancel_running, Some (Cancelled _) -> ()
          | _, completion ->
            raise_s
              [%sexp (scenario : background_case), (completion : P.Completion.t option)]);
         let notifications =
           List.filter snapshot.canonical_history.entries ~f:(fun entry ->
             match entry.P.History.provenance with
             | Runtime_notification _ -> true
             | _ -> false)
         in
         [%test_eq: int] 1 (List.length notifications);
         assert (
           P.Invocation.equal_outcome (Host.initial_outcome snapshot "begin") initial);
         let history = snapshot.canonical_history.entries in
         let acknowledgement_index, _ =
           List.findi_exn history ~f:(fun _ entry ->
             match
               Agent_session.History_codec.of_protocol entry
               |> protocol_ok
               |> History_entry.item
             with
             | Openai.Responses.Item.Function_call_output { call_id = "begin"; _ } -> true
             | _ -> false)
         in
         let notification_index, _ =
           List.findi_exn history ~f:(fun _ entry ->
             match entry.P.History.provenance with
             | Runtime_notification _ -> true
             | _ -> false)
         in
         assert (acknowledgement_index < notification_index);
         print_s
           [%sexp
             (scenario : background_case)
           , "acknowledgement retained; one terminal notification; requested follow-up \
              observed"]));
  [%expect
    {|
    (Check_failure
     "acknowledgement retained; one terminal notification; requested follow-up observed")
    (Missing_input
     "acknowledgement retained; one terminal notification; requested follow-up observed")
    (Cancel_running
     "acknowledgement retained; one terminal notification; requested follow-up observed")
    |}]
;;
