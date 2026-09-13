open Core
open Agent_server_test_support
module P = Agent_protocol
module Res = Openai.Responses
module Host = Embedded_extension_tests
module E = Agent_server.Embedded
module Lab = Documentation_lab_tests
module Workflow = Documentation_workflow_tests
module Team = Documentation_agent_team_tests

let json_outcome outcome =
  match Workflow.complete outcome with
  | `String text -> Jsonaf.of_string text
  | value -> value
;;

let await_watch env host id =
  let delivered () =
    List.find_map (Host.snapshot host).canonical_history.entries ~f:(fun entry ->
      match entry.P.History.provenance with
      | Runtime_notification _ ->
        (match
           Agent_session.History_codec.of_protocol entry
           |> protocol_ok
           |> History_entry.item
         with
         | Res.Item.Input_message { content = [ Text { text; _ } ]; _ } ->
           let _, body = String.lsplit2_exn text ~on:'\n' in
           let data = Jsonaf.of_string body in
           if
             Jsonaf.exactly_equal
               (Lab.field data "work")
               (P.Invocation.work_to_json (Subscription id))
           then Some (P.Completion.of_json (Lab.field data "completion") |> protocol_ok)
           else None
         | _ -> failwith "unexpected runtime notification framing")
      | _ -> None)
  in
  Background_shell_tests.wait env (fun () ->
    Option.is_some (delivered ())
    && Option.is_none (Host.snapshot host).session.active_operation);
  Option.value_exn (delivered ())
;;

let pending_watch = function
  | P.Invocation.Pending (Subscription id, _) -> id
  | outcome -> raise_s [%sexp "expected pending watch", (outcome : P.Invocation.outcome)]
;;

let reviewer_definition =
  [ "version", `Number "1"
  ; "root_file", `String "reviewer.chatmd"
  ; ( "sources"
    , `Array
        [ `Object [ "path", `String "reviewer.chatmd"; "text", `String Lab.reviewer ] ] )
  ; "tools", `Array [ `String "read_file" ]
  ]
;;

let%expect_test
    "lab captures a narrowed reviewer and polls exact receipts without duplicate delivery"
  =
  let queued = ref [] in
  let release, release_resolver = Eio.Promise.create () in
  let released = ref false in
  let release_review () =
    if not !released
    then (
      released := true;
      Eio.Promise.resolve release_resolver ())
  in
  let serial = ref 0 in
  let followed = ref false in
  let provider ~sw:_ ~inputs =
    let transcript = `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) in
    let role =
      List.find_map inputs ~f:(function
        | Res.Item.Input_message message ->
          let json = Res.Item.jsonaf_of_t (Input_message message) in
          if String.equal (Lab.text json "role") "developer"
          then
            if Team.contains json "You are a documentation-lab reviewer."
            then Some "reviewer"
            else if Team.contains json "You are the documentation-lab writer."
            then Some "writer"
            else None
          else None
        | _ -> None)
    in
    match role with
    | None ->
      let calls = !queued in
      queued := [];
      Fixtures.call_events calls
    | Some role ->
      Int.incr serial;
      let id = sprintf "lab-response-%d" !serial in
      if Team.contains transcript "FAIL_REVIEW"
      then failwith "controlled lab reviewer failure"
      else if Team.contains transcript "FOLLOW_UP"
      then (
        assert (Team.contains transcript "Review evidence: missing verification guidance.");
        followed := true;
        Team.answer
          ~id
          "Follow-up preserves earlier diagnosis; proposed text remains unexecuted.")
      else if String.equal role "writer"
      then
        Team.answer
          ~id
          "## Verification\n\n\
           Expected result: the staged check passes its mechanical conventions."
      else if Team.contains transcript "function_call_output"
      then (
        assert (Team.contains transcript "sample intentionally omits verification");
        Eio.Promise.await release;
        Team.answer ~id "Review evidence: missing verification guidance.")
      else
        Fixtures.call_events
          [ ( id
            , "read_file"
            , `Object [ "root", `String "project"; "file", `String "tutorials/broken.md" ]
            )
          ]
  in
  Host.with_host
    ~durable:true
    ~sources:Lab.sources
    ~workspace_files:Lab.workspace_files
    ~permission_profile:
      { (E.interactive_permission_profile ~authorize_shell_manifest:true) with
        tool_default = Allow
      }
    ~daemon_options:
      { Agent_server.Daemon.default_options with model_post_stream = Some provider }
    (fun env _ host ->
       Exn.protect ~finally:release_review ~f:(fun () ->
         let invoke id name fields =
           queued := [ id, name, `Object fields ];
           Workflow.send host id "Perform the requested lab review operation.";
           Workflow.finish_call env host id;
           Host.initial_outcome (Host.snapshot host) id
         in
         let call id name fields = invoke id name fields |> json_outcome in
         let definition = reviewer_definition in
         let validated =
           call
             "validate"
             "ochat_validate"
             (("target", `String "generated_chatmd") :: definition)
         in
         assert (Jsonaf.bool_exn (Lab.field validated "valid"));
         let create invocation_id key =
           call
             invocation_id
             "agent_create"
             (definition
              @ [ "start_immediately", `True
                ; "lifetime", `String "owned"
                ; "idempotency_key", `String key
                ])
         in
         let created = create "create-reviewer" "lab-reviewer" in
         let session = Lab.field created "session_id" in
         let retry = create "retry-reviewer" "lab-reviewer" in
         assert (Jsonaf.exactly_equal session (Lab.field retry "session_id"));
         let send key session message =
           call
             key
             "agent_send"
             [ "session_id", session
             ; "message", `String message
             ; "idempotency_key", `String key
             ]
         in
         let receipt =
           send
             "investigate"
             session
             "Review tutorials/broken.md and explain the missing verification guidance."
         in
         let watch_fields role session receipt =
           [ "tutorial_id", `String "broken"
           ; "role", `String role
           ; "session_id", session
           ; "receipt_id", Lab.field receipt "receipt_id"
           ]
         in
         let query = watch_fields "reviewer" session receipt in
         let watched = invoke "watch" "watch_lab_review" query |> pending_watch in
         Background_shell_tests.wait env (fun () ->
           List.exists (Host.snapshot host).schedules ~f:(fun timer ->
             match timer.status with
             | Scheduled -> Team.contains timer.payload "lab_review_tick"
             | _ -> false));
         (match invoke "duplicate" "watch_lab_review" query with
          | Fail error -> [%test_eq: string] "lab.watch_unavailable" error.code
          | outcome -> raise_s [%sexp (outcome : P.Invocation.outcome)]);
         release_review ();
         (match await_watch env host watched with
          | Succeeded value ->
            [%test_eq: string]
              "completed"
              (Lab.text (Lab.field (Lab.field value "wait") "receipt") "status");
            assert (Team.contains value "Review evidence: missing verification guidance.")
          | result -> raise_s [%sexp (result : P.Completion.t)]);
         let next =
           send "follow-up" session "FOLLOW_UP: refine the same investigation."
         in
         assert (
           not
             (Jsonaf.exactly_equal
                (Lab.field receipt "receipt_id")
                (Lab.field next "receipt_id")));
         let follow =
           invoke
             "watch-follow-up"
             "watch_lab_review"
             (watch_fields "reviewer" session next)
           |> pending_watch
         in
         (match await_watch env host follow with
          | Succeeded value ->
            assert (Team.contains value "Follow-up preserves earlier diagnosis")
          | result -> raise_s [%sexp (result : P.Completion.t)]);
         assert !followed;
         let writer =
           call
             "writer"
             "propose_fix"
             [ ( "input"
               , `String "Propose a verification section using the review evidence." )
             ]
         in
         let writer_id = Lab.field writer "session_id" in
         let writer_watch =
           invoke
             "watch-writer"
             "watch_lab_review"
             (watch_fields "writer" writer_id (Lab.field writer "receipt"))
           |> pending_watch
         in
         (match await_watch env host writer_watch with
          | Succeeded value -> assert (Team.contains value "Expected result:")
          | result -> raise_s [%sexp (result : P.Completion.t)]);
         let failed = send "failed-review" session "FAIL_REVIEW" in
         let failure_watch =
           invoke
             "watch-failure"
             "watch_lab_review"
             (watch_fields "reviewer" session failed)
           |> pending_watch
         in
         (match await_watch env host failure_watch with
          | Succeeded value ->
            [%test_eq: string]
              "failed"
              (Lab.text (Lab.field (Lab.field value "wait") "receipt") "status")
          | result -> raise_s [%sexp (result : P.Completion.t)]);
         let report = call "report" "lab_report" [] in
         [%test_eq: int] 4 (Lab.field report "reviews" |> Jsonaf.list_exn |> List.length);
         let notifications =
           List.count (Host.snapshot host).canonical_history.entries ~f:(fun entry ->
             match entry.P.History.provenance with
             | Runtime_notification _ -> true
             | _ -> false)
         in
         [%test_eq: int] 4 notifications;
         let closed = call "close" "close_lab" [] in
         assert (Jsonaf.bool_exn (Lab.field closed "closed"));
         assert (Team.contains closed "stop");
         print_endline
           "captured reviewer validated and created with only inherited read_file; \
            identical creation retry reuses identity";
         print_endline
           "pending probe arms a timer; duplicate receipt watch rejects; four receipts \
            produce four notifications";
         print_endline
           "same-session follow-up, authored writer proposal and failed child receipt \
            remain distinct; close retains stop evidence"));
  [%expect
    {|
    captured reviewer validated and created with only inherited read_file; identical creation retry reuses identity
    pending probe arms a timer; duplicate receipt watch rejects; four receipts produce four notifications
    same-session follow-up, authored writer proposal and failed child receipt remain distinct; close retains stop evidence
    |}]
;;

let%expect_test "closing the lab cancels an outstanding reviewer and its response watch" =
  let queued = ref [] in
  let started = ref false in
  let retired = ref false in
  let release, release_resolver = Eio.Promise.create () in
  let provider ~sw:_ ~inputs =
    let child =
      List.exists inputs ~f:(function
        | Res.Item.Input_message message ->
          let json = Res.Item.jsonaf_of_t (Input_message message) in
          String.equal (Lab.text json "role") "developer"
          && Team.contains json "You are a documentation-lab reviewer."
        | _ -> false)
    in
    match child with
    | true ->
      started := true;
      Exn.protect
        ~finally:(fun () -> retired := true)
        ~f:(fun () ->
          Eio.Promise.await release;
          Stdlib.Seq.empty)
    | false ->
      let calls = !queued in
      queued := [];
      Fixtures.call_events calls
  in
  Host.with_host
    ~durable:true
    ~sources:Lab.sources
    ~workspace_files:Lab.workspace_files
    ~daemon_options:
      { Agent_server.Daemon.default_options with model_post_stream = Some provider }
    (fun env _ host ->
       Exn.protect
         ~finally:(fun () -> ignore (Eio.Promise.try_resolve release_resolver ()))
         ~f:(fun () ->
           let invoke id name fields =
             queued := [ id, name, `Object fields ];
             Workflow.send host id "Perform the requested cancellation scenario.";
             Workflow.finish_call env host id;
             Host.initial_outcome (Host.snapshot host) id
           in
           let call id name fields = invoke id name fields |> json_outcome in
           let created =
             call
               "create"
               "agent_create"
               (reviewer_definition
                @ [ "start_immediately", `True
                  ; "lifetime", `String "owned"
                  ; "idempotency_key", `String "cancelled-lab-reviewer"
                  ])
           in
           let session = Lab.field created "session_id" in
           let receipt =
             call
               "send"
               "agent_send"
               [ "session_id", session
               ; "message", `String "Review the broken tutorial."
               ; "idempotency_key", `String "cancelled-review"
               ]
           in
           Background_shell_tests.wait env (fun () -> !started);
           let watch =
             invoke
               "watch"
               "watch_lab_review"
               [ "tutorial_id", `String "broken"
               ; "role", `String "reviewer"
               ; "session_id", session
               ; "receipt_id", Lab.field receipt "receipt_id"
               ]
             |> pending_watch
           in
           Background_shell_tests.wait env (fun () ->
             List.exists (Host.snapshot host).schedules ~f:(fun timer ->
               match timer.status with
               | Scheduled -> Team.contains timer.payload "lab_review_tick"
               | _ -> false));
           let closed = call "close" "close_lab" [] in
           assert (Jsonaf.bool_exn (Lab.field closed "closed"));
           let review = Lab.field closed "reviews" |> Jsonaf.list_exn |> List.hd_exn in
           [%test_eq: string] "cancelled" (Lab.text (Lab.field review "result") "type");
           (match await_watch env host watch with
            | Cancelled "Lab closed." -> ()
            | result -> raise_s [%sexp (result : P.Completion.t)]);
           Background_shell_tests.wait env (fun () -> !retired);
           let snapshot = Host.snapshot host in
           assert (
             List.for_all snapshot.schedules ~f:(fun timer ->
               match timer.status with
               | Scheduled | Delivering -> false
               | Delivered | Cancelled | Failed _ -> true));
           let observed = call "status" "agent_status" [ "session_id", session ] in
           [%test_eq: string] "stopped" (Lab.text observed "state");
           let closed_again = call "close-again" "close_lab" [] in
           assert (Jsonaf.bool_exn (Lab.field closed_again "closed"));
           let notifications =
             List.count (Host.snapshot host).canonical_history.entries ~f:(fun entry ->
               match entry.P.History.provenance with
               | Runtime_notification _ -> true
               | _ -> false)
           in
           [%test_eq: int] 1 notifications;
           print_endline
             "pending reviewer cancelled; timer retired; one correlated cancellation \
              retained after repeated close"));
  [%expect
    {| pending reviewer cancelled; timer retired; one correlated cancellation retained after repeated close |}]
;;
