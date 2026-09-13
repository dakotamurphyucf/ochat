open Core
open Agent_server_test_support
module Q = Authoring_context_tests
module Service = Chat_response.Authoring_context
module V = Chat_response.Authoring_validation
module Flow = Authoring_compaction_tests

let configured host ~default_tokens ~max_tokens ~preload_tokens =
  V.context_budget ~default_tokens ~max_tokens ~preload_tokens
  |> Result.bind ~f:(V.configure_context_budget host)
  |> Result.ok_or_failwith
;;

let%expect_test "query services honor caller budgets and invalidate old continuations" =
  let service = Service.create ~secret:"budget-fixture-key" () |> Result.ok_or_failwith in
  let original = Q.host () in
  let host =
    configured original ~default_tokens:6000 ~max_tokens:8000 ~preload_tokens:16000
  in
  let capabilities = Q.capabilities () in
  let query host request =
    Service.query service ~host ~capabilities ~scope:"budget-session:1" request
  in
  let first =
    query
      host
      (Q.request ~task:"moderator_tool" ~topic_id:"runtime.jobs.shell-example" "topic")
  in
  assert (not (Q.has_error first));
  Q.require_json (`Number "6000") (Q.field (Q.field first "budget") "max_tokens");
  Q.require_json `False (Q.field first "complete");
  let cursor = Q.field first "next_cursor" |> Jsonaf.string_exn in
  let continue = Q.request ~cursor "continue" in
  assert (not (Q.has_error (query host continue)));
  let changed =
    configured original ~default_tokens:7000 ~max_tokens:9000 ~preload_tokens:16000
  in
  let preload_changed =
    configured original ~default_tokens:6000 ~max_tokens:8000 ~preload_tokens:17000
  in
  List.iter
    [ changed; preload_changed; V.for_delegated host ]
    ~f:(fun caller -> assert (Q.has_error (query caller continue)));
  assert (Q.has_error (query host (Q.request ~cursor ~max_tokens:8001 "continue")));
  let child =
    query
      (V.for_delegated host)
      (Q.request ~task:"one_off_script" ~topic_id:"chatml.syntax.calls" "topic")
  in
  assert (not (Q.has_error child));
  Q.require_json (`Number "6000") (Q.field (Q.field child "budget") "max_tokens");
  print_endline
    "same query service uses caller defaults/ceiling; host budget or target change \
     rejects cursor; delegated host retains budgets";
  [%expect
    {| same query service uses caller defaults/ceiling; host budget or target change rejects cursor; delegated host retains budgets |}]
;;

let%expect_test "daemon budgets reach native retrieval and trusted reference publication" =
  let config =
    String.substr_replace_first
      Authoring_package_config_tests.server_config
      ~pattern:"(authoring_packages (\"./reports.json\" \"./private.json\"))"
      ~with_:
        "(authoring_budget ((default_tokens 6000) (max_tokens 8000) (preload_tokens \
         16000)))"
  in
  let running = ref None in
  let seen = ref [] in
  Fixtures.with_daemon
    ~config_file:"server.sexp"
    ~sources:
      [ "server.sexp", config
      ; ( "agent.chatmd"
        , {|<developer>Read the authoring reference.</developer>
<tool name="ochat_authoring_context"/>|}
        )
      ]
    ~connect:(fun ~sw:_ ~env:_ ~root daemon ->
      running := Some (root, daemon);
      connection daemon (principal ()))
    ~calls:
      [ ( "first"
        , "ochat_authoring_context"
        , Q.request ~task:"moderator_tool" ~topic_id:"runtime.jobs.shell-example" "topic"
        )
      ]
    ~expected_requests:3
    ~request_counts:(fun () -> 3, 3)
    ~inspect_request:(fun _ inputs -> seen := inputs)
    ~followup_calls:(function
      | 2 ->
        let first = Flow.response !seen "first" in
        assert (not (Q.has_error first));
        Q.require_json (`Number "6000") (Q.field (Q.field first "budget") "max_tokens");
        let cursor = Q.field first "next_cursor" |> Jsonaf.string_exn in
        [ "next", "ochat_authoring_context", Q.request ~cursor "continue"
        ; "over", "ochat_authoring_context", Q.request ~cursor ~max_tokens:8001 "continue"
        ]
      | 3 ->
        assert (not (Q.has_error (Flow.response !seen "next")));
        assert (Q.has_error (Flow.response !seen "over"));
        []
      | _ -> failwith "unexpected budget fixture request")
    ~after_turn:(fun env _ _ ->
      let root, daemon = Option.value_exn !running in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / root / "server.sexp")
        (String.substr_replace_first
           config
           ~pattern:"default_tokens 6000"
           ~with_:"default_tokens 7000");
      match Agent_server.Daemon.reload_config daemon with
      | Error [ diagnostic ] ->
        [%test_eq: string] "config.restart_required" diagnostic.code
      | _ -> failwith "budget change should require restart")
    (fun state ->
       let module I = Agent_protocol.Invocation in
       List.iter [ "first"; "next" ] ~f:(fun call_id ->
         let invocation = Fixtures.model_invocation state call_id in
         assert (Option.is_some invocation.I.authoring_reference);
         let output =
           List.find_exn state.conversation.canonical_history ~f:(fun entry ->
             Agent_protocol.History.Id.equal
               entry.id
               (Option.value_exn invocation.output_entry_id))
         in
         match output.provenance with
         | Runtime_authoring guidance ->
           assert (
             Agent_protocol.Authoring_guidance.equal_purpose guidance.purpose Reference)
         | _ -> failwith "configured query fingerprint lost trusted publication");
       assert (Option.is_none (Fixtures.model_invocation state "over").authoring_reference);
       print_endline
         "config -> native paging/default/ceiling -> persisted reference provenance; \
          reload requires restart");
  [%expect
    {| config -> native paging/default/ceiling -> persisted reference provenance; reload requires restart |}]
;;
