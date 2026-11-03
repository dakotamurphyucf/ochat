open! Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module Sources = Capability_sources
module Recovery = Instruction_example_recovery

let source_ok result =
  Result.map_error result ~f:Sources.Error.message |> Result.ok_or_failwith
;;

let%expect_test
    "installed instruction helper and Lifecycle resume through context and failure"
  =
  let inventory =
    Sources.installed ~limits:Sources.Limits.default ~checkpoint:ignore |> source_ok
  in
  let resource =
    Sources.find inventory (Sources.Id.of_string "instruction-selection" |> source_ok)
    |> source_ok
  in
  let sources =
    List.map (Sources.Resource.members resource) ~f:(fun member ->
      ( Source.Portable_path.to_string (Sources.Member.path member)
      , Sources.Member.contents member ))
  in
  let configuration =
    Instruction_runtime_tests.configuration
    |> String.substr_replace_all
         ~pattern:"(external_roots (\"./workspace\"))"
         ~with_:"(external_roots (\"./workspace\" \"./examples/libraries\"))"
    |> String.substr_replace_all
         ~pattern:"(path \"./agent.chatmd\")"
         ~with_:"(path \"./examples/learning/instruction-selection/agent.chatmd\")"
  in
  let saved = ref None in
  with_daemon
    ~sources:(("server.sexp", configuration) :: sources)
    ~config_file:"server.sexp"
    ~connect:(fun ~sw:_ ~env:_ ~root:_ daemon ->
      Instruction_runtime_tests.authenticated_connection daemon)
    ~expect_moderator:true
    ~initial_requests:1
    ~expected_requests:1
    ~calls:[]
    ~inspect_request:(fun _ inputs ->
      assert (
        String.is_substring
          (Instruction_runtime_tests.wire_text inputs)
          ~substring:"# Composed lifecycle decisions"))
    ~after_turn:(fun env handle entry ->
      let state =
        Agent_session.Session_actor.state entry.Agent_server.Session_registry.actor
        |> protocol_ok
      in
      let checkpoint =
        Agent_session.Moderator_checkpoint.decode state.moderator
        |> protocol_ok
        |> Option.value_exn
      in
      let record = function
        | Session.Snapshot.Record fields -> fields
        | value ->
          raise_s [%sexp "expected author component state", (value : Session.Snapshot.t)]
      in
      let fields = record checkpoint.current_state in
      let component name field =
        record (List.Assoc.find_exn fields name ~equal:String.equal)
        |> fun fields -> List.Assoc.find_exn fields field ~equal:String.equal
      in
      [%test_eq: Session.Snapshot.t] (Bool true) (component "instructions" "inspected");
      [%test_eq: Session.Snapshot.t] (Bool true) (component "review" "seen");
      let status =
        component "instructions" "last_status"
        |> Session.Snapshot.to_value
        |> Result.bind ~f:(fun value -> Chatml.Chatml_value_codec.export_json value)
        |> Result.ok_or_failwith
        |> P.Instruction_status.of_json
        |> protocol_ok
      in
      [%test_eq: int64] 0L status.revision;
      [%test_eq: int] 0 status.skills;
      [%test_eq: int] 0 status.project_files;
      assert (not status.project_selected);
      [%test_eq: Session.Snapshot.t]
        (Array [ String "instructions"; String "review" ])
        (List.Assoc.find_exn fields "order" ~equal:String.equal);
      let status_calls =
        List.filter state.invocations ~f:(fun invocation ->
          String.equal invocation.P.Invocation.context.tool_name "instructions_status")
      in
      [%test_eq: int] 1 (List.length status_calls);
      assert (Option.is_none state.instruction_selection);
      assert (
        List.exists state.moderator_executions ~f:(fun receipt ->
          match receipt.P.Moderator_execution.context.phase, receipt.status with
          | Session_start, Completed _ -> true
          | _ -> false));
      H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
      unload_idle_runtime env entry.runtime;
      assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime)))
    ~after_turn_with_daemon:(fun _ daemon entry ->
      saved := Some (Recovery.capture daemon (A.state entry.actor |> protocol_ok)))
    ~after_shutdown:(fun ~sw ~env ~root ~configuration ->
      Recovery.exercise (Option.value_exn !saved) ~sources ~sw ~env ~root ~configuration)
    (fun _ -> ());
  print_endline
    "installed helper + Lifecycle: native status and builtin guidance; deleted-source \
     physical resume; real compaction; provider failure preserves components; fresh turn \
     recovers";
  [%expect
    {| installed helper + Lifecycle: native status and builtin guidance; deleted-source physical resume; real compaction; provider failure preserves components; fresh turn recovers |}]
;;
