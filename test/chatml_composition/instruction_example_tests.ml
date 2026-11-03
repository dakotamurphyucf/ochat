open! Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module Sources = Capability_sources

let source_ok result =
  Result.map_error result ~f:Sources.Error.message |> Result.ok_or_failwith
;;

let%expect_test
    "installed instruction helper and Lifecycle compose in a real native callback"
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
  with_daemon
    ~sources:(("server.sexp", configuration) :: sources)
    ~config_file:"server.sexp"
    ~connect:(fun ~sw:_ ~env:_ ~root:_ daemon ->
      Instruction_runtime_tests.authenticated_connection daemon)
    ~expect_moderator:true
    ~initial_requests:1
    ~expected_requests:1
    ~calls:[]
    ~after_turn:(fun _ _ entry ->
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
      assert (Option.is_none state.instruction_selection))
    (fun _ -> ());
  print_endline
    "installed ordinary helper + Lifecycle: real provider turn and native status; two \
     checkpointed components; no activation";
  [%expect
    {| installed ordinary helper + Lifecycle: real provider turn and native status; two checkpointed components; no activation |}]
;;
