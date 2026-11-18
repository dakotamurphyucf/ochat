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

module Skill_recurrence = struct
  module Session = Agent_session
  module Native = Instruction_runtime_tests
  module Wire = Recurrence_runtime_tests
  module Checked = Recurrence_execution_tests
  module Selection = Session.Instruction_selection

  let recurrence_id = P.Id.Recurrence.of_string "rec_skill_review" |> protocol_ok
  let skill_member = "review/SKILL.md"
  let original_skill = Native.skill "review"

  let refreshed_skill =
    String.substr_replace_all original_skill ~pattern:"ORIGINAL" ~with_:"REFRESHED"
  ;;

  let sources =
    [ ( "agent.chatmd"
      , Native.agent
        ^ {|
<developer><doc src="builtin:recurrence"/></developer>
<tool name="recurrence_list"/>
<tool name="recurrence_create"/>
<tool name="weekly_worker" agent="worker.chatmd" local persistence="persistent"/>
|}
        ^ Checked.checked_list_declarations
        ^ {|<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
      )
    ; ( "worker.chatmd"
      , {|<developer>Review the selected instructions on an explicitly enabled recurrence.</developer>
<script id="worker" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
      )
    ; "server.sexp", Native.configuration
    ]
    @ Checked.checked_list_sources ~namespace:"skill-recurrence-example"
  ;;

  let assert_guidance inputs expected =
    let request = Native.wire_text inputs in
    assert (String.is_substring request ~substring:"# Recurring work");
    let guidance = Native.guidance_inputs inputs in
    [%test_eq: int] (Option.is_some expected |> Bool.to_int) (List.length guidance);
    Option.iter expected ~f:(fun (present, absent) ->
      let text = Jsonaf.to_string (List.hd_exn guidance) in
      assert (String.is_substring text ~substring:present);
      assert (not (String.is_substring text ~substring:absent)))
  ;;

  let original_guidance = Some ("SKILL-review-ORIGINAL", "SKILL-review-REFRESHED")
  let refreshed_guidance = Some ("SKILL-review-REFRESHED", "SKILL-review-ORIGINAL")

  let assert_list inputs key =
    let entries = Native.complete inputs key |> Wire.result |> Native.array "entries" in
    [%test_eq: int] 1 (List.length entries);
    let view = List.hd_exn entries in
    assert (
      Jsonaf.exactly_equal
        (P.Id.Recurrence.to_json recurrence_id)
        (Native.member "id" view));
    [%test_eq: string] "paused" (Native.string "kind" (Native.member "control" view))
  ;;

  let selection (state : Session.Session_state.t) =
    Option.value_exn state.instruction_selection
  ;;

  let recurrence (state : Session.Session_state.t) =
    let catalog = Option.value_exn state.recurrence_catalog in
    let entry =
      Session.Recurrence_catalog.find catalog recurrence_id |> Option.value_exn
    in
    let frontier = Session.Recurrence_entry.frontier entry |> protocol_ok in
    assert (List.is_empty (Session.Recurrence_frontier.executions frontier));
    assert (List.is_empty (Session.Recurrence_frontier.child_associations frontier));
    Session.Recurrence_entry.current entry |> Session.Recurrence_definition.digest
  ;;

  type saved =
    { session_id : P.Id.Session.t
    ; prompt_revision_id : P.Id.Prompt_revision.t
    ; selected : Jsonaf.t
    ; definition_sha256 : string
    ; reads : int
    }

  let reopen saved ~sw ~env ~root ~configuration =
    Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / root / "workspace" / skill_member);
    let calls = ref 0 in
    let failed = ref None in
    let post_stream ~sw:_ ~inputs =
      try
        Int.incr calls;
        assert_guidance inputs refreshed_guidance;
        match !calls with
        | 1 -> call_events (Wire.call "restored-list" "checked_list" Native.empty)
        | 2 ->
          assert_list inputs "restored-list";
          call_events []
        | _ -> failwith "restored checked recurrence requested an extra provider call"
      with
      | exn ->
        failed := Some exn;
        raise exn
    in
    let assert_retained (current : Session.Session_state.t) =
      assert (
        P.Id.Prompt_revision.equal
          saved.prompt_revision_id
          current.spec.prompt_revision_id);
      assert (
        Jsonaf.exactly_equal
          saved.selected
          (Selection.to_jsonaf (selection current) |> protocol_ok));
      [%test_eq: string] saved.definition_sha256 (recurrence current);
      [%test_eq: int] 1 (Recovery.invocation_count current "recurrence_create")
    in
    Recovery.with_reopened_session
      ~sw
      ~env
      ~root
      ~configuration
      ~inference_policy:
        (guidance_inference_policy ~default_model:"fixture-model" ~post_stream ())
      ~session_id:saved.session_id
      ~check_cold:(fun restored ->
        assert_retained restored;
        [%test_eq: int] 0 !calls)
      (fun _ entry handle ->
         let resumed =
           try
             Recovery.send
               env
               handle
               entry
               ~terminal:Completed
               "Read the saved recurrence."
           with
           | exn ->
             Option.iter !failed ~f:raise;
             raise exn
         in
         Option.iter !failed ~f:raise;
         assert_retained resumed;
         [%test_eq: int] 2 !calls;
         [%test_eq: int]
           (saved.reads + 1)
           (Recovery.invocation_count resumed "checked_list"))
  ;;

  let run () =
    let saved = ref None in
    let file = ref None in
    let inputs = ref [] in
    let chosen = ref Native.empty in
    let descriptor = ref Native.empty in
    let complete id = Native.complete !inputs id in
    let capture id =
      let discovery = complete id in
      [%test_eq: string list]
        [ skill_member ]
        (List.map (Native.array "members" discovery) ~f:(Native.string "member"));
      descriptor := Native.descriptor !chosen discovery;
      Wire.call (id ^ "-capture") "instructions_read" !descriptor
    in
    let activate capture_id ~revision ~key =
      let id = Native.string "capture_id" (complete capture_id) in
      Wire.call
        key
        "instructions_activate"
        (Native.activate !descriptor id ~revision ~key)
    in
    with_daemon
      ~config_file:"server.sexp"
      ~sources
      ~workspace_files:[ skill_member, original_skill ]
      ~request_guidance:true
      ~expect_moderator:true
      ~connect:(fun ~sw:_ ~env ~root daemon ->
        file := Some Eio.Path.(Eio.Stdenv.fs env / root / "workspace" / skill_member);
        Native.authenticated_connection daemon)
      ~calls:(Wire.call "roots" "instructions_refresh" (Native.refresh `Null))
      ~initial_requests:11
      ~expected_requests:11
      ~completion_timeout:90.
      ~inspect_request:(fun number request ->
        inputs := request;
        assert_guidance
          request
          (if number >= 8
           then refreshed_guidance
           else if number >= 5
           then original_guidance
           else None))
      ~followup_calls:(function
        | 2 ->
          let root = List.hd_exn (Native.array "roots" (complete "roots")) in
          chosen := Native.selection ~kind:"skills" ~root ~scope:`Null;
          Wire.call "discover" "instructions_refresh" (Native.refresh !chosen)
        | 3 -> capture "discover"
        | 4 -> activate "discover-capture" ~revision:"0" ~key:"use-original"
        | 5 ->
          ignore (complete "use-original" : Jsonaf.t);
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            (Option.value_exn !file)
            refreshed_skill;
          Wire.call "refresh" "instructions_refresh" (Native.refresh !chosen)
        | 6 -> capture "refresh"
        | 7 -> activate "refresh-capture" ~revision:"1" ~key:"use-refresh"
        | 8 ->
          ignore (complete "use-refresh" : Jsonaf.t);
          Wire.call "before-create" "checked_list" Native.empty
        | 9 ->
          let listed = complete "before-create" in
          assert (List.is_empty (Native.array "entries" (Wire.result listed)));
          let request =
            Wire.create
              recurrence_id
              listed
              ~target:(Wire.authored "single_turn")
              ~key:"create-review"
          in
          Wire.call "schedule-review" "recurrence_create" request
        | 10 ->
          ignore (complete "schedule-review" : Jsonaf.t);
          Wire.call "after-create" "checked_list" Native.empty
        | 11 ->
          assert_list !inputs "after-create";
          []
        | _ -> failwith "unexpected skill/recurrence provider request")
      ~after_turn:(fun env handle entry ->
        let state = A.state entry.Agent_server.Session_registry.actor |> protocol_ok in
        let selected = selection state in
        [%test_eq: int64] 2L (Selection.revision selected);
        [%test_eq: string list]
          [ refreshed_skill ]
          (List.map (Selection.skills selected) ~f:Instruction_content.Loaded.contents);
        [%test_eq: int] 1 (Recovery.invocation_count state "recurrence_create");
        let reads = Recovery.invocation_count state "checked_list" in
        [%test_eq: int] 2 reads;
        saved
        := Some
             { session_id = state.identity.session_id
             ; prompt_revision_id = state.spec.prompt_revision_id
             ; selected = Selection.to_jsonaf selected |> protocol_ok
             ; definition_sha256 = recurrence state
             ; reads
             };
        H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
        unload_idle_runtime env entry.runtime;
        assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime)))
      ~after_shutdown:(fun ~sw ~env ~root ~configuration ->
        reopen (Option.value_exn !saved) ~sw ~env ~root ~configuration)
      (fun _ -> ())
  ;;
end

let%expect_test
    "authored selected skill and checked recurrence survive explicit refresh and reopen"
  =
  Skill_recurrence.run ();
  print_endline
    "Checked recurrence observed the saved definition with exact refreshed skill \
     guidance before and after reopen.";
  [%expect
    {| Checked recurrence observed the saved definition with exact refreshed skill guidance before and after reopen. |}]
;;
