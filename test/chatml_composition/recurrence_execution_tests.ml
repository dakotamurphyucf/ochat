open! Core
open Agent_server_test_support
module A = Agent_session
module P = Agent_protocol
module Actor = A.Session_actor
module Daemon = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module Host = Agent_server.Recurrence_execution_host
module Driver = Agent_server.Recurrence_dispatcher
module Configuration = Agent_server.Recurrence_execution_configuration
module Native = Instruction_runtime_tests
module Wire = Recurrence_runtime_tests

let protect_cleanup ~finally f =
  match f () with
  | result ->
    Eio.Cancel.protect finally;
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try Eio.Cancel.protect finally with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let child_id = P.Id.Recurrence.of_string "rec_native_private_execution" |> protocol_ok
let existing_id = P.Id.Recurrence.of_string "rec_native_private_existing" |> protocol_ok
let private_text = "actual native recurrence private resource"

let worker =
  {|<developer>RECURRENCE-NATIVE-PRIVATE-WORKER.</developer>
<tool name="read_file"><read id="private" path="${workspace}"/></tool>
<script id="worker" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start ->
  let* result = Tool.call("read_file", `Object([
    {key = "root"; value = `String("private")},
    {key = "file"; value = `String("native-private.txt")}
  ])) in
  (match result with
  | `Error(message) -> Task.fail(message)
  | `Ok(value) -> let* ignored = Run.continue() in Task.pure(state + 1))
| _ -> Task.pure(state)
</script>|}
;;

let controller =
  {|<developer>Explicit native recurring automation controller.</developer>
<tool name="recurrence_list"/>
<tool name="recurrence_create"/>
<tool name="recurrence_get"/>
<tool name="recurrence_resume"/>
<tool name="weekly_worker" agent="worker.chatmd" local persistence="persistent"/>
<script id="checked" language="chatml" kind="tool" src="checked.chatml" project="chatml.project.json"/>
<tool name="checked_list" type="chatml" script="checked" entrypoint="run" input_schema="any.json" output_schema="any.json"><uses tool="recurrence_list"/></tool>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let state entry = Actor.state entry.Registry.actor |> protocol_ok

let diagnostic_state (current : A.Session_state.t) =
  let error (error : P.Invocation.tool_error) =
    [%sexp
      (error.code : string)
    , (String.prefix error.message 512 : string)
    , (error.retryable : bool)]
  in
  let json_kind = function
    | `Null -> "null"
    | `True | `False -> "bool"
    | `String _ -> "string"
    | `Number _ -> "number"
    | `Array _ -> "array"
    | `Object _ -> "object"
  in
  let outcome = function
    | P.Invocation.Complete value -> [%sexp "complete", (json_kind value : string)]
    | Pending _ -> [%sexp "pending"]
    | Fail failure -> [%sexp "fail", (error failure : Sexp.t)]
    | Cancelled reason -> [%sexp "cancelled", (String.prefix reason 512 : string)]
  in
  let receipts =
    List.map current.moderator_executions ~f:(fun receipt ->
      let status =
        match receipt.P.Moderator_execution.status with
        | Running -> [%sexp "running"]
        | Completed _ -> [%sexp "completed"]
        | Failed failure -> [%sexp "failed", (error failure : Sexp.t)]
        | Interrupted reason -> [%sexp "interrupted", (String.prefix reason 512 : string)]
      in
      [%sexp (receipt.context.phase : P.Moderator_execution.phase), (status : Sexp.t)])
  in
  let jobs =
    List.map current.jobs ~f:(fun job ->
      let status =
        match job.P.Job.status with
        | Queued -> [%sexp "queued"]
        | Running -> [%sexp "running"]
        | Waiting_permission id -> [%sexp "waiting_permission", (id : P.Id.Permission.t)]
        | Waiting_completion _ -> [%sexp "waiting_completion"]
        | Succeeded -> [%sexp "succeeded"]
        | Cancelled -> [%sexp "cancelled"]
        | Interrupted reason -> [%sexp "interrupted", (String.prefix reason 512 : string)]
        | Failed failure ->
          [%sexp
            "failed"
          , (failure.code : P.Error.code)
          , (String.prefix failure.message 512 : string)]
      in
      [%sexp (job.id : P.Id.Job.t), (job.attempt : int), (status : Sexp.t)])
  in
  let invocations =
    List.map current.invocations ~f:(fun invocation ->
      let status =
        match invocation.P.Invocation.status with
        | Admitted -> [%sexp "admitted"]
        | Dispatching -> [%sexp "dispatching"]
        | Resolved value -> [%sexp "resolved", (outcome value : Sexp.t)]
        | Published value -> [%sexp "published", (outcome value : Sexp.t)]
      in
      [%sexp (invocation.context.tool_name : string), (status : Sexp.t)])
  in
  let runs =
    Option.value_map current.run_state ~default:[] ~f:A.Run_state.runs
    |> List.map ~f:(fun run ->
      run.P.Run.id, run.lifecycle, run.owned_work, run.terminal_work)
  in
  [%sexp
    { session = (current.identity.session_id : P.Id.Session.t)
    ; lifecycle = (current.lifecycle : A.Session_state.Lifecycle.t)
    ; runtime_initialization =
        (current.runtime_initialization : A.Session_state.Runtime_initialization.t)
    ; receipts : Sexp.t list
    ; jobs : Sexp.t list
    ; invocations : Sexp.t list
    ; runs : (P.Id.Run.t
             * P.Run.Lifecycle.t
             * P.Run_work.t list
             * P.Run_work.Terminal.t list)
               list
    }]
;;

let%expect_test
    "native authored capture executes private cold startup and later previews under the \
     same parent loan"
  =
  let inputs = ref [] in
  let child_providers = ref 0 in
  let phase = ref "initial parent turn" in
  let last_parent = ref None in
  let last_child = ref None in
  let association = ref None in
  let attempt = ref None in
  let actual_daemon = ref None in
  let actual_switch = ref None in
  let actual_child = ref None in
  let root_finished = ref false in
  let second_created = ref false in
  let parent_requests = ref 0 in
  let child_polls = ref 0 in
  let parent_polls = ref 0 in
  let parent_revision_changes = ref 0 in
  let parent_last_revision = ref None in
  let phase_first_elapsed = ref [] in
  let failed_elapsed = ref 0. in
  let complete id = Native.complete !inputs id in
  let call = Wire.call in
  Fixtures.with_daemon
    ~config_file:"server.sexp"
    ~sources:
      [ "agent.chatmd", controller
      ; "worker.chatmd", worker
      ; "server.sexp", Native.configuration
      ; "any.json", "{}"
      ; ( "chatml.project.json"
        , {|{"schema_version":1,"namespace":"recurrence-native-example","modules":{"Entry":{"implementation":"checked.chatml"}},"dependencies":{"requests":{"source":{"kind":"local","path":"library"},"project":"chatml.project.json","environment":"pure"}},"environments":{"default":{"modules":{"Requests":"requests.Recurrence_requests"}}}}|}
        )
      ; ( "library/chatml.project.json"
        , [%blob
            "../../docs-src/examples/libraries/recurrence-requests/chatml.project.json"] )
      ; ( "library/src/recurrence_requests.chatml"
        , [%blob
            "../../docs-src/examples/libraries/recurrence-requests/src/recurrence_requests.chatml"]
        )
      ; ( "library/src/recurrence_requests.chatmli"
        , [%blob
            "../../docs-src/examples/libraries/recurrence-requests/src/recurrence_requests.chatmli"]
        )
      ; ( "checked.chatml"
        , {|let run ctx input =
  let* result = Tool.call("recurrence_list", Requests.list()) in
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(message) -> Task.fail(message)|}
        )
      ]
    ~workspace_files:[ "native-private.txt", private_text ]
    ~connect:(fun ~sw ~env:_ ~root:_ daemon ->
      actual_daemon := Some daemon;
      actual_switch := Some sw;
      Native.authenticated_connection daemon)
    ~calls:(call "execution-list" "checked_list" (Wire.object_ []))
    ~initial_requests:5
    ~expected_requests:8
    ~expect_moderator:true
    ~completion_timeout:60.
    ~inspect_request:(fun number request ->
      parent_requests := number;
      inputs := request)
    ~auxiliary_response:(fun inputs ->
      let child_request =
        List.exists inputs ~f:(function
          | Openai.Responses.Item.Input_message { role = Developer; content; _ } ->
            List.exists content ~f:(function
              | Text { text; _ } ->
                String.is_substring text ~substring:"RECURRENCE-NATIVE-PRIVATE-WORKER"
              | Image _ -> false)
          | _ -> false)
      in
      if child_request
      then (
        Int.incr child_providers;
        [%test_eq: int] 1 !child_providers;
        Some (Fixtures.call_events []))
      else None)
    ~followup_calls:(function
      | 2 ->
        let create =
          Wire.create
            child_id
            (complete "execution-list")
            ~target:(Wire.authored "workflow")
            ~key:"native-private-create"
        in
        let capture = Wire.member "capture" create in
        let schedule = Wire.member "schedule" capture in
        let schedule =
          Wire.object_
            (List.map (Native.fields schedule) ~f:(fun (name, value) ->
               if String.equal name "first_due"
               then name, P.Timestamp.to_json (P.Timestamp.now ())
               else name, value))
        in
        let capture =
          Wire.object_
            (List.map (Native.fields capture) ~f:(fun (name, value) ->
               if String.equal name "schedule" then name, schedule else name, value))
        in
        let create =
          Wire.object_
            (List.map (Native.fields create) ~f:(fun (name, value) ->
               if String.equal name "capture" then name, capture else name, value))
        in
        call "execution-create" "recurrence_create" create
      | 3 ->
        ignore (complete "execution-create" : Jsonaf.t);
        call "execution-get" "recurrence_get" (Wire.lookup child_id)
      | 4 ->
        call
          "execution-enable"
          "recurrence_resume"
          (Wire.mutation
             child_id
             (complete "execution-get")
             ~source:true
             ~key:"native-private-enable")
      | 5 ->
        ignore (complete "execution-enable" : Jsonaf.t);
        root_finished := true;
        []
      | 6 -> call "child-list" "recurrence_list" (Wire.object_ [])
      | 7 ->
        let child = Option.value_exn !actual_child in
        let daemon = Option.value_exn !actual_daemon in
        let target =
          Wire.object_
            [ "kind", `String "existing_session"
            ; ( "session"
              , P.Session_ref.to_json
                  (P.Session_ref.create
                     ~server_id:
                       (Agent_store.Session_store.server_id (Daemon.store daemon))
                     ~session_id:(state child).identity.session_id) )
            ; ( "input"
              , P.Session.Message_content.to_json
                  { kind = Plain_text; text = "Later explicit input"; attachments = [] } )
            ]
        in
        call
          "child-existing"
          "recurrence_create"
          (Wire.create
             existing_id
             (complete "child-list")
             ~target
             ~key:"native-private-existing")
      | 8 ->
        ignore (complete "child-existing" : Jsonaf.t);
        second_created := true;
        []
      | _ -> failwith "unexpected native private execution transcript")
    ~after_turn:(fun env handle parent ->
      let mono_clock = Eio.Stdenv.mono_clock env in
      let started = Eio.Time.Mono.now mono_clock in
      let elapsed () =
        let span = Mtime.span started (Eio.Time.Mono.now mono_clock) in
        Float.(Mtime.Span.to_float_ns span / 1_000_000_000.)
      in
      let set_phase name =
        phase := name;
        if not (List.Assoc.mem !phase_first_elapsed name ~equal:String.equal)
        then phase_first_elapsed := (name, elapsed ()) :: !phase_first_elapsed
      in
      set_phase "configure actual recurrence hosts";
      last_parent := Some (state parent);
      assert !root_finished;
      let daemon = Option.value_exn !actual_daemon in
      let sw = Option.value_exn !actual_switch in
      let server_id = Agent_store.Session_store.server_id (Daemon.store daemon) in
      let planning_configuration =
        Agent_server.Recurrence_automation_configuration.create
          ~principal_id:(P.Id.Principal.of_string "pri_native_planner" |> protocol_ok)
          ~controllers:[ (state parent).identity.session_id ]
          ~expires_at:None
          ~zone_directory:None
        |> protocol_ok
      in
      let planning =
        Agent_server.Recurrence_automation_host.open_owned
          planning_configuration
          ~server_id
          ~sw
          ~fs:(Eio.Stdenv.fs env)
          ~now:P.Timestamp.now
          ~checkpoint:Eio.Fiber.check
        |> protocol_ok
      in
      let grant =
        Configuration.Controller.create
          ~session_id:(state parent).identity.session_id
          ~existing_targets:[]
          ~captured_children:true
        |> protocol_ok
      in
      let configuration =
        Configuration.create
          ~principal_id:(P.Id.Principal.of_string "pri_native_executor" |> protocol_ok)
          ~controllers:[ grant ]
          ~expires_at:None
        |> protocol_ok
      in
      let host =
        Host.create
          configuration
          ~factory:(Daemon.factory daemon)
          ~server_id
          ~sw
          ~now:P.Timestamp.now
        |> protocol_ok
      in
      let driver =
        Driver.create
          ~factory:(Daemon.factory daemon)
          ~registry:(Daemon.registry daemon)
          ~host
      in
      let finally () =
        Driver.close driver;
        Host.close host;
        Agent_server.Recurrence_automation_host.close_and_wait planning
      in
      protect_cleanup ~finally (fun () ->
        Actor.set_recurrence_planning_host
          parent.actor
          (Agent_server.Recurrence_automation_host.planning_host planning)
        |> protocol_ok;
        try
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
            let acknowledged_child = ref false in
            let selected_child = ref None in
            let rec await_child () =
              Int.incr child_polls;
              let selected =
                match !selected_child with
                | Some child_id -> Some child_id
                | None ->
                  if not !acknowledged_child
                  then (
                    set_phase "process actual recurrence entry";
                    match Driver.process_entry driver parent with
                    | Ok () | Error { code = Conflict; retryable = true; _ } -> ()
                    | Error error -> raise_s [%sexp (error : P.Error.t)]);
                  set_phase "await admitted captured child";
                  let parent_state = state parent in
                  last_parent := Some parent_state;
                  let catalog = Option.value_exn parent_state.recurrence_catalog in
                  let recurrence =
                    A.Recurrence_catalog.find catalog child_id |> Option.value_exn
                  in
                  let frontier = A.Recurrence_entry.frontier recurrence |> protocol_ok in
                  (attempt
                   := match A.Recurrence_frontier.executions frontier with
                      | [] -> None
                      | execution :: _ ->
                        let current = A.Recurrence_execution.current_attempt execution in
                        Some
                          ( A.Recurrence_execution_attempt.number
                              (A.Recurrence_execution.Attempt.request current)
                          , A.Recurrence_execution.Attempt.rejection current
                          , A.Recurrence_execution.Attempt.uncertainty current
                          , Option.is_some (A.Recurrence_execution.Attempt.origin current)
                          , Option.is_some
                              (A.Recurrence_execution.Attempt.terminal current) ));
                  (match A.Recurrence_frontier.child_associations frontier with
                   | [ actual_association ] ->
                     (* Cache only the child identity selected by the actual
                        retained controller origin ACK. Joining that worker does
                        not require decoding the unchanged controller carrier on
                        every poll. Fresh origin/catalog checks remain below. *)
                     (acknowledged_child
                      := match !attempt with
                         | Some (_, _, _, true, _) -> true
                         | Some _ | None -> false);
                     association
                     := Some (A.Recurrence_child_association.to_json actual_association);
                     let reference =
                       A.Recurrence_child_association.reference actual_association
                     in
                     if !acknowledged_child
                     then selected_child := Some reference.child_session_id;
                     Some reference.child_session_id
                   | [] -> None
                   | _ -> failwith "native capture created multiple children")
              in
              let child =
                Option.bind selected ~f:(Registry.find (Daemon.registry daemon))
              in
              match child with
              | Some child ->
                set_phase "join actual captured child moderator";
                let current =
                  Actor.with_moderator_checkpoint child.actor (fun () ->
                    Actor.state child.actor)
                  |> protocol_ok
                in
                last_child := Some current;
                if
                  List.exists current.moderator_executions ~f:(fun receipt ->
                    match receipt.P.Moderator_execution.status with
                    | Failed _ | Interrupted _ -> true
                    | Running | Completed _ -> false)
                then
                  raise_s
                    [%sexp
                      "captured startup cannot complete"
                    , (diagnostic_state current : Sexp.t)];
                if
                  Int.equal !child_providers 1
                  && Option.is_none current.active_operation
                  && List.length current.invocations = 1
                  && List.for_all current.moderator_executions ~f:(fun receipt ->
                    match receipt.P.Moderator_execution.status with
                    | Completed _ -> true
                    | Running | Failed _ | Interrupted _ -> false)
                then child, current
                else (
                  Eio.Fiber.yield ();
                  await_child ())
              | None ->
                Eio.Fiber.yield ();
                await_child ()
            in
            let child, current = await_child () in
            set_phase "verify actual captured child startup";
            actual_child := Some child;
            let native = List.hd_exn current.invocations in
            [%test_eq: string] "read_file" native.context.tool_name;
            (match native.status with
             | Resolved (Complete value) | Published (Complete value) ->
               assert (
                 String.is_substring (Jsonaf.to_string value) ~substring:private_text)
             | _ -> raise_s [%sexp (native : P.Invocation.t)]);
            [%test_eq: int]
              1
              (List.count current.moderator_executions ~f:(fun receipt ->
                 P.Moderator_execution.equal_phase receipt.context.phase Session_start));
            let index = Option.value_exn current.run_state in
            let run = List.hd_exn (A.Run_state.runs index) in
            [%test_eq: int] 1 (List.length (A.Run_state.runs index));
            assert (P.Run.Mode.equal run.mode Workflow);
            let origin = A.Run_state.recurrence_origin index run.id |> Option.value_exn in
            let controller_catalog = Option.value_exn (state parent).recurrence_catalog in
            let controller_recurrence =
              A.Recurrence_catalog.find controller_catalog child_id |> Option.value_exn
            in
            let execution =
              A.Recurrence_frontier.executions
                (A.Recurrence_entry.frontier controller_recurrence |> protocol_ok)
              |> List.hd_exn
            in
            let attempt = A.Recurrence_execution.current_attempt execution in
            A.Recurrence_run_origin.validate
              origin
              ~intent:(A.Recurrence_execution.intent execution)
              ~attempt:(A.Recurrence_execution.Attempt.request attempt)
            |> protocol_ok;
            A.Recurrence_run_origin.validate_run
              origin
              ~run
              ~admission:(A.Recurrence_run_origin.admission origin)
            |> protocol_ok;
            assert (
              Option.equal
                A.Recurrence_run_origin.equal
                (Some origin)
                (A.Recurrence_execution.Attempt.origin attempt));
            let before = current in
            set_phase "submit parent existing-target selection turn";
            let submission =
              Agent_client.Session_handle.send_message
                handle
                { kind = Plain_text
                ; text = "Select the actual captured child as an existing target"
                ; attachments = []
                }
              |> protocol_ok
            in
            ignore (Option.value_exn submission.operation_id : P.Id.Operation.t);
            let rec await_second () =
              Int.incr parent_polls;
              set_phase "await parent existing-target selection turn";
              (* Capture before State: a committed append between the snapshot
                 and await resolves this exact promise. The final Operation
                 transition always emits completed/failed/cancelled evidence;
                 private observation ACKs need not wake this terminal wait. *)
              let changed = A.Durable_event_log.changed parent.durable_events in
              let current = state parent in
              last_parent := Some current;
              if
                not
                  (Option.equal
                     Int64.equal
                     !parent_last_revision
                     (Some current.counters.revision))
              then (
                Int.incr parent_revision_changes;
                parent_last_revision := Some current.counters.revision);
              if !second_created && Option.is_none current.active_operation
              then ()
              else (
                Eio.Promise.await changed;
                await_second ())
            in
            await_second ();
            [%test_eq: int] 1 !child_providers;
            let document state =
              state_document state
              |> Result.map_error ~f:(fun error ->
                Sexp.to_string_hum [%sexp (error : Agent_store.Store_error.t)])
              |> Result.ok_or_failwith
              |> Document_schema.Document.json
            in
            assert (Document_schema.Json.equal (document before) (document (state child)));
            let catalog = Option.value_exn (state parent).recurrence_catalog in
            let captured =
              A.Recurrence_catalog.find catalog child_id |> Option.value_exn
            in
            [%test_eq: int]
              1
              (List.length
                 (A.Recurrence_frontier.child_associations
                    (A.Recurrence_entry.frontier captured |> protocol_ok)));
            let existing =
              A.Recurrence_catalog.find catalog existing_id |> Option.value_exn
            in
            (match
               A.Recurrence_target.view
                 (A.Recurrence_definition.target (A.Recurrence_entry.current existing))
             with
             | Existing_session selected ->
               assert (
                 P.Id.Session.equal
                   (P.Session_ref.session_id selected.session)
                   current.identity.session_id)
             | Fresh_child _ -> failwith "existing target was recaptured as a child");
            print_endline
              "native captured Workflow starts once with its private native resource; \
               same-parent native preview reuses the live loan without child replay")
        with
        | exn ->
          failed_elapsed := elapsed ();
          Exn.reraise
            exn
            (Sexp.to_string_hum
               [%sexp
                 { phase = (!phase : string)
                 ; elapsed_seconds = (!failed_elapsed : float)
                 ; phase_first_elapsed =
                     (List.rev !phase_first_elapsed : (string * float) list)
                 ; parent_requests = (!parent_requests : int)
                 ; child_polls = (!child_polls : int)
                 ; parent_polls = (!parent_polls : int)
                 ; parent_revision_changes = (!parent_revision_changes : int)
                 ; parent_last_revision = (!parent_last_revision : int64 option)
                 ; parent = (Option.map !last_parent ~f:diagnostic_state : Sexp.t option)
                 ; child = (Option.map !last_child ~f:diagnostic_state : Sexp.t option)
                 ; association = (!association : Jsonaf.t option)
                 ; attempt =
                     (!attempt
                      : (int * P.Error.code option * P.Error.code option * bool * bool)
                          option)
                 ; child_providers = (!child_providers : int)
                 ; root_finished = (!root_finished : bool)
                 ; second_created = (!second_created : bool)
                 }])))
    (fun _ -> assert !second_created);
  [%expect
    {| native captured Workflow starts once with its private native resource; same-parent native preview reuses the live loan without child replay |}]
;;
