open! Core
open Creation_support
module Builder = A.Runtime_builder
module Owner = Agent_server.Runtime_owner
module Binding = A.Session_actor.Source_binding
module Held = A.Session_actor.Held_run
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Origin = A.Recurrence_run_origin

(* Real artifact capture and physical target ownership. The Definition/Intent
   below are DATA, not a controller admission or activation permit. No Manager,
   compiled runtime, Run authority or provider is supplied by this fixture. *)
let with_captured_target f =
  let captured = ref None in
  with_store
    ~prepare_initial:(fun env initial ->
      let workspace = initial.A.Session_state.spec.workspace_instance in
      let root = Eio.Path.(Eio.Stdenv.fs env / workspace.canonical_root.native_path) in
      let root_file = Eio.Path.(root / "held.chatmd") in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        root_file
        {|<script id="held" language="chatml" kind="moderator">let initial_state = fail("inert admission evaluated initializer")
let on_event = fun event state -> Task.pure(state)</script>|};
      let definition =
        A.Prompt_definition.create
          ~root_invocation:Ui
          ~id:(P.Id.Prompt_definition.of_string "prd_held_source" |> protocol_ok)
          ~config_name:"held-source"
          ~root_file:(Eio.Path.native_exn root_file)
          ~allowed_workspaces:[]
          ~permission_profile:"interactive"
          ~runtime_policy:None
          ~enabled:true
          ~description:None
        |> store_ok
      in
      let artifacts =
        Agent_store.Prompt_artifact_store.create
          ~env
          ~root:(Eio.Path.native_exn Eio.Path.(root / "held-artifacts"))
        |> store_ok
      in
      let revision =
        match
          A.Prompt_revision_builder.build
            ~env
            ~artifact_store:artifacts
            ~transaction_id
            ~created_at:timestamp
            definition
        with
        | Ok revision -> revision
        | Error diagnostics ->
          raise_s [%sexp (diagnostics : A.Prompt_revision_builder.Diagnostic.t list)]
      in
      let source =
        Builder.Source.prepare
          revision
          ~script_preparation:(Some (script_preparation ()))
          ~invocation:Ui
          ~env
          ~checkpoint:Eio.Fiber.check
        |> protocol_ok
      in
      Eio.Path.unlink root_file;
      captured := Some (source, root_file);
      let stopped =
        actor_state
          ~workspace_instance:workspace
          ~liveness:Detached
          ~start_immediately:false
      in
      { stopped with
        identity = initial.identity
      ; spec =
          { stopped.spec with
            prompt_definition_id = Some definition.id
          ; prompt_revision_id = A.Prompt_revision.id revision
          }
      })
    (fun fixture ->
       let source, root_file = Option.value_exn !captured in
       f fixture source root_file)
;;

let original_request fixture source ?(target = session) key =
  let observer =
    match Builder.Source.moderator source with
    | Legacy observer -> observer
    | Extension _ | No_moderator -> failwith "expected actual captured legacy source"
  in
  let source =
    P.Run_source.create
      ~observer
      ~generation:fixture.initial.identity.generation
      ~installation_epoch:1L
    |> protocol_ok
  in
  let target_data =
    A.Recurrence_target.create_existing
      ~session:target
      ~source
      ~configuration_revision:fixture.initial.spec.configuration_revision
      ~input:{ kind = Plain_text; text = "held input"; attachments = [] }
    |> protocol_ok
  in
  let definition =
    F.create
      ~key:
        (I.Definition.create ~controller:session ~recurrence_id:(id "held") ~revision:0L
         |> protocol_ok)
      ~creator:principal_id
      ~source
      ~birth_lifecycle_revision:P.Session_lifecycle.Revision.zero
      ~schedule:(interval ~first_due:(P.Timestamp.to_time_ns timestamp) ())
      ~target:target_data
      ~policy:A.Recurrence_policy.default
      ~created_at:timestamp
    |> protocol_ok
  in
  let intent =
    Intent.create
      ~definition
      ~control_revision:0L
      ~nominal_json:(N.create_interval definition ~slot:0L |> protocol_ok |> N.to_json)
      ~target
      ~source
      ~child:None
    |> protocol_ok
  in
  let attempt =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id
      ~expected_revision:fixture.initial.counters.revision
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    |> protocol_ok
  in
  intent, attempt
;;

let begin_preparation actor ~proof ~policy intent attempt =
  match
    A.Session_actor.begin_headless_run_preparation
      actor
      ~actor:proof
      ~authorize:policy
      ~request:(Attempt.request attempt ~intent |> protocol_ok)
    |> protocol_ok
  with
  | Prepare preparation -> preparation
  | Retained _ -> failwith "expected a genuine new held preparation"
;;

let admit actor binding intent attempt session =
  A.Session_actor.admit_held_recurrence_run actor ~binding ~intent ~attempt ~session
;;

let frames fixture =
  let journal =
    Agent_store.Journal.open_existing
      ~env:fixture.env
      ~directory:fixture.directory
      ~max_payload_length:1048576
      ~max_segment_bytes:4194304L
      ~max_segment_frames:64
    |> store_ok
  in
  let scan = Agent_store.Journal.scan journal |> store_ok in
  List.map scan.entries ~f:(fun entry -> Agent_store.Frame.payload entry.frame)
;;

let assert_same_state expected actual =
  assert (
    Jsonaf.exactly_equal
      (D.Document.json (state_document expected))
      (D.Document.json (state_document actual)))
;;

let assert_pristine_effects (before : A.Session_state.t) (after : A.Session_state.t) =
  (* Complete state equality after removing ONLY the real admission positions
     and Run index proves no history/allocator/lifecycle/Operation side effect. *)
  assert_same_state
    before
    { after with
      counters =
        { after.counters with
          revision = before.counters.revision
        ; transaction_sequence = before.counters.transaction_sequence
        ; event_sequence = before.counters.event_sequence
        }
    ; identity = { after.identity with updated_at = before.identity.updated_at }
    ; run_state = before.run_state
    }
;;

let%expect_test
    "actual captured target admits one inert held Run before any runtime effect"
  =
  with_captured_target (fun fixture source root_file ->
    let events = ref [] in
    let actor =
      create_actor
        fixture
        ~sw:fixture.sw
        ~published_events:(fun actual -> events := !events @ actual)
        ()
    in
    Exn.protect
      ~finally:(fun () -> A.Session_actor.shutdown actor)
      ~f:(fun () ->
        let builds = ref 0
        and guards = ref 0
        and policies = ref 0 in
        let original =
          Operator_authorization.guarded ~principal ~is_current:(fun () ->
            Int.incr guards;
            true)
        in
        let policy state =
          Int.incr policies;
          authorize state
        in
        let owner =
          Owner.create ~actor ~initial:None ~build:(fun () ->
            Int.incr builds;
            failwith "inert admission entered runtime builder")
        in
        Exn.protect
          ~finally:(fun () -> Owner.close_and_wait owner)
          ~f:(fun () ->
            let intent, attempt = original_request fixture source "held-actual" in
            let preparation =
              begin_preparation actor ~proof:original ~policy intent attempt
            in
            let escaped = ref None in
            Exn.protect
              ~finally:(fun () ->
                A.Session_actor.end_run_preparation actor preparation |> protocol_ok)
              ~f:(fun () ->
                Owner.with_prepared_source owner ~preparation ~source (fun prepared ->
                  let binding = Owner.Prepared.binding prepared in
                  let before = state actor in
                  let before_frames = frames fixture in
                  let foreign =
                    P.Session_ref.create
                      ~server_id:(P.Id.Server.of_string "srv_other_held" |> protocol_ok)
                      ~session_id:(P.Session_ref.session_id session)
                  in
                  let wrong_intent, wrong_attempt =
                    original_request fixture source ~target:foreign "held-actual"
                  in
                  List.iter
                    [ intent, attempt, foreign
                    ; wrong_intent, wrong_attempt, session
                    ; wrong_intent, wrong_attempt, foreign
                    ]
                    ~f:(fun (intent, attempt, supplied) ->
                      guards := 0;
                      policies := 0;
                      Record_tests.assert_code
                        Conflict
                        (admit actor binding intent attempt supplied);
                      [%test_eq: int] !guards 0;
                      [%test_eq: int] !policies 0;
                      assert_same_state before (state actor);
                      [%test_eq: string list] before_frames (frames fixture);
                      Owner.Prepared.check_basis prepared |> protocol_ok);
                  let held =
                    match admit actor binding intent attempt session |> protocol_ok with
                    | Held held -> held
                    | Retained _ | Rejected _ | Uncertain _ ->
                      failwith
                        "new inert admission did not return continuous Held custody"
                  in
                  let committed = state actor in
                  Held.check_basis held |> protocol_ok;
                  Owner.Prepared.check_basis prepared |> protocol_ok;
                  [%test_eq: int64]
                    committed.counters.revision
                    Int64.(before.counters.revision + 1L);
                  [%test_eq: int64]
                    committed.counters.transaction_sequence
                    Int64.(before.counters.transaction_sequence + 1L);
                  [%test_eq: int]
                    (List.length (frames fixture))
                    (List.length before_frames + 1);
                  [%test_eq: int64]
                    committed.counters.event_sequence
                    Int64.(before.counters.event_sequence + 1L);
                  (match !events with
                   | [ event ] ->
                     [%test_eq: int64] event.sequence committed.counters.event_sequence;
                     (match
                        P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                        |> protocol_ok
                      with
                      | Session_updated _ -> ()
                      | _ -> failwith "inert admission published an execution event")
                   | _ ->
                     failwith "inert admission did not publish exactly one state update");
                  assert_pristine_effects before committed;
                  let index = Option.value_exn committed.run_state in
                  let admission = Held.admission held in
                  let run = A.Run_state.find index admission.run_id |> Option.value_exn in
                  [%test_eq: int] (List.length (A.Run_state.runs index)) 1;
                  assert (P.Run.Lifecycle.equal run.lifecycle Admitted);
                  [%test_eq: int64] run.revision 0L;
                  assert (List.is_empty run.owned_work);
                  let origin =
                    A.Run_state.recurrence_origin index run.id |> Option.value_exn
                  in
                  Origin.validate origin ~intent ~attempt |> protocol_ok;
                  Origin.validate_run origin ~run ~admission |> protocol_ok;
                  assert_same_state committed (replayed fixture);
                  (match admit actor binding intent attempt session |> protocol_ok with
                   | Retained saved -> assert (P.Run_receipt.equal saved admission)
                   | Held _ | Rejected _ | Uncertain _ ->
                     failwith "duplicate recreated Held authority");
                  assert_same_state committed (state actor);
                  [%test_eq: int]
                    (List.length (frames fixture))
                    (List.length before_frames + 1);
                  [%test_eq: int] !builds 0;
                  assert (not (Eio.Path.is_file root_file));
                  assert (phys_equal (Binding.source binding) source);
                  escaped := Some held;
                  Ok ())
                |> protocol_ok);
            Record_tests.assert_code
              Conflict
              (Held.check_basis (Option.value_exn !escaped));
            [%test_eq: int] !builds 0;
            assert_same_state (state actor) (replayed fixture);
            print_endline
              "wrong-server callbacks/writes zero; one atomic Admitted Run and origin";
            print_endline "duplicate is DATA; runtime builder zero; escaped Held expires")));
  [%expect
    {|
    wrong-server callbacks/writes zero; one atomic Admitted Run and origin
    duplicate is DATA; runtime builder zero; escaped Held expires
    |}]
;;
