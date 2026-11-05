open! Core
open Support
module Cold = Activation_tests
module Service = Agent_server.Recurrence_run_service
module Owner = Agent_server.Runtime_owner
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Reservation = A.Recurrence_child_reservation
module Source = A.Runtime_builder.Source
module Kind = Agent_store.Captured_source_kind

module Cut = struct
  type t =
    | Activate
    | Revoked
  [@@deriving sexp_of]
end

let native_result = "546 actual specialist private read"

let parent_prompt =
  {|<developer>Headless authored recurrence controller.</developer>
<tool name="specialist" agent="specialist.chatmd" local persistence="persistent"/>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let specialist_prompt =
  {|<developer>Actual captured authored specialist.</developer>
<tool name="read_file"><read id="private" path="${workspace}"/></tool>
<script id="specialist" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start ->
  let* result = Tool.call("read_file", `Object([
    {key = "root"; value = `String("private")},
    {key = "file"; value = `String("private.txt")}
  ])) in
  (match result with
  | `Error(message) -> Task.fail(message)
  | `Ok(_) -> let* ignored = Run.continue() in Task.pure(state + 1))
| _ -> Task.pure(state)
</script>|}
;;

(* Support.with_daemon intentionally exercises the default Ui family. Keep this
   small composition-root variant so this law uses a real Headless definition;
   no copied definition, capability registry or admission issuer is supplied. *)
let with_headless_daemon env root config ~providers ~constructions f =
  Eio.Switch.run (fun sw ->
    let policy =
      T.inference_policy
        ~default_model:"fixture-model"
        ~post_stream:(fun ~sw:_ ~inputs:_ ->
          Int.incr providers;
          if !providers > 1 then failwith "authored recurrence replayed provider";
          Cold.completed_response "546 actual recurrence provider")
    in
    let policy =
      { policy with
        runtime_inference_ports =
          (fun actor ->
            Int.incr constructions;
            policy.runtime_inference_ports actor)
      }
    in
    let daemon =
      Daemon.start
        ~sw
        ~env
        ~config
        ~tool_dir:root
        ~home:root
        ~process_start_identity:None
        ~options:
          { Daemon.default_options with
            root_invocation = Headless
          ; qualify_chatml_extensions = true
          ; inference_policy = policy
          }
        ()
      |> protocol_ok
    in
    protect_cleanup
      ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
      (fun () ->
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
           let connection = T.connection daemon principal in
           protect_cleanup
             ~finally:(fun () -> Agent_client.Connection.close connection)
             (fun () ->
                T.initialize connection;
                f sw daemon connection))))
;;

let document child = T.state_document (state child) |> store_ok |> D.Document.json

let frames env child =
  let handle = Option.value_exn child.Registry.store_handle in
  let journal =
    Agent_store.Journal.open_existing
      ~env
      ~directory:(S.Handle.journal_directory handle)
      ~max_payload_length:16777216
      ~max_segment_bytes:67108864L
      ~max_segment_frames:4096
    |> store_ok
  in
  Agent_store.Journal.scan journal
  |> store_ok
  |> fun scan ->
  List.map scan.Agent_store.Journal.entries ~f:(fun entry ->
    Agent_store.Frame.payload entry.Agent_store.Journal.frame)
;;

let public_names parent =
  Owner.with_background_runtime parent.Registry.runtime (fun runtime ->
    let native = Option.value_exn runtime.A.Runtime_builder.native_runtime in
    assert (Lazy.is_val native.capabilities);
    Lazy.force native.capabilities
    |> Result.map ~f:(fun capabilities ->
      Chat_response.Tool_capability.references capabilities
      |> List.map ~f:(fun reference -> reference.Chat_response.Tool_capability.name))
    |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message))
  |> protocol_ok
;;

let preview daemon parent =
  Factory.preview_recurrence_authored_target
    (Daemon.factory daemon)
    parent
    ~actor:proof
    ~authorize
    ~name:"specialist"
    ~mode:Workflow
  |> protocol_ok
;;

let target_json target = Target.to_document target |> document_ok |> D.Document.json

let captured_artifact target =
  match Target.view target with
  | Existing_session _ -> failwith "preview returned an existing target"
  | Fresh_child { artifact; definition; parent = _; mode; source_kind } ->
    assert (Option.equal P.Run.Mode.equal mode (Some Workflow));
    (match definition.A.Prompt_definition.root_invocation with
     | Headless -> ()
     | Ui -> failwith "preview changed defining parent's Headless provenance");
    (match source_kind with
     | Kind.Authored_tool selector -> [%test_eq: string] "specialist" selector.name
     | Artifact -> failwith "preview omitted authored source discriminator");
    Agent_store.Captured_prompt_artifact.value artifact
;;

(* Source-only ownership is genuine and closes before execution. The returned
   observer is DATA derived from that actual staged captured source; Service
   must authenticate its installation again at the real Held writer. *)
let stage_observer env daemon child (artifact : Artifact.Artifact.t) =
  let before = document child in
  let before_frames = frames env child in
  let current = state child in
  let request =
    A.Run_admission_request.Headless.create
      ~session_id:current.identity.session_id
      ~generation:current.identity.generation
      ~expected_revision:current.counters.revision
      ~mode:Workflow
      ~input:Authored_start
      ~key:(P.Idempotency_key.of_string "546-source-restoration" |> protocol_ok)
    |> protocol_ok
  in
  let preparation =
    match
      Actor.begin_headless_run_preparation child.actor ~actor:proof ~authorize ~request
      |> protocol_ok
    with
    | Prepare preparation -> preparation
    | Retained _ -> failwith "source restoration unexpectedly retained Admission"
  in
  let observer =
    protect_cleanup
      ~finally:(fun () ->
        Actor.end_run_preparation child.actor preparation |> protocol_ok)
      (fun () ->
         Factory.with_prepared_run_source
           (Daemon.factory daemon)
           child
           ~preparation
           (fun prepared ->
              Owner.Prepared.check_basis prepared |> protocol_ok;
              let source =
                Actor.Source_binding.source (Owner.Prepared.binding prepared)
              in
              (match Source.selection source with
               | Captured retained ->
                 let actual =
                   A.Captured_recurrence_source.revision retained
                   |> A.Prompt_revision.artifact
                 in
                 assert (
                   P.Id.Prompt_revision.equal artifact.revision_id actual.revision_id);
                 [%test_eq: string] artifact.manifest_sha256 actual.manifest_sha256;
                 assert (P.Timestamp.equal artifact.created_at actual.created_at);
                 (match
                    A.Recurrence_child_origin.root_invocation
                      (A.Captured_recurrence_source.origin retained)
                  with
                  | Ui -> ()
                  | Headless -> failwith "captured specialist lost child Ui role")
               | Authored _ | Authored_child _ | Generated _ ->
                 failwith
                   "cold source was relabeled instead of retaining captured custody");
              match Source.moderator source with
              | Extension observer -> Ok observer
              | No_moderator | Legacy _ -> failwith "actual specialist v1 source missing")
         |> protocol_ok)
  in
  assert (Jsonaf.exactly_equal before (document child));
  [%test_eq: string list] before_frames (frames env child);
  assert (not (Owner.is_loaded child.runtime));
  observer
;;

let intent_attempt daemon parent child reservation observer =
  let recurrence = entry parent in
  let association =
    Entry.frontier recurrence
    |> protocol_ok
    |> Frontier.child_associations
    |> List.find_exn ~f:(fun association ->
      P.Id.Session.equal
        (A.Recurrence_child_association.reference association).child_session_id
        (state child).identity.session_id)
  in
  let child_data = Intent.Child.create ~reservation ~association |> protocol_ok in
  let current = state child in
  let source =
    P.Run_source.create
      ~observer
      ~generation:current.identity.generation
      ~installation_epoch:1L
    |> protocol_ok
  in
  let intent =
    Intent.create
      ~definition:(Entry.current recurrence)
      ~control_revision:(Entry.control_revision recurrence)
      ~nominal_json:(Reservation.nominal_json reservation)
      ~target:
        (P.Session_ref.create
           ~server_id:(S.server_id (Daemon.store daemon))
           ~session_id:current.identity.session_id)
      ~source
      ~child:(Some child_data)
    |> protocol_ok
  in
  let attempt =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id:principal.id
      ~expected_revision:current.counters.revision
      ~key:(P.Idempotency_key.of_string "546-actual-activation" |> protocol_ok)
    |> protocol_ok
  in
  intent, attempt
;;

let await_completed env child providers =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
    let rec loop () =
      let current = Dispatch_captured_tests.healthy child in
      let starts =
        List.filter current.moderator_executions ~f:(fun receipt ->
          P.Moderator_execution.equal_phase receipt.context.phase Session_start)
      in
      let completed =
        match starts with
        | [ { status = Completed _; _ } ] -> true
        | _ -> false
      in
      if
        completed
        && Int.equal !providers 1
        && Option.is_none current.active_operation
        && T.observed_idle current.lifecycle.observed
      then current
      else (
        Eio.Fiber.yield ();
        loop ())
    in
    loop ())
;;

let run env (cut : Cut.t) =
  let root = T.temporary_root env in
  protect_cleanup
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / root))
    (fun () ->
       let save file text =
         Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(env#fs / root / file) text
       in
       save "parent.chatmd" parent_prompt;
       save "specialist.chatmd" specialist_prompt;
       save "private.txt" native_result;
       let configuration = T.config root root (Filename.concat root "parent.chatmd") in
       let providers = ref 0 in
       let constructions = ref 0 in
       let parent_id, child_id, reservation, target =
         with_headless_daemon
           env
           root
           configuration
           ~providers
           ~constructions
           (fun sw daemon connection ->
              let created, _ = T.create_session ~start_immediately:true connection in
              let parent = loaded daemon created.id in
              start_parent_settled env sw connection parent;
              (match (state parent).spec.root_invocation with
               | Headless -> ()
               | Ui -> assert false);
              let names = public_names parent in
              assert (List.mem names "specialist" ~equal:String.equal);
              assert (not (List.mem names "read_file" ~equal:String.equal));
              let before = document parent in
              let baseline = !constructions, !providers in
              let target = preview daemon parent in
              let repeated = preview daemon parent in
              assert (Jsonaf.exactly_equal (target_json target) (target_json repeated));
              [%test_eq: string]
                (Target.digest target |> protocol_ok)
                (Target.digest repeated |> protocol_ok);
              assert (Jsonaf.exactly_equal before (document parent));
              [%test_eq: int * int] baseline (!constructions, !providers);
              assert (List.is_empty (state parent).invocations);
              let artifact = captured_artifact target in
              with_planner env sw daemon parent (fun () ->
                let permit = reserve parent target in
                let reservation = Actor.Recurrence_creation.reservation permit in
                let linked =
                  Factory.create_recurrence_child (Daemon.factory daemon) ~parent ~permit
                  |> protocol_ok
                in
                let child_id =
                  P.Session_ref.session_id (Factory.Recurrence_child.session linked)
                in
                let child, record = assert_dormant daemon child_id in
                assert (Ledger.equal_stage record.stage Linked);
                assert (
                  P.Id.Prompt_revision.equal
                    artifact.revision_id
                    (state child).spec.prompt_revision_id);
                (match (state child).spec.root_invocation with
                 | Ui -> ()
                 | Headless -> assert false);
                [%test_eq: int] 0 !providers;
                created.id, child_id, reservation, target))
       in
       (* New daemon/Store/Actor/Owner lifetimes; no previous permit, runtime,
         prepared source, native registry or inferred authority survives. *)
       with_headless_daemon
         env
         root
         configuration
         ~providers
         ~constructions
         (fun sw daemon connection ->
            let parent =
              Registry.load (Daemon.registry daemon) parent_id |> protocol_ok
            in
            start_parent_settled env sw connection parent;
            with_planner env sw daemon parent (fun () ->
              Registry.load (Daemon.registry daemon) child_id |> protocol_ok |> ignore;
              let child, record = assert_dormant daemon child_id in
              A.Recurrence_child_origin.of_record record
              |> protocol_ok
              |> fun origin ->
              A.Recurrence_child_origin.validate_definition
                origin
                (Entry.current (entry parent))
              |> protocol_ok;
              let artifact = captured_artifact target in
              let before_staging = !constructions in
              let observer = stage_observer env daemon child artifact in
              [%test_eq: int] before_staging !constructions;
              [%test_eq: int] 0 !providers;
              let intent, attempt =
                intent_attempt daemon parent child reservation observer
              in
              let service =
                Service.create
                  ~factory:(Daemon.factory daemon)
                  ~authorize:(fun _actor current -> authorize current)
                  ~automatic_policy:(fun _current _definition -> Ok true)
              in
              let before = document child in
              let before_frames = frames env child in
              let counts = !constructions, !providers in
              (match cut with
               | Activate -> ()
               | Revoked ->
                 let revoked =
                   Ledger.revoke
                     (S.delegations (Daemon.store daemon))
                     record
                     Authority_changed
                   |> store_ok
                 in
                 assert (
                   Option.equal
                     Ledger.equal_revocation
                     revoked.revocation
                     (Some Authority_changed)));
              let result =
                Service.activate
                  service
                  ~controller:parent
                  ~controller_actor:proof
                  ~target:child
                  ~target_actor:proof
                  ~intent
                  ~attempt
              in
              match cut with
              | Revoked ->
                let rejected =
                  match result with
                  | Error error -> Error error
                  | Ok (Service.Outcome.Attempted attempt) ->
                    Service.Attempt.result attempt
                  | Ok (Retained _) ->
                    failwith "revoked captured private source retained admission"
                in
                (match rejected with
                 | Error error -> assert (P.Error.equal_code error.code Permission_denied)
                 | Ok _ -> failwith "revoked captured private source reached admission");
                [%test_eq: int * int] counts (!constructions, !providers);
                assert (Jsonaf.exactly_equal before (document child));
                [%test_eq: string list] before_frames (frames env child);
                assert_dormant daemon child_id |> ignore
              | Activate ->
                let admission = Run_service_tests.activated child result in
                let current = await_completed env child providers in
                let _, _, run = Run_service_tests.selected_run child intent attempt in
                assert (P.Id.Run.equal admission.run_id run.id);
                let startup =
                  List.find_exn current.moderator_executions ~f:(fun receipt ->
                    P.Moderator_execution.equal_phase receipt.context.phase Session_start)
                in
                let invocation =
                  match current.invocations with
                  | [ invocation ] -> invocation
                  | invocations -> raise_s [%sexp (invocations : P.Invocation.t list)]
                in
                [%test_eq: string] "read_file" invocation.context.tool_name;
                assert (
                  Option.equal
                    P.Inference_origin.equal
                    invocation.inference_origin
                    (Some (Run run.id)));
                assert (
                  Option.equal
                    P.Id.Moderator_execution.equal
                    invocation.parent_event
                    (Some startup.context.id));
                (match invocation.status with
                 | Resolved (Complete result) | Published (Complete result) ->
                   assert (
                     String.is_substring
                       (Jsonaf.to_string result)
                       ~substring:native_result)
                 | _ -> raise_s [%sexp (invocation : P.Invocation.t)]);
                let stable = !constructions, !providers in
                (match
                   Service.activate
                     service
                     ~controller:parent
                     ~controller_actor:proof
                     ~target:child
                     ~target_actor:proof
                     ~intent
                     ~attempt
                   |> protocol_ok
                 with
                 | Retained retained ->
                   assert (P.Run_receipt.equal admission retained.admission);
                   retained.reconciliation |> protocol_ok |> ignore
                 | Attempted _ -> failwith "duplicate workflow reentered source/runtime");
                [%test_eq: int * int] stable (!constructions, !providers))))
;;

let%expect_test
    "captured authored private source survives cold restore and honors revocation"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    List.iter [ Cut.Activate; Revoked ] ~f:(fun cut ->
      try run env cut with
      | exn ->
        Exn.reraise
          exn
          (Sexp.to_string_hum
             [%sexp "actual captured authored recurrence", (cut : Cut.t)])));
  print_endline
    "Headless provenance; deterministic selected private capture; real cold child \
     Run/native dispatch once; revoked source refuses before effects";
  [%expect
    {| Headless provenance; deterministic selected private capture; real cold child Run/native dispatch once; revoked source refuses before effects |}]
;;
