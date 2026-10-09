open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol

let%expect_test "source-changing reset keeps managed receipt and replacement snapshot" =
  Delegation_lifecycle_tests.with_fixture
    (fun
        env
         _sw
         _ledger
         record
         _foreign
         actor
         _runtime
         _backend
         _closes
         _reject
         _original
       ->
       let reference = Agent_store.Delegation_store.reference record in
       let stop =
         A.Session_actor.stop_managed
           actor
           ~reference
           ~key:(P.Idempotency_key.of_string "source-reset-stop" |> protocol_ok)
           ~mode:Cancel
           ~generation:0
           ~max_receipts:(Some 8)
         |> protocol_ok
       in
       let manager_a, _, _ = handoff_definition env ~declare_tool:false in
       let manager_b, _, _ =
         handoff_definition
           env
           ~declare_tool:false
           ~events:
             "| `Session_start -> Task.bind(Runtime.emit(`String(\"replacement\")), fun \
              ignored -> Task.pure(state)) | _ -> Task.pure(state)"
       in
       let snapshot manager =
         Chat_response.Moderator_manager.identity_snapshot manager
         |> Result.ok_or_failwith
         |> A.Runtime_builder.encode_moderator_snapshot
       in
       A.Session_actor.change_moderator actor (Some (snapshot manager_a))
       |> protocol_ok
       |> ignore;
       let attachment, _ =
         A.Session_actor.attach actor ~mode:Owner_read_write ~subscribe:false
         |> protocol_ok
       in
       let before = A.Session_actor.state actor |> protocol_ok in
       let scope =
         A.Run_admission.Scope.create
           ~principal_id:(Option.value_exn before.identity.creating_principal)
           ~observer:
             (Chat_response.Moderator_manager.invocation_observer manager_a
              |> Option.value_exn)
           ~startup_pending:(fun () -> true)
           ~authorize:(fun _ -> Ok ())
         |> protocol_ok
       in
       let request =
         P.Run_start.create
           ~session_id:before.identity.session_id
           ~attachment_id:attachment.id
           ~generation:before.identity.generation
           ~expected_revision:before.counters.revision
           ~mode:Workflow
           ~input:Authored_start
           ~key:(P.Idempotency_key.of_string "source-reset-run" |> protocol_ok)
         |> protocol_ok
       in
       let receipt =
         A.Session_actor.admit_run
           actor
           ~scope
           ~request
           ~session:
             (P.Session_ref.create
                ~server_id:(P.Id.Server.create ())
                ~session_id:before.identity.session_id)
           ~request_sha256:(Chatmd_shell_spec.Source_ref.digest "source-reset-run")
           ~entry:None
         |> protocol_ok
       in
       let original = A.Session_actor.state actor |> protocol_ok in
       let candidate =
         A.Administration.reset
           original
           { keep_history = false
           ; keep_tasks = false
           ; keep_grants = false
           ; keep_labels = true
           ; workspace_instance = None
           }
         |> protocol_ok
       in
       (* A prepared replacement installs its captured new source. The reducer
         must retain the original immutable stop receipt even if the constructor
         candidate omits it; its Created-sensitive hooks remain authoritative. *)
       let candidate =
         { candidate with moderator = Some (snapshot manager_b); managed_stops = [] }
       in
       let delta =
         A.Run_source_change.prepare original ~delta:(Created candidate) ~now:timestamp
         |> protocol_ok
       in
       let prepared =
         match delta with
         | A.Session_delta.Created state -> state
         | _ -> failwith "source retirement hid the replacement shape"
       in
       let prepared_index = Option.value_exn prepared.run_state in
       let prepared_run =
         A.Run_state.find prepared_index receipt.run_id |> Option.value_exn
       in
       A.Session_state.validate prepared |> protocol_ok;
       let original_index = Option.value_exn original.run_state in
       let observer_b =
         Chat_response.Moderator_manager.invocation_observer manager_b |> Option.value_exn
       in
       let removed =
         A.Run_retirement.replace
           original_index
           ~change:Remove
           ~session_revision:(Int64.succ original.counters.revision)
           ~now:timestamp
         |> protocol_ok
       in
       let reinstalled =
         A.Run_retirement.replace
           removed
           ~change:(Replace observer_b)
           ~session_revision:(Int64.succ original.counters.revision)
           ~now:timestamp
         |> protocol_ok
       in
       A.Run_state.validate_transition ~previous:removed reinstalled |> protocol_ok;
       let changed_receipts =
         match A.Run_state.to_jsonaf prepared_index with
         | `Object fields ->
           `Object (List.Assoc.add fields ~equal:String.equal "receipts" (`Array []))
           |> A.Run_state.of_jsonaf
           |> protocol_ok
         | _ -> failwith "run index fixture object"
       in
       let transition =
         A.Session_transition.apply
           ~now:timestamp
           original
           ~delta
           ~payloads:
             [ P.Event.Durable.Payload.Session_updated (A.Session_state.summary prepared)
             ]
         |> protocol_ok
       in
       let replacement =
         List.find_map transition.events ~f:(fun event ->
           P.Event.Durable.replacement_snapshot event |> protocol_ok)
         |> Option.value_exn
       in
       print_s
         [%sexp
           { retired_before_validation =
               (P.Run.Lifecycle.equal prepared_run.lifecycle (Terminal Interrupted)
                : bool)
           ; one_installed_epoch =
               (Int64.equal
                  (A.Run_state.installation prepared_index).epoch
                  (Int64.succ
                     (A.Run_state.installation (Option.value_exn original.run_state))
                       .epoch)
                : bool)
           ; separate_commits_rotate_twice =
               (Int64.equal
                  (A.Run_state.installation reinstalled).epoch
                  Int64.((A.Run_state.installation original_index).epoch + 2L)
                : bool)
           ; skipped_epoch_rejected =
               (Result.is_error
                  (A.Run_state.complete_replacement
                     reinstalled
                     ~previous:original_index
                     ~source:(Some observer_b))
                : bool)
           ; changed_receipt_rejected =
               (Result.is_error
                  (A.Run_state.complete_replacement
                     changed_receipts
                     ~previous:original_index
                     ~source:(Some observer_b))
                : bool)
           ; managed_receipt_retained =
               (List.exists transition.state.managed_stops ~f:(A.Managed_stop.equal stop)
                : bool)
           ; replacement_generation =
               (Int.equal replacement.session.generation candidate.identity.generation
                : bool)
           ; replacement_revision =
               (Int64.equal replacement.revision transition.state.counters.revision
                : bool)
           }]);
  [%expect
    {|
    ((retired_before_validation true) (one_installed_epoch true)
     (separate_commits_rotate_twice true) (skipped_epoch_rejected true)
     (changed_receipt_rejected true) (managed_receipt_retained true)
     (replacement_generation true) (replacement_revision true))
    |}]
;;
