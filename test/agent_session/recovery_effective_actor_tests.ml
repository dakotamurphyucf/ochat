open! Core
open Fixtures
open Recovery_window_actor_tests
module Effective_capture = A.Recovery_effective_capture
module Effective_ledger = A.Recovery_effective_ledger
module Effective_record = A.Recovery_effective_record
module Runtime = Inference_runtime

exception Stop_before_attempt

let effective_records (state : State.t) =
  Option.value_map state.recovery_effective ~default:[] ~f:Effective_ledger.records
;;

let set_effective_replacement actor (source : P.History.entry) ~change ~text =
  let snapshot = handoff_snapshot 0 in
  let value = A.History_codec.user_text ~id:source.id text |> History_entry.payload in
  let replacement =
    Session.Moderator_state.Identity_snapshot.Replacement.
      { target_id = source.id; change_id = change; value; script_label = None }
  in
  let snapshot =
    { snapshot with
      revision = change + 1
    ; next_change_id = change + 1
    ; replacements = [ replacement ]
    }
  in
  Actor.change_moderator
    actor
    (Some (A.Runtime_builder.encode_moderator_snapshot snapshot))
  |> protocol_ok
  |> ignore;
  snapshot
;;

let%test_unit "actual root preparation retains only exact neutral sources and replays" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let post_stream ~sw:_ ~inputs:_ = failwith "preparation reached the provider" in
      let prepared_models = ref [] in
      let on_prepare request =
        if not (List.is_empty (Inference.Request.history request))
        then
          prepared_models
          := Inference.Request.Target.model (Inference.Request.target request)
             :: !prepared_models
      in
      let runtime_fixture =
        Inference_fixture.create_with_observer
          ~on_prepare
          ~namespace:"recovery-preparation"
          ~default_model:"model"
          ~post_stream
      in
      let runtime_ok result =
        result
        |> Result.map_error ~f:(fun e ->
          Sexp.to_string_hum (Runtime.Preparation_error.sexp_of_t e))
        |> Result.ok_or_failwith
      in
      let selected =
        Inference_fixture.capture_config runtime_fixture Chat_response.Config.default
        |> runtime_ok
      in
      let previous = Inference_fixture.resolve runtime_fixture selected |> runtime_ok in
      let inference_ok result =
        result
        |> Result.map_error ~f:(fun e ->
          Sexp.to_string_hum (Inference.Request.Error.sexp_of_t e))
        |> Result.ok_or_failwith
      in
      let selection =
        Inference.Selection.captured selected ~limits:document_limits |> inference_ok
      in
      let state_before =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let state_before =
        { state_before with
          spec = { state_before.spec with inference_target = selection }
        }
      in
      let storage = Job_artifact_fixtures.create env sw state_before in
      let owner =
        P.Session_ref.create
          ~server_id:(Store.Session_store.server_id storage.sessions)
          ~session_id
      in
      let fixture : fixture =
        { env; sessions = storage.sessions; handle = storage.session; owner }
      in
      let initial = Persistence.Restored.authored state_before in
      let active_journal = journal fixture Store.Journal.create in
      let archive =
        Archive.write_document
          ~env
          ~handle:fixture.handle
          ~max_payload_length:payload_limit
      in
      let after =
        Eio.Switch.run (fun sw ->
          with_actor
            fixture
            sw
            active_journal
            ~restored:initial
            ~previous_transaction_hash:None
            ~archive
            (fun actor _ ->
               let writer = attach actor in
               let policy =
                 A.Configuration_policy.
                   { select_profile =
                       (fun ~current:_ ~profile:_ ->
                         Error (P.Error.invalid_request "fixture has one target"))
                   ; approve = (fun ~current:_ ~proposed:_ -> Ok ())
                   ; resolve =
                       (fun target ->
                         Inference_fixture.resolve runtime_fixture target
                         |> Result.map_error ~f:(fun _ ->
                           P.Error.invalid_request "unavailable fixture target"))
                   }
               in
               Actor.set_configuration_policy actor policy |> protocol_ok;
               let canonical = history 50 "original canonical body" in
               let synthetic = history 51 "synthetic body must not survive" in
               let snapshot = ref None in
               let mismatch = ref false in
               let invalid_target = ref false in
               let cutover = ref false in
               let new_model = "model-after-cutover" in
               let switch_model () =
                 let module C = P.Session_configuration in
                 let request =
                   C.Update_request.
                     { session_id
                     ; attachment_id = writer
                     ; expected_generation = 0
                     ; expected_revision = 0L
                     ; patch =
                         C.Patch.create ~model:new_model ~settings:[] () |> protocol_ok
                     ; idempotency_key =
                         P.Idempotency_key.of_string "recovery-cutover" |> protocol_ok
                     }
                 in
                 let changed = Actor.update_configuration actor request |> protocol_ok in
                 assert changed.pending
               in
               let repeats = ref 1 in
               let allocated = ref 0 in
               let identity =
                 { (Inference_fixture.identity runtime_fixture) with
                   with_attempt =
                     (fun _ ~relation:_ ~f:_ ->
                       incr allocated;
                       raise Stop_before_attempt)
                 }
               in
               let worker =
                 A.Operation_worker.create ~run:(fun ~sw ~input caps ->
                   let history =
                     Option.value_map !snapshot ~default:input.history ~f:(fun snapshot ->
                       Chat_response.Moderator_manager.effective_entries_of_snapshot
                         snapshot
                         input.history
                       |> Result.map_error ~f:(fun _ -> "invalid moderator fixture")
                       |> Result.ok_or_failwith
                       |> List.map ~f:(fun row ->
                         row.Chat_response.Moderation.Effective_entry.entry))
                   in
                   let history =
                     if !mismatch
                     then
                       (A.History_codec.of_protocol synthetic |> protocol_ok)
                       :: List.map history ~f:(fun entry ->
                         if P.History.Id.equal (History_entry.id entry) canonical.id
                         then
                           A.History_codec.user_text
                             ~id:canonical.id
                             "changed bytes under the same occurrence"
                         else entry)
                     else history
                   in
                   let root = Option.value_exn caps.root_context in
                   for _ = 1 to !repeats do
                     try
                       root.with_context
                         ~previous
                         ~history
                         (fun context ~on_prepared ~on_dispatch:_ ->
                            let target = Runtime.Context.target context in
                            if !cutover
                            then (
                              cutover := false;
                              switch_model ();
                              assert (
                                String.equal
                                  (Inference.Request.Target.model target)
                                  "model"));
                            let target =
                              if !invalid_target
                              then
                                Inference.Request.Target.with_model
                                  target
                                  ~model:"different"
                                  ~limits:document_limits
                                |> inference_ok
                              else target
                            in
                            let request =
                              Inference.Request.create
                                ~target
                                ~history
                                ~tools:[]
                                ~assets:[]
                                ~limits:Transcript.Admission.default
                              |> inference_ok
                            in
                            match
                              Inference_client.run
                                context
                                ~sw
                                ~identity
                                ~relation:Root
                                ~request
                                ~before_dispatch:on_prepared
                                ~on_attempt:(fun _ -> assert false)
                                ~on_completion:(fun _ -> assert false)
                                ~on_event:ignore
                                ~on_observation:ignore
                            with
                            | Error (Preparation Target_mismatch) when !invalid_target ->
                              ()
                            | Ok _ | Error _ -> assert false)
                     with
                     | Stop_before_attempt -> ()
                   done;
                   Completed
                     { final_history = input.history
                     ; runtime_requests = []
                     ; moderator_snapshot = None
                     })
               in
               Actor.set_operation_worker actor (Some worker) |> protocol_ok;
               Actor.start actor ~attachment_id:writer |> protocol_ok |> ignore;
               Actor.append_history actor ~attachment_id:writer [ canonical ]
               |> protocol_ok
               |> ignore;
               let turn sequence =
                 Actor.submit_message
                   ~submitting_principal:principal_id
                   actor
                   ~attachment_id:writer
                   (history sequence "turn request")
                 |> protocol_ok
                 |> ignore;
                 Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                   await_idle actor)
               in
               let before = turn 52 in
               assert (Option.is_none before.recovery_effective);
               assert (Int.equal !allocated 1);
               let enabled =
                 Actor.enable_recovery_windows
                   actor
                   ~actor:(actor_proof ())
                   ~attachment_id:writer
                   ~authorize:(fun _ -> Ok ())
                 |> protocol_ok
               in
               let overlay_text = "moderator exact projection body" in
               snapshot
               := Some
                    (set_effective_replacement
                       actor
                       canonical
                       ~change:0
                       ~text:overlay_text);
               cutover := true;
               let prepared = turn 53 in
               [%test_eq: string list] [ "model"; "model" ] (List.rev !prepared_models);
               let first = List.hd_exn (effective_records prepared) in
               assert (Scope.equal enabled (Effective_record.scope first));
               let selected_member =
                 List.find_exn (Effective_record.members first) ~f:(fun member ->
                   P.History.Id.equal
                     canonical.id
                     (Citation.occurrence (Effective_record.Member.citation member)))
               in
               let citation = Effective_record.Member.citation selected_member in
               (match Citation.version citation with
                | Effective { origin = Replaced { change_id = 0; _ }; _ } -> ()
                | _ -> assert false);
               let resolved state =
                 Effective_capture.resolve_live
                   state
                   ~source_revision:(Effective_record.source_revision first)
                   citation
                 |> protocol_ok
               in
               let selected = resolved prepared |> Option.value_exn in
               assert (
                 String.is_substring
                   (Jsonaf.to_string selected.payload)
                   ~substring:overlay_text);
               assert (
                 P.History.equal_provenance
                   selected.provenance
                   (Moderator_replaced canonical.id));
               assert (
                 not
                   (String.is_substring
                      (Jsonaf.to_string (Effective_record.to_jsonaf first))
                      ~substring:overlay_text));
               mismatch := true;
               let mismatched_state = turn 54 in
               let records = effective_records mismatched_state in
               [%test_eq: int] 2 (List.length records);
               let latest = List.last_exn records in
               let members = Effective_record.members latest in
               let omitted id =
                 not
                   (List.exists members ~f:(fun member ->
                      P.History.Id.equal
                        id
                        (Citation.occurrence (Effective_record.Member.citation member))))
               in
               assert (omitted canonical.id && omitted synthetic.id);
               assert (Effective_record.input_count latest > List.length members);
               assert (
                 List.for_all members ~f:(fun m ->
                   Effective_record.Member.request_ordinal m > 0));
               assert (Option.is_some (resolved mismatched_state));
               snapshot
               := Some
                    (set_effective_replacement
                       actor
                       canonical
                       ~change:1
                       ~text:"new version");
               invalid_target := true;
               let after = turn 55 in
               [%test_eq: int] 2 (List.length (effective_records after));
               assert (Option.is_none (resolved after));
               [%test_eq: string list]
                 [ "model"; "model"; new_model ]
                 (List.rev !prepared_models);
               [%test_eq: int] 3 !allocated;
               invalid_target := false;
               repeats := 64;
               let saturated = turn 56 in
               [%test_eq: int] 67 (List.length !prepared_models);
               [%test_eq: int] 67 !allocated;
               let records = effective_records saturated in
               [%test_eq: int] 64 (List.length records);
               assert (
                 Effective_record.to_jsonaf (List.hd_exn records)
                 |> D.Json.equal (Effective_record.to_jsonaf first));
               saturated))
      in
      let replay_journal = journal fixture Store.Journal.open_existing in
      let scan = Store.Journal.scan replay_journal |> store_ok in
      let prepared_deltas =
        List.filter_map scan.entries ~f:(fun entry ->
          match Store.Frame.flags entry.frame with
          | 0 ->
            Some (Store.Transaction.decode (Store.Frame.payload entry.frame) |> store_ok)
          | _ -> None)
        |> List.filter ~f:(fun transaction ->
          List.mem
            (delta_kinds transaction)
            "recovery_effective_prepared"
            ~equal:String.equal)
      in
      [%test_eq: int] 64 (List.length prepared_deltas);
      assert (
        List.for_all prepared_deltas ~f:(fun transaction ->
          List.mem
            (D.Document.required_semantics transaction.delta)
            P.Session_storage_semantics.recovery_effective
            ~equal:String.equal));
      let recovered =
        Store.Recovery.load
          ~env
          ~journal:replay_journal
          ~snapshot_directory:
            (Store.Session_store.Handle.snapshot_directory fixture.handle)
          ~max_snapshot_payload_length:payload_limit
          ~session_id
          ~initial
          ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
          ~apply:(Persistence.apply_transaction ~limits:document_limits)
          ~validate_transaction:(Persistence.validate_transaction ~limits:document_limits)
          ~validate:Persistence.validate
        |> store_ok
      in
      let shelf (state : State.t) =
        Option.value_exn state.recovery_effective |> Effective_ledger.to_jsonaf
      in
      let replayed = Persistence.Restored.state recovered.state in
      assert (D.Json.equal (shelf after) (shelf replayed));
      let first = List.hd_exn (effective_records replayed) in
      let members = Effective_record.members first in
      assert (not (List.is_empty members));
      let source_id = (history 50 "").id in
      let resolves member =
        Effective_capture.resolve_live
          replayed
          ~source_revision:(Effective_record.source_revision first)
          (Effective_record.Member.citation member)
        |> protocol_ok
        |> Option.is_some
      in
      let target =
        List.find_exn members ~f:(fun member ->
          P.History.Id.equal
            source_id
            (Citation.occurrence (Effective_record.Member.citation member)))
      in
      assert (not (resolves target));
      assert (List.exists members ~f:resolves)))
;;

let%test_unit
    "effective coordinate projection defers unrelated canonical payload decoding"
  =
  let module Projection = A.Recovery_effective_projection in
  let oversized =
    { (history 60 "") with payload = `String (String.make (100 * 1024) 'x') }
  in
  let visible = history 61 "eligible exact neutral row" in
  let create history =
    Projection.create ~source_revision:1L ~overlay_incarnation:0L ~history ~moderator:None
    |> protocol_ok
  in
  let projection = create [ oversized; visible ] |> Option.value_exn in
  let row id = Projection.find projection id |> Option.value_exn in
  let no_version, remaining_bytes =
    Projection.Row.version
      (row oversized.id)
      ~remaining_bytes:Runtime.Recovery_input.max_projection_bytes
  in
  assert (Option.is_none no_version);
  let version, _ = Projection.Row.version (row visible.id) ~remaining_bytes in
  assert (Option.is_some version);
  assert (
    Option.is_none
      (create
         (List.init (A.Recovery_window_record.max_members + 1) ~f:(fun _ -> visible))))
;;
