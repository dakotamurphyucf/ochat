open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Goal = Goal_service_tests
module Direct = Descendant_turn_adapter_tests
module G = P.Goal
module GD = A.Goal_descendants
module D = Agent_store.Delegation_store
module O = Agent_store.Delegation_goal_origin
module S = A.Child_usage_snapshot

let within env phase f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. f with
  | Eio.Time.Timeout -> failwith ("554 composed ancestry timed out: " ^ phase)
;;

let intermediate_source =
  {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>554 actual intermediate owns its native grandchild creation.</developer>
<tool name="agent_create" type="inherited"/>|}
;;

let leaf_source marker =
  sprintf
    {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>%s</developer>|}
    marker
;;

let grandchild_source = leaf_source "554 genuine grandchild foreground provider."
let sibling_source = leaf_source "554 competing sibling must not reach provider."
let grandchild_message = "554 grandchild-only canonical response"

type member =
  { entry : Registry.entry
  ; reference : D.Reference.t
  ; record : D.record
  ; scope : O.Scope.t
  }

type t =
  { env : Eio_unix.Stdenv.base
  ; sw : Eio.Switch.t
  ; daemon : Daemon.t
  ; root : Registry.entry
  ; intermediate : member
  ; grandchild : member
  ; sibling : member
  ; root_admission : P.Run_receipt.t
  ; counters : Retry_support.counters
  ; failure : (exn * Stdlib.Printexc.raw_backtrace) option ref
  }

let raise_failure t =
  Option.iter !(t.failure) ~f:(fun (exn, backtrace) ->
    Exn.raise_with_original_backtrace exn backtrace)
;;

let snapshot parent reference =
  A.Child_contributions.snapshot
    (Option.value_exn parent.A.Session_state.child_contributions)
    ~reference
  |> protocol_ok
  |> Option.value_exn
;;

let assert_coverage coverage =
  assert (not (S.Coverage.historical_children_unknown coverage));
  [%test_eq: int64] 0L (S.Coverage.unobserved_children coverage)
;;

let root_descendants current =
  let goal = Goal.goal current in
  let descendants =
    A.Child_contributions.aggregate_root
      (Option.value_exn current.A.Session_state.child_contributions)
      ~owner:(G.session goal)
      ~goal_id:Goal.goal_id
    |> protocol_ok
  in
  assert_coverage (S.Descendants.coverage descendants);
  descendants
;;

let root_marks current =
  S.Partition.marks (S.Descendants.partition (root_descendants current))
;;

let assert_no_local_goal current =
  assert (Option.is_none current.A.Session_state.goal_catalog);
  assert (Option.is_none current.goal_accounts);
  assert (List.is_empty current.jobs);
  assert (List.is_empty current.schedules);
  assert (List.is_empty current.moderator_executions);
  assert (Option.is_none current.moderator)
;;

let with_writer entry f =
  let writer, _ =
    Actor.attach entry.Registry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Actor.detach entry.actor writer.id |> protocol_ok)
    (fun () -> f writer)
;;

(* This is the genuine no-moderator Factory path already used by the real
   inherited Attempt fixture. It installs no substitute worker or Manager. *)
let start_legacy daemon entry =
  Factory.prepare_session_start (Daemon.factory daemon) entry |> protocol_ok;
  Owner.ensure_loaded_authenticated entry.Registry.runtime ~actor:original |> protocol_ok;
  with_writer entry (fun writer ->
    Actor.start entry.actor ~actor:original ~attachment_id:writer.id
    |> protocol_ok
    |> ignore);
  let current =
    Actor.with_moderator_checkpoint entry.actor (fun () -> Actor.state entry.actor)
    |> protocol_ok
  in
  assert_no_local_goal current;
  assert (Option.is_none current.active_operation);
  assert (Option.is_none current.failure);
  assert (not current.halted);
  assert (P.Session.equal_desired_state current.lifecycle.desired Running);
  (match current.lifecycle.observed with
   | Idle -> ()
   | Stopped
   | Queued_for_slot
   | Starting
   | Recovering
   | Running_turn _
   | Compacting _
   | Waiting_for_permission _
   | Stopping
   | Failed _ -> failwith "actual legacy source did not reach Idle");
  match current.runtime_initialization with
  | Ready -> ()
  | Pending _ -> failwith "actual legacy source initialization is pending"
;;

let message entry text =
  let id = A.History_id_source.allocate entry.Registry.history_ids |> protocol_ok in
  Owner.parse_user_content entry.runtime ~id { kind = Plain_text; text; attachments = [] }
  |> protocol_ok
  |> A.History_codec.to_protocol
;;

let published entry call_id =
  let current = state entry in
  let invocation =
    List.find current.invocations ~f:(fun invocation ->
      String.equal invocation.P.Invocation.context.tool_name "agent_create"
      && Option.exists invocation.context.provider_call_id ~f:(String.equal call_id))
  in
  match invocation with
  | Some { status = Published (Complete value); _ } -> Some value
  | Some { status = Resolved (Fail failure) | Published (Fail failure); _ } ->
    raise_s [%sexp "real tree birth failed", (failure : P.Invocation.tool_error)]
  | Some
      { status =
          ( Admitted | Dispatching
          | Resolved (Complete _ | Pending _ | Cancelled _)
          | Published (Pending _ | Cancelled _) )
      ; _
      }
  | None -> None
;;

let member daemon parent value =
  let id =
    Jsonaf.member_exn "session_id" value
    |> Jsonaf.string_exn
    |> P.Id.Session.of_string
    |> protocol_ok
  in
  let entry = Registry.find (Daemon.registry daemon) id |> Option.value_exn in
  let born = state entry in
  assert_no_local_goal born;
  assert (not (Owner.is_loaded entry.runtime));
  assert (Option.is_none born.active_operation);
  let reference = Option.value_exn born.spec.delegation in
  let record =
    D.resolve (Agent_store.Session_store.delegations (Daemon.store daemon)) reference
    |> Existing.store_ok
  in
  assert (D.Reference.equal reference (D.reference record));
  assert (D.equal_stage record.stage Linked);
  assert (Option.is_none record.revocation);
  assert (D.Admission.equal_lifetime record.admission.lifetime Owned);
  assert (
    P.Id.Session.equal record.key.parent_session_id (state parent).identity.session_id);
  assert (P.Id.Session.equal record.admission.child_session_id born.identity.session_id);
  assert (P.Id.Prompt_revision.equal reference.revision_id born.spec.prompt_revision_id);
  let origin = Option.value_exn record.admission.goal_origin in
  let scope =
    match O.scopes origin with
    | [ scope ] -> scope
    | [] | _ :: _ :: _ -> failwith "real tree member did not retain exactly one Goal root"
  in
  assert (
    Option.exists (GD.origin (Option.value_exn born.goal_descendants)) ~f:(O.equal origin));
  let observed = snapshot (state parent) reference in
  assert (D.Reference.equal reference (S.reference observed));
  assert (S.Partition.equal S.Partition.empty (S.own observed));
  assert (
    S.Partition.equal S.Partition.empty (S.Descendants.partition (S.descendants observed)));
  assert_coverage (S.coverage observed);
  { entry; reference; record; scope }
;;

let assert_source env daemon member ~text ~records =
  let current = state member.entry in
  let artifacts =
    Agent_store.Prompt_artifact_store.open_existing
      ~env
      ~root:
        (Agent_store.Data_root.prompt_artifacts_path
           (Agent_store.Session_store.data_root (Daemon.store daemon)))
    |> Existing.store_ok
  in
  let artifact =
    Agent_store.Prompt_artifact_store.load artifacts current.spec.prompt_revision_id
    |> Existing.store_ok
  in
  assert (P.Id.Prompt_revision.equal artifact.revision_id member.reference.revision_id);
  [%test_eq: string] text artifact.root_chatmd;
  [%test_eq: string] member.record.admission.manifest_sha256 artifact.manifest_sha256;
  Owner.with_loaded_runtime member.entry.runtime (fun runtime ->
    Option.value_exn runtime.A.Runtime_builder.check_execution () |> protocol_ok;
    let ancestry = Option.value_exn runtime.check_ancestry () |> protocol_ok in
    assert (
      List.equal
        D.equal_record
        records
        (A.Delegation_authority.Checked_ancestry.records ancestry));
    assert (not (A.Prompt_source_lease.is_legacy runtime.source_lease));
    (match A.Prompt_source_lease.availability runtime.source_lease with
     | Live -> ()
     | Missing_receipt | Unavailable _ | Revoked | Closed ->
       failwith "tree source lease unavailable");
    let native = Option.value_exn runtime.native_runtime in
    let capabilities =
      Lazy.force native.capabilities
      |> Result.map_error ~f:(fun failure ->
        failure.Chat_response.Tool_capability.message)
      |> Result.ok_or_failwith
    in
    Chat_response.Background_request.rebind_capabilities
      ~pins:member.record.admission.capability_pins
      ~capabilities
    |> protocol_ok
    |> ignore;
    Ok ())
  |> protocol_ok
;;

module Provider_phase = struct
  type t =
    | Root_intermediate
    | Root_sibling
    | Root_finish
    | Intermediate_create
    | Intermediate_finish
    | Grandchild
end

let with_tree ~wrap_env ~before_provider f =
  let provider_phase = ref Provider_phase.Root_intermediate in
  let tree = ref None in
  let failure = ref None in
  let phase = ref "actual root Goal and two native children" in
  with_recoverable_fixture
    ~timeout:90.
    ~wrap_env
    ~prompt_text:(Deferred_source_support.prompt_text ^ "<tool name=\"agent_create\"/>")
    ~post_stream:(fun inputs ->
      match !provider_phase with
      | Root_intermediate ->
        provider_phase := Root_sibling;
        Birth.create_response
          ~key:"554-tree-intermediate"
          ~call_id:"554-tree-intermediate"
          ~tools:[ "agent_create" ]
          [ "child.chatmd", intermediate_source ]
      | Root_sibling ->
        provider_phase := Root_finish;
        Birth.create_response
          ~key:"554-tree-sibling"
          ~call_id:"554-tree-sibling"
          [ "child.chatmd", sibling_source ]
      | Root_finish ->
        provider_phase := Intermediate_create;
        completed_response "554 real Goal root finished both native births"
      | Intermediate_create ->
        provider_phase := Intermediate_finish;
        Birth.create_response
          ~key:"554-tree-grandchild"
          ~call_id:"554-tree-grandchild"
          [ "child.chatmd", grandchild_source ]
      | Intermediate_finish ->
        provider_phase := Grandchild;
        completed_response "554 actual intermediate completed native grandchild birth"
      | Grandchild ->
        let current = Option.value_exn !tree in
        (try before_provider current inputs with
         | exn ->
           let backtrace = Stdlib.Printexc.get_raw_backtrace () in
           failure := Some (exn, backtrace);
           Exn.raise_with_original_backtrace exn backtrace);
        completed_response "554 real grandchild provider completed once")
    (fun env sw _root daemon _policy root _runtime _revision counters ->
       try
         let session, before = Goal.create_goal daemon root in
         let request =
           A.Run_admission_request.Headless.create
             ~session_id:before.identity.session_id
             ~generation:before.identity.generation
             ~expected_revision:before.counters.revision
             ~mode:Workflow
             ~input:
               (User_submission
                  { kind = Plain_text
                  ; text = "554 create both native tree branches"
                  ; attachments = []
                  })
             ~key:(P.Idempotency_key.of_string "554-tree-root" |> protocol_ok)
           |> protocol_ok
           |> fun request ->
           A.Goal_activation_request.create
             ~goal_id:Goal.goal_id
             ~expected_goal_revision:(G.revision (Goal.goal before))
             ~expected_catalog_revision:
               (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
             ~request
           |> protocol_ok
         in
         let service =
           Agent_server.Goal_run_service.create
             ~factory:(Daemon.factory daemon)
             ~authorize:(fun _ current -> authorize current)
         in
         let root_admission =
           Agent_server.Goal_run_service.start_episode
             service
             ~entry:root
             ~actor:original
             ~session
             ~request
           |> protocol_ok
           |> Goal.activated
         in
         let rec await_roots () =
           match
             published root "554-tree-intermediate", published root "554-tree-sibling"
           with
           | Some intermediate, Some sibling
             when Option.is_none (state root).active_operation -> intermediate, sibling
           | (Some _ | None), (Some _ | None) ->
             Eio.Fiber.yield ();
             await_roots ()
         in
         let intermediate, sibling = within env "both genuine root births" await_roots in
         let intermediate = member daemon root intermediate in
         let sibling = member daemon root sibling in
         assert (not (D.Reference.equal intermediate.reference sibling.reference));
         assert (not (O.Scope.equal intermediate.scope sibling.scope));
         List.iter [ intermediate; sibling ] ~f:(fun member ->
           assert (P.Session_ref.equal session (O.Scope.owner member.scope));
           assert (P.Id.Goal.equal Goal.goal_id (O.Scope.goal_id member.scope));
           assert (
             P.Run_receipt.equal
               root_admission
               (P.Goal_episode.admission (O.Scope.creator_episode member.scope))));
         phase
         := "actual unbound intermediate creates grandchild through private ancestry";
         start_legacy daemon intermediate.entry;
         with_writer intermediate.entry (fun writer ->
           let message = message intermediate.entry "554 create the genuine grandchild" in
           let submitted =
             Actor.submit_message_authenticated
               intermediate.entry.actor
               ~actor:original
               ~attachment_id:writer.id
               message
             |> protocol_ok
           in
           assert (Option.is_some submitted.operation_id);
           within env "actual intermediate native completion" (fun () ->
             Goal_native_test_support.await_quiet intermediate.entry)
           |> ignore);
         let value =
           published intermediate.entry "554-tree-grandchild" |> Option.value_exn
         in
         let grandchild = member daemon intermediate.entry value in
         assert (O.Scope.equal intermediate.scope grandchild.scope);
         assert (
           not
             (P.Id.Session.equal
                grandchild.record.key.parent_session_id
                (state root).identity.session_id));
         start_legacy daemon grandchild.entry;
         start_legacy daemon sibling.entry;
         assert_source
           env
           daemon
           intermediate
           ~text:intermediate_source
           ~records:[ intermediate.record ];
         assert_source env daemon sibling ~text:sibling_source ~records:[ sibling.record ];
         assert_source
           env
           daemon
           grandchild
           ~text:grandchild_source
           ~records:[ grandchild.record; intermediate.record ];
         (* Root's prior confirmation can cover an earlier Running snapshot of I.
            Establish the real terminal I-own partition before measuring G's
            disjoint descendant increment; no count or execution grant is minted. *)
         Factory.import_child_usage
           (Daemon.factory daemon)
           ~parent:root
           ~child:intermediate.entry
           ~actor:original
           ~authorize_parent:authorize
           ~authorize_child:authorize
         |> protocol_ok
         |> ignore;
         let actual_intermediate =
           A.Child_accounting.snapshot (state intermediate.entry)
           |> protocol_ok
           |> Option.value_exn
         in
         assert (
           S.Partition.equal
             (S.own actual_intermediate)
             (S.own (snapshot (state root) intermediate.reference)));
         let current =
           { env
           ; sw
           ; daemon
           ; root
           ; intermediate
           ; grandchild
           ; sibling
           ; root_admission
           ; counters
           ; failure
           }
         in
         tree := Some current;
         phase := "actual cross-branch count competition";
         f current;
         raise_failure current
       with
       | exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         (try Eio.traceln "554 composed ancestry failed at %s" !phase with
          | _ -> ());
         Exn.raise_with_original_backtrace exn backtrace)
;;
