open! Core
include Support
module D = Document_schema
module F = A.Recurrence_definition
module R = Recurrence_rules
module I = A.Recurrence_identity
module N = A.Recurrence_nominal
module Reservation = A.Recurrence_child_reservation

let capture_owner_call f =
  match f () with
  | result -> Ok result
  | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
;;

let assert_owner_cancelled = function
  | Error (Eio.Cancel.Cancelled _, _) -> ()
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  | Ok _ -> failwith "cancelled in-flight owner call returned ordinarily"
;;

let fresh_target workspace =
  let definition =
    A.Prompt_definition.create
      ~id:(P.Id.Prompt_definition.of_string "prd_reserved_capture" |> protocol_ok)
      ~config_name:"captured-reservation"
      ~root_file:"/unavailable/captured-provenance.chatmd"
      ~root_invocation:Headless
      ~allowed_workspaces:(Option.to_list workspace.A.Workspace_instance.definition_id)
      ~permission_profile:"interactive"
      ~runtime_policy:None
      ~enabled:true
      ~description:None
    |> store_ok
  in
  let artifact =
    Agent_store.Prompt_artifact_store.Artifact.create
      ~revision_id:prompt_revision_id
      ~prompt_definition_id:definition.id
      ~root_chatmd:"<developer>captured reservation</developer>"
      ~sources:[]
      ~parser_schema_version:1
      ~runtime_schema_version:1
      ~created_at:timestamp
      ()
    |> store_ok
    |> Agent_store.Captured_prompt_artifact.create
    |> document_ok
  in
  let parent =
    A.Recurrence_target.Parent.create
      ~workspace
      ~permission_profile:"interactive"
      ~permission_profile_digest:(String.make 64 'a')
      ~runtime_policy:None
      ~configuration_revision:0L
      ~inference_selection:(inference_selection ())
      ~capability_pins:[]
    |> protocol_ok
  in
  A.Recurrence_target.create_fresh ~mode:Single_turn ~artifact ~definition ~parent
  |> protocol_ok
;;

let create_fresh actor name schedule =
  let workspace = (state actor).spec.workspace_instance in
  mutate
    actor
    ("create-" ^ name)
    (Create
       ( id name
       , { schedule
         ; target = fresh_target workspace
         ; policy = A.Recurrence_policy.default
         } ))
  |> ignore;
  mutate actor ("enable-" ^ name) (Enable (id name)) |> ignore
;;

let selected actor name = List.hd_exn (H.ready (frontier actor name))

let authorize_creation state definition =
  let open Result.Let_syntax in
  let%bind () = authorize state in
  match A.Recurrence_target.view (F.target definition) with
  | Fresh_child { artifact; definition; parent; mode = _; source_kind = _ } ->
    assert definition.enabled;
    assert (
      P.Id.Prompt_revision.equal
        (Agent_store.Captured_prompt_artifact.value artifact).revision_id
        prompt_revision_id);
    assert (
      D.Json.equal
        (A.Workspace_instance.to_jsonaf (A.Recurrence_target.Parent.workspace parent))
        (A.Workspace_instance.to_jsonaf state.spec.workspace_instance));
    Ok ()
  | Existing_session _ -> Error (P.Error.invalid_request "fixture target is not captured")
;;

let reserve ?(actor_proof = proof) ?(authorize = authorize_creation) actor name occurrence
  =
  A.Session_actor.reserve_recurrence_child
    actor
    ~actor:actor_proof
    ~authorize
    ~occurrence
    ~expected_control_revision:(E.control_revision (entry actor name))
;;

let row permit = A.Session_actor.Recurrence_creation.reservation permit
let protected actor name = H.protected_children (frontier actor name)

let assert_same_ids a b =
  assert (
    P.Id.Session.equal (Reservation.child_session_id a) (Reservation.child_session_id b));
  assert (
    P.Id.Transaction.equal (Reservation.transaction_id a) (Reservation.transaction_id b));
  assert (String.equal (Reservation.request_sha256 a) (Reservation.request_sha256 b))
;;

let restored_journal fixture =
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
  let restored, last_hash =
    List.fold
      scan.entries
      ~init:(A.Session_persistence.Restored.authored fixture.initial, None)
      ~f:(fun (restored, hash) entry ->
        if not (Int.equal (Agent_store.Frame.flags entry.Agent_store.Journal.frame) 0)
        then restored, hash
        else (
          let transaction =
            Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame)
            |> store_ok
          in
          A.Session_persistence.validate_transaction ~limits:document_limits transaction
          |> store_ok;
          let restored =
            A.Session_persistence.apply_transaction
              ~limits:document_limits
              restored
              transaction
            |> store_ok
          in
          restored, Some (Agent_store.Transaction.hash transaction)))
  in
  journal, restored, last_hash
;;

let reopen_fixture ?(index_projection = index_entry) fixture =
  Agent_store.Commit_writer.close fixture.writer;
  let journal, restored, previous_transaction_hash = restored_journal fixture in
  let initial = A.Session_persistence.Restored.state restored in
  let writer =
    Agent_store.Commit_writer.create
      ~sw:fixture.sw
      ~journal
      ~session_id
      ~next_transaction_sequence:(Int64.succ initial.counters.transaction_sequence)
      ~previous_transaction_hash
      ~queue_capacity:8
    |> store_ok
  in
  let durable =
    A.Session_persistence.create
      ~archive:(fun _ _ -> failwith "unexpected archive")
      ~pending_archive:None
      ~before_commit:
        (Some
           (fun state ->
             S.prepare_canonical_projection
               fixture.store
               fixture.handle
               ~metadata:(metadata state)
               ~entry:(index_projection state)
             |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error))
      ~command_accepted:(fun _ _ -> ())
      ~writer
      ~durability:Flush
      ~limits:document_limits
      ~archive_limits:document_limits
      ~retention_preflight:None
      ~restored
      ~previous_transaction_hash
  in
  { fixture with initial; writer; durable }
;;
