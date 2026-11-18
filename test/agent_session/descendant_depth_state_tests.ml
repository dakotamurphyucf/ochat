open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module GD = A.Goal_descendants
module C = A.Child_contributions
module DS = Agent_store.Delegation_store
module Origin = Agent_store.Delegation_goal_origin
module SD = A.Session_state_document

(* Shared lower native State fixture only. The constructor makes an unpublished
   new session; no old State/ledger is reset and no Factory/writer custody is
   inferred from these validated Reference/origin DATA values. *)
let new_child_state ~(parent : A.Session_state.t) ~scope ~own_reference =
  A.Goal_descendant_pending.validate_branch ~scope ~branch_reference:own_reference
  |> protocol_ok;
  let template =
    actor_state
      ~workspace_instance:parent.spec.workspace_instance
      ~liveness:Detached
      ~start_immediately:false
  in
  let identity =
    { template.identity with session_id = own_reference.DS.Reference.child_session_id }
  in
  let spec =
    { template.spec with
      delegation = Some own_reference
    ; prompt_revision_id = own_reference.revision_id
    ; protocol =
        { template.spec.protocol with prompt = Generated own_reference.revision_id }
    }
  in
  let state = A.Session_state.create ~identity ~spec ~initial_history:[] in
  let state =
    A.Child_accounting.initialize_new
      state
      ~max_children:(GD.Profile.max_branches goal_descendant_profile)
      ~max_encoded_bytes:(GD.Profile.child_max_encoded_bytes goal_descendant_profile)
    |> protocol_ok
  in
  let session =
    P.Session_ref.create
      ~session_id:state.identity.session_id
      ~server_id:(P.Session_ref.server_id (Origin.Scope.owner scope))
  in
  let origin =
    Origin.associated
      ~max_scopes:(GD.Profile.max_scopes goal_descendant_profile)
      [ scope ]
    |> protocol_ok
  in
  let descendants =
    GD.create_new
      ~profile:goal_descendant_profile
      ~session
      ~origin
      ~ledger:state.inference_ledger
    |> protocol_ok
  in
  let state = { state with goal_descendants = Some descendants } in
  A.Session_state.validate state |> protocol_ok;
  state
;;

let reference ~parent_session_id ~parent_generation ~child_session_id ~name =
  DS.reference_of_jsonaf
    (`Object
        [ ( "key"
          , `Object
              [ "parent_session_id", P.Id.Session.to_json parent_session_id
              ; "parent_generation", `Number (Int.to_string parent_generation)
              ; "principal_id", P.Id.Principal.to_json principal_id
              ; "idempotency_key", `String name
              ] )
        ; "child_session_id", P.Id.Session.to_json child_session_id
        ; "revision_id", `String ("prv_" ^ name)
        ; "request_sha256", `String (String.make 64 'a')
        ; "admission_sha256", `String (String.make 64 'b')
        ])
  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  |> protocol_ok
;;

let scope ~goal ~reference ~transaction_id =
  let key = reference.DS.Reference.key in
  let branch =
    Origin.Anchor.create
      ~parent_session_id:key.parent_session_id
      ~parent_generation:key.parent_generation
      ~principal_id:key.principal_id
      ~key:key.idempotency_key
      ~child_session_id:reference.child_session_id
      ~transaction_id
    |> protocol_ok
  in
  Origin.Scope.create
    ~owner:(P.Goal.session goal)
    ~goal_id:(P.Goal.id goal)
    ~source:(P.Goal.source goal)
    ~creator_episode:(P.Goal.episode goal |> Option.value_exn)
    ~branch
  |> protocol_ok
;;

let limits depth =
  D.Limits.create
    ~max_bytes:(D.Limits.max_bytes document_limits)
    ~max_depth:depth
    ~max_fields:(D.Limits.max_fields document_limits)
    ~max_nodes:(D.Limits.max_nodes document_limits)
  |> document_ok
;;

let assert_boundary state ~owner =
  A.Session_state.validate state |> protocol_ok;
  let document = SD.encode (SD.authored state) ~limits:document_limits |> document_ok in
  let gd =
    GD.required_depth (Option.value_exn state.A.Session_state.goal_descendants)
    |> protocol_ok
  in
  let children =
    C.required_depth (Option.value_exn state.child_contributions) |> protocol_ok
  in
  let required =
    match owner with
    | `Descendants ->
      assert (gd > children);
      gd + 2
    | `Children ->
      assert (children > gd);
      children + 2
  in
  let low = limits (required - 1) in
  let high = limits required in
  (* Both raw validation and inspect succeed at d-1: the guard must reject a
     promised future shape, rather than merely rediscover current invalid JSON. *)
  let raw = D.Document.json document in
  D.Json.validate raw ~limits:low |> document_ok;
  let fitting = D.Document.inspect raw ~limits:low |> document_ok in
  assert (Result.is_error (SD.encode (SD.authored state) ~limits:low));
  assert (Result.is_error (SD.decode ~limits:low fitting));
  let admitted = SD.encode (SD.authored state) ~limits:high |> document_ok in
  SD.decode ~limits:high admitted |> document_ok |> ignore;
  assert (D.Json.equal raw (D.Document.json admitted))
;;

let%expect_test
    "State rejects only the future descendant or scoped-child depth beyond current raw"
  =
  Goal_run_tests.with_model
    (fun ~basis:_ ~held:_ ~admission:_ ~intent:_ ~delta:_ parent ->
       let goal = Goal_run_tests.current_goal parent in
       let own_reference =
         reference
           ~parent_session_id:parent.identity.session_id
           ~parent_generation:parent.identity.generation
           ~child_session_id:second_session_id
           ~name:"depth_wire_child"
       in
       let scope =
         scope
           ~goal
           ~reference:own_reference
           ~transaction_id:(P.Id.Transaction.of_string "txn_depth_wire" |> protocol_ok)
       in
       let child = new_child_state ~parent ~scope ~own_reference in
       assert_boundary child ~owner:`Descendants;
       let deeper_reference =
         reference
           ~parent_session_id:child.identity.session_id
           ~parent_generation:child.identity.generation
           ~child_session_id:
             (P.Id.Session.of_string "ses_depth_wire_grandchild" |> protocol_ok)
           ~name:"depth_wire_grandchild"
       in
       let origin =
         Origin.associated
           ~max_scopes:(GD.Profile.max_scopes goal_descendant_profile)
           [ scope ]
         |> protocol_ok
       in
       let children = Option.value_exn child.child_contributions in
       let descendants = Option.value_exn child.goal_descendants in
       let update =
         GD.register_child descendants ~children ~reference:deeper_reference ~origin
         |> protocol_ok
       in
       let child =
         { child with
           goal_descendants = Some (GD.Update.accounting update)
         ; child_contributions =
             Some
               (match GD.Update.children update with
                | Preserve -> children
                | Replace children -> children)
         }
       in
       assert_boundary child ~owner:`Children);
  print_endline
    "current raw fits d-1; actual State encode/decode reserve both GD and C future depth \
     at d";
  [%expect
    {| current raw fits d-1; actual State encode/decode reserve both GD and C future depth at d |}]
;;
