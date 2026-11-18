open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module L = A.Inference_ledger
module Own = A.Delegated_usage
module SD = A.Session_state_document
module Delta = A.Session_delta

let ledger result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (L.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

(* Protocol association DATA only. This fixture executes the actual canonical
   transition/document writers; it does not mint a Factory/Actor observation. *)
let reference =
  Agent_store.Delegation_store.reference_of_jsonaf
    (`Object
        [ ( "key"
          , `Object
              [ "parent_session_id", `String "ses_delegated_usage_parent"
              ; "parent_generation", `Number "0"
              ; "principal_id", P.Id.Principal.to_json principal_id
              ; "idempotency_key", `String "delegated-writer"
              ] )
        ; "child_session_id", P.Id.Session.to_json session_id
        ; "revision_id", P.Id.Prompt_revision.to_json prompt_revision_id
        ; "request_sha256", `String (String.make 64 'a')
        ; "admission_sha256", `String (String.make 64 'b')
        ])
  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  |> protocol_ok
;;

let initial workspace_instance =
  let state =
    actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
  in
  let limits =
    L.Limits.create
      ~max_attempts:1
      ~max_turns:1
      ~max_retained_bytes:(4 * 1024 * 1024)
      ~document_limits
    |> ledger
  in
  let inference_ledger =
    L.create ~session_id ~generation:0 ~before_tracking_unknown:false ~limits |> ledger
  in
  { state with
    inference_ledger
  ; spec =
      { state.spec with
        delegation = Some reference
      ; prompt_definition_id = None
      ; protocol =
          { state.spec.protocol with
            prompt = Generated prompt_revision_id
          ; persistence = Durable
          }
      }
  }
  |> fun state ->
  A.Child_accounting.initialize_new state ~max_children:2 ~max_encoded_bytes:(1024 * 1024)
  |> protocol_ok
;;

let operation () =
  P.Operation.
    { id = P.Id.Operation.create ()
    ; generation = 0
    ; kind = Turn User_submit
    ; state = Running
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let transition state delta payloads =
  A.Session_transition.apply ~now:timestamp state ~delta ~payloads |> protocol_ok
;;

let admit_document previous transition =
  A.Session_document_transition.admit_encoded
    previous
    ~delta:transition.A.Session_transition.delta
    ~next:transition.state
    ~limits:document_limits
;;

let%expect_test "terminal old host turn and new host start keep exact ordered own folds" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial = initial workspace_instance in
    assert (
      Result.is_error
        (A.Session_state.upgrade_schema { initial with schema_version = 25 }));
    let old = operation () in
    let running =
      transition
        initial
        (Batch
           [ Active_operation_changed (Some old)
           ; Lifecycle_changed { desired = Running; observed = Running_turn old.id }
           ])
        [ Operation_started old ]
    in
    assert (
      Option.is_some
        (L.find_turn_handle
           running.state.inference_ledger
           ~operation_id:old.id
           ~generation:0));
    let before =
      SD.authored running.state
      |> fun document ->
      SD.encode document ~limits:document_limits
      |> document_ok
      |> SD.decode ~limits:document_limits
      |> document_ok
    in
    let next = operation () in
    let completed = { old with state = Completed } in
    let final =
      transition
        running.state
        (Batch
           [ Active_operation_changed (Some next)
           ; Lifecycle_changed { desired = Running; observed = Running_turn next.id }
           ])
        [ Operation_completed completed; Operation_started next ]
    in
    assert (
      Int64.equal
        final.state.counters.revision
        (Int64.succ running.state.counters.revision));
    assert (
      Option.is_none
        (L.find_turn_handle
           final.state.inference_ledger
           ~operation_id:old.id
           ~generation:0));
    let own = Option.value_exn final.state.delegated_usage in
    Own.validate own ~ledger:final.state.inference_ledger |> protocol_ok;
    let marks =
      A.Canonical_usage_fold.Marks.of_ledger final.state.inference_ledger
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (A.Canonical_usage_fold.Error.sexp_of_t error))
      |> Result.ok_or_failwith
    in
    assert (Int64.equal marks.turn 2L);
    assert (
      Int64.equal
        (P.Inference_query.Summary.coverage (L.summary final.state.inference_ledger))
          .retired_turns
        1L);
    let rec flatten = function
      | Delta.Batch changes -> List.concat_map changes ~f:flatten
      | leaf -> [ leaf ]
    in
    let leaves = flatten final.delta in
    let canonical =
      List.filter leaves ~f:(function
        | Delegated_inference_ledger_changed _ -> true
        | _ -> false)
    in
    assert (Int.equal (List.length canonical) 2);
    let admitted = admit_document before final |> document_ok in
    let restored =
      SD.Admitted.document admitted
      |> fun document ->
      SD.decode ~limits:document_limits document |> document_ok |> SD.value
    in
    assert (
      Int64.equal
        (L.revision restored.inference_ledger)
        (L.revision final.state.inference_ledger));
    Own.validate
      (Option.value_exn restored.delegated_usage)
      ~ledger:restored.inference_ledger
    |> protocol_ok;
    let ordinary =
      List.map leaves ~f:(function
        | Delegated_inference_ledger_changed (ledger, _) ->
          Delta.Inference_ledger_changed ledger
        | leaf -> leaf)
    in
    assert (Result.is_error (admit_document before { final with delta = Batch ordinary }));
    let first_pair =
      List.findi leaves ~f:(fun _ leaf ->
        match leaf with
        | Delegated_inference_ledger_changed _ -> true
        | _ -> false)
      |> Option.value_exn
      |> fst
    in
    let omitted =
      List.filteri leaves ~f:(fun index _ -> not (Int.equal index first_pair))
    in
    assert (Result.is_error (admit_document before { final with delta = Batch omitted }));
    let created =
      { running.state with
        active_operation = Some next
      ; lifecycle = { desired = Running; observed = Running_turn next.id }
      }
    in
    let replaced =
      transition
        running.state
        (Created created)
        [ Operation_completed completed; Operation_started next ]
    in
    let replaced_document = admit_document before replaced |> document_ok in
    let replaced_state =
      SD.Admitted.document replaced_document
      |> SD.decode ~limits:document_limits
      |> document_ok
      |> SD.value
    in
    Own.validate
      (Option.value_exn replaced_state.delegated_usage)
      ~ledger:replaced_state.inference_ledger
    |> protocol_ok;
    assert (
      Option.is_none
        (L.find_turn_handle
           replaced_state.inference_ledger
           ~operation_id:old.id
           ~generation:0));
    assert (
      Option.is_some
        (L.find_turn_handle
           replaced_state.inference_ledger
           ~operation_id:next.id
           ~generation:0));
    let dropped_own =
      List.map (flatten replaced.delta) ~f:(function
        | Delta.Created state -> Delta.Created { state with delegated_usage = None }
        | leaf -> leaf)
    in
    assert (
      Result.is_error (admit_document before { replaced with delta = Batch dropped_own }));
    let dropped_inventory =
      List.map (flatten replaced.delta) ~f:(function
        | Delta.Created state -> Delta.Created { state with child_contributions = None }
        | leaf -> leaf)
    in
    assert (
      Result.is_error
        (admit_document before { replaced with delta = Batch dropped_inventory }));
    print_endline
      "one actual host transaction: finish then retire; both paired edges survive \
       replay/Created; omitted/ordinary/dropped partitions reject");
  [%expect
    {| one actual host transaction: finish then retire; both paired edges survive replay/Created; omitted/ordinary/dropped partitions reject |}]
;;
