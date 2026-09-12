open Core
open Agent_server_test_support
module P = Agent_protocol
module I = P.Invocation
module G = P.Authoring_guidance
module State = Agent_session.Session_state
module D = Agent_session.Session_delta
module Recovery = Agent_session.Invocation_recovery

let alter invocation name replacement =
  match I.sexp_of_t invocation with
  | Sexp.List fields ->
    let fields =
      List.filter fields ~f:(function
        | Sexp.List (Sexp.Atom key :: _) -> not (String.equal key name)
        | _ -> true)
    in
    I.t_of_sexp (Sexp.List (fields @ Option.to_list replacement))
  | _ -> assert false
;;

let restore state =
  State.sexp_of_t state
  |> Sexp.to_string_mach
  |> Agent_session.Session_persistence.restore_snapshot
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

(* Called with the final state of the actual native-query/fake-provider fixture.
   Recovery receives persisted records only: no query service or runtime factory. *)
let verify (state : State.t) =
  let original = List.hd_exn state.invocations in
  let output_id = Option.value_exn original.output_entry_id in
  let output =
    List.find_exn state.conversation.canonical_history ~f:(fun entry ->
      P.History.Id.equal entry.id output_id)
  in
  let original_guidance =
    match output.provenance with
    | Runtime_authoring value -> value
    | _ -> assert false
  in
  let outcome =
    match original.status with
    | Published outcome -> outcome
    | _ -> assert false
  in
  let unresolved =
    alter original "output_entry_id" None
    |> fun value ->
    alter
      value
      "status"
      (Some (Sexp.List [ Sexp.Atom "status"; I.sexp_of_status (Resolved outcome) ]))
  in
  let plan state =
    Recovery.plan
      ~state
      ~namespace:(P.Id.Session.to_string state.identity.session_id)
      ~first_sequence:
        (Int64.to_int_exn
           (Int64.max
              state.conversation.next_history_sequence
              state.conversation.reserved_history_through))
      ~reason:"reference publication crash fixture"
    |> protocol_ok
  in
  List.iter [ false; true ] ~f:(fun output_already_saved ->
    let history =
      List.filter state.conversation.canonical_history ~f:(fun entry ->
        output_already_saved || not (P.History.Id.equal entry.id output_id))
    in
    let interrupted =
      { state with
        invocations =
          List.map state.invocations ~f:(fun invocation ->
            match P.Id.Invocation.equal invocation.I.context.id original.context.id with
            | true -> unresolved
            | false -> invocation)
      ; conversation =
          { state.conversation with
            canonical_history = history
          ; authoring_reference_index = None
          }
      }
      |> restore
    in
    let first = plan interrupted in
    (match output_already_saved with
     | true -> ()
     | false ->
       let legacy =
         { interrupted with
           conversation = { interrupted.conversation with authoring_publication = None }
         }
       in
       List.iter (plan legacy).appended ~f:(fun entry ->
         assert (P.History.equal_provenance entry.provenance Canonical)));
    let repeated = plan interrupted in
    assert (
      Sexp.equal (D.sexp_of_t (Batch first.deltas)) (D.sexp_of_t (Batch repeated.deltas)));
    assert (
      List.length first.appended
      =
      match output_already_saved with
      | true -> 0
      | false -> 1);
    let recovered =
      D.apply
        interrupted
        (Batch
           [ History_block_reserved (Int64.of_int first.next_sequence)
           ; Batch first.deltas
           ])
      |> protocol_ok
      |> restore
    in
    let invocation =
      List.find_exn recovered.invocations ~f:(fun invocation ->
        P.Id.Invocation.equal invocation.I.context.id original.context.id)
    in
    let recovered_id = Option.value_exn invocation.output_entry_id in
    let recovered_output =
      List.find_exn recovered.conversation.canonical_history ~f:(fun entry ->
        P.History.Id.equal entry.id recovered_id)
    in
    (match recovered_output.provenance with
     | Runtime_authoring value -> assert (G.equal value original_guidance)
     | _ -> failwith "recovery lost verified reference provenance");
    assert (List.is_empty (plan recovered).deltas));
  let change_output replacement =
    { state with
      conversation =
        { state.conversation with
          canonical_history =
            List.map state.conversation.canonical_history ~f:(fun entry ->
              match P.History.Id.equal entry.id output_id with
              | true -> replacement
              | false -> entry)
        ; authoring_reference_index = None
        }
    }
  in
  let forged_guidance =
    G.create_reference
      ~context_identity:original_guidance.context_identity
      ~policy_fingerprint:original_guidance.policy_fingerprint
      ~topics:
        (List.map original_guidance.topics ~f:(fun topic ->
           { topic with document_sha256 = String.make 64 'a' }))
      ~fragments:original_guidance.fragments
      ~payload:output.payload
    |> protocol_ok
  in
  assert (
    Result.is_error
      (State.validate
         (change_output { output with provenance = Runtime_authoring forged_guidance })));
  let call_id = Option.value_exn original.context.call_entry_id in
  let call =
    List.find_exn state.conversation.canonical_history ~f:(fun entry ->
      P.History.Id.equal entry.id call_id)
  in
  let call_guidance =
    G.create_reference
      ~context_identity:original_guidance.context_identity
      ~policy_fingerprint:original_guidance.policy_fingerprint
      ~topics:original_guidance.topics
      ~fragments:original_guidance.fragments
      ~payload:call.payload
    |> protocol_ok
  in
  let forged_call =
    { state with
      conversation =
        { state.conversation with
          canonical_history =
            List.map state.conversation.canonical_history ~f:(fun entry ->
              match P.History.Id.equal entry.id call_id with
              | true -> { entry with provenance = Runtime_authoring call_guidance }
              | false -> entry)
        }
    }
  in
  assert (Result.is_error (State.validate forged_call));
  let reset =
    D.apply state (Reset_generation (state.identity.generation + 1)) |> protocol_ok
  in
  assert (Option.is_none reset.conversation.authoring_publication)
;;
