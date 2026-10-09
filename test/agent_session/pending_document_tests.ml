open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema

let object_add json name value =
  match json with
  | `Object fields -> `Object ((name, value) :: fields)
  | `Null | `True | `False | `Number _ | `String _ | `Array _ -> assert false
;;

let%expect_test
    "whole state adoption restart transfers exact entry and private wrapper custody"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let entry =
      A.History_codec.user_text ~id:history_id "restart custody"
      |> A.History_codec.to_protocol
    in
    let input =
      P.Pending_input.create
        ~entry
        ~generation:initial.identity.generation
        ~binding:Agent_protocol.Pending_input.Binding.safe_boundary
      |> protocol_ok
      |> A.Pending_input_document.authored
           ~owner:(Submitting_principal principal_id)
           ~limits:document_limits
      |> document_ok
    in
    let raw =
      A.Pending_input_document.to_jsonaf input ~limits:document_limits |> document_ok
    in
    let marker =
      `Object [ "nested", `Object [ "null", `Null; "number", `Number "1.00" ] ]
    in
    let raw =
      match raw with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             if String.equal name "entry"
             then name, object_add value "future-entry" marker
             else name, value))
      | `Null | `True | `False | `Number _ | `String _ | `Array _ -> assert false
    in
    let raw = object_add raw "future-wrapper" marker in
    let input =
      A.Pending_input_document.of_jsonaf raw ~limits:document_limits |> document_ok
    in
    let state =
      { initial with
        lifecycle = { desired = Running; observed = Idle }
      ; conversation =
          { initial.conversation with
            deferred_user_entries = [ input ]
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          }
      }
    in
    let encoded =
      A.Session_state_document.authored state
      |> fun document ->
      A.Session_state_document.encode document ~limits:document_limits |> document_ok
    in
    let admitted =
      A.Session_state_document.decode ~limits:document_limits encoded |> document_ok
    in
    let state = A.Session_state_document.value admitted in
    let retention =
      A.Pending_disposition.Retention.create ~max_records:2 |> protocol_ok
    in
    let prepared =
      A.Pending_transition.prepare
        state
        ~change:(Adopt { boundary = Idle_start; runtime_admission_open = true })
        ~retention
        ~archive:None
        ~limits:document_limits
      |> protocol_ok
    in
    let delta = A.Pending_transition.delta prepared in
    let next = A.Session_delta.apply state delta |> protocol_ok in
    let transferred =
      A.Session_document_transition.admit admitted ~delta ~next ~limits:document_limits
      |> document_ok
    in
    let durable =
      A.Session_state_document.encode transferred ~limits:document_limits |> document_ok
    in
    let shared =
      A.Session_document_transition.admit_encoded
        admitted
        ~delta
        ~next
        ~limits:document_limits
      |> document_ok
      |> A.Session_state_document.Admitted.document
    in
    [%test_eq: string] (D.Document.to_string durable) (D.Document.to_string shared);
    let restarted =
      A.Session_state_document.decode ~limits:document_limits shared |> document_ok
    in
    let rewritten =
      A.Session_state_document.encode restarted ~limits:document_limits |> document_ok
    in
    let state = A.Session_state_document.value restarted in
    let disposition = List.hd_exn state.conversation.pending_dispositions in
    let payload = D.Document.payload rewritten in
    let exact = function
      | D.Json.Value value ->
        String.equal (Jsonaf.to_string marker) (Jsonaf.to_string value)
      | Absent | Null -> false
    in
    let conversation =
      match D.Json.field payload ~name:"conversation" with
      | Value value -> value
      | Absent | Null -> assert false
    in
    let canonical =
      match D.Json.field conversation ~name:"canonical_history" with
      | Value (`Array [ entry ]) -> entry
      | Absent | Null | Value _ -> assert false
    in
    let private_record =
      A.Pending_disposition_document.to_jsonaf disposition ~limits:document_limits
      |> document_ok
    in
    let custody =
      match D.Json.field private_record ~name:"custody" with
      | Value value -> value
      | Absent | Null -> assert false
    in
    printf
      "canonical-extension=%b wrapper-extension=%b owner=%b queue=%d canonical=%d \
       pending-revision=%Ld content-revision=%Ld\n"
      (exact (D.Json.field canonical ~name:"future-entry"))
      (exact (D.Json.field custody ~name:"future-wrapper"))
      (A.Pending_input_document.Owner.equal
         (Submitting_principal principal_id)
         (A.Pending_disposition_document.owner disposition))
      (List.length state.conversation.deferred_user_entries)
      (List.length state.conversation.canonical_history)
      (P.Pending_input.Revision.to_int64 state.conversation.pending_revision)
      (P.History.Content_revision.to_int64
         (List.hd_exn state.conversation.canonical_history).content_revision);
    printf
      "exact-repeat=%b whole-history-archive=%b\n"
      (String.equal (D.Document.to_string durable) (D.Document.to_string rewritten))
      (Option.is_some (A.Pending_transition.expiry_archive prepared));
    [%expect
      {|canonical-extension=true wrapper-extension=true owner=true queue=0 canonical=1 pending-revision=1 content-revision=0
exact-repeat=true whole-history-archive=false|}])
;;

let%expect_test "ordered state comparison retains exact private pending metadata" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let entry =
      A.History_codec.user_text ~id:history_id "semantic comparison"
      |> A.History_codec.to_protocol
    in
    let input =
      P.Pending_input.create
        ~entry
        ~generation:initial.identity.generation
        ~binding:P.Pending_input.Binding.safe_boundary
      |> protocol_ok
      |> A.Pending_input_document.authored
           ~owner:(Submitting_principal principal_id)
           ~limits:document_limits
      |> document_ok
    in
    let raw =
      A.Pending_input_document.to_jsonaf input ~limits:document_limits |> document_ok
    in
    let carrier number =
      object_add
        raw
        "future-wrapper"
        (`Object [ "nested", `Object [ "null", `Null; "number", `Number number ] ])
      |> fun raw ->
      A.Pending_input_document.of_jsonaf raw ~limits:document_limits |> document_ok
    in
    let state input =
      { initial with
        lifecycle = { desired = Running; observed = Idle }
      ; conversation =
          { initial.conversation with
            deferred_user_entries = [ input ]
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          }
      }
    in
    let previous = state (carrier "1.00") in
    let equal_copy = state (carrier "1.00") in
    let different_raw = state (carrier "1.0") in
    List.iter [ previous; equal_copy; different_raw ] ~f:(fun state ->
      A.Session_state.validate state |> protocol_ok);
    let same =
      A.Session_state_document.equal_values previous ~limits:document_limits equal_copy
      |> document_ok
    in
    let exact =
      not
        (A.Session_state_document.equal_values
           previous
           ~limits:document_limits
           different_raw
         |> document_ok)
    in
    let delta = A.Session_delta.Stop_epoch_changed 1L in
    let expected = A.Session_delta.apply previous delta |> protocol_ok in
    let candidate = A.Session_delta.apply different_raw delta |> protocol_ok in
    let admitted =
      Result.is_ok
        (A.Session_document_transition.admit
           (A.Session_state_document.authored previous)
           ~delta
           ~next:expected
           ~limits:document_limits)
    in
    let raw_mismatch_rejected =
      Result.is_error
        (A.Session_document_transition.admit
           (A.Session_state_document.authored previous)
           ~delta
           ~next:candidate
           ~limits:document_limits)
    in
    let invalid_candidate_rejected =
      let invalid =
        { expected with counters = { expected.counters with revision = -1L } }
      in
      Result.is_error
        (A.Session_document_transition.admit_encoded
           (A.Session_state_document.authored previous)
           ~delta
           ~next:invalid
           ~limits:document_limits)
    in
    print_s
      [%sexp
        { same : bool
        ; exact : bool
        ; admitted : bool
        ; raw_mismatch_rejected : bool
        ; invalid_candidate_rejected : bool
        }]);
  [%expect
    {|
    ((same true) (exact true) (admitted true) (raw_mismatch_rejected true)
     (invalid_candidate_rejected true))
    |}]
;;
