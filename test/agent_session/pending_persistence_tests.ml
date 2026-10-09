open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module S = Agent_store
module D = Document_schema
module Persistence = A.Session_persistence

let input initial sequence =
  let id =
    History_entry.Id.create ~namespace:(P.Id.Session.to_string session_id) ~sequence
    |> Result.ok_or_failwith
  in
  let entry =
    A.History_codec.user_text ~id "expiry custody" |> A.History_codec.to_protocol
  in
  let input =
    P.Pending_input.create
      ~entry
      ~generation:initial.A.Session_state.identity.generation
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
  let raw =
    match raw with
    | `Object fields ->
      `Object
        (("future-custody", `Object [ "null", `Null; "number", `Number "1.00" ]) :: fields)
    | `Null | `True | `False | `Number _ | `String _ | `Array _ -> assert false
  in
  A.Pending_input_document.of_jsonaf raw ~limits:document_limits |> document_ok
;;

let initial workspace =
  let initial =
    actor_state ~workspace_instance:workspace ~liveness:Detached ~start_immediately:false
  in
  let state =
    { initial with
      lifecycle = { desired = Running; observed = Idle }
    ; conversation =
        { initial.conversation with
          deferred_user_entries = [ input initial 0; input initial 1 ]
        ; next_history_sequence = 8L
        ; reserved_history_through = 8L
        }
    }
  in
  let retention = A.Pending_disposition.Retention.create ~max_records:2 |> protocol_ok in
  let prepared =
    A.Pending_transition.prepare
      state
      ~change:(Adopt { boundary = Idle_start; runtime_admission_open = true })
      ~retention
      ~archive:None
      ~limits:document_limits
    |> protocol_ok
  in
  let state =
    A.Session_delta.apply state (A.Pending_transition.delta prepared) |> protocol_ok
  in
  { state with
    conversation = { state.conversation with deferred_user_entries = [ input state 2 ] }
  }
;;

let prepare state =
  A.Pending_transition.prepare
    state
    ~change:(Adopt { boundary = Idle_start; runtime_admission_open = true })
    ~retention:(A.Pending_disposition.Retention.create ~max_records:1 |> protocol_ok)
    ~archive:None
    ~limits:document_limits
  |> protocol_ok
;;

let%expect_test
    "expiry archive rejection precedes disk journal and exact custody survives replay"
  =
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      let before = initial workspace in
      let storage = Job_artifact_fixtures.create env sw before in
      let handle = storage.session in
      let journal =
        S.Journal.create
          ~env
          ~directory:(S.Session_store.Handle.journal_directory handle)
          ~max_payload_length:1048576
          ~max_segment_bytes:4194304L
          ~max_segment_frames:16
        |> store_ok
      in
      let writer =
        S.Commit_writer.create
          ~sw
          ~journal
          ~session_id
          ~next_transaction_sequence:1L
          ~previous_transaction_hash:None
          ~queue_capacity:8
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () -> S.Commit_writer.close writer)
        ~f:(fun () ->
          let reject = ref true in
          let archives = ref [] in
          let failure =
            P.Error.create
              Persistence_error
              ~message:"expiry archive unavailable"
              ~retryable:false
              ()
          in
          let persistence =
            Persistence.create
              ~pending_archive:
                (Some
                   (fun archive ->
                     if !reject
                     then Error failure
                     else (
                       let%map.Result () =
                         A.Pending_archive.write
                           archive
                           ~env
                           ~handle
                           ~limits:document_limits
                       in
                       archives := archive :: !archives)))
              ~archive:(fun _ _ ->
                failwith "normal adoption must not archive whole history")
              ~before_commit:None
              ~command_accepted:(fun _ _ -> ())
              ~writer
              ~durability:Flush
              ~limits:document_limits
              ~archive_limits:document_limits
              ~retention_preflight:None
              ~restored:(Persistence.Restored.authored before)
              ~previous_transaction_hash:None
          in
          let prepared = prepare before in
          let transition =
            A.Session_transition.apply
              ~now:timestamp
              before
              ~delta:(A.Pending_transition.delta prepared)
              ~payloads:[]
            |> protocol_ok
          in
          let rejected =
            Persistence.commit persistence ~command_audit:None ~previous:before transition
          in
          let kept = Persistence.Restored.state (Persistence.restored persistence) in
          printf
            "archive-rejected=%b journal-empty=%b exact-private-retained=%b \
             pending-kept=%b\n"
            (match rejected with
             | Error actual ->
               P.Error.equal_code actual.code failure.code
               && String.equal actual.message failure.message
             | Ok () -> false)
            (List.is_empty (S.Journal.scan journal |> store_ok).entries)
            (List.equal
               A.Pending_disposition_document.equal
               before.conversation.pending_dispositions
               kept.conversation.pending_dispositions)
            (List.equal
               A.Pending_input_document.equal
               before.conversation.deferred_user_entries
               kept.conversation.deferred_user_entries);
          reject := false;
          Persistence.commit persistence ~command_audit:None ~previous:before transition
          |> protocol_ok;
          let archive = List.hd_exn !archives in
          let path =
            Filename.concat
              (S.Session_store.Handle.archive_directory handle)
              (A.Pending_archive.filename (A.Pending_archive.reference archive))
          in
          let raw = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) in
          let record =
            S.Document_record.decode_file
              ~limits:document_limits
              ~expected_digest:None
              raw
            |> Result.map_error ~f:(fun error ->
              Sexp.to_string_hum (S.Document_record.Error.sexp_of_t error))
            |> Result.ok_or_failwith
          in
          let exact_archive =
            String.equal
              (D.Document.to_string (A.Pending_archive.document archive))
              (D.Document.to_string (S.Document_record.document record))
          in
          let reopened =
            S.Journal.open_existing
              ~env
              ~directory:(S.Session_store.Handle.journal_directory handle)
              ~max_payload_length:1048576
              ~max_segment_bytes:4194304L
              ~max_segment_frames:16
            |> store_ok
          in
          let scan = S.Journal.scan reopened |> store_ok in
          let restarted =
            List.fold
              scan.entries
              ~init:(A.Session_state_document.authored before)
              ~f:(fun document entry ->
                let record =
                  S.Document_record.of_frame
                    entry.frame
                    ~limits:document_limits
                    ~expected_digest:None
                  |> Result.map_error ~f:(fun error ->
                    Sexp.to_string_hum (S.Document_record.Error.sexp_of_t error))
                  |> Result.ok_or_failwith
                in
                let transaction =
                  S.Transaction.decode_record record ~limits:document_limits |> store_ok
                in
                Persistence.apply_document document ~limits:document_limits transaction
                |> store_ok)
          in
          let restarted = A.Session_state_document.value restarted in
          let original_records =
            A.Pending_plan.expired_dispositions (A.Pending_transition.plan prepared)
          in
          let archived_records =
            match
              D.Json.field
                (D.Document.payload (S.Document_record.document record))
                ~name:"records"
            with
            | Value (`Array records) ->
              List.map records ~f:(fun raw ->
                A.Pending_disposition_document.of_jsonaf raw ~limits:document_limits
                |> document_ok)
            | Absent | Null | Value _ -> assert false
          in
          printf
            "archive-exact=%b evicted-custody-exact=%b retained=%d queue=%d canonical=%d \
             journal-records=%d future-lexemes=%b\n"
            exact_archive
            (List.equal
               A.Pending_disposition_document.equal
               original_records
               archived_records)
            (List.length restarted.conversation.pending_dispositions)
            (List.length restarted.conversation.deferred_user_entries)
            (List.length restarted.conversation.canonical_history)
            (List.length scan.entries)
            (String.is_substring
               (D.Document.to_string (S.Document_record.document record))
               ~substring:"1.00");
          [%expect
            {|archive-rejected=true journal-empty=true exact-private-retained=true pending-kept=true
archive-exact=true evicted-custody-exact=true retained=1 queue=0 canonical=3 journal-records=1 future-lexemes=true|}])))
;;

let%expect_test "expiry reference requires exact bounded removed custody" =
  with_actor_workspace (fun _ workspace ->
    let state = initial workspace in
    let prepared = prepare state in
    let delta = A.Pending_transition.delta prepared in
    let mutation, archive, reference =
      match delta with
      | Pending_inputs_changed (mutation, archive, Some reference) ->
        mutation, archive, reference
      | _ -> assert false
    in
    let raw = A.Pending_archive.Reference.to_jsonaf reference in
    let altered =
      match raw with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             ( name
             , if String.equal name "sha256" then `String (String.make 64 '0') else value
             )))
      | `Null | `True | `False | `Number _ | `String _ | `Array _ -> assert false
    in
    let forged = A.Pending_archive.Reference.of_jsonaf altered |> protocol_ok in
    let check delta =
      A.Pending_archive_transition.collect state ~delta ~limits:document_limits
    in
    printf
      "matching-admitted=%b missing-rejected=%b forged-rejected=%b\n"
      (Result.is_ok (check delta))
      (Result.is_error (check (Pending_inputs_changed (mutation, archive, None))))
      (Result.is_error (check (Pending_inputs_changed (mutation, archive, Some forged))));
    [%expect {|matching-admitted=true missing-rejected=true forged-rejected=true|}])
;;
