open Core
open Fixtures
module P = Agent_protocol
module State = Agent_session.Session_state
module M = Agent_session.Managed_submission
module Page = Agent_server.Managed_output_page

let field value name = Jsonaf.member_exn name value

let assistant number text =
  let id =
    History_entry.Id.create ~namespace:"page-output" ~sequence:number
    |> Result.ok_or_failwith
  in
  let item =
    Openai.Responses.Item.Input_message
      { role = Assistant
      ; content = [ Text { text; _type = "input_text" } ]
      ; _type = "message"
      }
  in
  History_entry.create_with_id ~id item |> Agent_session.History_codec.to_protocol
;;

let%expect_test
    "bounded output pages retain receipt provenance and expose retention gaps without \
     leaking redacted payloads"
  =
  Delegation_lifecycle_tests.with_fixture
    (fun
        _env
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
       let initial = Agent_session.Session_actor.state actor |> protocol_ok in
       let reference = Agent_store.Delegation_store.reference record in
       let input = Managed_submission_tests.input 20 "work" in
       let first = assistant 1 "first answer" in
       let second = assistant 2 "second answer" in
       let operation : P.Operation.t =
         { id = P.Id.Operation.create ()
         ; generation = 0
         ; kind = Turn User_submit
         ; state = Completed
         ; started_at = timestamp
         ; updated_at = timestamp
         }
       in
       let receipt =
         M.create
           ~reference
           ~key:(Managed_submission_tests.key "page")
           ~request_sha256:(Managed_submission_tests.digest "work")
           ~generation:0
           ~history_id:input.id
           ~now:timestamp
         |> protocol_ok
         |> M.reconcile
              ~generation:0
              ~reference:(Some reference)
              ~discarded:false
              ~adopted:true
              ~appended:[ first; second ]
              ~operation:(Some operation)
              ~terminals:[ operation.id, Completed ]
              ~now:timestamp
       in
       let state : State.t =
         { initial with
           managed_submissions = [ receipt ]
         ; conversation =
             { initial.conversation with
               canonical_history = [ input; first; second ]
             ; initial_prompt_entry_count = 0
             }
         }
       in
       State.validate state |> protocol_ok;
       [%test_eq: string]
         "first answer\nsecond answer"
         (Page.completed_answer ~state ~receipt_id:input.id |> protocol_ok);
       let signer = Agent_server.Managed_output_cursor.create () in
       let read ?(max_bytes = 4096) state receipt_id cursor =
         Page.read
           signer
           ~state
           ~receipt_id
           ~cursor
           ~history_epoch:(Window_start 0L)
           ~limit:1
           ~max_bytes
       in
       let page = read state (Some input.id) None |> protocol_ok in
       let first_record = field page "items" |> Jsonaf.list_exn |> List.hd_exn in
       [%test_eq: string] "output" (field first_record "kind" |> Jsonaf.string_exn);
       let value = field first_record "value" in
       assert (
         Jsonaf.exactly_equal
           (field value "submission_ids")
           (`Array [ P.History.Id.to_json input.id ]));
       assert (
         Jsonaf.exactly_equal
           (field value "operation_ids")
           (`Array [ P.Id.Operation.to_json operation.id ]));
       assert (Jsonaf.exactly_equal (field (field page "receipt") "terminal") `True);
       assert (Jsonaf.exactly_equal (field page "caught_up") `False);
       let cursor = P.Page.Cursor.of_json (field page "next_cursor") |> protocol_ok in
       let next = read state (Some input.id) (Some cursor) |> protocol_ok in
       assert (Jsonaf.exactly_equal (field next "caught_up") `True);
       let repeated = read state (Some input.id) None |> protocol_ok in
       assert (Jsonaf.exactly_equal page repeated);
       let with_example =
         { state with
           conversation =
             { state.conversation with
               canonical_history =
                 assistant 0 "private prompt example"
                 :: state.conversation.canonical_history
             ; initial_prompt_entry_count = 1
             }
         }
       in
       let visible = read with_example None None |> protocol_ok in
       assert (
         not
           (String.is_substring
              (Jsonaf.to_string visible)
              ~substring:"private prompt example"));
       let compacted =
         { state with
           conversation =
             { state.conversation with
               canonical_history = [ input; second ]
             ; compaction_generation = 1
             }
         }
       in
       (match read compacted (Some input.id) None with
        | Error error ->
          [%test_eq: string] "snapshot_required" (P.Error.code_to_string error.code)
        | Ok _ -> failwith "lost output was reported complete");
       let fresh = read compacted None None |> protocol_ok in
       assert (
         Result.is_error (Page.completed_answer ~state:compacted ~receipt_id:input.id));
       assert (Jsonaf.exactly_equal (field fresh "history_compacted") `True);
       [%test_eq: int] 1 (field fresh "items" |> Jsonaf.list_exn |> List.length);
       let changed_role =
         { state with
           conversation =
             { state.conversation with
               canonical_history = [ input; { first with role = User }; second ]
             }
         }
       in
       assert (Result.is_error (read changed_role (Some input.id) None));
       let redacted =
         { state with
           conversation =
             { state.conversation with
               canonical_history = [ input; { first with redacted = true }; second ]
             }
         }
       in
       let hidden = read redacted (Some input.id) None |> protocol_ok in
       assert (
         Result.is_error (Page.completed_answer ~state:redacted ~receipt_id:input.id));
       assert (
         not (String.is_substring (Jsonaf.to_string hidden) ~substring:"first answer"));
       assert (Result.is_error (read ~max_bytes:128 state (Some input.id) None));
       let large =
         { state with
           conversation =
             { state.conversation with
               canonical_history = [ input; assistant 1 (String.make 4000 'x'); second ]
             }
         }
       in
       let partial = read ~max_bytes:2048 large (Some input.id) None |> protocol_ok in
       let cursor = P.Page.Cursor.of_json (field partial "next_cursor") |> protocol_ok in
       let completed =
         read ~max_bytes:65536 large (Some input.id) (Some cursor) |> protocol_ok
       in
       let exact_bytes = String.length (Jsonaf.to_string completed) in
       let exact =
         read ~max_bytes:exact_bytes large (Some input.id) (Some cursor) |> protocol_ok
       in
       assert (Jsonaf.exactly_equal completed exact);
       print_endline
         "full records carry submission/operation provenance; reads do not consume; gaps \
          require snapshots; redacted bytes stay hidden; undersized limits reject");
  [%expect
    {| full records carry submission/operation provenance; reads do not consume; gaps require snapshots; redacted bytes stay hidden; undersized limits reject |}]
;;
