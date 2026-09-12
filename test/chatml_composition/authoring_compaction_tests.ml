open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module G = P.Authoring_guidance
module State = Agent_session.Session_state
module Q = Authoring_context_tests

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

type stage =
  | Initial_prepare
  | Initial_example
  | Await_compaction
  | Refreshed_example
  | Validate
  | Execute
  | Finished
[@@deriving equal, sexp]

let response inputs id =
  let json =
    List.find_map_exn inputs ~f:(function
      | Openai.Responses.Item.Function_call_output { call_id; output = Text text; _ }
        when String.equal id call_id -> Some (Jsonaf.of_string text)
      | _ -> None)
  in
  match P.Invocation.outcome_of_json json with
  | Ok (Complete (`String text)) -> Jsonaf.of_string text
  | Ok outcome -> raise_s [%sexp "unexpected query outcome", (outcome : I.outcome)]
  | Error _ -> json
;;

let content pages =
  List.concat_map pages ~f:Q.items
  |> List.filter_map ~f:(fun item ->
    Jsonaf.member "text" item |> Option.bind ~f:Jsonaf.string)
  |> String.concat ~sep:"\n"
;;

let coordinator pages =
  let text =
    List.concat_map pages ~f:Q.items
    |> List.filter_map ~f:(fun item ->
      match Jsonaf.member "topic_id" item with
      | Some (`String "runtime.jobs.shell-example") ->
        Jsonaf.member "text" item |> Option.bind ~f:Jsonaf.string
      | _ -> None)
    |> String.concat ~sep:"\n"
  in
  let _, reversed, finished =
    List.fold
      (String.split_lines text)
      ~init:(false, [], false)
      ~f:(fun (inside, lines, finished) line ->
        match inside, finished, line with
        | false, false, "```ocaml" -> true, lines, false
        | true, false, "```" -> false, lines, true
        | true, false, _ -> true, line :: lines, false
        | _ -> inside, lines, finished)
  in
  assert finished;
  String.concat ~sep:"\n" (List.rev reversed)
;;

let pointers state =
  List.filter_map state.State.conversation.canonical_history ~f:(fun entry ->
    match entry.P.History.provenance with
    | Runtime_authoring value when G.equal_purpose value.purpose Rediscovery -> Some entry
    | _ -> None)
;;

let wait_idle env entry =
  Background_shell_tests.wait env (fun () ->
    let state = A.state entry.Agent_server.Session_registry.actor |> protocol_ok in
    Option.iter state.failure ~f:(fun error -> raise_s [%sexp (error : P.Error.t)]);
    Option.is_none state.active_operation
    && List.is_empty state.conversation.deferred_user_entries)
;;

let%expect_test
    "X10 real background retrieval survives compaction through fresh query and authoring"
  =
  (* Isolated expect-test process: always use the real compactor's offline branch,
     even when the developer's shell has provider credentials. Never print them. *)
  let previous_key = Sys.getenv "OPENAI_API_KEY" in
  Core_unix.unsetenv "OPENAI_API_KEY";
  Exn.protect
    ~finally:(fun () ->
      match previous_key with
      | None -> Core_unix.unsetenv "OPENAI_API_KEY"
      | Some data -> Core_unix.putenv ~key:"OPENAI_API_KEY" ~data)
    ~f:(fun () ->
      let stage = ref Initial_prepare in
      let request_number = ref 0 in
      let initial_count = ref Int.max_value
      and final_count = ref Int.max_value in
      let pending = ref "background-package" in
      let inputs = ref [] in
      let pages = ref [] in
      let first_source = ref ""
      and fresh_source = ref "" in
      let first_receipts = ref [] in
      let partial_pages = ref 0 in
      let queried = ref [] in
      let query number request =
        let id = "reference-" ^ Int.to_string number in
        pending := id;
        queried := id :: !queried;
        [ id, "ochat_authoring_context", request ]
      in
      let example =
        Q.request
          ~task:"background_workflow"
          ~topic_id:"runtime.jobs.shell-example"
          ~max_tokens:6000
          "topic"
      in
      with_daemon
        ~sources:
          [ ( "agent.chatmd"
            , {|<developer>Author a background coordinator using the installed reference.</developer>
<authoring_context policy="manual"/>
<tool name="ochat_authoring_context"/><tool name="ochat_validate"/><tool name="run_chatml"/>
<script id="work" language="chatml" kind="tool">let run ctx input = Task.pure(`Complete(input))</script>
<tool name="fixture_work" type="chatml" script="work" entrypoint="run" input_schema="any.json" output_schema="any.json"/>|}
            )
          ; "any.json", {|{"type":"object","properties":{},"additionalProperties":false}|}
          ]
        ~calls:
          [ ( !pending
            , "ochat_authoring_context"
            , Q.request ~task:"background_workflow" ~max_tokens:1500 "prepare" )
          ]
        ~request_counts:(fun () -> !initial_count, !final_count)
        ~inspect_request:(fun number actual ->
          assert (number < 200);
          request_number := number;
          inputs := actual)
        ~followup_calls:(fun number ->
          match !stage with
          | Initial_prepare | Initial_example | Refreshed_example ->
            let page = response !inputs !pending in
            assert (not (Q.has_error page));
            pages := !pages @ [ page ];
            (match Q.field page "next_cursor" with
             | `String cursor ->
               incr partial_pages;
               assert (not (Jsonaf.bool_exn (Q.field page "complete")));
               let minimum = Q.field (Q.field page "budget") "minimum_next_tokens" in
               let budget =
                 match minimum with
                 | `Number n -> Int.max 6000 (Int.of_string n)
                 | `Null -> 6000
                 | _ -> assert false
               in
               query number (Q.request ~cursor ~max_tokens:budget "continue")
             | `Null ->
               assert (Jsonaf.bool_exn (Q.field page "complete"));
               (match !stage with
                | Initial_prepare ->
                  let text = content !pages in
                  List.iter
                    [ "background_job_completed"; "Pending"; "Internal_event" ]
                    ~f:(fun contract ->
                      assert (String.is_substring text ~substring:contract));
                  stage := Initial_example;
                  pages := [];
                  query number example
                | Initial_example ->
                  first_source := coordinator !pages;
                  stage := Await_compaction;
                  initial_count := number;
                  []
                | Refreshed_example ->
                  fresh_source := coordinator !pages;
                  assert (String.equal !first_source !fresh_source);
                  stage := Validate;
                  [ ( "validate-coordinator"
                    , "ochat_validate"
                    , `Object
                        [ "version", `Number "1"
                        ; "target", `String "moderator"
                        ; "source", `String !fresh_source
                        ; "tools", `Array [ `String "fixture_work" ]
                        ] )
                  ]
                | _ -> assert false)
             | _ -> assert false)
          | Await_compaction ->
            let text =
              `Array (List.map !inputs ~f:Openai.Responses.Item.jsonaf_of_t)
              |> Jsonaf.to_string
            in
            assert (String.is_substring text ~substring:"[Ochat authoring rediscovery]");
            assert (String.is_substring text ~substring:"runtime.jobs.shell-example");
            assert (not (String.is_substring text ~substring:"authoring.primer"));
            assert (
              not (String.is_substring text ~substring:"let on_event ctx state event"));
            pages := [];
            stage := Refreshed_example;
            query number example
          | Validate ->
            let report = response !inputs "validate-coordinator" in
            assert (Jsonaf.bool_exn (Q.field report "valid"));
            let prefix =
              String.substr_index_exn
                !fresh_source
                ~pattern:"let on_event ctx state event"
              |> String.prefix !fresh_source
            in
            let source =
              prefix
              ^ {|let main input =
  match field(input, "kind") with
  | `String("background_job_completed") ->
    let* terminal = completion(field(input, "result")) in
    (match terminal with
    | `Succeeded(value) -> Task.pure(value)
    | `Failed(error) -> Task.fail(error.message)
    | `Cancelled(reason) -> Task.fail(reason)
    | `Expired -> Task.fail("expired"))
  | _ -> Task.fail("unexpected event")|}
            in
            stage := Execute;
            [ ( "use-contract"
              , "run_chatml"
              , `Object
                  [ "source", `String source
                  ; "tools", `Array []
                  ; ( "input"
                    , `Object
                        [ "kind", `String "background_job_completed"
                        ; ( "result"
                          , `Object
                              [ "type", `String "succeeded"
                              ; "value", `String "fresh-contract-executed"
                              ] )
                        ] )
                  ] )
            ]
          | Execute ->
            let output =
              List.find_map_exn !inputs ~f:(function
                | Openai.Responses.Item.Function_call_output
                    { call_id = "use-contract"; output = Text text; _ } -> Some text
                | _ -> None)
            in
            assert (String.is_substring output ~substring:"fresh-contract-executed");
            stage := Finished;
            final_count := number;
            []
          | Finished -> failwith "unexpected turn after authoring finished")
        ~after_turn:(fun env handle entry ->
          assert (equal_stage !stage Await_compaction);
          let before = A.state entry.actor |> protocol_ok in
          assert (List.is_empty (pointers before));
          first_receipts
          := State.authoring_references before
             |> protocol_ok
             |> Chat_response.Authoring_reference_index.receipts;
          assert (not (List.is_empty !first_receipts));
          H.compact handle ~expected_revision:(Some before.counters.revision)
          |> protocol_ok
          |> ignore;
          wait_idle env entry;
          assert (!request_number = !initial_count);
          let compacted = A.state entry.actor |> protocol_ok in
          assert (
            compacted.conversation.compaction_generation
            = before.conversation.compaction_generation + 1);
          assert (not (List.is_empty compacted.conversation.compaction_archives));
          List.iter compacted.conversation.canonical_history ~f:(fun entry ->
            match entry.P.History.provenance with
            | Runtime_authoring _ -> failwith "compaction retained reference contents"
            | _ -> ());
          let restored =
            State.sexp_of_t compacted
            |> Sexp.to_string_mach
            |> Agent_session.Session_persistence.restore_snapshot
            |> store_ok
          in
          let remembered =
            State.authoring_references restored
            |> protocol_ok
            |> Chat_response.Authoring_reference_index.receipts
          in
          assert (
            List.equal
              Chat_response.Authoring_presence.equal_receipt
              !first_receipts
              remembered);
          H.send_message
            handle
            { kind = Plain_text
            ; text = "Continue authoring the coordinator's completion handling."
            ; attachments = []
            }
          |> protocol_ok
          |> ignore;
          wait_idle env entry)
        ~settle:(fun env entry -> wait_idle env entry)
        (fun state ->
           assert (equal_stage !stage Finished);
           assert (!partial_pages > 2);
           assert (List.is_empty state.jobs);
           assert (List.is_empty state.deliveries);
           assert (Option.is_none state.moderator);
           (match result state "use-contract" with
            | Complete (`String "fresh-contract-executed") -> ()
            | other -> raise_s [%sexp (other : I.outcome)]);
           assert (not (List.is_empty (pointers state)));
           let records =
             State.authoring_references state
             |> protocol_ok
             |> Chat_response.Authoring_reference_index.receipts
           in
           assert (
             List.exists records ~f:(fun receipt ->
               not
                 (List.exists !first_receipts ~f:(fun old ->
                    P.History.Id.equal
                      receipt.Chat_response.Authoring_presence.entry_id
                      old.entry_id))));
           List.iter !queried ~f:(fun id ->
             assert (Option.is_some (model_invocation state id).authoring_reference));
           print_endline
             "paginated background package -> persisted compaction -> manual pointer -> \
              fresh retrieval -> moderator validation -> executed completion parser"));
  [%expect
    {| paginated background package -> persisted compaction -> manual pointer -> fresh retrieval -> moderator validation -> executed completion parser |}]
;;
