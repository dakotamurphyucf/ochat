open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module Adapter = Agent_session.Standalone_delivery
module Contract = Agent_session.Standalone_completion_contract

let sources reject =
  match reject with
  | false -> Standalone_pending_tests.sources
  | true ->
    ("eventual.json", {|{"type":"string"}|})
    :: List.map Standalone_pending_tests.sources ~f:(fun (name, source) ->
      match name with
      | "agent.chatmd" ->
        ( name
        , String.substr_replace_all
            source
            ~pattern:{|completion_schema="output.json"|}
            ~with_:{|completion_schema="eventual.json"|} )
      | _ -> name, source)
;;

let rewrite_receipt (delivery : P.Delivery.t) receipt =
  match P.Delivery.to_json delivery with
  | `Object fields ->
    `Object
      (List.Assoc.add
         fields
         ~equal:String.equal
         "completion_projection"
         (P.Completion_projection.to_json receipt))
    |> P.Delivery.of_json
    |> protocol_ok
  | _ -> assert false
;;

let artifact_projection
      ~invocation
      ~(job : P.Job.t)
      ~result
      ~current_capabilities
      (delivery : P.Delivery.t)
  =
  let content = P.Completion.to_json result |> Jsonaf.to_string in
  let blob =
    P.Blob.Metadata.create
      ~id:(P.Id.Blob.create ())
      ~kind:File
      ~media_type:P.Job_artifact.media_type
      ~byte_length:(Int64.of_int (String.length content))
      ~digest:Digestif.SHA256.(digest_string content |> to_hex)
      ()
    |> protocol_ok
  in
  let reference =
    P.Job_artifact.create
      ~session_id:job.session_id
      ~job_id:job.id
      ~generation:job.generation
      ~attempt:job.attempt
      ~blob
    |> protocol_ok
  in
  let stored = P.Stored_completion.artifact reference result |> protocol_ok in
  let artifact_job = { job with result = Some (P.Stored_completion.to_json stored) } in
  let projected =
    Contract.project
      ~invocation
      ~job:artifact_job
      ~completion:result
      ~current_capabilities
    |> protocol_ok
  in
  let artifact_delivery =
    P.Delivery.create
      ~disclosure_pins:projected.disclosure_pins
      ~completion_projection:projected.receipt
      { delivery.context with completion = projected.completion }
    |> protocol_ok
  in
  Contract.validate_projection ~invocation ~job:artifact_job artifact_delivery
  |> protocol_ok;
  assert (
    Result.is_error (Contract.validate_projection ~invocation ~job artifact_delivery));
  assert (
    Result.is_error
      (Contract.project
         ~invocation
         ~job:artifact_job
         ~completion:(Succeeded (`String "PRIVATE-FORGED-MATERIALIZATION"))
         ~current_capabilities));
  let changed_blob = { blob with digest = String.make 64 '0' } in
  let changed_reference =
    P.Job_artifact.create
      ~session_id:job.session_id
      ~job_id:job.id
      ~generation:job.generation
      ~attempt:job.attempt
      ~blob:changed_blob
    |> protocol_ok
  in
  let changed_job =
    { artifact_job with
      result =
        Some
          (P.Stored_completion.to_json
             (Artifact
                { outcome = P.Stored_completion.outcome stored
                ; reference = changed_reference
                }))
    }
  in
  assert (
    Result.is_error
      (Contract.validate_projection ~invocation ~job:changed_job artifact_delivery))
;;

let%expect_test
    "standalone adapter retains real results and admits only checked immutable \
     projections"
  =
  List.iter [ false; true ] ~f:(fun reject ->
    with_daemon
      ~sources:(sources reject)
      ~calls:
        [ ( "adapter"
          , "compare_reports_async"
          , Standalone_tests.input "report-a.json" "report-b.json" )
        ]
      ~settle:Job_launch_tests.settle
      ~after_turn:(fun env handle entry ->
        Job_launch_tests.settle env entry;
        let state = A.state entry.actor |> protocol_ok in
        let invocation = model_invocation state "adapter" in
        let job = List.hd_exn state.jobs in
        let result = P.Job.terminal_completion job |> protocol_ok |> Option.value_exn in
        let current_capabilities = Completion_contract_tests.capabilities entry in
        let prepare state =
          Adapter.prepare
            ~state
            ~invocation_id:invocation.context.id
            ~job_id:job.id
            ~completion:result
            ~current_capabilities
            ~delivery_id:(P.Id.Delivery.create ())
            ~now:state.Agent_session.Session_state.identity.updated_at
            ~wake:Request_turn
          |> protocol_ok
        in
        let plan = prepare state in
        let receipt = Option.value_exn plan.delivery.completion_projection in
        [%test_eq: bool] reject receipt.rejected;
        (match reject, plan.delivery.context.completion with
         | false, completion -> assert (P.Completion.equal result completion)
         | true, Failed error ->
           assert (P.Invocation.equal_tool_error error P.Completion_projection.rejection);
           let encoded =
             let id =
               History_entry.Id.create ~namespace:"projection-test" ~sequence:0
               |> Result.ok_or_failwith
             in
             Agent_session.Notification_history.create ~id plan.delivery
             |> protocol_ok
             |> P.History.entry_to_json
             |> Jsonaf.to_string
           in
           assert (not (String.is_substring encoded ~substring:"invocation_count"))
         | _ -> failwith "unexpected adapter projection");
        artifact_projection ~invocation ~job ~result ~current_capabilities plan.delivery;
        assert (
          P.Delivery.equal
            plan.delivery
            (P.Delivery.of_json (P.Delivery.to_json plan.delivery) |> protocol_ok));
        let changed =
          rewrite_receipt
            plan.delivery
            { receipt with job_attempt = receipt.job_attempt + 1 }
        in
        assert (Result.is_error (Contract.validate_projection ~invocation ~job changed));
        assert (
          Result.is_error
            (P.Delivery.validate_transition ~previous:(Some plan.delivery) changed));
        let changed =
          rewrite_receipt
            plan.delivery
            { receipt with result_sha256 = String.make 64 '0' }
        in
        assert (Result.is_error (Contract.validate_projection ~invocation ~job changed));
        let limits = Agent_session.Staged_notifications.default_limits in
        assert (
          Result.is_error
            (Adapter.revalidate
               ~state
               ~staged:[]
               ~limits:{ limits with max_payload_bytes = 1 }
               plan));
        assert (
          Result.is_error
            (Adapter.revalidate ~state ~staged:[ plan.delivery ] ~limits plan));
        (* A DTO, even a correctly checked one, cannot bypass the dedicated actor
           admission path through a generic extension transaction. *)
        assert (
          Result.is_error
            (A.commit_extensions
               entry.actor
               ~generation:state.identity.generation
               ~expected_revision:state.counters.revision
               [ Delivery plan.delivery ]));
        H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
        assert (Result.is_error (A.admit_standalone_delivery entry.actor plan));
        let stopped = A.state entry.actor |> protocol_ok in
        let plan = prepare stopped in
        A.admit_standalone_delivery entry.actor plan |> protocol_ok;
        let saved = A.state entry.actor |> protocol_ok in
        [%test_eq: int] 1 (List.length saved.deliveries);
        [%test_eq: int]
          (List.length stopped.conversation.canonical_history)
          (List.length saved.conversation.canonical_history);
        assert (
          P.Completion.equal
            result
            (P.Job.terminal_completion (List.hd_exn saved.jobs)
             |> protocol_ok
             |> Option.value_exn));
        let restored =
          Agent_session.Session_persistence.restore_snapshot
            (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t saved))
          |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
          |> protocol_ok
        in
        assert (P.Delivery.equal plan.delivery (List.hd_exn restored.deliveries));
        Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
        H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
        let reloaded = A.state entry.actor |> protocol_ok in
        assert (P.Delivery.equal plan.delivery (List.hd_exn reloaded.deliveries));
        assert (Option.is_none reloaded.moderator);
        print_s
          [%sexp
            (reject : bool), "owned result retained; immutable intent only; reload stable"])
      (fun _ -> ()));
  [%expect
    {|
    (false "owned result retained; immutable intent only; reload stable")
    (true "owned result retained; immutable intent only; reload stable")
    |}]
;;
