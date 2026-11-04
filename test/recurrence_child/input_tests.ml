open! Core
open Support
module Owner = Agent_server.Runtime_owner
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Input = A.Runtime_input_preparation
module Accounting = A.Inference_ledger
module Observation = Inference.Observation
module Res = Openai.Responses

module Cold_cut = struct
  type t =
    | Adopt
    | Revoke_after_lexical_exit
    | Root_sync_failure
  [@@deriving sexp_of]
end

module Cut = struct
  type t =
    | Complete
    | Revoke_between_agents
    | Stop_after_first
    | Handoff
    | Cold of Cold_cut.t
  [@@deriving sexp_of]
end

let prompt =
  {|<developer>Actual loaded recurrence input owner.</developer>
<tool name="input_a" agent="input-a.chatmd" local/>
<tool name="input_b" agent="input-b.chatmd" local/>
<script id="controller" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let cold_prompt =
  {|<developer>Actual cold adopted recurrence input owner.</developer>
<tool name="input_a" agent="input-a.chatmd" local/>
<tool name="input_b" agent="input-b.chatmd" local/>
<tool name="post_scope_recipe" agent="input-post.chatmd" local/>
<tool name="append_to_file"/>
<script id="controller" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = match event with
| `Pre_tool_call(call) ->
  if call.name == "append_to_file" then
    let* model = Model.call("agent_prompt_v1", `Object([
      {key = "prompt"; value = `String("input-post.chatmd")},
      {key = "input"; value = `String("535_POST_SCOPE")},
      {key = "is_local"; value = `Bool(true)}
    ])) in
    (match model with
    | `Ok(value) -> Task.pure(state + 1)
    | `Refused(message) -> Task.fail(message)
    | `Error(message) -> Task.fail(message))
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

let native_response () =
  let open Res.Response_stream in
  let arguments =
    {|{"path":"post-scope.txt","content":"535 native after lexical close"}|}
  in
  let item arguments status =
    Item.Function_call
      { name = "append_to_file"
      ; arguments
      ; call_id = "535-post-scope-call"
      ; _type = "function_call"
      ; id = Some "535-post-scope-tool"
      ; status
      }
  in
  [ Output_item_added
      { item = item "" None; output_index = 0; type_ = "response.output_item.added" }
  ; Function_call_arguments_done
      { arguments
      ; item_id = "535-post-scope-tool"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item = item arguments (Some "completed")
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

(* Observe actual written bytes before sync, just as the child association crash
   fixture does. DATA identifies the selected root writer; it grants no authority. *)
let written_root env path target =
  let scan =
    Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
    |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
    |> store_ok
  in
  match List.last scan.entries with
  | Some entry when Int.equal (Agent_store.Frame.flags entry.frame) 0 ->
    let transaction =
      Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame) |> store_ok
    in
    if P.Id.Session.equal target transaction.session_id
    then
      A.Session_persistence.durable_events ~limits:D.Limits.default transaction
      |> store_ok
      |> List.find_map ~f:(fun event ->
        match
          P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
        with
        | Operation_started operation ->
          (match operation.kind with
           | Turn User_submit -> Some operation.id
           | Turn _ | Compaction -> None)
        | _ -> None)
    else None
  | Some _ | None -> None
;;

let content : P.Session.Message_content.t =
  { kind = Chatmd
  ; text =
      {|<user>before <agent src="input-a.chatmd" local><user>533_INPUT_A</user></agent> between <agent src="input-b.chatmd" local><user>533_INPUT_B</user></agent> after</user>|}
  ; attachments = []
  }
;;

let assert_no_root (state : A.Session_state.t) =
  assert (Option.is_none state.active_operation);
  assert (List.is_empty state.jobs)
;;

let require_completed_fact row =
  let handle = Accounting.Row.handle row in
  let record = Accounting.Row.record row in
  assert (
    Transcript.Scope.equal
      (Accounting.Handle.scope handle)
      (Observation.Attempt_record.scope record));
  assert (
    Observation.Observation_id.equal
      (Accounting.Handle.accounting_id handle)
      (Observation.Attempt_record.accounting_id record));
  match Observation.Attempt_record.state record with
  | Terminal terminal ->
    (match Inference.Event.Terminal.outcome terminal with
     | Completed -> ()
     | (Refused | Incomplete _ | Failed _) as outcome ->
       raise_s [%sexp (outcome : Inference.Event.Terminal.outcome)])
  | Prepared | Running | Interrupted _ -> failwith "actual input attempt did not complete"
;;

let require_completed row =
  let handle = Accounting.Row.handle row in
  assert (Option.is_none (Accounting.Handle.operation_id handle));
  assert (Option.is_none (Accounting.Handle.invocation_id handle));
  require_completed_fact row
;;

(* The controller and target are separate actual registered Factory sessions.
   Only the target's continuously owned preparation supplies its original. *)
let with_loaded_held daemon controller target ~original f =
  let before = state target in
  let source = A.Recurrence_source.installed before |> protocol_ok in
  let session =
    P.Session_ref.create
      ~server_id:(S.server_id (Daemon.store daemon))
      ~session_id:before.identity.session_id
  in
  let selected =
    Target.create_existing
      ~session
      ~source
      ~configuration_revision:before.spec.configuration_revision
      ~input:content
    |> protocol_ok
  in
  assert (P.Run.Mode.equal (Target.run_mode selected |> protocol_ok) Single_turn);
  let nominal = ready_nominal controller selected in
  let recurrence = entry controller in
  (* The Ready row includes its retained disposition/carrier. Projecting and
     re-encoding only the nominal would erase those factual fields. *)
  let nominal_json =
    Entry.frontier recurrence
    |> protocol_ok
    |> Frontier.to_document
    |> document_ok
    |> D.Document.payload
    |> fun payload ->
    match field payload "ready" with
    | `Array rows ->
      List.find_exn rows ~f:(fun row ->
        let retained = A.Recurrence_nominal.of_json row |> protocol_ok in
        A.Recurrence_identity.Occurrence.equal
          (A.Recurrence_nominal.key nominal)
          (A.Recurrence_nominal.key retained))
    | _ -> failwith "actual controller has no complete Ready row"
  in
  let intent =
    Intent.create
      ~definition:(Entry.current recurrence)
      ~control_revision:(Entry.control_revision recurrence)
      ~nominal_json
      ~target:session
      ~source
      ~child:None
    |> protocol_ok
  in
  let attempt =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id:principal.id
      ~expected_revision:(state target).counters.revision
      ~key:(P.Idempotency_key.of_string "533-loaded-input" |> protocol_ok)
    |> protocol_ok
  in
  let preparation =
    match
      Actor.begin_headless_run_preparation
        target.actor
        ~actor:original
        ~authorize
        ~request:(Attempt.request attempt ~intent |> protocol_ok)
      |> protocol_ok
    with
    | Prepare preparation -> preparation
    | Retained _ -> failwith "input fixture reused a retained Run"
  in
  protect_cleanup
    ~finally:(fun () -> Actor.end_run_preparation target.actor preparation |> protocol_ok)
    (fun () ->
       Factory.with_prepared_run_source
         (Daemon.factory daemon)
         target
         ~preparation
         (fun prepared ->
            Actor.record_recurrence_execution
              controller.actor
              ~actor:proof
              ~authorize:(fun current _ -> authorize current)
              ~intent
              ~attempt
            |> protocol_ok
            |> ignore;
            let held =
              match
                Actor.admit_held_recurrence_run
                  target.actor
                  ~binding:(Owner.Prepared.binding prepared)
                  ~intent
                  ~attempt
                  ~session
                |> protocol_ok
              with
              | Held held -> held
              | Retained _ -> failwith "input fixture has no live held owner"
              | Rejected error | Uncertain error -> raise_s [%sexp (error : P.Error.t)]
            in
            let observe () =
              Actor.observe_recurrence_run
                target.actor
                ~actor:proof
                ~authorize
                ~intent
                ~attempt
              |> protocol_ok
              |> Option.value_exn
            in
            Actor.import_recurrence_execution
              controller.actor
              ~actor:proof
              ~authorize:(fun current _ -> authorize current)
              ~automatic_policy:(fun _ _ -> Ok true)
              ~observation:(observe ())
            |> protocol_ok
            |> ignore;
            let observation = observe () in
            let permit =
              Actor.acquire_recurrence_activation
                controller.actor
                ~actor:proof
                ~authorize:(fun current _ -> authorize current)
                ~observation
              |> protocol_ok
            in
            protect_cleanup
              ~finally:(fun () ->
                Actor.release_recurrence_activation controller.actor permit |> protocol_ok)
              (fun () ->
                 let checked =
                   Actor.check_recurrence_activation controller.actor permit ~observation
                   |> protocol_ok
                 in
                 f prepared held checked intent attempt))
       |> protocol_ok)
;;

let finish_cold
      env
      target
      ~(cut : Cold_cut.t)
      ~before
      ~original
      ~original_live
      ~calls
      ~root_calls
      ~root_entered
      ~release_root
      ~parsed
      ~escaped_execution
      ~held_facts:(receipt, intent, attempt)
      ~returned_admission
      ~runtime
      ~startup_snapshot
      ~input_runtime
      ~marker_path
      ~fault_operation
      ~fault_loaded
  =
  let parsed = Option.value_exn parsed in
  let escaped = Option.value_exn escaped_execution in
  assert (Result.is_error (escaped.Input.Execution.checkpoint ()));
  let after_lexical = state target in
  let index = Option.value_exn after_lexical.run_state in
  let run = A.Run_state.find index receipt.P.Run_receipt.run_id |> Option.value_exn in
  assert (P.Run_source.equal run.source receipt.source);
  let origin = A.Run_state.recurrence_origin index run.id |> Option.value_exn in
  A.Recurrence_run_origin.validate origin ~intent ~attempt |> protocol_ok;
  A.Recurrence_run_origin.validate_run origin ~run ~admission:receipt |> protocol_ok;
  let resumed =
    List.filter after_lexical.moderator_executions ~f:(fun event ->
      P.Moderator_execution.equal_phase event.context.phase Session_resume
      && not
           (List.exists before.A.Session_state.moderator_executions ~f:(fun old ->
              P.Id.Moderator_execution.equal old.context.id event.context.id)))
  in
  (match resumed with
   | [ { status = Completed _; _ } ] -> ()
   | _ ->
     raise_s
       [%sexp "one real cold Resume required", (resumed : P.Moderator_execution.t list)]);
  let rows = Accounting.rows after_lexical.inference_ledger in
  List.iter rows ~f:(fun row ->
    if Option.is_none (Accounting.Handle.operation_id (Accounting.Row.handle row))
    then require_completed row);
  [%test_eq: int]
    (List.count rows ~f:(fun row ->
       Option.is_none (Accounting.Handle.operation_id (Accounting.Row.handle row))))
    2;
  let settled =
    match cut with
    | Root_sync_failure ->
      assert fault_loaded;
      assert (Option.is_some fault_operation);
      assert (Option.is_none returned_admission);
      assert (not (Owner.is_loaded target.runtime));
      assert (Option.is_none after_lexical.active_operation);
      [%test_eq: string list] [ "A"; "B" ] !calls;
      [%test_eq: int] 0 !root_calls;
      assert (List.is_empty after_lexical.jobs);
      assert (List.is_empty after_lexical.invocations);
      let replayed = ref 0 in
      (match
         Owner.Held_input_runtime.submit
           (Option.value_exn input_runtime)
           ~prepare:(fun ~runtime:_ ~scope:_ ->
             Int.incr replayed;
             failwith "escaped partial-install owner replayed its parser")
       with
       | Error error -> [%test_eq: P.Error.code] Conflict error.code
       | Ok _ -> failwith "partial installed owner admitted twice");
      [%test_eq: int] 0 !replayed;
      (* The failed sync poisons the writer. A later interruption cannot ACK;
         the written root candidate and its factual retirement stay unconfirmed. *)
      (match run.lifecycle with
       | Admitted | Active -> ()
       | Waiting _ | Terminal _ -> raise_s [%sexp (run : P.Run.t)]);
      let attempted_root = Option.value_exn fault_operation in
      assert (
        not
          (List.exists run.owned_work ~f:(fun work ->
             match work.P.Run_work.key with
             | Operation operation -> P.Id.Operation.equal operation attempted_root
             | Retained _ -> false)));
      assert (
        not
          (List.exists (A.Run_state.receipts index) ~f:(fun saved ->
             P.Id.Run.equal saved.run_id receipt.run_id
             && P.Run_receipt.Kind.equal saved.kind Terminal)));
      print_endline
        "root sync failure retires resources; Run interruption remains unconfirmed";
      after_lexical
    | Adopt | Revoke_after_lexical_exit ->
      assert (P.Run_receipt.equal receipt (Option.value_exn returned_admission));
      assert (Owner.is_loaded target.runtime);
      let operation = Option.value_exn after_lexical.active_operation in
      let work =
        P.Run_work.create ~key:(Operation operation.id) ~generation:operation.generation
        |> protocol_ok
      in
      assert (List.mem run.owned_work work ~equal:P.Run_work.equal);
      (match run.lifecycle with
       | Active | Admitted | Waiting _ -> ()
       | Terminal _ -> failwith "cold handoff closed its actual root Run");
      let entry = Input.Prepared_input.entry parsed in
      let saved =
        List.filter after_lexical.conversation.canonical_history ~f:(fun saved ->
          P.History.Id.equal saved.id entry.id)
      in
      (match saved with
       | [ saved ] -> assert (P.History.equal_entry entry saved)
       | _ -> failwith "cold root must append recorded input exactly once");
      let rec await_root () =
        match Eio.Promise.peek root_entered with
        | Some () -> ()
        | None ->
          if Option.is_none (state target).active_operation
          then failwith "cold root ended before actual provider";
          Eio.Fiber.yield ();
          await_root ()
      in
      protect_cleanup ~finally:release_root (fun () ->
        await_root ();
        [%test_eq: string list] [ "A"; "B"; "Root" ] !calls;
        assert (Operator_authorization.is_current original);
        (* Same physical adopted resource, no initializer replay. In particular,
           its cached start DATA must not call the now-closed startup guard. *)
        Owner.with_background_runtime target.runtime (fun installed ->
          assert (phys_equal installed (Option.value_exn runtime));
          let snapshot = installed.A.Runtime_builder.start_moderator () |> protocol_ok in
          assert (
            Option.equal Jsonaf.exactly_equal snapshot (Option.value_exn startup_snapshot));
          Ok ())
        |> protocol_ok;
        assert (
          Option.equal
            Jsonaf.exactly_equal
            after_lexical.moderator
            (state target).moderator);
        match cut with
        | Revoke_after_lexical_exit -> original_live := false
        | Adopt -> ()
        | Root_sync_failure -> assert false);
      let rec await_terminal () =
        let current = state target in
        if Option.is_none current.active_operation
        then current
        else (
          Eio.Fiber.yield ();
          await_terminal ())
      in
      let final = await_terminal () in
      let index = Option.value_exn final.run_state in
      let completed = A.Run_state.find index run.id |> Option.value_exn in
      let natives =
        List.filter final.invocations ~f:(fun invocation ->
          Option.equal
            String.equal
            invocation.context.provider_call_id
            (Some "535-post-scope-call"))
      in
      (match cut with
       | Adopt ->
         [%test_eq: string list] [ "A"; "B"; "Root"; "Post"; "Root" ] !calls;
         (match natives with
          | [ native ] ->
            [%test_eq: string] "append_to_file" native.context.tool_name;
            assert (
              Option.equal
                P.Inference_origin.equal
                native.inference_origin
                (Some (Run run.id)));
            (match native.status with
             | Resolved (Complete _) | Published (Complete _) -> ()
             | _ -> raise_s [%sexp (native : P.Invocation.t)])
          | _ -> raise_s [%sexp (natives : P.Invocation.t list)]);
         let job =
           match final.jobs with
           | [ job ] -> job
           | jobs -> raise_s [%sexp (jobs : P.Job.t list)]
         in
         assert (P.Job.equal_kind job.kind Model_call);
         assert (
           Option.equal P.Inference_origin.equal job.inference_origin (Some (Run run.id)));
         (match job.status with
          | Succeeded -> ()
          | _ -> raise_s [%sexp (job : P.Job.t)]);
         assert (
           Option.equal
             Jsonaf.exactly_equal
             (Option.bind job.result ~f:(Jsonaf.member "final_text"))
             (Some (`String "535-post-model-result")));
         let child = A.Run_job_occurrence.work job |> protocol_ok in
         List.iter [ work; child ] ~f:(fun expected ->
           assert (
             List.exists completed.terminal_work ~f:(fun terminal ->
               P.Run_work.equal terminal.work expected
               && P.Run_work.Terminal.equal_outcome terminal.outcome Succeeded)));
         (match completed.lifecycle with
          | Terminal (Completed _) -> ()
          | _ -> raise_s [%sexp (completed : P.Run.t)]);
         print_endline "cold adopted native and Model effects survive lexical close"
       | Revoke_after_lexical_exit ->
         [%test_eq: string list] [ "A"; "B"; "Root" ] !calls;
         assert (List.is_empty final.jobs);
         assert (
           not
             (List.exists natives ~f:(fun native ->
                match native.status with
                | Resolved (Complete _) | Published (Complete _) -> true
                | Admitted | Dispatching | Resolved _ | Published _ -> false)));
         (match completed.lifecycle with
          | Terminal (Failed _) -> ()
          | _ -> raise_s [%sexp (completed : P.Run.t)]);
         print_endline
           "revoked adopted original denies downstream native and Model effects"
       | Root_sync_failure -> assert false);
      let events =
        match
          A.Durable_event_log.replay
            target.durable_events
            ~after_sequence:before.counters.event_sequence
            ~through_sequence:final.counters.event_sequence
        with
        | Available events -> events
        | Snapshot_required -> failwith "cold handoff exceeded event replay window"
      in
      if
        not
          (List.exists events ~f:(fun event ->
             match
               P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
               |> protocol_ok
             with
             | Operation_completed actual ->
               (match cut, actual.state with
                | Adopt, Completed -> P.Id.Operation.equal actual.id operation.id
                | _ -> false)
             | Operation_failed actual ->
               (match cut, actual.state with
                | Revoke_after_lexical_exit, Failed error ->
                  P.Id.Operation.equal actual.id operation.id
                  && P.Error.equal_code error.code Permission_denied
                  && not error.retryable
                | _ -> false)
             | _ -> false))
      then
        raise_s
          [%sexp
            "missing expected root completion event"
          , (List.filter_map events ~f:(fun event ->
               match
                 P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                 |> protocol_ok
               with
               | Operation_completed actual | Operation_failed actual -> Some actual
               | _ -> None)
             : P.Operation.t list)];
      [%test_eq: int]
        (List.count (A.Run_state.receipts index) ~f:(fun saved ->
           P.Id.Run.equal saved.run_id receipt.run_id
           && P.Run_receipt.Kind.equal saved.kind Terminal))
        1;
      final
  in
  [%test_eq: string]
    (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / marker_path))
    (match cut with
     | Adopt -> "\n535 native after lexical close"
     | Revoke_after_lexical_exit | Root_sync_failure -> "");
  (* At the fault cut the root bytes were written before sync and are uncertain.
     Check confirmed factual cleanup, never assert absent disk bytes or NoEffects. *)
  let directory = Option.value_exn target.store_handle |> S.Handle.snapshot_directory in
  target.close ();
  let restored =
    Agent_store.Snapshot.load_current
      ~env
      ~directory
      ~max_payload_length:(64 * 1024 * 1024)
    |> store_ok
    |> Option.value_exn
    |> fun (installed : Agent_store.Snapshot.installed) ->
    A.Session_state_document.decode ~limits:D.Limits.default installed.snapshot.payload
    |> document_ok
    |> A.Session_state_document.value
  in
  assert (
    Jsonaf.exactly_equal
      (T.state_document settled |> store_ok |> D.Document.json)
      (T.state_document restored |> store_ok |> D.Document.json))
;;

let run (cut : Cut.t) =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    protect_cleanup
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      (fun () ->
         let prompt_file = Filename.concat root "input-owner.chatmd" in
         List.iter
           [ ( prompt_file
             , match cut with
               | Cold _ -> cold_prompt
               | Complete | Revoke_between_agents | Stop_after_first | Handoff -> prompt
             )
           ; Filename.concat root "input-a.chatmd", "<developer>533_AGENT_A</developer>"
           ; Filename.concat root "input-b.chatmd", "<developer>533_AGENT_B</developer>"
           ; ( Filename.concat root "input-post.chatmd"
             , "<developer>535_AGENT_POST</developer>" )
           ; Filename.concat root "post-scope.txt", ""
           ]
           ~f:(fun (path, text) ->
             Eio.Path.save
               ~create:(`Exclusive 0o600)
               Eio.Path.(Eio.Stdenv.fs env / path)
               text);
         let calls = ref [] in
         let root_calls = ref 0 in
         let fault_target = ref None in
         let fault_owner = ref None in
         let fault_armed = ref false in
         let fault_operation = ref None in
         let fault_loaded = ref false in
         let original_live = ref true in
         let first, first_u = Eio.Promise.create () in
         let second, second_u = Eio.Promise.create () in
         let release_second, release_second_u = Eio.Promise.create () in
         let completed, completed_u = Eio.Promise.create () in
         let release_completion, release_completion_u = Eio.Promise.create () in
         let stop_armed = ref false in
         let root_entered, root_entered_u = Eio.Promise.create () in
         let root_release, root_release_u = Eio.Promise.create () in
         let adoption_probe = ref None in
         let adoption_probed, adoption_probed_u = Eio.Promise.create () in
         let adoption_probe_verified = ref false in
         let source_callback_returned = ref false in
         let release, release_u = Eio.Promise.create () in
         let release_first () =
           if Option.is_none (Eio.Promise.peek release)
           then Eio.Promise.resolve release_u ()
         in
         let release_root () =
           if Option.is_none (Eio.Promise.peek root_release)
           then Eio.Promise.resolve root_release_u ()
         in
         let release_all () =
           release_root ();
           release_first ();
           if Option.is_none (Eio.Promise.peek release_completion)
           then Eio.Promise.resolve release_completion_u ();
           if Option.is_none (Eio.Promise.peek release_second)
           then Eio.Promise.resolve release_second_u ()
         in
         let policy =
           T.inference_policy
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs ->
               let transcript =
                 `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
               in
               let marker =
                 if String.is_substring transcript ~substring:"535_AGENT_POST"
                 then "Post"
                 else if
                   String.is_substring transcript ~substring:"533-input-A-result"
                   && String.is_substring transcript ~substring:"533-input-B-result"
                 then "Root"
                 else if String.is_substring transcript ~substring:"533_AGENT_A"
                 then "A"
                 else if String.is_substring transcript ~substring:"533_AGENT_B"
                 then "B"
                 else failwith "unexpected provider outside actual input Agents"
               in
               calls := !calls @ [ marker ];
               match marker with
               | "A" ->
                 Eio.Promise.resolve first_u ();
                 Eio.Promise.await release;
                 Activation_tests.completed_response "533-input-A-result"
               | "B" ->
                 Eio.Promise.resolve second_u ();
                 Eio.Promise.await release_second;
                 Activation_tests.completed_response "533-input-B-result"
               | "Post" -> Activation_tests.completed_response "535-post-model-result"
               | "Root" ->
                 Int.incr root_calls;
                 if Int.equal !root_calls 1
                 then (
                   (match cut with
                    | Cold Adopt ->
                      (* This provider is reached by the actual root worker. Its
                         parent source lease still exists. Reaching this worker
                         already proves root ACK; do not wait for its caller reply. *)
                      let probe =
                        try Ok ((Option.value_exn !adoption_probe) ()) with
                        | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
                      in
                      Eio.Promise.resolve adoption_probed_u probe
                    | _ -> ());
                   Eio.Promise.resolve root_entered_u ();
                   Eio.Promise.await root_release);
                 (match cut, !root_calls with
                  | Cold (Adopt | Revoke_after_lexical_exit), 1 -> native_response ()
                  | Cold Adopt, 2 | Handoff, 1 ->
                    Activation_tests.completed_response "533-input-Root-result"
                  | _ -> failwith "unexpected root provider after actual input")
               | _ -> failwith "unknown actual input provider marker")
         in
         let policy =
           { policy with
             runtime_inference_ports =
               (fun actor ->
                 Result.map (policy.runtime_inference_ports actor) ~f:(fun upstream ->
                   { upstream with
                     on_completion =
                       (fun completion ->
                         upstream.on_completion completion;
                         if !stop_armed
                         then (
                           stop_armed := false;
                           Eio.Promise.resolve completed_u ();
                           Eio.Promise.await release_completion))
                   }))
           }
         in
         let configuration = T.config root root prompt_file in
         let configuration =
           { configuration with
             workspaces =
               List.map
                 configuration.workspaces
                 ~f:(fun (workspace : Agent_server.Config.Workspace.t) ->
                   { workspace with
                     prompt_limits =
                       List.map
                         workspace.prompt_limits
                         ~f:(fun (limit : Agent_server.Config.Workspace.prompt_limit) ->
                           { limit with max_root_agents = 2 })
                   })
           }
         in
         let provider_calls = ref 0 in
         let daemon_env =
           match cut with
           | Cold Root_sync_failure ->
             Crash_fault_io.wrap
               env
               ~boundary:Before_sync
               ~matches:(fun path ->
                 String.is_substring path ~substring:"/journal/"
                 && String.is_suffix path ~suffix:".log")
               ~reached:(fun path ->
                 match !fault_armed, !fault_target with
                 | true, Some target ->
                   (match written_root env path target with
                    | None -> ()
                    | Some operation ->
                      fault_armed := false;
                      fault_operation := Some operation;
                      fault_loaded := Owner.is_loaded (Option.value_exn !fault_owner);
                      assert !fault_loaded;
                      raise
                        (Core_unix.Unix_error (EIO, "535 root journal before sync", path)))
                 | false, _ | true, None -> ())
           | Complete | Revoke_between_agents | Stop_after_first | Handoff
           | Cold (Adopt | Revoke_after_lexical_exit) -> env
         in
         with_daemon
           ~policy
           daemon_env
           root
           configuration
           ~provider_calls
           (fun sw daemon ->
              let connection = T.connection daemon principal in
              protect_cleanup
                ~finally:(fun () ->
                  release_all ();
                  Agent_client.Connection.close connection)
                (fun () ->
                   T.initialize connection;
                   let controller, _ =
                     T.create_session
                       ~start_immediately:true
                       ~key:"533-controller"
                       connection
                   in
                   let target_session, target_attachment =
                     T.create_session ~start_immediately:true ~key:"533-target" connection
                   in
                   let controller = loaded daemon controller.id in
                   let target = loaded daemon target_session.id in
                   start_parent_settled env sw connection controller;
                   start_parent_settled env sw connection target;
                   (match cut with
                    | Cold _ ->
                      assert (Option.is_none (state target).active_operation);
                      Owner.unload_and_wait target.runtime |> protocol_ok;
                      assert (not (Owner.is_loaded target.runtime))
                    | Complete | Revoke_between_agents | Stop_after_first | Handoff -> ());
                   fault_target := Some target_session.id;
                   fault_owner := Some target.runtime;
                   let before = state target in
                   assert_no_root before;
                   assert (List.is_empty (Accounting.rows before.inference_ledger));
                   assert (List.is_empty !calls);
                   let original =
                     Operator_authorization.guarded ~principal ~is_current:(fun () ->
                       !original_live)
                   in
                   with_planner env sw daemon controller (fun () ->
                     let parsed = ref None in
                     let submitted = ref None in
                     let returned_admission = ref None in
                     let runtime_used = ref None in
                     adoption_probe
                     := Some
                          (fun () ->
                            Owner.with_background_runtime target.runtime (fun installed ->
                              Ok
                                ( phys_equal installed (Option.value_exn !runtime_used)
                                , not !source_callback_returned )));
                     let input_runtime_used = ref None in
                     let startup_snapshot = ref None in
                     let parsed_effect_check = ref None in
                     let escaped_execution = ref None in
                     let admission = ref None in
                     let input_result = ref None in
                     let held_facts = ref None in
                     (* The entire source/held scope is supervised. Stop cancels that
                   actual lease; the observer must remain outside its context. *)
                     let worker =
                       Eio.Fiber.fork_promise ~sw (fun () ->
                         match
                           with_loaded_held
                             daemon
                             controller
                             target
                             ~original
                             (fun prepared held checked intent attempt ->
                                let receipt = Actor.Held_run.admission held in
                                admission := Some receipt;
                                held_facts := Some (receipt, intent, attempt);
                                (stop_armed
                                 := match cut with
                                    | Stop_after_first -> true
                                    | Complete | Revoke_between_agents | Handoff | Cold _
                                      -> false);
                                let result =
                                  match cut with
                                  | Cold _ ->
                                    Owner.with_held_input_runtime
                                      target.runtime
                                      ~prepared
                                      ~held
                                      ~checked
                                      (fun input_runtime ->
                                         input_runtime_used := Some input_runtime;
                                         Owner.Held_input_runtime.submit
                                           input_runtime
                                           ~prepare:(fun ~runtime ~scope ->
                                             runtime_used := Some runtime;
                                             startup_snapshot
                                             := Some
                                                  (runtime
                                                     .A.Runtime_builder.start_moderator
                                                     ()
                                                   |> protocol_ok);
                                             let execution =
                                               Actor.Held_input_scope.execution scope
                                               |> protocol_ok
                                             in
                                             escaped_execution := Some execution;
                                             Result.map
                                               (Factory.prepare_held_input
                                                  (Daemon.factory daemon)
                                                  ~entry:target
                                                  ~runtime
                                                  ~scope)
                                               ~f:(fun input ->
                                                 parsed := Some input;
                                                 (match cut with
                                                  | Cold Root_sync_failure ->
                                                    fault_armed := true
                                                  | _ -> ());
                                                 input))
                                         |> Result.map ~f:(fun saved ->
                                           returned_admission := Some saved;
                                           (match cut with
                                            | Cold Adopt ->
                                              let probe =
                                                Eio.Time.with_timeout_exn
                                                  (Eio.Stdenv.clock env)
                                                  10.
                                                  (fun () ->
                                                     Eio.Promise.await adoption_probed)
                                              in
                                              let same_runtime, source_still_open =
                                                match probe with
                                                | Ok result -> protocol_ok result
                                                | Error (exn, backtrace) ->
                                                  Exn.raise_with_original_backtrace
                                                    exn
                                                    backtrace
                                              in
                                              assert same_runtime;
                                              assert source_still_open;
                                              adoption_probe_verified := true
                                            | _ -> ());
                                           source_callback_returned := true))
                                  | Complete
                                  | Revoke_between_agents
                                  | Stop_after_first
                                  | Handoff ->
                                    Owner.with_held_recurrence_runtime
                                      target.runtime
                                      ~prepared
                                      ~held
                                      ~checked
                                      (fun runtime continuation ->
                                         Actor.with_held_input
                                           target.actor
                                           ~continuation
                                           ~prepare:(fun ~scope execution ->
                                             escaped_execution := Some execution;
                                             Factory.prepare_held_input
                                               (Daemon.factory daemon)
                                               ~entry:target
                                               ~runtime
                                               ~scope)
                                           (fun ~scope ~input ->
                                              let execution =
                                                Option.value_exn !escaped_execution
                                              in
                                              parsed_effect_check
                                              := Some
                                                   (execution.Input.Execution.checkpoint
                                                      ());
                                              parsed := Some input;
                                              match cut with
                                              | Handoff ->
                                                Result.map
                                                  (Actor.submit_held_run_input
                                                     target.actor
                                                     ~scope
                                                     ~input)
                                                  ~f:(fun submission ->
                                                    submitted := Some submission)
                                              | Complete
                                              | Revoke_between_agents
                                              | Stop_after_first -> Ok ()
                                              | Cold _ -> assert false))
                                in
                                input_result := Some result;
                                Ok ())
                         with
                         | () -> `Joined
                         | exception (Eio.Cancel.Cancelled _ as exn) ->
                           (match cut with
                            | Stop_after_first -> `Cancelled
                            | Complete | Revoke_between_agents | Handoff | Cold _ ->
                              let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                              Exn.raise_with_original_backtrace exn backtrace))
                     in
                     let await_boundary entered description =
                       let rec await () =
                         match Eio.Promise.peek entered with
                         | Some () -> ()
                         | None ->
                           (match Eio.Promise.peek worker with
                            | Some _ ->
                              ignore (Eio.Promise.await_exn worker);
                              (match !input_result with
                               | Some (Error error) -> raise_s [%sexp (error : P.Error.t)]
                               | Some (Ok ()) | None -> failwith description)
                            | None ->
                              Eio.Fiber.yield ();
                              await ())
                       in
                       await ()
                     in
                     let outcome =
                       protect_cleanup
                         ~finally:(fun () ->
                           match cut with
                           | Handoff | Cold _ -> ()
                           | Complete | Revoke_between_agents | Stop_after_first ->
                             release_all ())
                         (fun () ->
                            await_boundary
                              first
                              "input ended before actual first provider";
                            assert_no_root (state target);
                            [%test_eq: string list] [ "A" ] !calls;
                            assert (Option.is_none !parsed);
                            (match cut with
                             | Complete | Stop_after_first | Handoff | Cold _ -> ()
                             | Revoke_between_agents -> original_live := false);
                            release_first ();
                            (match cut with
                             | Revoke_between_agents -> ()
                             | Stop_after_first ->
                               await_boundary
                                 completed
                                 "input ended before completed first Agent";
                               let rows =
                                 Accounting.rows (state target).inference_ledger
                               in
                               [%test_eq: int] 1 (List.length rows);
                               List.iter rows ~f:require_completed;
                               Actor.stop
                                 target.actor
                                 ~attachment_id:target_attachment.id
                                 ~mode:Cancel
                               |> protocol_ok
                               |> ignore;
                               Eio.Promise.resolve release_completion_u ()
                             | Complete | Handoff | Cold _ ->
                               await_boundary
                                 second
                                 "input ended before actual second provider";
                               let during = state target in
                               assert_no_root during;
                               let receipt = Option.value_exn !admission in
                               let run =
                                 A.Run_state.find
                                   (Option.value_exn during.run_state)
                                   receipt.run_id
                                 |> Option.value_exn
                               in
                               (match run.lifecycle with
                                | Admitted | Active | Waiting _ -> ()
                                | Terminal _ ->
                                  failwith
                                    "first input Agent prematurely settled held Run");
                               let rows = Accounting.rows during.inference_ledger in
                               [%test_eq: int] 2 (List.length rows);
                               require_completed (List.hd_exn rows);
                               [%test_eq: string list] [ "A"; "B" ] !calls;
                               assert (Option.is_none !parsed);
                               Eio.Promise.resolve release_second_u ());
                            Eio.Promise.await_exn worker)
                     in
                     (match cut, outcome, !input_result with
                      | ( (Complete | Handoff | Cold (Adopt | Revoke_after_lexical_exit))
                        , `Joined
                        , Some (Ok ()) ) -> ()
                      | Cold Root_sync_failure, `Joined, Some (Error error) ->
                        if not (P.Error.equal_code error.code Persistence_error)
                        then raise_s [%sexp (error : P.Error.t)]
                      | Revoke_between_agents, `Joined, Some (Error error) ->
                        if not (P.Error.equal_code error.code Permission_denied)
                        then raise_s [%sexp (error : P.Error.t)]
                      | Stop_after_first, `Joined, Some (Error error) ->
                        (match error.code with
                         | Conflict | Permission_denied -> ()
                         | _ -> raise_s [%sexp (error : P.Error.t)])
                      | Stop_after_first, `Cancelled, _ -> ()
                      | _, _, Some (Error error) -> raise_s [%sexp (error : P.Error.t)]
                      | _ -> failwith "input scope outcome disagrees with the actual cut");
                     match cut with
                     | Cold cold_cut ->
                       (match cold_cut with
                        | Adopt -> assert !adoption_probe_verified
                        | Revoke_after_lexical_exit | Root_sync_failure -> ());
                       finish_cold
                         env
                         target
                         ~cut:cold_cut
                         ~before
                         ~original
                         ~original_live
                         ~calls
                         ~root_calls
                         ~root_entered
                         ~release_root
                         ~parsed:!parsed
                         ~escaped_execution:!escaped_execution
                         ~held_facts:(Option.value_exn !held_facts)
                         ~returned_admission:!returned_admission
                         ~runtime:!runtime_used
                         ~startup_snapshot:!startup_snapshot
                         ~input_runtime:!input_runtime_used
                         ~marker_path:(Filename.concat root "post-scope.txt")
                         ~fault_operation:!fault_operation
                         ~fault_loaded:!fault_loaded
                     | Complete | Revoke_between_agents | Stop_after_first | Handoff ->
                       (* Joining the whole worker includes source lease cancellation
                   and all held/input/graph cleanup, before any final oracle. *)
                       let after = state target in
                       let receipt, intent, attempt = Option.value_exn !held_facts in
                       let index = Option.value_exn after.run_state in
                       let run =
                         A.Run_state.find index receipt.run_id |> Option.value_exn
                       in
                       (match cut, run.lifecycle with
                        | Handoff, (Admitted | Active | Waiting _) -> ()
                        | ( (Complete | Revoke_between_agents | Stop_after_first)
                          , Terminal Interrupted ) -> ()
                        | _ -> raise_s [%sexp (run : P.Run.t)]);
                       assert (P.Run_source.equal run.source receipt.source);
                       let origin =
                         A.Run_state.recurrence_origin index run.id |> Option.value_exn
                       in
                       A.Recurrence_run_origin.validate origin ~intent ~attempt
                       |> protocol_ok;
                       (match cut with
                        | Handoff -> assert (List.is_empty after.jobs)
                        | Complete | Revoke_between_agents | Stop_after_first ->
                          assert_no_root after
                        | Cold _ -> assert false);
                       (match cut with
                        | Complete | Revoke_between_agents | Stop_after_first ->
                          assert (
                            List.equal
                              P.History.equal_entry
                              before.conversation.canonical_history
                              after.conversation.canonical_history)
                        | Handoff ->
                          let submission = Option.value_exn !submitted in
                          let operation_id = Option.value_exn submission.operation_id in
                          let operation = Option.value_exn after.active_operation in
                          assert (P.Id.Operation.equal operation.id operation_id);
                          let work =
                            P.Run_work.create
                              ~key:(Operation operation_id)
                              ~generation:after.identity.generation
                            |> protocol_ok
                          in
                          assert (List.mem run.owned_work work ~equal:P.Run_work.equal);
                          let input = Option.value_exn !parsed in
                          let entry = Input.Prepared_input.entry input in
                          let retained =
                            List.filter
                              after.conversation.canonical_history
                              ~f:(fun saved ->
                                P.History.Id.equal saved.id submission.history_id)
                          in
                          (match retained with
                           | [ saved ] -> assert (P.History.equal_entry saved entry)
                           | _ -> failwith "root input was not appended exactly once");
                          let rec await_root () =
                            match Eio.Promise.peek root_entered with
                            | Some () -> ()
                            | None ->
                              let current = state target in
                              if Option.is_none current.active_operation
                              then
                                raise_s
                                  [%sexp
                                    "root ended before actual provider"
                                  , (current.lifecycle : A.Session_state.Lifecycle.t)]
                              else (
                                Eio.Fiber.yield ();
                                await_root ())
                          in
                          protect_cleanup ~finally:release_root (fun () ->
                            await_root ();
                            [%test_eq: string list] [ "A"; "B"; "Root" ] !calls;
                            assert (Operator_authorization.is_current original));
                          let rec await_terminal () =
                            let current = state target in
                            if Option.is_none current.active_operation
                            then current
                            else (
                              Eio.Fiber.yield ();
                              await_terminal ())
                          in
                          let settled = await_terminal () in
                          let index = Option.value_exn settled.run_state in
                          let finished =
                            A.Run_state.find index receipt.run_id |> Option.value_exn
                          in
                          (match finished.lifecycle with
                           | Terminal (Completed _) -> ()
                           | _ -> raise_s [%sexp (finished : P.Run.t)]);
                          assert (
                            List.exists finished.terminal_work ~f:(fun terminal ->
                              P.Run_work.equal terminal.work work
                              && P.Run_work.Terminal.equal_outcome
                                   terminal.outcome
                                   Succeeded));
                          let terminal_receipts =
                            A.Run_state.receipts index
                            |> List.filter ~f:(fun item ->
                              P.Id.Run.equal item.run_id receipt.run_id
                              && P.Run_receipt.Kind.equal item.kind Terminal)
                          in
                          [%test_eq: int] 1 (List.length terminal_receipts);
                          let events =
                            match
                              A.Durable_event_log.replay
                                target.durable_events
                                ~after_sequence:before.counters.event_sequence
                                ~through_sequence:settled.counters.event_sequence
                            with
                            | Available events -> events
                            | Snapshot_required ->
                              failwith "input handoff exceeded event window"
                          in
                          assert (
                            List.exists events ~f:(fun event ->
                              match
                                P.Event.Durable.Payload.of_json
                                  ~kind:event.kind
                                  event.payload
                                |> protocol_ok
                              with
                              | Operation_completed { id; state = Completed; _ } ->
                                P.Id.Operation.equal id operation_id
                              | _ -> false));
                          assert (
                            String.is_substring
                              (Jsonaf.to_string
                                 (`Array
                                     (List.map
                                        settled.conversation.canonical_history
                                        ~f:P.History.entry_to_json)))
                              ~substring:"533-input-Root-result")
                        | Cold _ -> assert false);
                       let after = state target in
                       let rows = Accounting.rows after.inference_ledger in
                       let expected =
                         match cut with
                         | Complete -> [ "A"; "B" ]
                         | Handoff -> [ "A"; "B"; "Root" ]
                         | Revoke_between_agents | Stop_after_first -> [ "A" ]
                         | Cold _ -> assert false
                       in
                       [%test_eq: string list] expected !calls;
                       [%test_eq: int] (List.length expected) (List.length rows);
                       List.iter rows ~f:(fun row ->
                         match
                           Accounting.Handle.operation_id (Accounting.Row.handle row)
                         with
                         | None -> require_completed row
                         | Some operation ->
                           let submitted = Option.value_exn !submitted in
                           assert (
                             Option.equal
                               P.Id.Operation.equal
                               (Some operation)
                               submitted.operation_id);
                           assert (
                             Option.is_none
                               (Accounting.Handle.invocation_id
                                  (Accounting.Row.handle row)));
                           require_completed_fact row);
                       List.iter rows ~f:(fun row ->
                         assert (
                           P.Id.Session.equal
                             target_session.id
                             (Accounting.Handle.session_id (Accounting.Row.handle row))));
                       (match cut, !parsed with
                        | (Complete | Handoff), Some input ->
                          (match !parsed_effect_check with
                           | Some (Error error) ->
                             [%test_eq: P.Error.code] Conflict error.code;
                             [%test_eq: string]
                               "held input excludes new effects"
                               error.message
                           | Some (Ok ()) | None ->
                             failwith "Parsed_ready retained an effect port");
                          let entry = Input.Prepared_input.entry input in
                          assert (P.History.equal_role entry.role User);
                          assert (P.History.equal_kind entry.kind Message);
                          assert (
                            Jsonaf.exactly_equal
                              (P.Session.Message_content.to_json
                                 (Input.Prepared_input.content input))
                              (P.Session.Message_content.to_json content));
                          let json = Jsonaf.to_string entry.payload in
                          assert (String.is_substring json ~substring:"533-input-A-result");
                          assert (String.is_substring json ~substring:"533-input-B-result")
                        | (Revoke_between_agents | Stop_after_first), None ->
                          assert (Option.is_none !parsed_effect_check)
                        | _ -> failwith "parsed result disagrees with actual input cut");
                       let execution = Option.value_exn !escaped_execution in
                       assert (Result.is_error (execution.Input.Execution.checkpoint ()));
                       assert (Option.is_some !admission);
                       (match cut with
                        | Complete | Revoke_between_agents | Stop_after_first ->
                          assert (
                            Option.equal
                              Jsonaf.exactly_equal
                              before.moderator
                              after.moderator);
                          assert (
                            List.equal
                              P.Moderator_execution.equal
                              before.moderator_executions
                              after.moderator_executions)
                        | Handoff ->
                          let startups state =
                            List.filter
                              state.A.Session_state.moderator_executions
                              ~f:(fun receipt ->
                                match receipt.P.Moderator_execution.context.phase with
                                | Session_start | Session_resume -> true
                                | _ -> false)
                          in
                          assert (
                            List.equal
                              P.Moderator_execution.equal
                              (startups before)
                              (startups after))
                        | Cold _ -> assert false);
                       (match cut with
                        | Complete | Revoke_between_agents | Handoff ->
                          assert (Owner.is_loaded target.runtime)
                        | Stop_after_first ->
                          (match after.lifecycle.desired with
                           | Stopped -> ()
                           | Running -> failwith "actual Stop did not persist");
                          (* Factory may already be retiring this exact runtime after
                      the committed Stop. Join that actual retirement instead
                      of racing its resource close with non-waiting unload. *)
                          Owner.unload_and_wait target.runtime |> protocol_ok;
                          assert (not (Owner.is_loaded target.runtime))
                        | Cold _ -> assert false);
                       (match cut with
                        | Complete | Revoke_between_agents | Stop_after_first ->
                          print_s
                            [%sexp
                              (cut : Cut.t)
                            , "actual input facts retained; no Operation or initializer \
                               replay"]
                        | Handoff ->
                          let directory =
                            Option.value_exn target.store_handle
                            |> S.Handle.snapshot_directory
                          in
                          target.close ();
                          let restored =
                            Agent_store.Snapshot.load_current
                              ~env
                              ~directory
                              ~max_payload_length:(64 * 1024 * 1024)
                            |> store_ok
                            |> Option.value_exn
                            |> fun (installed : Agent_store.Snapshot.installed) ->
                            A.Session_state_document.decode
                              ~limits:D.Limits.default
                              installed.snapshot.payload
                            |> document_ok
                            |> A.Session_state_document.value
                          in
                          assert (
                            Jsonaf.exactly_equal
                              (T.state_document after |> store_ok |> D.Document.json)
                              (T.state_document restored |> store_ok |> D.Document.json));
                          print_endline
                            "two Agents and root retain one Run after lexical source \
                             close"
                        | Cold _ -> assert false))))))
;;

let%expect_test "loaded held input joins actual Agents without granting a root Operation" =
  List.iter [ Cut.Complete; Revoke_between_agents; Stop_after_first ] ~f:run;
  [%expect
    {|
    (Complete "actual input facts retained; no Operation or initializer replay")
    (Revoke_between_agents
     "actual input facts retained; no Operation or initializer replay")
    (Stop_after_first
     "actual input facts retained; no Operation or initializer replay")
    |}]
;;

let%expect_test "loaded rich input hands its same Run to the actual Factory worker" =
  run Handoff;
  [%expect {| two Agents and root retain one Run after lexical source close |}]
;;

let%expect_test "cold held input adopts resources and retires partial installation" =
  List.iter
    [ Cold_cut.Adopt; Revoke_after_lexical_exit; Root_sync_failure ]
    ~f:(fun cut -> run (Cold cut));
  [%expect
    {|
    cold adopted native and Model effects survive lexical close
    revoked adopted original denies downstream native and Model effects
    root sync failure retires resources; Run interruption remains unconfirmed
    |}]
;;
