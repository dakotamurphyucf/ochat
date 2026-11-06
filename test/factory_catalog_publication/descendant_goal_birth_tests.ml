open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module G = P.Goal
module GD = A.Goal_descendants
module Origin = Agent_store.Delegation_goal_origin
module Delegations = Agent_store.Delegation_store

let child_source =
  {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>554 native Goal child, stopped before initialization.</developer>
<script id="child" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state + 1)
</script>|}
;;

let creation_request ?(tools = []) sources =
  `Object
    [ "version", `Number "1"
    ; "root_file", `String "child.chatmd"
    ; ( "sources"
      , `Array
          (List.map sources ~f:(fun (path, text) ->
             `Object [ "path", `String path; "text", `String text ])) )
    ; "tools", `Array (List.map tools ~f:(fun name -> `String name))
    ; "start_immediately", `False
    ; "idempotency_key", `String "554-native-goal-birth"
    ]
;;

let create_response ?tools sources =
  let open Openai.Responses.Response_stream in
  let call arguments status : Item.t =
    Function_call
      { name = "agent_create"
      ; arguments
      ; call_id = "554-native-goal-create"
      ; _type = "function_call"
      ; id = Some "554-native-goal-create-item"
      ; status
      }
  in
  let arguments = Jsonaf.to_string (creation_request ?tools sources) in
  [ Output_item_added
      { item = call "" None; output_index = 0; type_ = "response.output_item.added" }
  ; Function_call_arguments_done
      { arguments
      ; item_id = "554-native-goal-create-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item = call arguments (Some "completed")
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

(* A real selected native binding. Its counter measures the actual implementation
   prefix, independently of invocation admission and outcome persistence. *)
let native_effect_registry calls =
  let module Definition = struct
    type input = string

    let name = "read_file"
    let description = Some "Inherited native final-wait fixture"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun _ ->
         Int.incr calls;
         Openai.Responses.Tool_output.Output.Text "native effect executed")
  in
  Chat_response.Tool_capability.create
    ~owner:"inherited-native-wait"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "inherited-native-wait")
    [ Chatmd_shell_spec.Source_ref.digest "inherited-native-wait-v1", implementation ]
  |> Result.map_error ~f:(fun error -> error.Chat_response.Tool_capability.message)
  |> Result.ok_or_failwith
;;

let with_native_goal_child
      ?test_root
      ?timeout
      ?(parent_extra_tools = [])
      ?(inherited_tools = [])
      ?(wrap_env = Fn.id)
      ?budget
      ?(child_sources = [ "child.chatmd", child_source ])
      ?(post_creation_stream =
        fun _ -> completed_response "554 Goal parent finished creating its stopped child")
      f
  =
  let first = ref true in
  with_recoverable_fixture
    ?test_root
    ?timeout
    ~wrap_env
    ~prompt_text:
      (Deferred_source_support.prompt_text
       ^ String.concat
           (List.map ("agent_create" :: parent_extra_tools) ~f:(fun name ->
              let escaped =
                String.concat_map name ~f:(function
                  | '&' -> "&amp;"
                  | '"' -> "&quot;"
                  | '<' -> "&lt;"
                  | '>' -> "&gt;"
                  | char -> String.of_char char)
              in
              sprintf "<tool name=\"%s\"/>" escaped))
       ^ String.concat (List.map inherited_tools ~f:snd))
    ~post_stream:(fun inputs ->
      if !first
      then (
        first := false;
        create_response ~tools:(List.map inherited_tools ~f:fst) child_sources)
      else post_creation_stream inputs)
    (fun env sw _root daemon _policy parent _runtime _revision counters ->
       let within phase f =
         try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. f with
         | Eio.Time.Timeout -> failwith ("native descendant test timed out: " ^ phase)
       in
       let session, before = Goal.create_goal daemon parent in
       let before =
         match budget with
         | None -> before
         | Some budget ->
           (* Freeze the actual configured policy before episode activation and
              child origin issuance. Existing callers keep the original path. *)
           Goal_admitted_guard_tests.command
             daemon
             parent
             ~key:"554-native-child-budget"
             (Change_budget (Goal.goal_id, budget))
           |> protocol_ok
           |> ignore;
           state parent
       in
       let request =
         A.Run_admission_request.Headless.create
           ~session_id:before.identity.session_id
           ~generation:before.identity.generation
           ~expected_revision:before.counters.revision
           ~mode:Workflow
           ~input:
             (User_submission
                { kind = Plain_text
                ; text = "Create the stopped child."
                ; attachments = []
                })
           ~key:(P.Idempotency_key.of_string "554-native-goal-parent" |> protocol_ok)
         |> protocol_ok
         |> fun request ->
         A.Goal_activation_request.create
           ~goal_id:Goal.goal_id
           ~expected_goal_revision:(G.revision (Goal.goal before))
           ~expected_catalog_revision:
             (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
           ~request
         |> protocol_ok
       in
       let service =
         Agent_server.Goal_run_service.create
           ~factory:(Daemon.factory daemon)
           ~authorize:(fun _ current -> authorize current)
       in
       let admission =
         Agent_server.Goal_run_service.start_episode
           service
           ~entry:parent
           ~actor:original
           ~session
           ~request
         |> protocol_ok
         |> Goal.activated
       in
       let rec await_created () =
         let current = state parent in
         let invocation =
           List.find current.invocations ~f:(fun invocation ->
             String.equal invocation.P.Invocation.context.tool_name "agent_create")
         in
         match invocation with
         | Some { status = Published (Complete value); _ }
           when Option.is_none current.active_operation -> current, value
         | Some { status = Resolved (Fail failure) | Published (Fail failure); _ } ->
           raise_s
             [%sexp "native child creation failed", (failure : P.Invocation.tool_error)]
         | Some _ | None ->
           Eio.Fiber.yield ();
           await_created ()
       in
       let current, value = within "child creation" await_created in
       let child_id =
         Jsonaf.member_exn "session_id" value
         |> Jsonaf.string_exn
         |> P.Id.Session.of_string
         |> protocol_ok
       in
       let child = Registry.find (Daemon.registry daemon) child_id |> Option.value_exn in
       let born = state child in
       assert (not (Owner.is_loaded child.runtime));
       (* Generated stopped seeds use Ready before any runtime is built. Actual
          callback evidence, rather than that authored-initialization tag, proves
          this real child script has not run. *)
       assert (List.is_empty born.moderator_executions);
       assert (Option.is_none born.moderator);
       let reference = Option.value_exn born.spec.delegation in
       let record =
         Delegations.resolve
           (Agent_store.Session_store.delegations (Daemon.store daemon))
           reference
         |> Source_preparation_tests.store_ok
       in
       let origin = Option.value_exn record.admission.goal_origin in
       let scope =
         match Origin.scopes origin with
         | [ scope ] -> scope
         | _ -> failwith "native Goal child did not retain its one genuine root"
       in
       assert (P.Session_ref.equal (Origin.Scope.owner scope) session);
       assert (P.Id.Goal.equal (Origin.Scope.goal_id scope) Goal.goal_id);
       assert (
         P.Run_receipt.equal
           (P.Goal_episode.admission (Origin.Scope.creator_episode scope))
           admission);
       let child_accounting = Option.value_exn born.goal_descendants in
       assert (Option.exists (GD.origin child_accounting) ~f:(Origin.equal origin));
       let parent_accounting = Option.value_exn current.goal_descendants in
       assert (
         List.exists
           (GD.branches parent_accounting ~goal_id:Goal.goal_id)
           ~f:(fun branch ->
             Origin.Scope.equal (GD.Branch.scope branch) scope
             && Delegations.Reference.equal (GD.Branch.reference branch) reference));
       let snapshot =
         A.Child_contributions.snapshot
           (Option.value_exn current.child_contributions)
           ~reference
         |> protocol_ok
         |> Option.value_exn
       in
       assert (
         A.Child_usage_snapshot.Partition.equal
           (A.Child_usage_snapshot.own snapshot)
           A.Child_usage_snapshot.Partition.empty);
       [%test_eq: int] 2 !(counters.providers);
       f env sw daemon parent child scope reference counters)
;;

let%expect_test
    "actual Goal native creation freezes association before child Init and confirms \
     direct Turn before worker"
  =
  let read_scope : unit Eio.Fiber.key = Eio.Fiber.create_key () in
  let read_path = ref None in
  let read_barriers = ref [] in
  let read_hits = ref 0 in
  let child_writer = ref None in
  let writer_hits = ref 0 in
  let reached, signal_reached = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let release_writer () =
    if Option.is_none (Eio.Promise.peek release)
    then Eio.Promise.resolve signal_release ()
  in
  with_native_goal_child
    ~wrap_env:(fun env ->
      let env =
        Crash_fault_io.wrap
          env
          ~boundary:Before_open_in
          ~matches:(fun path ->
            Option.is_some (Eio.Fiber.get read_scope)
            && Option.exists !read_path ~f:(String.equal path))
          ~reached:(fun _ ->
            match !read_barriers with
            | [] -> ()
            | (entered, resume) :: remaining ->
              read_barriers := remaining;
              Int.incr read_hits;
              Eio.Promise.resolve entered ();
              Eio.Promise.await resume)
      in
      Crash_fault_io.wrap
        env
        ~boundary:Before_sync
        ~matches:(fun path ->
          String.is_substring path ~substring:"/journal/"
          && String.is_suffix path ~suffix:".log")
        ~reached:(fun path ->
          if Option.is_none !child_writer
          then ()
          else (
            let scan : Agent_store.Journal_segment.scan =
              Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
              |> Agent_store.Journal_segment.scan_contents
                   ~max_payload_length:(64 * 1024 * 1024)
              |> Source_preparation_tests.store_ok
            in
            match List.last scan.entries with
            | Some (frame : Agent_store.Journal_segment.entry)
              when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
              let transaction =
                Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
                |> Source_preparation_tests.store_ok
              in
              let selected =
                Option.exists !child_writer ~f:(fun child ->
                  P.Id.Session.equal child transaction.session_id)
              in
              if selected
              then (
                let delta =
                  A.Session_delta_document.decode
                    ~limits:Document_schema.Limits.default
                    transaction.delta
                  |> document_ok
                  |> A.Session_delta_document.value
                in
                let canonical_turn =
                  Child_usage_fault_tests.Fault.leaves delta
                  |> List.exists ~f:(function
                    | A.Session_delta.Goal_descendants_changed update ->
                      (match GD.Update.reason update with
                       | Record_included { receipts; _ } ->
                         List.exists receipts ~f:(fun receipt ->
                           let occurrence =
                             A.Goal_descendant_included.pending receipt
                             |> A.Goal_descendant_pending.occurrence
                           in
                           match A.Goal_descendant_occurrence.kind occurrence with
                           | Host_turn _ -> true
                           | Attempt _ -> false)
                       | Register_new_root _
                       | Register_first_unrelated_child _
                       | Register_child _
                       | Register_branch _
                       | Register_retained_branch _
                       | Reserve _
                       | Import _
                       | Close_before_writer _ -> false)
                    | _ -> false)
                in
                if canonical_turn
                then (
                  child_writer := None;
                  Int.incr writer_hits;
                  Eio.Promise.resolve signal_reached ();
                  Eio.Promise.await release))
            | Some _ | None -> ())))
    (fun env sw daemon parent child scope reference counters ->
       let within phase f =
         try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. f with
         | Eio.Time.Timeout -> failwith ("native descendant test timed out: " ^ phase)
       in
       let child_id = (state child).identity.session_id in
       let root = Agent_store.Session_store.data_root (Daemon.store daemon) in
       let record_name =
         Delegations.Key.sexp_of_t reference.key
         |> Sexp.to_string_mach
         |> Digestif.SHA256.digest_string
         |> Digestif.SHA256.to_hex
         |> fun hash -> hash ^ ".frame"
       in
       read_path
       := Some
            (Filename.concat
               (Filename.concat (Agent_store.Data_root.path root) "delegations")
               record_name);
       (* The path selects real private-record IO; it is only a test locator,
          never an ancestry proof or authority constructor. *)
       (* Reuse the genuine Associated child and Factory-installed source host.
          This controlled actual worker isolates Turn ordering from the separate
          inference Attempt producer; it constructs no Handle or root ACK. *)
       Agent_server.Session_factory.prepare_session_start (Daemon.factory daemon) child
       |> protocol_ok;
       Owner.ensure_loaded_authenticated child.runtime ~actor:original |> protocol_ok;
       let writer, _ =
         Actor.attach child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
       in
       protect_cleanup
         ~finally:(fun () -> Actor.detach child.actor writer.id |> protocol_ok)
         (fun () ->
            Actor.start child.actor ~actor:original ~attachment_id:writer.id
            |> protocol_ok
            |> ignore;
            (* Finish the real startup moderator callback before testing a fresh
               idle Turn; concurrent metadata writes correctly invalidate capture. *)
            let rec await_startup () =
              let current =
                Actor.with_moderator_checkpoint child.actor (fun () ->
                  Actor.state child.actor)
                |> protocol_ok
              in
              if
                List.exists current.moderator_executions ~f:(fun receipt ->
                  match receipt.P.Moderator_execution.status with
                  | Failed _ | Interrupted _ -> true
                  | Running | Completed _ -> false)
              then
                raise_s
                  [%sexp
                    "unexpected terminal moderator receipt"
                  , (current.moderator_executions : P.Moderator_execution.t list)];
              if
                Option.is_none current.active_operation
                && Option.is_some current.moderator
                && (not (List.is_empty current.moderator_executions))
                && List.for_all current.moderator_executions ~f:(fun receipt ->
                  match receipt.P.Moderator_execution.status with
                  | Completed _ -> true
                  | Running | Failed _ | Interrupted _ -> false)
              then ()
              else (
                Eio.Fiber.yield ();
                await_startup ())
            in
            within "startup" await_startup;
            let entered, signal_entered = Eio.Promise.create () in
            let worker_calls = ref 0 in
            let worker =
              A.Operation_worker.create ~run:(fun ~sw:_ ~input _capabilities ->
                Int.incr worker_calls;
                let parent_now = state parent in
                let child_now = state child in
                let branches =
                  GD.branches
                    (Option.value_exn parent_now.goal_descendants)
                    ~goal_id:Goal.goal_id
                in
                let branch =
                  List.find branches ~f:(fun branch ->
                    Origin.Scope.equal (GD.Branch.scope branch) scope
                    && Delegations.Reference.equal (GD.Branch.reference branch) reference)
                  |> Option.value_exn
                in
                assert (Option.is_none (GD.Branch.pending branch));
                [%test_eq: int64] 1L (GD.Branch.confirmed_prefix branch);
                let included =
                  match GD.Branch.last branch with
                  | Some (Included included) -> included
                  | None | Some (No_writer_closed _) ->
                    failwith "worker preceded root Included ACK"
                in
                let child_heads =
                  GD.local_included (Option.value_exn child_now.goal_descendants)
                in
                assert (
                  List.equal A.Goal_descendant_included.equal [ included ] child_heads);
                let occurrence =
                  A.Goal_descendant_pending.occurrence
                    (A.Goal_descendant_included.pending included)
                in
                (match A.Goal_descendant_occurrence.kind occurrence with
                 | Host_turn { operation_id } ->
                   assert (P.Id.Operation.equal operation_id input.operation.id)
                 | Attempt _ -> failwith "direct Turn retained an inference occurrence");
                assert (
                  Option.is_some
                    (A.Inference_ledger.find_turn_handle
                       child_now.inference_ledger
                       ~operation_id:input.operation.id
                       ~generation:input.operation.generation));
                let observed =
                  A.Child_contributions.snapshot
                    (Option.value_exn parent_now.child_contributions)
                    ~reference
                  |> protocol_ok
                  |> Option.value_exn
                in
                [%test_eq: int64]
                  (A.Goal_descendant_occurrence.Frame.candidate_session_revision
                     (A.Goal_descendant_occurrence.frame occurrence))
                  (A.Child_usage_snapshot.revision observed);
                let marks =
                  A.Child_usage_snapshot.Partition.marks
                    (A.Child_usage_snapshot.own observed)
                in
                [%test_eq: int64] 1L marks.turn;
                [%test_eq: int64] 0L marks.attempt;
                let scoped =
                  A.Child_usage_snapshot.goal_scopes observed
                  |> Option.value_exn
                  |> List.hd_exn
                in
                assert (
                  Option.exists
                    (A.Child_usage_snapshot.Goal_scope.included scoped)
                    ~f:(A.Goal_descendant_included.equal included));
                Eio.Promise.resolve signal_entered ();
                A.Operation_worker.Completed
                  { final_history = input.history
                  ; runtime_requests = []
                  ; moderator_snapshot = None
                  })
            in
            Actor.set_operation_worker child.actor (Some worker) |> protocol_ok;
            let id = A.History_id_source.allocate child.history_ids |> protocol_ok in
            let message =
              Owner.parse_user_content
                child.runtime
                ~id
                { kind = Plain_text
                ; text = "554 actual direct inherited Turn"
                ; attachments = []
                }
              |> protocol_ok
              |> A.History_codec.to_protocol
            in
            let before_submit = state child in
            let submitted =
              match
                Actor.submit_message_authenticated
                  child.actor
                  ~actor:original
                  ~attachment_id:writer.id
                  message
              with
              | Ok submitted -> submitted
              | Error failure ->
                let current = state child in
                raise_s
                  [%sexp
                    "direct inherited Turn rejected"
                  , (failure : P.Error.t)
                  , (before_submit.counters.revision : int64)
                  , (current.counters.revision : int64)
                  , (current.active_operation : P.Operation.t option)]
            in
            assert (Option.is_some submitted.operation_id);
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
              Eio.Promise.await entered);
            let after =
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                Goal_native_test_support.await_quiet child)
            in
            assert (Option.is_none after.failure);
            [%test_eq: int] 2 !(counters.providers);
            (* The controlled worker intentionally has no moderator snapshot.
               Synchronize its actual checkpoint, rather than waiting for the
               replaced startup manager to publish another moderator state. *)
            let await_idle_checkpoint () =
              let current =
                Actor.with_moderator_checkpoint child.actor (fun () ->
                  Actor.state child.actor)
                |> protocol_ok
              in
              assert (Option.is_none current.active_operation)
            in
            within "post-turn checkpoint" await_idle_checkpoint;
            let second_id =
              A.History_id_source.allocate child.history_ids |> protocol_ok
            in
            let second_message =
              Owner.parse_user_content
                child.runtime
                ~id:second_id
                { kind = Plain_text
                ; text = "554 cancellation during the real canonical child writer"
                ; attachments = []
                }
              |> protocol_ok
              |> A.History_codec.to_protocol
            in
            within "parsed-input checkpoint" await_idle_checkpoint;
            child_writer := Some child_id;
            let caller, signal_caller = Eio.Promise.create () in
            let cancelled = Failure "554 direct Turn waiter cancelled during writer" in
            let submission =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Eio.Cancel.sub (fun context ->
                  Eio.Promise.resolve signal_caller context;
                  Actor.submit_message_authenticated
                    child.actor
                    ~actor:original
                    ~attachment_id:writer.id
                    second_message))
            in
            protect_cleanup ~finally:release_writer (fun () ->
              within "canonical writer barrier" (fun () -> Eio.Promise.await reached);
              [%test_eq: int] 1 !writer_hits;
              [%test_eq: int] 1 !worker_calls;
              Eio.Cancel.cancel (Eio.Promise.await caller) cancelled;
              (* Caller must finish cancellation while real writer is still
                 suspended. Releasing it later produces a genuine delayed ACK. *)
              (match Result.try_with (fun () -> Eio.Promise.await_exn submission) with
               | Error (Eio.Cancel.Cancelled cause) -> assert (phys_equal cause cancelled)
               | Error exn -> raise exn
               | Ok _ -> failwith "canonical-writer waiter did not preserve cancellation");
              release_writer ();
              let rec await_factual_cleanup () =
                let child_now = state child in
                let parent_now = state parent in
                let branch =
                  GD.branches
                    (Option.value_exn parent_now.goal_descendants)
                    ~goal_id:Goal.goal_id
                  |> List.find_exn ~f:(fun branch ->
                    Origin.Scope.equal (GD.Branch.scope branch) scope
                    && Delegations.Reference.equal (GD.Branch.reference branch) reference)
                in
                if
                  Option.is_none child_now.active_operation
                  && Option.is_none (GD.Branch.pending branch)
                  && Int64.equal (GD.Branch.confirmed_prefix branch) 2L
                then child_now, parent_now, branch
                else (
                  Eio.Fiber.yield ();
                  await_factual_cleanup ())
              in
              let child_now, parent_now, branch =
                within "delayed ACK factual cleanup" await_factual_cleanup
              in
              let included =
                match GD.Branch.last branch with
                | Some (Included included) -> included
                | None | Some (No_writer_closed _) ->
                  failwith "delayed real ACK was falsely closed as no-writer"
              in
              assert (
                List.equal
                  A.Goal_descendant_included.equal
                  [ included ]
                  (GD.local_included (Option.value_exn child_now.goal_descendants)));
              let operation_id =
                A.Goal_descendant_included.pending included
                |> A.Goal_descendant_pending.occurrence
                |> A.Goal_descendant_occurrence.kind
                |> function
                | A.Goal_descendant_occurrence.Kind.Host_turn { operation_id } ->
                  operation_id
                | Attempt _ -> failwith "delayed Turn retained an inference receipt"
              in
              assert (
                Option.is_some
                  (A.Inference_ledger.find_turn_handle
                     child_now.inference_ledger
                     ~operation_id
                     ~generation:child_now.identity.generation));
              let imported =
                A.Child_contributions.snapshot
                  (Option.value_exn parent_now.child_contributions)
                  ~reference
                |> protocol_ok
                |> Option.value_exn
              in
              let marks =
                A.Child_usage_snapshot.own imported
                |> A.Child_usage_snapshot.Partition.marks
              in
              [%test_eq: int64] 2L marks.turn;
              [%test_eq: int] 1 !worker_calls;
              [%test_eq: int] 2 !(counters.providers));
            (* The delayed ACK's factual Failed terminal returns the still-running
               session to Idle; it does not install a session failure. Exercise
               the next Turn through the same public lifecycle, with a NEW guard. *)
            within "post-cancellation checkpoint" await_idle_checkpoint;
            let recovered = state child in
            assert (Option.is_none recovered.failure);
            assert (P.Session.equal_desired_state recovered.lifecycle.desired Running);
            let one_read, signal_one_read = Eio.Promise.create () in
            let resume_one, signal_resume_one = Eio.Promise.create () in
            let first_refresh, signal_first_refresh = Eio.Promise.create () in
            let cached_change, signal_cached_change = Eio.Promise.create () in
            let cached_checked, signal_cached_checked = Eio.Promise.create () in
            let first_contended_read, signal_first_contended_read =
              Eio.Promise.create ()
            in
            let resume_first, signal_resume_first = Eio.Promise.create () in
            let second_contended_read, signal_second_contended_read =
              Eio.Promise.create ()
            in
            let resume_second, signal_resume_second = Eio.Promise.create () in
            let native_calls = ref 0 in
            let halted_checks = ref 0 in
            let selected = native_effect_registry native_calls in
            let selected_reference =
              Chat_response.Tool_capability.references selected |> List.hd_exn
            in
            let native_entered, signal_native_entered = Eio.Promise.create () in
            let begin_native, signal_begin_native = Eio.Promise.create () in
            let at_final_wait, signal_final_wait = Eio.Promise.create () in
            let release_final_wait, signal_release_final_wait = Eio.Promise.create () in
            let release promise signal =
              if Option.is_none (Eio.Promise.peek promise)
              then Eio.Promise.resolve signal ()
            in
            let parent_history_attachment : P.Id.Attachment.t option ref = ref None in
            protect_cleanup
              ~finally:(fun () ->
                read_barriers := [];
                release resume_one signal_resume_one;
                release cached_change signal_cached_change;
                release resume_first signal_resume_first;
                release resume_second signal_resume_second;
                release begin_native signal_begin_native;
                release release_final_wait signal_release_final_wait;
                Option.iter !parent_history_attachment ~f:(fun id ->
                  parent_history_attachment := None;
                  Actor.detach parent.actor id |> protocol_ok))
              (fun () ->
                 let native_worker =
                   A.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
                     Int.incr worker_calls;
                     let parent_now = state parent in
                     let branch =
                       GD.branches
                         (Option.value_exn parent_now.goal_descendants)
                         ~goal_id:Goal.goal_id
                       |> List.find_exn ~f:(fun branch ->
                         Delegations.Reference.equal
                           (GD.Branch.reference branch)
                           reference)
                     in
                     assert (Option.is_none (GD.Branch.pending branch));
                     [%test_eq: int64] 3L (GD.Branch.confirmed_prefix branch);
                     Eio.Promise.resolve signal_native_entered ();
                     Eio.Promise.await begin_native;
                     let invocation =
                       P.Invocation.create
                         { id = P.Id.Invocation.create ()
                         ; session_id = input.session_id
                         ; generation = input.session_generation
                         ; origin = External_adapter
                         ; provider_call_id = None
                         ; call_entry_id = None
                         ; parent_invocation = None
                         ; parent_job = None
                         ; tool_name = selected_reference.name
                         ; implementation_revision =
                             selected_reference.implementation_revision
                         ; capability_fingerprint =
                             Chat_response.Tool_capability.fingerprint selected
                         ; input = `Object []
                         ; created_at = input.operation.started_at
                         ; deadline = None
                         }
                       |> protocol_ok
                     in
                     let execute ~invocation f =
                       capabilities.with_invocation
                         ~invocation
                         (fun ~dispatched ~capabilities ->
                            (* The metadata commit must refresh the SAME operation
                               before Claim, rather than issuing fresh authority. *)
                            capabilities.check_effect_current ~invocation:dispatched
                            |> protocol_ok;
                            let refresh () =
                              Eio.Fiber.with_binding read_scope () (fun () ->
                                capabilities.refresh_effect_current ~invocation:dispatched)
                            in
                            read_barriers := [ signal_one_read, resume_one ];
                            refresh () |> protocol_ok;
                            [%test_eq: int] 1 !read_hits;
                            Eio.Promise.resolve signal_first_refresh ();
                            Eio.Promise.await cached_change;
                            (* A retained admitted view tolerates later ordinary
                               history without retaining its old projection epoch. *)
                            capabilities.check_effect_current ~invocation:dispatched
                            |> protocol_ok;
                            Eio.Promise.resolve signal_cached_checked ();
                            read_barriers
                            := [ signal_first_contended_read, resume_first
                               ; signal_second_contended_read, resume_second
                               ];
                            (match refresh () with
                             | Error failure ->
                               [%test_eq: P.Error.code] Conflict failure.code;
                               assert failure.retryable
                             | Ok () -> failwith "both stale observations granted a view");
                            [%test_eq: int] 3 !read_hits;
                            assert (List.is_empty !read_barriers);
                            (* The SAME live invocation has no old cached view
                               after bounded contention. A fresh refresh can succeed. *)
                            assert (
                              Result.is_error
                                (capabilities.check_effect_current ~invocation:dispatched));
                            refresh () |> protocol_ok;
                            capabilities.check_effect_current ~invocation:dispatched
                            |> protocol_ok;
                            let result = f ~dispatched ~capabilities in
                            (* This invocation is still physically executing: the
                               negative result is policy retirement, not scope exit. *)
                            assert (
                              Result.is_error
                                (capabilities.check_effect_current ~invocation:dispatched));
                            result)
                     in
                     let resolved =
                       A.Native_tool_invocation.run_scoped
                         ~execute
                         ~registry:(fun () -> selected)
                         ~reference:selected_reference
                         ~invocation
                         ~is_halted:(fun () ->
                           Int.incr halted_checks;
                           if Int.equal !halted_checks 3
                           then (
                             Eio.Promise.resolve signal_final_wait ();
                             Eio.Promise.await release_final_wait);
                           false)
                         ~authorize:(fun _ _ -> Ok ())
                         ~prepare_output:(function
                           | Text text -> Ok (`String text)
                           | _ ->
                             Error
                               (P.Error.invalid_request
                                  "native wait fixture expected text"))
                       |> protocol_ok
                     in
                     (match resolved.P.Invocation.status with
                      | Resolved (Fail failure) ->
                        [%test_eq: string] "invocation.session_ended" failure.code
                      | Admitted | Dispatching
                      | Resolved (Complete _ | Pending _ | Cancelled _)
                      | Published _ ->
                        raise_s
                          [%sexp
                            "native effect escaped inherited Hold"
                          , (resolved : P.Invocation.t)]);
                     [%test_eq: int] 3 !halted_checks;
                     [%test_eq: int] 0 !native_calls;
                     A.Operation_worker.Completed
                       { final_history = input.history
                       ; runtime_requests = []
                       ; moderator_snapshot = None
                       })
                 in
                 Actor.set_operation_worker child.actor (Some native_worker)
                 |> protocol_ok;
                 let id = A.History_id_source.allocate child.history_ids |> protocol_ok in
                 let message =
                   Owner.parse_user_content
                     child.runtime
                     ~id
                     { kind = Plain_text
                     ; text = "554 inherited Hold while native final check awaits"
                     ; attachments = []
                     }
                   |> protocol_ok
                   |> A.History_codec.to_protocol
                 in
                 within "native-input checkpoint" await_idle_checkpoint;
                 let submitted =
                   Actor.submit_message_authenticated
                     child.actor
                     ~actor:original
                     ~attachment_id:writer.id
                     message
                   |> protocol_ok
                 in
                 assert (Option.is_some submitted.operation_id);
                 within "native worker" (fun () -> Eio.Promise.await native_entered);
                 let before_metadata = state parent in
                 let reader, _ =
                   Actor.attach parent.actor ~mode:Read_only ~subscribe:false
                   |> protocol_ok
                 in
                 Actor.detach parent.actor reader.id |> protocol_ok;
                 assert (
                   Int64.(
                     (state parent).counters.revision > before_metadata.counters.revision));
                 let history_writer, _ =
                   Actor.attach parent.actor ~mode:Read_write ~subscribe:false
                   |> protocol_ok
                 in
                 parent_history_attachment := Some history_writer.id;
                 let append_parent_history text =
                   let id =
                     A.History_id_source.allocate parent.history_ids |> protocol_ok
                   in
                   let entry =
                     Owner.parse_user_content
                       parent.runtime
                       ~id
                       { kind = Plain_text; text; attachments = [] }
                     |> protocol_ok
                     |> A.History_codec.to_protocol
                   in
                   Actor.append_history
                     parent.actor
                     ~attachment_id:history_writer.id
                     [ entry ]
                   |> protocol_ok
                   |> ignore
                 in
                 release begin_native signal_begin_native;
                 within "one successful private-record read" (fun () ->
                   Eio.Promise.await one_read);
                 append_parent_history "571 metadata during first admitted read";
                 release resume_one signal_resume_one;
                 within "recaptured admitted view" (fun () ->
                   Eio.Promise.await first_refresh);
                 append_parent_history "571 metadata after successful admitted refresh";
                 release cached_change signal_cached_change;
                 within "cached semantic view" (fun () ->
                   Eio.Promise.await cached_checked);
                 within "first contended private-record read" (fun () ->
                   Eio.Promise.await first_contended_read);
                 append_parent_history "571 first bounded contention";
                 release resume_first signal_resume_first;
                 within "second contended private-record read" (fun () ->
                   Eio.Promise.await second_contended_read);
                 append_parent_history "571 second bounded contention";
                 release resume_second signal_resume_second;
                 within "final native wait" (fun () -> Eio.Promise.await at_final_wait);
                 [%test_eq: int] 3 !halted_checks;
                 [%test_eq: int] 0 !native_calls;
                 Goal_admitted_guard_tests.command
                   daemon
                   parent
                   ~key:"554-inherited-native-wait-hold"
                   (Hold (Goal.goal_id, Operator_pause))
                 |> protocol_ok
                 |> ignore;
                 assert (
                   G.State.equal
                     (G.state (Goal.goal (state parent)))
                     (Open (Held Operator_pause)));
                 release release_final_wait signal_release_final_wait;
                 let after =
                   within "held native terminal" (fun () ->
                     Goal_native_test_support.await_quiet child)
                 in
                 assert (Option.is_none after.failure);
                 within "held-native checkpoint" await_idle_checkpoint;
                 let parent_now = state parent in
                 let branch =
                   GD.branches
                     (Option.value_exn parent_now.goal_descendants)
                     ~goal_id:Goal.goal_id
                   |> List.find_exn ~f:(fun branch ->
                     Delegations.Reference.equal (GD.Branch.reference branch) reference)
                 in
                 assert (Option.is_none (GD.Branch.pending branch));
                 [%test_eq: int64] 3L (GD.Branch.confirmed_prefix branch);
                 let included =
                   match GD.Branch.last branch with
                   | Some (Included included) -> included
                   | None | Some (No_writer_closed _) ->
                     failwith "post-ACK Hold refunded the real native Turn"
                 in
                 assert (
                   List.equal
                     A.Goal_descendant_included.equal
                     [ included ]
                     (GD.local_included (Option.value_exn after.goal_descendants)));
                 [%test_eq: int] 3 !halted_checks;
                 [%test_eq: int] 0 !native_calls;
                 [%test_eq: int] 2 !worker_calls;
                 [%test_eq: int] 2 !(counters.providers))));
  print_endline
    "native Goal owner captured; parent frontier and zero usage acknowledged; child not \
     initialized; direct Turn root-confirmed before actual worker; later Goal Hold \
     fences native effect";
  [%expect
    {| native Goal owner captured; parent frontier and zero usage acknowledged; child not initialized; direct Turn root-confirmed before actual worker; later Goal Hold fences native effect |}]
;;

let%expect_test
    "retained native ACK drains before unload and shutdown; fencing stays finite"
  =
  List.iter [ `Unload; `Shutdown ] ~f:(fun mode ->
    let target = ref None in
    let reached, signal_reached = Eio.Promise.create () in
    let release, signal_release = Eio.Promise.create () in
    let release_import () =
      if Option.is_none (Eio.Promise.peek release)
      then Eio.Promise.resolve signal_release ()
    in
    with_native_goal_child
      ~wrap_env:(fun env ->
        Crash_fault_io.wrap
          env
          ~boundary:Before_sync
          ~matches:(fun path ->
            String.is_substring path ~substring:"/journal/"
            && String.is_suffix path ~suffix:".log")
          ~reached:(fun path ->
            match !target with
            | None -> ()
            | Some (parent_id, reference) ->
              let scan =
                Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
                |> Agent_store.Journal_segment.scan_contents
                     ~max_payload_length:(64 * 1024 * 1024)
                |> Source_preparation_tests.store_ok
              in
              (match List.last scan.entries with
               | Some (frame : Agent_store.Journal_segment.entry)
                 when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
                 let transaction =
                   Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
                   |> Source_preparation_tests.store_ok
                 in
                 if P.Id.Session.equal parent_id transaction.session_id
                 then (
                   let delta =
                     A.Session_delta_document.decode
                       ~limits:Document_schema.Limits.default
                       transaction.delta
                     |> document_ok
                     |> A.Session_delta_document.value
                   in
                   let imports_child =
                     Child_usage_fault_tests.Fault.leaves delta
                     |> List.exists ~f:(function
                       | A.Session_delta.Goal_descendants_changed update ->
                         (match GD.Update.reason update with
                          | Import actual -> Delegations.Reference.equal actual reference
                          | Register_new_root _
                          | Register_first_unrelated_child _
                          | Register_child _
                          | Register_branch _
                          | Register_retained_branch _
                          | Reserve _
                          | Record_included _
                          | Close_before_writer _ -> false)
                       | _ -> false)
                   in
                   if imports_child
                   then (
                     target := None;
                     Eio.Promise.resolve signal_reached ();
                     Eio.Promise.await release))
               | Some _ | None -> ())))
      (fun env sw daemon parent child _scope reference counters ->
         let within phase f =
           try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. f with
           | Eio.Time.Timeout -> failwith ("source-close test timed out: " ^ phase)
         in
         let parent_snapshot_directory =
           Option.value_exn parent.store_handle
           |> Agent_store.Session_store.Handle.snapshot_directory
         in
         Factory.prepare_session_start (Daemon.factory daemon) child |> protocol_ok;
         Owner.ensure_loaded_authenticated child.runtime ~actor:original |> protocol_ok;
         let writer, _ =
           Actor.attach child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
         in
         let attached = ref true in
         let detach () =
           if !attached
           then (
             Actor.detach child.actor writer.id |> protocol_ok;
             attached := false)
         in
         protect_cleanup
           ~finally:(fun () ->
             release_import ();
             detach ())
           (fun () ->
              Actor.start child.actor ~actor:original ~attachment_id:writer.id
              |> protocol_ok
              |> ignore;
              let rec await_startup () =
                let current =
                  Actor.with_moderator_checkpoint child.actor (fun () ->
                    Actor.state child.actor)
                  |> protocol_ok
                in
                if
                  Option.is_none current.active_operation
                  && Option.is_some current.moderator
                  && (not (List.is_empty current.moderator_executions))
                  && List.for_all current.moderator_executions ~f:(fun execution ->
                    match execution.P.Moderator_execution.status with
                    | Completed _ -> true
                    | Running | Failed _ | Interrupted _ -> false)
                then ()
                else (
                  Eio.Fiber.yield ();
                  await_startup ())
              in
              within "startup" await_startup;
              let worker_calls = ref 0 in
              let worker =
                A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
                  Int.incr worker_calls;
                  A.Operation_worker.Completed
                    { final_history = input.history
                    ; runtime_requests = []
                    ; moderator_snapshot = None
                    })
              in
              Actor.set_operation_worker child.actor (Some worker) |> protocol_ok;
              let id = A.History_id_source.allocate child.history_ids |> protocol_ok in
              let message =
                Owner.parse_user_content
                  child.runtime
                  ~id
                  { kind = Plain_text
                  ; text = "565 retained ACK source-close boundary"
                  ; attachments = []
                  }
                |> protocol_ok
                |> A.History_codec.to_protocol
              in
              Actor.with_moderator_checkpoint child.actor (fun () ->
                Actor.state child.actor)
              |> protocol_ok
              |> ignore;
              target := Some ((state parent).identity.session_id, reference);
              let caller, signal_caller = Eio.Promise.create () in
              let submission =
                Eio.Fiber.fork_promise ~sw (fun () ->
                  Eio.Cancel.sub (fun context ->
                    Eio.Promise.resolve signal_caller context;
                    Actor.submit_message_authenticated
                      child.actor
                      ~actor:original
                      ~attachment_id:writer.id
                      message))
              in
              within "actual root import writer" (fun () -> Eio.Promise.await reached);
              [%test_eq: int] 0 !worker_calls;
              (* Detach while the original confirmation already owns its actual
             observation. Later cleanup captures the resulting stable State. *)
              detach ();
              let cancelled = Failure "565 sender cancelled during root confirmation" in
              Eio.Cancel.cancel (Eio.Promise.await caller) cancelled;
              (match Result.try_with (fun () -> Eio.Promise.await_exn submission) with
               | Error (Eio.Cancel.Cancelled cause) -> assert (phys_equal cause cancelled)
               | Error exn -> raise exn
               | Ok _ -> failwith "source-close sender did not preserve cancellation");
              let rec await_terminal () =
                let current = state child in
                if Option.is_none current.active_operation
                then current
                else (
                  Eio.Fiber.yield ();
                  await_terminal ())
              in
              let current = within "retained ACK terminal" await_terminal in
              (* Public State is quiescent while genuine ACK facts still wait for
             their parent writer. Plain unload must not cancel or detach it. *)
              (match Owner.unload child.runtime with
               | Error failure ->
                 [%test_eq: P.Error.code] Conflict failure.code;
                 [%test_eq: string]
                   "cannot unload runtime with outstanding descendant admission cleanup"
                   failure.message
               | Ok () -> failwith "ordinary unload detached an unconfirmed native ACK");
              assert (Owner.is_loaded child.runtime);
              (match
                 Actor.begin_lifecycle
                   child.actor
                   ~attachment_id:None
                   ~expected_generation:current.identity.generation
                   ~expected_revision:current.counters.revision
               with
               | Error failure ->
                 [%test_eq: P.Error.code] Conflict failure.code;
                 [%test_eq: string]
                   "session descendant admission cleanup is still outstanding"
                   failure.message
               | Ok fence ->
                 Actor.abort_lifecycle child.actor fence |> protocol_ok;
                 failwith "lifecycle fence hid a genuine unfinished native ACK");
              let closing =
                Eio.Fiber.fork_promise ~sw (fun () ->
                  match mode with
                  | `Unload -> Owner.unload_and_wait child.runtime |> protocol_ok
                  | `Shutdown -> Registry.shutdown (Daemon.registry daemon))
              in
              (match mode with
               | `Unload -> Eio.Fiber.yield ()
               | `Shutdown ->
                 let rec await_closing () =
                   if Registry.is_closing (Daemon.registry daemon)
                   then ()
                   else (
                     Eio.Fiber.yield ();
                     await_closing ())
                 in
                 within "registry closing flag" await_closing);
              assert (Option.is_none (Eio.Promise.peek closing));
              release_import ();
              within "source-close factual drain" (fun () ->
                Eio.Promise.await_exn closing);
              [%test_eq: int] 0 !worker_calls;
              [%test_eq: int] 2 !(counters.providers);
              let final =
                match mode with
                | `Unload ->
                  assert (not (Owner.is_loaded child.runtime));
                  state parent
                | `Shutdown ->
                  assert (
                    Option.is_none
                      (Registry.find
                         (Daemon.registry daemon)
                         reference.key.parent_session_id));
                  let installed =
                    Agent_store.Snapshot.load_current
                      ~env
                      ~directory:parent_snapshot_directory
                      ~max_payload_length:(64 * 1024 * 1024)
                    |> Source_preparation_tests.store_ok
                    |> Option.value_exn
                  in
                  A.Session_persistence.restore_snapshot
                    ~limits:Document_schema.Limits.default
                    installed.snapshot
                  |> Source_preparation_tests.store_ok
                  |> A.Session_persistence.Restored.state
              in
              let branch =
                GD.branches
                  (Option.value_exn final.goal_descendants)
                  ~goal_id:Goal.goal_id
                |> List.find_exn ~f:(fun branch ->
                  Delegations.Reference.equal (GD.Branch.reference branch) reference)
              in
              assert (Option.is_none (GD.Branch.pending branch));
              [%test_eq: int64] 1L (GD.Branch.confirmed_prefix branch);
              (match GD.Branch.last branch with
               | Some (Included _) -> ()
               | None | Some (No_writer_closed _) ->
                 failwith "source close refunded an acknowledged native Turn");
              print_endline
                (match mode with
                 | `Unload ->
                   "unload drained genuine ACK; finite unload and eviction refused \
                    pending facts"
                 | `Shutdown ->
                   "shutdown retained complete factual graph under closing and \
                    checkpointed Included"))));
  [%expect
    {|
    unload drained genuine ACK; finite unload and eviction refused pending facts
    shutdown retained complete factual graph under closing and checkpointed Included
    |}]
;;

(* No moderator script overrides the standard Factory Turn worker. A direct
   user Turn therefore performs its real foreground inference without adding a
   generated tool/recipe capability. Init remains effect-free. *)
let attempt_sources =
  [ ( "child.chatmd"
    , {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>554 actual inherited foreground inference, stopped before initialization.</developer>|}
    )
  ]
;;

(* The probe reads ACKed native owners at the backend's actual post-guard
   boundary. It never constructs an allocation, reservation or confirmation. *)
module Provider_probe = struct
  module L = A.Inference_ledger
  module S = A.Child_usage_snapshot

  type t =
    { parent : Registry.entry
    ; child : Registry.entry
    ; scope : Origin.Scope.t
    ; reference : Delegations.Reference.t
    ; parent_attempts_before : int64
    ; turns : int64
    ; attempts : int64
    ; attempt_ceiling : int64
    ; mutable submitted : (A.Goal_descendant_included.t * L.Handle.t) option
    }

  let create_with_counts
        ~turns
        ~attempts
        ~attempt_ceiling
        ~parent
        ~child
        ~scope
        ~reference
        ~parent_attempts_before
    =
    { parent
    ; child
    ; scope
    ; reference
    ; parent_attempts_before
    ; turns
    ; attempts
    ; attempt_ceiling
    ; submitted = None
    }
  ;;

  let create ~parent ~child ~scope ~reference ~parent_attempts_before =
    create_with_counts
      ~turns:1L
      ~attempts:1L
      ~attempt_ceiling:3L
      ~parent
      ~child
      ~scope
      ~reference
      ~parent_attempts_before
  ;;

  let branch t parent =
    GD.branches
      (Option.value_exn parent.A.Session_state.goal_descendants)
      ~goal_id:Goal.goal_id
    |> List.find_exn ~f:(fun branch ->
      Origin.Scope.equal (GD.Branch.scope branch) t.scope
      && Delegations.Reference.equal (GD.Branch.reference branch) t.reference)
  ;;

  let included branch =
    match GD.Branch.last branch with
    | Some (Included included) -> included
    | None | Some (No_writer_closed _) ->
      failwith "real provider preceded root Included Attempt confirmation"
  ;;

  let assert_marks t partition =
    let marks = S.Partition.marks partition in
    [%test_eq: int64] t.turns marks.turn;
    [%test_eq: int64] t.attempts marks.attempt;
    [%test_eq: int64] 0L marks.untracked_turns;
    [%test_eq: int64] 0L marks.untracked_attempts
  ;;

  let checked_root_snapshot t parent receipt =
    let account = Goal.accounting parent in
    [%test_eq: int64]
      t.parent_attempts_before
      (A.Goal_accounting.admitted_attempts account);
    [%test_eq: int64] 1L (A.Goal_accounting.admitted_turns account);
    let goal = Goal.goal parent in
    assert (G.State.equal (G.state goal) (Open Enabled));
    assert (
      Option.equal
        Int64.equal
        (G.Budget.admitted_attempts (G.budget goal))
        (Some t.attempt_ceiling));
    let children = Option.value_exn parent.child_contributions in
    let observed =
      A.Child_contributions.snapshot children ~reference:t.reference
      |> protocol_ok
      |> Option.value_exn
    in
    let occurrence =
      A.Goal_descendant_included.pending receipt |> A.Goal_descendant_pending.occurrence
    in
    assert (
      Int64.(
        S.revision observed
        >= A.Goal_descendant_occurrence.Frame.candidate_session_revision
             (A.Goal_descendant_occurrence.frame occurrence)));
    assert_marks t (S.own observed);
    let scoped =
      S.goal_scopes observed
      |> Option.value_exn
      |> List.find_exn ~f:(fun entry ->
        Origin.Scope.equal (S.Goal_scope.scope entry) t.scope)
    in
    assert_marks t (S.Goal_scope.own scoped);
    assert (S.Partition.equal (S.own observed) (S.Goal_scope.own scoped));
    assert (
      Option.exists
        (S.Goal_scope.included scoped)
        ~f:(A.Goal_descendant_included.equal receipt));
    let total =
      A.Child_contributions.aggregate_root
        children
        ~owner:(G.session goal)
        ~goal_id:Goal.goal_id
      |> protocol_ok
    in
    assert_marks t (S.Descendants.partition total);
    let coverage = S.Descendants.coverage total in
    assert (not (S.Coverage.historical_children_unknown coverage));
    [%test_eq: int64] 0L (S.Coverage.unobserved_children coverage);
    let available ceiling =
      GD.available
        (Option.value_exn parent.goal_descendants)
        ~children
        ~goal_id:Goal.goal_id
        ~local_count:(A.Goal_accounting.admitted_attempts account)
        ~ceiling:(Some ceiling)
        ~dimension:Attempt
      |> protocol_ok
    in
    let boundary = Int64.(t.parent_attempts_before + t.attempts) in
    assert (GD.Availability.equal (available boundary) Exhausted);
    assert (GD.Availability.equal (available (Int64.succ boundary)) Available);
    observed
  ;;

  let before_submit t =
    assert (Option.is_none t.submitted);
    let parent = state t.parent in
    let child = state t.child in
    (* DirectSubmit's actual operation has no selected local Goal. Inherited
       root policy remains genuinely bound without fabricating local attribution. *)
    assert (Option.is_none child.goal_catalog);
    assert (Option.is_none child.goal_accounts);
    let branch = branch t parent in
    assert (Option.is_none (GD.Branch.pending branch));
    [%test_eq: int64] Int64.(t.turns + t.attempts) (GD.Branch.confirmed_prefix branch);
    let receipt = included branch in
    assert (
      List.equal
        A.Goal_descendant_included.equal
        [ receipt ]
        (GD.local_included (Option.value_exn child.goal_descendants)));
    let occurrence =
      A.Goal_descendant_included.pending receipt |> A.Goal_descendant_pending.occurrence
    in
    let ordinal, operation_id, invocation_id =
      match A.Goal_descendant_occurrence.kind occurrence with
      | Attempt { ordinal; operation_id; invocation_id } ->
        ordinal, operation_id, invocation_id
      | Host_turn _ -> failwith "real provider only had a Turn receipt"
    in
    let operation = Option.value_exn child.active_operation in
    assert (Option.equal P.Id.Operation.equal operation_id (Some operation.id));
    assert (Option.is_none invocation_id);
    assert (List.is_empty child.jobs);
    let row = L.find child.inference_ledger ~ordinal |> Option.value_exn in
    let handle = L.Row.handle row in
    assert (P.Id.Session.equal (L.Handle.session_id handle) child.identity.session_id);
    [%test_eq: int] child.identity.generation (L.Handle.generation handle);
    assert (Option.equal P.Id.Operation.equal (L.Handle.operation_id handle) operation_id);
    assert (
      Option.equal P.Id.Invocation.equal (L.Handle.invocation_id handle) invocation_id);
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Running -> ()
     | Prepared | Terminal _ | Interrupted _ ->
       failwith "provider did not follow its actual Running canonical Attempt ACK");
    (* Graph_tracking acknowledges Running before Attempt.run. The fixture backend
       checks the composed submission guard immediately before this callback. *)
    (* Confirmation imports the actual captured snapshot, which may include
       later metadata. Its revision must cover the canonical edge; equality with
       that edge would incorrectly reject legitimate same-count newer facts. The
       exact Included, canonical Handle and all scoped counts remain mandatory. *)
    checked_root_snapshot t parent receipt |> ignore;
    t.submitted <- Some (receipt, handle)
  ;;

  let after_terminal t =
    let receipt, handle = Option.value_exn t.submitted in
    let child = state t.child in
    let parent = state t.parent in
    let branch = branch t parent in
    assert (Option.is_none (GD.Branch.pending branch));
    [%test_eq: int64] Int64.(t.turns + t.attempts) (GD.Branch.confirmed_prefix branch);
    assert (A.Goal_descendant_included.equal (included branch) receipt);
    assert (
      List.equal
        A.Goal_descendant_included.equal
        [ receipt ]
        (GD.local_included (Option.value_exn child.goal_descendants)));
    let row =
      L.find child.inference_ledger ~ordinal:(L.Handle.ordinal handle) |> Option.value_exn
    in
    assert (L.Handle.equal (L.Row.handle row) handle);
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Terminal terminal ->
       assert (
         Inference.Event.Terminal.equal_outcome
           (Inference.Event.Terminal.outcome terminal)
           Completed)
     | Prepared | Running | Interrupted _ ->
       failwith "actual inherited provider Attempt did not complete");
    let actual = A.Child_accounting.snapshot child |> protocol_ok |> Option.value_exn in
    assert_marks t (S.own actual);
    let scoped = S.goal_scopes actual |> Option.value_exn |> List.hd_exn in
    assert_marks t (S.Goal_scope.own scoped);
    checked_root_snapshot t parent receipt |> ignore
  ;;
end

let%expect_test "actual Factory provider follows inherited Attempt root confirmation once"
  =
  let provider_probe = ref None in
  let probe_failure = ref None in
  let budget =
    G.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some 3L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  with_native_goal_child
    ~budget
    ~child_sources:attempt_sources
    ~post_creation_stream:(fun _ ->
      match !provider_probe with
      | Some probe ->
        (try Provider_probe.before_submit probe with
         | exn ->
           let backtrace = Stdlib.Printexc.get_raw_backtrace () in
           probe_failure := Some (exn, backtrace);
           Exn.raise_with_original_backtrace exn backtrace);
        completed_response "554 actual inherited child provider completed"
      | None -> completed_response "554 Goal parent completed native child creation")
    (fun env _sw daemon parent child scope reference counters ->
       let within phase f =
         try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. f with
         | Eio.Time.Timeout -> failwith ("inherited Attempt timed out: " ^ phase)
       in
       Actor.with_moderator_checkpoint parent.actor (fun () -> Ok ()) |> protocol_ok;
       let parent_attempts_before =
         A.Goal_accounting.admitted_attempts (Goal.accounting (state parent))
       in
       [%test_eq: int64] 2L parent_attempts_before;
       let probe =
         Provider_probe.create ~parent ~child ~scope ~reference ~parent_attempts_before
       in
       Agent_server.Session_factory.prepare_session_start (Daemon.factory daemon) child
       |> protocol_ok;
       Owner.ensure_loaded_authenticated child.runtime ~actor:original |> protocol_ok;
       let writer, subscriber =
         Actor.attach child.actor ~mode:Read_write ~subscribe:true |> protocol_ok
       in
       protect_cleanup
         ~finally:(fun () -> Actor.detach child.actor writer.id |> protocol_ok)
         (fun () ->
            Actor.start child.actor ~actor:original ~attachment_id:writer.id
            |> protocol_ok
            |> ignore;
            let assert_legacy_idle () =
              let current =
                Actor.with_moderator_checkpoint child.actor (fun () ->
                  Actor.state child.actor)
                |> protocol_ok
              in
              (* start_internal returns only after its exact Starting -> Idle
                 lifecycle ACK. This source has no moderator Init/event owner. *)
              assert (P.Session.equal_desired_state current.lifecycle.desired Running);
              (match current.lifecycle.observed with
               | Idle -> ()
               | Stopped
               | Queued_for_slot
               | Starting
               | Recovering
               | Running_turn _
               | Compacting _
               | Waiting_for_permission _
               | Stopping
               | Failed _ -> failwith "actual legacy child did not reach Idle");
              assert (Option.is_none current.active_operation);
              assert (Option.is_none current.failure);
              assert (not current.halted);
              assert (Option.is_none current.moderator);
              assert (List.is_empty current.moderator_executions);
              assert (List.is_empty current.jobs);
              match current.runtime_initialization with
              | Ready -> ()
              | Pending _ ->
                failwith "actual legacy child initialization remained pending"
            in
            assert_legacy_idle ();
            [%test_eq: int] 2 !(counters.providers);
            let id = A.History_id_source.allocate child.history_ids |> protocol_ok in
            let message =
              Owner.parse_user_content
                child.runtime
                ~id
                { kind = Plain_text
                ; text = "Use actual Factory inference after root ACK."
                ; attachments = []
                }
              |> protocol_ok
              |> A.History_codec.to_protocol
            in
            assert_legacy_idle ();
            provider_probe := Some probe;
            let submitted =
              Actor.submit_message_authenticated
                child.actor
                ~actor:original
                ~attachment_id:writer.id
                message
              |> protocol_ok
            in
            assert (Option.is_some submitted.operation_id);
            let final =
              within "actual provider terminal" (fun () ->
                Goal_native_test_support.await_quiet child)
            in
            assert (Option.is_none final.failure);
            Actor.with_moderator_checkpoint child.actor (fun () -> Ok ()) |> protocol_ok;
            Option.iter !probe_failure ~f:(fun (exn, backtrace) ->
              Exn.raise_with_original_backtrace exn backtrace);
            if Option.is_none probe.submitted
            then (
              let subscriber = Option.value_exn subscriber in
              let rec failed_operation () =
                match A.Subscriber.take subscriber with
                | Some (Ok (Durable event))
                  when P.Event.Durable.equal_kind event.kind Operation_failed ->
                  event.payload
                | Some (Ok (Durable _)) | Some (Ok (Recoverable _)) -> failed_operation ()
                | Some (Error error) -> raise_s [%sexp (error : P.Error.t)]
                | None -> failwith "subscriber closed before failed operation"
              in
              let failure = within "failed operation diagnostic" failed_operation in
              raise_s [%sexp "actual provider was not reached", (failure : Jsonaf.t)]);
            Provider_probe.after_terminal probe;
            [%test_eq: int] 3 !(counters.providers)));
  print_endline
    "actual inherited Attempt root-confirmed before provider; final allowance consumed \
     once";
  [%expect
    {| actual inherited Attempt root-confirmed before provider; final allowance consumed once |}]
;;
