open! Core
open Agent_server_test_support
open Retry_support
open Descendant_turn_adapter_tests
module Common = Descendant_parent_progress_tests
module Included = A.Goal_descendant_included
module Pending = A.Goal_descendant_pending
module Store = Agent_store.Session_store
module Physical = Constructor_model_fault_tests.Physical
module Report = Agent_server.Session_disposal_report

exception Observed_unconfirmed

let%expect_test "lost return from the actual child Attempt writer cannot be reevaluated" =
  let text = "741 managed child acknowledgement fault" in
  let queued = ref None in
  let selected = ref None in
  let written = ref [] in
  let parent_retired, resume_child = Eio.Promise.create () in
  let fault, report_fault = Eio.Promise.create () in
  let dispatched = ref 0 in
  let allow_child () =
    if not (Eio.Promise.is_resolved parent_retired)
    then Eio.Promise.resolve resume_child ()
  in
  let capacity_source request =
    if Common.owns_input text request then Eio.Promise.await parent_retired;
    Inference_runtime.Context_capacity.Source.create
      ~encoder:Openai_responses_v1
      ~read:(fun () -> Incomplete Counter_unavailable)
  in
  let budget =
    P.Goal.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some 3L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  (try
     Birth.with_native_goal_child
       ~timeout:180.
       ~budget
       ~capacity_source
       ~child_sources:Birth.attempt_sources
       ~parent_extra_tools:[ "agent_send" ]
       ~wrap_env:(fun env ->
         Crash_fault_io.wrap
           env
           ~boundary:After_sync
           ~matches:(fun path ->
             Option.is_some !selected
             && String.is_substring path ~substring:"/journal/"
             && String.is_suffix path ~suffix:".log")
           ~reached:(fun path ->
             let child = Option.value_exn !selected in
             Option.iter (Common.transaction env path) ~f:(fun transaction ->
               if
                 P.Id.Session.equal
                   transaction.session_id
                   (P.Session_ref.session_id child)
               then
                 List.iter (Common.reasons transaction) ~f:(function
                   | Record_included { receipts; _ } ->
                     List.iter receipts ~f:(fun receipt ->
                       let occurrence = Pending.occurrence (Included.pending receipt) in
                       if Common.is_attempt_of occurrence child
                       then (
                         written := receipt :: !written;
                         if not (Eio.Promise.is_resolved fault)
                         then (
                           Eio.Promise.resolve report_fault receipt;
                           raise
                             (Core_unix.Unix_error
                                (EIO, "child attempt acknowledgement", path)))))
                   | _ -> ()))))
       ~post_creation_stream:(fun inputs ->
         if Common.is_child_input text inputs
         then (
           Int.incr dispatched;
           completed_response "741 unexpected child provider effect")
         else (
           match !queued with
           | Some arguments ->
             queued := None;
             function_response "agent_send" arguments
           | None -> completed_response "741 managing parent completed"))
       (fun env _sw daemon parent child scope reference _ ->
          Descendant_ancestry_support.start_legacy daemon child;
          let child_owner =
            A.Goal_descendants.session (Option.value_exn (state child).goal_descendants)
          in
          let key = P.Idempotency_key.of_string "741-uncertain-attempt" |> protocol_ok in
          protect_cleanup
            ~finally:(fun () ->
              allow_child ();
              selected := None)
            (fun () ->
               selected := Some child_owner;
               queued
               := Some
                    (`Object
                        [ "session_id", P.Id.Session.to_json reference.child_session_id
                        ; "idempotency_key", P.Idempotency_key.to_json key
                        ; "message", `String text
                        ]);
               Goal.ordinary
                 daemon
                 parent
                 (request parent "741-fault-parent" "Send once.")
               |> admitted
               |> ignore;
               within
                 ~seconds:30.
                 env
                 "fault parent reports actual managed admission"
                 (fun () -> Goal_native_test_support.await_quiet parent)
               |> ignore;
               let receipt = managed child reference key in
               let operation =
                 match receipt.status with
                 | Assigned operation -> operation
                 | status -> raise_s [%sexp (status : M.status)]
               in
               allow_child ();
               let persisted =
                 within env "actual child Attempt fsync" (fun () ->
                   Eio.Promise.await fault)
               in
               let actual = Included.pending persisted in
               (match A.Goal_descendant_occurrence.kind (Pending.occurrence actual) with
                | Attempt { operation_id = Some actual; invocation_id = None; _ } ->
                  assert (P.Id.Operation.equal operation actual)
                | Attempt _ | Host_turn _ -> failwith "fault selected another writer");
               (* The poisoned writer cannot persist even a terminal operation.
               Join actual owner closure without demanding a fabricated Idle ACK. *)
               let current = state child in
               assert (
                 Option.exists current.active_operation ~f:(fun active ->
                   P.Id.Operation.equal active.id operation));
               [%test_eq: int64] 0L (marks child).attempt;
               let physical =
                 Uncertain_writer_test_support.physical_inventory child ~env
               in
               let retirement = Registry.close_entry_reported child in
               assert (not (Report.disposed retirement));
               assert (
                 List.exists (Report.failures retirement) ~f:(function
                   | Protocol
                       { code = Conflict
                       ; message =
                           "child seal requires the same unattempted open preparation"
                       ; _
                       } -> true
                   | Protocol _ | Raised _ -> false));
               assert (List.equal Included.equal [ persisted ] !written);
               [%test_eq: int] 0 !dispatched;
               let root = branch parent scope reference in
               (match GD.Branch.pending root, GD.Branch.last root with
                | Some pending, _ -> assert (Pending.equal pending actual)
                | None, Some (Included included) ->
                  assert (Included.equal included persisted)
                | None, (None | Some (No_writer_closed _)) ->
                  failwith
                    "possibly written Attempt was unwound as an unstarted reservation");
               let handle = Option.value_exn child.store_handle in
               let snapshot =
                 Physical.current_snapshot
                   env
                   ~directory:(Store.Handle.snapshot_directory handle)
               in
               let recovered =
                 Physical.recovery_reader
                   env
                   handle
                   ~session_id:current.identity.session_id
                   ~initial_snapshot:snapshot.snapshot
                   ()
               in
               let recovered = A.Session_persistence.Restored.state recovered.state in
               let descendants = Option.value_exn recovered.goal_descendants in
               assert (
                 List.equal Included.equal [ persisted ] (GD.local_included descendants));
               [%test_eq: int] 1 (List.length (L.rows recovered.inference_ledger));
               [%test_eq: (string * string) list * (string * string) list]
                 physical
                 (Uncertain_writer_test_support.physical_inventory child ~env);
               (* This one retained owner cannot complete a graceful fixture close.
               Its open journal has survived actual retirement and strict replay. *)
               raise Observed_unconfirmed))
   with
   | Observed_unconfirmed -> ());
  print_endline
    "actual child Attempt fsynced; no substitute Attempt and no provider effect";
  [%expect
    {| actual child Attempt fsynced; no substitute Attempt and no provider effect |}]
;;
