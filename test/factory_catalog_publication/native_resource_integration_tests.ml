open! Core
open Agent_server_test_support
open Retry_support
module Existing = Delegated_load_tests
module Warm = Delegated_warm_resource_tests
module B = A.Runtime_builder
module Store = Agent_store.Session_store

module Order = struct
  type t =
    | Loaded_first
    | Native_first
end

let same_native previous current =
  assert (
    phys_equal
      (Option.value_exn previous.B.moderator_manager)
      (Option.value_exn current.B.moderator_manager));
  assert (
    phys_equal
      (Option.value_exn previous.native_runtime)
      (Option.value_exn current.native_runtime));
  assert (
    phys_equal
      (Option.value_exn previous.moderator_script_tools)
      (Option.value_exn current.moderator_script_tools));
  assert (
    Option.equal
      Agent_store.Prompt_source_receipt.equal
      (A.Prompt_source_lease.receipt previous.source_lease)
      (A.Prompt_source_lease.receipt current.source_lease))
;;

let no_lifecycle_replay before current =
  assert (
    List.equal
      P.Moderator_execution.equal
      before.A.Session_state.moderator_executions
      current.A.Session_state.moderator_executions);
  assert (P.Run_source.equal (Warm.source before) (Warm.source current));
  assert (Option.equal Jsonaf.exactly_equal before.moderator current.moderator);
  match current.runtime_initialization with
  | Ready -> ()
  | Pending _ -> failwith "635 native source preparation changed genuine initialization"
;;

module Policy_cancellation = struct
  type phase =
    | Before_construction
    | Constructing_seen
    | Published_wait

  let run ~phase env daemon (parent : Registry.entry) expected ~original =
    Cold_policy_resource_tests.with_native_request
      ~phase
      env
      daemon
      ~target_actor:parent.actor
      expected
      ~original
      ~f:(fun native ->
        Eio.Switch.run (fun sw ->
          let publication = ref Before_construction in
          let reached, reach = Eio.Promise.create () in
          let released, release = Eio.Promise.create () in
          let release_wait () =
            if Option.is_none (Eio.Promise.peek released)
            then Eio.Promise.resolve release ()
          in
          let sentinel = Failure "635 actual published policy caller cancellation" in
          let active_context = ref None in
          let attempt =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Existing.capture_load (fun () ->
                Eio.Cancel.sub (fun context ->
                  active_context := Some context;
                  Exn.protect
                    ~finally:(fun () -> active_context := None)
                    ~f:(fun () ->
                      let authorize () =
                        let open Result.Let_syntax in
                        let%bind () = native.authorize () in
                        (* Begin and construction checks call this outside the Actor.
                       The exact live phase, not a call ordinal, selects the wait. *)
                        match
                          Actor.with_quiescent_state parent.actor ~f:(fun _ -> Ok ())
                        with
                        | Error { code = Conflict; message; _ }
                          when String.equal
                                 message
                                 "policy restoration owns runtime construction" ->
                          (match !publication with
                           | Before_construction | Constructing_seen ->
                             publication := Constructing_seen;
                             Ok ()
                           | Published_wait ->
                             failwith "635 published policy returned to construction")
                        | Ok (Some ()) ->
                          (match !publication with
                           | Before_construction -> Ok ()
                           | Constructing_seen ->
                             publication := Published_wait;
                             Eio.Promise.resolve reach context;
                             Eio.Promise.await released;
                             Ok ()
                           | Published_wait ->
                             failwith "635 canceled policy resumed publication")
                        | Ok None -> failwith "635 policy phase was not quiescent"
                        | Error error -> Error error
                      in
                      Owner.prepare_delegated_tool
                        parent.runtime
                        ~request:native.request
                        ~governance:native.governance
                        ~delegation:native.delegation
                        ~event:(Pre_tool_call native.request.call)
                        ~authorize))))
          in
          Creation_tests.protect_cleanup
            ~finally:(fun () ->
              Option.iter !active_context ~f:(fun context ->
                Eio.Cancel.cancel context sentinel);
              release_wait ();
              Eio.Cancel.protect (fun () ->
                Eio.Time.with_timeout_exn env#clock 10. (fun () ->
                  Eio.Promise.await_exn attempt |> ignore)))
            (fun () ->
               let context =
                 Eio.Fiber.first
                   (fun () -> Eio.Promise.await reached)
                   (fun () ->
                      match Eio.Promise.await_exn attempt with
                      | Raised (exn, backtrace) ->
                        Exn.raise_with_original_backtrace exn backtrace
                      | Returned (Error error) ->
                        raise_s
                          [%sexp
                            "635 policy returned before publication", (error : P.Error.t)]
                      | Returned (Ok _) ->
                        failwith "635 policy completed before publication cancellation")
               in
               assert (not (Owner.is_loaded parent.runtime));
               assert (not (Owner.source_lane_idle parent.runtime));
               no_lifecycle_replay expected (Actor.state parent.actor |> protocol_ok);
               Eio.Cancel.cancel context sentinel;
               (match Eio.Promise.await_exn attempt with
                | Raised (Eio.Cancel.Cancelled cause, _) ->
                  assert (phys_equal cause sentinel)
                | Raised (exn, backtrace) ->
                  Exn.raise_with_original_backtrace exn backtrace
                | Returned (Error error) ->
                  raise_s
                    [%sexp
                      "635 policy cancellation returned an error", (error : P.Error.t)]
                | Returned (Ok _) -> failwith "635 policy cancellation was lost");
               (* B still owns a native background lease, so the whole lane is
                  not idle. The next real policy call and ordinary adoption must
                  succeed; both reject a leaked construction stage. *)
               assert (not (Owner.source_lane_idle parent.runtime)))))
  ;;
end

let%expect_test
    "registered Owned native resource survives ordinary promotion and repeated \
     re-adoption"
  =
  List.iter [ Order.Loaded_first; Native_first ] ~f:(fun order ->
    let io = Warm.Handoff_io.create () in
    let selected = ref None in
    let native_preparations = ref 0 in
    let fail_native_preparation = ref false in
    let failed_native_preparations = ref 0 in
    let preparation_failure = Failure "648 actual cold parent Native preparation" in
    let preparation_trace = ref None in
    Existing.with_child
      ~wrap_env:(Warm.Handoff_io.wrap io)
      ~on_inference_ports:(fun actor ->
        if Option.exists !selected ~f:(phys_equal actor)
        then (
          Int.incr native_preparations;
          if !fail_native_preparation
          then (
            fail_native_preparation := false;
            Int.incr failed_native_preparations;
            try raise preparation_failure with
            | exn ->
              let trace = Stdlib.Printexc.get_raw_backtrace () in
              preparation_trace := Some trace;
              Exn.raise_with_original_backtrace exn trace)))
      (fun env _sw daemon parent child _gate providers ->
         let within phase f =
           try Eio.Time.with_timeout_exn env#clock 10. f with
           | Eio.Time.Timeout -> failwith ("635 resource integration timed out: " ^ phase)
         in
         Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
         let initial = Warm.loaded parent in
         Ordinary_warm_resource_tests.assert_live_source initial;
         let before = Warm.settled parent in
         [%test_eq: int] 1 (List.length (Warm.completed before Session_start));
         io.cache
         <- Some
              (Filename.concat
                 (Store.Handle.cache_directory
                    (Option.value_exn parent.Registry.store_handle))
                 "cache.bin");
         within "unload real registered child" (fun () ->
           Owner.unload_and_wait child.runtime |> protocol_ok);
         assert (not (Owner.is_loaded child.runtime));
         (match order with
          | Loaded_first -> assert (Owner.is_loaded parent.runtime)
          | Native_first ->
            within
              "close original parent Product before native-only dependency"
              (fun () -> Owner.unload_and_wait parent.runtime |> protocol_ok);
            assert (not (Owner.is_loaded parent.runtime));
            [%test_eq: int] 1 io.cache_renames;
            (match A.Prompt_source_lease.availability initial.source_lease with
             | Closed -> ()
             | Missing_receipt | Live | Unavailable _ | Revoked ->
               failwith "635 native-first setup did not close the original source loan");
            selected := Some parent.actor);
         let prior_closes = io.cache_renames in
         (match order with
          | Loaded_first -> ()
          | Native_first ->
            (* The real parent's Native setup is entered before its inference
               ports are requested. This fails before Prepared_native/pin or the
               delegated child's Driver callback can return a runtime. *)
            fail_native_preparation := true;
            let recording = Stdlib.Printexc.backtrace_status () in
            Creation_tests.protect_cleanup
              ~finally:(fun () ->
                fail_native_preparation := false;
                Stdlib.Printexc.record_backtrace recording)
              (fun () ->
                 Stdlib.Printexc.record_backtrace true;
                 let failed =
                   within "actual cold parent preparation before child runtime" (fun () ->
                     Existing.capture_load (fun () -> Owner.ensure_loaded child.runtime))
                 in
                 match failed with
                 | Raised (actual, trace) ->
                   assert (phys_equal actual preparation_failure);
                   let expected =
                     Stdlib.Printexc.raw_backtrace_to_string
                       (Option.value_exn !preparation_trace)
                   in
                   assert (not (String.is_empty expected));
                   assert (
                     String.is_prefix
                       (Stdlib.Printexc.raw_backtrace_to_string trace)
                       ~prefix:expected)
                 | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
                 | Returned (Ok ()) ->
                   failwith "648 failing native preparation returned a child runtime");
            [%test_eq: int] 1 !failed_native_preparations;
            [%test_eq: int] 1 !native_preparations;
            assert (not (Owner.is_loaded child.runtime));
            assert (not (Owner.is_loaded parent.runtime));
            [%test_eq: int] prior_closes io.cache_renames;
            no_lifecycle_replay before (Warm.settled parent);
            [%test_eq: int] 0 !providers;
            let finish_partial phase runtime =
              within phase (fun () ->
                let outcome =
                  Existing.capture_load (fun () -> Owner.unload_and_wait runtime)
                in
                (match outcome with
                 | Returned (Ok ()) -> ()
                 | Returned (Error error) ->
                   print_s [%sexp (phase : string), (error : P.Error.t)]
                 | Raised (exn, _) -> print_s [%sexp (phase : string), (exn : exn)]);
                Existing.restore outcome)
            in
            finish_partial "finish child construction partial" child.runtime;
            finish_partial "finish parent native setup partial" parent.runtime;
            assert (not (Owner.is_loaded child.runtime));
            assert (not (Owner.is_loaded parent.runtime));
            [%test_eq: int] prior_closes io.cache_renames;
            no_lifecycle_replay before (Warm.settled parent);
            [%test_eq: int] 0 !providers);
         within "reload child through its genuine Owned source" (fun () ->
           Owner.ensure_loaded child.runtime |> protocol_ok);
         assert (Owner.is_loaded child.runtime);
         (match order with
          | Loaded_first -> assert (Owner.is_loaded parent.runtime)
          | Native_first -> assert (not (Owner.is_loaded parent.runtime)));
         no_lifecycle_replay before (Warm.settled parent);
         [%test_eq: int] 0 !providers;
         [%test_eq: int] prior_closes io.cache_renames;
         (match order with
          | Loaded_first ->
            within "ordinary parent adoption of retained native unit" (fun () ->
              Owner.ensure_loaded parent.runtime |> protocol_ok)
          | Native_first ->
            [%test_eq: int] 2 !native_preparations;
            let before_policy = Warm.settled parent in
            assert (
              List.is_empty
                (Delegated_policy_caller_support.policy_receipts before_policy));
            let original =
              Operator_authorization.trusted_local
                (Agent_server_test_support.principal ())
            in
            let phase = ref "635 native-backed policy" in
            let retained_b = Warm.loaded child in
            Ordinary_warm_resource_tests.assert_live_source retained_b;
            within "cancel actual published policy while child retains B" (fun () ->
              Policy_cancellation.run ~phase env daemon parent before_policy ~original);
            assert (not (Owner.is_loaded parent.runtime));
            assert (Owner.is_loaded child.runtime);
            Ordinary_warm_resource_tests.assert_live_source retained_b;
            let after_cancel = Warm.settled parent in
            no_lifecycle_replay before_policy after_cancel;
            assert (
              List.is_empty (Delegated_policy_caller_support.policy_receipts after_cancel));
            [%test_eq: int] 2 !native_preparations;
            [%test_eq: int] prior_closes io.cache_renames;
            [%test_eq: int] 0 !providers;
            within "actual policy adoption while real child retains B" (fun () ->
              Cold_policy_resource_tests.run_standalone_policy
                ~phase
                env
                daemon
                parent
                before_policy
                ~original
              |> ignore);
            let before_resume = Warm.settled parent in
            assert (not (Owner.is_loaded parent.runtime));
            assert (Owner.is_loaded child.runtime);
            [%test_eq: int] 2 !native_preparations;
            [%test_eq: int] prior_closes io.cache_renames;
            [%test_eq: int] 0 !providers;
            let lifecycle_receipts current =
              List.filter current.A.Session_state.moderator_executions ~f:(fun receipt ->
                match receipt.P.Moderator_execution.context.phase with
                | Session_start | Session_resume -> true
                | Turn_start
                | Message_appended
                | Pre_tool_call
                | Post_tool_response
                | Turn_end
                | Internal_event -> false)
            in
            assert (
              List.equal
                P.Moderator_execution.equal
                (lifecycle_receipts before_policy)
                (lifecycle_receipts before_resume));
            assert (
              P.Run_source.equal (Warm.source before_policy) (Warm.source before_resume));
            assert (
              Option.equal
                Jsonaf.exactly_equal
                before_policy.moderator
                before_resume.moderator);
            let writer, _ =
              Actor.attach parent.actor ~mode:Read_write ~subscribe:false |> protocol_ok
            in
            Creation_tests.protect_cleanup
              ~finally:(fun () -> Actor.detach parent.actor writer.id |> protocol_ok)
              (fun () ->
                 within
                   "first authenticated Product adoption and pending Resume"
                   (fun () ->
                      Owner.ensure_loaded_authenticated parent.runtime ~actor:original
                      |> protocol_ok;
                      let pending_runtime = Warm.loaded parent in
                      (* A new callback is required only from this actual pending
                      lifecycle owner, never from ordinary warm re-adoption. *)
                      let resume = B.runtime_resume pending_runtime |> protocol_ok in
                      B.Runtime_resume.check_pending resume |> protocol_ok;
                      Actor.start ~actor:original parent.actor ~attachment_id:writer.id
                      |> protocol_ok
                      |> ignore;
                      Factory.drain_idle_moderator (Daemon.factory daemon) parent
                      |> protocol_ok
                      |> ignore;
                      let resumed = Warm.settled parent in
                      [%test_eq: int]
                        (List.length (Warm.completed before_resume Session_resume) + 1)
                        (List.length (Warm.completed resumed Session_resume));
                      same_native pending_runtime (Warm.loaded parent);
                      [%test_eq: int] 2 !native_preparations)));
         let promoted = Warm.loaded parent in
         Ordinary_warm_resource_tests.assert_live_source promoted;
         (match order with
          | Loaded_first -> same_native initial promoted
          | Native_first ->
            assert (
              not
                (phys_equal
                   (Option.value_exn initial.native_runtime)
                   (Option.value_exn promoted.native_runtime))));
         let previous = ref promoted in
         for _cycle = 1 to 3 do
           within "ordinary unload while actual child still retains B" (fun () ->
             Owner.unload_and_wait parent.runtime |> protocol_ok);
           assert (not (Owner.is_loaded parent.runtime));
           assert (Owner.is_loaded child.runtime);
           [%test_eq: int] prior_closes io.cache_renames;
           within "ordinary re-adoption of retained Product" (fun () ->
             Owner.ensure_loaded parent.runtime |> protocol_ok);
           let current = Warm.loaded parent in
           assert (not (phys_equal !previous.worker current.worker));
           same_native !previous current;
           Ordinary_warm_resource_tests.assert_live_source current;
           previous := current;
           [%test_eq: int] prior_closes io.cache_renames;
           [%test_eq: int] 0 !providers
         done;
         within "release actual child's retained native loan" (fun () ->
           Owner.unload_and_wait child.runtime |> protocol_ok);
         assert (not (Owner.is_loaded child.runtime));
         [%test_eq: int] prior_closes io.cache_renames;
         (match order with
          | Loaded_first -> ()
          | Native_first ->
            let module D = Agent_store.Delegation_store in
            let parent_before = Warm.settled parent in
            let child_state = Actor.state child.actor |> protocol_ok in
            let key = P.Idempotency_key.of_string "561-actual-child" |> protocol_ok in
            let reference =
              A.Child_contributions.references
                (Option.value_exn parent_before.child_contributions)
              |> List.find_exn ~f:(fun reference ->
                P.Idempotency_key.equal reference.D.Reference.key.idempotency_key key
                && P.Id.Session.equal
                     reference.child_session_id
                     child_state.identity.session_id)
            in
            assert (
              Option.equal D.Reference.equal child_state.spec.delegation (Some reference));
            let ledger = Store.delegations (Daemon.store daemon) in
            let record =
              D.resolve ledger reference |> Source_preparation_tests.store_ok
            in
            (match record.stage, record.revocation with
             | Linked, None -> ()
             | (Reserved | Artifact_installed | Child_installed | Linked), Some _
             | (Reserved | Artifact_installed | Child_installed), None ->
               failwith "635 private denial requires the actual current child admission");
            let revoked =
              D.revoke ledger record Authority_changed
              |> Source_preparation_tests.store_ok
            in
            assert (
              Option.equal D.equal_revocation revoked.revocation (Some Authority_changed));
            within
              "deny fresh actual child load from revoked private admission"
              (fun () ->
                 match Owner.ensure_loaded child.runtime with
                 | Error { code = Permission_denied; retryable = false; message; _ }
                   when String.equal message "native source private parent changed" ->
                   (* Source selection now rejects the revoked private record
                      before borrowing its retained native implementation. *)
                   ()
                 | Error error -> raise_s [%sexp (error : P.Error.t)]
                 | Ok () ->
                   failwith "635 retained native unit bypassed private revocation");
            assert (not (Owner.is_loaded child.runtime));
            assert (Owner.is_loaded parent.runtime);
            let retained = Warm.loaded parent in
            same_native !previous retained;
            Ordinary_warm_resource_tests.assert_live_source retained;
            no_lifecycle_replay parent_before (Warm.settled parent);
            [%test_eq: int] 2 !native_preparations;
            [%test_eq: int] prior_closes io.cache_renames;
            [%test_eq: int] 0 !providers);
         within "release final ordinary Product reference" (fun () ->
           Owner.unload_and_wait parent.runtime |> protocol_ok);
         assert (not (Owner.is_loaded parent.runtime));
         [%test_eq: int] (prior_closes + 1) io.cache_renames;
         within "fresh construction after actual last reference joined" (fun () ->
           Owner.ensure_loaded parent.runtime |> protocol_ok);
         let fresh = Warm.loaded parent in
         assert (
           not
             (phys_equal
                (Option.value_exn !previous.native_runtime)
                (Option.value_exn fresh.native_runtime)));
         Ordinary_warm_resource_tests.assert_live_source fresh;
         within "close fresh independent construction" (fun () ->
           Owner.unload_and_wait parent.runtime |> protocol_ok);
         [%test_eq: int] (prior_closes + 2) io.cache_renames;
         [%test_eq: int] 0 !providers));
  print_endline
    "loaded-first and native-first: genuine Owned dependency, same physical unit across \
     promotion and three re-adoptions, exact final close and fresh construction";
  [%expect
    {| loaded-first and native-first: genuine Owned dependency, same physical unit across promotion and three re-adoptions, exact final close and fresh construction |}]
;;
