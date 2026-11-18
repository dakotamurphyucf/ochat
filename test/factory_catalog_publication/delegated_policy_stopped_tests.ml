open! Core
open Agent_server_test_support
open Retry_support
module F = Delegated_policy_caller_support
module Warm = Delegated_warm_resource_tests
module Resource_loan = Ordinary_warm_resource_tests.Resource_loan
module M = Chat_response.Moderator_manager
module C = Chat_response.Tool_capability
module E = P.Moderator_execution
module I = P.Invocation
module B = A.Runtime_builder
module N = A.Native_tool_invocation

(* Compaction is supported at Pre_tool_call. Only this fixed prospective event
   requests it; setup leaves the actual parent quiescent. *)
let stopped_prompt =
  String.substr_replace_all
    F.parent_prompt
    ~pattern:"| `Ok(value) -> Task.pure(state + 1)"
    ~with_:
      {| | `Ok(value) ->
    if call.id == "inv_592_stopped_policy" then
      Task.bind(Runtime.request_compaction(), fun ignored_compaction -> Task.pure(state + 1))
    else Task.pure(state + 1)|}
;;

(* This trusted public Owner host uses a genuine controller-free producer.
   Factory-registered children use Factory mediation; stateful Independent
   stopped-parent composition remains the separate593 acceptance boundary. *)
let with_standalone_caller (t : F.t) f =
  F.with_standalone_caller
    ~env:t.env
    ~daemon:t.daemon
    ~source_state:t.parent_initial
    ~original:t.b
    f
;;

module Native_host = struct
  type t =
    { mutable selected : (string * (unit -> unit)) option
    ; mutable entered : int
    ; failure : F.Failure.t
    }

  let create () = { selected = None; entered = 0; failure = F.Failure.create () }

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:Before_open_in
      ~matches:(fun path ->
        Option.exists t.selected ~f:(fun (selected, _) -> String.equal selected path))
      ~reached:(fun _ ->
        let _, invoke = Option.value_exn t.selected in
        (* One real native callback, before its first file read. Disarm before
           the parent's own policy IO. No replacement native implementation. *)
        t.selected <- None;
        t.entered <- t.entered + 1;
        F.Failure.guard t.failure invoke)
  ;;
end

let%expect_test
    "stopped parent executes exact retained policy without worker or automatic followup"
  =
  assert (not (String.equal stopped_prompt F.parent_prompt));
  let host = Native_host.create () in
  let observe_transfer = ref false in
  let transfer_checks = ref [] in
  F.with_fixture
    ~on_b_current:(fun current ->
      if !observe_transfer then transfer_checks := current :: !transfer_checks)
    ~wrap_env:(Native_host.wrap host)
    ~prompt_text:stopped_prompt
    (fun t ->
       with_standalone_caller t (fun caller_actor caller_writer ->
         Eio.Switch.run (fun sw ->
           let unrelated = Actor.state caller_actor |> protocol_ok in
           assert (Option.is_none unrelated.spec.delegation);
           assert (
             not
               (P.Id.Session.equal
                  unrelated.identity.session_id
                  t.parent_initial.identity.session_id));
           let loan = Resource_loan.create ~sw t.parent in
           protect_cleanup
             ~finally:(fun () ->
               host.selected <- None;
               Resource_loan.release loan;
               F.within t "join stopped policy Product loan" (fun () ->
                 Resource_loan.join loan))
             (fun () ->
                let retained =
                  F.within t "retain actual parent before Stop" (fun () ->
                    Resource_loan.await loan)
                in
                assert (phys_equal retained.worker t.parent_runtime.worker);
                assert (
                  Option.equal
                    phys_equal
                    retained.moderator_manager
                    t.parent_runtime.moderator_manager);
                Actor.stop t.parent.actor ~attachment_id:t.parent_writer ~mode:Cancel
                |> protocol_ok
                |> ignore;
                F.within
                  t
                  "join actual stopped parent and Owned child retirement"
                  (fun () -> Owner.unload_and_wait t.parent.runtime |> protocol_ok);
                let stopped = F.state t.parent in
                assert (P.Session.equal_desired_state stopped.lifecycle.desired Stopped);
                assert (Option.is_none stopped.active_operation);
                assert (not (Owner.is_loaded t.parent.runtime));
                assert (
                  P.Session.equal_desired_state
                    (Actor.state caller_actor |> protocol_ok).lifecycle.desired
                    Running);
                let source = Warm.source stopped in
                let observer =
                  Owner.retained_moderation_source t.parent.runtime
                  |> protocol_ok
                  |> Option.value_exn
                in
                assert (
                  I.equal_observer
                    observer
                    (Option.value_exn
                       (M.invocation_observer
                          (Option.value_exn retained.moderator_manager))));
                let ports = !(t.counters.inference_ports) in
                let providers = t.provider.child_calls in
                let confirmed = ref None in
                let policy_metadata_ack = ref None in
                let candidate =
                  P.Id.Invocation.of_string "inv_592_stopped_policy" |> protocol_ok
                in
                let committed_call = ref None in
                let worker_calls = ref 0 in
                let invoke () =
                  let borrowed = N.borrow () |> protocol_ok in
                  let lending = N.borrowed_invocation borrowed in
                  assert (
                    P.Id.Session.equal
                      lending.context.session_id
                      unrelated.identity.session_id);
                  assert (I.equal_origin lending.context.origin Model);
                  assert (String.equal lending.context.tool_name "read_file");
                  (match N.borrowed_agent_actor borrowed with
                   | Ok (Some original) ->
                     assert (Operator_authorization.is_current original);
                     assert (
                       P.Id.Principal.equal
                         (Operator_authorization.principal original).id
                         (Operator_authorization.principal t.b).id)
                   | Ok None | Error _ ->
                     failwith "actual standalone native lost original B");
                  let selected = N.borrowed_capabilities borrowed |> protocol_ok in
                  let reference =
                    C.find selected ~name:"read_file"
                    |> Result.map_error ~f:(fun error -> error.C.message)
                    |> Result.ok_or_failwith
                    |> C.reference
                  in
                  let caller = N.borrowed_policy_caller borrowed |> protocol_ok in
                  Exn.protect
                    ~finally:(fun () -> A.Delegated_policy_caller.close caller)
                    ~f:(fun () ->
                      let call : Chat_response.Moderation.Tool_call.t =
                        { id = P.Id.Invocation.to_string candidate
                        ; name = reference.name
                        ; args =
                            `Object
                              [ "root", `String "data"; "file", `String "nested.txt" ]
                        ; kind = Function
                        ; payload_text = {|{"root":"data","file":"nested.txt"}|}
                        ; meta = `Null
                        }
                      in
                      let request : A.Script_tool_calls.preparation =
                        { invocation_id = candidate
                        ; session_id = unrelated.identity.session_id
                        ; generation = unrelated.identity.generation
                        ; owner = Native_call lending.context.id
                        ; caller = Some caller
                        ; selected
                        ; call
                        }
                      in
                      let authorize () =
                        Actor.check_delegated_policy_caller caller_actor request
                      in
                      authorize () |> protocol_ok;
                      let captured =
                        Actor.Causal_governance.capture caller_actor ~request
                        |> protocol_ok
                      in
                      let governance =
                        Actor.Causal_governance.bind_standalone caller_actor captured
                        |> protocol_ok
                      in
                      let delegation : E.delegation =
                        { child_session_id = request.session_id
                        ; child_generation = request.generation
                        ; child_invocation_id = candidate
                        ; admission_sha256 = C.fingerprint selected
                        }
                      in
                      (* Trusted public Owner host protocol, not a private Factory
                         relation or Independent permission. The real retained access
                         is captured before the parent
                         callback enters its native read. This exact read executes
                         outside the Actor mailbox; no Manager snapshot is taken. *)
                      host.selected
                      <- Some
                           ( Filename.concat
                               stopped.spec.workspace_instance.canonical_root.native_path
                               "policy.txt"
                           , fun () ->
                               assert (Option.is_none !policy_metadata_ack);
                               let before = F.state t.parent in
                               let executing =
                                 F.policy_receipts before
                                 |> Delegated_policy_claim_tests.one
                                      "actual stopped policy before metadata ACK"
                               in
                               assert (E.equal_status executing.status Running);
                               let native =
                                 List.filter before.invocations ~f:(fun invocation ->
                                   Option.exists
                                     invocation.I.parent_event
                                     ~f:
                                       (P.Id.Moderator_execution.equal
                                          executing.context.id))
                                 |> Delegated_policy_claim_tests.one
                                      "actual policy native at metadata ACK"
                               in
                               assert (I.equal_status native.status Dispatching);
                               assert (String.equal native.context.tool_name "read_file");
                               let patch =
                                 P.Session_metadata.Patch.create
                                   ~name:(Set "604 actual in-flight policy metadata")
                                   ~set_labels:[ "policy", "metadata-ack" ]
                                   ~remove_labels:[]
                                 |> protocol_ok
                               in
                               Actor.update_metadata
                                 t.parent.actor
                                 ~attachment_id:t.parent_writer
                                 ~expected_metadata_revision:
                                   before.identity.metadata_revision
                                 ~patch
                                 ()
                               |> protocol_ok
                               |> ignore;
                               let updated = F.state t.parent in
                               [%test_eq: int64]
                                 Int64.(before.identity.metadata_revision + 1L)
                                 updated.identity.metadata_revision;
                               [%test_eq: int64] stopped.stop_epoch updated.stop_epoch;
                               assert (
                                 P.Session.equal_desired_state
                                   updated.lifecycle.desired
                                   Stopped);
                               assert (P.Run_source.equal source (Warm.source updated));
                               assert (
                                 E.equal
                                   executing
                                   (F.policy_receipts updated
                                    |> Delegated_policy_claim_tests.one
                                         "same running policy after metadata ACK"));
                               policy_metadata_ack
                               := Some updated.identity.metadata_revision );
                      let decision =
                        Owner.prepare_delegated_tool
                          t.parent.runtime
                          ~request
                          ~governance
                          ~delegation
                          ~event:(Pre_tool_call call)
                          ~authorize
                        |> function
                        | Ok decision -> decision
                        | Error failure ->
                          let failed = F.state t.parent in
                          raise_s
                            [%sexp
                              (failure : P.Error.t)
                            , (List.map (F.policy_receipts failed) ~f:(fun receipt ->
                                 receipt.E.status)
                               : E.status list)
                            , (List.map failed.invocations ~f:(fun invocation ->
                                 invocation.I.status)
                               : I.status list)]
                      in
                      assert (E.Decision.equal decision Approve);
                      assert (Option.is_none !confirmed);
                      let captured =
                        Chat_response.Moderation.Event.to_value (Pre_tool_call call)
                        |> Session.Snapshot.of_value
                        |> Result.ok_or_failwith
                      in
                      confirmed
                      := Some
                           ( lending.context.id
                           , `Object [ "snapshot", Session.Snapshot.to_jsonaf captured ]
                           ))
                in
                let ready, publish = Eio.Promise.create () in
                let exited, exit = Eio.Promise.create () in
                let module Definition = struct
                  type input = string

                  let name = "read_file"
                  let description = Some "Actual standalone stopped-policy host read"
                  let type_ = "function"
                  let parameters = `Object [ "type", `String "object" ]
                  let input_of_string text = text
                end
                in
                let implementation =
                  Ochat_function.create_function
                    (module Definition)
                    (fun input ->
                       assert (String.equal input "{}");
                       invoke ();
                       let path =
                         Filename.concat
                           unrelated.spec.workspace_instance.canonical_root.native_path
                           "direct.txt"
                       in
                       Openai.Responses.Tool_output.Output.Text
                         (Eio.Path.load Eio.Path.(t.env#fs / path)))
                in
                let selected =
                  C.create
                    ~owner:"actual-standalone-policy-host"
                    ~resource_fingerprint:
                      (Chatmd_shell_spec.Source_ref.digest "597 stopped native host")
                    [ ( Chatmd_shell_spec.Source_ref.digest
                          "597 actual read_file implementation"
                      , implementation )
                    ]
                  |> Result.map_error ~f:(fun error -> error.C.message)
                  |> Result.ok_or_failwith
                in
                let reference = List.hd_exn (C.references selected) in
                let worker =
                  A.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
                    Exn.protect
                      ~finally:(fun () -> Eio.Promise.resolve exit ())
                      ~f:(fun () ->
                        Int.incr worker_calls;
                        let outcome =
                          try
                            let id = P.Id.Invocation.create () in
                            let call_id =
                              History_entry.Id_source.allocate capabilities.id_source
                              |> Result.ok_or_failwith
                            in
                            let call =
                              Openai.Responses_history.create_with_id_exn
                                ~id:call_id
                                (Openai.Responses.Item.Function_call
                                   { name = reference.name
                                   ; arguments = "{}"
                                   ; call_id = P.Id.Invocation.to_string id
                                   ; _type = "function_call"
                                   ; id = None
                                   ; status = None
                                   })
                            in
                            capabilities.commit_entry call |> protocol_ok;
                            assert (Option.is_none !committed_call);
                            committed_call := Some call;
                            let invocation =
                              I.create
                                { id
                                ; session_id = input.session_id
                                ; generation = input.session_generation
                                ; origin = Model
                                ; provider_call_id = Some (P.Id.Invocation.to_string id)
                                ; call_entry_id = Some call_id
                                ; parent_invocation = None
                                ; parent_job = None
                                ; tool_name = reference.name
                                ; implementation_revision =
                                    reference.implementation_revision
                                ; capability_fingerprint = C.fingerprint selected
                                ; input = `Object []
                                ; created_at = P.Timestamp.now ()
                                ; deadline = None
                                }
                              |> protocol_ok
                            in
                            let resolved =
                              N.run
                                ~capabilities
                                ~registry:(fun () -> selected)
                                ~reference
                                ~invocation
                                ~is_halted:(fun () -> false)
                                ~authorize:(fun _ _ ->
                                  capabilities.original_agent_actor ()
                                  |> Result.map ~f:(fun original ->
                                    assert (
                                      Option.exists
                                        original
                                        ~f:Operator_authorization.is_current)))
                                ~prepare_output:(function
                                  | Openai.Responses.Tool_output.Output.Text text ->
                                    Ok (`String text)
                                  | Content _ ->
                                    Error
                                      (P.Error.invalid_request
                                         "actual native reader returned nontext"))
                              |> protocol_ok
                            in
                            (match resolved.status with
                             | Resolved (Complete (`String text)) ->
                               assert (
                                 String.is_substring
                                   text
                                   ~substring:"592 direct.txt real native sentinel")
                             | status ->
                               raise_s
                                 [%sexp
                                   "standalone native did not resolve"
                                 , (status : I.status)]);
                            Ok ()
                          with
                          | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
                        in
                        Eio.Promise.resolve publish outcome;
                        Eio.Fiber.await_cancel ()))
                in
                Actor.set_operation_worker caller_actor (Some worker) |> protocol_ok;
                let ids =
                  A.History_id_source.create
                    ~namespace:(P.Id.Session.to_string unrelated.identity.session_id)
                    ~block_size:8
                    ~reserve:(fun ~count ->
                      Actor.reserve_history_block caller_actor ~count)
                  |> protocol_ok
                in
                let message =
                  A.History_codec.user_text
                    ~id:(A.History_id_source.allocate ids |> protocol_ok)
                    "592 genuine standalone original B"
                  |> A.History_codec.to_protocol
                in
                let submitted =
                  Actor.submit_message_authenticated
                    caller_actor
                    ~actor:t.b
                    ~attachment_id:caller_writer
                    message
                  |> protocol_ok
                in
                let operation = Option.value_exn submitted.operation_id in
                let cleanup () =
                  Eio.Cancel.protect (fun () ->
                    Actor.cancel_operation
                      caller_actor
                      ~attachment_id:caller_writer
                      ~operation_id:operation
                    |> protocol_ok
                    |> ignore;
                    F.within t "join genuine standalone lending worker" (fun () ->
                      Eio.Promise.await exited))
                in
                protect_cleanup ~finally:cleanup (fun () ->
                  F.within t "genuine operation lends Caller to stopped policy" (fun () ->
                    match Eio.Promise.await ready with
                    | Ok () -> ()
                    | Error (exn, backtrace) ->
                      Exn.raise_with_original_backtrace exn backtrace));
                let caller_after =
                  F.within t "join cancelled standalone lending operation" (fun () ->
                    let rec idle () =
                      let current = Actor.state caller_actor |> protocol_ok in
                      match current.active_operation, current.failure with
                      | None, None -> current
                      | _, Some error -> raise_s [%sexp (error : P.Error.t)]
                      | Some _, None ->
                        Eio.Fiber.yield ();
                        idle ()
                    in
                    idle ())
                in
                [%test_eq: int] 1 !worker_calls;
                [%test_eq: int] 1 host.entered;
                assert (Option.is_none host.selected);
                let policy_metadata_revision = Option.value_exn !policy_metadata_ack in
                let metadata_after_policy = F.state t.parent in
                [%test_eq: int64]
                  Int64.(stopped.identity.metadata_revision + 1L)
                  policy_metadata_revision;
                [%test_eq: int64]
                  policy_metadata_revision
                  metadata_after_policy.identity.metadata_revision;
                [%test_eq: string option]
                  (Some "604 actual in-flight policy metadata")
                  metadata_after_policy.identity.display_name;
                [%test_eq: string option]
                  metadata_after_policy.identity.display_name
                  metadata_after_policy.spec.protocol.display_name;
                [%test_eq: (string * string) list]
                  [ "policy", "metadata-ack"; "suite", "restart" ]
                  metadata_after_policy.spec.protocol.labels;
                let original_native, expected_event = Option.value_exn !confirmed in
                let actual =
                  Delegated_policy_claim_tests.find_invocation
                    caller_after
                    original_native
                in
                (* The earlier N.run result was Resolved. Actual operation
                   cancellation/join publishes its retained successful outcome. *)
                (match actual.status with
                 | Published (Complete (`String text)) ->
                   assert (
                     String.is_substring
                       text
                       ~substring:"592 direct.txt real native sentinel")
                 | status ->
                   raise_s
                     [%sexp
                       "actual lending native was not published", (status : I.status)]);
                let actual_call =
                  Option.value_exn !committed_call |> A.History_codec.to_protocol
                in
                assert (
                  List.exists
                    caller_after.conversation.canonical_history
                    ~f:(P.History.equal_entry actual_call));
                let output_id = Option.value_exn actual.output_entry_id in
                let output =
                  List.filter caller_after.conversation.canonical_history ~f:(fun entry ->
                    P.History.Id.equal entry.P.History.id output_id)
                  |> Delegated_policy_claim_tests.one "actual lending output occurrence"
                in
                assert (not (P.History.Id.equal actual_call.id output.id));
                A.Invocation_history.validate_publication
                  ~history:caller_after.conversation.canonical_history
                  actual
                |> protocol_ok;
                assert (Option.is_none caller_after.active_operation);
                assert (List.is_empty caller_after.moderator_executions);
                assert (
                  List.for_all caller_after.invocations ~f:(fun invocation ->
                    not (P.Id.Invocation.equal invocation.I.context.id candidate)));
                let after = F.state t.parent in
                let receipt =
                  F.policy_receipts after
                  |> Delegated_policy_claim_tests.one "stopped exact parent claim"
                in
                (match receipt.status, receipt.decision with
                 | Completed _, Some Approve -> ()
                 | (Running | Failed _ | Interrupted _), _
                 | Completed _, (None | Some (Reject _ | Rewrite_args _ | Redirect _)) ->
                   raise_s [%sexp "stopped policy did not commit", (receipt : E.t)]);
                let delegation = Option.value_exn receipt.delegation in
                assert (P.Id.Invocation.equal delegation.child_invocation_id candidate);
                assert (
                  P.Id.Session.equal
                    delegation.child_session_id
                    unrelated.identity.session_id);
                assert (I.equal_observer receipt.context.source observer);
                assert (Jsonaf.exactly_equal expected_event receipt.context.event);
                let requests = Option.value_exn receipt.requests in
                assert (
                  (not requests.request_turn)
                  && requests.request_compaction
                  && Option.is_none requests.end_session);
                assert (Option.equal E.equal_intent receipt.intent (Some Pending));
                let native =
                  List.filter after.invocations ~f:(fun invocation ->
                    Option.exists
                      invocation.I.parent_event
                      ~f:(P.Id.Moderator_execution.equal receipt.context.id))
                  |> Delegated_policy_claim_tests.one "stopped compiled policy native"
                in
                (match native.status with
                 | Resolved (Complete (`String value)) ->
                   assert (
                     String.is_substring
                       value
                       ~substring:"592 parent policy real native sentinel")
                 | Admitted | Dispatching
                 | Resolved (Complete _ | Pending _ | Fail _ | Cancelled _)
                 | Published _ ->
                   raise_s [%sexp "stopped policy native failed", (native : I.t)]);
                assert (P.Session.equal_desired_state after.lifecycle.desired Stopped);
                assert (
                  Option.is_none after.active_operation
                  && not (Owner.is_loaded t.parent.runtime));
                assert (P.Run_source.equal source (Warm.source after));
                assert (
                  List.equal
                    P.History.equal_entry
                    stopped.conversation.canonical_history
                    after.conversation.canonical_history);
                [%test_eq: int64]
                  stopped.conversation.reserved_history_through
                  after.conversation.reserved_history_through;
                [%test_eq: int] ports !(t.counters.inference_ports);
                [%test_eq: int] providers t.provider.child_calls;
                [%test_eq: int] 0 (List.length (Warm.completed after Session_resume));
                assert (
                  List.equal
                    E.equal
                    (Warm.completed stopped Session_start)
                    (Warm.completed after Session_start));
                let installed =
                  A.Moderator_checkpoint.decode after.moderator
                  |> protocol_ok
                  |> Option.value_exn
                in
                let before_snapshot =
                  A.Moderator_checkpoint.decode stopped.moderator
                  |> protocol_ok
                  |> Option.value_exn
                in
                (match before_snapshot.current_state, installed.current_state with
                 | Int before, Int after -> [%test_eq: int] (before + 1) after
                 | _ -> failwith "stopped policy counter changed representation");
                assert (
                  Jsonaf.exactly_equal
                    (Warm.identity retained)
                    (Session.Moderator_state.Identity_snapshot.to_jsonaf installed));
                assert (Option.is_none t.provider.call);
                (* An unrelated actual ACK while still stopped must preserve this
                   newly acknowledged policy intent's original, not just its DATA. *)
                let patch =
                  P.Session_metadata.Patch.create
                    ~name:(Set "592 stopped policy intent retained")
                    ~set_labels:[]
                    ~remove_labels:[]
                  |> protocol_ok
                in
                Actor.update_metadata
                  t.parent.actor
                  ~attachment_id:t.parent_writer
                  ~expected_metadata_revision:after.identity.metadata_revision
                  ~patch
                  ()
                |> protocol_ok
                |> ignore;
                let updated = F.state t.parent in
                [%test_eq: string option]
                  (Some "592 stopped policy intent retained")
                  updated.identity.display_name;
                [%test_eq: int64]
                  Int64.(after.identity.metadata_revision + 1L)
                  updated.identity.metadata_revision;
                assert (P.Session.equal_desired_state updated.lifecycle.desired Stopped);
                assert (not (Owner.is_loaded t.parent.runtime));
                [%test_eq: int64] after.stop_epoch updated.stop_epoch;
                assert (P.Run_source.equal source (Warm.source updated));
                assert (
                  E.equal
                    receipt
                    (F.policy_receipts updated
                     |> Delegated_policy_claim_tests.one "same stopped intent after ACK"));
                [%test_eq: int] ports !(t.counters.inference_ports);
                [%test_eq: int] providers t.provider.child_calls;
                (* Use the public lifecycle route with A. Reattachment is warm;
                   it must neither consume B's intent nor replay Init/resume. *)
                Owner.ensure_loaded_authenticated t.parent.runtime ~actor:t.a
                |> protocol_ok;
                let rebound = Warm.loaded t.parent in
                assert (not (phys_equal rebound.worker retained.worker));
                assert (
                  Option.equal
                    phys_equal
                    rebound.moderator_manager
                    retained.moderator_manager);
                Actor.start ~actor:t.a t.parent.actor ~attachment_id:t.parent_writer
                |> protocol_ok
                |> ignore;
                let started = F.state t.parent in
                assert (P.Session.equal_desired_state started.lifecycle.desired Running);
                assert (Option.is_none started.active_operation);
                assert (P.Run_source.equal source (Warm.source started));
                assert (
                  E.equal
                    receipt
                    (F.policy_receipts started
                     |> Delegated_policy_claim_tests.one
                          "same intent before explicit apply"));
                assert (
                  List.equal
                    E.equal
                    (Warm.completed after Session_start)
                    (Warm.completed started Session_start));
                [%test_eq: int] 0 (List.length (Warm.completed started Session_resume));
                let before_provider = t.provider.child_calls in
                let before_compaction = started.conversation.compaction_generation in
                let before_archives =
                  List.length started.conversation.compaction_archives
                in
                (* Only this one Actor RPC is armed. Scheduler inputs remain parked,
                   the genuine standalone B operation is joined, and no cleanup runs here.
                   plan_agent_custody selects the SAME Event_intent; actual compaction
                   admission transfers its custody and must call B's genuine guard.
                   Missing custody or ambient A cannot satisfy this observation. *)
                let applied =
                  Exn.protect
                    ~finally:(fun () -> observe_transfer := false)
                    ~f:(fun () ->
                      observe_transfer := true;
                      Actor.apply_moderator_follow_up t.parent.actor |> protocol_ok)
                in
                assert applied;
                assert (not (List.is_empty !transfer_checks));
                assert (List.for_all !transfer_checks ~f:Fn.id);
                let compacted =
                  F.within t "same stopped policy compaction completes once" (fun () ->
                    F.await_idle t t.parent)
                in
                assert (Option.is_none compacted.failure);
                [%test_eq: int]
                  (before_compaction + 1)
                  compacted.conversation.compaction_generation;
                [%test_eq: int]
                  (before_archives + 1)
                  (List.length compacted.conversation.compaction_archives);
                [%test_eq: int] (before_provider + 1) t.provider.child_calls;
                let consumed =
                  F.policy_receipts compacted
                  |> Delegated_policy_claim_tests.one "same intent consumed once"
                in
                assert (
                  P.Id.Moderator_execution.equal receipt.context.id consumed.context.id);
                assert (Option.equal E.equal_intent (Some Applied) consumed.intent);
                (* Compaction-only is Applied immediately; a retained operation
                   binding belongs only to compaction-plus-turn continuation. *)
                assert (Option.is_none consumed.compaction_operation_id);
                let events =
                  match
                    A.Durable_event_log.replay
                      t.parent.durable_events
                      ~after_sequence:started.counters.event_sequence
                      ~through_sequence:compacted.counters.event_sequence
                  with
                  | Available events ->
                    List.map events ~f:(fun (event : P.Event.Durable.t) ->
                      P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                      |> protocol_ok)
                  | Snapshot_required ->
                    failwith "explicit compaction exceeded actual event replay window"
                in
                let started_operation =
                  List.filter_map events ~f:(function
                    | Operation_started operation -> Some operation
                    | _ -> None)
                  |> Delegated_policy_claim_tests.one "one actual compaction admission"
                in
                [%test_eq: P.Operation.kind] Compaction started_operation.kind;
                let completed_operation =
                  List.filter_map events ~f:(function
                    | Operation_completed operation -> Some operation
                    | _ -> None)
                  |> Delegated_policy_claim_tests.one "one actual compaction completion"
                in
                [%test_eq: P.Operation.kind] Compaction completed_operation.kind;
                assert (P.Id.Operation.equal started_operation.id completed_operation.id);
                (match completed_operation.state with
                 | Completed -> ()
                 | Starting | Running | Cancelling | Cancelled | Failed _ | Interrupted _
                   -> failwith "explicit original-backed compaction did not complete");
                let archive = List.hd_exn compacted.conversation.compaction_archives in
                assert (P.Id.Operation.equal started_operation.id archive.operation_id);
                assert (
                  A.Session_state.Compaction_archive.equal_kind Compaction archive.kind);
                assert (P.Run_source.equal source (Warm.source compacted));
                let before_noop =
                  state_document compacted
                  |> Source_preparation_tests.store_ok
                  |> Document_schema.Document.json
                in
                assert (not (Actor.apply_moderator_follow_up t.parent.actor |> protocol_ok));
                let final = F.state t.parent in
                assert (
                  Jsonaf.exactly_equal
                    before_noop
                    (state_document final
                     |> Source_preparation_tests.store_ok
                     |> Document_schema.Document.json));
                [%test_eq: int] (before_provider + 1) t.provider.child_calls))));
  [%expect {| |}]
;;
