open! Core
open Agent_server_test_support
open Retry_support
module Host = Cold_policy_recovered_support
module Cold = Cold_policy_resource_tests
module Resource = Factory.For_testing
module B = A.Runtime_builder
module D = Agent_store.Delegation_store
module Scope = Owner.Scope_outcome
module Native = Owner.Native_borrow_outcome
module Release = Owner.Native_release

(* This is a lower trusted host fixture, not Factory child admission. Reserve
   actual captured generated-definition DATA for a fresh child candidate. The
   independently authenticated standalone Caller is NOT that reserved candidate.
   Every borrow still checks the exact record, both actual Actors and genuine
   native authority. Reserved claims neither Linked nor child execution. *)
let reference_and_guard (host : Host.t) (native : Cold.native_request) ~runtime =
  let parent = Host.state host in
  let child = Actor.state native.caller_actor |> protocol_ok in
  let definition = Delegated_load_tests.definition host.env host.root runtime in
  let artifact = A.Generated_definition.artifact definition in
  let candidate = P.Id.Session.create () in
  let target =
    match Inference.Selection.view child.spec.inference_target with
    | Captured target -> target
    | Unresolved -> failwith "645 actual standalone Caller has no captured target"
  in
  let ledger = Agent_store.Session_store.delegations (Daemon.store host.daemon) in
  let record =
    D.reserve
      ledger
      ~key:
        { parent_session_id = parent.identity.session_id
        ; parent_generation = parent.identity.generation
        ; principal_id = principal.id
        ; idempotency_key =
            P.Idempotency_key.of_string "645-actual-host-borrow" |> protocol_ok
        }
      ~request_sha256:(Chatmd_shell_spec.Source_ref.digest "645 actual host borrow DATA")
      ~admission:
        { child_session_id = candidate
        ; revision_id = artifact.revision_id
        ; transaction_id = P.Id.Transaction.create ()
        ; manifest_sha256 = artifact.manifest_sha256
        ; parent_revision_id = parent.spec.prompt_revision_id
        ; parent_stop_epoch = None
        ; authority_sha256 =
            Chatmd_shell_spec.Source_ref.digest "645 lower trusted host DATA"
        ; authored_tool = None
        ; goal_origin = Some Agent_store.Delegation_goal_origin.unrelated
        ; captured_recurrence = None
        ; capability_pins = A.Generated_definition.capability_pins definition
        ; lifetime = Owned
        ; created_at = P.Timestamp.now ()
        ; inference_target = Some target
        }
      ~max_records:32
      ~max_bytes:1048576
    |> Existing.store_ok
    |> function
    | D.New record -> record
    | Replay _ | Conflict _ -> failwith "645 expected a new actual host ledger record"
  in
  let reference = D.reference record in
  let authorize ~reference:current ~target:(expected : A.Session_state.t) =
    let open Result.Let_syntax in
    let%bind () = native.authorize () in
    let%bind actual =
      D.resolve ledger current
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    in
    let%bind current_parent = Actor.state host.actor in
    let%bind current_child = Actor.state native.caller_actor in
    if
      D.Reference.equal current reference
      && D.equal_record actual record
      && Option.is_none actual.revocation
      && D.equal_stage actual.stage Reserved
      && P.Id.Session.equal expected.identity.session_id parent.identity.session_id
      && Int.equal expected.identity.generation parent.identity.generation
      && P.Id.Session.equal current_parent.identity.session_id parent.identity.session_id
      && Int.equal current_parent.identity.generation parent.identity.generation
      && P.Id.Prompt_revision.equal
           current_parent.spec.prompt_revision_id
           parent.spec.prompt_revision_id
      && P.Id.Session.equal current_child.identity.session_id child.identity.session_id
      && Int.equal current_child.identity.generation child.identity.generation
      && P.Id.Prompt_revision.equal
           current_child.spec.prompt_revision_id
           child.spec.prompt_revision_id
    then native.authorize ()
    else Error (P.Error.invalid_request "645 actual lower host association changed")
  in
  reference, authorize
;;

let with_borrow_owner ?when_idle f =
  let phase = ref "645 actual native release fixture" in
  Host.with_recovered_host ~phase (fun host with_request ->
    with_request ~original (fun native ->
      let owner = ref None in
      let association = ref None in
      protect_cleanup
        ~finally:(fun () -> Option.iter !owner ~f:Owner.close_and_wait)
        (fun () ->
           protect_cleanup
             ~finally:(fun () ->
               Option.iter !owner ~f:(fun owner ->
                 Owner.unload_and_wait owner |> protocol_ok))
             (fun () ->
                Host.with_native_construction host native (fun constructed ->
                  Host.with_native_promotion host constructed (fun promotion _product ->
                    let binding = Resource.Attachment.binding promotion |> protocol_ok in
                    let loan, _access =
                      Resource.Attachment.publish
                        promotion
                        ~basis:constructed.basis
                        ~expected:constructed.expected
                      |> protocol_ok
                    in
                    let raw = B.Product.Binding.runtime binding in
                    let attachment =
                      B.Product.Binding.attach binding ~runtime:raw ~close:(fun () ->
                        Resource.Loan.close loan |> protocol_ok)
                      |> protocol_ok
                    in
                    let native_access =
                      Resource.native_access
                        host.cell
                        ~prepare:(fun ~admission:_ ~stage:_ ->
                          failwith "645 existing actual unit rebuilt")
                    in
                    let actual =
                      Owner.create_with_deferred_run_unload
                        ~source_owner:host.source_owner
                        ~actor:host.actor
                        ~initial:(Some attachment)
                        ~before_unload:(fun ~closing:_ -> Ok ())
                        ~policy_resource_cleanup:(Resource.cleanup_port host.cell)
                        ~policy_resource_access:None
                        ~native_resource_access:(Some native_access)
                        ~build:(fun ~initialization_cleanup:_ ~original_actor:_ _ ->
                          failwith "645 fixture unexpectedly loaded")
                        ~build_deferred:
                          (fun
                            ~initialization_cleanup:_
                            ~source_consumer:_
                            ~source:_
                            ~expected_product:_
                            ~construction:_ ->
                          failwith "645 fixture unexpectedly constructed")
                        ~ordinary_warm:None
                        ~delegated_load:None
                    in
                    owner := Some actual;
                    let reference, authorize =
                      reference_and_guard host native ~runtime:raw
                    in
                    association := Some (reference, authorize);
                    f host actual reference authorize;
                    Ok ())
                  |> protocol_ok;
                  Ok ())
                |> protocol_ok);
           (* Finite unload releases the original runtime and its nonclosing
              Actor retirement. Permanent close follows this cold phase, so
              setup still requires the idle unloaded Actor and open caller. *)
           Option.iter when_idle ~f:(fun when_idle ->
             let reference, authorize = Option.value_exn !association in
             when_idle host (Option.value_exn !owner) reference authorize))))
;;

let positive cleanup =
  match Scope.Cleanup.status cleanup with
  | Never_acquired | Joined -> ()
  | Failed _ -> failwith "645 actual healthy cleanup was negative"
;;

let released release =
  assert (Release.equal_status Released (Release.status release |> protocol_ok))
;;

let%expect_test
    "actual native pin witnesses isolate nested loans and survive throwing capture"
  =
  let module CO = A.Close_observation in
  let module Diagnostic = A.Cleanup_diagnostic in
  let positive_observation actual =
    (match CO.exit actual with
     | Returned (Ok ()) -> ()
     | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
     | Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace);
    assert (List.is_empty (CO.diagnostics actual));
    assert (Option.is_none (CO.primary actual))
  in
  let preparation stage outcome =
    let actual = Option.value_exn (Native.preparation_failure outcome) in
    assert (
      Native.Preparation_failure.equal_stage
        stage
        (Native.Preparation_failure.stage actual));
    let observation = Native.Preparation_failure.observation actual in
    let whole = Native.observation outcome in
    assert (
      List.equal Diagnostic.equal (CO.diagnostics observation) (CO.diagnostics whole));
    assert (Option.equal Diagnostic.equal (CO.primary observation) (CO.primary whole));
    actual, observation
  in
  let actual_exception actual expected =
    let exn, trace =
      match CO.exit actual with
      | Raised (exn, trace) -> exn, trace
      | Returned _ -> failwith "observed native borrow lost its actual raw failure"
    in
    assert (phys_equal exn expected);
    let group = Option.value_exn (CO.primary actual) in
    assert (List.equal Diagnostic.equal (CO.diagnostics actual) [ group ]);
    match Diagnostic.failures group with
    | [ Raised (origin, original_trace) ] ->
      assert (phys_equal origin expected);
      let original = Stdlib.Printexc.raw_backtrace_to_string original_trace in
      assert (not (String.is_empty original));
      assert (
        String.is_prefix (Stdlib.Printexc.raw_backtrace_to_string trace) ~prefix:original)
    | _ -> failwith "observed native borrow lost actual control provenance"
  in
  let cold_preparation (host : Host.t) reference authorize =
    assert (not (Resource.pending host.cell));
    let error = P.Error.invalid_request "663 actual cold native preparation Error" in
    let sentinel = Failure "663 actual cold native preparation raised" in
    let raise_prepare = ref false in
    let prepare_calls = ref 0 in
    let pin_calls = ref 0 in
    let check_calls = ref 0 in
    let custody_calls = ref 0 in
    let body_calls = ref 0 in
    let captures = ref 0 in
    let setups = ref [] in
    let producer_trace = ref None in
    let before_unload = ref (fun () -> Ok ()) in
    let native_access =
      Owner.Native_resource_access.create
        ~prepare:(fun ~admission ~stage ->
          let expected = Host.state host in
          Owner.Native_resource_admission.check admission ~expected |> protocol_ok;
          assert (
            D.Reference.equal
              (Owner.Native_resource_admission.reference admission)
              reference);
          (match
             Owner.Native_resource_admission.origin admission
             |> Owner.Native_resource_origin.route
           with
           | Direct actual -> assert (D.Reference.equal actual reference)
           | Ancestry _ -> failwith "639 direct preparation received ancestry admission");
          assert (phys_equal (Owner.Native_source_stage.admission stage) admission);
          Owner.Native_source_stage.check stage |> protocol_ok;
          let setup =
            Owner.Native_source_stage.prepare
              stage
              ~source_owner:host.source_owner
              ~source:host.source
              ~expected
            |> protocol_ok
          in
          Actor.Native_setup.check setup |> protocol_ok;
          setups := setup :: !setups;
          Int.incr prepare_calls;
          if !raise_prepare
          then (
            try raise sentinel with
            | exn ->
              let trace = Stdlib.Printexc.get_raw_backtrace () in
              producer_trace := Some trace;
              Exn.raise_with_original_backtrace exn trace)
          else Error error)
        ~pin:(fun ~admission:_ ~expected_native:_ ->
          Int.incr pin_calls;
          failwith "663 failed cold preparation reached native pin")
        ~check:(fun _ ~admission:_ ~expected:_ ->
          Int.incr check_calls;
          failwith "663 failed cold preparation checked a native loan")
        ~check_custody:(fun _ _ ->
          Int.incr custody_calls;
          failwith "639 failed native preparation checked custody")
    in
    let owner =
      Owner.create_with_deferred_run_unload
        ~source_owner:host.source_owner
        ~actor:host.actor
        ~initial:None
        ~before_unload:(fun ~closing -> if closing then Ok () else !before_unload ())
        ~policy_resource_cleanup:(Resource.cleanup_port host.cell)
        ~policy_resource_access:None
        ~native_resource_access:(Some native_access)
        ~build:(fun ~initialization_cleanup:_ ~original_actor:_ _ ->
          failwith "663 cold native evidence entered ordinary build")
        ~build_deferred:
          (fun
            ~initialization_cleanup:_
            ~source_consumer:_
            ~source:_
            ~expected_product:_
            ~construction:_ -> failwith "663 cold native evidence entered Run build")
        ~ordinary_warm:None
        ~delegated_load:None
    in
    protect_cleanup
      ~finally:(fun () -> Owner.close_and_wait owner)
      (fun () ->
         assert (not (Owner.is_loaded owner));
         let borrow () =
           Owner.with_native_resources_observed
             owner
             ~source_owner:host.source_owner
             ~reference
             ~authorize
             ~capture_release:(fun _ -> Int.incr captures)
             ~f:(fun ~check:_ _ ->
               Int.incr body_calls;
               CO.create (Returned (Ok ())) ~diagnostics:[] ())
         in
         let no_native_borrow outcome =
           let cleanup = Native.cleanup outcome in
           positive_observation (Native.Cleanup.observation cleanup);
           (match Native.Cleanup.pin cleanup with
            | Not_acquired -> ()
            | Acquired _ -> failwith "663 failed source preparation acquired a pin");
           match Scope.Cleanup.status (Native.Cleanup.outer_lease cleanup) with
           | Never_acquired -> ()
           | Joined | Failed _ ->
             failwith "663 failed source preparation entered native resource borrow"
         in
         let first = borrow () in
         let first_evidence, first_observation = preparation Source_callback first in
         (match CO.exit first_observation, Native.primary first with
          | Returned (Error actual), Returned (Error raw) ->
            assert (phys_equal actual error);
            assert (phys_equal raw error)
          | _ -> failwith "663 cold preparation Error lost its actual selected exit");
         let first_group = Option.value_exn (CO.primary first_observation) in
         (match Diagnostic.failures first_group with
          | [ Protocol actual ] -> assert (phys_equal actual error)
          | _ -> failwith "663 cold preparation Error lost its actual source carrier");
         (match Native.resume first with
          | Error actual -> assert (phys_equal actual error)
          | Ok () -> failwith "663 cold preparation Error raw projection changed");
         no_native_borrow first;
         [%test_eq: int] 1 !prepare_calls;
         (match Actor.Native_setup.check (List.hd_exn !setups) with
          | Error _ -> ()
          | Ok () -> failwith "663 actual failed native setup stayed open");
         raise_prepare := true;
         let second = borrow () in
         let _, second_observation = preparation Source_callback second in
         actual_exception second_observation sentinel;
         (match CO.exit second_observation, Native.primary second with
          | Raised (_, actual_trace), Raised (raw, raw_trace) ->
            assert (phys_equal raw sentinel);
            let expected =
              Stdlib.Printexc.raw_backtrace_to_string (Option.value_exn !producer_trace)
            in
            assert (not (String.is_empty expected));
            assert (
              String.is_prefix
                (Stdlib.Printexc.raw_backtrace_to_string actual_trace)
                ~prefix:expected);
            assert (
              String.is_prefix
                (Stdlib.Printexc.raw_backtrace_to_string raw_trace)
                ~prefix:expected)
          | _ -> failwith "663 cold preparation raise lost actual producer trace");
         (match Native.resume second with
          | _ -> failwith "663 cold preparation raised raw projection changed"
          | exception actual ->
            let actual_trace = Stdlib.Printexc.get_raw_backtrace () in
            assert (phys_equal actual sentinel);
            assert (
              String.is_prefix
                (Stdlib.Printexc.raw_backtrace_to_string actual_trace)
                ~prefix:
                  (Stdlib.Printexc.raw_backtrace_to_string
                     (Option.value_exn !producer_trace))));
         no_native_borrow second;
         assert (
           phys_equal (Option.value_exn (Native.preparation_failure first)) first_evidence);
         assert (
           phys_equal
             (Native.Preparation_failure.observation first_evidence)
             first_observation);
         assert (
           not
             (Diagnostic.equal
                first_group
                (Option.value_exn (CO.primary second_observation))));
         (match !setups with
          | [ second_setup; first_setup ] ->
            assert (not (phys_equal second_setup first_setup));
            List.iter !setups ~f:(fun setup ->
              match Actor.Native_setup.check setup with
              | Error _ -> ()
              | Ok () -> failwith "663 actual cold setup escaped Source cleanup")
          | _ -> failwith "663 cold preparation did not enter two real setups");
         [%test_eq: int] 2 !prepare_calls;
         [%test_eq: int] 0 !pin_calls;
         [%test_eq: int] 0 !check_calls;
         [%test_eq: int] 0 !custody_calls;
         [%test_eq: int] 0 !captures;
         [%test_eq: int] 0 !body_calls;
         assert (not (Owner.is_loaded owner));
         let expected = Host.state host in
         let observe () =
           Owner.observe_native_source
             owner
             ~handle:host.handle
             ~profile:host.profile
             ~expected
         in
         assert (Option.is_none (observe () |> protocol_ok));
         let assert_busy () =
           match observe () with
           | Error { code = Conflict; retryable = true; _ } -> ()
           | Error error -> raise_s [%sexp (error : P.Error.t)]
           | Ok _ -> failwith "unfinished Owner retirement exposed a native Source fact"
         in
         let failure = P.Error.invalid_request "actual whole-Owner retirement failure" in
         (before_unload
          := fun () ->
               assert_busy ();
               Error failure);
         (match Owner.unload_and_wait owner with
          | Error actual -> assert (phys_equal actual failure)
          | Ok () -> failwith "whole-Owner retirement dropped the failure");
         assert (not (Owner.is_loaded owner));
         assert_busy ();
         (before_unload := fun () -> Ok ());
         Owner.unload_and_wait owner |> protocol_ok;
         assert (Option.is_none (observe () |> protocol_ok)))
  in
  let installed_source = ref None in
  let when_idle host owner reference authorize =
    let observed = Option.value_exn !installed_source in
    let expected = Host.state host in
    assert (
      Option.is_none
        (Owner.observe_native_source
           owner
           ~handle:host.handle
           ~profile:host.profile
           ~expected
         |> protocol_ok));
    (* The observation owns no loan: actual finite unload can release the unit,
       and the saved Source must then refuse current use before cold setup. *)
    (match Owner.Native_source.check observed ~profile:host.Host.profile ~expected with
     | Error { code = Conflict; _ } -> ()
     | Error error -> raise_s [%sexp (error : P.Error.t)]
     | Ok () -> failwith "668 released physical Source remained current");
    cold_preparation host reference authorize
  in
  let run () =
    with_borrow_owner ~when_idle (fun host owner reference authorize ->
      let expected = Host.state host in
      let observed =
        Owner.observe_native_source
          owner
          ~handle:host.handle
          ~profile:host.profile
          ~expected
        |> protocol_ok
        |> Option.value_exn
      in
      installed_source := Some observed;
      assert (phys_equal (Owner.Native_source.source observed) host.source);
      Owner.Native_source.check observed ~profile:host.profile ~expected |> protocol_ok;
      let repeated =
        Owner.observe_native_source
          owner
          ~handle:host.handle
          ~profile:host.profile
          ~expected
        |> protocol_ok
        |> Option.value_exn
      in
      assert (
        phys_equal
          (Owner.Native_source.source repeated)
          (Owner.Native_source.source observed));
      let foreign =
        { expected with
          identity =
            { expected.identity with generation = expected.identity.generation + 1 }
        }
      in
      (match
         Owner.Native_source.check observed ~profile:host.profile ~expected:foreign
       with
       | Error { code = Conflict; _ } -> ()
       | Error error -> raise_s [%sexp (error : P.Error.t)]
       | Ok () -> failwith "668 installed Source accepted a foreign generation");
      let foreign_domain =
        Stdlib.Domain.spawn (fun () ->
          Owner.Native_source.check observed ~profile:host.profile ~expected)
        |> Stdlib.Domain.join
      in
      (match foreign_domain with
       | Error { code = Invalid_request; _ } -> ()
       | Error error -> raise_s [%sexp (error : P.Error.t)]
       | Ok () -> failwith "668 installed Source crossed its creating domain");
      Owner.Native_source.check observed ~profile:host.profile ~expected |> protocol_ok;
      let expected_summary = expected |> A.Session_state.summary in
      let callback_error = P.Error.invalid_request "661 actual nested native callback" in
      let callback_group = ref None in
      let a = ref None in
      let b = ref None in
      let outer_custody = ref None in
      let origin_checks = ref 0 in
      let origin =
        Owner.Native_resource_origin.create
          ~source_owner:host.source_owner
          ~route:(Direct reference)
          ~authorize:(fun ~route ~target ->
            Int.incr origin_checks;
            match route with
            | Direct actual ->
              assert (D.Reference.equal actual reference);
              Result.map (authorize ~reference:actual ~target) ~f:(fun () ->
                Owner.Native_resource_origin.Validation.Direct)
            | Ancestry _ -> failwith "639 Direct origin changed route")
      in
      (match Owner.Native_resource_origin.parent origin |> protocol_ok with
       | None -> ()
       | Some _ -> failwith "639 Direct origin fabricated an ancestry parent");
      let borrow capture_release f =
        Owner.with_native_resources_observed
          owner
          ~source_owner:host.source_owner
          ~reference
          ~authorize
          ~capture_release
          ~f
      in
      let denial =
        P.Error.create
          Permission_denied
          ~message:"663 genuine host authorization denied"
          ~retryable:false
          ()
      in
      let denied_capture = ref 0 in
      let denied_body = ref 0 in
      let denied_authorize = ref 0 in
      let denied =
        Owner.with_native_resources_observed
          owner
          ~source_owner:host.source_owner
          ~reference
          ~authorize:(fun ~reference ~target ->
            let open Result.Let_syntax in
            let%bind () = authorize ~reference ~target in
            Int.incr denied_authorize;
            Error denial)
          ~capture_release:(fun _ -> Int.incr denied_capture)
          ~f:(fun ~check:_ _ ->
            Int.incr denied_body;
            CO.create (Returned (Ok ())) ~diagnostics:[] ())
      in
      let _, denied_observation = preparation Admission denied in
      (match CO.exit denied_observation, Native.primary denied with
       | Returned (Error actual), Returned (Error raw) ->
         assert (phys_equal actual denial);
         assert (phys_equal raw denial)
       | _ -> failwith "663 denied admission lost its actual typed primary");
      positive_observation (Native.Cleanup.observation (Native.cleanup denied));
      (match Native.Cleanup.pin (Native.cleanup denied) with
       | Not_acquired -> ()
       | Acquired _ -> failwith "663 authorization refusal acquired a native pin");
      [%test_eq: int] 1 !denied_authorize;
      [%test_eq: int] 0 !denied_capture;
      [%test_eq: int] 0 !denied_body;
      let outer =
        Owner.with_native_source_resources_for_origin_observed
          owner
          ~source_owner:host.source_owner
          ~origin
          ~capture_release:(fun actual -> a := Some actual)
          ~f:(fun ~check ~custody ~source _resources ->
            outer_custody := Some custody;
            let checked_before_custody = !origin_checks in
            Owner.Native_resource_custody.check custody |> protocol_ok;
            [%test_eq: int] checked_before_custody !origin_checks;
            let foreign =
              Stdlib.Domain.spawn (fun () -> Owner.Native_resource_custody.check custody)
              |> Stdlib.Domain.join
            in
            (match foreign with
             | Error { code = Invalid_request; _ } -> ()
             | Error error -> raise_s [%sexp (error : P.Error.t)]
             | Ok () -> failwith "639 custody crossed its creating domain");
            assert (phys_equal source host.source);
            let actual_a = Option.value_exn !a in
            assert (Release.equal_status Held (Release.status actual_a |> protocol_ok));
            let refusal =
              Eio.Fiber.fork_promise ~sw:host.sw (fun () ->
                Release.close_observed actual_a)
              |> Eio.Promise.await_exn
            in
            let refusal_error =
              match CO.exit refusal with
              | Returned (Error ({ code = Conflict; _ } as error)) -> error
              | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
              | Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace
              | Returned (Ok ()) ->
                failwith "645 external close consumed a live callback pin"
            in
            let refusal_group = Option.value_exn (CO.primary refusal) in
            assert (List.equal Diagnostic.equal (CO.diagnostics refusal) [ refusal_group ]);
            (match Diagnostic.failures refusal_group with
             | [ Protocol actual ] -> assert (phys_equal actual refusal_error)
             | _ -> failwith "native live refusal lost its actual control failure");
            assert (Release.equal_status Held (Release.status actual_a |> protocol_ok));
            let inner =
              borrow
                (fun actual -> b := Some actual)
                (fun ~check _ ->
                   check () |> protocol_ok;
                   let group = Diagnostic.create [ Protocol callback_error ] in
                   callback_group := Some group;
                   CO.create
                     (Returned (Error callback_error))
                     ~diagnostics:[ group ]
                     ~primary:group
                     ())
            in
            let callback_group = Option.value_exn !callback_group in
            let inner_observation = Native.observation inner in
            let _, inner_preparation = preparation Lease_callback inner in
            (match CO.exit inner_preparation with
             | Returned (Error actual) -> assert (phys_equal actual callback_error)
             | _ -> failwith "663 native callback evidence lost actual Error");
            (match CO.exit inner_observation with
             | Returned (Error error) -> assert (phys_equal error callback_error)
             | Returned (Ok ()) | Raised _ ->
               failwith "observed native callback primary was replaced");
            assert (
              List.equal
                Diagnostic.equal
                (CO.diagnostics inner_observation)
                [ callback_group ]);
            assert (
              Option.equal
                Diagnostic.equal
                (CO.primary inner_observation)
                (Some callback_group));
            (match Native.resume inner with
             | Error error -> assert (phys_equal error callback_error)
             | Ok () -> failwith "raw native projection lost callback failure");
            positive_observation (Native.Cleanup.observation (Native.cleanup inner));
            let actual_b = Option.value_exn !b in
            assert (not (phys_equal actual_a actual_b));
            released actual_b;
            Release.close actual_b |> protocol_ok;
            Release.close actual_b |> protocol_ok;
            (* SAME released B never joins/closes the competing live A pin. *)
            assert (Release.equal_status Held (Release.status actual_a |> protocol_ok));
            let checked_before_custody = !origin_checks in
            Owner.Native_resource_custody.check custody |> protocol_ok;
            [%test_eq: int] checked_before_custody !origin_checks;
            check () |> protocol_ok;
            positive (Native.Cleanup.combined (Native.cleanup inner));
            assert (
              Option.is_none (Native.Cleanup.pin_only_failure (Native.cleanup inner)));
            (* Both are actual existing Owners around the same physical Cell;
               this outcome belongs only to the admitted borrowed provider. *)
            let cleanup = Native.cleanup inner in
            let original_cleanup = Native.Cleanup.observation cleanup in
            let foreign_owner = Host.owner host in
            assert (not (phys_equal owner foreign_owner));
            let checks_before_outer_guard = !origin_checks in
            (match Native.Cleanup.outer_recovery cleanup ~owner with
             | Ok None -> ()
             | Ok (Some _) -> failwith "639 positive outer cleanup admitted repair"
             | Error error -> raise_s [%sexp (error : P.Error.t)]);
            (match Native.Cleanup.outer_recovery cleanup ~owner:foreign_owner with
             | Error { code = Invalid_request; _ } -> ()
             | Error error -> raise_s [%sexp (error : P.Error.t)]
             | Ok _ -> failwith "639 outer cleanup accepted another actual Owner");
            let foreign_domain =
              Stdlib.Domain.spawn (fun () -> Native.Cleanup.outer_recovery cleanup ~owner)
              |> Stdlib.Domain.join
            in
            (match foreign_domain with
             | Error { code = Invalid_request; _ } -> ()
             | Error error -> raise_s [%sexp (error : P.Error.t)]
             | Ok _ -> failwith "639 outer cleanup crossed its creating domain");
            [%test_eq: int] checks_before_outer_guard !origin_checks;
            assert (phys_equal original_cleanup (Native.Cleanup.observation cleanup));
            released actual_b;
            let parent = Resource.Parent_cleanup.create ~sw:host.sw in
            Resource.Parent_cleanup.capture_release parent actual_b;
            Resource.Parent_cleanup.record parent ~owner inner;
            (match Resource.Parent_cleanup.record parent ~owner inner with
             | () -> failwith "645 duplicate Parent report replaced immutable outcome"
             | exception Invalid_argument _ -> ());
            (* Parent consumes only actual cleanup. The handled callback failure
             must not turn positive release/outer custody into a negative. *)
            positive_observation (Resource.Parent_cleanup.close parent);
            CO.create (Returned (Ok expected_summary)) ~diagnostics:[] ())
      in
      let outer_observation = Native.observation outer in
      assert (Option.is_none (Native.preparation_failure outer));
      (match CO.exit outer_observation with
       | Returned (Ok actual) -> assert (phys_equal actual expected_summary)
       | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
       | Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace);
      assert (List.is_empty (CO.diagnostics outer_observation));
      assert (Option.is_none (CO.primary outer_observation));
      let actual_summary = Native.resume outer |> protocol_ok in
      assert (phys_equal actual_summary expected_summary);
      let actual_a = Option.value_exn !a in
      released actual_a;
      (match Owner.Native_resource_custody.check (Option.value_exn !outer_custody) with
       | Error { code = Conflict; _ } -> ()
       | Error error -> raise_s [%sexp (error : P.Error.t)]
       | Ok () -> failwith "639 released loan retained current custody");
      Release.close actual_a |> protocol_ok;
      let captured = ref None in
      let entered = ref false in
      let sentinel = Failure "645 actual capture callback threw" in
      let failed =
        borrow
          (fun actual ->
             captured := Some actual;
             raise sentinel)
          (fun ~check:_ _ ->
             entered := true;
             CO.create (Returned (Ok ())) ~diagnostics:[] ())
      in
      assert (not !entered);
      (match Native.primary failed with
       | Raised (exn, _) when phys_equal exn sentinel -> ()
       | Raised (exn, bt) -> Exn.raise_with_original_backtrace exn bt
       | Returned _ -> failwith "645 capture exception lost its original primary");
      actual_exception (Native.observation failed) sentinel;
      let _, failed_preparation = preparation Lease_callback failed in
      actual_exception failed_preparation sentinel;
      positive_observation (Native.Cleanup.observation (Native.cleanup failed));
      let actual = Option.value_exn !captured in
      (match Native.Cleanup.pin (Native.cleanup failed) with
       | Acquired { release; cleanup } ->
         assert (phys_equal actual release);
         positive cleanup
       | Not_acquired -> failwith "645 throwing capture lost actual returned pin custody");
      released actual;
      Release.close actual |> protocol_ok;
      positive (Native.Cleanup.outer_lease (Native.cleanup failed));
      positive (Native.Cleanup.preparation (Native.cleanup failed));
      let cancellation = Failure "645 actual caller cancellation after pin return" in
      let cancelled_capture = ref None in
      let cancelled_entered = ref false in
      let cancelled =
        Eio.Cancel.sub (fun context ->
          borrow
            (fun actual ->
               cancelled_capture := Some actual;
               Eio.Cancel.cancel context cancellation)
            (fun ~check:_ _ ->
               cancelled_entered := true;
               CO.create (Returned (Ok ())) ~diagnostics:[] ()))
      in
      assert (not !cancelled_entered);
      (match Native.primary cancelled with
       | Raised (Eio.Cancel.Cancelled cause, _) -> assert (phys_equal cause cancellation)
       | Raised (exn, bt) -> Exn.raise_with_original_backtrace exn bt
       | Returned _ -> failwith "645 actual cancellation after capture disappeared");
      (match CO.exit (Native.observation cancelled) with
       | Raised (Eio.Cancel.Cancelled cause, _) -> assert (phys_equal cause cancellation)
       | Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace
       | Returned _ -> failwith "observed native cancellation after capture disappeared");
      let _, cancelled_preparation = preparation Lease_callback cancelled in
      (match CO.exit cancelled_preparation with
       | Raised (Eio.Cancel.Cancelled cause, _) -> assert (phys_equal cause cancellation)
       | _ -> failwith "663 cancelled pin callback lost actual preparation evidence");
      positive_observation (Native.Cleanup.observation (Native.cleanup cancelled));
      let actual = Option.value_exn !cancelled_capture in
      (match Native.Cleanup.pin (Native.cleanup cancelled) with
       | Acquired { release; cleanup } ->
         assert (phys_equal release actual);
         positive cleanup
       | Not_acquired -> failwith "645 actual cancellation abandoned returned pin");
      released actual;
      Release.close actual |> protocol_ok;
      positive (Native.Cleanup.combined (Native.cleanup cancelled)))
  in
  let provider_close () =
    with_borrow_owner (fun host owner reference authorize ->
      let expected = Host.state host in
      let actual_release = ref None in
      let closing, finish_closing = Eio.Promise.create () in
      let forked = ref false in
      protect_cleanup
        ~finally:(fun () -> if !forked then Eio.Promise.await closing |> Host.resume)
        (fun () ->
           let origin =
             Owner.Native_resource_origin.create
               ~source_owner:host.source_owner
               ~route:(Direct reference)
               ~authorize:(fun ~route:_ ~target ->
                 Result.map (authorize ~reference ~target) ~f:(fun () ->
                   Owner.Native_resource_origin.Validation.Direct))
           in
           let outcome =
             Owner.with_native_source_resources_for_origin_observed
               owner
               ~source_owner:host.source_owner
               ~origin
               ~capture_release:(fun release -> actual_release := Some release)
               ~f:(fun ~check:_ ~custody ~source _ ->
                 Owner.Native_resource_custody.check custody |> protocol_ok;
                 Owner.Native_resource_custody.check_source custody ~source |> protocol_ok;
                 Eio.Switch.check host.sw;
                 Eio.Fiber.fork ~sw:host.sw (fun () ->
                   Eio.Promise.resolve
                     finish_closing
                     (Host.capture (fun () -> Owner.close owner)));
                 forked := true;
                 (* The new fiber reaches the actual Actor-retire RPC after
                    permanent exclusion, before its reply can retire this loan.
                    No join of our own native callback occurs here. *)
                 assert (Option.is_none (Eio.Promise.peek closing));
                 let release = Option.value_exn !actual_release in
                 assert (Release.equal_status Held (Release.status release |> protocol_ok));
                 (match Owner.Native_resource_custody.check custody with
                  | Error { code = Server_shutting_down; message; _ } ->
                    [%test_eq: string] "session runtime owner is closed" message
                  | Error error -> raise_s [%sexp (error : P.Error.t)]
                  | Ok () -> failwith "639 closed provider retained native current use");
                 (match Owner.Native_resource_custody.check_source custody ~source with
                  | Error { code = Server_shutting_down; _ } -> ()
                  | Error error -> raise_s [%sexp (error : P.Error.t)]
                  | Ok () -> failwith "closed provider retained native Source custody");
                 (match
                    Owner.observe_native_source
                      owner
                      ~handle:host.handle
                      ~profile:host.profile
                      ~expected
                  with
                  | Error { code = Server_shutting_down; _ } -> ()
                  | Error error -> raise_s [%sexp (error : P.Error.t)]
                  | Ok _ -> failwith "closing provider exposed a native Source fact");
                 assert (Release.equal_status Held (Release.status release |> protocol_ok));
                 CO.create (Returned (Ok ())) ~diagnostics:[] ())
           in
           Native.resume outcome |> protocol_ok;
           positive_observation (Native.Cleanup.observation (Native.cleanup outcome));
           let release = Option.value_exn !actual_release in
           released release;
           (* Current-use refusal never gates factual release or its repeat. *)
           Release.close release |> protocol_ok))
  in
  let recording = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
    ~f:(fun () ->
      Stdlib.Printexc.record_backtrace true;
      run ();
      provider_close ());
  print_endline
    "real pins: live refusal, distinct released witness, immutable Parent, throwing \
     capture and cancellation custody";
  [%expect
    {| real pins: live refusal, distinct released witness, immutable Parent, throwing capture and cancellation custody |}]
;;

let%expect_test
    "Product attachment close forwards same Cell producer through a real cache fault"
  =
  let module Observation = A.Close_observation in
  let module Diagnostic = A.Cleanup_diagnostic in
  let module Cache_failure = Native_resource_failure_tests.Cache_failure in
  let fault = Cache_failure.create () in
  let sentinel = Failure "657 actual promoted native cache close" in
  let gate = ref None in
  let producer_trace = ref None in
  let fail _path =
    Option.iter !gate ~f:(fun (reached, release) ->
      Eio.Promise.resolve reached ();
      Eio.Promise.await release);
    try raise sentinel with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      producer_trace := Some backtrace;
      Exn.raise_with_original_backtrace exn backtrace
  in
  let phase = ref "657 actual Product Cell close receipt" in
  let run () =
    Host.with_recovered_host
      ~phase
      ~wrap_env:(Cache_failure.wrap ~fail fault)
      (fun host with_request ->
         fault.cache
         <- Some
              (Filename.concat
                 (Agent_store.Session_store.Handle.cache_directory host.handle)
                 "cache.bin");
         with_request ~original (fun native ->
           Host.with_native_construction host native (fun constructed ->
             let matching_source =
               B.Source.prepare
                 host.revision
                 ~script_preparation:Daemon.default_options.script_preparation
                 ~invocation:
                   (A.Runtime_script_preparation.Invocation.of_root
                      constructed.expected.spec.root_invocation)
                 ~env:host.env
                 ~checkpoint:Eio.Fiber.check
               |> protocol_ok
             in
             B.Source.validate_target matching_source constructed.expected |> protocol_ok;
             assert (not (phys_equal matching_source host.source));
             Host.with_native_promotion host constructed (fun promotion _product ->
               let check_source source =
                 Resource.Native_loan.check_retained_source
                   constructed.loan
                   ~source
                   ~expected:constructed.expected
               in
               check_source host.source |> protocol_ok;
               (match check_source matching_source with
                | Error { code = Conflict; message; _ } ->
                  [%test_eq: string] message "retained native Source identity changed"
                | Error error -> raise_s [%sexp (error : P.Error.t)]
                | Ok () -> failwith "attachment accepted a different physical Source");
               (match
                  Resource.Native_loan.with_source_resources
                    constructed.loan
                    ~f:(fun ~source:_ _ ->
                      failwith "attachment allowed resource projection")
                with
                | Error { code = Conflict; message; _ } ->
                  [%test_eq: string]
                    message
                    "native attachment construction or cleanup remains owned"
                | Error error -> raise_s [%sexp (error : P.Error.t)]
                | Ok _ -> failwith "attachment accepted a strict resource check");
               let binding = Resource.Attachment.binding promotion |> protocol_ok in
               let loan, _access =
                 Resource.Attachment.publish
                   promotion
                   ~basis:constructed.basis
                   ~expected:constructed.expected
                 |> protocol_ok
               in
               let attachment =
                 B.Product.Binding.attach_reported
                   binding
                   ~runtime:(B.Product.Binding.runtime binding)
                   ~close:(fun () ->
                     let observed = Resource.Loan.close_reported loan in
                     let exit =
                       match Observation.exit observed with
                       | Returned (Ok ()) -> Observation.Returned ()
                       | Raised (exn, backtrace) -> Observation.Raised (exn, backtrace)
                       | Returned (Error error) ->
                         (match protocol_ok (Error error) with
                          | () -> Observation.Returned ()
                          | exception exn ->
                            Observation.Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
                     in
                     Observation.create
                       exit
                       ~diagnostics:(Observation.diagnostics observed)
                       ?primary:(Observation.primary observed)
                       ())
                 |> protocol_ok
               in
               let attempts_before = fault.attempts in
               let publications_before = fault.publications in
               (* Actual promoted Native/Product references use the SAME count. *)
               Resource.Native_loan.close constructed.loan |> protocol_ok;
               [%test_eq: int] fault.attempts attempts_before;
               let close_faulted expected_attempts =
                 Eio.Switch.run (fun sw ->
                   let reached, reached_u = Eio.Promise.create () in
                   let release, release_u = Eio.Promise.create () in
                   let first, first_u = Eio.Promise.create () in
                   let joined, joined_u = Eio.Promise.create () in
                   let joining, joining_u = Eio.Promise.create () in
                   gate := Some (reached_u, release);
                   fault.armed <- true;
                   Exn.protect
                     ~finally:(fun () ->
                       if not (Eio.Promise.is_resolved release)
                       then Eio.Promise.resolve release_u ();
                       gate := None)
                     ~f:(fun () ->
                       Eio.Fiber.fork ~sw (fun () ->
                         Eio.Promise.resolve
                           first_u
                           (B.Attachment.close_reported attachment));
                       Eio.Promise.await reached;
                       [%test_eq: int] expected_attempts fault.attempts;
                       [%test_eq: bool] (Eio.Promise.is_resolved first) false;
                       Eio.Fiber.fork ~sw (fun () ->
                         Eio.Promise.resolve joining_u ();
                         Eio.Promise.resolve
                           joined_u
                           (Resource.close_pending_reported host.cell));
                       Eio.Promise.await joining;
                       let common =
                         Eio.Fiber.fork_promise ~sw (fun () ->
                           B.Prepared_native.close_reported constructed.prepared)
                       in
                       let common_joined =
                         Eio.Fiber.fork_promise ~sw (fun () ->
                           B.Prepared_native.close_reported constructed.prepared)
                       in
                       Eio.Fiber.yield ();
                       [%test_eq: bool] (Eio.Promise.is_resolved joined) false;
                       assert (Option.is_none (Eio.Promise.peek common));
                       assert (Option.is_none (Eio.Promise.peek common_joined));
                       Eio.Promise.resolve release_u ();
                       ( Eio.Promise.await first
                       , Eio.Promise.await joined
                       , Eio.Promise.await_exn common
                       , Eio.Promise.await_exn common_joined )))
               in
               let first, joined, common, common_joined =
                 close_faulted (attempts_before + 1)
               in
               let common_group =
                 match Observation.diagnostics common with
                 | [ group ] -> group
                 | _ -> failwith "661 actual common attempt lost its single cache origin"
               in
               let assert_common actual group =
                 assert (
                   List.equal Diagnostic.equal (Observation.diagnostics actual) [ group ]);
                 assert (
                   Option.equal Diagnostic.equal (Observation.primary actual) (Some group));
                 let producer_trace =
                   match Diagnostic.failures group with
                   | [ Raised (actual, trace) ] ->
                     assert (phys_equal actual sentinel);
                     trace
                   | _ -> failwith "661 common native close lost actual raw cache failure"
                 in
                 match Observation.exit actual with
                 | Raised (actual, trace) ->
                   assert (phys_equal actual sentinel);
                   assert (phys_equal trace producer_trace)
                 | Returned () ->
                   failwith "661 common native close swallowed actual cache failure"
               in
               assert_common common common_group;
               assert_common common_joined common_group;
               let first_groups = Observation.diagnostics first in
               [%test_eq: bool]
                 (List.equal
                    Diagnostic.equal
                    first_groups
                    (Observation.diagnostics joined))
                 true;
               [%test_eq: int] (List.length first_groups) 1;
               let first_group = List.hd_exn first_groups in
               [%test_eq: bool]
                 (Option.exists
                    (Observation.primary first)
                    ~f:(Diagnostic.equal first_group))
                 true;
               [%test_eq: bool]
                 (Option.exists
                    (Observation.primary joined)
                    ~f:(Diagnostic.equal first_group))
                 true;
               let expected_trace = Option.value_exn !producer_trace in
               (match Diagnostic.failures common_group with
                | [ Raised (_, trace) ] ->
                  assert (
                    String.is_prefix
                      (Stdlib.Printexc.raw_backtrace_to_string trace)
                      ~prefix:(Stdlib.Printexc.raw_backtrace_to_string expected_trace))
                | _ -> failwith "common native producer lost actual fault trace");
               let first_trace =
                 match Diagnostic.failures first_group with
                 | [ Raised (actual, backtrace) ] when phys_equal actual sentinel ->
                   backtrace
                 | _ -> failwith "657 expected actual native cache failure"
               in
               [%test_eq: bool]
                 (Stdlib.Printexc.raw_backtrace_length expected_trace > 0)
                 true;
               [%test_eq: bool]
                 (String.is_prefix
                    (Stdlib.Printexc.raw_backtrace_to_string first_trace)
                    ~prefix:(Stdlib.Printexc.raw_backtrace_to_string expected_trace))
                 true;
               (match Observation.exit first, Observation.exit joined with
                | Raised (left, left_trace), Raised (right, right_trace) ->
                  [%test_eq: bool]
                    (phys_equal left sentinel && phys_equal right sentinel)
                    true;
                  [%test_eq: bool]
                    (phys_equal left_trace first_trace
                     && phys_equal right_trace first_trace)
                    true
                | _ -> failwith "657 lost actual Cell close raw outcome");
               [%test_eq: int] fault.attempts (attempts_before + 1);
               [%test_eq: int] fault.publications publications_before;
               (* A separately executed retained retry shares the SAME sentinel,
               but owns a distinct actual producer carrier. *)
               let second, second_joined, second_common, second_common_joined =
                 close_faulted (attempts_before + 2)
               in
               assert (
                 List.equal
                   Diagnostic.equal
                   (Observation.diagnostics second)
                   (Observation.diagnostics second_joined));
               let second_common_group =
                 match Observation.diagnostics second_common with
                 | [ group ] -> group
                 | _ -> failwith "661 retained common retry lost its actual cache origin"
               in
               assert (not (Diagnostic.equal common_group second_common_group));
               assert_common second_common second_common_group;
               assert_common second_common_joined second_common_group;
               assert_common common common_group;
               [%test_eq: int] fault.attempts (attempts_before + 2);
               [%test_eq: int] (List.length (Observation.diagnostics second)) 1;
               let second_group = List.hd_exn (Observation.diagnostics second) in
               [%test_eq: bool]
                 (Option.exists
                    (Observation.primary second)
                    ~f:(Diagnostic.equal second_group))
                 true;
               [%test_eq: bool] (Diagnostic.equal first_group second_group) false;
               (match Diagnostic.failures second_group with
                | [ Raised (actual, _) ] ->
                  [%test_eq: bool] (phys_equal actual sentinel) true
                | _ -> failwith "657 expected distinct actual retained cache failure");
               let final = B.Attachment.close_reported attachment in
               Observation.resume final;
               [%test_eq: int] (List.length (Observation.diagnostics final)) 0;
               [%test_eq: int] fault.attempts (attempts_before + 3);
               [%test_eq: int] fault.publications (publications_before + 1);
               [%test_eq: bool] (Resource.pending host.cell) false;
               (match Diagnostic.failures first_group with
                | [ Raised (actual, trace) ] ->
                  [%test_eq: bool]
                    (phys_equal actual sentinel && phys_equal trace first_trace)
                    true
                | _ -> failwith "657 original Cell report mutated");
               (* Both entrypoints retain ONE implementation; settled repeat close
               does not save the actual cache or execute physical release again. *)
               B.close (B.Attachment.runtime attachment);
               Resource.close_pending host.cell |> protocol_ok;
               B.Prepared_native.close_reported constructed.prepared |> Observation.resume;
               B.Prepared_native.close constructed.prepared;
               [%test_eq: int] fault.attempts (attempts_before + 3);
               [%test_eq: int] fault.publications (publications_before + 1);
               Ok ())
             |> protocol_ok;
             Ok ())
           |> protocol_ok))
  in
  let recording = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
    ~f:(fun () ->
      Stdlib.Printexc.record_backtrace true;
      run ());
  print_endline
    "same Cell producer joined; distinct retry retained; actual cache repaired";
  [%expect
    {| same Cell producer joined; distinct retry retained; actual cache repaired |}]
;;
