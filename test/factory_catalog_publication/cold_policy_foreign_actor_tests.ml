open! Core
open Agent_server_test_support
open Retry_support
module Host = Cold_policy_recovered_support
module Cold = Cold_policy_resource_tests
module Resource = Factory.For_testing
module Consumer = Owner.Resource_consumer
module Frames = Delegated_policy_claim_fault_tests
module Capture = Actor.Delegated_policy_claim.Capture

let denied message = function
  | Error (error : P.Error.t) ->
    assert (P.Error.equal_code error.code Conflict);
    assert (not error.retryable);
    [%test_eq: string] message error.message
  | Ok _ -> failwith "634 foreign Actor accepted another Actor's restoration"
;;

let%expect_test "genuine recovered policy tokens reject every foreign Actor port" =
  let phase = ref "real Factory seed and physical recovery" in
  (try
     Host.with_recovered_host ~phase (fun host with_request ->
       with_request ~original (fun (native : Cold.native_request) ->
         phase := "actual admitted resource-only construction";
         Host.with_construction host native ~admission:Actor_direct (fun constructed ->
           let other = native.caller_actor in
           assert (not (phys_equal host.actor other));
           let before = Actor.state other |> protocol_ok in
           phase := "foreign history reservation";
           Actor.reserve_policy_restoration_history_block
             other
             ~restoration:constructed.restoration
             ~count:1
           |> denied "policy restoration belongs to another Actor";
           phase := "foreign inference graph open";
           Actor.open_policy_restoration_inference_owner
             other
             ~restoration:constructed.restoration
             ~source:
               (Transcript.Source_id.of_string "634-foreign-actor"
                |> Result.ok_or_failwith)
           |> denied "policy restoration belongs to another Actor";
           phase := "foreign resource publication";
           Actor.publish_policy_restoration
             other
             ~restoration:constructed.restoration
             ~access:constructed.access
           |> denied "policy restoration belongs to another Actor";
           Frames.assert_same before (Actor.state other |> protocol_ok);
           Actor.Policy_restoration.check constructed.restoration |> protocol_ok;
           (* This lower trusted host owns the genuine first Cell lease BEFORE
              direct Actor publication. It does not claim RuntimeOwner custody. *)
           let loan = Host.publish_cell constructed |> protocol_ok in
           let published = ref None in
           protect_cleanup
             ~finally:(fun () ->
               Option.iter !published ~f:Actor.Policy_restoration.Published.close;
               Resource.Loan.close loan |> protocol_ok)
             (fun () ->
                phase := "publish on actual issuing Actor under real lease custody";
                let actual =
                  Actor.publish_policy_restoration
                    host.actor
                    ~restoration:constructed.restoration
                    ~access:constructed.access
                  |> protocol_ok
                in
                published := Some actual;
                Actor.Policy_restoration.Published.check actual |> protocol_ok;
                phase := "foreign capture of actual Published resource";
                Capture.create other ~access:(Restored_unloaded actual)
                |> denied "restored policy belongs to another Actor";
                Frames.assert_same before (Actor.state other |> protocol_ok);
                (* The same actual issuer remains usable after every refusal. No
                  callback, native effect, Model or ordinary worker is invoked. *)
                let own =
                  Capture.create host.actor ~access:(Restored_unloaded actual)
                  |> protocol_ok
                in
                Capture.with_extent own (fun () -> Ok ()) |> protocol_ok;
                (match Capture.disposition own with
                 | Released -> ()
                 | Waiting | Retained ->
                   failwith "634 actual empty capture extent did not join");
                assert (not (Actor.Delegated_policy_claim.pending other));
                Frames.assert_same constructed.expected (Host.state host);
                Ok ()))
         |> protocol_ok))
   with
   | exn ->
     let bt = Stdlib.Printexc.get_raw_backtrace () in
     (try Eio.traceln "634 recovered foreign Actor phase: %s" !phase with
      | _ -> ());
     Exn.raise_with_original_backtrace exn bt);
  let check_scoped_source product_source_selection =
    Host.with_recovered_host ~product_source_selection ~phase (fun host with_request ->
      let other_source =
        A.Runtime_builder.Source.prepare
          host.revision
          ~script_preparation:Daemon.default_options.script_preparation
          ~invocation:
            (A.Runtime_script_preparation.Invocation.of_root
               host.recovered_state.spec.root_invocation)
          ~env:host.env
          ~checkpoint:Eio.Fiber.check
        |> protocol_ok
      in
      assert (not (phys_equal other_source host.source));
      A.Runtime_builder.Source.validate_target other_source (Host.state host)
      |> protocol_ok;
      with_request ~original (fun (native : Cold.native_request) ->
        let expired = ref None in
        let callbacks = ref 0 in
        let report =
          Owner.with_delegated_policy_resource_reported
            (Host.owner host)
            ~request:native.request
            ~governance:native.governance
            ~authorize:native.authorize
            ~f:(fun resource ->
              Int.incr callbacks;
              expired := Some resource;
              let expected = Host.state host in
              let ports = !(host.counters.inference_ports) in
              let providers = !(host.counters.providers) in
              let lifecycle_calls = !(host.lifecycle_calls) in
              Owner.Delegated_policy_resource.check_current resource |> protocol_ok;
              (match product_source_selection with
               | Host.Product_source_selection.Selected ->
                 Owner.Delegated_policy_resource.check_source resource ~source:host.source
                 |> protocol_ok;
                 (match
                    Owner.Delegated_policy_resource.check_source
                      resource
                      ~source:other_source
                  with
                  | Error error ->
                    let expected =
                      P.Error.create
                        Conflict
                        ~message:"delegated policy resource selected Source changed"
                        ~retryable:false
                        ()
                    in
                    [%test_eq: Sexp.t]
                      (P.Error.sexp_of_t expected)
                      (P.Error.sexp_of_t error)
                  | Ok () ->
                    failwith "equal-target Source replaced actual selected identity");
                 Owner.Delegated_policy_resource.check_source resource ~source:host.source
                 |> protocol_ok
               | Legacy_missing ->
                 let actual_missing = Option.value_exn !(host.missing_product_source) in
                 (match
                    Owner.Delegated_policy_resource.check_source
                      resource
                      ~source:host.source
                  with
                  | Error error ->
                    [%test_eq: Sexp.t]
                      (P.Error.sexp_of_t actual_missing)
                      (P.Error.sexp_of_t error)
                  | Ok () -> failwith "legacy policy resource invented a selected Source"));
              Owner.Delegated_policy_resource.check_current resource |> protocol_ok;
              [%test_eq: int] ports !(host.counters.inference_ports);
              [%test_eq: int] providers !(host.counters.providers);
              [%test_eq: int] lifecycle_calls !(host.lifecycle_calls);
              Frames.assert_same expected (Host.state host);
              Ok ())
        in
        Owner.Scope_outcome.resume report |> protocol_ok;
        [%test_eq: int] 1 !callbacks;
        (* The real Caller is still current: only the scoped resource has ended. *)
        native.authorize () |> protocol_ok;
        let expired = Option.value_exn !expired in
        let expired_current =
          match Owner.Delegated_policy_resource.check_current expired with
          | Error ({ code = Conflict; retryable = true; _ } as error) -> error
          | Error error -> raise_s [%sexp (error : P.Error.t)]
          | Ok () -> failwith "expired policy resource remained current"
        in
        match
          Owner.Delegated_policy_resource.check_source expired ~source:host.source
        with
        | Error error ->
          [%test_eq: Sexp.t] (P.Error.sexp_of_t expired_current) (P.Error.sexp_of_t error)
        | Ok () -> failwith "expired policy resource retained selected Source custody"))
  in
  check_scoped_source Selected;
  check_scoped_source Legacy_missing;
  print_endline
    "real recovered admission and Published tokens stay with their issuing Actor";
  [%expect
    {| real recovered admission and Published tokens stay with their issuing Actor |}]
;;

let held_target cell target ~native ~product =
  match Resource.Target.status target ~cell |> protocol_ok with
  | Held facets ->
    assert (Option.equal A.Runtime_builder.Native_identity.equal facets.native native);
    assert (Option.equal A.Runtime_builder.Product_identity.equal facets.product product)
  | Released -> failwith "644 actual unit released before its full cleanup"
;;

let released_target cell target =
  match Resource.Target.status target ~cell |> protocol_ok with
  | Released -> ()
  | Held _ -> failwith "644 actual full cleanup did not release its target"
;;

let%expect_test "Cell cleanup identity follows actual ordinary loans and replacements" =
  let phase = ref "644 ordinary Cell target" in
  let expired = ref None in
  Host.with_recovered_host ~phase (fun host with_request ->
    let releases = ref 0 in
    host.hooks
    <- { Host.no_hooks with on_physical_release = (fun () -> Int.incr releases) };
    let owner = Host.owner host in
    let retired = ref None in
    let queued_replacement = ref None in
    for unit_number = 1 to 2 do
      with_request ~original (fun (native : Cold.native_request) ->
        Host.with_construction host native ~admission:Actor_direct (fun constructed ->
          let target = Resource.Ordinary.target constructed.ordinary in
          let consumer = Resource.Target.capture_consumer target ~owner |> protocol_ok in
          assert (
            Consumer.Status.equal
              (Consumer.check consumer |> protocol_ok)
              Consumer.Status.Held);
          if Int.equal unit_number 1
          then (
            let refused =
              Stdlib.Domain.spawn (fun () ->
                Result.is_error (Resource.Target.capture_consumer target ~owner)
                && Result.is_error (Consumer.check consumer))
              |> Stdlib.Domain.join
            in
            assert refused);
          let loan = Host.publish_cell constructed |> protocol_ok in
          let pin = ref None in
          protect_cleanup
            ~finally:(fun () ->
              Resource.Loan.close loan |> protocol_ok;
              Option.iter !pin ~f:(fun pin ->
                Resource.Native_loan.close pin |> protocol_ok))
            (fun () ->
               let product = Resource.Loan.product loan ~cell:host.cell |> protocol_ok in
               let identity = A.Runtime_builder.Product.identity product in
               assert (Resource.Target.equal target (Resource.Loan.target loan));
               held_target host.cell target ~native:None ~product:(Some identity);
               Option.iter !retired ~f:(fun (old, ordinary, old_consumer) ->
                 assert (not (Resource.Target.equal old target));
                 released_target host.cell old;
                 assert (
                   Consumer.Status.equal
                     (Consumer.check old_consumer |> protocol_ok)
                     Consumer.Status.Released);
                 assert (
                   Consumer.Status.equal
                     (Consumer.check consumer |> protocol_ok)
                     Consumer.Status.Held);
                 (* A's genuine token remains Released while B is Held. Signalling
                    A must neither elect work against B nor disturb its custody. *)
                 (match Consumer.signal old_consumer |> protocol_ok with
                  | Consumer.Request.Released -> ()
                  | Retirement _ ->
                    failwith "638 released resource elected replacement retirement");
                 held_target host.cell target ~native:None ~product:(Some identity);
                 assert (
                   Consumer.Status.equal
                     (Consumer.check consumer |> protocol_ok)
                     Consumer.Status.Held);
                 [%test_eq: int] !releases (unit_number - 1);
                 (* Queue B's real whole-owner retirement, but execute it only
                    after this actual construction scope returns. A must not
                    join or replace that unrelated election. *)
                 let work =
                   match Owner.signal_resource_retirement owner |> protocol_ok with
                   | Owner.Resource_retirement.Run work -> work
                   | Join _ -> failwith "638 replacement already had a retirement"
                 in
                 queued_replacement := Some work;
                 (match Consumer.signal old_consumer |> protocol_ok with
                  | Consumer.Request.Released -> ()
                  | Retirement _ ->
                    failwith "638 released resource joined replacement retirement");
                 (match Owner.signal_resource_retirement owner |> protocol_ok with
                  | Owner.Resource_retirement.Join actual ->
                    assert (phys_equal actual (Owner.Resource_retirement.completion work))
                  | Run _ -> failwith "638 old resource replaced the queued retirement");
                 held_target host.cell target ~native:None ~product:(Some identity);
                 [%test_eq: int] !releases (unit_number - 1);
                 Resource.Ordinary.close_original ordinary |> protocol_ok;
                 [%test_eq: int] !releases (unit_number - 1));
               let native =
                 Resource.pin_native
                   host.cell
                   ~expected_native:(A.Runtime_builder.Product.native_identity product)
                   ~expected:(Host.state host)
                 |> protocol_ok
               in
               pin := Some native;
               assert (Resource.Target.equal target (Resource.Native_loan.target native));
               (* Loaded-first pin is not a native-first facet or physical close. *)
               Resource.Loan.close loan |> protocol_ok;
               held_target host.cell target ~native:None ~product:(Some identity);
               assert (
                 Consumer.Status.equal
                   (Consumer.check consumer |> protocol_ok)
                   Consumer.Status.Held);
               [%test_eq: int] !releases (unit_number - 1);
               Resource.Native_loan.close native |> protocol_ok;
               released_target host.cell target;
               assert (
                 Consumer.Status.equal
                   (Consumer.check consumer |> protocol_ok)
                   Consumer.Status.Released);
               (match Resource.Target.capture_consumer target ~owner with
                | Error _ -> ()
                | Ok _ ->
                  failwith "638 consumer capture accepted an already released unit");
               [%test_eq: int] !releases unit_number;
               (* Positive sticky old custody neither inspects nor closes unit 2. *)
               Option.iter !retired ~f:(fun (old, _, old_consumer) ->
                 released_target host.cell old;
                 assert (
                   Consumer.Status.equal
                     (Consumer.check old_consumer |> protocol_ok)
                     Consumer.Status.Released));
               (match Resource.Target.check_foreign target with
                | Error _ -> ()
                | Ok _ -> failwith "644 Released target accepted a foreign Cell");
               retired := Some (target, constructed.ordinary, consumer);
               Ok ()))
        |> protocol_ok);
      Option.iter !queued_replacement ~f:(fun work ->
        Owner.Resource_retirement.run work;
        Owner.Resource_retirement.await (Owner.Resource_retirement.completion work)
        |> protocol_ok);
      queued_replacement := None;
      [%test_eq: int] !releases unit_number
    done;
    with_request ~original (fun (native : Cold.native_request) ->
      Host.with_construction host native ~admission:Actor_direct (fun constructed ->
        let target = Resource.Ordinary.target constructed.ordinary in
        (* The SAME captured full composite closes first. Build.closed is its
           trusted positive acknowledgement, not the callback's return value. *)
        Resource.Ordinary.close_original constructed.ordinary |> protocol_ok;
        Resource.Ordinary.close_original constructed.ordinary |> protocol_ok;
        expired := Some (host.cell, constructed.ordinary);
        released_target host.cell target;
        [%test_eq: int] !releases 3;
        Ok ())
      |> protocol_ok);
    (* Both constructors mint a real partial unit before any allocation. *)
    let expected = Host.state host in
    let failed =
      P.Error.create Invalid_state ~message:"644 retained partial" ~retryable:false ()
    in
    let partial_consumer = ref None in
    Resource.Empty_partial.with_ordinary
      host.cell
      ~expected
      ~failure:failed
      ~during:(fun partial ->
        let target = Resource.Empty_partial.target partial in
        let consumer = Resource.Target.capture_consumer target ~owner |> protocol_ok in
        partial_consumer := Some consumer;
        assert (
          Consumer.Status.equal
            (Consumer.check consumer |> protocol_ok)
            Consumer.Status.Held);
        held_target host.cell target ~native:None ~product:None;
        (match Resource.Empty_partial.try_unallocated_acknowledgement partial with
         | Error _ -> ()
         | Ok () -> failwith "644 unallocated partial acknowledged a full close");
        held_target host.cell target ~native:None ~product:None)
      ~after_failure:(fun target ->
        held_target host.cell target ~native:None ~product:None;
        Resource.close_pending host.cell |> protocol_ok;
        released_target host.cell target;
        assert (
          Consumer.Status.equal
            (Consumer.check (Option.value_exn !partial_consumer) |> protocol_ok)
            Consumer.Status.Released))
    |> protocol_ok;
    Resource.Empty_partial.with_native
      host.cell
      ~expected
      ~failure:failed
      ~during:(fun target ->
        let consumer = Resource.Target.capture_consumer target ~owner |> protocol_ok in
        partial_consumer := Some consumer;
        assert (
          Consumer.Status.equal
            (Consumer.check consumer |> protocol_ok)
            Consumer.Status.Held);
        held_target host.cell target ~native:None ~product:None)
      ~after_failure:(fun target ->
        held_target host.cell target ~native:None ~product:None;
        Resource.close_pending host.cell |> protocol_ok;
        released_target host.cell target;
        assert (
          Consumer.Status.equal
            (Consumer.check (Option.value_exn !partial_consumer) |> protocol_ok)
            Consumer.Status.Released))
    |> protocol_ok;
    (* A real empty constructor callback can fail before its explicit supplied
       failure. Its actual target must remain cleanable and the original escape
       must retain its raw producer trace. *)
    let check_callback_failure run =
      let sentinel = Failure "665 actual empty constructor callback" in
      let target = ref None in
      let producer_trace = ref None in
      let recording = Stdlib.Printexc.backtrace_status () in
      Exn.protect
        ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
        ~f:(fun () ->
          Stdlib.Printexc.record_backtrace true;
          (match
             run ~during:(fun actual ->
               target := Some actual;
               try raise sentinel with
               | exn ->
                 let trace = Stdlib.Printexc.get_raw_backtrace () in
                 producer_trace := Some trace;
                 Exn.raise_with_original_backtrace exn trace)
           with
           | _ -> failwith "665 empty constructor callback escape disappeared"
           | exception exn ->
             let trace = Stdlib.Printexc.get_raw_backtrace () in
             assert (phys_equal exn sentinel);
             let original =
               Stdlib.Printexc.raw_backtrace_to_string (Option.value_exn !producer_trace)
             in
             assert (not (String.is_empty original));
             assert (
               String.is_prefix
                 (Stdlib.Printexc.raw_backtrace_to_string trace)
                 ~prefix:original));
          let actual = Option.value_exn !target in
          held_target host.cell actual ~native:None ~product:None;
          Resource.close_pending host.cell |> protocol_ok;
          released_target host.cell actual)
    in
    check_callback_failure (fun ~during ->
      Resource.Empty_partial.with_ordinary
        host.cell
        ~expected
        ~failure:failed
        ~during:(fun partial -> during (Resource.Empty_partial.target partial))
        ~after_failure:(fun _ ->
          failwith "665 entered normal failure branch after escape"));
    check_callback_failure (fun ~during ->
      Resource.Empty_partial.with_native
        host.cell
        ~expected
        ~failure:failed
        ~during
        ~after_failure:(fun _ ->
          failwith "665 entered normal failure branch after escape")));
  let cell, ordinary = Option.value_exn !expired in
  (match Resource.Ordinary.access ordinary with
   | Error error ->
     assert (P.Error.equal_code error.code Conflict);
     assert (not error.retryable)
   | Ok _ -> failwith "665 expired probe disclosed restored access");
  Resource.Ordinary.close_original ordinary |> protocol_ok;
  Resource.cleanup_port cell |> ignore;
  Resource.close_pending cell |> protocol_ok;
  assert (not (Resource.pending cell));
  released_target cell (Resource.Ordinary.target ordinary);
  (* This fresh real Host is sequential, not a nested Eio scheduler. The previous
     Host has already finished every physical resource and original finalizer. *)
  phase := "638 fresh Held signal during permanent close";
  Host.with_recovered_host ~phase (fun host _with_request ->
    let owner = Host.owner host in
    let expected = Host.state host in
    let failed =
      P.Error.create
        Invalid_state
        ~message:"638 real empty close window"
        ~retryable:false
        ()
    in
    let target = ref None in
    let consumer = ref None in
    let unexpected = ref None in
    let closing, finish_closing = Eio.Promise.create () in
    let forked = ref false in
    protect_cleanup
      ~finally:(fun () ->
        (* Complete the genuine partial before joining either producer. If the
           regression elects work, run that exact claim rather than abandoning a
           queued fence or waiting for its own constructor to finish. *)
        Resource.close_pending host.cell |> protocol_ok;
        Option.iter !unexpected ~f:(fun claim ->
          let completion =
            match claim with
            | Owner.Resource_retirement.Run work ->
              Owner.Resource_retirement.run work;
              Owner.Resource_retirement.completion work
            | Join completion -> completion
          in
          Owner.Resource_retirement.await completion |> protocol_ok);
        if !forked then Eio.Promise.await closing |> Host.resume)
      (fun () ->
         Resource.Empty_partial.with_ordinary
           host.cell
           ~expected
           ~failure:failed
           ~during:(fun partial ->
             let actual = Resource.Empty_partial.target partial in
             target := Some actual;
             let selected =
               Resource.Target.capture_consumer actual ~owner |> protocol_ok
             in
             consumer := Some selected;
             Eio.Switch.check host.sw;
             Eio.Fiber.fork ~sw:host.sw (fun () ->
               Eio.Promise.resolve
                 finish_closing
                 (Host.capture (fun () -> Owner.close owner)));
             forked := true;
             (* Pinned Eio runs the new fiber immediately and queues this caller
                ahead of the Actor. Close's first suspension is its genuine
                retire RPC: permanent admission is closed, no Whole is elected,
                and the reply has not installed a foreground retirement yet. *)
             assert (Option.is_none (Eio.Promise.peek closing));
             (match Consumer.signal selected with
              | Error error ->
                assert (P.Error.equal_code error.code Conflict);
                assert (not error.retryable);
                [%test_eq: string]
                  "resource consumer cannot elect after permanent close"
                  error.message
              | Ok Consumer.Request.Released ->
                failwith "638 live partial was treated as already Released"
              | Ok (Retirement claim) ->
                unexpected := Some claim;
                failwith "638 fresh Cell election raced permanent close");
             assert (
               Consumer.Status.equal
                 (Consumer.check selected |> protocol_ok)
                 Consumer.Status.Held))
           ~after_failure:(fun actual ->
             held_target host.cell actual ~native:None ~product:None)
         |> protocol_ok);
    released_target host.cell (Option.value_exn !target);
    assert (
      Consumer.Status.equal
        (Consumer.check (Option.value_exn !consumer) |> protocol_ok)
        Consumer.Status.Released);
    assert (not (Resource.pending host.cell)));
  print_endline
    "ordinary Product/pin share one target; full cleanup releases only that unit";
  [%expect
    {| ordinary Product/pin share one target; full cleanup releases only that unit |}]
;;

let%expect_test
    "native publication and repeated Product attachment share the full cleanup target"
  =
  let module Cache_failure = Native_resource_failure_tests.Cache_failure in
  let phase = ref "644 native Cell target" in
  let fault = Cache_failure.create () in
  Host.with_recovered_host
    ~wrap_env:(Cache_failure.wrap fault)
    ~phase
    (fun host with_request ->
       fault.cache
       <- Some
            (Filename.concat
               (Agent_store.Session_store.Handle.cache_directory host.handle)
               "cache.bin");
       let releases = ref 0 in
       host.hooks
       <- { Host.no_hooks with on_physical_release = (fun () -> Int.incr releases) };
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock host.env) 10. (fun () ->
         with_request ~original (fun (native : Cold.native_request) ->
           Host.with_native_construction host native (fun constructed ->
             let target = Resource.Native.target constructed.native in
             let prepared_identity = ref None in
             assert (
               Resource.Target.equal target (Resource.Native_loan.target constructed.loan));
             held_target
               host.cell
               target
               ~native:
                 (Some (A.Runtime_builder.Prepared_native.identity constructed.prepared))
               ~product:None;
             let product_loan = ref None in
             protect_cleanup
               ~finally:(fun () ->
                 Option.iter !product_loan ~f:(fun loan ->
                   Resource.Loan.close loan |> protocol_ok))
               (fun () ->
                  Host.with_native_promotion host constructed (fun promotion product ->
                    let native =
                      A.Runtime_builder.Prepared_native.identity
                        (Resource.Attachment.prepared promotion |> protocol_ok)
                    in
                    prepared_identity := Some native;
                    assert (
                      Resource.Target.equal target (Resource.Attachment.target promotion));
                    held_target
                      host.cell
                      target
                      ~native:(Some native)
                      ~product:(Some (A.Runtime_builder.Product.identity product));
                    phase := "644 close first nonlast attachment";
                    Resource.Attachment.close promotion |> protocol_ok;
                    Resource.Attachment.close promotion |> protocol_ok;
                    (* Real attachment close is nonlast: common native scope lives. *)
                    held_target host.cell target ~native:(Some native) ~product:None;
                    [%test_eq: int] !releases 0;
                    Ok ())
                  |> protocol_ok;
                  Host.with_native_promotion host constructed (fun promotion product ->
                    let native = Option.value_exn !prepared_identity in
                    held_target
                      host.cell
                      target
                      ~native:(Some native)
                      ~product:(Some (A.Runtime_builder.Product.identity product));
                    let loan, _access =
                      Resource.Attachment.publish
                        promotion
                        ~basis:constructed.basis
                        ~expected:constructed.expected
                      |> protocol_ok
                    in
                    product_loan := Some loan;
                    assert (Resource.Target.equal target (Resource.Loan.target loan));
                    Resource.Loan.close loan |> protocol_ok;
                    held_target
                      host.cell
                      target
                      ~native:(Some native)
                      ~product:(Some (A.Runtime_builder.Product.identity product));
                    [%test_eq: int] !releases 0;
                    (* Actual common cache persistence fails before its child Switch
                    can exit. Only explicit SAME-pin retry can release this unit. *)
                    let attempts = fault.attempts in
                    let publications = fault.publications in
                    protect_cleanup
                      ~finally:(fun () -> fault.armed <- false)
                      (fun () ->
                         phase := "644 observe actual cache failure on last pin";
                         fault.armed <- true;
                         (match Resource.Native_loan.close constructed.loan with
                          | Ok () ->
                            failwith "644 target swallowed actual cache close failure"
                          | Error error ->
                            raise_s
                              [%sexp
                                "644 changed raised cache failure", (error : P.Error.t)]
                          | exception Core_unix.Unix_error (EIO, operation, path) ->
                            [%test_eq: string]
                              operation
                              "635 native final cache publication";
                            assert (Cache_failure.temporary fault path));
                         [%test_eq: int] fault.failures 1;
                         [%test_eq: int] fault.attempts (attempts + 1);
                         [%test_eq: int] fault.publications publications;
                         held_target
                           host.cell
                           target
                           ~native:(Some native)
                           ~product:(Some (A.Runtime_builder.Product.identity product));
                         [%test_eq: int] !releases 0;
                         phase := "644 retry same retained last pin";
                         Resource.Native_loan.close constructed.loan |> protocol_ok;
                         released_target host.cell target;
                         [%test_eq: int] !releases 1;
                         [%test_eq: int] fault.failures 1;
                         [%test_eq: int] fault.attempts (attempts + 2);
                         [%test_eq: int] fault.publications (publications + 1));
                    Ok ())
                  |> protocol_ok;
                  Ok ()))
           |> protocol_ok)));
  print_endline
    "native Build/publication/attachments/Product retain one shared cleanup identity";
  [%expect
    {| native Build/publication/attachments/Product retain one shared cleanup identity |}]
;;
