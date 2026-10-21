open Core
module A = Chatml_semantics
module C = Chatml_compilation
module H = Support
module Contract = Chatml.Chatml_compiler_contract

let standard target body =
  let contract = Contract.for_target target ~checkpoint:H.checkpoint in
  contract, H.input contract [ "entry.chatml", body, [] ]
;;

let%test_unit "static host schemes remain bounded across referenced modules" =
  let contract = Contract.for_target Moderator_v1 ~checkpoint:H.checkpoint in
  List.iter [ 0; 1; 2 ] ~f:(fun count ->
    let modules =
      List.init count ~f:(fun index ->
        let name = if Int.equal index 0 then "Counter" else "Counter2" in
        let path = if Int.equal index 0 then "counter.chatml" else "counter2.chatml" in
        name, path)
    in
    let entry_body =
      "let initial_state = 0;; let on_event ctx state event = Task.pure(state)"
      ^ (List.map modules ~f:(fun (name, _) ->
           ";; let " ^ String.lowercase name ^ " = " ^ name ^ ".initial")
         |> String.concat ~sep:"")
    in
    let input =
      H.input
        contract
        ~interfaces:(List.map modules ~f:(fun (_, path) -> path, "val initial : int", []))
        (("entry.chatml", entry_body, modules)
         :: List.map modules ~f:(fun (_, path) -> path, "let initial = 0", []))
    in
    let work = ref 0 in
    let checkpoint () = incr work in
    let snapshot =
      match H.analyze contract input ~checkpoint with
      | Ok (Complete snapshot) -> snapshot
      | Ok (Failed _) | Error _ -> failwith "ordinary checked module unavailable"
    in
    assert (A.Frozen.node_count (A.Snapshot.types snapshot) <= 20_000);
    assert (!work <= 400_000))
;;

let%test_unit
    "joined analysis uses the exact selected custom contract for full and prefix"
  =
  Eio_main.run (fun env ->
    let required_bindings =
      Chatml.Chatml_builtin_spec.
        [ "initial_state", TInt; "hook", TFun ([ TInt; TInt ], task_ty TInt) ]
    in
    let contract = H.contract ~requirements:required_bindings () in
    let body =
      "let poison = 1 / 0;; let initial_state = 0;; let hook event state = \
       Task.pure(state);; let helper = 1"
    in
    let input = H.input contract [ "entry.chatml", body, [] ] in
    let run input ~policy =
      C.analyze_prepared_with_contract_and_policy
        input
        ~contract
        ~policy
        ~semantic_limits:(H.limits ())
        ~max_dependency_depth:16
        ~env
        ()
    in
    let snapshot =
      match run input ~policy:(Bounded C.default_limits) with
      | Ok (A.Complete snapshot) -> snapshot
      | _ -> failwith "custom contract did not produce complete semantic facts"
    in
    assert (
      A.Captured.Digest.equal
        (A.Identity.selected_contract (A.Snapshot.identity snapshot))
        (Contract.fingerprint contract ~checkpoint:H.checkpoint));
    let context =
      H.context snapshot (H.point snapshot input "entry.chatml" (String.length body))
    in
    assert (
      List.mem
        (H.names snapshot (H.candidates snapshot context Value))
        "helper"
        ~equal:String.equal);
    (match
       C.analyze_prepared_prefix_with_contract_and_policy
         input
         ~source:(A.Captured.id (H.source input "entry.chatml"))
         ~cursor_byte:(String.length body)
         ~contract
         ~policy:(Bounded C.default_limits)
         ~semantic_limits:(H.limits ())
         ~max_dependency_depth:16
         ~env
         ()
     with
     | Ok (A.Prefix.Provisional provisional) ->
       assert (
         A.Captured.Digest.equal
           (A.Provisional.full_input_fingerprint provisional)
           (Chatml_prepared_program.fingerprint input));
       assert (
         A.Captured.Digest.equal
           (A.Identity.selected_contract
              (A.Snapshot.identity (A.Provisional.snapshot provisional)))
           (Contract.fingerprint contract ~checkpoint:H.checkpoint))
     | _ -> failwith "custom contract prefix facts unavailable");
    let partial = H.input contract [ "entry.chatml", "let initial_state =", [] ] in
    (match run partial ~policy:(Bounded C.default_limits) with
     | Ok (A.Failed _) -> ()
     | _ -> failwith "invalid custom-contract source was not partial");
    let foreign = Contract.for_target One_off_v1 ~checkpoint:H.checkpoint in
    (match
       C.analyze_prepared_with_contract_and_policy
         input
         ~contract:foreign
         ~policy:(Bounded C.default_limits)
         ~semantic_limits:(H.limits ())
         ~max_dependency_depth:16
         ~env
         ()
     with
     | Error (Analysis _) -> ()
     | Ok _ | Error (Policy _) -> failwith "foreign contract was accepted");
    let entered = Atomic.make 0 in
    let measured =
      Domain_fixture.environment
        env
        ~before:(fun () -> Atomic.incr entered)
        ~finally:(fun () -> ())
    in
    let input_bytes =
      Chatml_prepared_program.Accounting.input_bytes
        (Chatml_prepared_program.accounting input)
    in
    (match
       C.analyze_prepared_with_contract_and_policy
         input
         ~contract
         ~policy:(Bounded { C.default_limits with max_source_bytes = input_bytes + 1 })
         ~semantic_limits:(H.limits ())
         ~max_dependency_depth:16
         ~env:measured
         ()
     with
     | Error (Policy error) ->
       [%test_eq: string] error.code "chatml.metadata_allocation_limit"
     | _ -> failwith "custom contract metadata admission escaped");
    [%test_eq: int] (Atomic.get entered) 0)
;;

let%test_unit
    "joined standard analysis and pure analysis agree without initializer execution"
  =
  Eio_main.run (fun env ->
    List.iter
      [ ( Contract.Target.One_off_v1
        , C.One_off_v1
        , "let poison = 1 / 0;; let main input = Task.pure(input)" )
      ; Tool_v1, Tool_v1, "let run ctx input = Task.pure(`Complete(input))"
      ; ( Moderator_v1
        , Moderator_v1
        , "let initial_state = 0;; let on_event ctx state event = Task.pure(state)" )
      ; ( Delegated_moderator_v1
        , Delegated_moderator_v1
        , "let initial_state = 0;; let on_event ctx state event = Task.pure(state)" )
      ]
      ~f:(fun (target, adapter, body) ->
        let contract, input = standard target body in
        let pure = H.complete contract input in
        List.iter [ C.Unrestricted; Bounded C.default_limits ] ~f:(fun policy ->
          match
            C.analyze_prepared_with_policy
              input
              ~policy
              ~semantic_limits:(H.limits ())
              ~max_dependency_depth:16
              ~env
              ~target:adapter
              ()
          with
          | Ok (Complete joined) ->
            assert (
              A.Captured.Digest.equal
                (A.Identity.input_fingerprint (A.Snapshot.identity pure))
                (A.Identity.input_fingerprint (A.Snapshot.identity joined)));
            assert (
              Int.equal
                (List.length (A.Snapshot.host_requirements pure))
                (List.length (A.Snapshot.host_requirements joined)));
            let context =
              H.context joined (H.point joined input "entry.chatml" (String.length body))
            in
            List.iter (H.candidates joined context Value) ~f:(fun binding ->
              let symbol =
                A.Snapshot.symbol joined (A.Snapshot.Binding.symbol binding)
                |> Result.ok
                |> Option.value_exn
              in
              match A.Snapshot.Symbol.origin symbol with
              | Host { contract = owner; _ } ->
                assert (
                  A.Captured.Digest.equal
                    owner
                    (A.Identity.selected_contract (A.Snapshot.identity joined)))
              | Authored _ | Implicit_specification_parameter _ -> ())
          | _ -> failwith "pure and joined analysis differ")))
;;

let%test_unit
    "policy admission happens before native worker, invalid programs stay partial"
  =
  Eio_main.run (fun env ->
    let _, input = standard One_off_v1 "let main input =" in
    let entered = Atomic.make 0 in
    let env =
      Domain_fixture.environment
        env
        ~before:(fun () -> Atomic.incr entered)
        ~finally:(fun () -> ())
    in
    (match
       C.analyze_prepared
         input
         ~limits:{ C.default_limits with max_source_bytes = 1 }
         ~semantic_limits:(H.limits ())
         ~max_dependency_depth:16
         ~env
         ~target:One_off_v1
         ()
     with
     | Error (Policy error) -> [%test_eq: string] error.code "chatml.source_limit"
     | _ -> failwith "policy source admission");
    assert (Int.equal (Atomic.get entered) 0);
    (match
       C.analyze_prepared
         input
         ~semantic_limits:(H.limits ())
         ~max_dependency_depth:16
         ~env
         ~target:One_off_v1
         ()
     with
     | Ok (Failed partial) ->
       (match A.Partial.invalidity partial with
        | Parse _ -> ()
        | _ -> failwith "parse partial")
     | _ -> failwith "invalid analysis is not a policy failure");
    assert (Int.equal (Atomic.get entered) 1))
;;

let cancelled_exception_marker exn = raise exn [@@inline never]

let%test_unit "analysis preserves unexpected domain exceptions and waits for cleanup" =
  Eio_main.run (fun env ->
    let _, input = standard One_off_v1 "let main input = Task.pure(input)" in
    let expected = Chatml_typechecker.Type_error "unexpected domain exception" in
    let finished = Atomic.make false in
    let failing =
      Domain_fixture.environment
        env
        ~before:(fun () -> raise expected)
        ~finally:(fun () -> Atomic.set finished true)
    in
    (match
       C.analyze_prepared_with_policy
         input
         ~policy:Unrestricted
         ~semantic_limits:(H.limits ())
         ~max_dependency_depth:16
         ~env:failing
         ~target:One_off_v1
         ()
     with
     | _ -> failwith "unexpected exception converted into an ordinary error"
     | exception actual -> assert (phys_equal actual expected));
    assert (Atomic.get finished);
    let expected_cancelled = Eio.Cancel.Cancelled Exit in
    let finished_cancelled = Atomic.make false in
    let cancelled =
      Domain_fixture.environment
        env
        ~before:(fun () -> cancelled_exception_marker expected_cancelled)
        ~finally:(fun () -> Atomic.set finished_cancelled true)
    in
    (match
       C.analyze_prepared_with_policy
         input
         ~policy:Unrestricted
         ~semantic_limits:(H.limits ())
         ~max_dependency_depth:16
         ~env:cancelled
         ~target:One_off_v1
         ()
     with
     | _ -> failwith "cancelled exception converted into an ordinary error"
     | exception actual ->
       let backtrace = Stdlib.Printexc.get_raw_backtrace () in
       assert (phys_equal actual expected_cancelled);
       assert (
         String.is_substring
           (Stdlib.Printexc.raw_backtrace_to_string backtrace)
           ~substring:"cancelled_exception_marker"));
    assert (Atomic.get finished_cancelled);
    let started, signal_started = Eio.Promise.create () in
    let block, _ = Eio.Promise.create () in
    let finished = Atomic.make false in
    let gated =
      Domain_fixture.environment
        env
        ~before:(fun () ->
          Eio.Promise.resolve signal_started ();
          Eio.Promise.await block)
        ~finally:(fun () -> Atomic.set finished true)
    in
    Eio.Switch.run (fun sw ->
      Eio.Cancel.sub (fun context ->
        Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.await started;
          Eio.Cancel.cancel context Exit);
        match
          C.analyze_prepared_with_policy
            input
            ~policy:Unrestricted
            ~semantic_limits:(H.limits ())
            ~max_dependency_depth:16
            ~env:gated
            ~target:One_off_v1
            ()
        with
        | _ -> failwith "cancelled analysis returned output"
        | exception Eio.Cancel.Cancelled _ -> assert (Atomic.get finished))))
;;
