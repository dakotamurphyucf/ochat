open Core
module A = Chatml_semantics
module C = Chatml_compilation
module H = Support
module Contract = Chatml.Chatml_compiler_contract

let standard target body =
  let contract = Contract.for_target target ~checkpoint:H.checkpoint in
  contract, H.input contract [ "entry.chatml", body, [] ]
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
