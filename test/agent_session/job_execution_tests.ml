open Core
open Fixtures
open Job_fixtures

let with_actor ?reject_save f =
  Job_fixtures.with_actor ?reject_save (fun _env sw actor writer backend ->
    f sw actor writer backend)
;;

let%expect_test
    "background native scopes own nested calls, reject escape and survive client detach"
  =
  with_actor (fun _sw actor writer backend ->
    let job = add_claimed_job actor in
    let calls = ref 0 in
    let nested = ref (fun () -> ()) in
    let registry = native_registry calls ~raises:false ~on_call:(fun () -> !nested ()) in
    let reference, invocation = native_context registry (root job) in
    let escaped = ref None in
    let execute_native execute invocation =
      N.run_scoped
        ~execute
        ~registry:(fun () -> registry)
        ~reference
        ~invocation
        ~is_halted:(fun () -> false)
        ~authorize:(fun _ _ -> Ok ())
        ~prepare_output:(fun _ -> Ok (`String "disclosed"))
    in
    with_job actor job (fun ~job:claimed ~execute ->
      [%test_eq: int] 1 claimed.attempt;
      escaped := Some execute;
      let before = A.state actor |> protocol_ok in
      reject "duplicate scope" (with_job actor job (fun ~job:_ ~execute:_ -> Ok ()));
      reject "early completion" (complete actor job);
      List.iter
        [ ( "foreign job"
          , { invocation.context with
              parent_job = Some (Agent_protocol.Id.Job.create ())
            } )
        ; "missing job", { invocation.context with parent_job = None }
        ; "no deadline", { invocation.context with deadline = None }
        ; "expired deadline", { invocation.context with deadline = Some timestamp }
        ; "foreign generation", { invocation.context with generation = 1 }
        ; ( "provider authority"
          , { invocation.context with origin = Model; provider_call_id = Some "fake" } )
        ]
        ~f:(fun (label, context) ->
          reject
            label
            (execute
               ~invocation:(I.create context |> protocol_ok)
               (fun ~dispatched:_ -> failwith "invalid background caller reached effects")));
      assert_same_session_snapshot before (A.state actor |> protocol_ok);
      A.detach actor writer.id |> protocol_ok;
      (nested
       := fun () ->
            match !calls with
            | 1 ->
              let borrowed = N.borrow () |> protocol_ok in
              let parent = N.borrowed_invocation borrowed in
              let child =
                I.create
                  { parent.context with
                    id = Agent_protocol.Id.Invocation.create ()
                  ; parent_job = None
                  ; parent_invocation = Some parent.context.id
                  }
                |> protocol_ok
              in
              execute_native (N.execute_borrowed borrowed) child |> protocol_ok |> ignore
            | _ -> ());
      execute_native execute invocation |> protocol_ok |> ignore;
      Ok ())
    |> protocol_ok;
    reject
      "expired executor"
      ((Option.value_exn !escaped) ~invocation:(root job) (fun ~dispatched:_ ->
         failwith "expired callback executed"));
    complete actor job |> protocol_ok |> ignore;
    let state = Agent_session.Memory_backend.state backend in
    print_s
      [%sexp
        { calls = (!calls : int)
        ; invocations = (List.length state.invocations : int)
        ; outputs = (List.map state.invocations ~f:(fun i -> i.I.status) : I.status list)
        ; foreground = (Option.is_some state.active_operation : bool)
        ; history = (List.length state.conversation.canonical_history : int)
        ; attached = (List.length state.attachments : int)
        }]);
  [%expect
    {|
    ("duplicate scope" Conflict)
    ("early completion" Conflict)
    ("foreign job" Conflict)
    ("missing job" Conflict)
    ("no deadline" Conflict)
    ("expired deadline" Conflict)
    ("foreign generation" Conflict)
    ("provider authority" Conflict)
    ("expired executor" Conflict)
    ((calls 2) (invocations 2)
     (outputs
      ((Resolved (Complete (String disclosed)))
       (Resolved (Complete (String disclosed)))))
     (foreground false) (history 0) (attached 0))
    |}]
;;

let%expect_test "job cancellation and permission cleanup share the durable transition" =
  List.iter [ `Approve; `Cancel; `Interrupt; `Stop ] ~f:(fun mode ->
    let reject_cancel = ref true in
    with_actor
      ~reject_save:(fun transition ->
        !reject_cancel
        && List.exists
             transition.Agent_session.Session_transition.state.jobs
             ~f:(fun job ->
               match job.status with
               | Cancelled -> true
               | _ -> false))
      (fun sw actor writer backend ->
         let job = add_claimed_job actor in
         let finished, finished_u = Eio.Promise.create () in
         let completed_callback = ref false in
         Eio.Fiber.fork ~sw (fun () ->
           let cancelled =
             try
               with_job actor job (fun ~job:_ ~execute ->
                 execute ~invocation:(root job) (fun ~dispatched ->
                   let resolution =
                     A.request_permission
                       actor
                       ~permission:(permission dispatched)
                       ~timeout_seconds:None
                       ~fallback:Deny
                     |> protocol_ok
                   in
                   Eio.Fiber.check ();
                   [%test_eq: Agent_protocol.Permission.choice]
                     Approve_once
                     resolution.choice;
                   completed_callback := true;
                   Ok (I.Complete `Null))
                 |> Result.map ~f:ignore)
               |> protocol_ok;
               false
             with
             | Eio.Cancel.Cancelled _ -> true
           in
           Eio.Promise.resolve finished_u cancelled);
         let pending = await_permission actor in
         let before = A.state actor |> protocol_ok in
         (match mode with
          | `Approve ->
            A.respond_permission
              actor
              ~attachment_id:writer.id
              ~principal_id:(Some principal_id)
              ~permission_id:pending.id
              ~permission_generation:0
              ~choice:Approve_once
              ~reason:None
            |> protocol_ok
            |> ignore
          | `Cancel ->
            reject "failed cancel save" (A.cancel_job_internal actor ~job_id:job.id);
            assert_same_session_snapshot before (A.state actor |> protocol_ok);
            reject_cancel := false;
            A.cancel_job_internal actor ~job_id:job.id |> protocol_ok |> ignore
          | `Interrupt ->
            A.interrupt_job
              actor
              ~job_id:job.id
              ~generation:0
              ~attempt:job.attempt
              ~reason:"worker interrupted"
            |> protocol_ok
            |> ignore
          | `Stop ->
            reject_cancel := false;
            A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore);
         let cancelled = Eio.Promise.await finished in
         let state = Agent_session.Memory_backend.state backend in
         let job =
           List.find_exn state.jobs ~f:(fun j -> Agent_protocol.Id.Job.equal j.id job.id)
         in
         print_s
           [%sexp
             (mode : [ `Approve | `Cancel | `Interrupt | `Stop ])
           , (cancelled : bool)
           , (!completed_callback : bool)
           , (List.map state.permissions ~f:(fun p -> p.Agent_protocol.Permission.state)
              : Agent_protocol.Permission.state list)
           , (List.map state.invocations ~f:(fun i -> i.I.status) : I.status list)];
         match mode with
         | `Approve -> complete actor job |> protocol_ok |> ignore
         | _ -> reject "late completion" (complete actor job)));
  [%expect
    {|
    (Approve false true (Approved) ((Resolved (Complete Null))))
    ("failed cancel save" Internal_error)
    (Cancel true false (Cancelled)
     ((Resolved (Cancelled "background job cancelled"))))
    ("late completion" Already_resolved)
    (Interrupt true false (Cancelled)
     ((Resolved (Cancelled "background job cancelled"))))
    ("late completion" Already_resolved)
    (Stop true false (Cancelled)
     ((Resolved (Cancelled "background job cancelled"))))
    ("late completion" Already_resolved)
    |}]
;;

let%expect_test
    "foreground completion leaves concurrent background invocation ownership intact"
  =
  let release, release_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        Eio.Promise.await release;
        match completed_worker_result input caps with
        | Ok result -> Agent_session.Operation_worker.Completed result
        | Error failure -> Failed failure))
    (fun _env actor _writer backend ->
       let job = add_claimed_job actor in
       with_job actor job (fun ~job:_ ~execute ->
         execute ~invocation:(root job) (fun ~dispatched ->
           Eio.Promise.resolve release_u ();
           let state = await_idle actor in
           assert (List.exists state.invocations ~f:(I.equal dispatched));
           print_s
             [%sexp
               (("foreground ended", Option.is_none state.active_operation)
                : string * bool)];
           Ok (I.Complete (`String "background finished later")))
         |> Result.map ~f:ignore)
       |> protocol_ok;
       complete actor job |> protocol_ok |> ignore;
       let state = Agent_session.Memory_backend.state backend in
       print_s
         [%sexp (List.map state.invocations ~f:(fun i -> i.I.status) : I.status list)]);
  [%expect
    {|
    ("foreground ended" true)
    ((Resolved (Complete (String "background finished later"))))
    |}]
;;

let%expect_test
    "returning before joined invocation completion fails and cancels escaped work"
  =
  with_actor (fun sw actor _writer backend ->
    let job = add_claimed_job actor in
    let entered, entered_u = Eio.Promise.create () in
    let finished, finished_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    reject
      "unfinished job callback"
      (with_job actor job (fun ~job:_ ~execute ->
         Eio.Fiber.fork ~sw (fun () ->
           let cancelled =
             try
               execute ~invocation:(root job) (fun ~dispatched:_ ->
                 Eio.Promise.resolve entered_u ();
                 Eio.Promise.await never;
                 failwith "unjoined work resumed")
               |> ignore;
               false
             with
             | Eio.Cancel.Cancelled _ -> true
           in
           Eio.Promise.resolve finished_u cancelled);
         Eio.Promise.await entered;
         Ok ()));
    [%test_eq: bool] true (Eio.Promise.await finished);
    let state = Agent_session.Memory_backend.state backend in
    print_s
      [%sexp
        (List.map state.invocations ~f:(fun i ->
           match i.I.status with
           | Resolved (Cancelled _) -> "cancelled"
           | _ -> "unexpected")
         : string list)];
    A.complete_job
      actor
      ~job_id:job.id
      ~generation:job.generation
      ~attempt:job.attempt
      (Agent_session.Runtime_builder.Model_failed "callback did not join its work")
    |> protocol_ok
    |> ignore);
  [%expect
    {|
    ("unfinished job callback" Conflict)
    (cancelled)
    |}]
;;

let%expect_test
    "background permission outlives a foreground turn without reviving its operation"
  =
  let release, release_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        Eio.Promise.await release;
        match completed_worker_result input caps with
        | Ok result -> Agent_session.Operation_worker.Completed result
        | Error failure -> Failed failure))
    (fun _env actor writer backend ->
       let job = add_claimed_job actor in
       Eio.Fiber.both
         (fun () ->
            with_job actor job (fun ~job:_ ~execute ->
              execute ~invocation:(root job) (fun ~dispatched ->
                let resolution =
                  A.request_permission
                    actor
                    ~permission:(permission dispatched)
                    ~timeout_seconds:None
                    ~fallback:Deny
                  |> protocol_ok
                in
                [%test_eq: Agent_protocol.Permission.choice]
                  Approve_once
                  resolution.choice;
                Ok (I.Complete `Null))
              |> Result.map ~f:ignore)
            |> protocol_ok)
         (fun () ->
            let pending = await_permission actor in
            Eio.Promise.resolve release_u ();
            let rec await_foreground () =
              let state = A.state actor |> protocol_ok in
              match state.active_operation with
              | Some _ ->
                Eio.Fiber.yield ();
                await_foreground ()
              | None -> state
            in
            let state = await_foreground () in
            print_s
              [%sexp
                (( "still waiting for job permission"
                 , match state.lifecycle.observed with
                   | Waiting_for_permission id ->
                     Agent_protocol.Id.Permission.equal id pending.id
                   | _ -> false )
                 : string * bool)];
            A.respond_permission
              actor
              ~attachment_id:writer.id
              ~principal_id:(Some principal_id)
              ~permission_id:pending.id
              ~permission_generation:0
              ~choice:Approve_once
              ~reason:None
            |> protocol_ok
            |> ignore);
       complete actor job |> protocol_ok |> ignore;
       let state = Agent_session.Memory_backend.state backend in
       print_s
         [%sexp
           (state.lifecycle.observed : Agent_protocol.Session.observed_state)
         , (Option.is_none state.active_operation : bool)]);
  [%expect
    {|
    ("still waiting for job permission" true)
    (Idle true)
    |}]
;;
