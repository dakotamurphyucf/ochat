open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module S = P.Subscription

let advance seconds =
  Time_ns.add (P.Timestamp.to_time_ns timestamp) (Time_ns.Span.of_int_sec seconds)
  |> P.Timestamp.of_time_ns
;;

let commit actor changes =
  let state = A.state actor |> protocol_ok in
  A.commit_extensions
    actor
    ~generation:0
    ~expected_revision:state.counters.revision
    changes
;;

let prepare actor =
  let registry = native_registry (ref 0) ~raises:false in
  let request =
    Background_execution_tests.capture_tool
      registry
      Chat_response.One_off_request.default_policy
  in
  let parent =
    add_claimed_job actor ~payload:(Chat_response.Background_request.to_json request)
  in
  let invocation = root parent in
  let invocation =
    I.create { invocation.context with deadline = Some (advance 1) } |> protocol_ok
  in
  let dispatched = I.dispatch invocation |> protocol_ok in
  let subscription =
    Subscription_transaction_tests.make_subscription
      ~parent_job:(parent.id, parent.attempt)
      dispatched
  in
  let resolved =
    I.resolve
      dispatched
      ~session_id
      ~generation:0
      (Pending (Subscription subscription.context.id, `String "accepted"))
    |> protocol_ok
  in
  commit
    actor
    [ Invocation invocation
    ; Invocation dispatched
    ; Subscription subscription
    ; Invocation resolved
    ]
  |> protocol_ok
  |> ignore;
  let dependency : J.dependency =
    { invocation_id = invocation.context.id
    ; work = Subscription subscription.context.id
    ; deadline = advance 1
    ; completion_schema = Some (`Object [ "const", `String "expected" ])
    ; max_output_bytes = 1_000_000
    ; max_output_depth = 128
    }
  in
  let parent =
    A.defer_background_job
      actor
      ~job_id:parent.id
      ~generation:0
      ~attempt:parent.attempt
      dependency
    |> protocol_ok
  in
  parent, subscription, dependency
;;

let%expect_test
    "subscription dependencies retain deadlines, contracts and atomic cancellation on \
     rejected saves"
  =
  List.iter [ `Success; `Schema; `Late; `Expired; `Cancel ] ~f:(fun mode ->
    let now = ref timestamp in
    let reject_save = ref false in
    with_actor
      ~now:(fun () -> !now)
      ~reject_save:(fun _ -> !reject_save)
      (fun _env _sw actor _writer backend ->
         let parent, subscription, _ = prepare actor in
         (match mode with
          | `Success | `Schema | `Late ->
            (match mode with
             | `Late -> now := advance 2
             | _ -> ());
            let value =
              match mode with
              | `Schema -> "wrong"
              | _ -> "expected"
            in
            let completed, _ =
              S.finish
                subscription
                ~expected_epoch:0
                ~now:!now
                (Succeeded (`String value))
              |> protocol_ok
            in
            commit actor [ Subscription completed ] |> protocol_ok |> ignore
          | `Expired -> now := advance 2
          | `Cancel -> ());
         let finish () =
           match mode with
           | `Cancel -> A.cancel_job_internal actor ~job_id:parent.id
           | _ ->
             A.refresh_background_job
               actor
               ~job_id:parent.id
               ~generation:0
               ~attempt:parent.attempt
         in
         let before = Agent_session.Memory_backend.state backend in
         reject_save := true;
         assert (Result.is_error (finish ()));
         let rejected = Agent_session.Memory_backend.state backend in
         assert (
           Sexp.equal
             (Agent_session.Session_state.sexp_of_t before)
             (Agent_session.Session_state.sexp_of_t rejected));
         reject_save := false;
         let result = finish () |> protocol_ok in
         let state = Agent_session.Memory_backend.state backend in
         let restored =
           Agent_session.Session_persistence.restore_snapshot
             (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
           |> store_ok
         in
         Agent_session.Session_state.validate restored |> protocol_ok;
         let completion =
           J.terminal_completion result |> protocol_ok |> Option.value_exn
         in
         print_s
           [%sexp
             (mode : [ `Success | `Schema | `Late | `Expired | `Cancel ])
           , (completion : P.Completion.t)
           , ((List.hd_exn restored.subscriptions).result : P.Completion.t option)]));
  [%expect
    {|
    (Success (Succeeded (String expected)) ((Succeeded (String expected))))
    (Schema
     (Failed
      ((code background.invalid_completion)
       (message
        "The eventual result does not satisfy the captured completion contract.")
       (retryable false) (details Null)))
     ((Succeeded (String wrong))))
    (Late Expired ((Succeeded (String expected))))
    (Expired Expired ((Cancelled "owning job stopped waiting")))
    (Cancel (Cancelled "job cancelled")
     ((Cancelled "owning job stopped waiting")))
    |}]
;;

let%expect_test "restored subscription waits reject lost or forged attempt ancestry" =
  with_actor (fun _env _sw actor _writer backend ->
    let parent, subscription, _ = prepare actor in
    let state = Agent_session.Memory_backend.state backend in
    let validate label context =
      let result =
        S.create context
        |> Result.bind ~f:(fun subscription ->
          Agent_session.Session_state.validate
            { state with subscriptions = [ subscription ] })
      in
      match result with
      | Error _ -> print_endline label
      | Ok () -> failwith (label ^ " was accepted")
    in
    validate "legacy without attempt" { subscription.context with parent_job = None };
    validate
      "future attempt"
      { subscription.context with parent_job = Some (parent.id, parent.attempt + 1) };
    validate
      "foreign parent"
      { subscription.context with parent_job = Some (P.Id.Job.create (), parent.attempt) };
    validate "lost source" { subscription.context with source = None };
    validate "foreign generation" { subscription.context with generation = 1 };
    validate
      "foreign invocation"
      { subscription.context with invocation_id = P.Id.Invocation.create () };
    let retried = { parent with attempt = parent.attempt + 1 } in
    assert (
      Result.is_error
        (Agent_session.Session_state.validate { state with jobs = [ retried ] })));
  [%expect
    {|
    legacy without attempt
    future attempt
    foreign parent
    lost source
    foreign generation
    foreign invocation
    |}]
;;

let%expect_test
    "saved dependency codecs preserve legacy jobs and distinguish subscriptions"
  =
  with_actor (fun _env _sw actor _writer _backend ->
    let parent, _, dependency = prepare actor in
    List.iter
      [ I.Job (P.Id.Job.of_string "job_child" |> protocol_ok)
      ; Subscription (P.Id.Subscription.of_string "sub_child" |> protocol_ok)
      ]
      ~f:(fun work ->
        let dependency =
          { dependency with
            work
          ; invocation_id = P.Id.Invocation.of_string "inv_target" |> protocol_ok
          }
        in
        let parent = { parent with status = Waiting_completion dependency } in
        let json = J.to_json parent in
        let restored = J.of_json json |> protocol_ok in
        let snapshot = J.sexp_of_t parent in
        let from_snapshot = J.t_of_sexp snapshot in
        List.iter [ restored; from_snapshot ] ~f:(fun saved ->
          match saved.J.status with
          | Waiting_completion decoded -> assert (J.equal_dependency dependency decoded)
          | _ -> failwith "dependency lost");
        let status = Jsonaf.member_exn "status" json in
        print_s [%sexp (status : Jsonaf.t)];
        print_s [%sexp (J.sexp_of_dependency dependency : Sexp.t)];
        let replace json name value =
          match json with
          | `Object fields ->
            `Object (List.Assoc.add fields ~equal:String.equal name value)
          | _ -> failwith "expected object fixture"
        in
        List.iter
          [ "schema_version", `Number "3"
          ; "job_id", `String "job_other"
          ; "work", I.work_to_json (Subscription (P.Id.Subscription.create ()))
          ]
          ~f:(fun (field, value) ->
            let changed = replace status field value in
            (* Adding the other version's ownership field is ambiguous. *)
            match field, work with
            | "job_id", I.Job _ | "work", Subscription _ -> ()
            | _ -> assert (Result.is_error (J.of_json (replace json "status" changed))))));
  [%expect
    {|
    (Object
     ((type (String waiting_completion)) (schema_version (Number 1))
      (invocation_id (String inv_target)) (job_id (String job_child))
      (deadline (String 2026-08-15T12:00:01.000000000Z))
      (completion_schema (Object ((const (String expected)))))
      (max_output_bytes (Number 1000000)) (max_output_depth (Number 128))))
    ((invocation_id inv_target) (job_id job_child)
     (deadline 2026-08-15T12:00:01.000000000Z)
     (completion_schema (Object ((const (String expected)))))
     (max_output_bytes 1000000) (max_output_depth 128))
    (Object
     ((type (String waiting_completion)) (schema_version (Number 2))
      (invocation_id (String inv_target))
      (work (Object ((type (String subscription)) (id (String sub_child)))))
      (deadline (String 2026-08-15T12:00:01.000000000Z))
      (completion_schema (Object ((const (String expected)))))
      (max_output_bytes (Number 1000000)) (max_output_depth (Number 128))))
    ((invocation_id inv_target) (work (Subscription sub_child))
     (deadline 2026-08-15T12:00:01.000000000Z)
     (completion_schema (Object ((const (String expected)))))
     (max_output_bytes 1000000) (max_output_depth 128))
    |}]
;;
