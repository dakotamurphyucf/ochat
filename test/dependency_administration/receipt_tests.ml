open! Core
open Support

let%expect_test "acknowledged Pending precedes source work; replay survives lost response"
  =
  with_directory (fun env directory ->
    let store = open_store env directory in
    let mutex = Eio.Mutex.create () in
    let receipt = receipt store mutex in
    let request = request "lost-response" in
    let key, digest = identity request in
    let expected_canonical =
      {|{"definitions":["prd_alpha","prd_beta"],"expected_generation":3,"idempotency_key":"lost-response"}|}
    in
    assert (
      String.equal
        digest
        (Digestif.SHA256.digest_string expected_canonical |> Digestif.SHA256.to_hex));
    assert (String.equal key.method_name "prompt.dependencies.update");
    assert (Option.is_none key.session_id);
    let executions = ref 0 in
    let result =
      dispatch receipt request (fun ~checkpoint ->
        assert_pending store request;
        (match lookup store request with
         | Store.Replay record ->
           assert (P.Timestamp.equal record.created_at now);
           assert (
             Option.equal
               P.Timestamp.equal
               record.expires_at
               (Some (P.Timestamp.of_string "2026-10-12T12:00:00Z" |> protocol_ok)))
         | _ -> assert false);
        Eio.Mutex.use_rw ~protect:true mutex (fun () -> ());
        checkpoint ();
        incr executions;
        printf "before effect: protected Pending\n";
        Ok (committed request))
      |> protocol_ok
    in
    assert (S.Update_result.matches_request result request);
    (* Response delivery is separate from the already durable operation. *)
    let reopened = open_store env directory in
    let receipt = Support.receipt reopened (Eio.Mutex.create ()) in
    let replay =
      dispatch
        receipt
        (Support.request ~names:[ "prd_alpha"; "prd_beta" ] "lost-response")
        (fun ~checkpoint:_ -> failwith "must not execute replay")
      |> protocol_ok
    in
    assert (S.Update_result.equal replay result);
    assert_success reopened request;
    printf
      "after lost response: replay generation %d, executions %d\n"
      (S.Generation.to_int (S.Update_result.generation replay))
      !executions;
    assert_error
      (dispatch
         receipt
         (Support.request ~generation_number:4 "lost-response")
         (fun ~checkpoint:_ -> failwith "must not execute conflict"))
      Idempotency_conflict);
  [%expect
    {|
    before effect: protected Pending
    after lost response: replay generation 4, executions 1 |}]
;;

let%test_unit "duplicate Pending does not await source work or hold receipt mutex" =
  with_directory (fun env directory ->
    Eio.Switch.run (fun sw ->
      let store = open_store env directory in
      let receipt = receipt store (Eio.Mutex.create ()) in
      let request = request "concurrent" in
      let entered, signal_entered = Eio.Promise.create () in
      let release, signal_release = Eio.Promise.create () in
      let first =
        Eio.Fiber.fork_promise ~sw (fun () ->
          dispatch receipt request (fun ~checkpoint:_ ->
            Eio.Promise.resolve signal_entered ();
            Eio.Promise.await release;
            Ok (committed request)))
      in
      Eio.Promise.await entered;
      assert_error
        (dispatch receipt request (fun ~checkpoint:_ -> failwith "duplicate effect"))
        Interrupted;
      let other = Support.request "other-concurrent" in
      ignore
        (dispatch receipt other (fun ~checkpoint:_ -> Ok (committed other)) |> protocol_ok);
      Eio.Promise.resolve signal_release ();
      ignore (Eio.Promise.await_exn first |> protocol_ok);
      assert_success store request))
;;

let%test_unit
    "rejection persists; uncertain and mismatched claimed commits remain Pending"
  =
  with_directory (fun env directory ->
    let store = open_store env directory in
    let receipt = receipt store (Eio.Mutex.create ()) in
    let failure =
      P.Error.create
        Invalid_state
        ~message:"known precommit rejection"
        ~retryable:false
        ()
    in
    let rejected = request "rejected" in
    assert_error
      (dispatch receipt rejected (fun ~checkpoint:_ ->
         Error (Dependency_administration.Failure.Rejected failure)))
      Invalid_state;
    (match lookup store rejected with
     | Store.Replay { outcome = Failure actual; _ } ->
       assert (String.equal actual.message failure.message);
       assert (P.Error.equal_code actual.code failure.code)
     | _ -> failwith "known rejection was not recorded");
    List.iter [ "uncertain"; "mismatched" ] ~f:(fun name ->
      let request = Support.request name in
      assert_error
        (dispatch receipt request (fun ~checkpoint:_ ->
           if String.equal name "uncertain"
           then Error (Dependency_administration.Failure.Uncertain failure)
           else Ok (committed (Support.request ~generation_number:4 name))))
        Interrupted;
      assert_pending store request;
      assert_error
        (dispatch receipt request (fun ~checkpoint:_ ->
           failwith "unknown outcome retries"))
        Interrupted);
    let reopened = open_store env directory in
    assert_error
      (dispatch
         (Support.receipt reopened (Eio.Mutex.create ()))
         rejected
         (fun ~checkpoint:_ -> failwith "replayed rejection executes"))
      Invalid_state)
;;

let%test_unit "guard exceptions escape after unlocking without poisoning the shared mutex"
  =
  with_directory (fun env directory ->
    let store = open_store env directory in
    let receipt = receipt store (Eio.Mutex.create ()) in
    let request = request "guard-identity" in
    let marker = io_marker directory in
    (match
       dispatch
         ~authorize:(fun () -> raise marker)
         receipt
         request
         (fun ~checkpoint:_ -> failwith "unauthorized execution")
     with
     | _ -> failwith "guard exception disappeared"
     | exception exn -> assert (phys_equal exn marker));
    (match lookup store request with
     | Store.Missing -> ()
     | _ -> failwith "guard failure wrote Pending");
    ignore
      (dispatch receipt request (fun ~checkpoint:_ -> Ok (committed request))
       |> protocol_ok);
    let denied = Support.request "denied" in
    let denied_error =
      P.Error.create Permission_denied ~message:"actor is not current" ~retryable:false ()
    in
    assert_error
      (dispatch
         ~authorize:(fun () -> Error denied_error)
         receipt
         denied
         (fun ~checkpoint:_ -> failwith "denied execution"))
      Permission_denied;
    match lookup store denied with
    | Store.Missing -> ()
    | _ -> failwith "denied guard wrote receipt")
;;

let%test_unit
    "callback exception and cancellation preserve Pending and exception identity"
  =
  with_directory (fun env directory ->
    let store = open_store env directory in
    let receipt = receipt store (Eio.Mutex.create ()) in
    let marker = io_marker directory in
    let request = request "callback-identity" in
    (match dispatch receipt request (fun ~checkpoint:_ -> raise marker) with
     | _ -> failwith "callback exception disappeared"
     | exception exn -> assert (phys_equal marker exn));
    assert_pending store request;
    let cancellation = ref None in
    let request = Support.request "cancelled" in
    (match
       Eio.Cancel.sub (fun context ->
         dispatch receipt request (fun ~checkpoint:_ ->
           Eio.Cancel.cancel context marker;
           try
             Eio.Fiber.check ();
             failwith "cancelled operation continued"
           with
           | exn ->
             cancellation := Some exn;
             raise exn))
     with
     | _ -> failwith "cancellation disappeared"
     | exception exn ->
       assert (Option.value_map !cancellation ~default:false ~f:(phys_equal exn)));
    assert_pending store request;
    assert_error
      (dispatch receipt request (fun ~checkpoint:_ ->
         failwith "cancelled replay executes"))
      Interrupted)
;;

let%test_unit "acknowledged completion does not reauthorize or checkpoint after commit" =
  with_directory (fun env directory ->
    let store = open_store env directory in
    let receipt = receipt store (Eio.Mutex.create ()) in
    let request = request "revoked-after-ack" in
    let current = ref true in
    let authorize () =
      if !current
      then Ok ()
      else Error (P.Error.create Permission_denied ~message:"revoked" ~retryable:false ())
    in
    let checkpoint () = assert !current in
    ignore
      (dispatch ~authorize ~checkpoint receipt request (fun ~checkpoint:_ ->
         current := false;
         Ok (committed request))
       |> protocol_ok);
    assert_success store request;
    assert_error
      (dispatch ~authorize receipt request (fun ~checkpoint:_ ->
         failwith "revoked replay"))
      Permission_denied)
;;

let%test_unit
    "before/after terminal index rename failure never rolls back or retries committed \
     effect"
  =
  List.iter [ false; true ] ~f:(fun after ->
    with_directory (fun env directory ->
      let armed = ref None in
      let faulted = fault_env env ~armed ~marker:(io_marker directory) in
      let store = open_store faulted directory in
      let receipt = receipt store (Eio.Mutex.create ()) in
      let request = request "completion-fault" in
      let effects = ref 0 in
      assert_error
        (dispatch receipt request (fun ~checkpoint:_ ->
           assert_pending store request;
           incr effects;
           armed := Some after;
           Ok (committed request)))
        Interrupted;
      assert (!effects = 1);
      assert_pending store request;
      let durable = open_store env directory in
      let result =
        dispatch
          (Support.receipt durable (Eio.Mutex.create ()))
          request
          (fun ~checkpoint:_ -> failwith "receipt uncertainty reran committed effect")
      in
      if after
      then (
        ignore (protocol_ok result);
        assert_success durable request)
      else (
        assert_error result Interrupted;
        assert_pending durable request)))
;;

let%test_unit "Pending acknowledgment failure never begins source effects" =
  with_directory (fun env directory ->
    let armed = ref None in
    let faulted = fault_env env ~armed ~marker:(io_marker directory) in
    let store = open_store faulted directory in
    let receipt = receipt store (Eio.Mutex.create ()) in
    armed := Some false;
    let request = request "pending-write-fault" in
    assert_error
      (dispatch receipt request (fun ~checkpoint:_ ->
         failwith "source effects before durable Pending"))
      Persistence_error;
    match lookup store request with
    | Store.Missing -> ()
    | _ -> failwith "unacknowledged Pending was installed")
;;
