open! Core
open Support
module C = Agent_client

let status = D.Status.{ history = Tracked (generation 8); publication = Uncertain }
let error () = P.Error.create Interrupted ~message:"reply lost" ~retryable:false ()
let pending connection = C.Connection.pending_commands connection |> List.hd_exn

let%expect_test
    "a lost update reply permits status and original receipt reads, never another \
     admission"
  =
  Eio_main.run (fun _ ->
    let executions = ref 0 in
    let receipt = ref P.Command_receipt.Missing in
    let original = request () in
    let connection =
      C.Transport.create
        ~request:(function
          | P.Command.Protocol_initialize _ ->
            public (Protocol_initialize (initialization ()))
          | Prompt_dependencies_update actual ->
            incr executions;
            assert (D.Update_request.equal actual original);
            Error (error ())
          | Prompt_dependencies_status -> public (Prompt_dependencies_status status)
          | Command_receipt query ->
            assert (String.equal query.method_name "prompt.dependencies.update");
            assert (
              Jsonaf.exactly_equal
                query.original_params
                (D.Update_request.to_json original));
            public (Command_receipt !receipt)
          | _ -> assert false)
        ~next_notification:(fun () -> failwith "admin must not consume notifications")
        ~close:ignore
      |> C.Connection.create
    in
    initialize connection;
    assert (Result.is_error (C.Admin.update_dependencies connection original));
    assert (
      D.Status.equal (C.Admin.dependency_selection_status connection |> checked) status);
    let replacement =
      D.Update_request.create
        ~definitions:(D.Update_request.definitions original)
        ~expected_generation:(generation 7)
        ~idempotency_key:(P.Idempotency_key.of_string "replacement" |> checked)
      |> checked
    in
    assert (Result.is_error (C.Admin.update_dependencies connection replacement));
    let original_pending = pending connection in
    assert (Result.is_ok (C.Connection.reconcile connection original_pending));
    assert (Int.equal (List.length (C.Connection.pending_commands connection)) 1);
    receipt := Pending { accepted_sequence = None; expires_at = None };
    assert (Result.is_ok (C.Connection.reconcile connection original_pending));
    assert (Int.equal !executions 1);
    let wrong =
      D.Update_result.create
        ~previous_generation:(generation 6)
        ~generation:(generation 7)
        ~targets:(D.Update_result.targets (result ()))
      |> checked
    in
    receipt := Committed (Prompt_dependencies_updated wrong);
    assert (Result.is_error (C.Connection.reconcile connection original_pending));
    assert (Int.equal (List.length (C.Connection.pending_commands connection)) 1);
    receipt := Committed (Prompt_dependencies_updated (result ()));
    (match C.Connection.reconcile connection original_pending |> checked with
     | Committed (Prompt_dependencies_updated actual) ->
       assert (D.Update_result.equal actual (result ()))
     | _ -> assert false);
    assert (List.is_empty (C.Connection.pending_commands connection));
    C.Connection.close connection;
    print_endline
      "update admissions: 1; status: uncertain generation 8; missing/pending/mismatched \
       receipt: unresolved; exact committed receipt: reconciled");
  [%expect
    {| update admissions: 1; status: uncertain generation 8; missing/pending/mismatched receipt: unresolved; exact committed receipt: reconciled |}]
;;

let%test_unit
    "successful wrong-target replies and cancelled transports retain the original intent"
  =
  Eio_main.run (fun _ ->
    let admitted = ref 0 in
    let wrong =
      D.Update_result.create
        ~previous_generation:(generation 7)
        ~generation:(generation 8)
        ~targets:
          [ D.Update_result.Target.
              { definition_id = definition "prd_other"
              ; outcome = Ready (revision "prv_other")
              }
          ]
      |> checked
    in
    let connection =
      C.Transport.create
        ~request:(function
          | P.Command.Prompt_dependencies_update _ ->
            incr admitted;
            public (Prompt_dependencies_update wrong)
          | _ -> assert false)
        ~next_notification:(fun () -> None)
        ~close:ignore
      |> C.Connection.create
    in
    assert (Result.is_error (C.Admin.update_dependencies connection (request ())));
    assert (Int.equal (List.length (C.Connection.pending_commands connection)) 1);
    assert (Result.is_error (C.Admin.update_dependencies connection (request ())));
    assert (Int.equal !admitted 1);
    C.Connection.close connection;
    let entered, signal = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let cancellation = Failure "cancel original update" in
    let connection =
      C.Transport.create
        ~request:(fun _ ->
          Eio.Promise.resolve signal ();
          Eio.Promise.await never)
        ~next_notification:(fun () -> None)
        ~close:ignore
      |> C.Connection.create
    in
    Eio.Cancel.sub (fun context ->
      Eio.Fiber.both
        (fun () ->
           match C.Admin.update_dependencies connection (request ()) with
           | _ -> assert false
           | exception Eio.Cancel.Cancelled reason ->
             assert (phys_equal reason cancellation))
        (fun () ->
           Eio.Promise.await entered;
           Eio.Cancel.cancel context cancellation));
    assert (Int.equal (List.length (C.Connection.pending_commands connection)) 1);
    C.Connection.close connection)
;;
