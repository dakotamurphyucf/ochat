open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module I = Agent_session.External_ingress
module Setup = Subscription_transaction_tests

type mode =
  | Commit
  | Revoke
  | Cancelled
  | Rejected_save
  | Unselected_subscription
  | Expired
  | Elapsed_expired
  | Bytes
  | Missing_principal
[@@deriving sexp_of]

let with_handler actor parent f =
  A.with_job_execution
    actor
    ~job_id:parent.P.Job.id
    ~generation:0
    ~attempt:parent.attempt
    ~deadline:(Some deadline)
    (fun services ->
       services.execute ~invocation:(root parent) (fun ~dispatched:root ->
         let invocation =
           P.Invocation.create
             { root.context with
               id = P.Id.Invocation.create ()
             ; parent_job = None
             ; parent_invocation = Some root.context.id
             ; tool_name = "register_helper"
             }
           |> protocol_ok
         in
         services.moderator_execute ~invocation (fun ~dispatched ~commit ->
           f ~owner:(P.Job.Invocation dispatched.context.id) ~dispatched ~commit)
         |> Result.map ~f:(fun () -> P.Invocation.Complete `Null)))
;;

let%expect_test
    "ingress registration commits with its subscription epoch, rolls back reservations \
     and inherits the actual host producer"
  =
  List.iter
    [ Commit
    ; Revoke
    ; Cancelled
    ; Rejected_save
    ; Unselected_subscription
    ; Expired
    ; Elapsed_expired
    ; Bytes
    ; Missing_principal
    ]
    ~f:(fun mode ->
      let now = ref timestamp in
      let monotonic = ref Mtime.min_stamp in
      let reject = ref false in
      let producer = P.Id.Principal.of_string "pri_ingress_owner" |> protocol_ok in
      let owner_ref = ref None in
      let saved_id = ref None in
      let limits =
        { Agent_session.Staged_ingress.default_limits with
          max_active = 1
        ; max_retained = 2
        ; max_retained_bytes =
            (match mode with
             | Bytes -> 8_192
             | _ -> 32_768)
        }
      in
      with_actor
        ~now:(fun () -> !now)
        ~monotonic_now:(fun () -> !monotonic)
        ~ingress_limits:limits
        ~prepare_state:(fun state ->
          { state with
            identity =
              { state.identity with
                creating_principal =
                  (match mode with
                   | Missing_principal -> None
                   | _ -> Some producer)
              }
          })
        ~reject_save:(fun next ->
          !reject
          && not
               (List.is_empty
                  next.Agent_session.Session_transition.state.ingress_registrations))
        (fun _ _ actor _ backend ->
           A.change_moderator actor (Some (Setup.encode Setup.before))
           |> protocol_ok
           |> ignore;
           let parent = add_claimed_job actor in
           let result =
             with_handler actor parent (fun ~owner ~dispatched ~commit ->
               owner_ref := Some owner;
               let creation, subscription =
                 A.create_script_subscription
                   actor
                   ~owner
                   ~source:Setup.source
                   ~kind:"helper"
                   ~lifetime_ms:1_000
                   ~wake:No_wake
                   ~completion_schema:(Some `True)
                 |> protocol_ok
               in
               let register source =
                 A.create_script_ingress
                   actor
                   ~owner
                   ~source
                   ~subscription_id:subscription.context.id
                   ~expected_epoch:0
                   ~namespace:"external.report"
                   ~schema:`True
               in
               (match
                  register { Setup.source with source_sha256 = String.make 64 'b' }
                with
                | Error { code = Conflict; _ } -> ()
                | _ -> failwith "foreign moderator registered ingress");
               match mode, register Setup.source with
               | Missing_principal, Error { code = Permission_denied; _ } ->
                 Error (handoff_error "registration has no trusted producer")
               | Missing_principal, _ ->
                 failwith "missing principal acquired ingress authority"
               | Bytes, Error { code = Resource_limit; _ } ->
                 Error (handoff_error "byte quota rejected creation")
               | Bytes, _ -> failwith "byte quota allowed an oversized reservation"
               | _, first ->
                 let abandoned, first = first |> protocol_ok in
                 assert (P.Id.Principal.equal producer first.context.producer);
                 assert (
                   List.is_empty (A.state actor |> protocol_ok).ingress_registrations);
                 (match register Setup.source with
                  | Error { code = Resource_limit; _ } -> ()
                  | _ -> failwith "staged registration did not reserve shared capacity");
                 A.abort_ingress_mutation actor ~owner ~receipt:abandoned |> protocol_ok;
                 assert (
                   Result.is_error
                     (A.read_script_ingress
                        actor
                        ~owner
                        ~source:Setup.source
                        ~id:first.context.id));
                 let receipt, registration = register Setup.source |> protocol_ok in
                 saved_id := Some registration.context.id;
                 let subs, registrations =
                   match mode with
                   | Revoke ->
                     let finished, _ =
                       A.finish_script_subscription
                         actor
                         ~owner
                         ~source:Setup.source
                         ~id:subscription.context.id
                         ~expected_epoch:0
                         (Succeeded (`String "done"))
                       |> protocol_ok
                     in
                     let revoked, value =
                       A.revoke_script_ingress
                         actor
                         ~owner
                         ~source:Setup.source
                         ~id:registration.context.id
                         ~reason:"helper no longer needed"
                       |> protocol_ok
                     in
                     assert (Option.is_some value.revoked);
                     [ creation; finished ], [ receipt; revoked ]
                   | Unselected_subscription -> [], [ receipt ]
                   | _ -> [ creation ], [ receipt ]
                 in
                 A.select_subscription_mutations
                   actor
                   ~owner
                   ~source:Setup.source
                   ~receipts:subs
                 |> protocol_ok;
                 (match
                    A.select_ingress_mutations
                      actor
                      ~owner
                      ~source:Setup.source
                      ~receipts:(registrations @ registrations)
                  with
                  | Error { code = Conflict; _ } -> ()
                  | _ -> failwith "duplicate ingress receipt selection was accepted");
                 A.select_ingress_mutations
                   actor
                   ~owner
                   ~source:Setup.source
                   ~receipts:registrations
                 |> protocol_ok;
                 (match mode with
                  | Rejected_save -> reject := true
                  | Expired -> now := P.Timestamp.add_ms timestamp 1_000 |> protocol_ok
                  | Elapsed_expired -> monotonic := Mtime.of_uint64_ns 1_000_000_000L
                  | _ -> ());
                 let outcome =
                   match mode with
                   | Cancelled -> P.Invocation.Cancelled "cancelled"
                   | _ ->
                     Pending (Subscription subscription.context.id, `String "accepted")
                 in
                 let resolved =
                   P.Invocation.resolve dispatched ~session_id ~generation:0 outcome
                   |> protocol_ok
                 in
                 let result = commit ~resolved ~snapshot:Setup.after in
                 reject := false;
                 result)
           in
           reject := false;
           (match mode, result with
            | (Commit | Revoke | Cancelled), Ok _ -> ()
            | ( ( Rejected_save
                | Unselected_subscription
                | Expired
                | Elapsed_expired
                | Bytes
                | Missing_principal )
              , Error _ ) -> ()
            | _ -> failwith "unexpected ingress transaction outcome");
           let state = A.state actor |> protocol_ok in
           let count =
             match mode with
             | Commit | Revoke -> 1
             | _ -> 0
           in
           [%test_eq: int] count (List.length state.ingress_registrations);
           [%test_eq: int] count (List.length state.subscriptions);
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           (match state.ingress_registrations with
            | [ registration ] ->
              assert (
                P.Id.Capability.equal registration.context.id (Option.value_exn !saved_id));
              let sub = List.hd_exn state.subscriptions in
              (match mode, registration.revoked, sub.result with
               | Commit, None, None -> ()
               | Revoke, Some _, Some (Succeeded _) ->
                 assert (registration.context.epoch < sub.epoch)
               | _ -> failwith "registration epoch ordering was lost")
            | [] -> ()
            | _ -> assert false);
           assert (
             Option.is_some
               (A.with_quiescent_state actor ~f:(fun _ -> Ok ()) |> protocol_ok));
           (match !saved_id with
            | None -> ()
            | Some id ->
              assert (
                Result.is_error
                  (A.read_script_ingress
                     actor
                     ~owner:(Option.value_exn !owner_ref)
                     ~source:Setup.source
                     ~id)));
           print_s [%sexp (mode : mode), (count : int)]));
  [%expect
    {|
    (Commit 1)
    (Revoke 1)
    (Cancelled 0)
    (Rejected_save 0)
    (Unselected_subscription 0)
    (Expired 0)
    (Elapsed_expired 0)
    (Bytes 0)
    (Missing_principal 0)
    |}]
;;

let%expect_test
    "lower host quotas block registration without blocking revocation of retained ingress"
  =
  let original = ref None in
  with_actor
    ~ingress_limits:
      { Agent_session.Staged_ingress.default_limits with
        max_active = 0
      ; max_retained = 0
      ; max_retained_bytes = 0
      }
    ~prepare_state:(fun state ->
      let subscription, registration = External_ingress_tests.fixture () in
      let accepted, _ =
        I.prepare
          registration
          ~session_id
          ~generation:0
          ~source:registration.context.source
          ~producer:registration.context.producer
          ~namespace:registration.context.namespace
          ~subscription
          ~key:(P.Idempotency_key.of_string "already-accepted" |> protocol_ok)
          ~payload:(External_ingress_tests.report "retain")
          ~now:timestamp
          ~create_event_id:P.Id.Ingress_event.create
        |> protocol_ok
        |> External_ingress_tests.accepted
      in
      original := Some accepted;
      let state =
        { state with
          identity =
            { state.identity with creating_principal = Some accepted.context.producer }
        }
      in
      Agent_session.Session_transition.apply
        ~now:timestamp
        state
        ~payloads:[]
        ~delta:
          (Batch
             [ Invocation_changed (invocation_fixture ())
             ; Subscription_changed subscription
             ; Ingress_changed registration
             ; Ingress_changed accepted
             ])
      |> protocol_ok
      |> fun transition -> transition.Agent_session.Session_transition.state)
    (fun _ _ actor _ backend ->
       A.change_moderator actor (Some (Setup.encode Setup.before))
       |> protocol_ok
       |> ignore;
       let parent = add_claimed_job actor in
       let previous = Option.value_exn !original in
       with_handler actor parent (fun ~owner ~dispatched ~commit ->
         (match
            A.create_script_ingress
              actor
              ~owner
              ~source:Setup.source
              ~subscription_id:previous.context.subscription_id
              ~expected_epoch:0
              ~namespace:"external.new"
              ~schema:`True
          with
          | Error { code = Resource_limit; _ } -> ()
          | _ -> failwith "lower quotas allowed another registration");
         let receipt, _ =
           A.revoke_script_ingress
             actor
             ~owner
             ~source:Setup.source
             ~id:previous.context.id
             ~reason:"disabled"
           |> protocol_ok
         in
         A.select_ingress_mutations
           actor
           ~owner
           ~source:Setup.source
           ~receipts:[ receipt ]
         |> protocol_ok;
         let resolved =
           P.Invocation.resolve dispatched ~session_id ~generation:0 (Complete `Null)
           |> protocol_ok
         in
         commit ~resolved ~snapshot:Setup.after)
       |> protocol_ok
       |> ignore;
       let state = A.state actor |> protocol_ok in
       let retained = List.hd_exn state.ingress_registrations in
       assert (Option.equal String.equal retained.revoked (Some "disabled"));
       assert (List.equal I.equal_receipt previous.receipts retained.receipts);
       assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
       print_endline
         "admission blocked; revocation committed; prior event receipt retained");
  [%expect {| admission blocked; revocation committed; prior event receipt retained |}]
;;
