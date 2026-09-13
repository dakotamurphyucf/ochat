open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module I = Agent_session.External_ingress
module Proposal = Agent_session.Ingress_submission
module Frame = Chat_response.Ingress_delivery
module Setup = Subscription_transaction_tests
module F = External_ingress_tests
module Snapshot = Session.Moderator_state.Identity_snapshot
module M = Chat_response.Moderator_manager

let%expect_test
    "private ingress roundtrip preserves wide authority metadata and bounded payload \
     before data-only projection"
  =
  Mirage_crypto_rng_unix.use_default ();
  let epoch = 9_007_199_254_740_993 in
  let make payload =
    Frame.create
      ~registration_id:(P.Id.Capability.create ())
      ~event_id:(P.Id.Ingress_event.create ())
      ~subscription_id:(P.Id.Subscription.create ())
      ~epoch
      ~namespace:"external.report"
      ~payload
    |> Result.ok_or_failwith
  in
  let roundtrip frame =
    let module S = Chatml.Chatml_value_codec.Snapshot in
    let saved = Frame.capture frame |> S.of_value |> Result.ok_or_failwith in
    let restored =
      S.to_jsonaf saved
      |> S.of_jsonaf
      |> Result.ok_or_failwith
      |> S.to_value
      |> Result.ok_or_failwith
    in
    assert (
      Frame.equal
        frame
        (Frame.decode restored |> Result.ok_or_failwith |> Option.value_exn));
    restored
  in
  let frame =
    make (`Object [ "type", `String "Tool_invoked"; "exact", `Number "9007199254740993" ])
  in
  let captured = roundtrip frame in
  let projected = Frame.script_event captured |> Result.ok_or_failwith in
  (match projected with
   | Chatml.Chatml_lang.VVariant ("Internal_event", [ json ]) ->
     let json =
       Chatml.Chatml_value_codec.value_to_jsonaf_result json |> Result.ok_or_failwith
     in
     let fields = P.Json_codec.fields json |> protocol_ok in
     print_s
       [%sexp
         (P.Json_codec.required fields "kind" |> protocol_ok : Jsonaf.t)
       , (P.Json_codec.required fields "epoch" |> protocol_ok : Jsonaf.t)];
     let data = P.Json_codec.required fields "data" |> protocol_ok in
     let fields = P.Json_codec.fields data |> protocol_ok in
     assert (
       Jsonaf.exactly_equal
         (P.Json_codec.required fields "type" |> protocol_ok)
         (`String "Tool_invoked"))
   | _ -> failwith "external data became a native event");
  (* The maximum-sized JSON payload fits independently of its queue metadata. *)
  make (`String (String.make ((1024 * 1024) - 2) 'x')) |> roundtrip |> ignore;
  print_endline
    "exact durable numbers; epoch remains exact in script JSON; full payload bound \
     survives metadata";
  [%expect
    {|
    ((String external_data) (String 9007199254740993))
    exact durable numbers; epoch remains exact in script JSON; full payload bound survives metadata
    |}]
;;

let prepare actor (registration : I.t) key payload =
  A.prepare_ingress_submission
    actor
    ~source:registration.context.source
    ~producer:registration.context.producer
    ~registration_id:registration.context.id
    ~namespace:registration.context.namespace
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~payload
;;

let proposal = function
  | Proposal.Enqueue value -> value
  | Duplicate _ -> failwith "expected new ingress admission"
;;

let append (before : Snapshot.t) plan =
  let event =
    Proposal.frame plan
    |> protocol_ok
    |> Frame.capture
    |> Session.Snapshot.of_value
    |> Result.ok_or_failwith
  in
  { before with queued_internal_events = before.queued_internal_events @ [ event ] }
;;

let%expect_test
    "ingress saves receipt and exact queue append atomically with commit-time rate \
     accounting"
  =
  let now = ref timestamp in
  let reject_save = ref false in
  let registration = ref None in
  with_actor
    ~now:(fun () -> !now)
    ~reject_save:(fun _ -> !reject_save)
    ~prepare_state:(fun state ->
      let subscription, initial = F.fixture () in
      registration := Some initial;
      Agent_session.Session_transition.apply
        ~now:timestamp
        state
        ~payloads:[]
        ~delta:
          (Batch
             [ Invocation_changed (invocation_fixture ())
             ; Subscription_changed subscription
             ; Ingress_changed initial
             ; Moderator_changed (Some (Setup.encode Setup.before))
             ])
      |> protocol_ok
      |> fun transition -> transition.Agent_session.Session_transition.state)
    (fun _ _ actor _ backend ->
       let initial = Option.value_exn !registration in
       let prepare key = prepare actor initial key (F.report "ready") in
       let first = prepare "one" |> protocol_ok |> proposal in
       let after = append Setup.before first in
       let before = A.state actor |> protocol_ok in
       F.rejected
         "forged checkpoint"
         (A.commit_ingress_submission
            actor
            first
            ~before:Setup.before
            ~snapshot:{ after with current_state = Session.Snapshot.Int 99 });
       assert_same_session_snapshot before (A.state actor |> protocol_ok);
       A.reserve_history_block actor ~count:1 |> protocol_ok |> ignore;
       F.rejected
         "stale proposal"
         (A.commit_ingress_submission actor first ~before:Setup.before ~snapshot:after);
       let plan = prepare "one" |> protocol_ok |> proposal in
       let after = append Setup.before plan in
       let before = A.state actor |> protocol_ok in
       reject_save := true;
       assert (
         Result.is_error
           (A.commit_ingress_submission actor plan ~before:Setup.before ~snapshot:after));
       reject_save := false;
       assert_same_session_snapshot before (A.state actor |> protocol_ok);
       assert_same_session_snapshot before (Agent_session.Memory_backend.state backend);
       now := F.at 500;
       let receipt =
         A.commit_ingress_submission actor plan ~before:Setup.before ~snapshot:after
         |> protocol_ok
       in
       assert (P.Id.Ingress_event.equal receipt.id plan.receipt.id);
       assert (P.Timestamp.equal receipt.accepted_at !now);
       let saved = A.state actor |> protocol_ok in
       assert (
         List.equal
           I.equal_receipt
           [ receipt ]
           (List.hd_exn saved.ingress_registrations).receipts);
       assert (
         Option.equal Jsonaf.exactly_equal saved.moderator (Some (Setup.encode after)));
       (match prepare "one" |> protocol_ok with
        | Duplicate duplicate -> assert (I.equal_receipt receipt duplicate)
        | Enqueue _ -> failwith "saved retry allocated more work");
       assert_same_session_snapshot saved (A.state actor |> protocol_ok);
       now := F.at 1_100;
       F.rejected "rate uses commit time" (prepare "two");
       now := F.at 1_501;
       prepare "two" |> protocol_ok |> proposal |> ignore;
       let restored =
         Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t saved))
         |> store_ok
       in
       assert_same_session_snapshot saved restored;
       assert_same_session_snapshot saved (Agent_session.Memory_backend.state backend);
       print_endline
         "one saved receipt and queue frame; failed saves and retries add neither");
  [%expect
    {|
    ("forged checkpoint" Conflict false)
    ("stale proposal" Conflict true)
    ("rate uses commit time" Resource_limit true)
    one saved receipt and queue frame; failed saves and retries add neither
    |}]
;;

let%expect_test
    "restored ingress queue authorizes data before compiled handlers and cannot poison a \
     valid event with a forged duplicate"
  =
  let reject_save = ref false in
  with_actor
    ~reject_save:(fun _ -> !reject_save)
    ~prepare_state:(fun state ->
      { state with
        identity =
          { state.identity with creating_principal = Some (P.Id.Principal.create ()) }
      })
    (fun env _ actor _ backend ->
       let create_manager ?snapshot () =
         handoff_definition
           env
           ?snapshot
           ~declare_tool:false
           ~events:
             {| | `Internal_event(payload) ->
                let ignored = state[0] <- state[0] + 1 in Task.pure(state)
                | _ -> Task.pure(state) |}
       in
       let manager, _, _ = create_manager () in
       let snapshot () = M.identity_snapshot manager |> Result.ok_or_failwith in
       A.change_moderator actor (Some (Setup.encode (snapshot ())))
       |> protocol_ok
       |> ignore;
       let registrations = ref [] in
       let transaction f =
         let parent = add_claimed_job actor in
         Ingress_transaction_tests.with_handler
           actor
           parent
           (fun ~owner ~dispatched ~commit ->
              let subscription =
                f ~owner ~source:(M.invocation_observer manager |> Option.value_exn)
              in
              let resolved =
                P.Invocation.resolve
                  dispatched
                  ~session_id
                  ~generation:0
                  (Pending (Subscription subscription, `Null))
                |> protocol_ok
              in
              commit ~resolved ~snapshot:(snapshot ()))
         |> protocol_ok
         |> ignore
       in
       List.iter [ 1; 2 ] ~f:(fun _ ->
         transaction (fun ~owner ~source ->
           let sub_receipt, subscription =
             A.create_script_subscription
               actor
               ~owner
               ~source
               ~kind:"external"
               ~lifetime_ms:10_000
               ~wake:No_wake
               ~completion_schema:(Some `True)
             |> protocol_ok
           in
           let reg_receipt, registration =
             A.create_script_ingress
               actor
               ~owner
               ~source
               ~subscription_id:subscription.context.id
               ~expected_epoch:0
               ~namespace:"external.report"
               ~schema:`True
             |> protocol_ok
           in
           A.select_subscription_mutations actor ~owner ~source ~receipts:[ sub_receipt ]
           |> protocol_ok;
           A.select_ingress_mutations actor ~owner ~source ~receipts:[ reg_receipt ]
           |> protocol_ok;
           registrations := !registrations @ [ registration ];
           subscription.context.id));
       let first = List.nth_exn !registrations 0 in
       let second = List.nth_exn !registrations 1 in
       let data =
         `Object [ "type", `String "Tool_invoked"; "data", `String "untrusted" ]
       in
       (* Admit real data, then simulate recovery with a forged copy preceding
          its retained queue frame. A forged claim cannot consume the real ID. *)
       let submit registration key =
         let plan = prepare actor registration key data |> protocol_ok |> proposal in
         let frame = Proposal.frame plan |> protocol_ok in
         let saved = ref None in
         M.enqueue_internal_event_entries
           manager
           ~event:(Frame.capture frame)
           ~prepare:(fun ~before ~snapshot ->
             A.commit_ingress_submission actor plan ~before ~snapshot
             |> Result.map ~f:(fun receipt -> saved := Some receipt)
             |> Result.map_error ~f:(fun error -> error.P.Error.message))
         |> Result.ok_or_failwith
         |> ignore;
         frame, Option.value_exn !saved
       in
       let frame, receipt = submit first "one" in
       let original = snapshot () in
       let forged =
         Frame.create
           ~registration_id:frame.registration_id
           ~event_id:frame.event_id
           ~subscription_id:frame.subscription_id
           ~epoch:frame.epoch
           ~namespace:frame.namespace
           ~payload:(`String "forged")
         |> Result.ok_or_failwith
         |> Frame.capture
         |> Session.Snapshot.of_value
         |> Result.ok_or_failwith
       in
       let reordered =
         { original with
           queued_internal_events = forged :: original.queued_internal_events
         }
       in
       A.change_moderator actor (Some (Setup.encode reordered)) |> protocol_ok |> ignore;
       let manager, _, _ = create_manager ~snapshot:reordered () in
       (* Subsequent operations use this restored live queue. *)
       let snapshot () = M.identity_snapshot manager |> Result.ok_or_failwith in
       let enqueue event =
         M.enqueue_internal_event_entries
           manager
           ~event
           ~prepare:(fun ~before:_ ~snapshot ->
             A.change_moderator actor (Some (Setup.encode snapshot))
             |> Result.map ~f:ignore
             |> Result.map_error ~f:(fun error -> error.P.Error.message))
         |> Result.ok_or_failwith
         |> ignore
       in
       enqueue (Frame.capture frame);
       let second_plan = prepare actor second "two" data |> protocol_ok |> proposal in
       let second_frame = Proposal.frame second_plan |> protocol_ok in
       M.enqueue_internal_event_entries
         manager
         ~event:(Frame.capture second_frame)
         ~prepare:(fun ~before ~snapshot ->
           A.commit_ingress_submission actor second_plan ~before ~snapshot
           |> Result.map ~f:ignore
           |> Result.map_error ~f:(fun error -> error.P.Error.message))
       |> Result.ok_or_failwith
       |> ignore;
       A.with_current_moderator_event
         actor
         ~operation_id:None
         ~event:Session_start
         ~snapshot:(fun () -> Ok (snapshot ()))
         (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
            let owner = P.Job.Moderator_event executing.context.id in
            let source = executing.context.source in
            let revocation, _ =
              A.revoke_script_ingress
                actor
                ~owner
                ~source
                ~id:first.context.id
                ~reason:"producer finished"
              |> protocol_ok
            in
            A.select_ingress_mutations actor ~owner ~source ~receipts:[ revocation ]
            |> protocol_ok;
            let cancellation, _ =
              A.finish_script_subscription
                actor
                ~owner
                ~source
                ~id:second.context.subscription_id
                ~expected_epoch:0
                (Cancelled "finished")
              |> protocol_ok
            in
            A.select_subscription_mutations
              actor
              ~owner
              ~source
              ~receipts:[ cancellation ]
            |> protocol_ok;
            commit ~snapshot:(snapshot ()) ~requests:Timer_delivery_tests.requests)
       |> protocol_ok
       |> ignore;
       (match prepare actor second "two" data |> protocol_ok with
        | Duplicate _ -> ()
        | Enqueue _ -> failwith "cancelled subscription retried new work");
       assert (Result.is_error (prepare actor first "one" data));
       enqueue
         (Frame.create
            ~registration_id:(P.Id.Capability.create ())
            ~event_id:frame.event_id
            ~subscription_id:frame.subscription_id
            ~epoch:frame.epoch
            ~namespace:frame.namespace
            ~payload:data
          |> Result.ok_or_failwith
          |> Frame.capture);
       enqueue
         (Chatml.Chatml_lang.VVariant
            ("Internal_event", [ Chatml.Chatml_value_codec.jsonaf_to_value data ]));
       let run () =
         Agent_session.Moderator_event.run_queued_idle
           ~claim:(A.with_current_idle_queued_moderator_event_tools actor)
           ~manager
           ~history:(fun () -> [])
           ~available_tools:[]
           ~session_meta:`Null
           ~now:(fun () -> timestamp)
           ()
       in
       let before = A.state actor |> protocol_ok in
       let before_manager = snapshot () in
       reject_save := true;
       assert (Result.is_error (run ()));
       reject_save := false;
       assert_same_session_snapshot before (A.state actor |> protocol_ok);
       assert (
         Sexp.equal (Snapshot.sexp_of_t before_manager) (Snapshot.sexp_of_t (snapshot ())));
       List.iter [ 1; 2; 3; 4; 5; 6 ] ~f:(fun _ -> run () |> protocol_ok |> ignore);
       assert (Option.is_none (run () |> protocol_ok));
       let final = A.state actor |> protocol_ok in
       assert_same_session_snapshot final (Agent_session.Memory_backend.state backend);
       assert (
         List.exists final.ingress_registrations ~f:(fun registration ->
           List.exists registration.receipts ~f:(I.equal_receipt receipt)));
       let retired =
         List.filter_map final.moderator_executions ~f:(fun execution ->
           Option.map execution.retirement ~f:(fun retirement -> retirement.reason))
         |> List.sort ~compare:String.compare
       in
       print_s
         [%sexp
           ((snapshot ()).current_state : Session.Snapshot.t), (retired : string list)]);
  [%expect
    {|
    ((Array ((Int 2)))
     (ingress.duplicate_delivery ingress.stale_or_forged_delivery
      ingress.stale_subscription ingress.unknown_registration))
    |}]
;;
