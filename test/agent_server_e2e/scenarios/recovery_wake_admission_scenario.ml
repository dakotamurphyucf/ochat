open! Core
module A = Agent_session
module D = Agent_server.Daemon
module F = Crash_recovery_fixture
module H = Agent_client.Session_handle
module Notes = Recovery_note_provider_scenario
module P = Agent_protocol
module Provider = Support.Tui_stream_provider

let ok = F.protocol_ok
let require = F.require

type trial =
  | Oversized
  | Fitting
  | Checkpoint
[@@deriving equal]

let payload trial =
  match trial with
  | Fitting -> "Follow the completed wake."
  | (Oversized | Checkpoint) as trial ->
    (* Distinct printable strings exercise the real encoder without relying on
       a guessed byte-to-token ratio or asking the provider to exceed its limit. *)
    List.init (if equal_trial trial Oversized then 640 else 200) ~f:(fun i -> Md5.digest_string (Int.to_string i) |> Md5.to_hex)
    |> String.concat
;;

(* Publish on the first Turn_end, after notification consumption for that
   boundary. The following boundary commits this exact wake before the second
   Turn_end stages the independent moderator intent and the capacity payload. *)
let prompt trial =
  {|<config model="gpt-4.1" max_tokens="100"/>
<developer>Call watch once and then finish the task.</developer>
<script id="wake-admission" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = { invocation = ""; phase = 0 }
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("ready"))) in
  Task.pure({ invocation = p.context.invocation_id; phase = 0 })
| `Turn_end -> (match state.phase with
  | 0 -> (match state.invocation with
    | "" -> Task.pure(state)
    | invocation ->
      let reference = { key = "finished"; invocation_id = `Some(invocation); work = `None } in
      let* delivery = Notification.publish(reference, `Succeeded(`String("ordinary wake delivery")), `Request_turn) in
      Task.pure({ invocation = invocation; phase = 1 }))
  | 1 ->
      let* () = Turn.append_item(Item.output_text_message("wake-fixture", "|}
  ^ payload trial
  ^ {|")) in
      let* () = Runtime.request_turn() in
      Task.pure({ invocation = state.invocation; phase = 2 })
  | _ -> Task.pure(state))
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="wake-admission" input_schema="object.json" output_schema="string.json"/>
|}
;;

let delivery_and_intent (state : A.Session_state.t) =
  let delivery =
    match state.deliveries with
    | [ value ] -> value
    | _ -> F.fail "expected exactly one authored wake delivery"
  in
  let history_id =
    match delivery.status with
    | Committed { history_id; _ } -> history_id
    | _ -> F.fail "wake delivery had no committed canonical occurrence"
  in
  let frames =
    List.filter state.conversation.canonical_history ~f:(fun entry ->
      match entry.P.History.provenance with
      | Runtime_notification _ -> true
      | _ -> false)
  in
  require
    (match frames with
     | [ entry ] -> P.History.Id.equal history_id entry.id
     | _ -> false)
    "wake history contains a different or repeated notification occurrence";
  let intent =
    List.filter state.moderator_executions ~f:(fun event ->
      P.Moderator_execution.equal_phase event.context.phase Turn_end
      && Option.exists event.requests ~f:(fun requests -> requests.request_turn))
    |> function
    | [ event ] -> event
    | _ -> F.fail "expected exactly one authored follow-up intent"
  in
  delivery, intent
;;

let expect_pending state ~operation =
  let delivery, intent = delivery_and_intent state in
  require
    (Option.exists intent.context.operation_id ~f:(P.Id.Operation.equal operation)
     && Option.exists state.active_operation ~f:(fun active ->
       P.Id.Operation.equal active.id operation))
    "moderator follow-up belongs to a different foreground operation";
  require
    (Option.equal
       P.Delivery.equal_wake_disposition
       (Some Pending_wake)
       delivery.wake_disposition)
    "wake was acknowledged before the physical write";
  (match intent.intent with
   | Some Pending -> ()
   | disposition ->
     raise_s
       [%sexp
         "moderator follow-up left pending before the physical write"
       , (disposition : P.Moderator_execution.intent option)
       , (intent.context.operation_id : P.Id.Operation.t option)
       , (state.active_operation : P.Operation.t option)]);
  delivery, intent
;;

let same_intents
      (first : P.Delivery.t * P.Moderator_execution.t)
      (second : P.Delivery.t * P.Moderator_execution.t)
  =
  P.Id.Delivery.equal (fst first).context.id (fst second).context.id
  && P.Id.Moderator_execution.equal (snd first).context.id (snd second).context.id
;;

let require_accepted state ~operation expected =
  let delivery, intent = delivery_and_intent state in
  require
    (same_intents expected (delivery, intent))
    "provider write changed the selected wake identity";
  require
    (Option.equal
       P.Delivery.equal_wake_disposition
       (Some (Accepted_wake operation))
       delivery.wake_disposition)
    "provider request was written without the actual wake acknowledgement";
  require
    (Option.equal P.Moderator_execution.equal_intent (Some Applied) intent.intent)
    "provider request was written without applying its exact moderator follow-up"
;;

let latest_root (state : A.Session_state.t) ~operation =
  let module L = A.Inference_ledger in
  let roots =
    List.filter (L.rows state.inference_ledger) ~f:(fun row ->
      let handle = L.Row.handle row in
      Option.exists (L.Handle.operation_id handle) ~f:(P.Id.Operation.equal operation)
      &&
      match (L.Handle.scope handle).relation with
      | Root -> true
      | Nested _ -> false)
  in
  ( List.length roots
  , List.last roots |> Option.map ~f:(fun row -> L.Row.handle row |> L.Handle.ordinal) )
;;

let run_trial env environment trial =
  let fixtures = (Support.Temporary_environment.roots environment).temporary in
  List.iter
    [ "object.json", {|{"type":"object","properties":{}}|}
    ; "string.json", {|{"type":"string"}|}
    ]
    ~f:(fun (name, contents) -> F.write env (Filename.concat fixtures name) contents);
  let approached, signal_approach = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let prepared, signal_prepared = Eio.Promise.create () in
  let prepare_released, resume_preparation = Eio.Promise.create () in
  let check_next_preparation = ref false in
  let check_next_write = ref false in
  let auth_count = ref 0 in
  let preparations = ref 0 in
  let auth ~sw:_ _ =
    Int.incr auth_count;
    if !check_next_write
    then (
      check_next_write := false;
      Eio.Promise.resolve signal_approach ();
      Eio.Promise.await release);
    Openai.Responses_driver.Auth.bearer "recovery-wake-loopback"
  in
  let capacity = Recovery_successor_guard_scenario.config () in
  let capacity =
    match trial with
    | Fitting | Oversized -> capacity
    | Checkpoint ->
      let module C = Agent_server.Recovery_capacity_configuration in
      let targets = List.map capacity.targets ~f:(fun target ->
        { target with C.Target.input = Some 12_000L; session_total = Some 15_000L;
                      checkpoint_reserve = 9_000L; warning_lead = 32L }) in
      C.create ~mode:Automatic ~host_total:16_000L ~targets |> ok
  in
  Notes.with_fixture
    ~prompt_xml:
      (prompt trial
       ^ (match trial with
          | Oversized | Fitting -> ""
          | Checkpoint ->
            {|<tool name="context_checkpoint"/><tool name="recovery_note_write"/>|}))
    ~recovery_capacity:capacity
    ~on_preparation:(fun () ->
      Int.incr preparations;
      if !check_next_preparation
      then (
        check_next_preparation := false;
        Eio.Promise.resolve signal_prepared ();
        Eio.Promise.await prepare_released))
    env
    environment
    ~auth
    ~max_sessions:1
    (fun ~sw ~provider ~daemon ~proof ~client ->
       Exn.protect
         ~finally:(fun () ->
           ignore (Eio.Promise.try_resolve signal_release () : bool);
           ignore (Eio.Promise.try_resolve resume_preparation () : bool))
         ~f:(fun () ->
           let handle = Notes.open_session sw env client ~mode:Owner_read_write in
           let entry =
             Agent_server.Session_registry.find (D.registry daemon) (H.session_id handle)
             |> Option.value_exn
           in
           let module Actor = A.Session_actor in
           let state () = Actor.state entry.actor |> ok in
           let module Store_handle = Agent_store.Session_store.Handle in
           let durable_handle = Option.value_exn entry.store_handle in
           let replay () =
             Support.Background_fixture.checkpoint_from_paths
               env
               ~snapshot_directory:(Store_handle.snapshot_directory durable_handle)
               ~journal_directory:(Store_handle.journal_directory durable_handle)
             |> Option.value_exn
                  ~message:"canonical wake journal was unavailable for replay"
           in
           let attachment_id = (H.attachment handle).id in
           let module Factory = Agent_server.Session_factory in
           let identity =
             P.Session_ref.create
               ~server_id:(Agent_store.Session_store.server_id (D.store daemon))
               ~session_id:(H.session_id handle)
           in
           ignore
             (Actor.enable_recovery_windows
                entry.actor
                ~actor:proof
                ~attachment_id
                ~authorize:(fun _ ->
                  Factory.check_registered_owner
                    (D.factory daemon)
                    entry
                    ~session:identity)
              |> ok
              : P.Recovery_window.Scope.t);
           ignore (H.start handle ~queue_if_limited:false |> ok : P.Session.t);
           let submission =
             H.send_message
               handle
               { kind = Plain_text
               ; text = "Call the configured watch tool."
               ; attachments = []
               }
             |> ok
           in
           let failures_after sequence =
             match
               Agent_client.Connection.request
                 client
                 (Session_attach
                    { session_id = H.session_id handle
                    ; requested_mode = Read_only
                    ; subscribe = false
                    ; after_sequence = Some sequence
                    ; reclaim_token = None
                    ; idempotency_key = F.key "ordinary-wake-terminal-events"
                    })
               |> ok
             with
             | P.Public.Result.Session_attach { replay = Events events; _ } ->
               List.filter_map events ~f:(fun event ->
                 match Support.Public_view.shared_payload_opt event with
                 | Some (Operation_failed { id; state = Failed failure; _ })
                   when Option.exists submission.operation_id ~f:(P.Id.Operation.equal id)
                   -> Some failure
                 | _ -> None)
             | _ -> F.fail "missing canonical terminal replay"
           in
           let await_request index =
             try Provider.await_request provider env index with
             | Eio.Time.Timeout ->
               let latest = state () in
               raise_s
                 [%sexp
                   "provider request did not reach the loopback peer"
                 , (index : int)
                 , (submission : P.Method_result.Send_message.t)
                 , (!preparations : int)
                 , (!auth_count : int)
                 , (latest.lifecycle : A.Session_state.Lifecycle.t)
                 , (latest.runtime_initialization
                    : A.Session_state.Runtime_initialization.t)
                 , (latest.active_operation : P.Operation.t option)
                 , (latest.failure : P.Error.t option)
                 , (failures_after submission.mutation.latest_event_sequence
                    : P.Error.t list)]
           in
           let first = await_request 0 in
           Notes.function_call first ~id:"initial-watch" ~name:"watch" (`Object []);
           let second = await_request 1 in
           (match Notes.output second ~id:"initial-watch" with
            | Complete (`String "ready") -> ()
            | _ -> F.fail "authored notification source did not resolve its watched tool");
           let operation = Option.value_exn (state ()).active_operation in
           let count, prior = latest_root (state ()) ~operation:operation.id in
           require
             (Int.equal count 2)
             "fixture did not establish its original ordinary attempts";
           let prior = Option.value_exn prior in
           let preparation_before = !preparations in
           let authentication_before = !auth_count in
           check_next_preparation := equal_trial trial Oversized;
           check_next_write := true;
           Provider.reply_summary second "The original task finished.";
           let await_finished () =
             Support.Background_fixture.await
               env
               "original ordinary wake foreground terminal"
               (fun () ->
                  let state = state () in
                  Option.some_if
                    (not
                       (Option.exists state.active_operation ~f:(fun active ->
                          P.Id.Operation.equal active.id operation.id)))
                    state)
           in
           let await_barrier barrier description =
             Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
               Eio.Fiber.first
                 (fun () -> Eio.Promise.await barrier)
                 (fun () ->
                    let terminal = await_finished () in
                    let failures =
                      failures_after submission.mutation.latest_event_sequence
                    in
                    let moderator =
                      List.map terminal.moderator_executions ~f:(fun row ->
                        row.context.phase, row.status)
                    in
                    raise_s
                      [%sexp
                        (description : string)
                      , (failures : P.Error.t list)
                      , (moderator
                         : (P.Moderator_execution.phase * P.Moderator_execution.status)
                             list)]))
           in
           (match trial with
            | Oversized ->
              await_barrier prepared "oversized wake ended before preparation";
              let expected = expect_pending (state ()) ~operation:operation.id in
              Eio.Promise.resolve resume_preparation ();
              let terminal =
                Eio.Fiber.first await_finished (fun () ->
                  Eio.Promise.await approached;
                  F.fail "oversized ordinary wake entered provider authentication")
              in
              let delivery, intent = delivery_and_intent terminal in
              require
                (same_intents expected (delivery, intent))
                "rejected preparation changed its exact selected wake or follow-up";
              (match failures_after submission.mutation.latest_event_sequence with
               | [ failure ] when P.Error.equal_code failure.code Resource_limit -> ()
               | failures ->
                 raise_s
                   [%sexp
                     "oversized notification did not hit the real input limit"
                   , (failures : P.Error.t list)]);
              (match delivery.wake_disposition, intent.intent with
               | Some (Discarded_wake _), Some (Discarded _) -> ()
               | wake, follow_up ->
                 raise_s
                   [%sexp
                     "unsent wake or moderator follow-up did not retire unaccepted"
                   , (wake : P.Delivery.wake_disposition option)
                   , (follow_up : P.Moderator_execution.intent option)]);
              require
                (Int.equal !preparations (preparation_before + 1)
                 && Int.equal !auth_count authentication_before
                 && Int.equal (Provider.request_count provider) count
                 && Int.equal (fst (latest_root terminal ~operation:operation.id)) count)
                "denied wake admitted a provider attempt despite the prepare-time \
                 rejection";
              let restored = replay () in
              let saved_delivery, saved_intent = delivery_and_intent restored in
              require
                (P.Delivery.equal delivery saved_delivery
                 && P.Moderator_execution.equal intent saved_intent
                 && Int.equal (fst (latest_root restored ~operation:operation.id)) count)
                "canonical replay changed the denied wake disposition or attempt ledger"
            | Checkpoint ->
              let module Soft = Recovery_soft_checkpoint_scenario in
              await_barrier approached "checkpoint did not reach authentication";
              let initial_wake_state = state () in
              let delivery, pending_intent = expect_pending initial_wake_state ~operation:operation.id in
              Eio.Promise.resolve signal_release ();
              let selected = await_request count in
              require
                (Set.equal (Recovery_checkpoint_scenario.tools selected)
                   (String.Set.singleton "recovery_note_write"))
                "wake expected a checkpoint-only inference";
              let saved, _ = expect_pending (replay ()) ~operation:operation.id in
              require (P.Delivery.equal saved delivery)
                "checkpoint-only write changed the notification";
              (* Refusing the optional note must also leave the wake available
                 for the host's next ordinary request. *)
              Provider.reply_summary selected "No checkpoint note.";
              let fresh = await_request (count + 1) in
              let restored = replay () in
              let successor = Option.value_exn restored.active_operation in
              require
                (not (P.Id.Operation.equal operation.id successor.id)
                 && not (P.Recovery_window.Scope.equal
                    (Soft.active_scope restored) (Soft.active_scope initial_wake_state))
                 && String.is_substring (Provider.body fresh |> Jsonaf.to_string)
                      ~substring:"ordinary wake delivery")
                "fresh successor did not recover the original wake";
              let row = List.find_exn restored.deliveries ~f:(fun row ->
                P.Id.Delivery.equal row.context.id delivery.context.id) in
              require
                (Option.equal P.Delivery.equal_wake_disposition
                   row.wake_disposition (Some (Accepted_wake successor.id)))
                "fresh successor did not acknowledge the exact notification";
              let follow_up = List.find_exn restored.moderator_executions ~f:(fun row ->
                P.Id.Moderator_execution.equal row.context.id pending_intent.context.id) in
              (match follow_up.intent with
               | Some Applied -> ()
               | disposition -> raise_s
                   [%sexp "successor wake follow-up", (disposition : P.Moderator_execution.intent option)]);
              Provider.reply_summary fresh "The successor handled the wake.";
              let settled = Support.Background_fixture.await env "settled successor wake"
                (fun () ->
                   let value = state () in
                   Option.some_if (Option.is_none value.active_operation) value)
              in
              let saved = List.find_exn settled.deliveries ~f:(fun current ->
                P.Id.Delivery.equal current.context.id delivery.context.id) in
              require
                (P.Delivery.equal saved row && Provider.request_count provider = count + 2)
                "successor completion repeated the wake or changed its disposition"
            | Fitting ->
              await_barrier approached "fitting wake ended before provider authentication";
              let expected = expect_pending (state ()) ~operation:operation.id in
              require
                (Int.equal (Provider.request_count provider) count)
                "authentication barrier did not precede the physical write";
              Eio.Promise.resolve signal_release ();
              let wake_request = await_request count in
              let body = Provider.body wake_request |> Jsonaf.to_string in
              require
                (String.is_substring body ~substring:"ordinary wake delivery"
                 && String.is_substring body ~substring:(payload trial))
                "the physical request lacked the selected wake and moderator edit";
              let physically_submitted = state () in
              require_accepted physically_submitted ~operation:operation.id expected;
              let restored = replay () in
              require_accepted restored ~operation:operation.id expected;
              require
                (Int.equal
                   (fst (latest_root restored ~operation:operation.id))
                   (count + 1))
                "provider received the wake before its exact durable attempt existed";
              let roots, current =
                latest_root physically_submitted ~operation:operation.id
              in
              require
                (Int.equal roots (count + 1)
                 && Option.exists current ~f:(fun value ->
                   Int64.equal value (Int64.succ prior)))
                "fitting wake did not bind exactly one new physical root";
              Provider.reply_summary wake_request "The wake was handled.";
              let terminal = await_finished () in
              require_accepted terminal ~operation:operation.id expected;
              require
                (List.is_empty (failures_after submission.mutation.latest_event_sequence)
                 && Int.equal (Provider.request_count provider) (count + 1)
                 && Int.equal !auth_count (authentication_before + 1)
                 && Int.equal
                      (fst (latest_root terminal ~operation:operation.id))
                      (count + 1))
                "completed wake re-admitted the original occurrence");
           H.close handle))
;;

let denied env environment = run_trial env environment Oversized
let accepted env environment = run_trial env environment Fitting

let checkpoint env environment = run_trial env environment Checkpoint
