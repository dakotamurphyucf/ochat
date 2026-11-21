open! Core
module A = Agent_session
module P = Agent_protocol
module H = Agent_client.Session_handle
module Catalog = Recovery_notes.Catalog
module Ledger = A.Recovery_rollover
module Opportunity = A.Recovery_checkpoint_opportunity
module Provider = Support.Tui_stream_provider
module Notes = Recovery_note_provider_scenario
module Reminder = Recovery_reminder_scenario
module Checkpoint = Recovery_checkpoint_scenario
module Fixture = Crash_recovery_fixture
module Scope = P.Recovery_window.Scope

let ok = Fixture.protocol_ok
let require = Fixture.require
let required = "Preserve this authored foreground task."
let private_body = "The note records a private and exact completed decision. β"
let guidance = "This single request can preserve essential work"
let auth ~sw:_ _ = Openai.Responses_driver.Auth.bearer "local-soft-checkpoint"

let prompt =
  {|<config model="gpt-4.1" max_tokens="100"/>
<developer>Preserve required checkpoint instructions.</developer>
<tool name="context_checkpoint"/><tool name="recovery_note_write"/>
<tool name="context_remaining"/>|}
;;

let set_policy actor =
  let module C = A.Session_context_capacity in
  let module R = Inference_runtime.Context_capacity in
  let state = A.Session_actor.state actor |> ok in
  let target = A.Configuration_transition.target state |> ok in
  let policy =
    R.Policy.create
      ~target
      ~encoder:Openai_responses_v1
      ~model_limit:(Bounded { total = 16_000L; input = Some 12_000L; output = Some 500L })
      ~allowance:
        { host_total = None
        ; session_total = None
        ; omitted_output_default = None
        ; checkpoint_reserve = 11_936L
        ; warning_lead = 32L
        }
      ~estimate:
        (Some { estimator = Openai.Inference_adapter.capacity_estimator; margin = 0L })
    |> Result.ok
    |> Option.value_exn
  in
  let host =
    C.Host.create
      ~mode_for_session:(fun _ -> Some Automatic)
      ~targets:[ policy, `Automatic ]
    |> Result.ok
    |> Option.value_exn
  in
  A.Session_actor.set_context_capacity_host actor host |> ok
;;

let send handle =
  H.send_message handle { kind = Plain_text; text = required; attachments = [] } |> ok
;;

let opportunity (state : A.Session_state.t) =
  Option.bind state.recovery_rollover ~f:Ledger.checkpoint_opportunity
;;

let check_candidate actor request =
  let names = Checkpoint.tools request in
  require
    (Set.equal names (String.Set.singleton "recovery_note_write"))
    "checkpoint request advertised an ordinary or unselected tool";
  require
    (Reminder.has_entry
       request
       ~role:"developer"
       ~substring:"Preserve required checkpoint instructions.")
    "checkpoint request lost authored guidance";
  require
    (Reminder.has_entry request ~role:"user" ~substring:required)
    "checkpoint request lost required input";
  require
    (Reminder.has_entry request ~role:"user" ~substring:guidance)
    "checkpoint request lost bounded guidance";
  let state = A.Session_actor.state actor |> ok in
  let opportunity = opportunity state |> Option.value_exn in
  let claim = Opportunity.claim opportunity in
  require
    (Option.is_some (Opportunity.attempt opportunity))
    "physical request crossed without the accepted offer";
  require
    (Option.is_none (Opportunity.outcome opportunity))
    "checkpoint outcome predated provider response";
  match claim.decision with
  | Offer (Create_note { catalog; path }) ->
    ( claim
    , `Object
        [ "kind", `String "create"
        ; "path", `String (Catalog.Note_path.to_string path)
        ; ( "expected_catalog_revision"
          , `String (Int64.to_string (Catalog.Catalog_revision.to_int64 catalog)) )
        ; "expected", `Null
        ; "content", `String private_body
        ] )
  | Offer Register | Withhold _ ->
    Fixture.fail "first checkpoint did not select a note creation"
;;

let partial_call request ~id ~name ~arguments =
  let module E = Openai.Responses.Response_stream in
  let item arguments status =
    E.Item.Function_call
      { name; arguments; call_id = id; _type = "function_call"; id = Some id; status }
  in
  Provider.emit
    request
    [ Provider.added (item "" None)
    ; E.Function_call_arguments_done
        { arguments
        ; item_id = id
        ; output_index = 0
        ; type_ = "response.function_call_arguments.done"
        }
    ; Provider.done_ (item arguments (Some "completed"))
    ]
;;

let safe_catalog state =
  Option.value_map state.A.Session_state.recovery_notes ~default:[] ~f:Catalog.list
;;

let invocation_result (invocation : P.Invocation.t) =
  match invocation.status with
  | Admitted -> "admitted"
  | Dispatching -> "dispatching"
  | Resolved (Fail error) | Published (Fail error) -> error.code
  | Resolved _ -> "resolved"
  | Published _ -> "published"
;;

let active_scope (state : A.Session_state.t) =
  Option.value_exn state.recovery_windows
  |> A.Recovery_window_ledger.active
  |> A.Recovery_window_record.scope
;;

let committed_checkpoint (state : A.Session_state.t) ~predecessor =
  let ledger = Option.value_exn state.recovery_rollover in
  let successor = active_scope state in
  List.find_map_exn state.conversation.compaction_archives ~f:(fun archive ->
    if A.Session_state.Compaction_archive.equal_kind archive.kind Rollover
    then (
      match Ledger.find ledger ~id:archive.operation_id with
      | Some { intent; status = Terminal (Committed committed); _ }
        when Scope.equal intent.expected.scope predecessor
             && Scope.equal committed.successor successor -> Some committed.checkpoint
      | Some _ | None -> None)
    else None)
;;

let verify_result state ~(claim : Opportunity.Claim.t) ~create =
  let same_window = Scope.equal (active_scope state) claim.expected.scope in
  let stored_opportunity = opportunity state in
  let outcome = Option.bind stored_opportunity ~f:Opportunity.outcome in
  let matches =
    match create, same_window, outcome with
    | true, true, Some (Checkpointed _)
    | false, true, Some (Unavailable Ineligible_response)
    | _, false, None -> true
    | _ -> false
  in
  if not matches
  then
    raise_s
      [%sexp
        "checkpoint opportunity did not settle with its exact factual outcome"
      , (outcome : Opportunity.outcome option)
      , (List.length (safe_catalog state) : int)
      , (List.map state.invocations ~f:invocation_result : string list)];
  if create
  then (
    let catalog = Option.value_exn state.recovery_notes in
    let notes = safe_catalog state in
    match notes with
    | [ note ] ->
      let content =
        Catalog.read catalog note.selection |> Result.ok |> Option.value_exn
      in
      require
        (String.equal (Catalog.Content.to_string content) private_body)
        "committed note differed from the selected private content";
      let proof =
        if same_window
        then
          A.Recovery_checkpoint_cut.opportunity_checkpoint
            state
            (Option.value_exn stored_opportunity)
          |> Option.value_exn
        else (
          match committed_checkpoint state ~predecessor:claim.expected.scope with
          | Checkpointed checkpoint ->
            (match Ledger.Checkpoint.recipe checkpoint with
             | Authored authored -> authored
             | Owner_selection _ ->
               Fixture.fail "committed cut lost authored checkpoint provenance")
          | Archive_only _ -> Fixture.fail "committed cut lost its checkpoint")
      in
      require
        (Option.exists proof.soft_note ~f:(fun authored ->
           P.Id.Operation.equal authored.opportunity claim.id
           && Catalog.equal_selection authored.created note.selection))
        "checkpoint did not bind the actual immutable creation receipt"
    | _ -> Fixture.fail "single checkpoint opportunity wrote an unexpected note set")
  else (
    if not same_window
    then (
      match committed_checkpoint state ~predecessor:claim.expected.scope with
      | Archive_only _ -> ()
      | Checkpointed _ -> Fixture.fail "ineligible response produced a checkpoint");
    require (List.is_empty (safe_catalog state)) "ineligible response changed local notes";
    require
      (List.is_empty state.invocations)
      "multiple output candidates entered the original canonical invocation branch")
;;

let run env environment =
  let preparations = ref 0 in
  Notes.with_fixture
    ~prompt_xml:prompt
    ~on_preparation:(fun () -> Int.incr preparations)
    env
    environment
    ~auth
    ~max_sessions:2
    (fun ~sw ~provider ~daemon ~proof ~client ->
       let run_one ~create =
         let handle = Notes.open_session sw env client ~mode:Owner_read_write in
         let actor = Reminder.enabled_actor daemon proof handle in
         H.start handle ~queue_if_limited:false |> ok |> ignore;
         set_policy actor;
         let start = Provider.request_count provider in
         ignore (send handle : P.Method_result.Send_message.t);
         let request = Provider.await_request provider env start in
         let claim, args = check_candidate actor request in
         let at_candidate = !preparations in
         require (at_candidate >= 2) "checkpoint candidate skipped real local preparation";
         let id = "soft-note-cut" in
         partial_call
           request
           ~id
           ~name:"recovery_note_write"
           ~arguments:(Jsonaf.to_string args);
         if not create
         then
           partial_call
             request
             ~id:"unselected-sibling"
             ~name:"context_remaining"
             ~arguments:"{}";
         (* Calls are complete as stream items, but the settled response is not yet proven. *)
         require
           (List.is_empty (safe_catalog (A.Session_actor.state actor |> ok)))
           "tool effect predated the terminal";
         Provider.finish request;
         let next =
           Support.Tui_fixture.await env (fun () ->
             match Provider.request_at provider (start + 1) with
             | Some request -> Some (Some request)
             | None ->
               let state = A.Session_actor.state actor |> ok in
               if
                 !preparations > at_candidate
                 && (not (Scope.equal claim.expected.scope (active_scope state)))
                 && Option.is_none state.active_operation
               then Some None
               else None)
         in
         require
           (!preparations > at_candidate)
           "worker reused the old prepared request after checkpoint settlement";
         let settled = A.Session_actor.state actor |> ok in
         verify_result settled ~claim ~create;
         Option.iter next ~f:(fun fresh ->
           require
             (not (Reminder.has_entry fresh ~role:"user" ~substring:guidance))
             "ordinary fresh preparation retained checkpoint-only guidance";
           require
             (Set.mem (Checkpoint.tools fresh) "context_remaining")
             "fresh ordinary request silently retained checkpoint-only capabilities";
           if Scope.equal claim.expected.scope (active_scope settled)
           then (
             if create
             then
               require
                 (P.Invocation.equal_outcome (Notes.output fresh ~id) (Complete `Null))
                 "actual authored note acknowledgement was not body-free";
             require
               (not
                  (String.is_substring
                     (Jsonaf.to_string (Provider.body fresh))
                     ~substring:private_body))
               "private note content escaped into ordinary request history");
           Provider.reply_summary fresh "Done.");
         ignore (Reminder.await_inactive env actor : A.Session_state.t)
       in
       run_one ~create:true;
       run_one ~create:false)
;;

let rec accepted_attempt : A.Session_delta.t -> Opportunity.Attempt_id.t option = function
  | Recovery_rollover_changed (Checkpoint_opportunity (Accept { attempt; _ })) ->
    Some attempt
  | Batch deltas -> List.find_map deltas ~f:accepted_attempt
  | _ -> None
;;

let synced_marker = "checkpoint-accept-durable"

let run_lost_accept_child env ~root ~port =
  let module Store = Agent_store in
  let module D = Agent_server.Daemon in
  let physical_requests = ref (fun () -> -1) in
  let wrapped =
    Support.Crash_fault_io.wrap
      env
      ~boundary:After_sync
      ~matches:(fun path ->
        String.equal (Filename.basename (Filename.dirname path)) "journal"
        && String.is_suffix path ~suffix:".log")
      ~reached:(fun path ->
        let scan =
          Eio.Path.load (Fixture.path env path)
          |> Store.Journal_segment.scan_contents
               ~max_payload_length:D.default_options.factory_limits.max_journal_payload
          |> Fixture.store_ok
        in
        match List.last scan.entries with
        | Some frame when Int.equal (Store.Frame.flags frame.frame) 0 ->
          let transaction =
            Store.Frame.payload frame.frame
            |> Store.Transaction.decode
            |> Fixture.store_ok
          in
          let delta =
            A.Session_delta_document.decode
              ~limits:Document_schema.Limits.default
              transaction.delta
            |> Result.map_error ~f:(fun error ->
              Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
            |> Result.ok_or_failwith
            |> A.Session_delta_document.value
          in
          Option.iter (accepted_attempt delta) ~f:(fun attempt ->
            require
              (Int.equal (!physical_requests ()) 0)
              "checkpoint reached the provider without its journal acknowledgement";
            Fixture.write
              env
              (Filename.concat root "checkpoint-attempt")
              (Opportunity.Attempt_id.to_string attempt);
            Eio.Flow.copy_string (synced_marker ^ "\n") (Eio.Stdenv.stdout env);
            Eio.Fiber.await_cancel ())
        | Some _ | None -> ())
  in
  Support.Temporary_environment.with_ ~env (fun environment ->
    Notes.with_fixture
      ~root
      ~port
      ~prompt_xml:prompt
      wrapped
      environment
      ~auth
      ~max_sessions:1
      (fun ~sw ~provider ~daemon ~proof ~client ->
         (physical_requests := fun () -> Provider.request_count provider);
         let handle = Notes.open_session sw env client ~mode:Owner_read_write in
         let actor = Reminder.enabled_actor daemon proof handle in
         H.start handle ~queue_if_limited:false |> ok |> ignore;
         set_policy actor;
         Fixture.write
           env
           (Filename.concat root "checkpoint-session")
           (P.Id.Session.to_string (H.session_id handle));
         ignore (send handle : P.Method_result.Send_message.t);
         Eio.Fiber.await_cancel ()))
;;

let run_lost_accept env environment =
  let module D = Agent_server.Daemon in
  let root = (Support.Temporary_environment.roots environment).temporary in
  let port =
    Eio.Switch.run (fun sw ->
      let reservation = Support.Port_reservation.create ~sw ~env in
      let port = Support.Port_reservation.port reservation in
      Support.Port_reservation.release reservation;
      let child =
        Fixture.child
          ~sw
          env
          environment
          ~case:"soft-checkpoint-lost-accept"
          ~arguments:[ "soft-checkpoint-lost-accept"; root; Int.to_string port ]
      in
      Exn.protect
        ~finally:(fun () -> Fixture.terminate env child)
        ~f:(fun () ->
          Fixture.await_marker env child synced_marker;
          Fixture.kill env child);
      port)
  in
  let coordinate name = Eio.Path.load (Fixture.path env (Filename.concat root name)) in
  let session_id = coordinate "checkpoint-session" |> P.Id.Session.of_string |> ok in
  let original =
    coordinate "checkpoint-attempt"
    |> Opportunity.Attempt_id.of_string
    |> Result.ok
    |> Option.value_exn
  in
  Notes.with_fixture
    ~port
    ~prompt_xml:prompt
    env
    environment
    ~auth
    ~max_sessions:1
    (fun ~sw ~provider ~daemon ~proof:_ ~client ->
       let handle =
         H.attach
           ~sw
           ~clock:(Eio.Stdenv.clock env)
           ~connection:client
           ~session_id
           ~mode:Owner_read_write
           ~subscribe:false
           ()
         |> ok
       in
       let actor =
         Agent_server.Session_registry.find (D.registry daemon) session_id
         |> Option.value_exn
         |> fun (entry : Agent_server.Session_registry.entry) -> entry.actor
       in
       let restored = A.Session_actor.state actor |> ok in
       let saved = opportunity restored |> Option.value_exn in
       let claim = Opportunity.claim saved in
       require
         (Scope.equal claim.expected.scope (active_scope restored)
          && Option.exists
               (Opportunity.attempt saved)
               ~f:(Opportunity.Attempt_id.equal original))
         "reopened state lost the actual synchronized checkpoint attempt";
       H.start handle ~queue_if_limited:false |> ok |> ignore;
       set_policy actor;
       ignore (send handle : P.Method_result.Send_message.t);
       let next =
         Support.Tui_fixture.await env (fun () ->
           match Provider.request_at provider 0 with
           | Some request -> Some (Some request)
           | None ->
             let state = A.Session_actor.state actor |> ok in
             if
               (not (Scope.equal claim.expected.scope (active_scope state)))
               && Option.is_none state.active_operation
             then Some None
             else None)
       in
       Option.iter next ~f:(fun request ->
         require
           (Set.mem (Checkpoint.tools request) "context_remaining")
           "recovery resubmitted the consumed checkpoint opportunity";
         require
           (not (Reminder.has_entry request ~role:"user" ~substring:guidance))
           "reopened submission replayed the old checkpoint guidance";
         Provider.reply_summary request "Done.");
       let state = Reminder.await_inactive env actor in
       if Scope.equal claim.expected.scope (active_scope state)
       then (
         let retained = opportunity state |> Option.value_exn in
         require
           (Opportunity.Claim.equal (Opportunity.claim retained) claim
            && Option.exists
                 (Opportunity.attempt retained)
                 ~f:(Opportunity.Attempt_id.equal original)
            && Option.equal
                 Opportunity.equal_outcome
                 (Opportunity.outcome retained)
                 (Some Interrupted))
           "replayed local opportunity was not settled once as interrupted")
       else (
         match committed_checkpoint state ~predecessor:claim.expected.scope with
         | Archive_only _ -> ()
         | Checkpointed _ -> Fixture.fail "unacknowledged response produced a checkpoint");
       require (List.is_empty (safe_catalog state)) "replay fabricated a checkpoint note")
;;
