open! Core
module P = Agent_protocol
module A = Agent_session
module D = Agent_server.Daemon
module H = Agent_client.Session_handle
module Notes = Recovery_note_provider_scenario
module Checkpoint = Recovery_checkpoint_scenario
module Provider = Support.Tui_stream_provider
module Catalog = Recovery_notes.Catalog

let ok = Agent_server_test_support.protocol_ok
let require = Crash_recovery_fixture.require
let queued = "Recover the outstanding parser investigation."
let instruction = "Keep the user's exact queued message."

let config ?(input = 1800L) () =
  let module C = Agent_server.Recovery_capacity_configuration in
  C.create
    ~mode:Manual
    ~host_total:8192L
    ~targets:
      [ C.Target.
          { prompt = "restart.prompt"
          ; profile = "generated-loopback-fixture"
          ; model = "gpt-4.1"
          ; encoder = Openai_responses_v1
          ; total = 16384L
          ; input = Some input
          ; output = Some 500L
          ; session_total = Some 7000L
          ; omitted_output_default = Some 60L
          ; checkpoint_reserve = 512L
          ; warning_lead = 256L
          ; estimate_margin = 100L
          }
      ]
  |> ok
;;

let run env environment =
  let cut, signal = Eio.Promise.create () in
  let resume, release = Eio.Promise.create () in
  let stop_before_write = ref false in
  let preparations = ref 0 in
  let auth ~sw:_ _ =
    if !stop_before_write
    then (
      stop_before_write := false;
      Eio.Promise.resolve signal ();
      Eio.Promise.await resume);
    Openai.Responses_driver.Auth.bearer "local-successor-guard-fixture"
  in
  let source =
    {|<config model="gpt-4.1" max_tokens="100"/><developer>|}
    ^ instruction
    ^ {|</developer><tool name="recovery_note_write"/><tool name="context_checkpoint"/><tool name="context_new_window"/>|}
  in
  Notes.with_fixture
    ~on_preparation:(fun () -> Int.incr preparations)
    ~prompt_xml:source
    ~recovery_capacity:(config ())
    env
    environment
    ~auth
    ~max_sessions:2
    (fun ~sw ~provider ~daemon ~proof ~client ->
       let trial ~withdraw ~body =
         let first = Provider.request_count provider in
         let request = Provider.await_request provider env in
         let handle = Notes.open_session sw env client ~mode:Owner_read_write in
         let registry = D.registry daemon in
         let id = H.session_id handle in
         let entry = Agent_server.Session_registry.find registry id |> Option.value_exn in
         let owner =
           P.Session_ref.create
             ~server_id:(Agent_store.Session_store.server_id (D.store daemon))
             ~session_id:id
         in
         let check _state =
           Agent_server.Session_factory.check_registered_owner
             (D.factory daemon)
             entry
             ~session:owner
         in
         let attachment_id = (H.attachment handle).id in
         let scope =
           A.Session_actor.enable_recovery_windows
             entry.actor
             ~actor:proof
             ~attachment_id
             ~authorize:check
           |> ok
         in
         ignore (H.start handle ~queue_if_limited:false |> ok : P.Session.t);
         let send ?timing text =
           H.send_message handle ?timing { kind = Plain_text; text; attachments = [] }
           |> ok
         in
         ignore (send "Register today's handoff." : P.Method_result.Send_message.t);
         let before_checkpoint () =
           let receipt = send ~timing:After_current_operation queued in
           require
             (P.Method_result.Send_message.equal_disposition receipt.disposition Deferred)
             "the predecessor did not queue the required user message"
         in
         let acknowledged =
           Checkpoint.write_and_register
             ~body
             ~before_checkpoint
             request
             ~start:first
             scope
             ~id:"checkpoint"
         in
         let prior = !preparations in
         stop_before_write := withdraw;
         Checkpoint.request_transition acknowledged scope ~id:"roll";
         let state () = A.Session_actor.state entry.actor |> ok in
         if withdraw
         then (
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
             Eio.Promise.await cut);
           let current = state () in
           let active =
             Option.value_exn current.recovery_windows |> A.Recovery_window_ledger.active
           in
           require
             (not
                (P.Recovery_window.Scope.equal
                   scope
                   (A.Recovery_window_ledger.Window.scope active)))
             "the credential cut was not on the successor";
           let access =
             A.Recovery_note_access.operator
               proof
               ~owner
               ~generation:current.identity.generation
               ~attachment_id:(Some attachment_id)
               ~authorize:check
           in
           let selection =
             Catalog.
               { id = Note_id.of_int64 1L |> ok
               ; revision = Note_revision.of_int64 1L |> ok
               }
           in
           A.Session_actor.mutate_recovery_note_operator
             entry.actor
             ~access
             ~key:(Crash_recovery_fixture.key "withdraw-before-write")
             (Delete selection)
           |> ok
           |> ignore;
           Eio.Promise.resolve release ();
           Support.Tui_fixture.await env (fun () ->
             Option.some_if (Option.is_none (state ()).active_operation) ());
           require
             (Int.equal (Provider.request_count provider) (first + 3))
             "a withdrawn successor note crossed the physical provider boundary";
           require
             (List.length (state ()).conversation.deferred_user_entries = 1)
             "failed disclosure consumed the required user message")
         else (
           let successor = request (first + 3) in
           require
             (Int.equal (!preparations - prior) 2)
             "the no-hint successor did not use a second fresh preparation";
           let wire = Provider.body successor |> Jsonaf.to_string in
           require
             (String.is_substring wire ~substring:queued
              && String.is_substring wire ~substring:instruction)
             "dropping the oversized hint lost required user input or host instructions";
           require
             (not (String.is_substring wire ~substring:"Untrusted excerpts"))
             "the oversized optional hint crossed the provider boundary";
           Provider.reply_summary successor "Continue with the required input.";
           Support.Tui_fixture.await env (fun () ->
             Option.some_if (Option.is_none (state ()).active_operation) ());
           require
             (List.is_empty (state ()).conversation.deferred_user_entries)
             "successful no-hint submission left its required input pending");
         H.close handle
       in
       trial
         ~withdraw:true
         ~body:"This note is withdrawn during provider credential acquisition.";
       (* Four thousand bytes of printable, low-compressibility UTF-8. The complete
         local reference fits the excerpt ceiling and exceeds the selected target. *)
       let body =
         List.init 128 ~f:(fun i -> Md5.digest_string (Int.to_string i) |> Md5.to_hex)
         |> String.concat
       in
       trial ~withdraw:false ~body)
;;

let ordinary_hard_limit env environment =
  let preparations = ref 0 in
  let credentials = ref 0 in
  let auth ~sw:_ _ =
    Int.incr credentials;
    Openai.Responses_driver.Auth.bearer "manual-capacity-loopback"
  in
  let source =
    {|<config model="gpt-4.1" max_tokens="100"/><developer>Read the user's input.</developer>|}
  in
  Notes.with_fixture
    ~on_preparation:(fun () -> Int.incr preparations)
    ~prompt_xml:source
    ~recovery_capacity:(config ())
    env
    environment
    ~auth
    ~max_sessions:2
    (fun ~sw ~provider ~daemon ~proof ~client ->
       let text =
         List.init 640 ~f:(fun i -> Md5.digest_string (Int.to_string i) |> Md5.to_hex)
         |> String.concat
       in
       let submit ~enabled =
         let handle = Notes.open_session sw env client ~mode:Owner_read_write in
         let id = H.session_id handle in
         let entry =
           Agent_server.Session_registry.find (D.registry daemon) id |> Option.value_exn
         in
         let state () = A.Session_actor.state entry.actor |> ok in
         if enabled
         then (
           let owner =
             P.Session_ref.create
               ~server_id:(Agent_store.Session_store.server_id (D.store daemon))
               ~session_id:id
           in
           A.Session_actor.enable_recovery_windows
             entry.actor
             ~actor:proof
             ~attachment_id:(H.attachment handle).id
             ~authorize:(fun _ ->
               Agent_server.Session_factory.check_registered_owner
                 (D.factory daemon)
                 entry
                 ~session:owner)
           |> ok
           |> ignore);
         ignore (H.start handle ~queue_if_limited:false |> ok : P.Session.t);
         let before = state () in
         let submission =
           H.send_message handle { kind = Plain_text; text; attachments = [] } |> ok
         in
         if not enabled
         then (
           let req = Provider.await_request provider env 0 in
           require
             (String.is_substring (Provider.body req |> Jsonaf.to_string) ~substring:text)
             "unconfigured session failed to send its ordinary user input";
           Provider.reply_summary req "Received the input.");
         let after =
           Support.Tui_fixture.await env (fun () ->
             let current = state () in
             Option.some_if (Option.is_none current.active_operation) current)
         in
         if enabled
         then (
           require
             (List.is_empty (A.Inference_ledger.rows after.inference_ledger))
             "a request known to exceed the hard limit allocated an inference attempt";
           let reply =
             Agent_client.Connection.request
               client
               (Session_attach
                  { session_id = id
                  ; requested_mode = Read_only
                  ; subscribe = false
                  ; after_sequence = Some before.counters.event_sequence
                  ; reclaim_token = None
                  ; idempotency_key = Crash_recovery_fixture.key "capacity-failure-replay"
                  })
             |> ok
           in
           match reply with
           | P.Public.Result.Session_attach { replay = Events events; _ } ->
             require
               (List.exists events ~f:(fun event ->
                  match Support.Public_view.shared_payload_opt event with
                  | Some
                      (Operation_failed
                         { id; state = Failed { code = Resource_limit; _ }; _ }) ->
                    Option.exists submission.operation_id ~f:(P.Id.Operation.equal id)
                  | _ -> false))
               "the withheld operation lacked its factual capacity failure"
           | _ -> failwith "missing replay for the rejected preparation");
         H.close handle
       in
       submit ~enabled:true;
       require
         (!preparations = 1 && !credentials = 0 && Provider.request_count provider = 0)
         "known oversized input reached provider authentication or transport";
       submit ~enabled:false)
;;
