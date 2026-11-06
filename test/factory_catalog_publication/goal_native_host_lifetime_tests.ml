open! Core
open Agent_server_test_support
open Retry_support
open Goal_native_test_support
module Intent = P.Goal_native_resume_intent

let stored_intent entry invocation_id =
  List.find_exn (state entry).invocations ~f:(fun invocation ->
    P.Id.Invocation.equal invocation.P.Invocation.context.id invocation_id)
  |> fun invocation ->
  match invocation.P.Invocation.native_goal_action with
  | Some (P.Goal_native_action_intent.Resume intent) -> intent
  | None | Some (Start _) -> failwith "expected actual native Resume intent"
;;

let document_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;

let check_snapshot_marker current =
  let limits = Document_schema.Limits.default in
  let document =
    A.Session_state_document.authored current
    |> A.Session_state_document.encode ~limits
    |> document_ok
  in
  assert (
    List.mem
      (Document_schema.Document.required_semantics document)
      Intent.persistence_semantics
      ~equal:String.equal);
  ignore
    (A.Session_state_document.decode ~limits document |> document_ok
     : A.Session_state_document.t);
  let fields =
    match Document_schema.Document.json document with
    | `Object fields -> fields
    | _ -> failwith "session state document must be an object"
  in
  let other_semantics =
    Document_schema.Document.required_semantics document
    |> List.filter ~f:(fun name -> not (String.equal name Intent.persistence_semantics))
    |> List.map ~f:(fun name -> `String name)
  in
  List.iter
    [ `Array other_semantics
    ; `Array (`String "goal.future-resume.v999" :: other_semantics)
    ]
    ~f:(fun required ->
      let altered =
        `Object (List.Assoc.add fields ~equal:String.equal "required_semantics" required)
        |> Document_schema.Document.inspect ~limits
        |> document_ok
      in
      assert (Result.is_error (A.Session_state_document.decode ~limits altered)))
;;

(* Operator_only is the existing trusted lower-host fixture: real Factory,
   Actor and native callback admission, but no automatic execution schedulers.
   This does not expose an Operator_only transport command or manually drive
   the dispatcher that this test intentionally leaves absent. *)
let%expect_test
    "native Goal Start and Resume remain pending without their host; restart never \
     replays DATA"
  =
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let ordinary_entered, ordinary_entered_u = Eio.Promise.create () in
  let ordinary_release, ordinary_release_u = Eio.Promise.create () in
  let release_ordinary () =
    if Option.is_none (Eio.Promise.peek ordinary_release)
    then Eio.Promise.resolve ordinary_release_u ()
  in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr counters.providers;
      (match !(counters.providers) with
       | 2 ->
         Eio.Promise.resolve ordinary_entered_u ();
         Eio.Promise.await ordinary_release
       | _ -> ());
      completed_response "native Goal host-disabled actual provider")
  in
  Creation_tests.with_daemon
    ~startup_mode:Operator_only
    ~prompt_text:(prompt ~v1:false)
    ~policy
    (fun env sw root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         Exn.protect ~finally:release_ordinary ~f:(fun () ->
           let ( old_actor
               , accepted
               , invocation_id
               , start
               , start_id
               , session_id
               , session
               , resume )
             =
             Goal_job_runtime_tests.with_loaded_manual_entry daemon (fun entry runtime ->
               let session, before = Goal.create_goal daemon entry in
               let headless =
                 A.Run_admission_request.Headless.create
                   ~session_id:before.identity.session_id
                   ~generation:before.identity.generation
                   ~expected_revision:before.counters.revision
                   ~mode:Workflow
                   ~input:
                     (User_submission
                        { kind = Plain_text
                        ; text = "host-disabled original Goal episode"
                        ; attachments = []
                        })
                   ~key:
                     (P.Idempotency_key.of_string "native-host-disabled-episode"
                      |> protocol_ok)
                 |> protocol_ok
               in
               let request =
                 A.Goal_activation_request.create
                   ~goal_id:Goal.goal_id
                   ~expected_goal_revision:(G.revision (Goal.goal before))
                   ~expected_catalog_revision:
                     (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
                   ~request:headless
                 |> protocol_ok
               in
               let service =
                 Agent_server.Goal_run_service.create
                   ~factory:(Daemon.factory daemon)
                   ~authorize
               in
               let admission =
                 Agent_server.Goal_run_service.start_episode
                   service
                   ~entry
                   ~actor:original
                   ~session
                   ~request
                 |> protocol_ok
                 |> Goal.activated
               in
               ignore (Goal.await_input env entry counters : A.Session_state.t);
               let held = set_passive daemon entry in
               with_ordinary_owner daemon entry ~v1:false (fun receipt ->
                 assert (Option.is_none receipt);
                 Eio.Promise.await ordinary_entered;
                 let operation = Option.value_exn (state entry).active_operation in
                 Goal_native_start_selection_tests.create_unstarted_goal
                   entry
                   runtime
                   operation;
                 let resume =
                   Staging.exact_request (state entry) ~key:"native-host-disabled-resume"
                 in
                 let result, invocations =
                   callback entry runtime ~operation ~mode:"resume" ~run:(fun admission ->
                     Actor.stage_native_goal_resume
                       entry.actor
                       ~admission
                       ~authorize
                       ~request:resume)
                 in
                 assert (result |> protocol_ok);
                 let invocation = List.hd_exn invocations in
                 let accepted = stored_intent entry invocation.context.id in
                 (match Intent.phase accepted with
                  | Accepted _ -> ()
                  | Staged | Dispatch_claimed _ | Discarded _ | Rejected _ -> assert false);
                 let request =
                   Goal_native_start_selection_tests.start_request
                     entry
                     ~key:"native-host-disabled-start"
                 in
                 let result, starts =
                   callback entry runtime ~operation ~mode:"start" ~run:(fun admission ->
                     Actor.stage_native_goal_start
                       entry.actor
                       ~admission
                       ~authorize
                       ~request)
                 in
                 assert (result |> protocol_ok);
                 let start_id =
                   match starts with
                   | [ invocation ] -> invocation.P.Invocation.context.id
                   | _ -> failwith "missing actual host-disabled Start invocation"
                 in
                 let start =
                   Goal_native_start_selection_tests.stored_start entry start_id
                 in
                 (match P.Goal_native_start_intent.phase start with
                  | Accepted _ -> ()
                  | Staged | Dispatch_claimed _ | Admitted _ | Discarded _ | Rejected _ ->
                    failwith "host-disabled Start did not retain Accepted DATA");
                 release_ordinary ();
                 let current = await_quiet entry in
                 check_snapshot_marker current;
                 (* Select is an opaque scheduling reference, not Begin or a claim.
                There is deliberately no Driver.create/process_entry here. *)
                 let pending =
                   Actor.select_native_goal_resume entry.actor |> protocol_ok
                 in
                 assert (Option.is_some pending);
                 assert (
                   Option.is_some
                     (Actor.select_native_goal_action entry.actor |> protocol_ok));
                 assert (
                   P.Goal_native_start_intent.equal
                     start
                     (Goal_native_start_selection_tests.stored_start entry start_id));
                 assert (
                   Option.is_none
                     (G.episode
                        (Goal_native_start_selection_tests.unstarted_goal current)));
                 assert (
                   not
                     (List.exists current.conversation.canonical_history ~f:(fun item ->
                        P.History.equal_role User item.P.History.role
                        && String.is_substring
                             (Jsonaf.to_string item.payload)
                             ~substring:Goal_native_start_selection_tests.unstarted_input)));
                 [%test_eq: int] 2 !(counters.providers);
                 assert (G.equal (Goal.goal held) (Goal.goal current));
                 assert (
                   A.Goal_accounting.equal
                     (Goal.accounting held)
                     (Goal.accounting current));
                 assert (
                   P.Run.equal
                     (A.Run_state.find (Option.value_exn held.run_state) admission.run_id
                      |> Option.value_exn)
                     (A.Run_state.find
                        (Option.value_exn current.run_state)
                        admission.run_id
                      |> Option.value_exn));
                 assert (
                   Option.is_none
                     (Actor.goal_resume_receipt
                        entry.actor
                        ~actor:original
                        ~authorize:(authorize original)
                        ~session
                        ~request:resume
                      |> protocol_ok));
                 ( entry.actor
                 , accepted
                 , invocation.context.id
                 , start
                 , start_id
                 , held.identity.session_id
                 , session
                 , resume )))
           in
           (* Join both the writer and real entry bracket before host shutdown. *)
           let old_store = Daemon.store daemon in
           Daemon.shutdown daemon |> protocol_ok;
           assert (Agent_store.Session_store.is_closed old_store);
           (* Reopen with normal recovery loading and execution schedulers.
              Operator_only intentionally has no loader; recovered DATA must not
              dispatch even when a new execution-capable host is available. *)
           let recovered =
             Daemon.start
               ~sw
               ~env:(env :> Eio_unix.Stdenv.base)
               ~config:(config root root (Filename.concat root "parent.chatmd"))
               ~tool_dir:root
               ~home:root
               ~process_start_identity:None
               ~options:
                 { Daemon.default_options with
                   inference_policy = policy
                 ; startup_mode = Execute
                 }
               ()
             |> protocol_ok
           in
           protect_cleanup
             ~finally:(fun () -> Daemon.shutdown recovered |> protocol_ok)
             (fun () ->
                let loaded =
                  Registry.load (Daemon.registry recovered) session_id |> protocol_ok
                in
                assert (not (phys_equal loaded.Registry.actor old_actor));
                let runtime =
                  Owner.with_loaded_runtime loaded.runtime (fun runtime -> Ok runtime)
                  |> protocol_ok
                in
                await_activation loaded runtime;
                assert (Intent.equal accepted (stored_intent loaded invocation_id));
                assert (
                  P.Goal_native_start_intent.equal
                    start
                    (Goal_native_start_selection_tests.stored_start loaded start_id));
                assert (
                  Option.is_none
                    (Actor.select_native_goal_action loaded.actor |> protocol_ok));
                assert (
                  Option.is_none
                    (Actor.select_native_goal_resume loaded.actor |> protocol_ok));
                [%test_eq: int] 2 !(counters.providers);
                assert (
                  Option.is_none
                    (Actor.goal_resume_receipt
                       loaded.actor
                       ~actor:original
                       ~authorize:(authorize original)
                       ~session
                       ~request:resume
                     |> protocol_ok))))));
  print_endline
    "disabled host retains both Accepted action tags; joined callbacks and restart issue \
     no Start or Resume";
  [%expect
    {| disabled host retains both Accepted action tags; joined callbacks and restart issue no Start or Resume |}]
;;
