open! Core
open Support
module Cold = Activation_tests
module Owner = Agent_server.Runtime_owner
module Host = Agent_server.Recurrence_execution_host
module Configuration = Agent_server.Recurrence_execution_configuration
module Client = Agent_client

let ref_for daemon session_id =
  P.Session_ref.create ~server_id:(S.server_id (Daemon.store daemon)) ~session_id
;;

let select_parent daemon session_id =
  let indexed =
    S.list_sessions_checked (Daemon.store daemon)
    |> store_ok
    |> List.find_exn ~f:(fun (entry : Agent_store.Session_index.Entry.t) ->
      P.Id.Session.equal entry.session.id session_id)
  in
  let expected =
    P.Session_lifecycle.Expected.create
      ~reference:(ref_for daemon session_id)
      ~generation:indexed.session.generation
      ~session_revision:indexed.session.revision
      ~lifecycle_revision:indexed.lifecycle_revision
    |> protocol_ok
  in
  Daemon.select_session daemon ~principal ~expected |> protocol_ok
;;

let%expect_test
    "indexed Ready captured target previews without replaying its authored startup"
  =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
      Mirage_crypto_rng_unix.use_default ();
      let root = T.temporary_root env in
      protect_cleanup
        ~finally:(fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           let parent_file = Filename.concat root "parent.chatmd" in
           let target_file = Filename.concat root "captured-legacy.chatmd" in
           List.iter
             [ ( parent_file
               , {|<developer>Actual captured acquisition controller.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
               )
             ; target_file, Cold.Authored_case.prompt Legacy_empty
             ]
             ~f:(fun (path, contents) ->
               Eio.Path.save
                 ~create:(`Exclusive 0o600)
                 Eio.Path.(Eio.Stdenv.fs env / path)
                 contents);
           let counters : Cold.counters = { constructions = ref 0; providers = ref 0 } in
           let policy =
             T.inference_policy
               ~default_model:"fixture-model"
               ~post_stream:(fun ~sw:_ ~inputs:_ ->
                 Int.incr counters.providers;
                 failwith "captured metadata preview submitted inference")
           in
           let policy =
             { policy with
               runtime_inference_ports =
                 (fun actor ->
                   Int.incr counters.constructions;
                   policy.runtime_inference_ports actor)
             }
           in
           let config = T.config root root parent_file in
           with_daemon
             env
             root
             config
             ~policy
             ~provider_calls:counters.providers
             (fun sw daemon ->
                let connection = T.connection daemon principal in
                let parent, child_id, before, admission =
                  protect_cleanup
                    ~finally:(fun () -> Client.Connection.close connection)
                    (fun () ->
                       T.initialize connection;
                       let session, _ =
                         T.create_session ~start_immediately:true connection
                       in
                       let parent = loaded daemon session.id in
                       start_parent_settled env sw connection parent;
                       let child_id, before, admission =
                         with_planner env sw daemon parent (fun () ->
                           Cold.with_cold_held
                             env
                             daemon
                             parent
                             ~root_file:target_file
                             ~counters
                             ~original:proof
                             (fun child prepared held checked _intent _attempt ->
                                let admission =
                                  Owner.start_held_authored_runtime
                                    child.runtime
                                    ~prepared
                                    ~held
                                    ~checked:
                                      (Actor.Recurrence_activation.Checked.admitted
                                         checked)
                                  |> protocol_ok
                                in
                                assert (Owner.has_activation_handoff child.runtime);
                                let before = state child in
                                let run =
                                  A.Run_state.find
                                    (Option.value_exn before.run_state)
                                    admission.run_id
                                  |> Option.value_exn
                                in
                                (match run.lifecycle with
                                 | Terminal (Completed _) -> ()
                                 | _ ->
                                   raise_s
                                     [%sexp
                                       "captured startup did not settle", (run : P.Run.t)]);
                                Ok (before.identity.session_id, before, admission)))
                       in
                       parent, child_id, before, admission)
                in
                let parent_id = (state parent).identity.session_id in
                let old_run =
                  A.Run_state.find (Option.value_exn before.run_state) admission.run_id
                  |> Option.value_exn
                in
                let configuration =
                  Configuration.create
                    ~principal_id:
                      (P.Id.Principal.of_string "pri_544_captured_metadata" |> protocol_ok)
                    ~controllers:
                      [ Configuration.Controller.create
                          ~session_id:parent_id
                          ~existing_targets:[ child_id ]
                          ~captured_children:false
                        |> protocol_ok
                      ]
                    ~expires_at:None
                  |> protocol_ok
                in
                let config =
                  { config with
                    server =
                      { config.server with recurrence_execution = Some configuration }
                  }
                in
                Daemon.shutdown daemon |> protocol_ok;
                let reopened =
                  Daemon.start
                    ~sw
                    ~env:(env :> Eio_unix.Stdenv.base)
                    ~config
                    ~tool_dir:root
                    ~home:root
                    ~process_start_identity:None
                    ~options:
                      { Daemon.default_options with
                        startup_mode = On_demand
                      ; inference_policy = policy
                      ; qualify_chatml_extensions = true
                      }
                    ()
                  |> protocol_ok
                in
                protect_cleanup
                  ~finally:(fun () -> Daemon.shutdown reopened |> protocol_ok)
                  (fun () ->
                     [%test_eq: int] 0 (Registry.stats (Daemon.registry reopened)).loaded;
                     let parent = select_parent reopened parent_id in
                     assert (
                       Option.is_none (Registry.find (Daemon.registry reopened) child_id));
                     let host =
                       Option.value_exn (Daemon.recurrence_execution_host reopened)
                     in
                     let controller = Host.controller host parent |> protocol_ok in
                     let counts = !(counters.constructions), !(counters.providers) in
                     let selected =
                       Host.acquire_existing_target
                         controller
                         ~session:(ref_for reopened child_id)
                       |> protocol_ok
                     in
                     let child = loaded reopened child_id in
                     assert (not (Owner.is_loaded child.runtime));
                     assert (not (Owner.has_activation_handoff child.runtime));
                     assert (
                       not (Registry.execution_selected (Daemon.registry reopened) child));
                     let acquired = state child in
                     assert (
                       A.Session_state.Runtime_initialization.equal
                         Ready
                         acquired.runtime_initialization);
                     assert (
                       P.Session.equal_desired_state Running acquired.lifecycle.desired);
                     assert (
                       List.equal
                         P.Moderator_execution.equal
                         before.moderator_executions
                         acquired.moderator_executions);
                     assert (
                       Option.equal
                         Jsonaf.exactly_equal
                         before.moderator
                         acquired.moderator);
                     assert (
                       P.Run.equal
                         old_run
                         (A.Run_state.find
                            (Option.value_exn acquired.run_state)
                            admission.run_id
                          |> Option.value_exn));
                     assert (
                       List.exists
                         (A.Run_state.receipts (Option.value_exn acquired.run_state))
                         ~f:(P.Run_receipt.equal admission));
                     let preview = Host.preview_target_source selected |> protocol_ok in
                     assert (
                       P.Run_source.equal
                         old_run.source
                         (Factory.Recurrence_source_preview.source preview));
                     [%test_eq: int64]
                       acquired.counters.revision
                       (Factory.Recurrence_source_preview.expected_revision preview);
                     Host.acquire_existing_target
                       controller
                       ~session:(ref_for reopened child_id)
                     |> protocol_ok
                     |> ignore;
                     assert (phys_equal child (loaded reopened child_id));
                     let after = state child in
                     assert (
                       Jsonaf.exactly_equal
                         (T.state_document acquired |> store_ok |> D.Document.json)
                         (T.state_document after |> store_ok |> D.Document.json));
                     [%test_eq: int * int]
                       counts
                       (!(counters.constructions), !(counters.providers));
                     [%test_eq: int] 0 !(counters.providers);
                     assert (not (Owner.is_loaded child.runtime));
                     print_endline
                       "real captured legacy startup completed under its original \
                        reciprocal Run";
                     print_endline
                       "reopened Ready metadata keeps that proof; preview neither \
                        resumes nor restores handoff authority")))));
  [%expect
    {|
    real captured legacy startup completed under its original reciprocal Run
    reopened Ready metadata keeps that proof; preview neither resumes nor restores handoff authority
    |}]
;;
