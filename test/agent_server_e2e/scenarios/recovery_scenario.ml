open Core
module Config_fixture = Support.Config_fixture
module Daemon_host = Support.Daemon_host
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Port_reservation.create ~sw ~env in
    let port = Port_reservation.port reservation in
    Port_reservation.release reservation;
    port)
;;

let test_approval_prompt_workspace env _environment =
  Permission_scenario.run env ~case:(Some "reviewer.restart-claimed-job");
  Prompt_scenario.run env ~case:(Some "prompt.referenced-artifact-corruption");
  Cleanup_scenario.run env ~case:(Some "cleanup.identity-change-refusal")
;;

let legacy_spec () =
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog (Agent_server.Catalog_identity.prompt_definition "smoke"))
    ~workspace:
      (Configured (Agent_server.Catalog_identity.workspace_definition "physical"))
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:"unattended"
    ~start_immediately:false
    ~labels:[ "suite", "legacy-recovery" ]
    ()
  |> protocol_ok
;;

let legacy_request () =
  Agent_protocol.Session.Create_request.
    { spec = legacy_spec ()
    ; requested_mode = None
    ; subscribe = false
    ; idempotency_key =
        Agent_protocol.Idempotency_key.of_string "legacy-recovery" |> protocol_ok
    }
;;

let test_legacy_import env environment =
  let fixture =
    Config_fixture.create environment ~name:"legacy-import" ~http_port:(reserve_port env)
  in
  let roots = Temporary_environment.roots environment in
  let source_path = Filename.concat roots.temporary "legacy-source" in
  Eio.Path.mkdir ~perm:0o700 (Temporary_environment.path environment source_path);
  let sentinel = Filename.concat source_path "sentinel" in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    (Temporary_environment.path environment sentinel)
    "unchanged";
  let allocator =
    History_entry.Allocator.create ~namespace:"legacy-recovery" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let id = History_entry.Allocator.allocate allocator |> Result.ok_or_failwith in
  let legacy =
    { (Session.create
         ~id:"legacy-source"
         ~prompt_file:(Config_fixture.prompt_path fixture)
         ())
      with
      history = [ Agent_session.History_codec.user_text ~id "legacy message" ]
    ; next_history_sequence = 1
    ; kv_store = [ "legacy-key", "legacy-value" ]
    }
  in
  let imported = ref None in
  Daemon_host.with_
    env
    fixture
    ~options:Agent_server.Daemon.default_options
    (fun _sw daemon ->
       let principal =
         Agent_server.Daemon.authenticate_http_bearer
           daemon
           (Some (Config_fixture.admin_token fixture))
         |> protocol_ok
       in
       imported
       := Some
            (Agent_server.Daemon.import_legacy
               daemon
               ~principal
               ~source_id:legacy.id
               ~source_path
               ~legacy
               (legacy_request ())
             |> protocol_ok));
  Daemon_host.with_
    env
    fixture
    ~options:Agent_server.Daemon.default_options
    (fun _sw daemon ->
       let session = Option.value_exn !imported in
       let entry =
         Agent_server.Session_registry.load
           (Agent_server.Daemon.registry daemon)
           session.id
         |> protocol_ok
       in
       let state = Agent_session.Session_actor.state entry.actor |> protocol_ok in
       Crash_recovery_fixture.require_equal
         "legacy history identities, payloads and order"
         [%sexp_of: Agent_protocol.History.entry list]
         (Agent_session.History_codec.all_to_protocol legacy.history)
         state.conversation.canonical_history;
       require
         (Agent_protocol.Session.equal_desired_state state.lifecycle.desired Stopped
          && Sexp.equal
               (Agent_protocol.Session.sexp_of_observed_state state.lifecycle.observed)
               (Agent_protocol.Session.sexp_of_observed_state Stopped))
         "legacy import did not remain stopped after restart";
       require
         (List.equal
            (fun (left_key, left_value) (right_key, right_value) ->
               String.equal left_key right_key && String.equal left_value right_value)
            state.conversation.kv_store
            [ "legacy-key", "legacy-value" ])
         "legacy key/value state changed";
       require
         (String.equal
            (Eio.Path.load (Temporary_environment.path environment sentinel))
            "unchanged")
         "legacy source was mutated")
;;

let cases =
  [ "journal.tail-corruption", Recovery_stopped_store.test_journal
  ; "snapshot.current-fallback", Recovery_stopped_store.test_snapshot_fallback
  ; "snapshot.complete-corruption", Recovery_stopped_store.test_snapshot_corruption
  ; "index.missing-rebuild", Recovery_stopped_store.test_index_missing
  ; "index.corrupt-fail-closed", Recovery_stopped_store.test_index_corruption
  ; "migration.inspect-dry-run-apply", Recovery_stopped_store.test_migration
  ; "approval-workspace-prompt-repair", test_approval_prompt_workspace
  ; "legacy.import-source-immutable", test_legacy_import
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown recovery case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"recovery-migration" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (name, test) ->
      try test env environment with
      | exn -> raise_s [%sexp "recovery E2E case failed", (name : string), (exn : Exn.t)]);
    Eio.Flow.copy_string
      (Sexp.to_string_hum
         [%sexp
           { scenario = ("recovery-migration" : string)
           ; selected_case = (case : string option)
           ; passed_cases = (List.map selected ~f:fst : string list)
           }]
       ^ "\n")
      (Eio.Stdenv.stdout env))
;;
