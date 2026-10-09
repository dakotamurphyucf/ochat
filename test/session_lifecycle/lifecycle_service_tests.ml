open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module L = P.Session_lifecycle

let key text = P.Idempotency_key.of_string text |> protocol_ok

let store_ok result =
  Result.map_error result ~f:Agent_store.Store_error.to_protocol_error |> protocol_ok
;;

let status = function
  | Ok _ -> "ok"
  | Error (failure : P.Error.t) -> P.Error.code_to_string failure.code
;;

let inspect client session_id =
  C.Admin.get_session client session_id |> protocol_ok |> P.Public.Snapshot.fields
;;

let observation snapshot = Option.value_exn snapshot.P.Public.Snapshot.Fields.lifecycle

let lifecycle_request ~key_text snapshot =
  L.Request.create
    ~expected:(L.Observation.expected (observation snapshot))
    ~idempotency_key:(key key_text)
;;

let rejected_receipt client command =
  match
    C.Connection.request_without_history
      client
      (P.Command.Command_receipt
         { method_name = P.Command.method_name command
         ; original_params = P.Command.params command
         })
    |> protocol_ok
  with
  | P.Method_result.Command_receipt (Failed { code = Conflict; _ }) -> true
  | _ -> false
;;

let start_daemon sw env ~root ~configuration calls =
  Agent_server.Daemon.start
    ~options:
      { Agent_server.Daemon.default_options with
        inference_policy =
          inference_policy
            ~default_model:"fixture-model"
            ~post_stream:(fun ~sw:_ ~inputs:_ ->
              Int.incr calls;
              failwith "lifecycle management activated provider")
      }
    ~sw
    ~env
    ~config:configuration
    ~tool_dir:root
    ~home:root
    ~process_start_identity:None
    ()
  |> protocol_ok
;;

let query =
  P.Session.List_request.
    { organization = P.Session_organization.Query.default
    ; page = P.Page.Request.create ~limit:100 () |> protocol_ok
    ; desired_state = None
    ; prompt_id = None
    ; workspace_id = None
    ; owner_principal_id = None
    ; creator_principal_id = None
    ; active_owner_principal_id = None
    ; labels = []
    ; sort = P.Session_catalog_query.Sort.default
    ; archive = All
    }
;;

let%expect_test
    "archive restart inspect restore and explicit resume preserve identity workspace and \
     original receipts"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let content = Filename.concat workspace "retained.txt" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / content)
          "user workspace content";
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Lifecycle fixture.</developer>";
        let configuration = config root workspace prompt in
        let calls = ref 0 in
        let original, archive_command, archived_at =
          Eio.Switch.run (fun sw ->
            let daemon = start_daemon sw env ~root ~configuration calls in
            let client = connection daemon (principal ()) in
            initialize client;
            let session, attachment = create_session ~key:"lifecycle-create" client in
            let current = inspect client session.id in
            let archive_command =
              P.Command.Session_delete
                { session_id = session.id
                ; attachment_id = attachment.id
                ; expected_revision = current.revision
                ; policy = Archive
                ; confirmation = P.Id.Session.to_string session.id
                ; idempotency_key = key "archive-original"
                }
            in
            let archived_at =
              match
                C.Connection.request_without_history client archive_command |> protocol_ok
              with
              | P.Method_result.Session_delete result -> result.deleted_at
              | _ -> failwith "archive result"
            in
            let archived = inspect client session.id in
            let stats =
              Agent_server.Session_registry.stats (Agent_server.Daemon.registry daemon)
            in
            print_s
              [%sexp
                (( L.Observation.status (observation archived)
                 , L.Observation.admission (observation archived)
                 , stats.loaded
                 , String.equal
                     (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / content))
                     "user workspace content" )
                 : L.Result.Status.t * L.Result.Admission.t * int * bool)];
            C.Connection.close client;
            Agent_server.Daemon.shutdown daemon |> protocol_ok;
            session, archive_command, archived_at)
        in
        Eio.Switch.run (fun sw ->
          let daemon = start_daemon sw env ~root ~configuration calls in
          let client = connection daemon (principal ()) in
          initialize client;
          let archived = inspect client original.id in
          let restore = lifecycle_request ~key_text:"restore-original" archived in
          let restored = C.Admin.restore_session client restore |> protocol_ok in
          let retried = C.Admin.restore_session client restore |> protocol_ok in
          let restored_snapshot = inspect client original.id in
          let before_resume =
            Agent_server.Session_registry.stats (Agent_server.Daemon.registry daemon)
          in
          print_s
            [%sexp
              (( L.Result.status restored
               , L.Result.admission restored
               , L.Revision.equal
                   (L.Expected.lifecycle_revision (L.Result.expected restored))
                   (L.Expected.lifecycle_revision (L.Result.expected retried))
               , P.Id.Prompt_revision.equal
                   (Option.value_exn original.prompt_revision)
                   (Option.value_exn restored_snapshot.session.prompt_revision)
               , P.Id.Workspace_instance.equal
                   (Option.value_exn original.workspace_instance)
                   (Option.value_exn restored_snapshot.session.workspace_instance)
               , before_resume.loaded )
               : L.Result.Status.t * L.Result.Admission.t * bool * bool * bool * int)];
          let blocked_attach =
            C.Connection.request
              client
              (Session_attach
                 { session_id = original.id
                 ; requested_mode = Read_write
                 ; subscribe = false
                 ; after_sequence = None
                 ; reclaim_token = None
                 ; idempotency_key = key "before-resume"
                 })
          in
          print_endline (status blocked_attach);
          let stale_resume =
            L.Request.create
              ~expected:(L.Observation.expected (observation archived))
              ~idempotency_key:(key "stale-resume")
          in
          print_endline (status (C.Admin.resume_session client stale_resume));
          print_s
            [%sexp
              (rejected_receipt client (P.Command.Session_resume stale_resume) : bool)];
          let resumed =
            C.Admin.resume_session
              client
              (lifecycle_request ~key_text:"resume-original" restored_snapshot)
            |> protocol_ok
          in
          let after_resume = inspect client original.id in
          let same_original_receipt =
            match
              C.Connection.request_without_history client archive_command |> protocol_ok
            with
            | P.Method_result.Session_delete value ->
              P.Timestamp.equal value.deleted_at archived_at
            | _ -> false
          in
          print_s
            [%sexp
              (( L.Result.admission resumed
               , after_resume.session.desired_state
               , same_original_receipt
               , L.Observation.admission (observation after_resume)
               , !calls )
               : L.Result.Admission.t
                 * P.Session.desired_state
                 * bool
                 * L.Result.Admission.t
                 * int)];
          let attachment =
            match
              C.Connection.request
                client
                (Session_attach
                   { session_id = original.id
                   ; requested_mode = Read_write
                   ; subscribe = false
                   ; after_sequence = None
                   ; reclaim_token = None
                   ; idempotency_key = key "after-resume"
                   })
              |> protocol_ok
            with
            | P.Public.Result.Session_attach value -> value.attachment
            | _ -> failwith "attach result"
          in
          let current = inspect client original.id in
          let remove_command =
            P.Command.Session_delete
              { session_id = original.id
              ; attachment_id = attachment.id
              ; expected_revision = current.revision
              ; policy = Remove
              ; confirmation = P.Id.Session.to_string original.id
              ; idempotency_key = key "remove-original"
              }
          in
          C.Connection.request_without_history client remove_command
          |> protocol_ok
          |> ignore;
          let rows = C.Admin.list_sessions_page client query |> protocol_ok in
          let receipt =
            C.Connection.request_without_history
              client
              (Command_receipt
                 { method_name = P.Command.method_name remove_command
                 ; original_params = P.Command.params remove_command
                 })
            |> protocol_ok
          in
          let committed =
            match receipt with
            | P.Method_result.Command_receipt (Committed (Deleted_session id)) ->
              P.Id.Session.equal id original.id
            | _ -> false
          in
          print_s
            [%sexp
              (( List.is_empty rows.items
               , committed
               , String.equal
                   (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / content))
                   "user workspace content"
               , !calls )
               : bool * bool * bool * int)];
          C.Connection.close client;
          Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {|
    (Archived Explicit_resume_required 0 true)
    (Active Explicit_resume_required true true true 0)
    invalid_state
    conflict
    true
    (Automatic Stopped true Automatic 0)
    (true true true 0)
    |}]
;;

let create_owned_child env root daemon parent =
  let module G = Agent_session.Generated_definition in
  let module Capabilities = Chat_response.Tool_capability in
  let definition =
    Agent_server.Runtime_owner.with_background_runtime
      parent.Agent_server.Session_registry.runtime
      (fun runtime ->
         let native =
           Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime
         in
         let capabilities =
           Lazy.force native.capabilities
           |> Result.map_error ~f:(fun failure -> failure.Capabilities.message)
           |> Result.ok_or_failwith
         in
         let bundle =
           Chatmd_source_bundle.create
             ~root_file:"child.chatmd"
             ~sources:[ "child.chatmd", "<developer>Owned retained child.</developer>" ]
             ()
           |> Result.ok_or_failwith
         in
         G.prepare
           ~env
           ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
           ~revision_id:(P.Id.Prompt_revision.create ())
           ~created_at:(P.Timestamp.now ())
           ~current_capabilities:(fun () -> capabilities)
           ~references:(Capabilities.references capabilities)
           bundle
         |> Result.map_error ~f:(fun diagnostics ->
           P.Error.invalid_request
             (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
              |> String.concat ~sep:"\n")))
    |> protocol_ok
  in
  let state = Agent_session.Session_actor.state parent.actor |> protocol_ok in
  Agent_server.Session_factory.create_generated_session
    (Agent_server.Daemon.factory daemon)
    ~parent_session_id:state.identity.session_id
    ~idempotency_key:(key "retained-owned-child")
    ~display_name:None
    definition
  |> protocol_ok
;;

let%expect_test
    "stopped live descendant obligation rejects removal and retired historical linkage \
     does not"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Retained linkage fixture.</developer>";
        Eio.Switch.run (fun sw ->
          let calls = ref 0 in
          let daemon =
            start_daemon sw env ~root ~configuration:(config root workspace prompt) calls
          in
          let client = connection daemon (principal ()) in
          initialize client;
          let parent, attachment =
            create_session ~start_immediately:true ~key:"link-parent" client
          in
          let parent_entry =
            Agent_server.Session_registry.find
              (Agent_server.Daemon.registry daemon)
              parent.id
            |> Option.value_exn
          in
          let child_entry = create_owned_child env root daemon parent_entry in
          let child =
            Agent_session.Session_actor.state child_entry.actor
            |> protocol_ok
            |> Agent_session.Session_state.summary
          in
          C.Connection.request
            client
            (Session_stop
               { session_id = parent.id
               ; attachment_id = attachment.id
               ; mode = Cancel
               ; idempotency_key = key "stop-linked-parent"
               })
          |> protocol_ok
          |> ignore;
          let module D = Agent_store.Delegation_store in
          let ledger =
            Agent_server.Daemon.store daemon |> Agent_store.Session_store.delegations
          in
          let record =
            D.with_records ledger ~max_records:100 ~max_bytes:1_000_000 ~f:(fun records ->
              List.find records ~f:(fun record ->
                P.Id.Session.equal record.D.key.parent_session_id parent.id
                && P.Id.Session.equal record.admission.child_session_id child.id)
              |> Result.of_option
                   ~error:
                     (Agent_store.Store_error.Corrupt "owned child fixture record missing"))
            |> store_ok
          in
          let deletion key_text =
            let current = inspect client parent.id in
            P.Command.Session_delete
              { session_id = parent.id
              ; attachment_id = attachment.id
              ; expected_revision = current.revision
              ; policy = Remove
              ; confirmation = P.Id.Session.to_string parent.id
              ; idempotency_key = key key_text
              }
          in
          let blocked_command = deletion "blocked-live-link" in
          let blocked = C.Connection.request_without_history client blocked_command in
          let still_present = inspect client parent.id in
          print_s
            [%sexp
              (( status blocked
               , still_present.session.desired_state
               , (inspect client child.id).session.desired_state
               , L.Observation.status (observation still_present)
               , !calls )
               : string
                 * P.Session.desired_state
                 * P.Session.desired_state
                 * L.Result.Status.t
                 * int)];
          D.revoke ledger record Parent_stopped |> store_ok |> ignore;
          D.discard_uninstalled_staging ledger record |> store_ok;
          let repeated = C.Connection.request_without_history client blocked_command in
          print_s
            [%sexp
              ((rejected_receipt client blocked_command, status repeated) : bool * string)];
          C.Connection.request_without_history client (deletion "remove-retired-history")
          |> protocol_ok
          |> ignore;
          let rows = C.Admin.list_sessions_page client query |> protocol_ok in
          print_s
            [%sexp
              (( List.exists rows.items ~f:(fun row ->
                   P.Id.Session.equal row.session.id child.id)
               , not
                   (List.exists rows.items ~f:(fun row ->
                      P.Id.Session.equal row.session.id parent.id))
               , Option.is_some (D.find ledger record.key |> store_ok)
               , !calls )
               : bool * bool * bool * int)];
          C.Connection.close client;
          Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {|
    (conflict Stopped Stopped Active 0)
    (true conflict)
    (true true true 0)
    |}]
;;

let%expect_test
    "original lifecycle receipts require current visibility while target exists"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Current visibility fixture.</developer>";
        Eio.Switch.run (fun sw ->
          let calls = ref 0 in
          let daemon =
            start_daemon sw env ~root ~configuration:(config root workspace prompt) calls
          in
          let creator = connection daemon (principal_with_id "pri_foreign_creator") in
          initialize creator;
          let session, attachment = create_session ~key:"foreign-session" creator in
          let current = inspect creator session.id in
          C.Connection.request_without_history
            creator
            (Session_delete
               { session_id = session.id
               ; attachment_id = attachment.id
               ; expected_revision = current.revision
               ; policy = Archive
               ; confirmation = P.Id.Session.to_string session.id
               ; idempotency_key = key "creator-archive"
               })
          |> protocol_ok
          |> ignore;
          let administrator = connection daemon (principal ()) in
          initialize administrator;
          let restore =
            lifecycle_request
              ~key_text:"administrator-restore"
              (inspect administrator session.id)
          in
          C.Admin.restore_session administrator restore |> protocol_ok |> ignore;
          let resume =
            lifecycle_request
              ~key_text:"administrator-resume"
              (inspect administrator session.id)
          in
          C.Admin.resume_session administrator resume |> protocol_ok |> ignore;
          let reduced =
            connection
              daemon
              (principal_with_scopes
                 (P.Id.Principal.to_string (principal ()).id)
                 (Set.remove scopes P.Scope.Administer_configuration))
          in
          initialize reduced;
          let receipt command =
            C.Connection.request_without_history
              reduced
              (Command_receipt
                 { method_name = P.Command.method_name command
                 ; original_params = P.Command.params command
                 })
          in
          print_s
            [%sexp
              (( status (C.Admin.restore_session reduced restore)
               , status (C.Admin.resume_session reduced resume)
               , status (receipt (Session_restore restore))
               , status (receipt (Session_resume resume))
               , (Agent_server.Session_registry.stats
                    (Agent_server.Daemon.registry daemon))
                   .loaded
               , !calls )
               : string * string * string * string * int * int)];
          C.Connection.close reduced;
          C.Connection.close administrator;
          C.Connection.close creator;
          Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {| (permission_denied permission_denied permission_denied permission_denied 0 0) |}]
;;

let with_lifecycle_root env f =
  let root = temporary_root env in
  Exn.protect
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
    ~f:(fun () ->
      let workspace = Filename.concat root "workspace" in
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
      let content = Filename.concat workspace "retained.txt" in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(Eio.Stdenv.fs env / content)
        "fault fixture workspace";
      let prompt = Filename.concat root "root.chatmd" in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(Eio.Stdenv.fs env / prompt)
        "<developer>Lifecycle fault fixture.</developer>";
      f ~root ~content ~configuration:(config root workspace prompt))
;;

let delete_command client session attachment ~policy ~key_text ~revision_offset =
  let current = inspect client session.P.Session.id in
  P.Command.Session_delete
    { session_id = session.id
    ; attachment_id = attachment.P.Session.Attachment.id
    ; expected_revision = Int64.(current.revision + revision_offset)
    ; policy
    ; confirmation = P.Id.Session.to_string session.id
    ; idempotency_key = key key_text
    }
;;

let receipt_state client command =
  match
    C.Connection.request_without_history
      client
      (P.Command.Command_receipt
         { method_name = P.Command.method_name command
         ; original_params = P.Command.params command
         })
    |> protocol_ok
  with
  | P.Method_result.Command_receipt receipt ->
    (match receipt with
     | Missing -> "missing"
     | Unavailable -> "unavailable"
     | Pending _ -> "pending"
     | Failed _ -> "failed"
     | Committed _ -> "committed")
  | _ -> failwith "command receipt result"
;;

let%expect_test
    "failed no-effect receipt acknowledgement preserves primary rejection and Pending"
  =
  Eio_main.run (fun raw_env ->
    Mirage_crypto_rng_unix.use_default ();
    let fault = Lifecycle_faults.create () in
    let env = Lifecycle_faults.wrap_env fault raw_env in
    with_lifecycle_root raw_env (fun ~root ~content:_ ~configuration ->
      Eio.Switch.run (fun sw ->
        let calls = ref 0 in
        let daemon = start_daemon sw env ~root ~configuration calls in
        let client = connection daemon (principal ()) in
        initialize client;
        let session, attachment = create_session ~key:"reject-fault-create" client in
        let command =
          delete_command
            client
            session
            attachment
            ~policy:Archive
            ~key_text:"rejected-original"
            ~revision_offset:1L
        in
        Lifecycle_faults.arm fault Rejection_completion;
        let primary = C.Connection.request_without_history client command in
        let pending = receipt_state client command in
        let repeated = C.Connection.request_without_history client command in
        let same_error =
          match primary, repeated with
          | Error first, Error second ->
            Jsonaf.exactly_equal (P.Error.to_json first) (P.Error.to_json second)
          | Ok _, _ | Error _, Ok _ -> false
        in
        print_s
          [%sexp
            (( Lifecycle_faults.was_triggered fault
             , status primary
             , pending
             , same_error
             , receipt_state client command
             , L.Observation.status (observation (inspect client session.id))
             , !calls )
             : bool * string * string * bool * string * L.Result.Status.t * int)];
        C.Connection.close client;
        Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect {| (true conflict pending true failed Active 0) |}]
;;

let%expect_test
    "uncertain archive acknowledgement fences observations and reconciles original proof \
     after reopen"
  =
  Eio_main.run (fun raw_env ->
    Mirage_crypto_rng_unix.use_default ();
    let fault = Lifecycle_faults.create () in
    let env = Lifecycle_faults.wrap_env fault raw_env in
    with_lifecycle_root raw_env (fun ~root ~content ~configuration ->
      let calls = ref 0 in
      let session, command =
        Eio.Switch.run (fun sw ->
          let daemon = start_daemon sw env ~root ~configuration calls in
          let client = connection daemon (principal ()) in
          initialize client;
          let session, attachment = create_session ~key:"archive-fault-create" client in
          let command =
            delete_command
              client
              session
              attachment
              ~policy:Archive
              ~key_text:"uncertain-original"
              ~revision_offset:0L
          in
          Lifecycle_faults.arm fault Authority_acknowledgement;
          let result = C.Connection.request_without_history client command in
          let inspection = C.Admin.get_session client session.id in
          let catalog = C.Admin.list_sessions_page client query in
          print_s
            [%sexp
              (( Lifecycle_faults.was_triggered fault
               , status result
               , status inspection
               , status catalog
               , !calls )
               : bool * string * string * string * int)];
          C.Connection.close client;
          Agent_server.Daemon.shutdown daemon |> protocol_ok;
          session, command)
      in
      Eio.Switch.run (fun sw ->
        let daemon = start_daemon sw env ~root ~configuration calls in
        let client = connection daemon (principal ()) in
        initialize client;
        let replay = C.Connection.request_without_history client command |> protocol_ok in
        let replayed_at =
          match replay with
          | P.Method_result.Session_delete value -> value.deleted_at
          | _ -> failwith "original archive replay"
        in
        let repeated =
          C.Connection.request_without_history client command |> protocol_ok
        in
        let same_result =
          match repeated with
          | P.Method_result.Session_delete value ->
            P.Timestamp.equal replayed_at value.deleted_at
          | _ -> false
        in
        let actual = inspect client session.id in
        print_s
          [%sexp
            (( L.Observation.status (observation actual)
             , L.Observation.admission (observation actual)
             , same_result
             , receipt_state client command
             , (Agent_server.Session_registry.stats (Agent_server.Daemon.registry daemon))
                 .loaded
             , String.equal
                 (Eio.Path.load Eio.Path.(Eio.Stdenv.fs raw_env / content))
                 "fault fixture workspace"
             , !calls )
             : L.Result.Status.t * L.Result.Admission.t * bool * string * int * bool * int)];
        C.Connection.close client;
        Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {|
    (true persistence_error persistence_error persistence_error 0)
    (Archived Explicit_resume_required true committed 0 true 0)
    |}]
;;

let%expect_test
    "failed destructive removal retains exact host result and stable proof for startup \
     retry"
  =
  Eio_main.run (fun raw_env ->
    Mirage_crypto_rng_unix.use_default ();
    List.iter [ Lifecycle_faults.Payload_deletion; Final_cleanup ] ~f:(fun phase ->
      let fault = Lifecycle_faults.create () in
      let env = Lifecycle_faults.wrap_env fault raw_env in
      with_lifecycle_root raw_env (fun ~root ~content ~configuration ->
        let calls = ref 0 in
        let command =
          Eio.Switch.run (fun sw ->
            let daemon = start_daemon sw env ~root ~configuration calls in
            let client = connection daemon (principal ()) in
            initialize client;
            let session, attachment = create_session ~key:"remove-fault-create" client in
            (* External workspace content is asserted below. Remove the unused
               empty session-layout workspace to exercise terminal retirement. *)
            let session_directory =
              Agent_store.Data_root.session_path
                (Agent_store.Session_store.data_root (Agent_server.Daemon.store daemon))
                session.id
            in
            Eio.Path.rmdir
              Eio.Path.(
                Eio.Stdenv.fs raw_env / Filename.concat session_directory "workspace");
            let command =
              delete_command
                client
                session
                attachment
                ~policy:Remove
                ~key_text:"remove-fault-original"
                ~revision_offset:0L
            in
            Lifecycle_faults.arm fault phase;
            let result = C.Connection.request_without_history client command in
            let pending_cleanup =
              Agent_store.Session_store.pending_removal_ids
                (Agent_server.Daemon.store daemon)
              |> store_ok
            in
            print_s
              [%sexp
                (( Lifecycle_faults.was_triggered fault
                 , status result
                 , receipt_state client command
                 , not (List.is_empty pending_cleanup)
                 , List.is_empty
                     (C.Admin.list_sessions_page client query |> protocol_ok).items )
                 : bool * string * string * bool * bool)];
            C.Connection.close client;
            Agent_server.Daemon.shutdown daemon |> protocol_ok;
            command)
        in
        Eio.Switch.run (fun sw ->
          let daemon = start_daemon sw env ~root ~configuration calls in
          let client = connection daemon (principal ()) in
          initialize client;
          let replayed = C.Connection.request_without_history client command in
          print_s
            [%sexp
              (( status replayed
               , receipt_state client command
               , List.is_empty
                   (Agent_store.Session_store.pending_removal_ids
                      (Agent_server.Daemon.store daemon)
                    |> store_ok)
               , String.equal
                   (Eio.Path.load Eio.Path.(Eio.Stdenv.fs raw_env / content))
                   "fault fixture workspace"
               , !calls )
               : string * string * bool * bool * int)];
          C.Connection.close client;
          Agent_server.Daemon.shutdown daemon |> protocol_ok))));
  [%expect
    {|
    (true persistence_error committed true true)
    (ok committed true true 0)
    (true persistence_error committed true true)
    (ok committed true true 0)
    |}]
;;

let%expect_test
    "caller cancellation at irreversible lifecycle boundaries preserves exact completed \
     outcome"
  =
  Eio_main.run (fun raw_env ->
    Mirage_crypto_rng_unix.use_default ();
    List.iter
      [ Lifecycle_faults.Authority_acknowledgement; Payload_deletion; Final_cleanup ]
      ~f:(fun phase ->
        let fault = Lifecycle_faults.create () in
        let env = Lifecycle_faults.wrap_env fault raw_env in
        with_lifecycle_root raw_env (fun ~root ~content ~configuration ->
          Eio.Switch.run (fun sw ->
            let calls = ref 0 in
            let daemon = start_daemon sw env ~root ~configuration calls in
            let client = connection daemon (principal ()) in
            initialize client;
            let session, attachment =
              create_session ~key:"cancel-boundary-create" client
            in
            let policy =
              match phase with
              | Authority_acknowledgement -> P.Session.Delete_request.Archive
              | Payload_deletion | Final_cleanup -> Remove
              | Rejection_completion
              | Actor_lock_release
              | Pending_claim
              | Mutation_completion -> failwith "not an irreversible boundary"
            in
            (match policy with
             | Archive -> ()
             | Remove ->
               let session_directory =
                 Agent_store.Data_root.session_path
                   (Agent_store.Session_store.data_root
                      (Agent_server.Daemon.store daemon))
                   session.id
               in
               Eio.Path.rmdir
                 Eio.Path.(
                   Eio.Stdenv.fs raw_env / Filename.concat session_directory "workspace"));
            let command =
              delete_command
                client
                session
                attachment
                ~policy
                ~key_text:"cancelled-original"
                ~revision_offset:0L
            in
            let finished, finish = Eio.Promise.create () in
            Eio.Fiber.fork ~sw (fun () ->
              (try
                 Eio.Switch.run (fun caller ->
                   Lifecycle_faults.arm_cancel fault phase ~cancel:(fun () ->
                     Eio.Switch.fail caller Exit);
                   try
                     ignore
                       (C.Connection.request_without_history client command
                        : (P.Method_result.t, P.Error.t) Result.t)
                   with
                   | Eio.Cancel.Cancelled _ -> ())
               with
               | Exit -> ());
              Eio.Promise.resolve finish ());
            Eio.Promise.await finished;
            let original_receipt = receipt_state client command in
            let replay = C.Connection.request_without_history client command in
            let rows = C.Admin.list_sessions_page client query |> protocol_ok in
            let actual_disposition =
              match policy with
              | Archive ->
                List.exists rows.items ~f:(fun row ->
                  P.Id.Session.equal row.session.id session.id
                  && row.archived
                  && L.Result.Admission.equal row.admission Explicit_resume_required)
              | Remove ->
                not
                  (List.exists rows.items ~f:(fun row ->
                     P.Id.Session.equal row.session.id session.id))
            in
            print_s
              [%sexp
                (( Lifecycle_faults.was_triggered fault
                 , original_receipt
                 , status replay
                 , actual_disposition
                 , List.is_empty
                     (Agent_store.Session_store.pending_removal_ids
                        (Agent_server.Daemon.store daemon)
                      |> store_ok)
                 , String.equal
                     (Eio.Path.load Eio.Path.(Eio.Stdenv.fs raw_env / content))
                     "fault fixture workspace"
                 , (Agent_server.Session_registry.stats
                      (Agent_server.Daemon.registry daemon))
                     .loaded
                 , !calls )
                 : bool * string * string * bool * bool * bool * int * int)];
            C.Connection.close client;
            Agent_server.Daemon.shutdown daemon |> protocol_ok))));
  [%expect
    {|
    (true committed ok true true true 0 0)
    (true committed ok true true true 0 0)
    (true committed ok true true true 0 0)
    |}]
;;

let%expect_test "lifecycle fault adapter preserves distinct opened rename destination" =
  Eio_main.run (fun raw_env ->
    Mirage_crypto_rng_unix.use_default ();
    let fault = Lifecycle_faults.create () in
    let env = Lifecycle_faults.wrap_env fault raw_env in
    with_lifecycle_root raw_env (fun ~root ~content:_ ~configuration:_ ->
      let path = Eio.Path.(Eio.Stdenv.fs env / root) in
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "source");
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "destination");
      Eio.Switch.run (fun sw ->
        let source = Eio.Path.open_dir ~sw Eio.Path.(path / "source") in
        let destination = Eio.Path.open_dir ~sw Eio.Path.(path / "destination") in
        Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(source / "entry") "original";
        Eio.Path.rename Eio.Path.(source / "entry") Eio.Path.(destination / "moved");
        print_s
          [%sexp
            (( String.equal (Eio.Path.load Eio.Path.(destination / "moved")) "original"
             , List.is_empty (Eio.Path.read_dir source) )
             : bool * bool)])));
  [%expect {| (true true) |}]
;;

let%expect_test
    "failed provisional cleanup retains actual owner and issuing ID for shutdown retry"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Owned cleanup.</developer>";
        Eio.Switch.run (fun sw ->
          let calls = ref 0 in
          let daemon =
            start_daemon sw env ~root ~configuration:(config root workspace prompt) calls
          in
          let client = connection daemon (principal ()) in
          Exn.protect
            ~finally:(fun () ->
              C.Connection.close client;
              Agent_server.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              initialize client;
              let session, _ = create_session ~key:"cleanup-owner-create" client in
              let module Registry = Agent_server.Session_registry in
              let entry =
                Registry.remove (Agent_server.Daemon.registry daemon) session.id
                |> Option.value_exn
              in
              let retained_state =
                Agent_session.Session_actor.state entry.actor |> protocol_ok
              in
              let reading = Registry.create () in
              let checked_index =
                Agent_server.Session_factory.index_entry retained_state
              in
              Registry.index reading checked_index;
              let entered, enter = Eio.Promise.create () in
              let release, resume = Eio.Promise.create () in
              let read_result, finish_read = Eio.Promise.create () in
              Registry.install_reader reading (fun _ ->
                Eio.Promise.resolve enter ();
                Eio.Promise.await release;
                Ok retained_state);
              Eio.Fiber.fork ~sw (fun () ->
                Eio.Promise.resolve
                  finish_read
                  (Registry.read_state reading ~authorize:(fun _ -> Ok ()) session.id));
              Eio.Promise.await entered;
              Registry.index
                reading
                { checked_index with
                  session =
                    { checked_index.session with
                      metadata_revision =
                        Int64.succ checked_index.session.metadata_revision
                    }
                };
              Eio.Promise.resolve resume ();
              let replacement_conflict =
                match Eio.Promise.await read_result with
                | Error failure -> P.Error.equal_code failure.code Conflict
                | Ok _ -> false
              in
              Registry.shutdown reading;
              print_s [%sexp (replacement_conflict : bool)];
              let second, _ = create_session ~key:"closing-loader-create" client in
              let provisional =
                Registry.remove (Agent_server.Daemon.registry daemon) second.id
                |> Option.value_exn
              in
              let second_state =
                Agent_session.Session_actor.state provisional.actor |> protocol_ok
              in
              let loading = Registry.create () in
              Registry.index
                loading
                (Agent_server.Session_factory.index_entry second_state);
              let loader_entered, loader_enter = Eio.Promise.create () in
              let loader_release, loader_resume = Eio.Promise.create () in
              let loaded_result, finish_load = Eio.Promise.create () in
              let closed_result, finish_close = Eio.Promise.create () in
              let provisional_closed = ref 0 in
              Registry.install_loader loading (fun _ ->
                Eio.Promise.resolve loader_enter ();
                Eio.Promise.await loader_release;
                Ok
                  { provisional with
                    close =
                      (fun () ->
                        Int.incr provisional_closed;
                        provisional.close ())
                  });
              Eio.Fiber.fork ~sw (fun () ->
                Eio.Promise.resolve finish_load (Registry.load loading second.id));
              Eio.Promise.await loader_entered;
              Eio.Fiber.fork ~sw (fun () ->
                Registry.shutdown loading;
                Eio.Promise.resolve finish_close ());
              let rec await_closing () =
                if Registry.is_closing loading
                then ()
                else (
                  Eio.Fiber.yield ();
                  await_closing ())
              in
              await_closing ();
              let provisional_joined = Option.is_none (Eio.Promise.peek closed_result) in
              Eio.Promise.resolve loader_resume ();
              let provisional_rejected =
                match Eio.Promise.await loaded_result with
                | Error failure -> P.Error.equal_code failure.code Server_shutting_down
                | Ok _ -> false
              in
              Eio.Promise.await closed_result;
              print_s
                [%sexp
                  (( provisional_joined
                   , provisional_rejected
                   , !provisional_closed
                   , Option.is_none (Registry.find loading second.id) )
                   : bool * bool * int * bool)];
              let third, _ = create_session ~key:"retry-shutdown-create" client in
              let shutdown_entry =
                Registry.remove (Agent_server.Daemon.registry daemon) third.id
                |> Option.value_exn
              in
              let shutdown_attempts = ref 0 in
              let close_entered, close_enter = Eio.Promise.create () in
              let close_release, close_resume = Eio.Promise.create () in
              let closing = Registry.create () in
              Registry.add
                closing
                ~session_id:third.id
                { shutdown_entry with
                  close =
                    (fun () ->
                      Int.incr shutdown_attempts;
                      if !shutdown_attempts = 1
                      then failwith "owned entry close failed"
                      else (
                        Eio.Promise.resolve close_enter ();
                        Eio.Promise.await close_release;
                        shutdown_entry.close ()))
                }
              |> protocol_ok;
              let close_failed =
                try
                  Registry.shutdown closing;
                  false
                with
                | Failure message -> String.equal message "owned entry close failed"
              in
              let owner_retained = Option.is_some (Registry.find closing third.id) in
              let first_shutdown, first_finish = Eio.Promise.create () in
              let second_shutdown, second_finish = Eio.Promise.create () in
              let second_entered, second_enter = Eio.Promise.create () in
              Eio.Fiber.fork ~sw (fun () ->
                Registry.shutdown closing;
                Eio.Promise.resolve first_finish ());
              Eio.Promise.await close_entered;
              Eio.Fiber.fork ~sw (fun () ->
                Eio.Promise.resolve second_enter ();
                Registry.shutdown closing;
                Eio.Promise.resolve second_finish ());
              Eio.Promise.await second_entered;
              Eio.Fiber.yield ();
              let serialized =
                Option.is_none (Eio.Promise.peek first_shutdown)
                && Option.is_none (Eio.Promise.peek second_shutdown)
                && !shutdown_attempts = 2
              in
              Eio.Promise.resolve close_resume ();
              Eio.Promise.await first_shutdown;
              Eio.Promise.await second_shutdown;
              print_s
                [%sexp
                  (( close_failed
                   , owner_retained
                   , serialized
                   , !shutdown_attempts
                   , Option.is_none (Registry.find closing third.id) )
                   : bool * bool * bool * int * bool)];
              let fourth, _ = create_session ~key:"cancel-provisional-create" client in
              let cancel_entry =
                Registry.remove (Agent_server.Daemon.registry daemon) fourth.id
                |> Option.value_exn
              in
              let cancel_state =
                Agent_session.Session_actor.state cancel_entry.actor |> protocol_ok
              in
              let cancelled_registry = Registry.create () in
              Registry.index
                cancelled_registry
                (Agent_server.Session_factory.index_entry cancel_state);
              let cancel_attempts = ref 0 in
              let cancellation_preserved = ref false in
              (try
                 Eio.Switch.run (fun caller ->
                   Registry.install_loader cancelled_registry (fun _ ->
                     Eio.Switch.fail caller Exit;
                     Ok
                       { cancel_entry with
                         close =
                           (fun () ->
                             Int.incr cancel_attempts;
                             if !cancel_attempts = 1
                             then failwith "cancelled provisional cleanup failure"
                             else cancel_entry.close ())
                       });
                   try
                     ignore
                       (Registry.load cancelled_registry fourth.id
                        : (Registry.entry, P.Error.t) Result.t)
                   with
                   | Eio.Cancel.Cancelled _ -> cancellation_preserved := true)
               with
               | Exit -> ());
              let cancelled_retry_refused =
                match Registry.load cancelled_registry fourth.id with
                | Error failure -> P.Error.equal_code failure.code Conflict
                | Ok _ -> false
              in
              Registry.shutdown cancelled_registry;
              print_s
                [%sexp
                  ((!cancellation_preserved, cancelled_retry_refused, !cancel_attempts)
                   : bool * bool * int)];
              let fifth, _ = create_session ~key:"read-boundary-close-create" client in
              let read_entry =
                Registry.remove (Agent_server.Daemon.registry daemon) fifth.id
                |> Option.value_exn
              in
              let final_read = Registry.create () in
              Registry.add final_read ~session_id:fifth.id read_entry |> protocol_ok;
              let post_authorization_unavailable =
                match
                  Registry.read_state final_read fifth.id ~authorize:(fun _ ->
                    read_entry.close ();
                    Ok ())
                with
                | Error failure -> P.Error.equal_code failure.code Persistence_error
                | Ok _ -> false
              in
              ignore (Registry.remove final_read fifth.id : Registry.entry option);
              Registry.shutdown final_read;
              print_s [%sexp (post_authorization_unavailable : bool)];
              let attempts = ref 0 in
              let entry =
                { entry with
                  close =
                    (fun () ->
                      Int.incr attempts;
                      if !attempts <= 2
                      then failwith "original provisional cleanup failure"
                      else entry.close ())
                }
              in
              let registry = Registry.create () in
              let target = Registry_lifecycle_tests.indexed "ses_rejected_provisional" in
              let other = Registry_lifecycle_tests.indexed "ses_other_provisional" in
              Registry.index_all registry [ target; other ];
              Registry.install_loader registry (fun _ -> Ok entry);
              let original_admission =
                match Registry.load registry target.session.id with
                | Error failure -> P.Error.equal_code failure.code Persistence_error
                | Ok _ -> false
              in
              let retry_refused =
                match Registry.load registry target.session.id with
                | Error failure -> P.Error.equal_code failure.code Conflict
                | Ok _ -> false
              in
              let unrelated_progress =
                Result.is_ok
                  (Registry.with_lifecycle registry other.session.id (fun _ -> Ok ()))
              in
              let retry_failure_preserved =
                try
                  Registry.shutdown registry;
                  false
                with
                | Failure message ->
                  String.equal message "original provisional cleanup failure"
              in
              Registry.shutdown registry;
              print_s
                [%sexp
                  (( original_admission
                   , retry_refused
                   , unrelated_progress
                   , retry_failure_preserved
                   , !attempts
                   , !calls )
                   : bool * bool * bool * bool * int * int)]))));
  [%expect
    {|
    true
    (true true 1 true)
    (true true true 2 true)
    (true true 2)
    true
    (true true true true 3 0)
    |}]
;;

let%expect_test
    "failed lifecycle retirement keeps primary Pending and actual detached cleanup owner"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Lifecycle cleanup ownership.</developer>";
        Eio.Switch.run (fun sw ->
          let module Registry = Agent_server.Session_registry in
          let module Service = Agent_server.Session_lifecycle_service in
          let module I = Agent_store.Idempotency_store in
          let calls = ref 0 in
          let daemon =
            start_daemon sw env ~root ~configuration:(config root workspace prompt) calls
          in
          let client = connection daemon (principal ()) in
          Exn.protect
            ~finally:(fun () ->
              C.Connection.close client;
              Agent_server.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              initialize client;
              let session, attachment = create_session ~key:"cleanup-lifecycle" client in
              let registry = Agent_server.Daemon.registry daemon in
              let original = Registry.remove registry session.id |> Option.value_exn in
              let closes = ref 0 in
              let entry =
                { original with
                  close =
                    (fun () ->
                      Int.incr closes;
                      if !closes <= 2
                      then failwith "retired lifecycle close failed"
                      else original.close ())
                }
              in
              Registry.add registry ~session_id:session.id entry |> protocol_ok;
              let store = Agent_server.Daemon.store daemon in
              let idempotency =
                I.open_or_create
                  ~env
                  ~path:
                    (Filename.concat
                       (Agent_store.Session_store.data_root store
                        |> Agent_store.Data_root.indexes_path)
                       "idempotency.sexp")
                |> store_ok
              in
              let request : P.Session.Delete_request.t =
                { session_id = session.id
                ; attachment_id = attachment.id
                ; expected_revision =
                    (Agent_session.Session_actor.state original.actor |> protocol_ok)
                      .counters
                      .revision
                ; policy = Archive
                ; confirmation = P.Id.Session.to_string session.id
                ; idempotency_key = key "retire-original"
                }
              in
              let command = P.Command.Session_delete request in
              let receipt_key : I.Key.t =
                { principal_id = (principal ()).id
                ; session_id = Some session.id
                ; method_name = P.Command.method_name command
                ; idempotency_key = request.idempotency_key
                }
              in
              let digest =
                P.Command.params command
                |> P.Json_codec.canonical_string
                |> protocol_ok
                |> Digestif.SHA256.digest_string
                |> Digestif.SHA256.to_hex
              in
              let now = P.Timestamp.of_time_ns Time_ns.epoch in
              I.record
                idempotency
                { key = receipt_key
                ; request_digest = digest
                ; accepted_transaction_sequence = None
                ; outcome = Pending
                ; created_at = now
                ; expires_at = None
                ; retention = Protected
                }
              |> store_ok
              |> ignore;
              let factory = Agent_server.Daemon.factory daemon in
              let service =
                Service.create
                  ~store
                  ~registry
                  ~idempotency
                  ~now:(fun () -> now)
                  ~read_owned_session:
                    (Agent_server.Session_factory.read_owned_session factory)
                  ~validate_removal:(fun _ -> Ok ())
              in
              let authorizations = ref 0 in
              let primary =
                P.Error.create
                  Conflict
                  ~message:"original authority changed"
                  ~retryable:false
                  ()
              in
              let result =
                Service.execute
                  service
                  ~key:receipt_key
                  ~request_digest:digest
                  ~authorize:(fun _ ->
                    Int.incr authorizations;
                    if !authorizations = 1 then Ok () else Error primary)
                  ~validate_attachment:(fun _ _ -> Ok ())
                  (Delete request)
              in
              let primary_and_secondary =
                match result with
                | Ok _ -> false
                | Error failure ->
                  Jsonaf.exactly_equal
                    (P.Error.to_json (Service.Failure.error failure))
                    (P.Error.to_json primary)
                  && Service.Failure.equal_disposition
                       (Service.Failure.disposition failure)
                       Recovery_required
                  && Option.exists
                       (Service.Failure.cleanup_error failure)
                       ~f:(fun failure ->
                         Option.is_some
                           (Registry.Cleanup_failure.exception_and_backtrace failure))
              in
              let pending =
                match I.lookup idempotency ~key:receipt_key ~request_digest:digest with
                | Replay { outcome = Pending; _ } -> true
                | Missing | Conflict _ | Replay _ -> false
              in
              let same_id_excluded =
                match Registry.load registry session.id with
                | Error { code = Conflict; _ } -> true
                | Ok _ | Error _ -> false
              in
              let unrelated =
                P.Id.Session.of_string "ses_cleanup_unrelated" |> protocol_ok
              in
              let unrelated_progress =
                Registry.with_lifecycle registry unrelated (fun _ -> Ok ())
                |> Result.is_ok
              in
              let first_retry_preserves =
                try
                  Registry.shutdown registry;
                  false
                with
                | Failure message -> String.equal message "retired lifecycle close failed"
              in
              let store_retained = not (Agent_store.Session_store.is_closed store) in
              Registry.shutdown registry;
              print_s
                [%sexp
                  (( primary_and_secondary
                   , pending
                   , same_id_excluded
                   , unrelated_progress
                   , first_retry_preserves
                   , store_retained
                   , !closes
                   , !calls )
                   : bool * bool * bool * bool * bool * bool * int * int)]))));
  [%expect {| (true true true true true true 3 0) |}]
;;

let%expect_test "startup rollback retains actual failed owner and its storage until retry"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Retained startup rollback ownership.</developer>";
        Eio.Switch.run (fun sw ->
          let module R = Agent_server.Session_registry in
          let calls = ref 0 in
          let daemon =
            start_daemon sw env ~root ~configuration:(config root workspace prompt) calls
          in
          let client = connection daemon (principal ()) in
          Exn.protect
            ~finally:(fun () ->
              C.Connection.close client;
              Agent_server.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              initialize client;
              let session, _ = create_session ~key:"startup-rollback" client in
              let registry = Agent_server.Daemon.registry daemon in
              let original = R.remove registry session.id |> Option.value_exn in
              let handle = original.store_handle |> Option.value_exn in
              let attempts = ref 0 in
              let retained =
                { original with
                  close =
                    (fun () ->
                      Int.incr attempts;
                      if Int.equal !attempts 1
                      then failwith "original startup close failure";
                      original.close ())
                }
              in
              R.add registry ~session_id:session.id retained |> protocol_ok;
              let primary =
                P.Error.create
                  Persistence_error
                  ~message:"original startup admission failure"
                  ~retryable:false
                  ()
              in
              R.rollback_recovered
                registry
                ~primary:(R.Cleanup_failure.rejected primary)
                [ retained ];
              let owns_handle = R.retains_cleanup_handle registry handle in
              let directory_present =
                Eio.Path.is_directory
                  Eio.Path.(
                    Eio.Stdenv.fs env / Agent_store.Session_store.Handle.directory handle)
              in
              let same_owner_still_bound =
                Option.exists (R.find registry session.id) ~f:(fun entry ->
                  phys_equal entry.actor original.actor)
              in
              let fresh_load_refused =
                match R.load registry session.id with
                | Error error -> P.Error.equal_code error.code Conflict
                | Ok _ -> false
              in
              R.shutdown registry;
              let owner_released = not (R.retains_cleanup_handle registry handle) in
              print_s
                [%sexp
                  (( owns_handle
                   , directory_present
                   , same_owner_still_bound
                   , fresh_load_refused
                   , owner_released
                   , !attempts
                   , !calls )
                   : bool * bool * bool * bool * bool * int * int)]))));
  [%expect {| (true true true true true 2 0) |}]
;;

let%expect_test "provider close serializes retries and denies admission before yielding" =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let module Port = Agent_server.Provider_operator_port in
      let entered, entered_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let first, first_u = Eio.Promise.create () in
      let second, second_u = Eio.Promise.create () in
      let attempts = ref 0 in
      let dispatches = ref 0 in
      let port =
        Port.create
          ~dispatch:(fun ~actor:_ _ ->
            Int.incr dispatches;
            Error P.Provider_operator.Error.Unsupported)
          ~receipt:(fun ~actor:_ _ ->
            Int.incr dispatches;
            Error P.Provider_operator.Error.Unsupported)
          ~close:(fun () ->
            Int.incr attempts;
            if Int.equal !attempts 1
            then (
              Eio.Promise.resolve entered_u ();
              Eio.Promise.await release;
              failwith "original provider close failure"))
      in
      Eio.Fiber.fork ~sw (fun () ->
        let preserved =
          try
            Port.close port;
            false
          with
          | Failure message -> String.equal message "original provider close failure"
        in
        Eio.Promise.resolve first_u preserved);
      Eio.Promise.await entered;
      let actor = Operator_authorization.trusted_local (principal ()) in
      let denied =
        match
          Port.dispatch (Some port) ~actor (P.Command.Provider_status { profile = None })
        with
        | Error error -> P.Error.equal_code error.code Server_shutting_down
        | Ok _ -> false
      in
      Eio.Fiber.fork ~sw (fun () ->
        Port.close port;
        Eio.Promise.resolve second_u ());
      Eio.Fiber.yield ();
      let serialized =
        Option.is_none (Eio.Promise.peek second) && Int.equal !attempts 1
      in
      Eio.Promise.resolve release_u ();
      let preserved = Eio.Promise.await first in
      Eio.Promise.await second;
      Port.close port;
      print_s
        [%sexp
          ((denied, serialized, preserved, !attempts, !dispatches)
           : bool * bool * bool * int * int)]));
  [%expect {| (true true true 2 0) |}]
;;

let%expect_test "startup failure retains store until actual operator retry completes" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Startup scope.</developer>";
        let store_root =
          Eio.Switch.run (fun sw ->
            let daemon =
              start_daemon
                sw
                env
                ~root
                ~configuration:(config root workspace prompt)
                (ref 0)
            in
            let path =
              Agent_server.Daemon.store daemon
              |> Agent_store.Session_store.data_root
              |> Agent_store.Data_root.path
            in
            Agent_server.Daemon.shutdown daemon |> protocol_ok;
            path)
        in
        let attempts = ref 0 in
        let open_store sw nonce =
          Agent_store.Session_store.open_existing
            ~env
            ~sw
            ~root:store_root
            ~process_start_identity:None
            ~lock_nonce:nonce
        in
        Eio.Switch.run (fun sw ->
          let store = open_store sw "startup-owned" |> store_ok in
          let guard = Agent_server.Startup_cleanup.create ~sw ~store in
          let registry = Agent_server.Session_registry.create () in
          Agent_server.Startup_cleanup.adopt_registry_exn guard registry;
          let port =
            Agent_server.Provider_operator_port.create
              ~dispatch:(fun ~actor:_ _ -> Error P.Provider_operator.Error.Unsupported)
              ~receipt:(fun ~actor:_ _ -> Error P.Provider_operator.Error.Unsupported)
              ~close:(fun () ->
                Int.incr attempts;
                if Int.equal !attempts 1 then failwith "original operator cleanup failure")
          in
          Agent_server.Startup_cleanup.adopt_operator_exn guard port;
          let primary =
            P.Error.create
              Persistence_error
              ~message:"original startup failure"
              ~retryable:false
              ()
          in
          let original_preserved =
            match
              Agent_server.Startup_cleanup.protect guard (fun () -> Error primary)
            with
            | Error error -> String.equal error.message primary.message
            | Ok () -> false
          in
          let still_owned =
            match open_store sw "startup-competitor" with
            | Error _ -> true
            | Ok unexpected ->
              Agent_store.Session_store.close unexpected |> store_ok;
              false
          in
          print_s
            [%sexp ((original_preserved, still_owned, !attempts) : bool * bool * int)]);
        Eio.Switch.run (fun sw ->
          let reopened = open_store sw "startup-after-retry" |> store_ok in
          Agent_store.Session_store.close reopened |> store_ok;
          print_s [%sexp (!attempts : int)])));
  [%expect
    {|
    (true true 1)
    2
    |}]
;;

let%expect_test
    "Factory activation failure retains concrete recovery cleanup after lock sync fault"
  =
  Eio_main.run (fun raw_env ->
    Mirage_crypto_rng_unix.use_default ();
    let fault = Lifecycle_faults.create () in
    let env = Lifecycle_faults.wrap_env fault raw_env in
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Actual recovery activation ownership.</developer>";
        Eio.Switch.run (fun sw ->
          let module R = Agent_server.Session_registry in
          let module S = Agent_store.Session_store in
          let module A = Agent_session.Session_actor in
          let fail_activation = ref false in
          let failed_actor = ref None in
          let providers = ref 0 in
          let base =
            inference_policy
              ~default_model:"fixture-model"
              ~post_stream:(fun ~sw:_ ~inputs:_ ->
                Int.incr providers;
                failwith "recovery cleanup activated provider")
          in
          let policy =
            { base with
              Agent_server.Session_factory.runtime_inference_ports =
                (fun actor ->
                  if !fail_activation
                  then (
                    failed_actor := Some actor;
                    Lifecycle_faults.arm fault Actor_lock_release;
                    Error Inference_runtime.Preparation_error.Target_unavailable)
                  else base.runtime_inference_ports actor)
            }
          in
          let daemon =
            Agent_server.Daemon.start
              ~options:
                { Agent_server.Daemon.default_options with inference_policy = policy }
              ~sw
              ~env
              ~config:(config root workspace prompt)
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ()
            |> protocol_ok
          in
          let client = connection daemon (principal ()) in
          Exn.protect
            ~finally:(fun () ->
              C.Connection.close client;
              Agent_server.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              initialize client;
              let session, _ =
                create_session
                  ~start_immediately:true
                  ~key:"factory-recovery-owner"
                  client
              in
              let other, _ = create_session ~key:"factory-recovery-unrelated" client in
              let registry = Agent_server.Daemon.registry daemon in
              let original = R.remove registry session.id |> Option.value_exn in
              Agent_server.Runtime_owner.close_and_wait original.runtime;
              original.close ();
              let store = Agent_server.Daemon.store daemon in
              let indexed =
                Agent_store.Session_index.find_checked (S.session_index store) session.id
                |> store_ok
                |> Option.value_exn
              in
              R.index registry indexed;
              fail_activation := true;
              let primary_preserved =
                match
                  Agent_server.Session_factory.recover_session
                    (Agent_server.Daemon.factory daemon)
                    indexed
                with
                | Error error ->
                  P.Error.equal_code error.code Invalid_state
                  && String.equal
                       error.message
                       (Sexp.to_string_hum
                          (Inference_runtime.Preparation_error.sexp_of_t
                             Target_unavailable))
                | Ok unexpected ->
                  unexpected.close ();
                  false
              in
              let actor_was_owned = Option.is_some !failed_actor in
              let completed_actor_stage =
                Option.exists !failed_actor ~f:(fun actor ->
                  Result.is_error (A.state actor))
              in
              let same_id_fenced =
                match R.load registry session.id with
                | Error error -> P.Error.equal_code error.code Conflict
                | Ok _ -> false
              in
              let unrelated_progress =
                Result.is_ok (R.with_lifecycle registry other.id (fun _ -> Ok ()))
              in
              let root_still_owned =
                match
                  S.open_existing
                    ~env
                    ~sw
                    ~root:(S.data_root store |> Agent_store.Data_root.path)
                    ~process_start_identity:None
                    ~lock_nonce:"recovery-competitor"
                with
                | Error _ -> true
                | Ok unexpected ->
                  S.close unexpected |> store_ok;
                  false
              in
              let triggered = Lifecycle_faults.was_triggered fault in
              R.shutdown registry;
              print_s
                [%sexp
                  (( primary_preserved
                   , actor_was_owned
                   , completed_actor_stage
                   , same_id_fenced
                   , unrelated_progress
                   , root_still_owned
                   , triggered
                   , !providers )
                   : bool * bool * bool * bool * bool * bool * bool * int)]))));
  [%expect {| (true true true true true true true 0) |}]
;;

let%expect_test
    "schedule lifecycle admission rejects before claim and same key admits exactly once"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    with_lifecycle_root env (fun ~root ~content:_ ~configuration ->
      Eio.Switch.run (fun sw ->
        let calls = ref 0 in
        let daemon = start_daemon sw env ~root ~configuration calls in
        let client = connection daemon (principal ()) in
        initialize client;
        let session, attachment =
          create_session ~key:"schedule-admission-create" client
        in
        let command =
          P.Command.Schedule_create
            { session_id = session.id
            ; attachment_id = attachment.id
            ; payload = `Null
            ; due = After_ms 60000
            ; misfire = Deliver_once_immediately
            ; idempotency_key = key "schedule-admission-original"
            }
        in
        let rejected, missing =
          Agent_server.Session_registry.with_lifecycle
            (Agent_server.Daemon.registry daemon)
            session.id
            (fun _ ->
               let rejected =
                 match C.Connection.request_without_history client command with
                 | Error { P.Error.code = Conflict; retryable = true; _ } -> true
                 | Ok _ | Error _ -> false
               in
               Ok (rejected, String.equal (receipt_state client command) "missing"))
          |> protocol_ok
        in
        let admitted () =
          match C.Connection.request_without_history client command |> protocol_ok with
          | P.Method_result.Schedule_create result -> result.schedule.id
          | _ -> failwith "schedule create result"
        in
        let original = admitted () in
        let replay = admitted () in
        let snapshot = inspect client session.id in
        print_s
          [%sexp
            (rejected : bool)
          , (missing : bool)
          , (P.Id.Schedule.equal original replay : bool)
          , (Int.equal (List.length snapshot.schedules) 1 : bool)
          , (String.equal (receipt_state client command) "committed" : bool)
          , (!calls : int)];
        let metadata_command =
          P.Command.Session_update_metadata
            { session_id = session.id
            ; attachment_id = attachment.id
            ; expected_metadata_revision = session.metadata_revision
            ; patch =
                P.Session_metadata.Patch.create
                  ~name:(Set "admitted metadata")
                  ~set_labels:[]
                  ~remove_labels:[]
                |> protocol_ok
            ; idempotency_key = key "metadata-admission-original"
            }
        in
        let rejected_metadata, missing_metadata =
          Agent_server.Session_registry.with_lifecycle
            (Agent_server.Daemon.registry daemon)
            session.id
            (fun _ ->
               let rejected =
                 match C.Connection.request_without_history client metadata_command with
                 | Error { P.Error.code = Conflict; retryable = true; _ } -> true
                 | Ok _ | Error _ -> false
               in
               Ok
                 (rejected, String.equal (receipt_state client metadata_command) "missing"))
          |> protocol_ok
        in
        ignore
          (C.Connection.request_without_history client metadata_command |> protocol_ok
           : P.Method_result.t);
        ignore
          (C.Connection.request_without_history client metadata_command |> protocol_ok
           : P.Method_result.t);
        let changed = (inspect client session.id).session in
        print_s
          [%sexp
            (rejected_metadata : bool)
          , (missing_metadata : bool)
          , (Int64.equal changed.metadata_revision Int64.(session.metadata_revision + 1L)
             : bool)
          , (Option.equal
               String.equal
               changed.spec.display_name
               (Some "admitted metadata")
             : bool)];
        C.Connection.close client;
        Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {|
    (true true true true true 0)
    (true true true true)
  |}]
;;

let%expect_test
    "schedule post-effect completion failure retains Pending and suppresses duplicate"
  =
  Eio_main.run (fun raw_env ->
    Mirage_crypto_rng_unix.use_default ();
    let fault = Lifecycle_faults.create () in
    let env = Lifecycle_faults.wrap_env fault raw_env in
    with_lifecycle_root raw_env (fun ~root ~content:_ ~configuration ->
      Eio.Switch.run (fun sw ->
        let calls = ref 0 in
        let daemon = start_daemon sw env ~root ~configuration calls in
        let client = connection daemon (principal ()) in
        initialize client;
        let session, attachment =
          create_session ~key:"schedule-uncertain-create" client
        in
        let command =
          P.Command.Schedule_create
            { session_id = session.id
            ; attachment_id = attachment.id
            ; payload = `Null
            ; due = After_ms 60000
            ; misfire = Deliver_once_immediately
            ; idempotency_key = key "schedule-uncertain-original"
            }
        in
        Lifecycle_faults.arm fault Mutation_completion;
        let failed =
          match C.Connection.request_without_history client command with
          | Error { P.Error.code = Persistence_error; _ } -> true
          | Ok _ | Error _ -> false
        in
        let pending =
          match
            C.Connection.request_without_history
              client
              (P.Command.Command_receipt
                 { method_name = P.Command.method_name command
                 ; original_params = P.Command.params command
                 })
            |> protocol_ok
          with
          | P.Method_result.Command_receipt (Pending { accepted_sequence = Some _; _ }) ->
            true
          | _ -> false
        in
        let suppressed =
          match C.Connection.request_without_history client command with
          | Error { P.Error.code = Interrupted; _ } -> true
          | Ok _ | Error _ -> false
        in
        print_s
          [%sexp
            (Lifecycle_faults.was_triggered fault : bool)
          , (failed : bool)
          , (pending : bool)
          , (suppressed : bool)
          , (Int.equal (List.length (inspect client session.id).schedules) 1 : bool)
          , (!calls : int)];
        C.Connection.close client;
        Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect {| (true true true true true 0) |}]
;;

let%expect_test "prepared writer rechecks attachment after durable pending admission" =
  Eio_main.run (fun raw_env ->
    Mirage_crypto_rng_unix.use_default ();
    let fault = Lifecycle_faults.create () in
    let env = Lifecycle_faults.wrap_env fault raw_env in
    with_lifecycle_root raw_env (fun ~root ~content:_ ~configuration ->
      Eio.Switch.run (fun sw ->
        let calls = ref 0 in
        let daemon = start_daemon sw env ~root ~configuration calls in
        let client = connection daemon (principal ()) in
        initialize client;
        let session, attachment = create_session ~key:"prepared-writer-create" client in
        let entry =
          Agent_server.Session_registry.find
            (Agent_server.Daemon.registry daemon)
            session.id
          |> Option.value_exn
        in
        let command =
          P.Command.Schedule_create
            { session_id = session.id
            ; attachment_id = attachment.id
            ; payload = `Null
            ; due = After_ms 60000
            ; misfire = Deliver_once_immediately
            ; idempotency_key = key "prepared-writer-original"
            }
        in
        Lifecycle_faults.arm_cancel fault Pending_claim ~cancel:(fun () ->
          Agent_session.Session_actor.detach entry.actor attachment.id |> protocol_ok);
        let rejected =
          match C.Connection.request_without_history client command with
          | Error { P.Error.code = Invalid_request; _ } -> true
          | Ok _ | Error _ -> false
        in
        print_s
          [%sexp
            (Lifecycle_faults.was_triggered fault : bool)
          , (rejected : bool)
          , (List.is_empty (inspect client session.id).schedules : bool)
          , (String.equal (receipt_state client command) "failed" : bool)
          , (!calls : int)];
        C.Connection.close client;
        Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect {| (true true true true 0) |}]
;;
