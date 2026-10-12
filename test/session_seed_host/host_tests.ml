open! Core
open Agent_server_test_support
open Support

let%expect_test
    "actual stopped seed retains neutral history and initializes only on authored run"
  =
  with_root (fun env ~root ~prompt_file:_ config ->
    with_host env ~root config (fun sw daemon client ->
      let transcript = transcript () in
      let original = request ~code:(catalog_code client) ~key:"neutral-seed" transcript in
      let seeded = C.Session_handle.seed client original |> protocol_ok in
      let state = assert_stopped daemon seeded transcript in
      let again = C.Session_handle.seed client original |> protocol_ok in
      assert (P.Id.Session.equal seeded.session_id again.session_id);
      let retained =
        request
          ~code:(Retained (expected daemon state))
          ~key:"retained-neutral-seed"
          transcript
      in
      let child = C.Session_handle.seed client retained |> protocol_ok in
      let child_state = assert_stopped daemon child transcript in
      assert (not (P.Id.Session.equal seeded.session_id child.session_id));
      assert (
        P.Id.Prompt_revision.equal
          state.spec.prompt_revision_id
          child_state.spec.prompt_revision_id);
      assert (
        String.equal
          state.spec.permission_profile_digest
          child_state.spec.permission_profile_digest);
      assert (
        Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.equal
          state.spec.root_invocation
          child_state.spec.root_invocation);
      let handle =
        C.Session_handle.attach
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~connection:client
          ~session_id:child.session_id
          ~mode:Read_write
          ~subscribe:false
          ()
        |> protocol_ok
      in
      Exn.protect
        ~finally:(fun () -> C.Session_handle.close handle)
        ~f:(fun () ->
          let current = Support.state daemon child.session_id in
          let receipt =
            C.Session_handle.start_run
              handle
              ~generation:current.identity.generation
              ~expected_revision:current.counters.revision
              ~mode:Workflow
              ~input:Authored_start
            |> protocol_ok
          in
          let entry = entry daemon child.session_id in
          let rec completed attempts =
            if attempts = 0 then failwith "seeded authored run did not finish";
            let state = A.Session_actor.state entry.actor |> protocol_ok in
            match
              List.find
                (A.Run_state.runs (Option.value_exn state.run_state))
                ~f:(fun run -> P.Id.Run.equal run.id receipt.run_id)
            with
            | Some run
              when P.Run.Lifecycle.equal run.lifecycle (Terminal (Completed None)) ->
              state
            | None | Some _ ->
              Eio.Fiber.yield ();
              completed (attempts - 1)
          in
          let after = completed 1000 in
          assert (
            List.equal
              P.History.equal_entry
              (List.take
                 after.conversation.canonical_history
                 (List.length (P.Session_seed.Transcript.entries transcript)))
              (P.Session_seed.Transcript.entries transcript));
          assert (
            A.Session_state.Runtime_initialization.equal
              after.runtime_initialization
              Ready);
          assert (Option.is_some after.moderator);
          print_endline
            "stopped exact neutral prefix; retained code new identity; one explicit \
             authored initialization")));
  [%expect
    {| stopped exact neutral prefix; retained code new identity; one explicit authored initialization |}]
;;

let%expect_test
    "actual lost seed reply reconciles original client intent without new creation"
  =
  with_root (fun env ~root ~prompt_file:_ config ->
    with_host env ~root config (fun _ daemon _ ->
      let notifications = Eio.Stream.create 256 in
      let authorized = ref true in
      let actor =
        Operator_authorization.guarded ~principal:(principal ()) ~is_current:(fun () ->
          !authorized)
      in
      let context =
        S.Connection_context.create_authenticated
          ~actor
          ~connection_id:"seed-lost-reply"
          ~transport:In_memory
          ~publish_notification:(Eio.Stream.add notifications)
          ~max_attachments:64
      in
      let drop = ref true in
      let committed = ref None in
      let client =
        C.In_memory.create
          ~notifications
          ~close:(fun () -> S.Daemon.close_connection daemon context)
          ~request:(fun command ->
            let result =
              S.Dispatcher.dispatch_command (S.Daemon.dispatcher daemon) ~context command
            in
            match command, result with
            | Session_seed _, Ok (Non_history value) when !drop ->
              (match P.Public_result.Non_history.value value with
               | P.Method_result.Session_seed value -> committed := Some value.session_id
               | _ -> failwith "seed reply kind");
              drop := false;
              Error
                (P.Error.create
                   Interrupted
                   ~message:"fixture dropped committed seed reply"
                   ~retryable:false
                   ())
            | _ -> result)
      in
      Exn.protect
        ~finally:(fun () -> C.Connection.close client)
        ~f:(fun () ->
          initialize client;
          let transcript = transcript () in
          let request =
            request ~code:(catalog_code client) ~key:"lost-seed-reply" transcript
          in
          (match C.Session_handle.seed client request with
           | Error { code = Interrupted; _ } -> ()
           | Error _ | Ok _ -> failwith "fixture did not lose reply");
          let pending =
            match C.Connection.pending_commands client with
            | [ pending ] -> pending
            | _ -> failwith "one original pending intent"
          in
          let receipt = C.Connection.reconcile client pending |> protocol_ok in
          let session_id = Option.value_exn !committed in
          (match receipt with
           | Committed (Created_session received) ->
             assert (P.Id.Session.equal session_id received)
           | _ -> failwith "original seed receipt");
          assert (List.is_empty (C.Connection.pending_commands client));
          let replay = C.Session_handle.seed client request |> protocol_ok in
          assert (P.Id.Session.equal replay.session_id session_id);
          ignore (assert_stopped daemon replay transcript : A.Session_state.t);
          authorized := false;
          let original = P.Command.Session_seed request in
          let receipt =
            C.Connection.request_without_history
              client
              (Command_receipt
                 { method_name = P.Command.method_name original
                 ; original_params = P.Command.params original
                 })
          in
          (match receipt with
           | Error { code = Permission_denied; _ } -> ()
           | _ -> failwith "revoked receipt allowed");
          (match C.Session_handle.seed client request with
           | Error { code = Permission_denied; _ } -> ()
           | _ -> failwith "revoked replay allowed");
          print_endline
            "original pending intent -> exact Created receipt -> same identity; revoked \
             reads and replay denied")));
  [%expect
    {| original pending intent -> exact Created receipt -> same identity; revoked reads and replay denied |}]
;;

let%expect_test
    "Pending seed close preserves real first Created and startup recovers original \
     identity"
  =
  with_root (fun env ~root ~prompt_file:_ config ->
    let obstruction = ref None in
    let active_request = ref None in
    let contention = ref None in
    let fault_env =
      Receipt_fault.wrap env ~obstruct:(fun path ->
        if Option.is_none !obstruction
        then (
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / path);
          obstruction := Some path;
          Option.iter !active_request ~f:(fun (sw, daemon, request) ->
            let result, resolve = Eio.Promise.create () in
            contention := Some result;
            Eio.Fiber.fork ~sw (fun () ->
              let client = connection daemon (principal ()) in
              Exn.protect
                ~finally:(fun () -> C.Connection.close client)
                ~f:(fun () ->
                  initialize client;
                  Eio.Promise.resolve resolve (C.Session_handle.seed client request)));
            (* Competing original request starts during real receipt completion. *)
            Eio.Fiber.yield ())))
    in
    let original = ref None in
    let first_created = ref None in
    let created_id = ref None in
    Eio.Switch.run (fun sw ->
      let defaults = S.Daemon.default_options.factory_limits in
      let daemon =
        start
          ~factory_limits:{ defaults with max_segment_frames = 1 }
          sw
          fault_env
          ~root
          config
      in
      let client = connection daemon (principal ()) in
      initialize client;
      Exn.protect
        ~finally:(fun () ->
          C.Connection.close client;
          S.Daemon.shutdown daemon |> protocol_ok)
        ~f:(fun () ->
          (* Large valid literal under unchanged default 16 MiB request/Created
             ceilings; exact pure admission threshold checks remain separate. *)
          let large =
            transcript
              ~additional_text:(String.make ((16 * 1024 * 1024) - (128 * 1024)) 'x')
              ()
          in
          let request =
            request ~code:(catalog_code client) ~key:"recover-real-pending" large
          in
          active_request := Some (sw, daemon, request);
          original := Some request;
          (match C.Session_handle.seed client request with
           | Error { code = Interrupted; _ } -> ()
           | Error _ | Ok _ ->
             failwith "real terminal receipt obstruction did not retain Pending");
          assert (Option.is_some !obstruction);
          (match Eio.Promise.await (Option.value_exn !contention) with
           | Error { code = Interrupted; _ } -> ()
           | Error _ | Ok _ ->
             failwith "active request duplicated or acknowledged creation");
          let store = S.Daemon.store daemon in
          let indexed =
            Agent_store.Session_index.list_checked
              (Agent_store.Session_store.session_index store)
            |> store_ok
          in
          let id =
            match indexed with
            | [ entry ] -> entry.session.id
            | _ -> failwith "one unpublished seed identity"
          in
          created_id := Some id;
          assert (Option.is_none (S.Session_registry.find (S.Daemon.registry daemon) id));
          let directory =
            Filename.concat
              (Agent_store.Data_root.session_path
                 (Agent_store.Session_store.data_root store)
                 id)
              "journal"
          in
          let reader =
            Agent_store.Retention_reader.create
              ~env
              ~root:directory
              ~max_entries:100
              ~max_bytes:(32 * 1024 * 1024)
            |> store_ok
          in
          let frame =
            Agent_store.Journal.initial_entry
              reader
              ~env
              ~max_payload_length:defaults.max_journal_payload
              ~checkpoint:Eio.Fiber.check
            |> store_ok
            |> Option.value_exn
          in
          assert (
            String.length (Agent_store.Frame.payload frame.frame)
            > (16 * 1024 * 1024) - (128 * 1024));
          first_created := Some (Agent_store.Frame.payload frame.frame)));
    let id = Option.value_exn !created_id in
    let data_root = Filename.concat root "data" in
    let journal =
      Filename.concat
        (Filename.concat
           (Filename.concat data_root "sessions")
           (P.Id.Session.to_string id))
        "journal"
    in
    let reader =
      Agent_store.Retention_reader.create
        ~env
        ~root:journal
        ~max_entries:100
        ~max_bytes:(32 * 1024 * 1024)
      |> store_ok
    in
    let frame =
      Agent_store.Journal.initial_entry
        reader
        ~env
        ~max_payload_length:S.Daemon.default_options.factory_limits.max_journal_payload
        ~checkpoint:Eio.Fiber.check
      |> store_ok
      |> Option.value_exn
    in
    assert (
      String.equal
        (Agent_store.Frame.payload frame.frame)
        (Option.value_exn !first_created));
    Eio.Path.rmdir Eio.Path.(Eio.Stdenv.fs env / Option.value_exn !obstruction);
    with_host env ~root config (fun _ daemon client ->
      let original = Option.value_exn !original in
      let replay = C.Session_handle.seed client original |> protocol_ok in
      assert (P.Id.Session.equal replay.session_id id);
      ignore
        (assert_stopped daemon replay (P.Session_seed.Request.transcript original)
         : A.Session_state.t);
      let indexed =
        Agent_store.Session_index.list_checked
          (Agent_store.Session_store.session_index (S.Daemon.store daemon))
        |> store_ok
      in
      assert (Int.equal (List.length indexed) 1);
      print_endline
        "large admitted seed; active replay stays Pending; close retains Created; \
         default startup completes original Success once"));
  [%expect
    {| large admitted seed; active replay stays Pending; close retains Created; default startup completes original Success once |}]
;;

let%expect_test
    "retained authored seed uses frozen code when current definition source is \
     unavailable"
  =
  with_root (fun env ~root ~prompt_file config ->
    let selected = ref None in
    let transcript = transcript () in
    with_host env ~root config (fun _ daemon client ->
      let seeded =
        C.Session_handle.seed
          client
          (request ~code:(catalog_code client) ~key:"retain-before-removal" transcript)
        |> protocol_ok
      in
      let state = assert_stopped daemon seeded transcript in
      selected := Some (expected daemon state, state.spec.prompt_revision_id));
    Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / prompt_file);
    with_host env ~root config (fun _ daemon client ->
      let current =
        C.Connection.request_without_history
          client
          (Prompt_get
             { prompt_id = S.Catalog_identity.prompt_definition "restart.prompt" })
        |> protocol_ok
      in
      (match current with
       | Prompt_get { availability = Unavailable _; _ } -> ()
       | Prompt_get { availability = Available; _ } | _ ->
         failwith "missing original was recaptured");
      let previous, revision = Option.value_exn !selected in
      let seeded =
        C.Session_handle.seed
          client
          (request ~code:(Retained previous) ~key:"retained-after-removal" transcript)
        |> protocol_ok
      in
      let state = assert_stopped daemon seeded transcript in
      assert (P.Id.Prompt_revision.equal state.spec.prompt_revision_id revision);
      assert (
        not
          (P.Id.Session.equal
             seeded.session_id
             (P.Session_ref.session_id (P.Session_lifecycle.Expected.reference previous))));
      print_endline
        "unavailable current source; enabled metadata and exact retained policy admit \
         old code into new stopped identity"));
  [%expect
    {| unavailable current source; enabled metadata and exact retained policy admit old code into new stopped identity |}]
;;
