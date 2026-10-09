open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client

let query ?cursor ~archive () =
  P.Session.List_request.
    { organization = Agent_protocol.Session_organization.Query.default
    ; page = P.Page.Request.create ~limit:1 ?cursor () |> protocol_ok
    ; desired_state = None
    ; prompt_id = None
    ; workspace_id = None
    ; owner_principal_id = None
    ; creator_principal_id = None
    ; active_owner_principal_id = None
    ; labels = []
    ; sort = { field = Display_name; direction = Descending }
    ; archive
    }
;;

let status = function
  | Ok _ -> "ok"
  | Error (e : P.Error.t) -> P.Error.code_to_string e.code
;;

let name (entry : P.Session_catalog.t) =
  Option.value entry.session.spec.display_name ~default:"unnamed"
;;

let key text = P.Idempotency_key.of_string text |> protocol_ok

let edit_command
      (session : P.Session.t)
      (attachment : P.Session.Attachment.t)
      ~revision
      ~name
      ~key_text
  =
  P.Command.Session_update_metadata
    { session_id = session.id
    ; attachment_id = attachment.id
    ; expected_metadata_revision = revision
    ; patch =
        P.Session_metadata.Patch.create ~name:(Set name) ~set_labels:[] ~remove_labels:[]
        |> protocol_ok
    ; idempotency_key = key key_text
    }
;;

let edit client command = C.Connection.request_without_history client command

let mutation = function
  | P.Method_result.Session_update_metadata result -> result
  | _ -> failwith "metadata result"
;;

let start_catalog_daemon sw env ~root config =
  Agent_server.Daemon.start
    ~options:
      { Agent_server.Daemon.default_options with
        inference_policy =
          Agent_server_test_support.inference_policy
            ~default_model:"fixture-model"
            ~post_stream:(fun ~sw:_ ~inputs:_ ->
              failwith "catalog query activated provider")
      }
    ~sw
    ~env
    ~config
    ~tool_dir:root
    ~home:root
    ~process_start_identity:None
    ()
  |> protocol_ok
;;

let%expect_test
    "catalog services preserve selected order and receipts and expire changed cursors"
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
        let prompt_file = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Catalog fixture.</developer>";
        Eio.Switch.run (fun sw ->
          let daemon =
            start_catalog_daemon sw env ~root (config root workspace prompt_file)
          in
          let client = connection daemon (principal ()) in
          initialize client;
          let sessions =
            List.mapi [ "a"; "z"; "m" ] ~f:(fun i name ->
              let session, attachment =
                create_session ~key:(sprintf "catalog-create-%d" i) client
              in
              let command =
                edit_command
                  session
                  attachment
                  ~revision:0L
                  ~name
                  ~key_text:(sprintf "catalog-name-%d" i)
              in
              let result = edit client command |> protocol_ok |> mutation in
              let retried = edit client command |> protocol_ok |> mutation in
              if not (Int64.equal result.mutation.revision retried.mutation.revision)
              then failwith "retry repeated commit";
              result.session, attachment)
          in
          let q = query ~archive:Active () in
          let all =
            C.Admin.enumerate_sessions client ~query:q ~max_sessions:10 ~max_pages:10
            |> protocol_ok
          in
          print_s [%sexp (List.map all ~f:name : string list)];
          let isolated =
            connection
              daemon
              (principal_with_scopes
                 "pri_isolated_catalog"
                 (P.Scope.Set.of_list [ View_session_transcript; Send_messages ]))
          in
          initialize isolated;
          let hidden =
            C.Admin.enumerate_sessions isolated ~query:q ~max_sessions:10 ~max_pages:10
            |> protocol_ok
          in
          print_s [%sexp (List.length hidden : int)];
          let first_session, first_attachment = List.hd_exn sessions in
          print_endline
            (status
               (edit
                  isolated
                  (edit_command
                     first_session
                     first_attachment
                     ~revision:0L
                     ~name:"a"
                     ~key_text:"catalog-name-0")));
          C.Connection.close isolated;
          print_endline
            (status
               (C.Admin.enumerate_sessions client ~query:q ~max_sessions:10 ~max_pages:1));
          let page = C.Admin.list_sessions_page client q |> protocol_ok in
          let cursor = Option.value_exn page.next_cursor in
          let next = query ~cursor ~archive:Active () in
          let changed_query =
            { next with sort = { field = Display_name; direction = Ascending } }
          in
          print_endline (status (C.Admin.list_sessions_page client changed_query));
          let other = connection daemon (principal_with_id "pri_other_catalog") in
          initialize other;
          print_endline (status (C.Admin.list_sessions_page other next));
          let reduced =
            connection
              daemon
              (principal_with_scopes
                 "pri_restart_test"
                 (Core.Set.remove scopes P.Scope.Send_messages))
          in
          initialize reduced;
          print_endline (status (C.Admin.list_sessions_page reduced next));
          C.Connection.close reduced;
          let session, attachment = List.hd_exn sessions in
          ignore
            (edit
               client
               (edit_command
                  session
                  attachment
                  ~revision:1L
                  ~name:"b"
                  ~key_text:"catalog-name-changed")
             |> protocol_ok);
          print_endline (status (C.Admin.list_sessions_page client next));
          let conflicting =
            `Object
              [ "limit", `Number "1"
              ; "owner_principal_id", P.Id.Principal.to_json (principal ()).id
              ; ( "creator_principal_id"
                , P.Id.Principal.to_json (principal_with_id "pri_other_catalog").id )
              ]
          in
          print_endline (status (P.Session.List_request.of_json conflicting));
          let current =
            C.Admin.get_session client session.id
            |> protocol_ok
            |> P.Public.Snapshot.fields
          in
          ignore
            (C.Connection.request_without_history
               client
               (Session_delete
                  { session_id = session.id
                  ; attachment_id = attachment.id
                  ; expected_revision = current.revision
                  ; policy = Archive
                  ; confirmation = P.Id.Session.to_string session.id
                  ; idempotency_key = key "catalog-archive"
                  })
             |> protocol_ok);
          let before =
            Agent_server.Session_registry.stats (Agent_server.Daemon.registry daemon)
          in
          let archived =
            C.Admin.enumerate_sessions
              client
              ~query:(query ~archive:Archived ())
              ~max_sessions:10
              ~max_pages:10
            |> protocol_ok
          in
          let after =
            Agent_server.Session_registry.stats (Agent_server.Daemon.registry daemon)
          in
          print_s
            [%sexp
              { archived_names = (List.map archived ~f:name : string list)
              ; no_activation = (Int.equal before.loaded after.loaded : bool)
              }];
          C.Connection.close other;
          C.Connection.close client;
          Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {|
    (z m a)
    0
    permission_denied
    invalid_request
    invalid_request
    invalid_request
    invalid_request
    conflict
    invalid_request
    ((archived_names (b)) (no_activation true)) |}]
;;

let%expect_test
    "catalog ties use typed ascending IDs and ownership is not creator identity"
  =
  let timestamp = P.Timestamp.of_time_ns Time_ns.epoch in
  let creator = (principal_with_id "pri_creator_catalog").id in
  let owner = (principal_with_id "pri_owner_catalog").id in
  let entry id =
    P.Session_catalog.
      { effective_organization = Agent_protocol.Session_organization.Values.empty
      ; session =
          P.Session.
            { id = P.Id.Session.of_string id |> protocol_ok
            ; creator = Some creator
            ; created_at = timestamp
            ; updated_at = timestamp
            ; generation = 0
            ; spec = session_spec ()
            ; desired_state = Stopped
            ; observed_state = Stopped
            ; prompt_revision = None
            ; workspace_instance = None
            ; active_operation = None
            ; revision = 0L
            ; metadata_revision = 0L
            ; organization = Agent_protocol.Session_organization.Values.empty
            ; latest_event_sequence = 0L
            ; inference_summary = History_entry.Payload.Presence.Absent
            }
      ; active_owner_principal_id = Some owner
      ; archived = false
      ; lifecycle_revision = P.Session_lifecycle.Revision.zero
      ; admission = P.Session_lifecycle.Result.Admission.Automatic
      }
  in
  let a = entry "ses_catalog_a"
  and z = entry "ses_catalog_z" in
  List.iter [ P.Session_catalog_query.Sort.Ascending; Descending ] ~f:(fun direction ->
    print_s
      [%sexp
        (Agent_server.Session_catalog_policy.compare
           { field = Display_name; direction }
           a
           z
         < 0
         : bool)]);
  let expired = P.Timestamp.of_string "1970-01-01T00:00:01Z" |> protocol_ok in
  let now = P.Timestamp.of_string "1970-01-01T00:00:02Z" |> protocol_ok in
  let grace = P.Timestamp.of_string "1970-01-01T00:00:03Z" |> protocol_ok in
  let attachment mode grace =
    P.Session.Attachment.
      { id = P.Id.Attachment.of_string "att_catalog_owner" |> protocol_ok
      ; session_id = a.session.id
      ; mode
      ; owner_lease =
          Some
            { generation = 1L
            ; expires_at = expired
            ; disconnect_grace_until = grace
            ; principal_id = Some owner
            ; reclaim_token_sha256 = None
            }
      }
  in
  List.iter
    [ attachment Owner_read_write None
    ; attachment Owner_read_write (Some grace)
    ; attachment Read_only (Some grace)
    ]
    ~f:(fun attachment ->
      print_s
        [%sexp
          (Option.equal
             P.Id.Principal.equal
             (Agent_server.Session_catalog_policy.active_owner ~now [ attachment ])
             (Some owner)
           : bool)]);
  let base = query ~archive:Active () in
  let filter =
    { base with
      creator_principal_id = Some creator
    ; active_owner_principal_id = Some owner
    }
  in
  print_s [%sexp (Agent_server.Session_catalog_policy.matches filter a : bool)];
  [%expect
    {|
    true
    true
    false
    true
    false
    true |}]
;;

let%expect_test "durable restart retains metadata and read-only original mutation receipt"
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
        let prompt_file = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Durable catalog fixture.</developer>";
        let config = config root workspace prompt_file in
        let command, expected =
          Eio.Switch.run (fun sw ->
            let daemon = start_catalog_daemon sw env ~root config in
            let client = connection daemon (principal ()) in
            initialize client;
            let session, attachment =
              create_session ~key:"catalog-durable-create" client
            in
            let command =
              edit_command
                session
                attachment
                ~revision:0L
                ~name:"saved name"
                ~key_text:"catalog-durable-name"
            in
            let result = edit client command |> protocol_ok |> mutation in
            C.Connection.close client;
            Agent_server.Daemon.shutdown daemon |> protocol_ok;
            command, result)
        in
        Eio.Switch.run (fun sw ->
          let daemon = start_catalog_daemon sw env ~root config in
          let client = connection daemon (principal ()) in
          initialize client;
          let entries =
            C.Admin.enumerate_sessions
              client
              ~query:(query ~archive:Active ())
              ~max_sessions:10
              ~max_pages:10
            |> protocol_ok
          in
          let entry =
            List.find_exn entries ~f:(fun entry ->
              P.Id.Session.equal entry.P.Session_catalog.session.id expected.session.id)
          in
          print_s
            [%sexp
              { name = (entry.session.spec.display_name : string option)
              ; metadata_revision = (entry.session.metadata_revision : int64)
              }];
          let before =
            Agent_server.Session_registry.stats (Agent_server.Daemon.registry daemon)
          in
          let receipt =
            C.Connection.request_without_history
              client
              (Command_receipt
                 { method_name = P.Command.method_name command
                 ; original_params = P.Command.params command
                 })
            |> protocol_ok
          in
          let original =
            match receipt with
            | Command_receipt (Committed (Session_mutation { session_id; mutation })) ->
              P.Id.Session.equal session_id expected.session.id
              && Int64.equal mutation.revision expected.mutation.revision
            | _ -> false
          in
          let after =
            Agent_server.Session_registry.stats (Agent_server.Daemon.registry daemon)
          in
          print_s
            [%sexp
              { original_receipt = (original : bool)
              ; no_activation = (Int.equal before.loaded after.loaded : bool)
              }];
          C.Connection.close client;
          Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {|
    ((name ("saved name")) (metadata_revision 1))
    ((original_receipt true) (no_activation true)) |}]
;;
