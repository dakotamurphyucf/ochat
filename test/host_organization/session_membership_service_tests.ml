open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module V = P.Session_organization.Values
module Patch = P.Session_organization.Patch

let key value = P.Idempotency_key.of_string value |> protocol_ok

let status = function
  | Ok _ -> "ok"
  | Error (error : P.Error.t) -> P.Error.code_to_string error.code
;;

let owner_scopes =
  P.Scope.Set.of_list
    [ View_organization
    ; Manage_organization
    ; Create_sessions
    ; View_session_transcript
    ; Send_messages
    ]
;;

let owner = principal_with_scopes "pri_membership_rpc" owner_scopes

let query ?cursor ?(organization = P.Session_organization.Query.default) () =
  P.Session.List_request.
    { page = P.Page.Request.create ~limit:1 ?cursor () |> protocol_ok
    ; organization
    ; desired_state = None
    ; prompt_id = None
    ; workspace_id = None
    ; owner_principal_id = None
    ; creator_principal_id = None
    ; active_owner_principal_id = None
    ; labels = []
    ; sort = { field = Created_at; direction = Ascending }
    ; archive = All
    }
;;

let result client command =
  C.Connection.request_without_history client command |> protocol_ok
;;

let edit client request =
  match result client (Session_update_organization request) with
  | Session_update_organization value -> value
  | _ -> failwith "unexpected membership result"
;;

let receipt client command =
  C.Connection.request_without_history
    client
    (Command_receipt
       { method_name = P.Command.method_name command
       ; original_params = P.Command.params command
       })
;;

let%expect_test
    "actual membership RPC shares metadata CAS, receipts and tombstone paging across \
     restart"
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
          "<developer>Membership fixture.</developer>";
        let config = config root workspace prompt_file in
        let session_id, original, original_command =
          Eio.Switch.run (fun sw ->
            let daemon = Organization_service_tests.start sw env ~root config in
            let client = connection daemon owner in
            let host_id = (Organization_service_tests.initialize_host client).server_id in
            let create =
              P.Organization_request.Create.
                { host_id
                ; name = P.Organization_group.Name.create "Membership" |> protocol_ok
                ; idempotency_key = key "membership-project"
                }
            in
            let project = C.Admin.create_project client create |> protocol_ok in
            let collection =
              C.Admin.create_collection
                client
                { create with idempotency_key = key "membership-collection" }
              |> protocol_ok
            in
            let session, attachment = create_session ~key:"membership-session" client in
            let second, second_attachment =
              create_session ~key:"membership-second-session" client
            in
            let other_project =
              C.Admin.create_project
                client
                { create with idempotency_key = key "membership-other-project" }
              |> protocol_ok
            in
            let second_patch =
              Patch.create
                ~project:(Set other_project.id)
                ~add_collections:[ collection.id ]
                ~remove_collections:[]
              |> protocol_ok
            in
            ignore
              (edit
                 client
                 P.Session_organization.Request.
                   { host_id
                   ; session_id = second.id
                   ; attachment_id = second_attachment.id
                   ; expected_metadata_revision = 0L
                   ; patch = second_patch
                   ; idempotency_key = key "membership-other-project-edit"
                   });
            let patch =
              Patch.create
                ~project:(Set project.id)
                ~add_collections:[ collection.id ]
                ~remove_collections:[]
              |> protocol_ok
            in
            let request =
              P.Session_organization.Request.
                { host_id
                ; session_id = session.id
                ; attachment_id = attachment.id
                ; expected_metadata_revision = 0L
                ; patch
                ; idempotency_key = key "membership-edit"
                }
            in
            let original = edit client request in
            let name_patch =
              P.Session_metadata.Patch.create
                ~name:(Set "Renamed")
                ~set_labels:[]
                ~remove_labels:[]
              |> protocol_ok
            in
            ignore
              (result
                 client
                 (Session_update_metadata
                    { session_id = session.id
                    ; attachment_id = attachment.id
                    ; expected_metadata_revision = 1L
                    ; patch = name_patch
                    ; idempotency_key = key "membership-name"
                    }));
            let retry = edit client request in
            print_s
              [%sexp
                (Int64.equal original.session.metadata_revision 1L : bool)
              , (V.equal original.session.organization retry.session.organization : bool)
              , (Int64.equal retry.session.metadata_revision 1L : bool)];
            print_endline
              (status
                 (C.Connection.request_without_history
                    client
                    (Session_update_organization
                       { request with idempotency_key = key "membership-stale" })));
            let page = C.Admin.list_sessions_page client (query ()) |> protocol_ok in
            let cursor = Option.value_exn page.next_cursor in
            ignore
              (C.Admin.delete_project
                 client
                 { host_id
                 ; id = project.id
                 ; expected_revision = 0L
                 ; idempotency_key = key "membership-project-delete"
                 }
               |> protocol_ok);
            print_endline (status (C.Admin.list_sessions_page client (query ~cursor ())));
            let all =
              C.Admin.enumerate_sessions
                client
                ~query:(query ())
                ~max_sessions:10
                ~max_pages:10
              |> protocol_ok
            in
            let entry =
              List.find_exn all ~f:(fun entry ->
                P.Id.Session.equal entry.P.Session_catalog.session.id session.id)
            in
            print_s
              [%sexp
                (Option.equal
                   P.Id.Project.equal
                   entry.session.organization.project_id
                   (Some project.id)
                 : bool)
              , (Option.is_none entry.effective_organization.project_id : bool)
              , (List.equal
                   P.Id.Collection.equal
                   entry.effective_organization.collection_ids
                   [ collection.id ]
                 : bool)
              , ((match entry.session.observed_state with
                  | Stopped -> true
                  | Queued_for_slot
                  | Starting
                  | Recovering
                  | Idle
                  | Running_turn _
                  | Compacting _
                  | Waiting_for_permission _
                  | Stopping
                  | Failed _ -> false)
                 : bool)];
            let collection_filter =
              P.Session_organization.Query.create
                ~project:Any
                ~collection_all_of:[ collection.id ]
              |> protocol_ok
            in
            let shared =
              C.Admin.enumerate_sessions
                client
                ~query:(query ~organization:collection_filter ())
                ~max_sessions:10
                ~max_pages:10
              |> protocol_ok
            in
            print_s [%sexp (Int.equal (List.length shared) 2 : bool)];
            let no_view =
              connection
                daemon
                (principal_with_scopes
                   "pri_membership_rpc"
                   (Set.remove owner_scopes P.Scope.View_organization))
            in
            initialize no_view;
            let redacted =
              C.Admin.enumerate_sessions
                no_view
                ~query:(query ())
                ~max_sessions:10
                ~max_pages:10
              |> protocol_ok
            in
            print_s
              [%sexp
                (List.for_all redacted ~f:(fun entry ->
                   V.equal entry.P.Session_catalog.effective_organization V.empty)
                 : bool)];
            print_endline
              (status
                 (C.Admin.list_sessions_page
                    no_view
                    (query ~organization:collection_filter ())));
            C.Connection.close no_view;
            let lost_reply = P.Command.Session_update_organization request in
            C.Connection.close client;
            let fresh = connection daemon owner in
            initialize fresh;
            print_s
              [%sexp
                ((match receipt fresh lost_reply with
                  | Ok (Command_receipt (Committed (Session_mutation { session_id; _ })))
                    -> P.Id.Session.equal session_id session.id
                  | _ -> false)
                 : bool)];
            let reduced =
              principal_with_scopes
                "pri_membership_rpc"
                (Set.remove owner_scopes P.Scope.Manage_organization)
            in
            let denied = connection daemon reduced in
            initialize denied;
            print_endline (status (receipt denied lost_reply));
            C.Connection.close denied;
            C.Connection.close fresh;
            Agent_server.Daemon.shutdown daemon |> protocol_ok;
            session.id, original, lost_reply)
        in
        Eio.Switch.run (fun sw ->
          let daemon = Organization_service_tests.start sw env ~root config in
          let fresh = connection daemon owner in
          initialize fresh;
          let sessions =
            C.Admin.enumerate_sessions
              fresh
              ~query:(query ())
              ~max_sessions:10
              ~max_pages:10
            |> protocol_ok
          in
          let entry =
            List.find_exn sessions ~f:(fun entry ->
              P.Id.Session.equal entry.P.Session_catalog.session.id session_id)
          in
          print_s
            [%sexp
              (Int64.equal entry.session.metadata_revision 2L : bool)
            , (V.equal entry.session.organization original.session.organization : bool)
            , (Option.is_none entry.effective_organization.project_id : bool)
            , ((match receipt fresh original_command with
                | Ok (Command_receipt (Committed _)) -> true
                | _ -> false)
               : bool)];
          C.Connection.close fresh;
          Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {|
    (true true true)
    conflict
    conflict
    (true true true true)
    true
    true
    permission_denied
    true
    permission_denied
    (true true true true)
    |}]
;;
