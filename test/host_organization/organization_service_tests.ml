open Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let key value = P.Idempotency_key.of_string value |> protocol_ok
let name value = P.Organization_group.Name.create value |> protocol_ok

let status = function
  | Ok _ -> "ok"
  | Error (error : P.Error.t) -> P.Error.code_to_string error.code
;;

let organization_principal id =
  P.Principal.create
    ~id:(P.Id.Principal.of_string id |> protocol_ok)
    ~authentication_kind:"test"
    ~scopes:(P.Scope.Set.of_list [ View_organization; Manage_organization ])
    ~attributes:[]
  |> protocol_ok
;;

let initialize_host connection =
  C.Session_handle.initialize
    connection
    ~implementation_name:"organization-fixture"
    ~implementation_version:"dev"
  |> protocol_ok
;;

let start sw env ~root config =
  Agent_server.Daemon.start
    ~options:
      { Agent_server.Daemon.default_options with
        inference_policy =
          inference_policy
            ~default_model:"fixture-model"
            ~post_stream:(fun ~sw:_ ~inputs:_ ->
              failwith "organization activated execution")
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

let%expect_test "actual host CRUD, page authority and durable exact receipts" =
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
          "<developer>Organization fixture.</developer>";
        let config = config root workspace prompt_file in
        let original =
          Eio.Switch.run (fun sw ->
            let daemon = start sw env ~root config in
            let client =
              connection daemon (organization_principal "pri_organization_owner")
            in
            let initialized = initialize_host client in
            let host_id = initialized.server_id in
            let create =
              P.Organization_request.Create.
                { host_id
                ; name = name "First"
                ; idempotency_key = key "organization-create"
                }
            in
            let first = C.Admin.create_project client create |> protocol_ok in
            ignore
              (C.Admin.create_project
                 client
                 { create with
                   name = name "Second"
                 ; idempotency_key = key "organization-second"
                 }
               |> protocol_ok
               : P.Organization_group.Project.t);
            let query =
              P.Organization_request.List.
                { host_id
                ; creator_principal_id = None
                ; page = P.Page.Request.create ~limit:1 () |> protocol_ok
                }
            in
            let page = C.Admin.list_projects_page client query |> protocol_ok in
            print_s
              [%sexp
                (List.length page.items : int), (Option.is_some page.next_cursor : bool)];
            let other =
              connection daemon (organization_principal "pri_organization_other")
            in
            ignore (initialize_host other : P.Initialize.Response.t);
            print_endline
              (status
                 (C.Admin.get_project
                    other
                    P.Organization_request.Project.Get.{ host_id; id = first.id }));
            let foreign =
              C.Admin.enumerate_projects other ~query ~max_groups:4 ~max_pages:4
              |> protocol_ok
            in
            print_s [%sexp (List.length foreign : int)];
            let rename =
              P.Organization_request.Project.Update.
                { host_id
                ; id = first.id
                ; expected_revision = 0L
                ; name = name "Saved"
                ; idempotency_key = key "organization-rename"
                }
            in
            let saved = C.Admin.update_project client rename |> protocol_ok in
            let retry = C.Admin.update_project client rename |> protocol_ok in
            print_s
              [%sexp
                (P.Organization_group.Project.equal saved retry : bool)
              , (saved.revision : int64)];
            print_endline
              (status
                 (C.Admin.update_project client { rename with name = name "Different" }));
            print_endline
              (status
                 (C.Admin.list_projects_page
                    client
                    { query with page = { query.page with cursor = page.next_cursor } }));
            print_endline
              (status
                 (C.Admin.enumerate_projects client ~query ~max_groups:1 ~max_pages:4));
            let collection =
              C.Admin.create_collection
                client
                { create with
                  name = name "Cross project collection"
                ; idempotency_key = key "organization-collection"
                }
              |> protocol_ok
            in
            print_s [%sexp (collection.revision : int64)];
            C.Connection.close other;
            C.Connection.close client;
            Agent_server.Daemon.shutdown daemon |> protocol_ok;
            host_id, first.id, rename, saved)
        in
        Eio.Switch.run (fun sw ->
          let daemon = start sw env ~root config in
          let client =
            connection daemon (organization_principal "pri_organization_owner")
          in
          ignore (initialize_host client : P.Initialize.Response.t);
          let host_id, id, rename, saved = original in
          let current =
            C.Admin.get_project client P.Organization_request.Project.Get.{ host_id; id }
            |> protocol_ok
          in
          print_s [%sexp (P.Organization_group.Project.equal current saved : bool)];
          let receipt =
            C.Connection.request_without_history
              client
              (Command_receipt
                 { method_name = "project.update"
                 ; original_params = P.Organization_request.Project.Update.to_json rename
                 })
            |> protocol_ok
          in
          print_s
            [%sexp
              ((match receipt with
                | Command_receipt (Committed (Project_mutation { project_id; revision }))
                  -> P.Id.Project.equal project_id id && Int64.equal revision 1L
                | _ -> false)
               : bool)];
          let deletion =
            P.Organization_request.Project.Delete.
              { host_id
              ; id
              ; expected_revision = 1L
              ; idempotency_key = key "organization-delete"
              }
          in
          let deleted = C.Admin.delete_project client deletion |> protocol_ok in
          let retry = C.Admin.delete_project client deletion |> protocol_ok in
          print_s
            [%sexp (P.Organization_result.Project_deleted.equal deleted retry : bool)];
          print_endline
            (status
               (C.Admin.get_project
                  client
                  P.Organization_request.Project.Get.{ host_id; id }));
          C.Connection.close client;
          Agent_server.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {|
    (1 true)
    organization_not_found
    0
    (true 1)
    idempotency_conflict
    conflict
    invalid_request
    0
    true
    true
    true
    organization_not_found
    |}]
;;

exception Startup_primary_failure

let%expect_test "daemon startup failure releases store and acquired operator immediately" =
  List.iter
    [ `Before_activation; `Composition_error; `Composition_exception ]
    ~f:(fun cut ->
      Eio_main.run (fun env ->
        Mirage_crypto_rng_unix.use_default ();
        let root = temporary_root env in
        Exn.protect
          ~finally:(fun () ->
            Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
          ~f:(fun () ->
            Eio.Switch.run (fun sw ->
              let workspace = Filename.concat root "workspace" in
              Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
              let prompt_file = Filename.concat root "root.chatmd" in
              Eio.Path.save
                ~create:(`Exclusive 0o600)
                Eio.Path.(Eio.Stdenv.fs env / prompt_file)
                "<developer>Startup ownership fixture.</developer>";
              let configuration = config root workspace prompt_file in
              let acquired = ref 0
              and closed = ref 0
              and evaluator_calls = ref 0 in
              let factory ~sw:_ ~server_id:_ =
                Int.incr acquired;
                Ok
                  (Agent_server.Provider_operator_port.create
                     ~dispatch:(fun ~actor:_ _ ->
                       failwith "operator dispatch unreachable")
                     ~receipt:(fun ~actor:_ _ -> failwith "operator receipt unreachable")
                     ~close:(fun () -> Int.incr closed))
              in
              let options =
                { Agent_server.Daemon.default_options with
                  provider_operator_factory = Some factory
                ; policy_evaluator_resolver =
                    (match cut with
                     | `Composition_exception ->
                       Some
                         (fun id ->
                           [%test_eq: string] "restart.permission" id;
                           Int.incr evaluator_calls;
                           raise Startup_primary_failure)
                     | `Before_activation | `Composition_error -> None)
                }
              in
              let invalid_configuration =
                match cut with
                | `Composition_error ->
                  { configuration with
                    workspaces = configuration.workspaces @ configuration.workspaces
                  }
                | `Composition_exception ->
                  { configuration with
                    permission_profiles =
                      List.map configuration.permission_profiles ~f:(fun profile ->
                        { profile with
                          tool_default = Agent_server.Config.Permission_profile.Policy
                        })
                  }
                | `Before_activation -> configuration
              in
              let caught =
                try
                  match
                    Agent_server.Daemon.start
                      ~options
                      ~sw
                      ~env
                      ~config:invalid_configuration
                      ~tool_dir:root
                      ~home:root
                      ~process_start_identity:None
                      ~before_activation:(fun _ ->
                        match cut with
                        | `Before_activation -> raise Startup_primary_failure
                        | `Composition_error | `Composition_exception -> Ok ())
                      ()
                  with
                  | Error error ->
                    (match cut, error.P.Error.code with
                     | `Composition_error, Journal_corrupt ->
                       [%test_eq: string]
                         "workspace catalog contains duplicate identities"
                         error.message;
                       true
                     | _ ->
                       raise_s [%sexp "unexpected startup outcome", (error : P.Error.t)])
                  | Ok _ -> false
                with
                | Startup_primary_failure ->
                  (match cut with
                   | `Before_activation | `Composition_exception -> true
                   | `Composition_error -> false)
              in
              assert caught;
              [%test_eq: int]
                (match cut with
                 | `Composition_exception -> 1
                 | _ -> 0)
                !evaluator_calls;
              let expected =
                match cut with
                | `Before_activation -> 0
                | _ -> 1
              in
              [%test_eq: int] expected !acquired;
              [%test_eq: int] expected !closed;
              let store =
                Agent_store.Session_store.open_existing
                  ~env
                  ~sw
                  ~root:configuration.server.data_dir
                  ~process_start_identity:None
                  ~lock_nonce:"after-daemon-startup-fault"
                |> store_ok
              in
              Agent_store.Session_store.close store |> store_ok))));
  print_endline
    "three startup outcomes preserved; operator closed once and store reacquired in the \
     same switch";
  [%expect
    {|
    three startup outcomes preserved; operator closed once and store reacquired in the same switch
    |}]
;;
