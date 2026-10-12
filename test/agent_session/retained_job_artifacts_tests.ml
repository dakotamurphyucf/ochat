open Core
open Fixtures
module P = Agent_protocol
module B = Chat_response.Background_request
module C = Chat_response.Tool_capability
module State = Agent_session.Session_state
module Archive = Agent_session.Compaction_archive
module Store = Agent_store.Session_store
module Daemon = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module Retained = Agent_server.Retained_job_artifacts
module Support = Agent_server_test_support

let cap = function
  | Ok value -> value
  | Error error -> failwith error.C.message
;;

let selected revision =
  B.Agent_artifact.create
    ~revision_id:revision
    ~manifest_sha256:
      (Chatml_source.Digest.of_contents "complete manifest" ~checkpoint:ignore)
    ~root_relative_path:
      (Source.Portable_path.create "nested/child.chatmd" |> Or_error.ok_exn)
;;

let requests () =
  let module Native = struct
    type input = string

    let name = "child"
    let description = None
    let type_ = "function"
    let parameters = `True
    let input_of_string = Fn.id
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Native)
      (fun _ -> failwith "unexpected dispatch")
  in
  let registry =
    C.create
      ~owner:"retained-fixture"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
      [ Chatmd_shell_spec.Source_ref.digest "child-v1", implementation ]
    |> cap
  in
  let binding = C.find registry ~name:"child" |> cap in
  B.tool
    ~capabilities:registry
    ~reference:(C.reference binding)
    ~input:`Null
    ~policy:Chat_response.One_off_request.default_policy
  |> protocol_ok
;;

let job state ~request ~status =
  P.Job.
    { id = P.Id.Job.create ()
    ; session_id = state.State.identity.session_id
    ; generation = state.identity.generation
    ; kind = Async_tool
    ; payload = B.to_json request
    ; status
    ; retry_policy = Never
    ; attempt = 1
    ; created_at = timestamp
    ; started_at = Some timestamp
    ; next_run_at = None
    ; completed_at = Some timestamp
    ; result = None
    ; delivery = Not_required
    ; launch = None
    ; progress = None
    }
;;

let limits ?(sessions = 1) ?(archives = 1) ?(jobs = 2) ?(work = 100_000) () =
  Retained.Limits.create
    ~max_sessions:sessions
    ~max_archives:archives
    ~max_jobs:jobs
    ~max_work_steps:work
  |> protocol_ok
;;

let with_inventory f =
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      let home = workspace.canonical_root.native_path in
      let root = Filename.concat home "root.chatmd" in
      Eio.Path.save
        Eio.Path.(Eio.Stdenv.fs env / root)
        ~create:(`Exclusive 0o600)
        "<developer>retention fixture</developer>";
      let daemon =
        Daemon.start
          ~sw
          ~env
          ~config:(Support.config home home root)
          ~tool_dir:home
          ~home
          ~process_start_identity:None
          ~options:{ Daemon.default_options with startup_mode = Operator_only }
          ()
        |> protocol_ok
      in
      Exn.protect
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () -> Daemon.shutdown daemon |> protocol_ok))
        ~f:(fun () ->
          let original =
            actor_state
              ~workspace_instance:workspace
              ~liveness:Detached
              ~start_immediately:false
          in
          let old_revision = original.spec.prompt_revision_id in
          let current_revision = P.Id.Prompt_revision.create () in
          let selected_revision = P.Id.Prompt_revision.create () in
          let v1 = requests () in
          let v2 = B.with_agent_artifact v1 (selected selected_revision) |> protocol_ok in
          let historical =
            { original with jobs = [ job original ~request:v2 ~status:Succeeded ] }
          in
          let reference =
            Archive.reference_for
              (Agent_session.Session_state_document.authored historical)
              ~limits:document_limits
              ~kind:Edit
              (P.Id.Operation.create ())
            |> protocol_ok
          in
          let current =
            { original with
              spec = { original.spec with prompt_revision_id = current_revision }
            ; jobs = [ job original ~request:v1 ~status:Cancelled ]
            ; conversation =
                { original.conversation with compaction_archives = [ reference ] }
            }
          in
          let store = Daemon.store daemon in
          let archive_path =
            Eio.Switch.run (fun stored_sw ->
              let handle =
                Store.create_session
                  store
                  ~sw:stored_sw
                  ~transaction_id:(P.Id.Transaction.create ())
                  ~actor_lock_nonce:"retained-fixture"
                  { schema_version = Store.current_metadata_schema_version
                  ; session = State.summary current
                  ; prompt_artifact = P.Id.Prompt_revision.to_string current_revision
                  ; workspace_identity = workspace.conflict_domain
                  ; data_schema_version = State.current_schema_version
                  }
                |> store_ok
              in
              Exn.protect
                ~finally:(fun () -> Store.close_session store handle |> store_ok)
                ~f:(fun () ->
                  ignore
                    (Agent_store.Journal.create
                       ~env
                       ~directory:(Store.Handle.journal_directory handle)
                       ~max_payload_length:1048576
                       ~max_segment_bytes:1048576L
                       ~max_segment_frames:1024
                     |> store_ok
                     : Agent_store.Journal.t);
                  Archive.write
                    ~env
                    ~handle
                    ~max_payload_length:1048576
                    reference
                    (Agent_session.Session_state_document.authored historical)
                  |> protocol_ok;
                  ignore
                    (Agent_store.Snapshot.install
                       ~env
                       ~directory:(Store.Handle.snapshot_directory handle)
                       ~max_payload_length:1048576
                       (snapshot_of_state current |> store_ok)
                     |> store_ok
                     : Agent_store.Snapshot.installed);
                  Filename.concat
                    (Store.Handle.archive_directory handle)
                    (Archive.filename reference)))
          in
          let entries = Store.list_sessions_checked store |> store_ok in
          Registry.index_all (Daemon.registry daemon) entries;
          f
            env
            daemon
            entries
            archive_path
            [ old_revision; current_revision; selected_revision ])))
;;

let collect daemon entries ?(checkpoint = ignore) ?(authorize = fun _ -> Ok ()) limits =
  Retained.collect
    ~factory:(Daemon.factory daemon)
    ~entries
    ~authorize
    ~request_policy:Chat_response.One_off_request.default_policy
    ~limits
    ~checkpoint
;;

let rejection result =
  match result with
  | Ok _ -> failwith "expected rejection"
  | Error error -> error.P.Error.code
;;

let%expect_test "indexed retention covers terminal jobs and archives under one allowance" =
  with_inventory (fun env daemon entries archive_path expected ->
    let result = collect daemon entries (limits ()) |> protocol_ok in
    List.iter expected ~f:(fun id ->
      assert (List.mem (Retained.revisions result) id ~equal:P.Id.Prompt_revision.equal));
    assert (List.length (Retained.revisions result) = 3);
    let receipt = Retained.receipt result in
    assert (
      Retained.Receipt.sessions receipt = 1
      && Retained.Receipt.archives receipt = 1
      && Retained.Receipt.jobs receipt = 2);
    let work = Retained.Receipt.work_steps receipt in
    ignore (collect daemon entries (limits ~work ()) |> protocol_ok : Retained.t);
    [%test_eq: P.Error.code]
      Resource_limit
      (rejection (collect daemon entries (limits ~work:(work - 1) ())));
    [%test_eq: P.Error.code]
      Resource_limit
      (rejection (collect daemon entries (limits ~jobs:1 ())));
    let original = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / archive_path) in
    Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / archive_path);
    [%test_eq: P.Error.code]
      Resource_limit
      (rejection (collect daemon entries (limits ~archives:0 ())));
    assert (Result.is_error (collect daemon entries (limits ())));
    Eio.Path.save
      Eio.Path.(Eio.Stdenv.fs env / archive_path)
      ~create:(`Exclusive 0o600)
      original;
    ignore (collect daemon entries (limits ()) |> protocol_ok : Retained.t);
    assert (List.is_empty (Registry.entries (Daemon.registry daemon)));
    print_endline
      "three revisions, two terminal jobs; exact work passes, incomplete inventory \
       rejects");
  [%expect
    {| three revisions, two terminal jobs; exact work passes, incomplete inventory rejects |}]
;;

let%expect_test "retention cleanup preserves cancellation and refuses an installed loader"
  =
  with_inventory (fun _env daemon entries _path _expected ->
    let failure = Failure "original retention checkpoint" in
    let same =
      try
        ignore (collect daemon entries ~checkpoint:(fun () -> raise failure) (limits ()));
        false
      with
      | exn -> phys_equal failure exn
    in
    assert same;
    let cancelled =
      Eio.Cancel.sub (fun context ->
        let observed = ref None in
        try
          ignore
            (collect
               daemon
               entries
               ~checkpoint:(fun () ->
                 Eio.Cancel.cancel context Exit;
                 try Eio.Fiber.check () with
                 | exn ->
                   observed := Some exn;
                   raise exn)
               (limits ()));
          false
        with
        | exn -> Option.exists !observed ~f:(phys_equal exn))
    in
    assert cancelled;
    ignore (collect daemon entries (limits ()) |> protocol_ok : Retained.t);
    Registry.install_loader (Daemon.registry daemon) (fun _ ->
      failwith "retention must not load");
    let authorizations = ref 0 in
    assert (
      Result.is_error
        (collect
           daemon
           entries
           ~authorize:(fun _ ->
             incr authorizations;
             Ok ())
           (limits ())));
    assert (!authorizations = 0);
    print_endline
      "original exceptions preserved; leases released; loader blocks collection before \
       authorization");
  [%expect
    {| original exceptions preserved; leases released; loader blocks collection before authorization |}]
;;
