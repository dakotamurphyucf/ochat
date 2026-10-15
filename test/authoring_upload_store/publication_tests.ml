open! Core
module Fixture = Authoring_upload_store_test
module P = Agent_protocol
module B = P.Authoring_bundle.Bundle
module Store = Agent_store.Authoring_upload_store
module Captured = Chatml_source
module Document = Chatml_authoring_bundle_document
module Markdown = Prompt.Chat_markdown
module Project_fixture = Chatml_project_build_tests.Authoring_bundle_tests
module Recomposition = Chatml_project_build_tests.Recomposition_tests
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Host = Agent_server.Authoring_upload_host
module Context_service = Agent_server.Authoring_context_service

let context_analysis_policy () =
  let semantic =
    Chatml_semantics.Limits.create
      ~max_input_bytes:1_000_000
      ~recording:
        (Chatml_typechecker.Semantic.Limits.create
           ~max_facts:20_000
           ~max_edges:40_000
           ~max_label_bytes:400_000
           ~max_work_steps:400_000
         |> Or_error.ok_exn)
      ~type_graph:
        (Chatml_semantics.Frozen.Limits.create
           ~max_nodes:20_000
           ~max_edges:40_000
           ~max_label_bytes:400_000
         |> Or_error.ok_exn)
      ~max_published_facts:20_000
      ~max_published_edges:40_000
      ~max_published_label_bytes:400_000
      ~max_work_steps:400_000
    |> Or_error.ok_exn
  in
  Chatml_authoring_analysis.Policy.create
    ~compilation:Chatml_compilation.default_limits
    ~semantic
    ~max_dependency_depth:16
    ~max_results:2
    ~max_projected_diagnostics:4
    ~max_projected_sites:12
    ~max_projection_sources:8
    ~max_position_index_entries:256
    ~max_projection_source_bytes:1_000_000
  |> Or_error.ok_exn
;;

let context_initialize ~server_id ~principal =
  P.Initialize.Response.create
    ~protocol_name:"ochat.agent"
    ~selected_version:(P.Version.create ~major:1 ~minor:0 |> Fixture.protocol_ok)
    ~implementation:
      (P.Initialize.Implementation.create ~name:"upload context test" ~version:"1"
       |> Fixture.protocol_ok)
    ~server_id
    ~enabled_features:[ P.Authoring.Features.core ]
    ~extensions:None
    ~principal
    ~limits:
      { max_request_bytes = 1_000_000
      ; max_event_bytes = 1024
      ; max_page_size = 100
      ; max_attachments_per_connection = 1
      }
    ~event_retention:
      { minimum_age_ms = 0; maximum_events = 1; oldest_replayable_sequence = None }
    ~timing:
      { heartbeat_interval_ms = 1000
      ; owner_lease_duration_ms = 10000
      ; owner_renew_after_ms = 5000
      ; disconnect_grace_default_ms = 0
      }
    ~server_time:Fixture.start
  |> Fixture.protocol_ok
;;

let host_ok = function
  | Ok value -> value
  | Error problem -> raise_s (Host.Error.sexp_of_t problem)
;;

let protocol_ok = Fixture.protocol_ok
let upload_ok = Fixture.upload_ok
let check = Fixture.check

let document_id =
  Captured.Id.create ~package_namespace:"docs" ~relative_path:"entry.chatmd"
  |> Or_error.ok_exn
;;

let document_bytes =
  {|<script id="main" language="chatml" kind="tool">let run = 0</script>|}
;;

let captured =
  Captured.create
    document_id
    ~contents:document_bytes
    ~expected_digest:None
    ~max_bytes:None
    ~checkpoint:Fn.id
  |> Or_error.ok_exn
;;

let document_manifest dir =
  let bundle =
    Chatmd_source_bundle.create
      ~root_file:"entry.chatmd"
      ~sources:[ "entry.chatmd", document_bytes ]
      ()
    |> Result.ok_or_failwith
  in
  let parsed = Markdown.parse_source_bundle ~dir bundle in
  let declaration = List.hd_exn (Markdown.Parsed.declarations parsed.parsed_root) in
  let binding =
    Chatml_prompt_binding.of_declaration parsed.parsed_root declaration ~checkpoint:Fn.id
    |> Or_error.ok_exn
  in
  let selector =
    B.Manifest.Script.Selector.create
      ~registration_source:document_id
      ~declaration_source:document_id
      ~start_byte:(Chatml_prompt_binding.start_byte binding)
      ~end_byte:(Chatml_prompt_binding.end_byte binding)
      ~script_id:(Chatml_prompt_binding.script_id binding)
      ~body_digest:(Chatml_prompt_binding.script_digest binding)
      ~body_byte_length:(String.length (Markdown.Declaration.script_body declaration))
    |> protocol_ok
  in
  let script = B.Manifest.Script.create ~selector ~choice:Source_only |> protocol_ok in
  let document_member =
    B.Manifest.Document_member.create
      ~source:document_id
      ~member:(Source.Portable_path.create "entry.chatmd" |> Or_error.ok_exn)
    |> protocol_ok
  in
  let basis =
    B.Manifest.Chatmd_document.create
      ~root_source:document_id
      ~document_members:[ document_member ]
      ~scripts:[ script ]
    |> protocol_ok
  in
  B.Manifest.create
    ~basis:(Chatmd_document basis)
    ~members:[ Fixture.member document_id document_bytes ]
  |> protocol_ok
;;

let validation_limits =
  Chatml_authoring_bundle_project.Limits.create
    ~max_work_steps:100000
    ~max_retained_input_bytes:1000000
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let%test_unit "session close waits for a genuine published upload read lease" =
  Fixture.with_root "authoring-session-read-lease" (fun env sw root _ ->
    let session =
      Agent_store.Session_store.create
        ~env
        ~sw
        ~root:
          (Filename.concat (Filename.dirname (Agent_store.Data_root.path root)) "session")
        ~server_id:Fixture.server_id
        ~process_start_identity:(Some "authoring-session-read-lease")
        ~lock_nonce:"authoring-session-read-lease"
      |> Fixture.store_ok
    in
    let child =
      Agent_store.Session_store.open_authoring_upload_store
        session
        ~sw
        ~limits:(Fixture.limits ())
        ~checkpoint:Fn.id
      |> upload_ok
    in
    let dir = Eio.Stdenv.fs env in
    let manifest = document_manifest dir in
    let begin_request =
      B.Begin.create_request
        ~idempotency_key:(P.Idempotency_key.of_string "joined-read-begin" |> protocol_ok)
        ~manifest
        ()
    in
    let upload_id =
      Store.begin_upload
        child
        ~principal:Fixture.principal
        ~request:begin_request
        ~now:(fun () -> Fixture.start)
        ~checkpoint:Fn.id
      |> upload_ok
      |> B.Begin.upload_id
    in
    let first = Int.min 64 (String.length document_bytes) in
    ignore
      (Fixture.send_chunk
         child
         upload_id
         document_id
         0
         (String.prefix document_bytes first)
       |> upload_ok
       : B.Chunk.response);
    if first < String.length document_bytes
    then
      ignore
        (Fixture.send_chunk
           child
           upload_id
           document_id
           first
           (String.drop_prefix document_bytes first)
         |> upload_ok
         : B.Chunk.response);
    let sealed =
      match
        Store.claim_commit
          child
          ~principal:Fixture.principal
          ~request:
            (B.Commit.create_request
               ~upload_id
               ~idempotency_key:
                 (P.Idempotency_key.of_string "joined-read-commit" |> protocol_ok))
          ~now:(fun () -> Fixture.start)
          ~checkpoint:Fn.id
        |> upload_ok
      with
      | Ready sealed -> sealed
      | Published _ | Rejected _ -> failwith "expected a ready publication"
    in
    let snapshot =
      Store.snapshot
        child
        ~principal:Fixture.principal
        sealed
        ~now:(fun () -> Fixture.start)
        ~checkpoint:Fn.id
      |> upload_ok
    in
    let validated =
      Document.validate
        manifest
        ~sources:(Store.Snapshot.sources snapshot)
        ~dir
        ~selected_projects:[]
        ~limits:validation_limits
        ~checkpoint:Fn.id
      |> Result.map_error ~f:(fun _ -> "valid source-only document rejected")
      |> Result.ok_or_failwith
    in
    let receipt =
      Store.publish
        child
        ~principal:Fixture.principal
        sealed
        ~snapshot
        ~proof:(Store.Proof.Chatmd_document validated)
        ~now:(fun () -> Fixture.start)
        ~checkpoint:Fn.id
        ~final_admission:Fn.id
      |> upload_ok
    in
    let entered, entered_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let reader_done, reader_done_u = Eio.Promise.create () in
    let close_done, close_done_u = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Store.with_published
        child
        ~principal:Fixture.principal
        ~bundle_id:(B.Receipt.bundle_id receipt)
        ~now:(fun () -> Fixture.start)
        ~checkpoint:Fn.id
        ~admit_metadata:(fun _ _ -> ())
        ~f:(fun _ ->
          Eio.Promise.resolve entered_u ();
          Eio.Promise.await release)
      |> upload_ok;
      Eio.Promise.resolve reader_done_u ());
    Eio.Promise.await entered;
    Eio.Fiber.fork ~sw (fun () ->
      Store.close_and_wait child |> upload_ok;
      Eio.Promise.resolve close_done_u ());
    let rec await_closed remaining =
      if remaining = 0 then failwith "upload close never stopped new admissions";
      match
        Store.status
          child
          ~principal:Fixture.principal
          ~upload_id
          ~now:(fun () -> Fixture.start)
          ~checkpoint:Fn.id
      with
      | Error Closed -> ()
      | Ok _ | Error _ ->
        Eio.Fiber.yield ();
        await_closed (remaining - 1)
    in
    await_closed 1000;
    assert (not (Store.fully_closed child));
    (match Agent_store.Session_store.close session with
     | Error (Locked _) -> ()
     | Ok () | Error _ -> failwith "parent released lock before published reader joined");
    Eio.Promise.resolve release_u ();
    Eio.Promise.await reader_done;
    Eio.Promise.await close_done;
    assert (Store.fully_closed child);
    Agent_store.Session_store.close session |> Fixture.store_ok)
;;

let%test_unit "published capture survives uncertain marker, restart and bounded borrow" =
  Fixture.with_root "ochat-upload-publication" (fun env sw root lock ->
    let dir = Eio.Stdenv.fs env in
    let store = Fixture.open_store env sw root lock in
    let manifest = document_manifest dir in
    let begin_key = P.Idempotency_key.of_string "publication-begin" |> protocol_ok in
    let commit_key = P.Idempotency_key.of_string "publication-commit" |> protocol_ok in
    let request = B.Begin.create_request ~idempotency_key:begin_key ~manifest () in
    let response =
      Store.begin_upload
        store
        ~principal:Fixture.principal
        ~request
        ~now:(fun () -> Fixture.start)
        ~checkpoint:Fn.id
      |> upload_ok
    in
    let upload_id = B.Begin.upload_id response in
    let first = Int.min 64 (String.length document_bytes) in
    Fixture.send_chunk store upload_id document_id 0 (String.prefix document_bytes first)
    |> upload_ok
    |> ignore;
    if first < String.length document_bytes
    then
      Fixture.send_chunk
        store
        upload_id
        document_id
        first
        (String.drop_prefix document_bytes first)
      |> upload_ok
      |> ignore;
    let commit = B.Commit.create_request ~upload_id ~idempotency_key:commit_key in
    let sealed =
      match
        Store.claim_commit
          store
          ~principal:Fixture.principal
          ~request:commit
          ~now:(fun () -> Fixture.start)
          ~checkpoint:Fn.id
        |> upload_ok
      with
      | Ready sealed -> sealed
      | Published _ | Rejected _ -> failwith "claim unexpectedly terminal"
    in
    let snapshot =
      Store.snapshot
        store
        ~principal:Fixture.principal
        sealed
        ~now:(fun () -> Fixture.start)
        ~checkpoint:Fn.id
      |> upload_ok
    in
    let validated =
      Document.validate
        manifest
        ~sources:(Store.Snapshot.sources snapshot)
        ~dir
        ~selected_projects:[]
        ~limits:validation_limits
        ~checkpoint:Fn.id
      |> Result.map_error ~f:(fun _ -> "valid source-only document rejected")
      |> Result.ok_or_failwith
    in
    let proof = Store.Proof.Chatmd_document validated in
    let sentinel = Failure "publication-fault-before-marker" in
    check
      "final admission exception remains original"
      (match
         Store.publish
           store
           ~principal:Fixture.principal
           sealed
           ~snapshot
           ~proof
           ~now:(fun () -> Fixture.start)
           ~checkpoint:Fn.id
           ~final_admission:(fun () -> raise sentinel)
       with
       | exception exn -> phys_equal exn sentinel
       | Ok _ | Error _ -> false);
    check
      "intent remains validating without a marker"
      (match
         Store.status
           store
           ~principal:Fixture.principal
           ~upload_id
           ~now:(fun () -> Fixture.start)
           ~checkpoint:Fn.id
         |> upload_ok
       with
       | Validating -> true
       | Open _ | Ready | Published _ | Rejected _ | Expired -> false);
    let sampled = ref 0 in
    check
      "fresh time rejects a marker after the original deadline"
      (match
         Store.publish
           store
           ~principal:Fixture.principal
           sealed
           ~snapshot
           ~proof
           ~now:(fun () ->
             incr sampled;
             if !sampled = 1 then Fixture.start else Fixture.later 121000)
           ~checkpoint:Fn.id
           ~final_admission:Fn.id
       with
       | Error Store.Error.Expired -> !sampled >= 2
       | Ok _ | Error _ -> false);
    let receipt =
      Store.publish
        store
        ~principal:Fixture.principal
        sealed
        ~snapshot
        ~proof
        ~now:(fun () -> Fixture.start)
        ~checkpoint:Fn.id
        ~final_admission:Fn.id
      |> upload_ok
    in
    let bundle_id = B.Receipt.bundle_id receipt in
    Store.close_and_wait store |> upload_ok;
    let reopened = Fixture.open_store env sw root lock in
    check
      "restart and same-key commit retain exact receipt"
      (match
         Store.claim_commit
           reopened
           ~principal:Fixture.principal
           ~request:commit
           ~now:(fun () -> Fixture.start)
           ~checkpoint:Fn.id
         |> upload_ok
       with
       | Published replay -> B.Receipt.equal receipt replay
       | Ready _ | Rejected _ -> false);
    Store.with_published
      reopened
      ~principal:Fixture.principal
      ~bundle_id
      ~now:(fun () -> Fixture.start)
      ~checkpoint:Fn.id
      ~admit_metadata:(fun _ _ -> ())
      ~f:(fun capture ->
        check
          "borrowed exact document"
          (match Store.Published_capture.uploaded_sources capture with
           | [ source ] ->
             Captured.Id.equal (Captured.id captured) (Captured.id source)
             && String.equal (Captured.contents captured) (Captured.contents source)
           | _ -> false);
        check
          "source-only has no invented graph"
          (List.is_empty (Store.Published_capture.graphs capture));
        check
          "self close cannot lose active lease"
          (match Store.close_and_wait reopened with
           | Error Store.Error.Busy -> true
           | Ok () | Error _ -> false);
        check
          "second capture cannot multiply active retained bytes"
          (match
             Store.with_published
               reopened
               ~principal:Fixture.principal
               ~bundle_id
               ~now:(fun () -> Fixture.start)
               ~checkpoint:Fn.id
               ~admit_metadata:(fun _ _ -> ())
               ~f:(fun _ -> ())
           with
           | Error Store.Error.Busy -> true
           | Ok () | Error _ -> false);
        let second_begin =
          B.Begin.create_request
            ~idempotency_key:
              (P.Idempotency_key.of_string "publication-second" |> protocol_ok)
            ~manifest
            ()
        in
        let second_id =
          Store.begin_upload
            reopened
            ~principal:Fixture.principal
            ~request:second_begin
            ~now:(fun () -> Fixture.start)
            ~checkpoint:Fn.id
          |> upload_ok
          |> B.Begin.upload_id
        in
        Fixture.send_chunk
          reopened
          second_id
          document_id
          0
          (String.prefix document_bytes first)
        |> upload_ok
        |> ignore;
        if first < String.length document_bytes
        then
          Fixture.send_chunk
            reopened
            second_id
            document_id
            first
            (String.drop_prefix document_bytes first)
          |> upload_ok
          |> ignore;
        let second =
          match
            Store.claim_commit
              reopened
              ~principal:Fixture.principal
              ~request:
                (B.Commit.create_request
                   ~upload_id:second_id
                   ~idempotency_key:
                     (P.Idempotency_key.of_string "publication-second-commit"
                      |> protocol_ok))
              ~now:(fun () -> Fixture.start)
              ~checkpoint:Fn.id
            |> upload_ok
          with
          | Ready second -> second
          | Published _ | Rejected _ -> failwith "second claim unexpectedly terminal"
        in
        let second_snapshot =
          Store.snapshot
            reopened
            ~principal:Fixture.principal
            second
            ~now:(fun () -> Fixture.start)
            ~checkpoint:Fn.id
          |> upload_ok
        in
        check
          "another interrupted marker leaves uncertain recovery work"
          (match
             Store.publish
               reopened
               ~principal:Fixture.principal
               second
               ~snapshot:second_snapshot
               ~proof
               ~now:(fun () -> Fixture.start)
               ~checkpoint:Fn.id
               ~final_admission:(fun () -> raise sentinel)
           with
           | exception exn -> phys_equal exn sentinel
           | Ok _ | Error _ -> false);
        check
          "recovery preserves the first published receipt"
          (match
             Store.status
               reopened
               ~principal:Fixture.principal
               ~upload_id
               ~now:(fun () -> Fixture.start)
               ~checkpoint:Fn.id
             |> upload_ok
           with
           | Published recovered -> B.Receipt.equal recovered receipt
           | Open _ | Ready | Validating | Rejected _ | Expired -> false);
        check
          "recover cannot erase the active reader lease"
          (match Store.close_and_wait reopened with
           | Error Store.Error.Busy -> true
           | Ok () | Error _ -> false);
        let removed =
          Store.prune
            reopened
            ~now:(fun () -> Fixture.later 121000)
            ~protected_uploads:[]
            ~checkpoint:Fn.id
          |> upload_ok
        in
        check "borrow protects expired files" (removed = 0))
    |> upload_ok;
    Store.close_and_wait reopened |> upload_ok)
;;

let%test_unit
    "actual Project proof installs its generated lock and rechecks recovery limits"
  =
  Fixture.with_root "ochat-upload-project-publication" (fun env sw root lock ->
    let contract =
      Chatml.Chatml_compiler_contract.for_target Moderator_v1 ~checkpoint:Fn.id
    in
    let original = Recomposition.library contract in
    let sources = Chatml_project_build_tests.Support.without_lock original in
    let entry = Project_fixture.find_entry original in
    let project =
      Project_fixture.project
        original
        ~source_members:sources
        ~selection:(Project_fixture.selection Library)
        ~entry:(Captured (Project_fixture.captured_entry entry))
        ~mode:
          (Compose
             { selected_previous_lock = None
             ; generated_lock =
                 Captured.Id.create
                   ~package_namespace:"role"
                   ~relative_path:"published.lock.json"
                 |> Or_error.ok_exn
             })
    in
    let manifest = Project_fixture.manifest project sources in
    let large_limits =
      Fixture.limits
        ~max_members:8
        ~max_member_bytes:10240
        ~max_chunk_bytes:10240
        ~max_bundle_bytes:65536
        ~max_reserved_bytes:131072
        ~max_reserved_per_principal:131072
        ~max_installed_bytes_per_bundle:65536
        ~max_installed_bytes:65536
        ~max_installed_per_principal:65536
        ~max_recovery_source_bytes:262144
        ~max_work_steps:100000
        ()
    in
    let open_store limits =
      Store.open_or_create
        ~env
        ~sw
        ~data_root:root
        ~daemon_lock:lock
        ~host_id:Fixture.server_id
        ~limits
        ~checkpoint:Fn.id
    in
    let store = open_store large_limits |> upload_ok in
    let begin_key = P.Idempotency_key.of_string "project-publication" |> protocol_ok in
    let request = B.Begin.create_request ~idempotency_key:begin_key ~manifest () in
    let upload_id =
      Store.begin_upload
        store
        ~principal:Fixture.principal
        ~request
        ~now:(fun () -> Fixture.start)
        ~checkpoint:Fn.id
      |> upload_ok
      |> B.Begin.upload_id
    in
    List.iter sources ~f:(fun source ->
      Fixture.send_chunk store upload_id (Captured.id source) 0 (Captured.contents source)
      |> upload_ok
      |> ignore);
    let commit =
      B.Commit.create_request
        ~upload_id
        ~idempotency_key:(P.Idempotency_key.of_string "project-commit" |> protocol_ok)
    in
    let sealed =
      match
        Store.claim_commit
          store
          ~principal:Fixture.principal
          ~request:commit
          ~now:(fun () -> Fixture.start)
          ~checkpoint:Fn.id
        |> upload_ok
      with
      | Ready sealed -> sealed
      | Published _ | Rejected _ -> failwith "project claim unexpectedly terminal"
    in
    let snapshot =
      Store.snapshot
        store
        ~principal:Fixture.principal
        sealed
        ~now:(fun () -> Fixture.start)
        ~checkpoint:Fn.id
      |> upload_ok
    in
    let validated =
      Chatml_authoring_bundle_project.Attempt.admit
        manifest
        ~sources:(Store.Snapshot.sources snapshot)
        ~limits:validation_limits
        ~checkpoint:Fn.id
      |> Project_fixture.adapter_ok
      |> fun attempt ->
      Chatml_authoring_bundle_project.Attempt.validate_top_project
        attempt
        ~previous:None
        ~contract
        ~checkpoint:Fn.id
      |> Project_fixture.adapter_ok
    in
    let receipt =
      Store.publish
        store
        ~principal:Fixture.principal
        sealed
        ~snapshot
        ~proof:(Store.Proof.Project validated)
        ~now:(fun () -> Fixture.start)
        ~checkpoint:Fn.id
        ~final_admission:Fn.id
      |> upload_ok
    in
    let generated_lock =
      Build.lock_source (Chatml_authoring_bundle_project.Validated.build validated)
    in
    let installed_bytes = Captured.byte_length generated_lock in
    check "genuine generated lock occupies installed storage" (installed_bytes > 0);
    let bundle_id = B.Receipt.bundle_id receipt in
    Store.with_published
      store
      ~principal:Fixture.principal
      ~bundle_id
      ~now:(fun () -> Fixture.start)
      ~checkpoint:Fn.id
      ~admit_metadata:(fun _ _ -> ())
      ~f:(fun capture ->
        match Store.Published_capture.graphs capture with
        | [ graph ] ->
          check
            "published graph has exact generated lock"
            (String.equal
               (Captured.contents (Store.Published_capture.Graph.lock_source graph))
               (Captured.contents generated_lock));
          let attempt =
            Chatml_authoring_bundle_project.Attempt.admit
              manifest
              ~sources:(Store.Published_capture.uploaded_sources capture)
              ~limits:validation_limits
              ~checkpoint:Fn.id
            |> Project_fixture.adapter_ok
          in
          let rebound =
            Chatml_authoring_bundle_project.Attempt.restore_published_top
              attempt
              ~lock_source:(Store.Published_capture.Graph.lock_source graph)
              ~sources:(Store.Published_capture.Graph.sources graph)
              ~contract
              ~checkpoint:Fn.id
            |> Project_fixture.adapter_ok
          in
          check
            "final Compose lock restores without the original previous Build"
            (Captured.Digest.equal
               (Chatml_authoring_bundle_project.Validated.input_fingerprint rebound)
               (Chatml_authoring_bundle_project.Validated.input_fingerprint validated)
             && Store.Published_capture.agrees_with_proof
                  capture
                  (Store.Proof.Project rebound)
                  ~checkpoint:Fn.id)
        | _ -> failwith "project publication lacked its graph")
    |> upload_ok;
    Store.close_and_wait store |> upload_ok;
    if installed_bytes > 1
    then (
      let smaller_limits =
        Fixture.limits
          ~max_members:8
          ~max_member_bytes:10240
          ~max_chunk_bytes:10240
          ~max_bundle_bytes:65536
          ~max_reserved_bytes:131072
          ~max_reserved_per_principal:131072
          ~max_installed_bytes_per_bundle:(installed_bytes - 1)
          ~max_installed_bytes:65536
          ~max_installed_per_principal:65536
          ~max_recovery_source_bytes:262144
          ~max_work_steps:100000
          ()
      in
      check
        "reopen under lower installed-byte cap fails closed"
        (match open_store smaller_limits with
         | Error (Store.Error.Storage _) | Error Store.Error.Limit_exceeded -> true
         | Ok _ | Error _ -> false));
    let reopened = open_store large_limits |> upload_ok in
    check
      "recovered Project retains its original receipt"
      (match
         Store.status
           reopened
           ~principal:Fixture.principal
           ~upload_id
           ~now:(fun () -> Fixture.start)
           ~checkpoint:Fn.id
         |> upload_ok
       with
       | Published recovered -> B.Receipt.equal receipt recovered
       | Open _ | Ready | Validating | Rejected _ | Expired -> false);
    let selected_environment =
      Prepared.Environment.name
        (Prepared.environment
           (Build.prepared (Chatml_authoring_bundle_project.Validated.build validated)))
    in
    let selected_current =
      Host.Rebind_policy.Selected.create ~environment:selected_environment ~contract
      |> Or_error.ok_exn
    in
    let selected_stale =
      Host.Rebind_policy.Selected.create ~environment:"changed_environment" ~contract
      |> Or_error.ok_exn
    in
    let selected_wrong_contract =
      Host.Rebind_policy.Selected.create
        ~environment:selected_environment
        ~contract:(Chatml.Chatml_compiler_contract.for_target Tool_v1 ~checkpoint:Fn.id)
      |> Or_error.ok_exn
    in
    let selected = ref selected_current in
    let selection_calls = ref 0 in
    let read_checks = ref 0 in
    let change_at_read_check = ref None in
    let read_allowed = ref true in
    Eio.Switch.run (fun host_sw ->
      let rebind_policy =
        Host.Rebind_policy.create
          ~authorize_read:(fun _ ->
            Int.incr read_checks;
            if Option.equal Int.equal !change_at_read_check (Some !read_checks)
            then selected := selected_stale;
            !read_allowed)
          ~select:(fun _ _ _ ->
            Int.incr selection_calls;
            Some !selected)
      in
      let rebind =
        Host.Rebind.create
          ~policy:rebind_policy
          ~limits:validation_limits
          ~document_dir:(Eio.Stdenv.fs env)
      in
      let actor =
        Operator_authorization.guarded
          ~principal:
            (P.Principal.create
               ~id:Fixture.principal
               ~authentication_kind:"local.rebind_test"
               ~scopes:P.Scope.Set.empty
               ~attributes:[]
             |> protocol_ok)
          ~is_current:(fun () -> true)
      in
      let host =
        Host.create
          ~rebind
          reopened
          ~sw:host_sw
          ~clock:(Eio_mock.Clock.make ())
          ~upload_policy:
            (Host.Upload_policy.create
               ~authorize_upload:(fun _ -> false)
               ~authorize_parent:(fun _ _ -> false))
      in
      let rebound = Host.rebind host ~actor ~bundle_id ~checkpoint:Fn.id |> host_ok in
      check
        "host restores the authentic selected Project under independent read grant"
        (B.Receipt.equal receipt (Host.Rebound.receipt rebound)
         &&
         match Host.Rebound.basis rebound with
         | Project project ->
           Captured.Digest.equal
             (Chatml_authoring_bundle_project.Validated.input_fingerprint project)
             (Chatml_authoring_bundle_project.Validated.input_fingerprint validated)
         | Chatmd_document _ -> false);
      let final_read_check = !read_checks in
      let connection =
        Agent_server.Connection_context.create_authenticated
          ~actor
          ~connection_id:"uploaded-project-context-test"
          ~transport:In_memory
          ~publish_notification:(fun _ -> ())
          ~max_attachments:1
      in
      let context_service =
        Context_service.create
          ~sw:host_sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~host:Fixture.server_id
          ~sources:None
          ~upload:(Some host)
          ~supplied_document:None
          ~selection_policy:
            (Context_service.Selection_policy.create
               ~project_contract:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
               ~document_static:(fun _ ~requested_environment:_ -> None))
          ~context_limits:
            (Chatml_authoring_context.Limits.create
               ~max_contexts:2
               ~max_sources:128
               ~max_open_documents:1
               ~max_document_bytes:1_000_000
               ~max_effective_input_bytes:1_000_000
               ~max_context_source_bytes:1_000_000
               ~max_total_source_bytes:2_000_000
             |> Or_error.ok_exn)
          ~analysis_policy:(context_analysis_policy ())
          ~protocol_limits:
            (P.Authoring.Limits.create
               ~max_sources:128
               ~max_source_bytes:1_000_000
               ~max_context_source_bytes:1_000_000
               ~max_request_bytes:1_000_000
               ~max_result_bytes:1_000_000
               ~max_path_candidates:16
             |> protocol_ok)
          ~limits:
            (Context_service.Limits.create
               ~max_connections:2
               ~max_active_requests_per_connection:2
               ~max_roots_per_connection:2
               ~max_response_bytes:1_000_000
               ~max_provisional_results:2
             |> Or_error.ok_exn)
      in
      (match
         Context_service.publish_initialize_and_wait
           context_service
           ~connection
           ~response:
             (context_initialize
                ~server_id:Fixture.server_id
                ~principal:(Operator_authorization.principal actor))
       with
       | Ok () -> ()
       | Error _ -> failwith "upload-only authoring initialize failed");
      let core request =
        Context_service.handle_core context_service ~actor ~connection request
      in
      let context =
        match
          core
            (P.Authoring.Wire.Request.Context_open
               (P.Authoring.Context.Open_request.create_uploaded_project bundle_id))
        with
        | Ok (Context_open context) -> context
        | Ok _ | Error _ -> failwith "published Project context admission failed"
      in
      check
        "upload-only client got genuine selected Project basis"
        (match P.Authoring.Context.basis context with
         | Project _ -> true
         | Document _ -> false);
      let prepared =
        Build.prepared (Chatml_authoring_bundle_project.Validated.build validated)
      in
      let entry =
        Prepared.sources prepared
        |> List.find_exn ~f:(fun source ->
          Captured.Id.equal (Captured.id source) (Prepared.entry prepared))
      in
      let read =
        P.Authoring.Source_read.Read_request.create
          ~context:(P.Authoring.Context.Expected.of_view context)
          ~source_id:(Captured.id entry)
        |> protocol_ok
      in
      (match core (Source_read read) with
       | Ok (Source_read result) ->
         [%test_eq: string]
           (P.Authoring.Source_read.Read_result.contents result)
           (Captured.contents entry)
       | Ok _ | Error _ -> failwith "published Project source unavailable");
      read_allowed := false;
      check
        "cached uploaded source repeats independent read grant"
        (match core (Source_read read) with
         | Error Context_service.Error.Unauthorized -> true
         | Ok _ | Error _ -> false);
      read_allowed := true;
      selected := selected_stale;
      check
        "cached uploaded source repeats selected environment"
        (match core (Source_read read) with
         | Error Context_service.Error.Stale_context -> true
         | Ok _ | Error _ -> false);
      selected := selected_current;
      (match Context_service.close_and_wait context_service with
       | Ok () -> ()
       | Error _ -> failwith "upload-only authoring close failed");
      check "successful rebind checked selected target" (!selection_calls > 1);
      check "successful rebind rechecked read grant" (final_read_check > 2);
      read_checks := 0;
      change_at_read_check := Some final_read_check;
      check
        "selection changed at the final post-borrow read check is rejected"
        (match Host.rebind host ~actor ~bundle_id ~checkpoint:Fn.id with
         | Error Host.Error.Stale_capture -> true
         | Ok _ | Error _ -> false);
      change_at_read_check := None;
      selected := selected_stale;
      check
        "current but wrong selected environment rejects saved graph"
        (match Host.rebind host ~actor ~bundle_id ~checkpoint:Fn.id with
         | Error Host.Error.Stale_capture -> true
         | Ok _ | Error _ -> false);
      selected := selected_wrong_contract;
      check
        "current but wrong compiler contract rejects saved graph"
        (match Host.rebind host ~actor ~bundle_id ~checkpoint:Fn.id with
         | Error Host.Error.Stale_capture -> true
         | Ok _ | Error _ -> false);
      read_allowed := false;
      check
        "read denial precedes a private published-capture borrow"
        (match Host.rebind host ~actor ~bundle_id ~checkpoint:Fn.id with
         | Error Host.Error.Unauthorized -> true
         | Ok _ | Error _ -> false);
      Host.close_and_wait host |> host_ok);
    let reopened = open_store large_limits |> upload_ok in
    ignore
      (Store.prune
         reopened
         ~now:(fun () -> Fixture.later 121000)
         ~protected_uploads:[]
         ~checkpoint:Fn.id
       |> upload_ok
       : int);
    check
      "expiry tombstones graph and installed files together"
      (match
         Store.status
           reopened
           ~principal:Fixture.principal
           ~upload_id
           ~now:(fun () -> Fixture.later 121000)
           ~checkpoint:Fn.id
         |> upload_ok
       with
       | Expired -> true
       | Open _ | Ready | Validating | Published _ | Rejected _ -> false);
    Store.close_and_wait reopened |> upload_ok)
;;
