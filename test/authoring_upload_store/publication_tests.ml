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
      ~f:(fun capture ->
        match Store.Published_capture.graphs capture with
        | [ graph ] ->
          check
            "published graph has exact generated lock"
            (String.equal
               (Captured.contents (Store.Published_capture.Graph.lock_source graph))
               (Captured.contents generated_lock))
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
