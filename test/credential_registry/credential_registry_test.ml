open! Core
module R = Credential_registry
module M = Credential_registry_model
module S = Private_storage
module B = Provider_secret_store

let model = function
  | Ok v -> v
  | Error e -> raise_s (M.Error.sexp_of_t e)
;;

let storage = function
  | Ok v -> v
  | Error e -> raise_s (S.Error.sexp_of_t e)
;;

let backend = function
  | Ok v -> v
  | Error e -> raise_s (B.Error.sexp_of_t e)
;;

let lifecycle = function
  | Ok v -> v
  | Error e -> raise_s (R.Error.sexp_of_t e)
;;

let id value = M.Id.create value |> model
let host = id "synthetic_host"
let binding = id "synthetic_binding"

let identity =
  M.Identity.api_key
    ~host
    ~provider:"synthetic"
    ~billing:"api"
    ~account:(Some "account_a")
    ~key_reference:(id "synthetic_key")
  |> model
;;

let secret value = B.Secret.of_bytes (Bytes.of_string value) |> backend

let verified =
  R.Verified.create
    ~identity
    ~grant:None
    ~material:(R.Material.api_key (secret "synthetic-token"))
  |> lifecycle
;;

let with_registry f =
  Eio_main.run (fun env ->
    let path =
      Eio_unix.run_in_systhread (fun () ->
        Core_unix.mkdtemp "/tmp/ochat-registry-synthetic-XXXXXX")
    in
    let anchor = Eio.Path.(Eio.Stdenv.fs env / path) in
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let directory =
            S.Directory.open_or_create
              ~sw
              ~anchor
              ~components:[ S.Name.create "private" |> storage ]
            |> storage
          in
          let secrets =
            B.open_private_files
              ~sw
              ~directory
              ~namespace:(B.Namespace.create "synthetic" |> backend)
            |> backend
          in
          let serial = ref 0 in
          let new_operation () =
            Int.incr serial;
            id (sprintf "operation_%d" !serial)
          in
          let registry =
            R.initialize_new
              ~sw
              ~wall_clock:(Eio.Stdenv.clock env)
              ~new_operation
              ~directory
              ~secrets
              ~environment:None
              ~host
              ~incarnation:(id "incarnation_a")
            |> lifecycle
          in
          f env sw anchor directory secrets registry new_operation)))
;;

let install registry operation =
  let candidate =
    R.begin_candidate
      registry
      ~binding
      ~operation
      ~expectation:(M.Expectation.exact identity)
    |> lifecycle
  in
  R.commit_candidate registry candidate verified |> lifecycle
;;

let%expect_test
    "foreign Existing revision is quarantined without deleting working or foreign \
     material"
  =
  with_registry (fun _ _ _ _ secrets registry _ ->
    install registry (id "working_login");
    let revision = B.Revision.create "foreign_collision" |> backend in
    B.create secrets ~revision (secret "synthetic-foreign-material") |> backend;
    let candidate =
      R.begin_candidate
        registry
        ~binding
        ~operation:(id "foreign_collision")
        ~expectation:(M.Expectation.exact identity)
      |> lifecycle
    in
    (match R.commit_candidate registry candidate verified with
     | Error Revision_quarantined -> ()
     | _ -> failwith "foreign revision activated");
    let status = R.reconcile registry ~binding |> lifecycle in
    assert (R.Status.equal_availability (R.Status.availability status) Available);
    assert (
      R.Status.equal_secret_cleanup (R.Status.cleanup status).secrets Cleanup_quarantined);
    B.Secret.with_string
      (B.read secrets ~revision |> backend)
      ~f:(fun value -> assert (String.equal value "synthetic-foreign-material"));
    print_endline
      "working login preserved; foreign revision remains quarantined and untouched");
  [%expect
    {| working login preserved; foreign revision remains quarantined and untouched |}]
;;

let%expect_test
    "durable tombstone precedes blocked attempt drain and restart reconciliation"
  =
  with_registry (fun env sw _ directory secrets registry new_operation ->
    install registry (id "working_login");
    let context =
      R.synchronize registry |> lifecycle |> R.Host_snapshot.bindings |> List.hd_exn
    in
    Eio.Switch.run (fun attempt_sw ->
      let admitted =
        R.admit
          registry
          ~sw:attempt_sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~maximum_wait:(Time_ns.Span.of_sec 0.05)
          ~binding
          ~expected_owner:(R.Host_snapshot.owner context)
          ~expected_epoch:(R.Host_snapshot.epoch context)
          ~renewal:None
        |> lifecycle
      in
      assert (M.Identity.equal (R.Admission.identity admitted) identity);
      let other =
        M.Identity.api_key
          ~host
          ~provider:"synthetic"
          ~billing:"api"
          ~account:(Some "account_b")
          ~key_reference:(id "different_key")
        |> model
      in
      assert (not (M.Identity.equal (R.Admission.identity admitted) other));
      let removal =
        R.disable
          registry
          ~sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~maximum_wait:(Time_ns.Span.of_sec 0.02)
          ~binding
          ~revocation:None
          ~reason:Logout
        |> lifecycle
      in
      assert removal.disabled;
      assert (R.Status.equal_drain removal.cleanup.drain Drain_pending);
      (match R.Admission.check_current admitted with
       | Error (Model Disabled) -> ()
       | _ -> failwith "stale admission remained valid");
      R.close registry;
      let restarted =
        R.open_existing
          ~sw
          ~wall_clock:(Eio.Stdenv.clock env)
          ~new_operation
          ~directory
          ~secrets
          ~environment:None
          ~host
        |> lifecycle
      in
      let current = R.status restarted ~binding |> lifecycle in
      assert (R.Status.equal_drain (R.Status.cleanup current).drain Drain_pending);
      match R.reconcile restarted ~binding with
      | Error Busy -> ()
      | _ -> failwith "held attempt fence ignored");
    let restarted =
      R.open_existing
        ~sw
        ~wall_clock:(Eio.Stdenv.clock env)
        ~new_operation
        ~directory
        ~secrets
        ~environment:None
        ~host
      |> lifecycle
    in
    let final = R.reconcile restarted ~binding |> lifecycle in
    assert (R.Status.equal_drain (R.Status.cleanup final).drain Drained);
    assert (R.Status.equal_secret_cleanup (R.Status.cleanup final).secrets Clean);
    assert (R.Status.equal_availability (R.Status.availability final) Disabled);
    print_endline
      "disable authoritative before drain; restart retains pending proof; released \
       attempt permits cleanup");
  [%expect
    {| disable authoritative before drain; restart retains pending proof; released attempt permits cleanup |}]
;;

let install_expired_oauth
      ?(continuity = M.Presence.Absent)
      ?(operation = id "oauth_login")
      registry
  =
  let identity =
    M.Identity.oauth
      ~host
      ~provider:"synthetic"
      ~billing:"subscription"
      ~issuer:"https://issuer.example"
      ~client_registration:"synthetic_client"
      ~resource:"https://resource.example"
      ~account:"account_a"
      ~verified_subject:"subject_a"
      ~required_scopes:[ "inference" ]
    |> model
  in
  let grant =
    M.Grant.create
      ~identity
      ~scopes:(Value [ "inference" ])
      ~expires_at_ms:(Value 0L)
      ~refresh_policy:Require_rotated
      ~effective:
        { scopes = [ "inference" ]
        ; scopes_provenance = Declared
        ; expiry = Known { at_ms = 0L; provenance = Declared }
        ; unknown_expiry_policy = Reject_unknown
        }
    |> model
  in
  let verified =
    R.Verified.create
      ~identity
      ~grant:(Some grant)
      ~material:
        (R.Material.oauth
           ~continuity
           ~access:(secret "synthetic-old-access")
           ~refresh:(Value (secret "synthetic-old-refresh")))
    |> lifecycle
  in
  let candidate =
    R.begin_candidate
      registry
      ~binding
      ~operation
      ~expectation:(M.Expectation.exact identity)
    |> lifecycle
  in
  R.commit_candidate registry candidate verified |> lifecycle
;;

let spawn_refresh env sw anchor =
  let output, sink = Eio.Process.pipe ~sw (Eio.Stdenv.process_mgr env) in
  let input, writer = Eio.Process.pipe ~sw (Eio.Stdenv.process_mgr env) in
  let child =
    Eio.Process.spawn
      ~sw
      (Eio.Stdenv.process_mgr env)
      ~stdin:input
      ~stdout:sink
      [ "./registry_probe.exe"; Eio.Path.native_exn anchor ]
  in
  Eio.Flow.close input;
  Eio.Flow.close sink;
  let reader = Eio.Buf_read.of_flow ~max_size:128 output in
  let ready =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Eio.Buf_read.line reader)
  in
  assert (String.equal ready "external-rotation-started");
  child, reader, writer
;;

let%expect_test
    "two processes serialize refresh and logout defeats late rotation publication"
  =
  with_registry (fun env sw anchor _ _ registry _ ->
    install_expired_oauth registry;
    let child, reader, writer = spawn_refresh env sw anchor in
    let exchange_count = ref 0 in
    let renewal =
      R.Renewal.create ~exchange:(fun ~sw:_ ~identity:_ ~grant:_ _ ->
        Int.incr exchange_count;
        Definitely_not_submitted)
    in
    (match
       R.refresh
         registry
         ~sw
         ~clock:(Eio.Stdenv.mono_clock env)
         ~maximum_wait:(Time_ns.Span.of_sec 0.02)
         ~binding
         ~renewal
     with
     | Error Timed_out -> ()
     | _ -> failwith "second process refresh entered rotation");
    assert (Int.equal !exchange_count 0);
    let removal =
      R.disable
        registry
        ~sw
        ~clock:(Eio.Stdenv.mono_clock env)
        ~maximum_wait:(Time_ns.Span.of_sec 0.02)
        ~binding
        ~revocation:None
        ~reason:Logout
      |> lifecycle
    in
    assert removal.disabled;
    Eio.Flow.copy_string "continue\n" writer;
    let result =
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        Eio.Buf_read.line reader)
    in
    assert (String.equal result "stale-epoch");
    (match Eio.Process.await child with
     | `Exited 0 -> ()
     | _ -> failwith "child refresh failed");
    let status = R.reconcile registry ~binding |> lifecycle in
    assert (R.Status.equal_availability (R.Status.availability status) Disabled);
    assert (R.Status.equal_secret_cleanup (R.Status.cleanup status).secrets Clean);
    print_endline
      "one external rotation; second process fenced; logout blocks stale publication");
  [%expect
    {| one external rotation; second process fenced; logout blocks stale publication |}]
;;

let%expect_test "process death after possibly-sent rotation cannot trigger external retry"
  =
  with_registry (fun env sw anchor directory secrets registry new_operation ->
    install_expired_oauth registry;
    let child, _, _ = spawn_refresh env sw anchor in
    Eio.Process.signal child 9;
    ignore (Eio.Process.await child : Eio.Process.exit_status);
    R.close registry;
    let restarted =
      R.open_existing
        ~sw
        ~wall_clock:(Eio.Stdenv.clock env)
        ~new_operation
        ~directory
        ~secrets
        ~environment:None
        ~host
      |> lifecycle
    in
    let status = R.status restarted ~binding |> lifecycle in
    assert (R.Status.equal_availability (R.Status.availability status) Renewal_uncertain);
    let exchanges = ref 0 in
    let renewal =
      R.Renewal.create ~exchange:(fun ~sw:_ ~identity:_ ~grant:_ _ ->
        Int.incr exchanges;
        Definitely_not_submitted)
    in
    (match
       R.refresh
         restarted
         ~sw
         ~clock:(Eio.Stdenv.mono_clock env)
         ~maximum_wait:(Time_ns.Span.of_sec 0.02)
         ~binding
         ~renewal
     with
     | Error (Model Renewal_uncertain) -> ()
     | _ -> failwith "uncertain rotation retried");
    assert (Int.equal !exchanges 0);
    print_endline
      "crash releases kernel lock; durable possibly-sent intent prohibits rotation retry");
  [%expect
    {| crash releases kernel lock; durable possibly-sent intent prohibits rotation retry |}]
;;

let%expect_test
    "unexpected exchange exception preserves diagnostic and durable uncertainty"
  =
  with_registry (fun env sw _ _ _ registry _ ->
    install_expired_oauth registry;
    let renewal =
      R.Renewal.create ~exchange:(fun ~sw:_ ~identity:_ ~grant:_ _ -> raise Exit)
    in
    (try
       ignore
         (R.refresh
            registry
            ~sw
            ~clock:(Eio.Stdenv.mono_clock env)
            ~maximum_wait:(Time_ns.Span.of_sec 0.02)
            ~binding
            ~renewal
          : (unit, R.Error.t) result);
       failwith "unexpected exception was swallowed"
     with
     | Exit -> ());
    assert (
      R.Status.equal_availability
        (R.status registry ~binding |> lifecycle |> R.Status.availability)
        Renewal_uncertain);
    print_endline "original exception propagated; possibly-sent journal retained");
  [%expect {| original exception propagated; possibly-sent journal retained |}]
;;

let%expect_test "close joins canceled owned exchange without orphan or false rollback" =
  with_registry (fun env sw _ directory secrets registry new_operation ->
    install_expired_oauth registry;
    let entered, enter = Eio.Promise.create () in
    let blocked, _ = Eio.Promise.create () in
    let joined, join = Eio.Promise.create () in
    let callback_finished = ref false in
    let renewal =
      R.Renewal.create ~exchange:(fun ~sw:_ ~identity:_ ~grant:_ _ ->
        Exn.protect
          ~finally:(fun () -> callback_finished := true)
          ~f:(fun () ->
            Eio.Promise.resolve enter ();
            Eio.Promise.await blocked;
            R.Renewal.Definitely_not_submitted))
    in
    (try
       Eio.Switch.run (fun exchange_sw ->
         Eio.Fiber.fork ~sw:exchange_sw (fun () ->
           ignore
             (R.refresh
                registry
                ~sw:exchange_sw
                ~clock:(Eio.Stdenv.mono_clock env)
                ~maximum_wait:(Time_ns.Span.of_sec 0.02)
                ~binding
                ~renewal
              : (unit, R.Error.t) result));
         Eio.Promise.await entered;
         Eio.Fiber.fork ~sw (fun () ->
           R.close registry;
           Eio.Promise.resolve join ());
         Eio.Fiber.yield ();
         assert (Option.is_none (Eio.Promise.peek joined));
         Eio.Switch.fail exchange_sw Exit)
     with
     | Exit | Eio.Cancel.Cancelled Exit -> ());
    Eio.Promise.await joined;
    assert !callback_finished;
    let reopened =
      R.open_existing
        ~sw
        ~wall_clock:(Eio.Stdenv.clock env)
        ~new_operation
        ~directory
        ~secrets
        ~environment:None
        ~host
      |> lifecycle
    in
    assert (
      R.Status.equal_availability
        (R.status reopened ~binding |> lifecycle |> R.Status.availability)
        Renewal_uncertain);
    print_endline
      "close waited for canceled callback finalizer; durable uncertainty survived");
  [%expect
    {| close waited for canceled callback finalizer; durable uncertainty survived |}]
;;

let%expect_test
    "declared environment port composes configuration currentness without ambient \
     fallback"
  =
  with_registry (fun env sw _ directory secrets registry new_operation ->
    R.close registry;
    let first = id "config_first" in
    let second = id "config_second" in
    let current = ref (Some first) in
    let calls = ref 0 in
    let environment =
      R.Environment.create
        ~status:(fun ~binding:_ ~name:_ ->
          if Option.is_some !current then Available else Secret_unavailable)
        ~resolve:
          (fun
            ~sw:_ ~binding:_ ~identity:_ ~name ~expected_configuration_revision:_ ->
          assert (String.equal name "EXPLICIT_SYNTHETIC_KEY");
          Int.incr calls;
          match !current with
          | None -> Error Binding_unavailable
          | Some revision ->
            Ok
              (R.Environment.resolved
                 ~access:(secret "synthetic-environment-token")
                 ~configuration_revision:(Some revision)
                 ~check_current:(fun () ->
                   if Option.equal M.Id.equal !current (Some revision)
                   then Ok ()
                   else Error (Model Stale_revision))))
    in
    let registry =
      R.open_existing
        ~sw
        ~wall_clock:(Eio.Stdenv.clock env)
        ~new_operation
        ~directory
        ~secrets
        ~environment:(Some environment)
        ~host
      |> lifecycle
    in
    let configure revision operation =
      let candidate =
        R.begin_candidate
          registry
          ~binding
          ~operation
          ~expectation:(M.Expectation.exact identity)
        |> lifecycle
      in
      R.commit_environment_candidate
        registry
        candidate
        ~identity
        ~name:"EXPLICIT_SYNTHETIC_KEY"
        ~configuration_revision:(Some revision)
      |> lifecycle
    in
    configure first (id "environment_first");
    let context () =
      R.synchronize registry |> lifecycle |> R.Host_snapshot.bindings |> List.hd_exn
    in
    let initial = context () in
    (match R.Host_snapshot.source initial with
     | Some (Environment_reference _) -> ()
     | _ -> failwith "lost declared source");
    Eio.Switch.run (fun attempt_sw ->
      let admission =
        R.admit
          registry
          ~sw:attempt_sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~maximum_wait:(Time_ns.Span.of_sec 0.02)
          ~binding
          ~expected_owner:(R.Host_snapshot.owner initial)
          ~expected_epoch:(R.Host_snapshot.epoch initial)
          ~renewal:None
        |> lifecycle
      in
      assert (
        Option.equal
          String.equal
          (R.Admission.credential_revision admission)
          (Some "config_first"));
      current := Some second;
      match R.Admission.check_current admission with
      | Error (Model Stale_revision) -> ()
      | _ -> failwith "changed environment guard passed");
    configure second (id "environment_second");
    let fresh = context () in
    assert (Int64.(R.Host_snapshot.epoch fresh > R.Host_snapshot.epoch initial));
    current := None;
    (match
       R.admit
         registry
         ~sw
         ~clock:(Eio.Stdenv.mono_clock env)
         ~maximum_wait:(Time_ns.Span.of_sec 0.02)
         ~binding
         ~expected_owner:(R.Host_snapshot.owner fresh)
         ~expected_epoch:(R.Host_snapshot.epoch fresh)
         ~renewal:None
     with
     | Error Binding_unavailable -> ()
     | _ -> failwith "missing declared environment silently fell back");
    assert (Int.equal !calls 2);
    print_endline
      "explicit port only; revision guard invalidates lease; missing environment has no \
       fallback");
  [%expect
    {| explicit port only; revision guard invalidates lease; missing environment has no fallback |}]
;;

let%expect_test
    "lost publication acknowledgment reconciles exact operation without fallback or new \
     revision"
  =
  with_registry (fun _ _ _ _ secrets registry _ ->
    install registry (id "working_login");
    let original = id "replacement_committed" in
    let candidate =
      R.begin_candidate
        registry
        ~binding
        ~operation:original
        ~expectation:(M.Expectation.exact identity)
      |> lifecycle
    in
    let publications = ref 0 in
    R.For_testing.set_after_publication_hook
      registry
      (Some
         (fun () ->
           Int.incr publications;
           if Int.equal !publications 2 then raise Exit));
    (try
       R.commit_candidate registry candidate verified |> lifecycle;
       failwith "lost acknowledgment was swallowed"
     with
     | Exit -> ());
    R.For_testing.set_after_publication_hook registry None;
    (match R.reconcile_operation registry ~binding ~operation:original |> lifecycle with
     | Committed -> ()
     | _ -> failwith "real committed pointer not recovered");
    let active =
      R.synchronize registry |> lifecycle |> R.Host_snapshot.bindings |> List.hd_exn
    in
    assert (Int64.equal (R.Host_snapshot.epoch active) 2L);
    assert (
      Option.equal
        String.equal
        (R.Host_snapshot.credential_revision active)
        (Some "replacement_committed"));
    (match R.commit_candidate registry candidate verified with
     | Error (Model Stale_epoch) -> ()
     | _ -> failwith "original candidate replayed");
    (match B.read secrets ~revision:(B.Revision.create "fresh_retry" |> backend) with
     | Error error when B.Error.equal_code (B.Error.code error) Missing -> ()
     | _ -> failwith "fresh retry revision unexpectedly published");
    print_endline
      "exact receipt proves commit; new epoch/revision retained; replay and fallback \
       absent");
  [%expect
    {| exact receipt proves commit; new epoch/revision retained; replay and fallback absent |}]
;;

let%expect_test
    "protected continuity survives restart and refresh cannot silently discard it"
  =
  let exercise preserve =
    with_registry (fun env sw _ directory secrets registry new_operation ->
      install_expired_oauth
        ~continuity:(Value (secret "synthetic-original-proof"))
        registry;
      R.close registry;
      let registry =
        R.open_existing
          ~sw
          ~wall_clock:(Eio.Stdenv.clock env)
          ~new_operation
          ~directory
          ~secrets
          ~environment:None
          ~host
        |> lifecycle
      in
      let renewal =
        R.Renewal.create ~exchange:(fun ~sw:_ ~identity ~grant material ->
          let continuity =
            R.Material.with_oauth material ~f:(fun ~access:_ ~refresh:_ ~continuity ->
              (match continuity with
               | Value proof ->
                 B.Secret.with_string proof ~f:(fun value ->
                   assert (String.equal value "synthetic-original-proof"))
               | Absent | Null -> failwith "restart lost original protected proof");
              if preserve then continuity else M.Presence.Absent)
            |> lifecycle
          in
          let expiry =
            Int64.of_float ((Eio.Time.now (Eio.Stdenv.clock env) +. 3600.) *. 1000.)
          in
          let previous = M.Grant.effective grant in
          let grant =
            M.Grant.create
              ~identity
              ~scopes:(Value previous.scopes)
              ~expires_at_ms:(Value expiry)
              ~refresh_policy:Require_rotated
              ~effective:
                { previous with expiry = Known { at_ms = expiry; provenance = Declared } }
            |> model
          in
          let verified =
            R.Verified.create
              ~identity
              ~grant:(Some grant)
              ~material:
                (R.Material.oauth
                   ~access:(secret "synthetic-new-access")
                   ~refresh:(Value (secret "synthetic-new-refresh"))
                   ~continuity)
            |> lifecycle
          in
          R.Renewal.Verified verified)
      in
      let result =
        R.refresh
          registry
          ~sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~maximum_wait:(Time_ns.Span.of_sec 0.02)
          ~binding
          ~renewal
      in
      if preserve
      then (
        result |> lifecycle;
        let current =
          R.synchronize registry |> lifecycle |> R.Host_snapshot.bindings |> List.hd_exn
        in
        assert (Int64.equal (R.Host_snapshot.epoch current) 1L);
        assert (
          Option.equal
            String.equal
            (R.Host_snapshot.credential_revision current)
            (Some "operation_1")))
      else (
        match result with
        | Error (Model Invalid_grant) -> ()
        | _ -> failwith "continuity silently discarded"))
  in
  exercise true;
  exercise false;
  print_endline
    "protected original proof reconstructed; complete refresh retained; omitted proof \
     rejected";
  [%expect
    {| protected original proof reconstructed; complete refresh retained; omitted proof rejected |}]
;;

let%expect_test
    "prospective revision capacity rejects rotation before provider callback while \
     logout still cleans"
  =
  with_registry (fun env sw _ _ _ registry _ ->
    for iteration = 1 to 64 do
      install_expired_oauth
        ~operation:(id (sprintf "full_revision_%d" iteration))
        registry
    done;
    let exchanges = ref 0 in
    let renewal =
      R.Renewal.create ~exchange:(fun ~sw:_ ~identity:_ ~grant:_ _ ->
        Int.incr exchanges;
        R.Renewal.Definitely_not_submitted)
    in
    (match
       R.refresh
         registry
         ~sw
         ~clock:(Eio.Stdenv.mono_clock env)
         ~maximum_wait:(Time_ns.Span.of_sec 0.02)
         ~binding
         ~renewal
     with
     | Error (Model Capacity) -> ()
     | _ -> failwith "full capacity reached external rotation");
    assert (Int.equal !exchanges 0);
    (match
       R.begin_candidate
         registry
         ~binding
         ~operation:(id "full_candidate")
         ~expectation:(M.Expectation.exact identity)
     with
     | Error (Model Capacity) -> ()
     | _ -> failwith "full capacity began enrollment");
    let removal =
      R.disable
        registry
        ~sw
        ~clock:(Eio.Stdenv.mono_clock env)
        ~maximum_wait:(Time_ns.Span.of_sec 0.02)
        ~binding
        ~revocation:None
        ~reason:Logout
      |> lifecycle
    in
    assert removal.disabled;
    assert (R.Status.equal_secret_cleanup removal.cleanup.secrets Clean);
    print_endline
      "zero provider calls at capacity; new enrollment rejected; local logout still \
       cleans all owned revisions");
  [%expect
    {| zero provider calls at capacity; new enrollment rejected; local logout still cleans all owned revisions |}]
;;

let%expect_test
    "metadata synchronization probes neither closed secrets nor environment callbacks"
  =
  with_registry (fun env sw _ directory secrets registry new_operation ->
    install registry (id "working_login");
    B.close secrets;
    let snapshot =
      R.synchronize registry |> lifecycle |> R.Host_snapshot.bindings |> List.hd_exn
    in
    assert (
      R.Host_snapshot.equal_availability (R.Host_snapshot.availability snapshot) Ready);
    assert (
      R.Status.equal_availability
        (R.status registry ~binding |> lifecycle |> R.Status.availability)
        Secret_unavailable);
    R.close registry;
    let callbacks = ref 0 in
    let environment =
      R.Environment.create
        ~status:(fun ~binding:_ ~name:_ ->
          Int.incr callbacks;
          Secret_unavailable)
        ~resolve:
          (fun
            ~sw:_ ~binding:_ ~identity:_ ~name:_ ~expected_configuration_revision:_ ->
          Int.incr callbacks;
          Error Binding_unavailable)
    in
    let registry =
      R.open_existing
        ~sw
        ~wall_clock:(Eio.Stdenv.clock env)
        ~new_operation
        ~directory
        ~secrets
        ~environment:(Some environment)
        ~host
      |> lifecycle
    in
    let candidate =
      R.begin_candidate
        registry
        ~binding
        ~operation:(id "metadata_environment")
        ~expectation:(M.Expectation.exact identity)
      |> lifecycle
    in
    R.commit_environment_candidate
      registry
      candidate
      ~identity
      ~name:"DECLARED_SYNTHETIC_ENV"
      ~configuration_revision:(Some (id "declared_config"))
    |> lifecycle;
    let configured =
      R.synchronize registry |> lifecycle |> R.Host_snapshot.bindings |> List.hd_exn
    in
    assert (
      R.Host_snapshot.equal_availability (R.Host_snapshot.availability configured) Ready);
    assert (Int.equal !callbacks 0);
    ignore (R.status registry ~binding |> lifecycle : R.Status.t);
    assert (Int.equal !callbacks 1);
    print_endline
      "metadata Ready does not claim secret availability; opening/synchronize have zero \
       credential callbacks");
  [%expect
    {| metadata Ready does not claim secret availability; opening/synchronize have zero credential callbacks |}]
;;

let%expect_test
    "separate secret root absence is confirmed there and closed backend preserves \
     pending cleanup"
  =
  with_registry (fun env sw anchor directory _ registry new_operation ->
    R.close registry;
    let secret_directory =
      S.Directory.open_or_create
        ~sw
        ~anchor
        ~components:[ S.Name.create "separate-secrets" |> storage ]
      |> storage
    in
    let open_backend () =
      B.open_private_files
        ~sw
        ~directory:secret_directory
        ~namespace:(B.Namespace.create "synthetic" |> backend)
      |> backend
    in
    let secrets = open_backend () in
    let open_registry secrets =
      R.open_existing
        ~sw
        ~wall_clock:(Eio.Stdenv.clock env)
        ~new_operation
        ~directory
        ~secrets
        ~environment:None
        ~host
      |> lifecycle
    in
    let registry = open_registry secrets in
    let original = id "never_created_revision" in
    let candidate =
      R.begin_candidate
        registry
        ~binding
        ~operation:original
        ~expectation:(M.Expectation.exact identity)
      |> lifecycle
    in
    R.For_testing.set_after_publication_hook registry (Some (fun () -> raise Exit));
    (try
       R.commit_candidate registry candidate verified |> lifecycle;
       failwith "staging acknowledgment unexpectedly returned"
     with
     | Exit -> ());
    R.For_testing.set_after_publication_hook registry None;
    R.cancel_pending_candidate registry ~binding ~operation:original |> lifecycle;
    B.close secrets;
    (match R.reconcile registry ~binding with
     | Error (Secret_store Closed) -> ()
     | _ -> failwith "closed backend confirmed cleanup");
    let pending = R.status registry ~binding |> lifecycle in
    assert (
      R.Status.equal_secret_cleanup (R.Status.cleanup pending).secrets Cleanup_pending);
    R.close registry;
    let reopened = open_registry (open_backend ()) in
    let complete = R.reconcile reopened ~binding |> lifecycle in
    assert (R.Status.equal_secret_cleanup (R.Status.cleanup complete).secrets Clean);
    print_endline
      "closed backend leaves missing revision pending; reopening confirms absence in \
       independent secret root");
  [%expect
    {| closed backend leaves missing revision pending; reopening confirms absence in independent secret root |}]
;;

let%expect_test "final candidate admission rejects authority lost during durable staging" =
  with_registry (fun env sw _ directory secrets registry new_operation ->
    let original = id "guard_working_login" in
    install registry original;
    let before =
      R.synchronize registry |> lifecycle |> R.Host_snapshot.bindings |> List.hd_exn
    in
    let attempted = id "guard_replacement" in
    let candidate =
      R.begin_candidate
        registry
        ~binding
        ~operation:attempted
        ~expectation:(M.Expectation.exact identity)
      |> lifecycle
    in
    let authorized = ref true in
    (* This hook runs after the staging metadata publication. Admission has to
       observe revocation after that yielding work, not only before commit entry. *)
    R.For_testing.set_after_publication_hook
      registry
      (Some (fun () -> authorized := false));
    (match
       R.commit_candidate
         ~authorize_commit:(fun () -> !authorized)
         registry
         candidate
         verified
     with
     | Error Authorization_denied -> ()
     | _ -> failwith "expired authority published a staged candidate");
    R.For_testing.set_after_publication_hook registry None;
    let after =
      R.synchronize registry |> lifecycle |> R.Host_snapshot.bindings |> List.hd_exn
    in
    assert (Int64.equal (R.Host_snapshot.epoch before) (R.Host_snapshot.epoch after));
    (match R.Host_snapshot.source after with
     | Some (M.Active.Protected_revision revision) ->
       assert (M.Id.equal revision original)
     | _ -> failwith "working credential changed after denied publication");
    (match R.reconcile_operation registry ~binding ~operation:attempted |> lifecycle with
     | M.Operation.Rejected -> ()
     | _ -> failwith "denied operation was not durably rejected");
    let cleaned = R.reconcile registry ~binding |> lifecycle in
    assert (R.Status.equal_secret_cleanup (R.Status.cleanup cleaned).secrets Clean);
    R.close registry;
    let reopened =
      R.open_existing
        ~sw
        ~wall_clock:(Eio.Stdenv.clock env)
        ~new_operation
        ~directory
        ~secrets
        ~environment:None
        ~host
      |> lifecycle
    in
    let retained =
      R.synchronize reopened |> lifecycle |> R.Host_snapshot.bindings |> List.hd_exn
    in
    assert (Int64.equal (R.Host_snapshot.epoch before) (R.Host_snapshot.epoch retained));
    install reopened (id "guard_later_login");
    let exceptional =
      R.begin_candidate
        reopened
        ~binding
        ~operation:(id "guard_exception")
        ~expectation:(M.Expectation.exact identity)
      |> lifecycle
    in
    (try
       ignore
         (R.commit_candidate
            ~authorize_commit:(fun () -> raise Exit)
            reopened
            exceptional
            verified
          : (unit, R.Error.t) result);
       failwith "unexpected guard exception disappeared"
     with
     | Exit -> ());
    install reopened (id "guard_after_exception");
    print_endline
      "late denial preserves epoch and working key across restart; staged cleanup and \
       exceptional guard release original candidate");
  [%expect
    {| late denial preserves epoch and working key across restart; staged cleanup and exceptional guard release original candidate |}]
;;
