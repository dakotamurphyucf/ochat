open! Core
module B = Inference_host.Credential_bridge
module S = Inference_host.Provider_configuration
module C = Credential_registry
module M = Credential_registry_model
module D = Openai.Responses_driver
module R = Inference.Request
module RT = Inference_runtime

let ok result =
  Result.map_error result ~f:(fun _ -> "fixture failure") |> Result.ok_or_failwith
;;

let id value = M.Id.create value |> ok
let host = id "bridge_fixture"
let limits = Document_schema.Limits.default

let mapping name =
  let identity =
    M.Identity.api_key
      ~host
      ~provider:"openai"
      ~billing:"api"
      ~account:None
      ~key_reference:(id name)
    |> ok
  in
  let profile =
    D.Profile.create
      ~id:name
      ~account:None
      ~endpoint:"http://127.0.0.1:9/v1/responses"
      ~capabilities:
        (D.Capability.create ~baseline:[ Text_input, Supported ] ~models:[] |> ok)
      ~defaults:[]
    |> ok
  in
  ( B.Mapping.create profile ~revision:"fixture-config" ~binding:(id name) ~identity |> ok
  , identity )
;;

let with_fixture f =
  Eio_main.run (fun env ->
    let path =
      Eio_unix.run_in_systhread (fun () -> Core_unix.mkdtemp "/tmp/ochat-bridge-XXXXXX")
    in
    let anchor = Eio.Path.(Eio.Stdenv.fs env / path) in
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      ~f:(fun () -> Eio.Switch.run (fun sw -> f env sw anchor)))
;;

let open_host ?oauth ?net env sw anchor mode ~mappings ~environment ~authorize =
  let configuration =
    S.create
      ~anchor
      ~components:[ Private_storage.Name.create "private" |> ok ]
      ~host
      ~secret_namespace:(Provider_secret_store.Namespace.create "bridge" |> ok)
      ~mode
    |> ok
  in
  let serial = ref 0 in
  S.open_host
    configuration
    ~sw
    ~env
    ~driver:
      (D.create
         ~net:
           (Option.value
              net
              ~default:(Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Net.t))
         ~clock:(Eio.Stdenv.clock env)
         ()
       |> ok)
    ~new_operation:(fun () ->
      Int.incr serial;
      id (sprintf "operation_%d" !serial))
    ~environment
    ~oauth
    ~mappings
    ~authorize
    ~maximum_wait:(Time_ns.Span.of_sec 0.05)
    ~transport_policy:Http_sse
    ~limits:RT.Limits.default
;;

let authorize ~principal ~profile:_ ~operation:_ = String.equal principal "operator"

let configure bridge profile operation =
  B.configure_environment
    bridge
    ~principal:"operator"
    ~profile
    ~operation:(id operation)
    ~name:(String.uppercase profile)
    ~configuration_revision:None
  |> ok
;;

let availability bridge profile =
  B.status bridge ~principal:"operator" ~profile |> ok |> B.Status.availability
;;

let%expect_test
    "explicit initialization, source removal and restart preserve unrelated bindings"
  =
  with_fixture (fun env sw anchor ->
    let one, identity_one = mapping "one"
    and two, identity_two = mapping "two" in
    let lookups = ref 0 in
    let status_probes = ref 0 in
    let entry name identity =
      B.Environment.Entry.create
        ~binding:(id name)
        ~identity
        ~name:(String.uppercase name)
        ~configuration_revision:None
        ~resolve:(fun ~sw:_ ->
          Int.incr lookups;
          Ok
            (C.Environment.resolved
               ~access:
                 (Provider_secret_store.Secret.of_bytes (Bytes.of_string "synthetic-key")
                  |> ok)
               ~configuration_revision:None
               ~check_current:(fun () -> Ok ())))
        ~status:(fun () ->
          Int.incr status_probes;
          Available)
      |> ok
    in
    let environment =
      B.Environment.create [ entry "one" identity_one; entry "two" identity_two ] |> ok
    in
    let opened =
      open_host
        env
        sw
        anchor
        (Initialize (id "incarnation"))
        ~mappings:[ one; two ]
        ~environment:(Some environment)
        ~authorize
      |> ok
    in
    let bridge = S.Opened.bridge opened in
    assert (Int.equal !lookups 0);
    assert (Int.equal !status_probes 0);
    (match B.status bridge ~principal:"denied" ~profile:"one" with
     | Error Denied -> ()
     | _ -> failwith "status authorization bypassed");
    assert (Int.equal !status_probes 0);
    (match availability bridge "one" with
     | Unavailable Missing -> ()
     | _ -> failwith "uninitialized binding available");
    configure bridge "one" "enroll_one";
    configure bridge "two" "enroll_two";
    assert (Int.equal !status_probes 0);
    ignore (availability bridge "one" : B.Status.availability);
    assert (Int.equal !status_probes 1);
    let read_invoked = ref false in
    (match
       B.enroll
         bridge
         ~principal:"denied"
         ~profile:"one"
         ~operation:(id "denied")
         ~sw
         ~read:(fun ~sw:_ ->
           read_invoked := true;
           Error B.Error.Invalid_credential)
     with
     | Error Denied -> ()
     | _ -> failwith "authorization did not precede secure input");
    assert (not !read_invoked);
    ignore (B.remove bridge ~principal:"operator" ~profile:"one" ~sw |> ok : C.removal);
    assert (B.Status.equal_availability (availability bridge "one") Disabled);
    assert (B.Status.equal_availability (availability bridge "two") Configured);
    assert (Int.equal !lookups 0);
    C.close (S.Opened.registry opened);
    let reopened =
      open_host
        env
        sw
        anchor
        Existing
        ~mappings:[ one; two ]
        ~environment:(Some environment)
        ~authorize
      |> ok
      |> S.Opened.bridge
    in
    assert (B.Status.equal_availability (availability reopened "one") Disabled);
    assert (B.Status.equal_availability (availability reopened "two") Configured);
    assert (Int.equal !lookups 0);
    print_endline
      "disabled binding survives reopen; other profile stays configured; key lookups=0");
  [%expect
    {| disabled binding survives reopen; other profile stays configured; key lookups=0 |}]
;;

let%expect_test "Existing missing authority is typed and never enrolled by opening" =
  with_fixture (fun env sw anchor ->
    let mapping, _ = mapping "one" in
    match
      open_host env sw anchor Existing ~mappings:[ mapping ] ~environment:None ~authorize
    with
    | Error (Lifecycle (Storage Missing)) -> print_endline "setup required"
    | Error (Storage error)
      when Private_storage.Error.equal_code (Private_storage.Error.code error) Missing ->
      print_endline "setup required"
    | _ -> failwith "missing authority was fabricated");
  [%expect {| setup required |}]
;;

let%expect_test
    "disable and reenrollment invalidate held plans while the same context can prepare \
     again"
  =
  with_fixture (fun env sw anchor ->
    let mapping, identity = mapping "one" in
    let lookups = ref 0 in
    let environment =
      B.Environment.Entry.create
        ~binding:(id "one")
        ~identity
        ~name:"ONE"
        ~configuration_revision:None
        ~status:(fun () -> Available)
        ~resolve:(fun ~sw:_ ->
          Int.incr lookups;
          Ok
            (C.Environment.resolved
               ~access:
                 (Provider_secret_store.Secret.of_bytes (Bytes.of_string "synthetic-key")
                  |> ok)
               ~configuration_revision:None
               ~check_current:(fun () -> Ok ())))
      |> ok
      |> List.return
      |> B.Environment.create
      |> ok
    in
    let bridge =
      open_host
        env
        sw
        anchor
        (Initialize (id "incarnation"))
        ~mappings:[ mapping ]
        ~environment:(Some environment)
        ~authorize
      |> ok
      |> S.Opened.bridge
    in
    configure bridge "one" "first_enrollment";
    let target =
      B.capture
        bridge
        ~principal:"operator"
        ~default_profile:"one"
        ~current:None
        ~model:"model"
        ~settings:[]
      |> ok
    in
    let context = B.resolve bridge ~principal:"operator" target |> ok in
    let request = R.create ~target ~history:[] ~tools:[] ~assets:[] ~limits |> ok in
    let held = RT.Context.prepare context ~preparation_id:"held" request |> ok in
    ignore (B.remove bridge ~principal:"operator" ~profile:"one" ~sw |> ok : C.removal);
    configure bridge "one" "second_enrollment";
    ignore
      (RT.Context.prepare context ~preparation_id:"fresh" request |> ok : RT.Prepared.t);
    let scope =
      Transcript.Scope.create
        ~source:(Transcript.Source_id.of_string "bridge-test" |> ok)
        ~attempt:(Transcript.Attempt_id.of_string "held" |> ok)
        ~relation:Root
      |> ok
    in
    let attempt =
      RT.Prepared.start
        held
        ~scope
        ~accounting_id:(Inference.Observation.Observation_id.of_string "held" |> ok)
      |> ok
    in
    let terminal =
      RT.Attempt.run attempt ~sw ~on_event:ignore ~on_observation:ignore
      |> ok
      |> RT.Receipt.terminal
    in
    assert (Int.equal !lookups 0);
    assert (
      Inference.Event.Terminal.equal_delivery
        (Inference.Event.Terminal.delivery terminal)
        Definitely_not_submitted);
    (match Inference.Event.Terminal.outcome terminal with
     | Failed (Authentication Denied) -> ()
     | _ -> raise_s [%sexp (terminal : Inference.Event.Terminal.t)]);
    print_endline "old plan denied before key lookup; same context prepares a fresh plan");
  [%expect {| old plan denied before key lookup; same context prepares a fresh plan |}]
;;

let%expect_test
    "expired OAuth after restart refreshes during dispatch under a stable epoch"
  =
  with_fixture (fun env sw anchor ->
    let identity =
      M.Identity.oauth
        ~host
        ~provider:"openai"
        ~billing:"subscription"
        ~issuer:"https://issuer.example"
        ~client_registration:"fixture-client"
        ~resource:"https://resource.example"
        ~account:"fixture-account"
        ~verified_subject:"fixture-subject"
        ~required_scopes:[ "inference" ]
      |> ok
    in
    let profile =
      D.Profile.create
        ~id:"oauth"
        ~account:(Some "fixture-account")
        ~endpoint:"http://127.0.0.1:9/v1/responses"
        ~capabilities:
          (D.Capability.create ~baseline:[ Text_input, Supported ] ~models:[] |> ok)
        ~defaults:[]
      |> ok
    in
    let mapping =
      B.Mapping.create profile ~revision:"fixture-config" ~binding:(id "oauth") ~identity
      |> ok
    in
    let secret value =
      Provider_secret_store.Secret.of_bytes (Bytes.of_string value) |> ok
    in
    let verified ~expiry ~access ~refresh =
      let grant =
        M.Grant.create
          ~identity
          ~scopes:(Value [ "inference" ])
          ~expires_at_ms:(Value expiry)
          ~refresh_policy:Require_rotated
          ~effective:
            { scopes = [ "inference" ]
            ; scopes_provenance = Declared
            ; expiry = Known { at_ms = expiry; provenance = Declared }
            ; unknown_expiry_policy = Reject_unknown
            }
        |> ok
      in
      C.Verified.create
        ~identity
        ~grant:(Some grant)
        ~material:
          (C.Material.oauth
             ~access:(secret access)
             ~refresh:(Value (secret refresh))
             ~continuity:Absent)
      |> ok
    in
    let address_lookups = ref 0 in
    let net = Eio_mock.Net.make "oauth-refresh-dispatch" in
    Eio_mock.Net.on_getaddrinfo
      net
      [ `Run
          (fun () ->
            Int.incr address_lookups;
            [])
      ];
    let renewals = ref 0 in
    let leases = ref 0 in
    let old_epoch = ref None in
    let new_revision = ref None in
    let renewal =
      C.Renewal.create ~exchange:(fun ~sw:_ ~identity:actual ~grant:_ _ ->
        assert (M.Identity.equal actual identity);
        Int.incr renewals;
        C.Renewal.Verified
          (verified
             ~expiry:Int64.max_value
             ~access:"synthetic-new-access"
             ~refresh:"synthetic-new-refresh"))
    in
    let oauth =
      B.OAuth.create
        ~renewal:(fun actual ->
          if M.Identity.equal actual identity then Some renewal else None)
        ~lease:(fun admission ~identity:actual ~profile:_ ->
          assert (M.Identity.equal actual identity);
          assert (Option.equal Int64.equal !old_epoch (Some (C.Admission.epoch admission)));
          Int.incr leases;
          new_revision := C.Admission.credential_revision admission;
          C.Admission.with_access admission ~f:(fun access ->
            Provider_secret_store.Secret.with_string access ~f:D.Auth.bearer))
    in
    let first =
      open_host
        ~net:(net :> [ `Generic ] Eio.Net.ty Eio.Net.t)
        ~oauth
        env
        sw
        anchor
        (Initialize (id "incarnation"))
        ~mappings:[ mapping ]
        ~environment:None
        ~authorize
      |> ok
    in
    let registry = S.Opened.registry first in
    let candidate =
      C.begin_candidate
        registry
        ~binding:(id "oauth")
        ~operation:(id "login")
        ~expectation:(M.Expectation.exact identity)
      |> ok
    in
    C.commit_candidate
      registry
      candidate
      (verified
         ~expiry:0L
         ~access:"synthetic-old-access"
         ~refresh:"synthetic-old-refresh")
    |> ok;
    let before =
      C.synchronize registry |> ok |> C.Host_snapshot.bindings |> List.hd_exn
    in
    old_epoch := Some (C.Host_snapshot.epoch before);
    let old_revision = C.Host_snapshot.credential_revision before in
    C.close registry;
    let bridge =
      open_host
        ~net:(net :> [ `Generic ] Eio.Net.ty Eio.Net.t)
        ~oauth
        env
        sw
        anchor
        Existing
        ~mappings:[ mapping ]
        ~environment:None
        ~authorize
      |> ok
      |> S.Opened.bridge
    in
    (match availability bridge "oauth" with
     | Unavailable Renewal_required -> ()
     | _ -> failwith "expiry status hidden");
    let target =
      B.capture
        bridge
        ~principal:"operator"
        ~default_profile:"oauth"
        ~current:None
        ~model:"model"
        ~settings:[]
      |> ok
    in
    let context = B.resolve bridge ~principal:"operator" target |> ok in
    let request = R.create ~target ~history:[] ~tools:[] ~assets:[] ~limits |> ok in
    let prepared = RT.Context.prepare context ~preparation_id:"expired" request |> ok in
    let scope =
      Transcript.Scope.create
        ~source:(Transcript.Source_id.of_string "oauth-test" |> ok)
        ~attempt:(Transcript.Attempt_id.of_string "expired" |> ok)
        ~relation:Root
      |> ok
    in
    let attempt =
      RT.Prepared.start
        prepared
        ~scope
        ~accounting_id:(Inference.Observation.Observation_id.of_string "expired" |> ok)
      |> ok
    in
    let terminal =
      RT.Attempt.run attempt ~sw ~on_event:ignore ~on_observation:ignore
      |> ok
      |> RT.Receipt.terminal
    in
    (* The injected resolver deterministically returns no addresses. Reaching its
       one lookup proves the fresh lease passed admission/currentness without
       contacting any real socket or relying on an unused local port. *)
    (match Inference.Event.Terminal.outcome terminal with
     | Failed (Transport Connection) -> ()
     | _ -> raise_s [%sexp (terminal : Inference.Event.Terminal.t)]);
    assert (Int.equal !renewals 1);
    assert (Int.equal !leases 1);
    assert (Int.equal !address_lookups 1);
    assert (Option.is_some !new_revision);
    assert (not (Option.equal String.equal old_revision !new_revision));
    print_endline
      "one refresh; stable authorization epoch; new credential revision; no login port");
  [%expect
    {|
    +oauth-refresh-dispatch: getaddrinfo ~service:9 127.0.0.1
    one refresh; stable authorization epoch; new credential revision; no login port |}]
;;
