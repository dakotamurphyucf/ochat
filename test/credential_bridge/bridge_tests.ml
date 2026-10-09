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
    "verified OAuth choice after restart refreshes once under the canonical epoch"
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
    let choices =
      B.Compatible_profile.of_string
        {|[{"id":"oauth-choice","credential_owner":"oauth","revision":"choice-v1","defaults":{}}]|}
      |> ok
    in
    let compose registry mappings =
      B.create
        ~oauth
        ~compatible_profiles:choices
        ~approved_profiles:[ "oauth" ]
        (D.create
           ~net:(net :> [ `Generic ] Eio.Net.ty Eio.Net.t)
           ~clock:(Eio.Stdenv.clock env)
           ()
         |> ok)
        ~registry
        ~mappings
        ~authorize
        ~clock:(Eio.Stdenv.mono_clock env)
        ~maximum_wait:(Time_ns.Span.of_sec 0.05)
        ~transport_policy:Http_sse
        ~limits:RT.Limits.default
      |> ok
    in
    let pending = compose registry [] in
    assert (
      Result.is_error
        (B.capture
           pending
           ~principal:"operator"
           ~default_profile:"oauth-choice"
           ~current:None
           ~model:"model"
           ~settings:[]));
    assert (Result.is_error (B.publish_mapping pending mapping));
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
    B.publish_mapping pending mapping |> ok;
    let verified_choice =
      B.capture
        pending
        ~principal:"operator"
        ~default_profile:"oauth-choice"
        ~current:None
        ~model:"model"
        ~settings:[]
      |> ok
    in
    assert (
      Option.equal
        String.equal
        (R.Target.account verified_choice)
        (Some "fixture-account"));
    let before =
      C.synchronize registry |> ok |> C.Host_snapshot.bindings |> List.hd_exn
    in
    old_epoch := Some (C.Host_snapshot.epoch before);
    let old_revision = C.Host_snapshot.credential_revision before in
    C.close registry;
    let reopened =
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
    in
    let bridge = compose (S.Opened.registry reopened) [ mapping ] in
    (match availability bridge "oauth-choice" with
     | Unavailable Renewal_required -> ()
     | _ -> failwith "expiry status hidden");
    let target =
      B.capture
        bridge
        ~principal:"operator"
        ~default_profile:"oauth-choice"
        ~current:None
        ~model:"model"
        ~settings:[]
      |> ok
    in
    let canonical_target =
      B.capture
        bridge
        ~principal:"operator"
        ~default_profile:"oauth"
        ~current:None
        ~model:"model"
        ~settings:[]
      |> ok
    in
    (match R.Target.auth_binding target, R.Target.auth_binding canonical_target with
     | Value choice, Value canonical -> assert (R.Auth_binding.equal choice canonical)
     | _ -> failwith "choice did not retain canonical OAuth binding");
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

let%expect_test "compatible choices share binding without inheriting authorization" =
  with_fixture (fun env sw anchor ->
    let canonical, identity = mapping "one" in
    let environment =
      B.Environment.Entry.create
        ~binding:(id "one")
        ~identity
        ~name:"ONE"
        ~configuration_revision:None
        ~resolve:(fun ~sw:_ ->
          Ok
            (C.Environment.resolved
               ~access:
                 (Provider_secret_store.Secret.of_bytes (Bytes.of_string "synthetic-key")
                  |> ok)
               ~configuration_revision:None
               ~check_current:(fun () -> Ok ())))
        ~status:(fun () -> Available)
      |> ok
      |> List.return
      |> B.Environment.create
      |> ok
    in
    let opened =
      open_host
        env
        sw
        anchor
        (Initialize (id "incarnation"))
        ~mappings:[ canonical ]
        ~environment:(Some environment)
        ~authorize
      |> ok
    in
    let choices =
      B.Compatible_profile.of_string
        {|[{"id":"alternative","credential_owner":"one","revision":"choice-v1","defaults":{}}]|}
      |> ok
    in
    let allowed = ref (String.Set.of_list [ "one"; "alternative" ]) in
    let bridge =
      B.create
        ~compatible_profiles:choices
        ~approved_profiles:[ "one"; "unmapped-oauth" ]
        (D.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) () |> ok)
        ~registry:(S.Opened.registry opened)
        ~mappings:[ canonical ]
        ~authorize:(fun ~principal ~profile ~operation:_ ->
          String.equal principal "operator" && Set.mem !allowed profile)
        ~clock:(Eio.Stdenv.mono_clock env)
        ~maximum_wait:(Time_ns.Span.of_sec 0.05)
        ~transport_policy:Http_sse
        ~limits:RT.Limits.default
      |> ok
    in
    configure bridge "one" "configure-owner";
    let capture profile =
      B.capture
        bridge
        ~principal:"operator"
        ~default_profile:profile
        ~current:None
        ~model:"model"
        ~settings:[]
    in
    let one = capture "one" |> ok
    and alternative = capture "alternative" |> ok in
    print_s
      [%sexp
        { distinct_profile =
            (not (String.equal (R.Target.profile one) (R.Target.profile alternative))
             : bool)
        ; same_endpoint =
            (String.equal (R.Target.endpoint one) (R.Target.endpoint alternative) : bool)
        ; same_account =
            (Option.equal
               String.equal
               (R.Target.account one)
               (R.Target.account alternative)
             : bool)
        ; same_binding =
            ((match R.Target.auth_binding one, R.Target.auth_binding alternative with
              | Value one, Value alternative -> R.Auth_binding.equal one alternative
              | Absent, _ | Null, _ | Value _, (Absent | Null) -> false)
             : bool)
        }];
    allowed := String.Set.singleton "one";
    assert (Result.is_error (capture "alternative"));
    allowed := String.Set.singleton "alternative";
    assert (Result.is_error (capture "alternative"));
    allowed := String.Set.of_list [ "one"; "alternative" ];
    let read = ref false in
    assert (
      Result.is_error
        (B.enroll
           bridge
           ~principal:"operator"
           ~profile:"alternative"
           ~operation:(id "choice-enroll")
           ~sw
           ~read:(fun ~sw:_ ->
             read := true;
             Error B.Error.Invalid_credential)));
    assert (not !read);
    assert (
      Result.is_error (B.remove bridge ~principal:"operator" ~profile:"alternative" ~sw));
    let held_context = B.resolve bridge ~principal:"operator" alternative |> ok in
    let held_request =
      R.create ~target:alternative ~history:[] ~tools:[] ~assets:[] ~limits |> ok
    in
    let held =
      RT.Context.prepare held_context ~preparation_id:"before-owner-epoch" held_request
      |> ok
    in
    let view = B.with_response_limit bridge ~max_body_bytes:1024 |> ok in
    ignore (B.remove bridge ~principal:"operator" ~profile:"one" ~sw |> ok : C.removal);
    assert (
      Result.is_error
        (B.capture
           view
           ~principal:"operator"
           ~default_profile:"alternative"
           ~current:None
           ~model:"model"
           ~settings:[]));
    assert (B.Status.equal_availability (availability bridge "alternative") Disabled);
    configure bridge "one" "owner-reenrolled";
    ignore
      (B.capture
         view
         ~principal:"operator"
         ~default_profile:"alternative"
         ~current:None
         ~model:"model"
         ~settings:[]
       |> ok
       : R.Target.t);
    let scope =
      Transcript.Scope.create
        ~source:(Transcript.Source_id.of_string "choice-epoch" |> ok)
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
    assert (
      Inference.Event.Terminal.equal_delivery
        (Inference.Event.Terminal.delivery terminal)
        Definitely_not_submitted);
    print_s [%sexp { held_choice_epoch_rejected = true; shared_view_reenrollment = true }];
    print_s
      [%sexp
        { dual_authorization = true
        ; choice_cannot_enroll = true
        ; owner_disable_shared = true
        }]);
  [%expect
    {|
    ((distinct_profile true) (same_endpoint true) (same_account true)
     (same_binding true))
    ((held_choice_epoch_rejected true) (shared_view_reenrollment true))
    ((dual_authorization true) (choice_cannot_enroll true)
     (owner_disable_shared true))
  |}]
;;

let%expect_test
    "compatible authored descriptors reject identity policy and malformed defaults"
  =
  let bad =
    [ {|[{"id":"choice","credential_owner":"one","revision":"r","defaults":{},"account":"guess"}]|}
    ; {|[{"id":"one","credential_owner":"one","revision":"r","defaults":{}}]|}
    ; {|[{"id":"choice","credential_owner":"one","revision":"r","defaults":{"temperature":"wrong"}}]|}
    ; {|[{"id":"choice","credential_owner":"one","revision":"r","defaults":{"unknown":true}}]|}
    ; {|[{"id":"choice","credential_owner":"one","revision":"r","defaults":{"temperature":0.5,"temperature":0.6}}]|}
    ; {|[{"id":"choice","credential_owner":"one","revision":"r","defaults":{}},{"id":"choice","credential_owner":"one","revision":"r","defaults":{}}]|}
    ]
  in
  List.iter bad ~f:(fun json ->
    assert (Result.is_error (B.Compatible_profile.of_string json)));
  assert (Result.is_error (B.Compatible_profile.of_string (String.make 1_048_577 ' ')));
  let declaration id owner =
    B.Compatible_profile.create ~id ~credential_owner:owner ~revision:"r" ~defaults:[]
    |> ok
  in
  assert (
    Result.is_error
      (B.Compatible_profile.validate_set
         [ declaration "choice" "unknown" ]
         ~credential_owners:[ "one" ]));
  assert (
    Result.is_error
      (B.Compatible_profile.validate_set
         [ declaration "choice" "one"; declaration "other" "choice" ]
         ~credential_owners:[ "one" ]));
  let many =
    List.init 128 ~f:(fun index -> declaration (sprintf "choice-%d" index) "one")
  in
  assert (
    Result.is_error (B.Compatible_profile.validate_set many ~credential_owners:[ "one" ]));
  print_s [%sexp { malformed_cases = (List.length bad : int); oversized = true }];
  [%expect {| ((malformed_cases 6) (oversized true)) |}]
;;

let%expect_test "derived choice retains qualified policies and cannot guess account" =
  let capabilities =
    D.Capability.create
      ~baseline:
        [ Text_input, Supported
        ; Websocket, Unsupported
        ; Setting "temperature", Supported
        ]
      ~models:[]
    |> ok
  in
  let canonical =
    D.Profile.create
      ~id:"canonical"
      ~account:(Some "verified-account")
      ~endpoint:"http://127.0.0.1:9/v1/responses"
      ~capabilities
      ~defaults:[]
    |> ok
    |> fun profile ->
    D.Profile.with_response_content_type_policy profile Allow_absent_event_stream
  in
  let choice =
    B.Compatible_profile.of_string
      {|[{"id":"choice","credential_owner":"canonical","revision":"r","defaults":{"temperature":0.5}}]|}
    |> ok
    |> List.hd_exn
  in
  let derived = B.Compatible_profile.derive choice ~canonical |> ok in
  assert (
    Option.equal String.equal (D.Profile.account derived) (D.Profile.account canonical));
  assert (String.equal (D.Profile.endpoint derived) (D.Profile.endpoint canonical));
  assert (
    D.Capability.equal_support
      (D.Profile.capability derived ~model:"any" ~feature:Websocket)
      Unsupported);
  assert (
    D.Profile.Response_content_type_policy.equal
      (D.Profile.response_content_type_policy derived)
      (D.Profile.response_content_type_policy canonical));
  let defaults = D.Profile.effective_settings derived [] |> ok in
  assert (List.length defaults = 1);
  let unrelated =
    D.Profile.with_configuration canonical ~id:"unrelated" ~defaults:[] |> ok
  in
  assert (Result.is_error (B.Compatible_profile.derive choice ~canonical:unrelated));
  print_s
    [%sexp
      { inherited_identity = true
      ; no_capability_expansion = true
      ; inherited_transport_policy = true
      ; distinct_defaults = true
      ; wrong_owner_rejected = true
      }];
  [%expect
    {|
    ((inherited_identity true) (no_capability_expansion true)
     (inherited_transport_policy true) (distinct_defaults true)
     (wrong_owner_rejected true))
  |}]
;;

let%expect_test "serialized choice revision binds canonical configuration across restart" =
  with_fixture (fun env sw anchor ->
    let canonical, identity = mapping "one" in
    let environment =
      B.Environment.Entry.create
        ~binding:(id "one")
        ~identity
        ~name:"ONE"
        ~configuration_revision:None
        ~resolve:(fun ~sw:_ ->
          Ok
            (C.Environment.resolved
               ~access:
                 (Provider_secret_store.Secret.of_bytes (Bytes.of_string "synthetic-key")
                  |> ok)
               ~configuration_revision:None
               ~check_current:(fun () -> Ok ())))
        ~status:(fun () -> Available)
      |> ok
      |> List.return
      |> B.Environment.create
      |> ok
    in
    let choices =
      B.Compatible_profile.of_string
        {|[{"id":"choice","credential_owner":"one","revision":"declared-v1","defaults":{}}]|}
      |> ok
    in
    let opened =
      open_host
        env
        sw
        anchor
        (Initialize (id "incarnation"))
        ~mappings:[ canonical ]
        ~environment:(Some environment)
        ~authorize
      |> ok
    in
    let compose opened mapping =
      B.create
        ~compatible_profiles:choices
        (D.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) () |> ok)
        ~registry:(S.Opened.registry opened)
        ~mappings:[ mapping ]
        ~authorize
        ~clock:(Eio.Stdenv.mono_clock env)
        ~maximum_wait:(Time_ns.Span.of_sec 0.05)
        ~transport_policy:Http_sse
        ~limits:RT.Limits.default
      |> ok
    in
    let bridge = compose opened canonical in
    configure bridge "one" "configure-owner";
    let capture bridge name =
      B.capture
        bridge
        ~principal:"operator"
        ~default_profile:name
        ~current:None
        ~model:"model"
        ~settings:[]
      |> ok
    in
    let canonical_target = capture bridge "one" in
    let saved =
      capture bridge "choice"
      |> R.Target.to_json
      |> Jsonaf.to_string
      |> Jsonaf.of_string
      |> fun json -> R.Target.of_json json ~limits |> ok
    in
    let changed_profile =
      D.Profile.create
        ~id:"one"
        ~account:None
        ~endpoint:"http://127.0.0.1:9/v1/responses"
        ~capabilities:
          (D.Capability.create ~baseline:[ Text_input, Supported ] ~models:[] |> ok)
        ~defaults:[]
      |> ok
    in
    let changed =
      B.Mapping.create
        changed_profile
        ~revision:"canonical-v2"
        ~binding:(id "one")
        ~identity
      |> ok
    in
    B.publish_mapping bridge changed |> ok;
    let require_stale bridge =
      match B.resolve bridge ~principal:"operator" saved with
      | Error (Profile Incompatible_identity) -> ()
      | _ -> failwith "saved old choice accepted new canonical configuration"
    in
    require_stale bridge;
    assert (
      Result.is_error
        (B.capture
           bridge
           ~principal:"operator"
           ~default_profile:"one"
           ~current:(Some saved)
           ~model:"model"
           ~settings:[]));
    ignore (B.resolve bridge ~principal:"operator" canonical_target |> ok : RT.Context.t);
    let fresh = capture bridge "choice" in
    assert (
      not
        (Option.equal
           String.equal
           (R.Target.profile_revision saved)
           (R.Target.profile_revision fresh)));
    C.close (S.Opened.registry opened);
    let reopened =
      open_host
        env
        sw
        anchor
        Existing
        ~mappings:[ changed ]
        ~environment:(Some environment)
        ~authorize
      |> ok
    in
    let restarted = compose reopened changed in
    require_stale restarted;
    let restored_fresh = capture restarted "choice" in
    assert (
      Option.equal
        String.equal
        (R.Target.profile_revision fresh)
        (R.Target.profile_revision restored_fresh));
    ignore (B.resolve restarted ~principal:"operator" restored_fresh |> ok : RT.Context.t);
    print_s
      [%sexp
        { changed_revision = true
        ; saved_choice_rejected = true
        ; recapture_rejected = true
        ; canonical_provenance_preserved = true
        ; restart_stable = true
        }]);
  [%expect
    {|
    ((changed_revision true) (saved_choice_rejected true)
     (recapture_rejected true) (canonical_provenance_preserved true)
     (restart_stable true))
  |}]
;;
