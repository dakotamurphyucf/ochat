open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model
module C = Credential_registry
module S = Private_storage
module Secret = Provider_secret_store
module B = Inference_host.Credential_bridge
module A = Provider_operator.Profile_admin
module Actor = Operator_authorization
module Runtime = Provider_runtime
module Backend = Inference_host.Backend
module RT = Inference_runtime

let ok r =
  Result.map_error r ~f:(fun _ -> "synthetic runtime-host fixture failed")
  |> Result.ok_or_failwith
;;

let id s = M.Id.create s |> ok
let profile s = DTO.Profile_id.of_string s |> ok
let key s = P.Idempotency_key.of_string s |> ok

let principal =
  P.Principal.create
    ~id:(P.Id.Principal.of_string "pri_operator" |> ok)
    ~authentication_kind:"synthetic"
    ~scopes:(P.Scope.Set.of_list [ Provider_view; Provider_manage; Provider_select ])
    ~attributes:[]
  |> ok
;;

let denied =
  P.Principal.create
    ~id:(P.Id.Principal.of_string "pri_denied" |> ok)
    ~authentication_kind:"synthetic"
    ~scopes:P.Scope.Set.empty
    ~attributes:[]
  |> ok
;;

let actor = Actor.trusted_local principal
let denied_actor = Actor.trusted_local denied

let authorized actor =
  Actor.is_current actor && P.Id.Principal.equal (Actor.principal actor).id principal.id
;;

let components = [ S.Name.create "authority" |> ok ]
let namespace = Secret.Namespace.create "runtime-host-test" |> ok
let server_id = P.Id.Server.of_string "srv_runtime_host" |> ok

let with_fixture f =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    let suffix = P.Id.Transaction.create () |> P.Id.Transaction.to_string in
    let anchor =
      Eio.Path.(Eio.Stdenv.fs env / "/tmp" / ("ochat-runtime-host-" ^ suffix))
    in
    Eio.Path.mkdir ~perm:0o700 anchor;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let driver =
            Openai.Responses_driver.create
              ~net:(Eio.Stdenv.net env)
              ~clock:(Eio.Stdenv.clock env)
              ()
            |> ok
          in
          let transport =
            Provider_oauth.Transport.create
              ~net:(Eio.Stdenv.net env)
              ~clock:(Eio.Stdenv.mono_clock env)
            |> ok
          in
          Exn.protect
            ~finally:(fun () -> Provider_oauth.Transport.close transport)
            ~f:(fun () ->
              let oauth =
                Provider_oauth_registry.create
                  ~transport
                  ~policy:
                    (Provider_oauth.Policy.direct_codex
                       ~expected_account:None
                       ~callback_port:1455
                       ()
                     |> ok)
                  ~wall_clock:(Eio.Stdenv.clock env)
              in
              let make name =
                let identity =
                  M.Identity.api_key
                    ~host:(id "host")
                    ~provider:"openai"
                    ~billing:"api"
                    ~account:None
                    ~key_reference:(id name)
                  |> ok
                in
                let route =
                  Openai.Responses_driver.Profile.create
                    ~id:name
                    ~account:None
                    ~endpoint:"http://127.0.0.1:9/v1/responses"
                    ~capabilities:
                      (Openai.Responses_driver.Capability.create
                         ~baseline:[ Text_input, Supported ]
                         ~models:[]
                       |> ok)
                    ~defaults:[]
                  |> ok
                in
                let mapping =
                  B.Mapping.create
                    route
                    ~revision:"configured"
                    ~binding:(id name)
                    ~identity
                  |> ok
                in
                let template =
                  A.Template.create
                    ~profile:(profile name)
                    ~binding:(id name)
                    ~revision:(DTO.Revision.of_string "configured" |> ok)
                    ~authentication:Api_key
                    ~expectation:(M.Expectation.exact identity)
                    ~expected_account:None
                    ~mapping:(fun actual ->
                      if M.Identity.equal identity actual
                      then Ok mapping
                      else Error B.Error.Invalid_mapping)
                  |> ok
                in
                template, mapping
              in
              let declarations = List.map [ "one"; "two" ] ~f:make in
              let serial = ref 0
              and login_calls = ref 0 in
              let new_operation () =
                incr serial;
                id (sprintf "operation-%d" !serial)
              in
              let create () =
                Provider_runtime_host.create
                  ~sw
                  ~env
                  ~server_id
                  ~anchor
                  ~components
                  ~host:(id "host")
                  ~secret_namespace:namespace
                  ~driver
                  ~templates:(List.map declarations ~f:fst)
                  ~mappings:(List.map declarations ~f:snd)
                  ~default_profile:(profile "one")
                  ~environment:None
                  ~environment_sources:[]
                  ~oauth
                  ~oauth_lease:None
                  ~start_login:(fun ~sw:_ ~template:_ ~mode:_ ->
                    incr login_calls;
                    failwith "unexpected OAuth login")
                  ~inference_principal:"operator"
                  ~authorize_bridge:(fun ~principal ~profile:_ ~operation:_ ->
                    String.equal principal "operator"
                    || String.equal principal "pri_operator")
                  ~authorize:(fun p ~operation:_ ~profile:_ -> authorized p)
                  ~authorize_setup:authorized
                  ~authorize_status:authorized
                  ~new_operation
                  ~new_revision:(fun () ->
                    incr serial;
                    DTO.Revision.of_string (sprintf "revision-%d" !serial) |> ok)
                  ~maximum_wait:(Time_ns.Span.of_sec 0.05)
                  ~limits:DTO.Limits.default
                  ~inference_limits:RT.Limits.default
                  ~transport_policy:Http_sse
                |> ok
              in
              f env sw anchor create login_calls))))
;;

let setup_request = { DTO.Setup_request.idempotency_key = key "original-setup" }

let setup runtime =
  Runtime.dispatch runtime ~actor (P.Command.Provider_setup setup_request) |> ok
;;

let status runtime =
  match
    Runtime.dispatch runtime ~actor (P.Command.Provider_status { profile = None }) |> ok
  with
  | P.Method_result.Provider_status result -> result
  | _ -> failwith "wrong status result"
;;

let enroll runtime ~sw name key_name reads =
  Runtime.enroll_private_key
    runtime
    ~actor
    ~profile:(profile name)
    ~key:(key key_name)
    ~source_reference:("synthetic:" ^ key_name)
    ~sw
    ~read:(fun ~sw:_ ->
      incr reads;
      Secret.Secret.of_bytes (Bytes.of_string "synthetic-protected-key")
      |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential))
  |> ok
;;

let capture runtime =
  Backend.capture (Runtime.backend runtime) ~current:None ~model:"synthetic" ~settings:[]
  |> ok
;;

let%expect_test
    "actual host setup is authorized, explicit, shared and survives lost reply/restart"
  =
  with_fixture (fun _ sw anchor create login_calls ->
    let runtime = create () in
    assert (status runtime).setup_required;
    (match
       Runtime.dispatch
         runtime
         ~actor:denied_actor
         (P.Command.Provider_setup setup_request)
     with
     | Error Denied -> ()
     | _ -> failwith "denied setup created authority");
    let reads = ref 0 in
    assert (
      not (Eio.Path.is_file Eio.Path.(anchor / "authority" / "provider-registry.json")));
    (* Drop the setup response, then reopen the same actual factory. *)
    ignore (setup runtime : P.Method_result.t);
    Runtime.close runtime;
    let reopened = create () in
    assert (not (status reopened).setup_required);
    ignore (setup reopened : P.Method_result.t);
    (match
       Runtime.enroll_private_key
         reopened
         ~actor:denied_actor
         ~profile:(profile "one")
         ~key:(key "denied-input")
         ~source_reference:"synthetic"
         ~sw
         ~read:(fun ~sw:_ ->
           incr reads;
           Error B.Error.Invalid_credential)
     with
     | Error Denied -> ()
     | _ -> failwith "denied input not rejected");
    assert (!reads = 0);
    ignore (enroll reopened ~sw "one" "key-one" reads : DTO.Configuration_result.t);
    assert (!reads = 1);
    assert (String.equal (Inference.Request.Target.profile (capture reopened)) "one");
    Runtime.close reopened;
    let final = create () in
    assert (not (status final).setup_required);
    assert (String.equal (Inference.Request.Target.profile (capture final)) "one");
    assert (!reads = 1 && !login_calls = 0));
  print_endline
    "denied setup/input had no effects; exact setup recovered; protected enrollment \
     survives reopen";
  [%expect
    {| denied setup/input had no effects; exact setup recovered; protected enrollment survives reopen |}]
;;

let%expect_test
    "selection changes fresh capture while held plans fail after credential replacement"
  =
  with_fixture (fun _ sw _ create login_calls ->
    let runtime = create () in
    ignore (setup runtime : P.Method_result.t);
    let reads = ref 0 in
    ignore (enroll runtime ~sw "one" "one-first" reads : DTO.Configuration_result.t);
    ignore (enroll runtime ~sw "two" "two-first" reads : DTO.Configuration_result.t);
    let target = capture runtime in
    let context = Backend.resolve (Runtime.backend runtime) target |> ok in
    let request =
      Inference.Request.create
        ~target
        ~history:[]
        ~tools:[]
        ~assets:[]
        ~limits:Document_schema.Limits.default
      |> ok
    in
    let held = RT.Context.prepare context ~preparation_id:"held" request |> ok in
    let selection = (status runtime).selection |> Option.value_exn in
    ignore
      (Runtime.dispatch
         runtime
         ~actor
         (P.Command.Provider_select
            { profile = profile "two"
            ; expected_revision = selection.revision
            ; idempotency_key = key "select-two"
            })
       |> ok
       : P.Method_result.t);
    assert (String.equal (Inference.Request.Target.profile (capture runtime)) "two");
    let preserved =
      Backend.capture
        (Runtime.backend runtime)
        ~current:(Some target)
        ~model:"synthetic"
        ~settings:[]
      |> ok
    in
    assert (String.equal (Inference.Request.Target.profile preserved) "one");
    ignore (enroll runtime ~sw "one" "one-replacement" reads : DTO.Configuration_result.t);
    let scope =
      Transcript.Scope.create
        ~source:(Transcript.Source_id.of_string "host-test" |> ok)
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
    (match Inference.Event.Terminal.outcome terminal with
     | Failed (Authentication Denied) -> ()
     | _ -> failwith "old held plan dispatched");
    assert (!login_calls = 0));
  print_endline
    "selection affects fresh capture; recapture preserves identity; replacement denies \
     old plan before dispatch";
  [%expect
    {| selection affects fresh capture; recapture preserves identity; replacement denies old plan before dispatch |}]
;;

let%expect_test
    "partial original setup publishes remaining profile metadata without replacing \
     registry"
  =
  with_fixture (fun env sw anchor create login_calls ->
    let directory = S.Directory.open_or_create ~sw ~anchor ~components |> ok in
    let intents =
      Provider_operator.Command_intents.create
        directory
        ~host:(id "host")
        ~maximum_records:1024
      |> ok
    in
    let command = P.Command.Provider_setup setup_request in
    let original = id "partial-original-incarnation" in
    (match
       Provider_operator.Command_intents.begin_
         intents
         ~principal:principal.id
         ~key:setup_request.idempotency_key
         ~method_name:(P.Command.method_name command)
         ~params:(P.Command.params command)
         ~operation:original
       |> ok
     with
     | Fresh _ -> ()
     | Existing _ -> failwith "unexpected preexisting setup");
    let secrets = Secret.open_private_files ~sw ~directory ~namespace |> ok in
    let registry =
      C.initialize_new
        ~metadata_admission:C.Metadata_admission.nonblocking
        ~sw
        ~wall_clock:(Eio.Stdenv.clock env)
        ~new_operation:(fun () -> id "unused")
        ~directory
        ~secrets
        ~environment:None
        ~host:(id "host")
        ~incarnation:original
      |> ok
    in
    (* Exact durable prefix of factory setup: intent + registry committed,
       profile metadata absent. This simulates restart after that prefix, not
       an injected filesystem/cancellation failure. *)
    C.close registry;
    Secret.close secrets;
    let runtime = create () in
    assert (status runtime).setup_required;
    (match
       Runtime.dispatch
         runtime
         ~actor
         (P.Command.Provider_setup { idempotency_key = key "different-setup" })
     with
     | Error Submission_uncertain -> ()
     | _ -> failwith "different setup adopted partial authority");
    let result = setup runtime in
    (match result with
     | P.Method_result.Provider_setup result ->
       assert (
         String.equal (DTO.Revision.to_string result.revision) (M.Id.to_string original))
     | _ -> failwith "wrong setup result");
    assert (not (status runtime).setup_required);
    Runtime.close runtime;
    let reopened = create () in
    assert (not (status reopened).setup_required);
    (match Runtime.receipt reopened ~actor command |> ok with
     | Committed (Provider_setup result) ->
       assert (
         String.equal (DTO.Revision.to_string result.revision) (M.Id.to_string original))
     | _ -> failwith "exact original receipt unavailable");
    assert (!login_calls = 0));
  print_endline
    "partial setup recovered original incarnation; unrelated owner operation cannot \
     adopt it";
  [%expect
    {| partial setup recovered original incarnation; unrelated owner operation cannot adopt it |}]
;;

exception Synthetic_request_cancelled

let%expect_test "runtime request cancellation leaves host-owned setup joined by close" =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let entered, entered_u = Eio.Promise.create () in
      let blocked, _ = Eio.Promise.create () in
      let cleaned = ref 0 in
      (* Only the explicit Runtime initialization port is substituted here.
         It cannot create an alternate authority or produce a fake Opened. *)
      let runtime =
        Runtime.create
          ~sw
          ~server_id
          ~authorize_setup:authorized
          ~authorize_status:authorized
          ~setup_receipt:(fun ~actor:_ _ -> Ok P.Command_receipt.Missing)
          ~existing:(fun ~sw:_ -> Ok None)
          ~initialize:(fun ~sw:_ ~actor:_ _ ->
            Exn.protect
              ~finally:(fun () -> incr cleaned)
              ~f:(fun () ->
                Eio.Promise.resolve entered_u ();
                Eio.Promise.await blocked;
                Error DTO.Error.Store_unavailable))
        |> ok
      in
      let caller_switch, caller_switch_u = Eio.Promise.create () in
      let caller_done, caller_done_u = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Exn.protect
          ~finally:(fun () -> Eio.Promise.resolve caller_done_u ())
          ~f:(fun () ->
            try
              Eio.Switch.run (fun caller_sw ->
                Eio.Promise.resolve caller_switch_u caller_sw;
                ignore (setup runtime : P.Method_result.t))
            with
            | Synthetic_request_cancelled -> ()));
      Eio.Promise.await entered;
      Eio.Switch.fail (Eio.Promise.await caller_switch) Synthetic_request_cancelled;
      Eio.Promise.await caller_done;
      assert (!cleaned = 0);
      Runtime.close runtime;
      assert (!cleaned = 1);
      match Runtime.dispatch runtime ~actor (P.Command.Provider_setup setup_request) with
      | Error Closed -> ()
      | _ -> failwith "closed host admitted setup"));
  print_endline
    "cancelled caller returns; host close cancels and joins exactly one blocked \
     initializer";
  [%expect
    {| cancelled caller returns; host close cancels and joins exactly one blocked initializer |}]
;;
