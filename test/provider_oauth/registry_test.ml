open! Core
module O = Provider_oauth
module A = Provider_oauth_registry
module C = Credential_registry
module M = Credential_registry_model
module S = Private_storage
module B = Provider_secret_store
module D = Openai.Responses_driver

let model = function
  | Ok value -> value
  | Error error -> raise_s (M.Error.sexp_of_t error)
;;

let storage = function
  | Ok value -> value
  | Error error -> raise_s (S.Error.sexp_of_t error)
;;

let backend = function
  | Ok value -> value
  | Error error -> raise_s (B.Error.sexp_of_t error)
;;

let registry = function
  | Ok value -> value
  | Error error -> raise_s (C.Error.sexp_of_t error)
;;

let adapter = function
  | Ok value -> value
  | Error error -> raise_s (A.Error.sexp_of_t error)
;;

let id name = M.Id.create name |> model
let host = id "oauth_synthetic_host"
let binding = id "oauth_synthetic_binding"

let with_registry ?wall_clock ?(prepare_clock = fun _ -> ()) f =
  Eio_main.run (fun env ->
    prepare_clock env;
    let wall_clock = Option.value wall_clock ~default:(Eio.Stdenv.clock env) in
    let path =
      Eio_unix.run_in_systhread (fun () ->
        Core_unix.mkdtemp "/tmp/ochat-oauth-adapter-XXXXXX")
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
              ~components:[ S.Name.create "owned" |> storage ]
            |> storage
          in
          let secrets =
            B.open_private_files
              ~sw
              ~directory
              ~namespace:(B.Namespace.create "oauth_synthetic" |> backend)
            |> backend
          in
          let serial = ref 0 in
          let new_operation () =
            incr serial;
            id (sprintf "operation_%d" !serial)
          in
          let current =
            C.initialize_new
              ~sw
              ~wall_clock
              ~new_operation
              ~directory
              ~secrets
              ~environment:None
              ~host
              ~incarnation:(id "incarnation")
            |> registry
          in
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
            f env sw directory secrets new_operation current))))
;;

let snapshot current =
  C.synchronize current
  |> registry
  |> C.Host_snapshot.bindings
  |> List.find_exn ~f:(fun value -> M.Id.equal (C.Host_snapshot.id value) binding)
;;

let expectation =
  M.Expectation.oauth_acquisition
    ~host
    ~provider:"openai"
    ~billing:"subscription"
    ~issuer:(O.Policy.issuer Flow_test.policy)
    ~client_registration:(O.Policy.client_registration Flow_test.policy)
    ~resource:(O.Policy.resource Flow_test.policy)
    ~account:None
    ~required_scopes:[ "openid"; "profile"; "email"; "offline_access" ]
  |> model
;;

let start ?wall_clock env sw oauth current operation transport =
  let wall_clock = Option.value wall_clock ~default:(Eio.Stdenv.clock env) in
  A.Acquisition.start
    oauth
    ~registry:current
    ~sw
    ~host
    ~binding
    ~operation
    ~expectation
    ~refresh_policy:Preserve_omitted
    ~start:(fun ~sw ->
      O.Login.start_device
        ~transport
        ~policy:Flow_test.policy
        ~sw
        ~clock:(Eio.Stdenv.mono_clock env)
        ~wall_clock
        ~maximum_wait:(Time_ns.Span.of_sec 10.))
  |> adapter
;;

let set_mock_wall_time env clock time =
  let quiet =
    { Eio.Debug.traceln = (fun ?__POS__:_ fmt -> Format.ifprintf Format.err_formatter fmt)
    }
  in
  Eio.Fiber.with_binding (Eio.Stdenv.debug env)#traceln quiet (fun () ->
    Eio_mock.Clock.set_time clock time)
;;

let%expect_test
    "synthetic login commit survives restart and exact renewal changes only revision"
  =
  let mock_clock = Eio_mock.Clock.make () in
  let wall_clock = (mock_clock :> float Eio.Time.clock_ty Eio.Resource.t) in
  let now = 1_700_000_000. in
  with_registry
    ~wall_clock
    ~prepare_clock:(fun env -> set_mock_wall_time env mock_clock now)
    (fun env sw directory secrets new_operation current ->
       let exchanges = ref 0 in
       let original = Flow_test.tokens env ~now ~lifetime:1L () in
       let renewed =
         Flow_test.modify_json
           (Flow_test.tokens env ~now ())
           ~remove:[ "id_token"; "scope"; "refresh_token"; "expires_in" ]
           ~replace:[]
       in
       let transport =
         O.For_testing.scripted_transport
           ~clock:(Eio.Stdenv.mono_clock env)
           (fun endpoint ~body:_ ~on_possible_submission ->
              match endpoint with
              | User_code -> Ok (200, Flow_test.challenge)
              | Device_poll -> Ok (200, Flow_test.grant)
              | Token ->
                on_possible_submission ();
                incr exchanges;
                Ok (200, if !exchanges = 1 then original else renewed))
       in
       let oauth = A.create ~transport ~policy:Flow_test.policy ~wall_clock in
       let acquisition, _ =
         start ~wall_clock env sw oauth current (id "login") transport
       in
       A.Acquisition.complete acquisition ~authorize_commit:(fun () -> true) |> adapter;
       A.Acquisition.close acquisition |> adapter;
       let before = snapshot current in
       (* Renewal is demand-based; expire the original grant before reopening. *)
       set_mock_wall_time env mock_clock (now +. 2.);
       C.close current;
       let restarted =
         C.open_existing
           ~sw
           ~wall_clock
           ~new_operation
           ~directory
           ~secrets
           ~environment:None
           ~host
         |> registry
       in
       let identity = C.Host_snapshot.identity before |> Option.value_exn in
       let renewal = A.renewal oauth identity |> Option.value_exn in
       C.refresh
         restarted
         ~sw
         ~clock:(Eio.Stdenv.mono_clock env)
         ~maximum_wait:(Time_ns.Span.of_sec 10.)
         ~binding
         ~renewal
       |> registry;
       let after = snapshot restarted in
       printf
         "exchanges:%d epoch-preserved:%b revision-changed:%b\n"
         !exchanges
         (Int64.equal (C.Host_snapshot.epoch before) (C.Host_snapshot.epoch after))
         (not
            (Option.equal
               String.equal
               (C.Host_snapshot.credential_revision before)
               (C.Host_snapshot.credential_revision after)));
       Eio.Switch.run (fun attempt ->
         let admission =
           C.admit
             restarted
             ~sw:attempt
             ~clock:(Eio.Stdenv.mono_clock env)
             ~maximum_wait:(Time_ns.Span.of_sec 10.)
             ~binding
             ~expected_owner:(C.Host_snapshot.owner after)
             ~expected_epoch:(C.Host_snapshot.epoch after)
             ~renewal:None
           |> registry
         in
         let profile =
           D.Profile.create
             ~id:"synthetic"
             ~account:(M.Identity.account identity)
             ~endpoint:"https://chatgpt.com/backend-api/codex/responses"
             ~capabilities:
               (D.Capability.create ~baseline:[ Text_input, Supported ] ~models:[]
                |> Or_error.ok_exn)
             ~defaults:[]
           |> Or_error.ok_exn
         in
         let lease =
           A.lease oauth admission ~identity ~profile
           |> Result.map_error ~f:(fun error ->
             Sexp.to_string (D.Auth.sexp_of_error error))
           |> Result.ok_or_failwith
         in
         let foreign =
           M.Identity.oauth
             ~host
             ~provider:"openai"
             ~billing:"subscription"
             ~issuer:(O.Policy.issuer Flow_test.policy)
             ~client_registration:(O.Policy.client_registration Flow_test.policy)
             ~resource:(O.Policy.resource Flow_test.policy)
             ~account:"other-account"
             ~verified_subject:"synthetic-subject"
             ~required_scopes:[ "openid"; "profile"; "email"; "offline_access" ]
           |> model
         in
         let foreign_profile =
           D.Profile.create
             ~id:"synthetic"
             ~account:(Some "other-account")
             ~endpoint:(D.Profile.endpoint profile)
             ~capabilities:
               (D.Capability.create ~baseline:[ Text_input, Supported ] ~models:[]
                |> Or_error.ok_exn)
             ~defaults:[]
           |> Or_error.ok_exn
         in
         printf
           "foreign-admission-refused:%b\n"
           (Result.is_error
              (A.lease oauth admission ~identity:foreign ~profile:foreign_profile));
         printf
           "lease-fenced:%b revision-current:%b\n"
           (Option.is_some (D.Auth.identity lease))
           (Option.equal
              String.equal
              (D.Auth.credential_revision lease)
              (C.Admission.credential_revision admission)));
       O.Transport.close transport;
       C.close restarted);
  [%expect
    {|
    exchanges:2 epoch-preserved:true revision-changed:true
    foreign-admission-refused:true
    lease-fenced:true revision-current:true
    |}]
;;

let%expect_test
    "cancelled acquisition owner joins blocked poll and removes original candidate"
  =
  with_registry (fun env _sw _directory _secrets _new_operation current ->
    let entered, enter = Eio.Promise.create () in
    let transport =
      O.For_testing.scripted_transport
        ~clock:(Eio.Stdenv.mono_clock env)
        (fun endpoint ~body:_ ~on_possible_submission:_ ->
           match endpoint with
           | User_code -> Ok (200, Flow_test.challenge)
           | Device_poll ->
             Eio.Promise.resolve enter ();
             Eio.Fiber.await_cancel ()
           | Token -> failwith "cancelled poll must never exchange")
    in
    let oauth =
      A.create ~transport ~policy:Flow_test.policy ~wall_clock:(Eio.Stdenv.clock env)
    in
    (try
       Eio.Switch.run (fun owner ->
         ignore
           (start env owner oauth current (id "cancelled_login") transport
            : A.Acquisition.t * O.Challenge.t);
         Eio.Promise.await entered;
         raise Exit)
     with
     | Exit -> ());
    let state = snapshot current in
    printf
      "candidate-cleared:%b active-absent:%b\n"
      (Option.is_none (C.Host_snapshot.pending_candidate_operation state))
      (Option.is_none (C.Host_snapshot.identity state));
    O.Transport.close transport;
    C.close current);
  [%expect {| candidate-cleared:true active-absent:true |}]
;;

let%expect_test
    "complete and close race joins exchange then cancels the original candidate"
  =
  with_registry (fun env sw _directory _secrets _new_operation current ->
    let entered, enter = Eio.Promise.create () in
    let transport =
      O.For_testing.scripted_transport
        ~clock:(Eio.Stdenv.mono_clock env)
        (fun endpoint ~body:_ ~on_possible_submission ->
           match endpoint with
           | User_code -> Ok (200, Flow_test.challenge)
           | Device_poll -> Ok (200, Flow_test.grant)
           | Token ->
             on_possible_submission ();
             Eio.Promise.resolve enter ();
             Eio.Fiber.await_cancel ())
    in
    let oauth =
      A.create ~transport ~policy:Flow_test.policy ~wall_clock:(Eio.Stdenv.clock env)
    in
    let acquisition, _ = start env sw oauth current (id "close_race") transport in
    Eio.Promise.await entered;
    let completing, started = Eio.Promise.create () in
    let completed, complete = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve started ();
      Eio.Promise.resolve
        complete
        (A.Acquisition.complete acquisition ~authorize_commit:(fun () -> true)));
    Eio.Promise.await completing;
    A.Acquisition.close acquisition |> adapter;
    let closed =
      match Eio.Promise.await completed with
      | Error (A.Error.OAuth error) -> O.Error.equal_code (O.Error.code error) Closed
      | Ok () | Error _ -> false
    in
    printf
      "complete-closed:%b original-candidate-cleared:%b\n"
      closed
      (Option.is_none (C.Host_snapshot.pending_candidate_operation (snapshot current)));
    O.Transport.close transport;
    C.close current);
  [%expect {| complete-closed:true original-candidate-cleared:true |}]
;;

let%expect_test "typed exchange failure leaves original candidate cancellable" =
  with_registry (fun env sw _directory _secrets _new_operation current ->
    let transport =
      O.For_testing.scripted_transport
        ~clock:(Eio.Stdenv.mono_clock env)
        (fun endpoint ~body:_ ~on_possible_submission ->
           match endpoint with
           | User_code -> Ok (200, Flow_test.challenge)
           | Device_poll -> Ok (200, Flow_test.grant)
           | Token ->
             on_possible_submission ();
             Ok (400, "{}"))
    in
    let oauth =
      A.create ~transport ~policy:Flow_test.policy ~wall_clock:(Eio.Stdenv.clock env)
    in
    let acquisition, _ = start env sw oauth current (id "typed_failure") transport in
    let failed =
      Result.is_error
        (A.Acquisition.complete acquisition ~authorize_commit:(fun () -> true))
    in
    A.Acquisition.close acquisition |> adapter;
    printf
      "failed:%b original-candidate-cleared:%b\n"
      failed
      (Option.is_none (C.Host_snapshot.pending_candidate_operation (snapshot current)));
    O.Transport.close transport;
    C.close current);
  [%expect {| failed:true original-candidate-cleared:true |}]
;;

let%expect_test "lost commit acknowledgment never repeats exchange or erases publication" =
  with_registry (fun env sw _directory _secrets _new_operation current ->
    let exchanges = ref 0 in
    let transport =
      O.For_testing.scripted_transport
        ~clock:(Eio.Stdenv.mono_clock env)
        (fun endpoint ~body:_ ~on_possible_submission ->
           match endpoint with
           | User_code -> Ok (200, Flow_test.challenge)
           | Device_poll -> Ok (200, Flow_test.grant)
           | Token ->
             on_possible_submission ();
             incr exchanges;
             Ok (200, Flow_test.tokens env ()))
    in
    let oauth =
      A.create ~transport ~policy:Flow_test.policy ~wall_clock:(Eio.Stdenv.clock env)
    in
    let acquisition, _ = start env sw oauth current (id "lost_ack") transport in
    let publications = ref 0 in
    C.For_testing.set_after_publication_hook
      current
      (Some
         (fun () ->
           incr publications;
           (* First records staged ownership; second publishes the active candidate. *)
           if !publications = 2 then raise Exit));
    (try
       ignore
         (A.Acquisition.complete acquisition ~authorize_commit:(fun () -> true)
          : (unit, A.Error.t) result)
     with
     | Exit -> ());
    C.For_testing.set_after_publication_hook current None;
    let retry_refused =
      match A.Acquisition.complete acquisition ~authorize_commit:(fun () -> true) with
      | Error Closed -> true
      | _ -> false
    in
    let uncertain =
      match A.Acquisition.close acquisition with
      | Error (Registry Publication_uncertain) -> true
      | _ -> false
    in
    printf
      "retry-refused:%b uncertain:%b exchanges:%d active-preserved:%b\n"
      retry_refused
      uncertain
      !exchanges
      (Option.is_some (C.Host_snapshot.identity (snapshot current)));
    O.Transport.close transport;
    C.close current);
  [%expect {| retry-refused:true uncertain:true exchanges:1 active-preserved:true |}]
;;

let%expect_test
    "unexpected provider exception survives candidate cleanup without poisoning owner"
  =
  with_registry (fun env sw _directory _secrets _new_operation current ->
    let transport =
      O.For_testing.scripted_transport
        ~clock:(Eio.Stdenv.mono_clock env)
        (fun endpoint ~body:_ ~on_possible_submission ->
           match endpoint with
           | User_code -> Ok (200, Flow_test.challenge)
           | Device_poll -> Ok (200, Flow_test.grant)
           | Token ->
             on_possible_submission ();
             raise Exit)
    in
    let oauth =
      A.create ~transport ~policy:Flow_test.policy ~wall_clock:(Eio.Stdenv.clock env)
    in
    let acquisition, _ = start env sw oauth current (id "worker_exception") transport in
    let primary =
      try
        ignore
          (A.Acquisition.complete acquisition ~authorize_commit:(fun () -> true)
           : (unit, A.Error.t) result);
        false
      with
      | Exit -> true
    in
    let close_primary =
      try
        ignore (A.Acquisition.close acquisition : (unit, A.Error.t) result);
        false
      with
      | Exit -> true
    in
    printf
      "primary-preserved:%b close-primary-preserved:%b original-candidate-cleared:%b\n"
      primary
      close_primary
      (Option.is_none (C.Host_snapshot.pending_candidate_operation (snapshot current)));
    O.Transport.close transport;
    C.close current);
  [%expect
    {| primary-preserved:true close-primary-preserved:true original-candidate-cleared:true |}]
;;

let%expect_test "revoked operator cannot commit after a blocked exchange" =
  with_registry (fun env sw _directory _secrets _new_operation current ->
    let entered, enter = Eio.Promise.create () in
    let release, resume = Eio.Promise.create () in
    let exchanges = ref 0 in
    let authorized = ref true in
    let transport =
      O.For_testing.scripted_transport
        ~clock:(Eio.Stdenv.mono_clock env)
        (fun endpoint ~body:_ ~on_possible_submission ->
           match endpoint with
           | User_code -> Ok (200, Flow_test.challenge)
           | Device_poll -> Ok (200, Flow_test.grant)
           | Token ->
             on_possible_submission ();
             incr exchanges;
             if !exchanges = 2
             then (
               Eio.Promise.resolve enter ();
               Eio.Promise.await release);
             Ok (200, Flow_test.tokens env ()))
    in
    let oauth =
      A.create ~transport ~policy:Flow_test.policy ~wall_clock:(Eio.Stdenv.clock env)
    in
    let original, _ = start env sw oauth current (id "original") transport in
    A.Acquisition.complete original ~authorize_commit:(fun () -> true) |> adapter;
    A.Acquisition.close original |> adapter;
    let before = snapshot current in
    let replacement, _ = start env sw oauth current (id "revoked") transport in
    Eio.Promise.await entered;
    authorized := false;
    Eio.Promise.resolve resume ();
    let denied =
      match
        A.Acquisition.complete replacement ~authorize_commit:(fun () -> !authorized)
      with
      | Error Denied -> true
      | Ok () | Error _ -> false
    in
    A.Acquisition.close replacement |> adapter;
    let after = snapshot current in
    printf
      "denied:%b previous-active-preserved:%b candidate-cleared:%b exchanges:%d\n"
      denied
      (Int64.equal (C.Host_snapshot.epoch before) (C.Host_snapshot.epoch after)
       && Option.equal
            String.equal
            (C.Host_snapshot.credential_revision before)
            (C.Host_snapshot.credential_revision after))
      (Option.is_none (C.Host_snapshot.pending_candidate_operation after))
      !exchanges;
    O.Transport.close transport;
    C.close current);
  [%expect
    {| denied:true previous-active-preserved:true candidate-cleared:true exchanges:2 |}]
;;
