open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module R = Inference.Request
module Backend = Inference_host.Backend

let ok sexp_of_error context = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (context : string), (sexp_of_error error : Sexp.t)]
;;

let%expect_test
    "bounded authored ingress drives the actual shared runtime host across reopen"
  =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    let directory =
      Eio_unix.run_in_systhread (fun () -> Core_unix.mkdtemp "/tmp/ochat-choices-XXXXXX")
    in
    let root = Eio.Path.(Eio.Stdenv.fs env / directory) in
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree root)
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let file = Eio.Path.(root / "choices.json") in
          let path = Filename.concat directory "choices.json" in
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            file
            {|[{"id":"short-output","credential_owner":"first-party-openai-responses","revision":"short-v1","defaults":{"max_output_tokens":32}}]|};
          let declarations =
            Provider_profile_choices.load ~env ~path
            |> ok DTO.Error.sexp_of_t "load authored choices"
          in
          assert (List.length declarations = 1);
          let principal =
            P.Principal.create
              ~id:
                (P.Id.Principal.of_string "pri_choices"
                 |> ok P.Error.sexp_of_t "protocol fixture")
              ~authentication_kind:"trusted-test"
              ~scopes:
                (P.Scope.Set.of_list [ Provider_view; Provider_manage; Provider_select ])
              ~attributes:[]
            |> ok P.Error.sexp_of_t "protocol fixture"
          in
          let actor = Operator_authorization.trusted_local principal in
          let server_id =
            P.Id.Server.of_string "srv_choices" |> ok P.Error.sexp_of_t "protocol fixture"
          in
          let compose () =
            Provider_platform.create
              ~sw
              ~env
              ~home:directory
              ~api_url:None
              ~lookup:(function
                | "OCHAT_PROVIDER_PROFILE_CHOICES" -> Some path
                | _ -> None)
              ~default_model:"model"
              ~namespace:"choice-test"
              ~callback_port:1455
              ()
            |> ok DTO.Error.sexp_of_t "compose production platform"
          in
          let platform = compose () in
          let runtime =
            Provider_platform.open_operator platform ~server_id
            |> ok DTO.Error.sexp_of_t "open operator"
          in
          ignore
            (Provider_runtime.dispatch
               runtime
               ~actor
               (P.Command.Provider_setup
                  { idempotency_key =
                      P.Idempotency_key.of_string "setup"
                      |> ok P.Error.sexp_of_t "protocol fixture"
                  })
             |> ok DTO.Error.sexp_of_t "setup canonical authority"
             : P.Method_result.t);
          let reads = ref 0 in
          ignore
            (Provider_runtime.enroll_private_key
               runtime
               ~actor
               ~profile:
                 (DTO.Profile_id.of_string "first-party-openai-responses"
                  |> ok P.Error.sexp_of_t "protocol fixture")
               ~key:
                 (P.Idempotency_key.of_string "canonical-key"
                  |> ok P.Error.sexp_of_t "protocol fixture")
               ~source_reference:"synthetic-choice-source"
               ~sw
               ~read:(fun ~sw:_ ->
                 incr reads;
                 Provider_secret_store.Secret.of_bytes (Bytes.of_string "synthetic-key")
                 |> Result.map_error ~f:(fun _ ->
                   Inference_host.Credential_bridge.Error.Invalid_credential))
             |> ok DTO.Error.sexp_of_t "enroll canonical credential"
             : DTO.Configuration_result.t);
          let canonical =
            Backend.capture
              (Provider_runtime.backend runtime)
              ~current:None
              ~model:"model"
              ~settings:[]
            |> ok Inference_runtime.Preparation_error.sexp_of_t "capture profile"
          in
          let revision =
            `Array
              [ `String "compatible-profile-v1"
              ; `String "host-config-v1"
              ; `String "short-v1"
              ]
            |> Jsonaf.to_string
            |> Digestif.SHA256.digest_string
            |> Digestif.SHA256.to_hex
            |> fun digest -> "choice-v1-" ^ digest
          in
          let selected =
            R.Target.create
              ~adapter:(R.Target.adapter canonical)
              ~profile:"short-output"
              ~profile_revision:(Some revision)
              ~account:(R.Target.account canonical)
              ~endpoint:(R.Target.endpoint canonical)
              ~model:"model"
              ~settings:[]
              ~limits:Document_schema.Limits.default
            |> ok R.Error.sexp_of_t "construct selected target"
            |> fun target ->
            R.Target.with_auth_binding
              target
              ~binding:(R.Target.auth_binding canonical)
              ~limits:Document_schema.Limits.default
            |> ok R.Error.sexp_of_t "construct selected target"
          in
          let choice =
            Backend.capture
              (Provider_runtime.backend runtime)
              ~current:(Some selected)
              ~model:"model"
              ~settings:[]
            |> ok Inference_runtime.Preparation_error.sexp_of_t "capture profile"
          in
          assert (String.equal (R.Target.profile choice) "short-output");
          assert (List.length (R.Target.settings choice) = 1);
          assert (!reads = 1);
          Provider_runtime.close runtime;
          let reopened =
            Provider_platform.open_operator (compose ()) ~server_id
            |> ok DTO.Error.sexp_of_t "reopen platform"
          in
          let restored =
            choice
            |> R.Target.to_json
            |> Jsonaf.to_string
            |> Jsonaf.of_string
            |> fun json ->
            R.Target.of_json json ~limits:Document_schema.Limits.default
            |> ok R.Error.sexp_of_t "restore choice target"
          in
          ignore
            (Backend.resolve (Provider_runtime.backend reopened) restored
             |> ok Inference_runtime.Preparation_error.sexp_of_t "resolve reopened choice"
             : Inference_runtime.Context.t);
          assert (!reads = 1);
          Provider_runtime.close reopened;
          Eio.Path.save ~create:(`Or_truncate 0o600) file "[]";
          let without_choice =
            Provider_platform.open_operator (compose ()) ~server_id
            |> ok DTO.Error.sexp_of_t "reopen platform"
          in
          assert (
            Result.is_error
              (Backend.resolve (Provider_runtime.backend without_choice) restored));
          ignore
            (Backend.capture
               (Provider_runtime.backend without_choice)
               ~current:None
               ~model:"model"
               ~settings:[]
             |> ok
                  Inference_runtime.Preparation_error.sexp_of_t
                  "capture canonical after choice removed"
             : R.Target.t);
          Provider_runtime.close without_choice;
          Eio.Path.save ~create:(`Or_truncate 0o600) file "[{bad";
          assert (Result.is_error (Provider_profile_choices.load ~env ~path));
          assert (
            Result.is_error
              (Provider_platform.create
                 ~sw
                 ~env
                 ~home:directory
                 ~api_url:None
                 ~lookup:(function
                   | "OCHAT_PROVIDER_PROFILE_CHOICES" -> Some path
                   | _ -> None)
                 ~default_model:"model"
                 ~namespace:"bad-test"
                 ~callback_port:1455
                 ()));
          Eio.Path.save ~create:(`Or_truncate 0o600) file (String.make 1_048_577 ' ');
          assert (Result.is_error (Provider_profile_choices.load ~env ~path));
          assert (
            Result.is_error (Provider_profile_choices.load ~env ~path:(path ^ ".missing")));
          print_s
            [%sexp
              { actual_host_choice = true
              ; one_credential_read = true
              ; durable_reopen = true
              ; invalid_configuration_rejected = true
              ; bounded_read = true
              }])));
  [%expect
    {|
    ((actual_host_choice true) (one_credential_read true) (durable_reopen true)
     (invalid_configuration_rejected true) (bounded_read true))
  |}]
;;
