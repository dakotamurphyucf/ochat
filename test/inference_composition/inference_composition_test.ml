open! Core

let%expect_test "CLI captures the configured provider base without remote fallback" =
  List.iter
    [ None
    ; Some "api.openai.com"
    ; Some "https://gateway.example/proxy"
    ; Some "https://gateway.example/proxy/"
    ; Some "http://127.0.0.1:8765"
    ; Some ""
    ]
    ~f:(fun api_url -> print_endline (Inference_composition.responses_endpoint ~api_url));
  [%expect
    {|
    https://api.openai.com/v1/responses
    https://api.openai.com/v1/responses
    https://gateway.example/proxy/v1/responses
    https://gateway.example/proxy/v1/responses
    http://127.0.0.1:8765/v1/responses
    https:///v1/responses
    |}]
;;

let%expect_test
    "missing registry requires setup but missing enrolled secret never means setup"
  =
  let module I = Inference_composition in
  let module S = Private_storage in
  let module Secret = Provider_secret_store in
  let module B = Inference_host.Credential_bridge in
  let ok r =
    Result.map_error r ~f:(fun _ -> "synthetic composition fixture failed")
    |> Result.ok_or_failwith
  in
  let id value = Credential_registry_model.Id.create value |> ok in
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    let suffix =
      Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
    in
    let home = Eio.Path.(Eio.Stdenv.fs env / "/tmp" / ("ochat-composition-" ^ suffix)) in
    Eio.Path.mkdir ~perm:0o700 home;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree home)
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let configuration mode =
            I.Configuration.of_environment
              ~env
              ~home:(Eio.Path.native_exn home)
              ~api_url:(Some "http://127.0.0.1:9")
              ~key_name:"OPENAI_API_KEY"
              ~lookup:(fun _ -> None)
              ~mode
            |> ok
          in
          let open_ mode =
            I.try_open (configuration mode) ~sw ~env ~default_model:"synthetic"
          in
          (match open_ Existing with
           | Error Setup_required -> ()
           | _ -> failwith "missing authority must require setup");
          (* A second Existing open also fails: the first did not fabricate authority. *)
          (match open_ Existing with
           | Error Setup_required -> ()
           | _ -> failwith "opening enrolled missing authority");
          let opened = open_ (Initialize (id "fixture-incarnation")) |> ok in
          B.enroll
            (I.Opened.bridge opened)
            ~principal:"local-operator"
            ~profile:"first-party-openai-responses"
            ~operation:(id "fixture-enrollment")
            ~sw
            ~read:(fun ~sw:_ ->
              Secret.Secret.of_bytes (Bytes.of_string "synthetic-only")
              |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential))
          |> ok;
          let directory =
            S.Directory.open_or_create
              ~sw
              ~anchor:home
              ~components:[ S.Name.create ".ochat-provider-credentials" |> ok ]
            |> ok
          in
          let secrets =
            Secret.open_private_files
              ~sw
              ~directory
              ~namespace:(Secret.Namespace.create "provider-credentials" |> ok)
            |> ok
          in
          Secret.delete
            secrets
            ~revision:(Secret.Revision.create "fixture-enrollment" |> ok)
          |> ok;
          let reopened = open_ Existing |> ok in
          let status =
            B.status
              (I.Opened.bridge reopened)
              ~principal:"local-operator"
              ~profile:"first-party-openai-responses"
            |> ok
          in
          match B.Status.availability status with
          | Unavailable Secret_unavailable -> ()
          | _ -> failwith "missing secret must stay a credential failure, not setup")));
  print_endline
    "absent registry: setup required, no enrollment; absent secret: existing authority \
     retained";
  [%expect
    {| absent registry: setup required, no enrollment; absent secret: existing authority retained |}]
;;
