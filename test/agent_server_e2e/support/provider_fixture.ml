open! Core
module I = Inference_composition
module B = Inference_host.Credential_bridge

let ok result =
  Result.map_error result ~f:(fun _ -> "synthetic provider fixture setup failed")
  |> Result.ok_or_failwith
;;

let id value = Credential_registry_model.Id.create value |> ok

let provision ~env ?(key = "e2e-private-synthetic-provider-key") fixture =
  let environment = Config_fixture.environment fixture in
  let home = (Temporary_environment.roots environment).home in
  Temporary_environment.register_secret environment key;
  Eio.Switch.run (fun sw ->
    let configuration mode =
      I.Configuration.of_environment
        ~env
        ~home
        ~api_url:None
        ~key_name:"OPENAI_API_KEY"
        ~lookup:(fun _ -> None)
        ~mode
      |> ok
    in
    match I.try_open (configuration Existing) ~sw ~env ~default_model:"gpt-4.1" with
    | Ok _ -> ()
    | Error Setup_required ->
      let opened =
        I.try_open
          (configuration (Initialize (id "e2e-fixture-incarnation")))
          ~sw
          ~env
          ~default_model:"gpt-4.1"
        |> ok
      in
      B.enroll
        (I.Opened.bridge opened)
        ~principal:"local-operator"
        ~profile:"first-party-openai-responses"
        ~operation:(id "e2e-fixture-enrollment")
        ~sw
        ~read:(fun ~sw:_ ->
          Provider_secret_store.Secret.of_bytes (Bytes.of_string key)
          |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential))
      |> ok
    | Error _ -> failwith "existing synthetic provider authority could not be opened")
;;
