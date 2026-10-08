open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module Platform = Inference_composition.Provider_platform
module Runtime = Provider_runtime
module B = Inference_host.Credential_bridge

let checked sexp_of_error result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (sexp_of_error error))
  |> Result.ok_or_failwith
;;

let protocol result = checked P.Error.sexp_of_t result
let provider result = checked DTO.Error.sexp_of_t result

let actor () =
  P.Principal.create
    ~id:(P.Id.Principal.of_string "pri_e2e_provider_fixture" |> protocol)
    ~authentication_kind:"local.provider_fixture"
    ~scopes:(P.Scope.Set.of_list [ Provider_view; Provider_manage; Provider_select ])
    ~attributes:[]
  |> protocol
  |> Operator_authorization.trusted_local
;;

let with_host ~env ?api_url ?(key = "e2e-private-synthetic-provider-key") fixture f =
  let environment = Config_fixture.environment fixture in
  let home = (Temporary_environment.roots environment).home in
  Temporary_environment.register_secret environment key;
  Eio.Switch.run (fun sw ->
    let platform =
      Platform.create
        ~sw
        ~env
        ~home
        ~api_url
        ~lookup:(fun _ -> None)
        ~default_model:"gpt-4.1"
        ~namespace:(Platform.new_namespace env)
        ~callback_port:1455
        ()
      |> provider
    in
    let runtime =
      Platform.open_operator
        platform
        ~server_id:(P.Id.Server.of_string "srv_e2e_provider_fixture" |> protocol)
      |> provider
    in
    Exn.protect
      ~finally:(fun () -> Runtime.close runtime)
      ~f:(fun () ->
        let actor = actor () in
        let status =
          match
            Runtime.dispatch runtime ~actor (P.Command.Provider_status { profile = None })
            |> provider
          with
          | P.Method_result.Provider_status status -> status
          | _ -> failwith "synthetic provider status returned a different result"
        in
        if status.setup_required
        then (
          (match
             Runtime.dispatch
               runtime
               ~actor
               (P.Command.Provider_setup
                  { idempotency_key =
                      P.Idempotency_key.of_string "e2e-fixture-setup" |> protocol
                  })
             |> provider
           with
           | P.Method_result.Provider_setup _ -> ()
           | _ -> failwith "synthetic provider setup returned a different result");
          Runtime.enroll_private_key
            runtime
            ~actor
            ~profile:(DTO.Profile_id.of_string "first-party-openai-responses" |> protocol)
            ~key:(P.Idempotency_key.of_string "e2e-fixture-enrollment" |> protocol)
            ~source_reference:"synthetic:e2e-private-key"
            ~sw
            ~read:(fun ~sw:_ ->
              Provider_secret_store.Secret.of_bytes (Bytes.of_string key)
              |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential))
          |> provider
          |> ignore);
        f (Platform.host platform)))
;;

let provision ~env ?key fixture = with_host ~env ?key fixture (fun _ -> ())
