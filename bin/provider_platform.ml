open! Core
module P = Agent_protocol
module Actor = Operator_authorization
module DTO = P.Provider_operator
module M = Credential_registry_model
module D = Openai.Responses_driver
module B = Inference_host.Credential_bridge
module O = Provider_oauth
module OR = Provider_oauth_registry
module Service = Provider_operator
module Admin = Service.Profile_admin

let invalid _ = DTO.Error.Invalid_request
let unavailable _ = DTO.Error.Store_unavailable
let checked result = result |> Result.map_error ~f:invalid

let constant result =
  Result.map_error result ~f:(fun _ -> "Invalid trusted provider label")
  |> Result.ok_or_failwith
;;

let generator env =
  P.Id.Generator.create ~bytes:(fun count ->
    let bytes = Cstruct.create count in
    Eio.Flow.read_exact (Eio.Stdenv.secure_random env) bytes;
    Cstruct.to_string bytes)
;;

let new_namespace env =
  P.Id.Operation.create_with (generator env) |> P.Id.Operation.to_string
;;

let required_scope = function
  | DTO.Operation.Status -> P.Scope.Provider_view
  | Select -> Provider_select
  | Setup | Login | Challenge | Cancel | Logout | Configure_environment -> Provider_manage
;;

let allowed actor operation =
  Actor.is_current actor
  && P.Principal.has_scope (Actor.principal actor) (required_scope operation)
;;

type t =
  { host : Inference_host.t
  ; sw : Eio.Switch.t
  ; initialize : P.Id.Server.t -> (Provider_runtime.t, DTO.Error.t) Result.t
  ; mutable runtime : (P.Id.Server.t * Provider_runtime.t) option
  }

let host t = t.host

let open_operator t ~server_id =
  match t.runtime with
  | Some (current, runtime) when P.Id.Server.equal current server_id -> Ok runtime
  | Some _ -> Error DTO.Error.Invalid_request
  | None ->
    let open Result.Let_syntax in
    let%map runtime = t.initialize server_id in
    t.runtime <- Some (server_id, runtime);
    runtime
;;

let factory t ~sw:_ ~server_id =
  open_operator t ~server_id
  |> Result.map ~f:Provider_runtime.operator_port
  |> Result.map_error ~f:Agent_server.Provider_operator_port.protocol_error
;;

let create
      ?(transport_policy = Inference.Observation.Transport_policy.Http_sse)
      ~sw
      ~env
      ~home
      ~api_url
      ~lookup
      ~default_model
      ~namespace
      ~callback_port
      ()
  =
  let open Result.Let_syntax in
  let generator = generator env in
  let new_operation () =
    M.Id.create P.Id.Operation.(create_with generator |> to_string) |> constant
  in
  let new_revision () =
    DTO.Revision.of_string (M.Id.to_string (new_operation ())) |> constant
  in
  let%bind () =
    if String.is_empty home || not (Filename.is_absolute home)
    then Error DTO.Error.Invalid_request
    else Ok ()
  in
  let%bind host_id = M.Id.create "local-runtime-host" |> checked in
  let%bind api_binding = M.Id.create "default-api-key" |> checked in
  let%bind codex_binding = M.Id.create "direct-codex-account" |> checked in
  let%bind api_id = DTO.Profile_id.of_string "first-party-openai-responses" |> checked in
  let%bind codex_id = DTO.Profile_id.of_string "direct-codex" |> checked in
  let%bind policy =
    O.Policy.direct_codex ~expected_account:None ~callback_port () |> checked
  in
  let%bind transport =
    O.Transport.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.mono_clock env)
    |> checked
  in
  Eio.Switch.on_release sw (fun () -> O.Transport.close transport);
  let oauth = OR.create ~transport ~policy ~wall_clock:(Eio.Stdenv.clock env) in
  let%bind driver =
    D.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) () |> checked
  in
  let%bind identity =
    M.Identity.api_key
      ~host:host_id
      ~provider:"openai"
      ~billing:"api"
      ~account:None
      ~key_reference:api_binding
    |> checked
  in
  let%bind api_profile =
    D.Profile.create
      ~id:(DTO.Profile_id.to_string api_id)
      ~account:None
      ~endpoint:(Provider_defaults.responses_endpoint ~api_url)
      ~capabilities:Provider_defaults.capabilities
      ~defaults:[]
    |> checked
  in
  let%bind api_mapping =
    B.Mapping.create api_profile ~revision:"host-config-v1" ~binding:api_binding ~identity
    |> checked
  in
  let%bind api_template =
    Admin.Template.create
      ~profile:api_id
      ~binding:api_binding
      ~revision:(DTO.Revision.of_string "host-config-v1" |> constant)
      ~authentication:Api_key
      ~expectation:(M.Expectation.exact identity)
      ~expected_account:None
      ~mapping:(fun identity ->
        B.Mapping.create
          api_profile
          ~revision:"host-config-v1"
          ~binding:api_binding
          ~identity)
    |> checked
  in
  let%bind codex_expectation =
    M.Expectation.oauth_acquisition
      ~host:host_id
      ~provider:"openai"
      ~billing:"subscription"
      ~issuer:(O.Policy.issuer policy)
      ~client_registration:(O.Policy.client_registration policy)
      ~resource:(O.Policy.resource policy)
      ~account:None
      ~required_scopes:[]
    |> checked
  in
  let%bind codex_capabilities =
    D.Capability.create
      ~baseline:
        (List.map
           [ D.Capability.Text_input
           ; Image_input
           ; Function_tools
           ; Opaque_replay
           ; Setting "instructions"
           ; Setting "parallel_tool_calls"
           ; Setting "reasoning"
           ; Setting "text"
           ; Setting "tool_choice"
           ; Setting "prompt_cache_key"
           ]
           ~f:(fun feature -> feature, D.Capability.Supported)
         @ List.map [ "temperature"; "top_p"; "max_output_tokens" ] ~f:(fun setting ->
           D.Capability.Setting setting, D.Capability.Unsupported))
      ~models:[]
    |> checked
  in
  let codex_mapping identity =
    D.Profile.create
      ~id:(DTO.Profile_id.to_string codex_id)
      ~account:(M.Identity.account identity)
      ~endpoint:"https://chatgpt.com/backend-api/codex/responses"
      ~capabilities:codex_capabilities
      ~defaults:[]
    |> Result.map_error ~f:(fun _ -> B.Error.Invalid_mapping)
    |> Result.bind ~f:(fun profile ->
      B.Mapping.create
        profile
        ~revision:"direct-codex-config-v1"
        ~binding:codex_binding
        ~identity)
  in
  let%bind codex_template =
    Admin.Template.create
      ~profile:codex_id
      ~binding:codex_binding
      ~revision:(DTO.Revision.of_string "direct-codex-config-v1" |> constant)
      ~authentication:Direct_codex
      ~expectation:codex_expectation
      ~expected_account:None
      ~mapping:codex_mapping
    |> checked
  in
  let%bind env_entry =
    B.Environment.Entry.create
      ~binding:api_binding
      ~identity
      ~name:"OPENAI_API_KEY"
      ~configuration_revision:None
      ~status:(fun () ->
        match lookup "OPENAI_API_KEY" with
        | None -> Credential_registry.Status.Missing
        | Some value ->
          (match D.Auth.bearer value with
           | Ok _ -> Available
           | Error _ -> Secret_unavailable))
      ~resolve:(fun ~sw:_ ->
        match lookup "OPENAI_API_KEY" with
        | None -> Error (B.Error.Lifecycle Credential_registry.Error.Missing_secret)
        | Some captured ->
          let%map access =
            Provider_secret_store.Secret.of_bytes (Bytes.of_string captured)
            |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential)
          in
          Credential_registry.Environment.resolved
            ~access
            ~configuration_revision:None
            ~check_current:(fun () ->
              if Option.equal String.equal (lookup "OPENAI_API_KEY") (Some captured)
              then Ok ()
              else Error Credential_registry.Error.Binding_unavailable))
    |> checked
  in
  let%bind environment = B.Environment.create [ env_entry ] |> checked in
  let%bind source =
    Service.Environment_source.create
      ~id:(DTO.Source_id.of_string "openai-api-key" |> constant)
      ~name:"OPENAI_API_KEY"
      ~revision:None
    |> checked
  in
  let%bind component =
    Private_storage.Name.create ".ochat-provider-credentials" |> checked
  in
  let%bind secret_namespace =
    Provider_secret_store.Namespace.create "provider-credentials" |> checked
  in
  let current = ref None in
  let backend =
    let module Backend = Inference_host.Backend in
    let get () =
      !current
      |> Result.of_option ~error:Inference_runtime.Preparation_error.Target_unavailable
    in
    let rec view bound =
      let selected () =
        let%bind runtime = get () in
        let backend = Provider_runtime.backend runtime in
        match bound with
        | None -> Ok backend
        | Some max_body_bytes -> Backend.with_response_limit backend ~max_body_bytes
      in
      Backend.create
        ~capture:(fun ~current ~model ~settings ->
          let%bind backend = selected () in
          Backend.capture backend ~current ~model ~settings)
        ~resolve:(fun target ->
          let%bind backend = selected () in
          Backend.resolve backend target)
        ~with_response_limit:(fun ~max_body_bytes ->
          if max_body_bytes <= 0
          then Error Inference_runtime.Preparation_error.Invalid_preparation
          else
            Ok
              (view
                 (Some
                    (Option.value_map
                       bound
                       ~default:max_body_bytes
                       ~f:(Int.min max_body_bytes)))))
    in
    view None
  in
  let%bind host =
    Inference_host.create_with_backend backend ~default_model ~namespace |> checked
  in
  let initialize server_id =
    let%map runtime =
      Provider_runtime_host.create
        ~sw
        ~env
        ~server_id
        ~anchor:Eio.Path.(Eio.Stdenv.fs env / home)
        ~components:[ component ]
        ~host:host_id
        ~secret_namespace
        ~driver
        ~templates:[ api_template; codex_template ]
        ~mappings:[ api_mapping ]
        ~default_profile:api_id
        ~environment:(Some environment)
        ~environment_sources:[ source ]
        ~oauth
        ~oauth_lease:
          (Some (B.OAuth.create ~lease:(OR.lease oauth) ~renewal:(OR.renewal oauth)))
        ~start_login:(fun ~sw ~template:_ ~mode ->
          match mode with
          | DTO.Login_mode.Browser ->
            O.Login.start_browser
              ~transport
              ~policy
              ~sw
              ~net:(Eio.Stdenv.net env)
              ~secure_random:(Eio.Stdenv.secure_random env)
              ~clock:(Eio.Stdenv.mono_clock env)
              ~wall_clock:(Eio.Stdenv.clock env)
              ~maximum_wait:(Time_ns.Span.of_min 15.)
          | Device ->
            O.Login.start_device
              ~transport
              ~policy
              ~sw
              ~clock:(Eio.Stdenv.mono_clock env)
              ~wall_clock:(Eio.Stdenv.clock env)
              ~maximum_wait:(Time_ns.Span.of_min 15.))
        ~inference_principal:"local-runtime"
        ~authorize_bridge:(fun ~principal ~profile ~operation ->
          List.mem
            [ DTO.Profile_id.to_string api_id; DTO.Profile_id.to_string codex_id ]
            profile
            ~equal:String.equal
          &&
          match operation with
          | B.Operation.Inference -> String.equal principal "local-runtime"
          | Status | Configure | Remove -> true)
        ~authorize:(fun principal ~operation ~profile:_ -> allowed principal operation)
        ~authorize_setup:(fun principal -> allowed principal Setup)
        ~authorize_status:(fun principal -> allowed principal Status)
        ~new_operation
        ~new_revision
        ~maximum_wait:(Time_ns.Span.of_sec 5.)
        ~limits:DTO.Limits.default
        ~inference_limits:Inference_runtime.Limits.default
        ~transport_policy
    in
    current := Some runtime;
    runtime
  in
  Ok { host; sw; initialize; runtime = None }
;;
