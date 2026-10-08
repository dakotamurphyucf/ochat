open! Core
module D = Openai.Responses_driver

let require result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let responses_endpoint ~api_url =
  let base =
    Option.value api_url ~default:"https://api.openai.com"
    |> String.chop_suffix_if_exists ~suffix:"/"
  in
  let base =
    if Option.is_some (Uri.scheme (Uri.of_string base)) then base else "https://" ^ base
  in
  base ^ "/v1/responses"
;;

module M = Credential_registry_model
module B = Inference_host.Credential_bridge
module S = Inference_host.Provider_configuration
module C = Credential_registry

module Error = struct
  type t =
    | Invalid_configuration
    | Setup_required
    | Provider of S.Error.t
    | Preparation of Inference_runtime.Preparation_error.t
  [@@deriving sexp_of]
end

(* The standard endpoint host policy permits fields implemented by this
     adapter. This is neither a model catalog nor a live support probe: a model
     rejecting a permitted field still returns a typed provider failure.
     Operator-owned profiles may narrow the baseline or refine named models. *)
let capabilities =
  D.Capability.create
    ~baseline:
      ([ D.Capability.Text_input
       ; Image_input
       ; Document_input
       ; Function_tools
       ; Custom_tools
       ; Opaque_replay
       ]
       @ List.map
           [ "instructions"
           ; "max_output_tokens"
           ; "parallel_tool_calls"
           ; "temperature"
           ; "top_p"
           ; "reasoning"
           ; "text"
           ; "tool_choice"
           ; "prompt_cache_key"
           ; "prompt_cache_retention"
           ; "prompt_cache_options"
           ]
           ~f:(fun name -> D.Capability.Setting name)
       |> List.map ~f:(fun feature -> feature, D.Capability.Supported))
    ~models:[]
  |> Or_error.ok_exn
;;

module Configuration = struct
  type t =
    { storage : S.t
    ; mappings : B.Mapping.t list
    ; default_profile : string
    ; environment : B.Environment.t option
    ; oauth : B.OAuth.t option
    ; principal : string
    ; authorize : principal:string -> profile:string -> operation:B.Operation.t -> bool
    ; transport_policy : Inference.Observation.Transport_policy.t
    }

  let create
        ~storage
        ~mappings
        ~default_profile
        ~environment
        ~oauth
        ~principal
        ~authorize
        ~transport_policy
    =
    if
      String.is_empty principal
      || String.length principal > 512
      || (not (Stdlib.String.is_valid_utf_8 principal))
      || not
           (List.exists mappings ~f:(fun mapping ->
              String.equal (B.Mapping.profile mapping) default_profile))
    then Error Error.Invalid_configuration
    else
      Ok
        { storage
        ; mappings
        ; default_profile
        ; environment
        ; oauth
        ; principal
        ; authorize
        ; transport_policy
        }
  ;;

  let of_environment ~env ~home ~api_url ~key_name ~lookup ~mode =
    let open Result.Let_syntax in
    let invalid result =
      Result.map_error result ~f:(fun _ -> Error.Invalid_configuration)
    in
    let%bind () =
      if String.is_empty home then Error Error.Invalid_configuration else Ok ()
    in
    let%bind host = M.Id.create "local-runtime-host" |> invalid in
    let%bind binding = M.Id.create "default-api-key" |> invalid in
    let%bind namespace =
      Provider_secret_store.Namespace.create "provider-credentials" |> invalid
    in
    let%bind component =
      Private_storage.Name.create ".ochat-provider-credentials" |> invalid
    in
    let%bind storage =
      S.create
        ~anchor:Eio.Path.(Eio.Stdenv.fs env / home)
        ~components:[ component ]
        ~host
        ~secret_namespace:namespace
        ~mode
      |> invalid
    in
    let%bind profile =
      D.Profile.create
        ~id:"first-party-openai-responses"
        ~account:None
        ~endpoint:(responses_endpoint ~api_url)
        ~capabilities
        ~defaults:[]
      |> invalid
    in
    let%bind identity =
      M.Identity.api_key
        ~host
        ~provider:"openai"
        ~billing:"api"
        ~account:None
        ~key_reference:binding
      |> invalid
    in
    let%bind mapping =
      B.Mapping.create profile ~revision:"host-config-v1" ~binding ~identity |> invalid
    in
    (* The lifecycle calls this only for an explicitly authorized status probe.
       Metadata synchronization and opening do not inspect the selected key. *)
    let availability () =
      match lookup key_name with
      | None -> C.Status.Missing
      | Some value ->
        (match D.Auth.bearer value with
         | Ok _ -> Available
         | Error _ -> Secret_unavailable)
    in
    let%bind entry =
      B.Environment.Entry.create
        ~binding
        ~identity
        ~name:key_name
        ~configuration_revision:None
        ~status:availability
        ~resolve:(fun ~sw:_ ->
          match lookup key_name with
          | None -> Error (B.Error.Lifecycle C.Error.Missing_secret)
          | Some captured ->
            let%map access =
              Provider_secret_store.Secret.of_bytes (Bytes.of_string captured)
              |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential)
            in
            C.Environment.resolved
              ~access
              ~configuration_revision:None
              ~check_current:(fun () ->
                if Option.equal String.equal (lookup key_name) (Some captured)
                then Ok ()
                else Error C.Error.Binding_unavailable))
      |> invalid
    in
    let%bind environment = B.Environment.create [ entry ] |> invalid in
    create
      ~storage
      ~mappings:[ mapping ]
      ~default_profile:(B.Mapping.profile mapping)
      ~environment:(Some environment)
      ~oauth:None
      ~principal:"local-operator"
      ~authorize:(fun ~principal ~profile:_ ~operation:_ ->
        String.equal principal "local-operator")
      ~transport_policy:Inference.Observation.Transport_policy.Http_sse
  ;;
end

module Opened = struct
  type t =
    { host : Inference_host.t
    ; bridge : B.t
    }

  let host t = t.host
  let bridge t = t.bridge
end

let try_open configuration ~sw ~env ~default_model =
  let open Result.Let_syntax in
  let%bind driver =
    D.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
    |> Result.map_error ~f:(fun _ -> Error.Invalid_configuration)
  in
  let new_operation () =
    M.Id.create Agent_protocol.Id.Operation.(create () |> to_string)
    |> Result.map_error ~f:(fun _ -> "invalid host operation identity")
    |> Result.ok_or_failwith
  in
  let%bind opened =
    S.open_host
      configuration.Configuration.storage
      ~sw
      ~env
      ~driver
      ~new_operation
      ~environment:configuration.environment
      ~oauth:configuration.oauth
      ~mappings:configuration.mappings
      ~authorize:configuration.authorize
      ~maximum_wait:(Time_ns.Span.of_sec 10.)
      ~transport_policy:configuration.transport_policy
      ~limits:Inference_runtime.Limits.default
    |> Result.map_error ~f:(function
      | S.Error.Lifecycle (C.Error.Model M.Error.Missing_registry) -> Error.Setup_required
      | error -> Provider error)
  in
  let bridge = S.Opened.bridge opened in
  let rec backend bridge =
    Inference_host.Backend.create
      ~capture:(fun ~current ~model ~settings ->
        B.capture
          bridge
          ~principal:configuration.principal
          ~default_profile:configuration.default_profile
          ~current
          ~model
          ~settings
        |> Result.map_error ~f:B.preparation_error)
      ~resolve:(B.resolver bridge ~principal:configuration.principal)
      ~with_response_limit:(fun ~max_body_bytes ->
        B.with_response_limit bridge ~max_body_bytes
        |> Result.map_error ~f:B.preparation_error
        |> Result.map ~f:backend)
  in
  let namespace = Agent_protocol.Id.Session.(create () |> to_string) in
  let%map host =
    Inference_host.create_with_backend (backend bridge) ~default_model ~namespace
    |> Result.map_error ~f:(fun error -> Error.Preparation error)
  in
  { Opened.host; bridge }
;;

let try_create configuration ~sw ~env ~default_model =
  try_open configuration ~sw ~env ~default_model |> Result.map ~f:Opened.host
;;

let try_create_default ~sw ~env ~default_model =
  let open Result.Let_syntax in
  let%bind home =
    Sys.getenv "HOME" |> Result.of_option ~error:Error.Invalid_configuration
  in
  let%bind configuration =
    Configuration.of_environment
      ~env
      ~home
      ~api_url:(Sys.getenv "API_URL")
      ~key_name:"OPENAI_API_KEY"
      ~lookup:Sys.getenv
      ~mode:S.Mode.Existing
  in
  try_create configuration ~sw ~env ~default_model
;;

let create ~sw ~env ~default_model =
  match try_create_default ~sw ~env ~default_model with
  | Ok host -> host
  | Error Error.Setup_required ->
    failwith
      "Provider credentials require explicit host setup; existing authority was not \
       initialized or replaced."
  | Error error -> failwith (Sexp.to_string_hum (Error.sexp_of_t error))
;;

let context host config =
  Inference_host.capture_config host config
  |> require
  |> Inference_host.resolve host
  |> require
;;

let try_execution host config =
  let open Result.Let_syntax in
  let%bind target = Inference_host.capture_config host config in
  let%map context = Inference_host.resolve host target in
  Inference_client.Execution.create
    ~context
    ~identity:(Inference_host.identity host)
    ~relation:Transcript.Scope.Root
    ~before_dispatch:(fun _ -> ())
    ~on_attempt:(fun _ -> ())
    ~on_observation:(fun _ -> ())
    ~on_completion:(fun _ -> ())
;;

let execution host config = try_execution host config |> require

let bounded_host host ~max_body_bytes =
  Inference_host.with_response_limit host ~max_body_bytes |> require
;;

let bounded_execution host ~max_body_bytes config =
  execution (bounded_host host ~max_body_bytes) config
;;

let daemon_options host =
  let inference_policy : Agent_server.Session_factory.inference_policy =
    { capture_inference_target =
        (fun ~prompt_revision_id:_ ~config -> Inference_host.capture_config host config)
    ; recapture_inference_target =
        (fun ~current ~prompt_revision_id:_ ~config ->
          Inference_host.recapture_config host ~current config)
    ; migrate_inference_target = None
    ; migrate_model_job_target = None
    ; approve_inference_target_change =
        (fun ~current:_ ~proposed ->
          Inference_host.resolve host proposed |> Result.map ~f:(fun _ -> ()))
    ; resolve_inference_context = Inference_host.resolve host
    ; runtime_inference_ports =
        (fun _ ->
          (* Actual fresh identities are mandatory independently of rendering.
             These explicit observers do not claim durable usage accounting. *)
          Ok
            { new_preparation_id = (Inference_host.identity host).new_preparation_id
            ; on_admitted = (fun ~scope:_ ~accounting_id:_ -> ())
            ; on_attempt = (fun _ -> ())
            ; on_observation = (fun _ -> ())
            ; on_completion = (fun _ -> ())
            })
    }
  in
  { Agent_server.Daemon.default_options with inference_policy }
;;
