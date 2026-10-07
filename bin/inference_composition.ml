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

let create ~env ~default_model =
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
  in
  let profile =
    D.Profile.create
      ~id:"first-party-openai-responses"
      ~account:None
      ~endpoint:(responses_endpoint ~api_url:(Sys.getenv "API_URL"))
      ~capabilities
      ~defaults:[]
    |> Or_error.ok_exn
  in
  let lease =
    match Sys.getenv "OPENAI_API_KEY" with
    | None -> Error D.Auth.Missing
    | Some key -> D.Auth.bearer key
  in
  let auth ~sw:_ _ = lease in
  let driver =
    D.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) () |> Or_error.ok_exn
  in
  let namespace = Agent_protocol.Id.Session.(create () |> to_string) in
  Inference_host.create
    driver
    ~profile
    ~profile_revision:None
    ~auth
    ~default_model
    ~namespace
    ~limits:Inference_runtime.Limits.default
  |> require
;;

let context host config =
  Inference_host.capture_config host config
  |> require
  |> Inference_host.resolve host
  |> require
;;

let execution host config =
  Inference_client.Execution.create
    ~context:(context host config)
    ~identity:(Inference_host.identity host)
    ~relation:Transcript.Scope.Root
    ~before_dispatch:(fun _ -> ())
    ~on_attempt:(fun _ -> ())
    ~on_observation:(fun _ -> ())
    ~on_completion:(fun _ -> ())
;;

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
