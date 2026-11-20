open! Core
module P = Agent_protocol
module Configuration = Recovery_capacity_configuration
module Catalog = Agent_session.Prompt_catalog
module Request_target = Inference.Request.Target
module Driver = Openai.Inference_adapter
module Capacity = Inference_runtime.Context_capacity
module Host = Agent_session.Session_context_capacity.Host

let resolve qualification prompts (inference : Session_factory.inference_policy) =
  let open Option.Let_syntax in
  let%bind (entry : Catalog.entry) =
    Catalog.find_by_name prompts qualification.Configuration.Target.prompt
  in
  let%bind revision =
    match entry.availability with
    | Ready revision -> Some revision
    | Disabled | Unavailable _ -> None
  in
  let%bind target =
    Session_factory.capture_prompt_inference_target inference revision |> Result.ok
  in
  if
    String.equal (Request_target.adapter target) "openai.responses"
    && String.equal (Request_target.profile target) qualification.profile
    && String.equal (Request_target.model target) qualification.model
    && Capacity.Encoder.equal qualification.encoder Driver.capacity_encoder
    && Result.is_ok (inference.resolve_inference_context target)
  then Some target
  else None
;;

let create (configuration : Configuration.t) ~prompts ~inference =
  let open Result.Let_syntax in
  let%bind policies =
    List.filter_map configuration.targets ~f:(fun qualification ->
      Option.map (resolve qualification prompts inference) ~f:(fun target ->
        Capacity.Policy.create
          ~target
          ~encoder:qualification.encoder
          ~model_limit:(Configuration.Target.model_limit qualification)
          ~allowance:
            (Configuration.Target.allowance
               qualification
               ~host_total:configuration.host_total)
          ~estimate:(Some (Configuration.Target.estimate qualification))
        |> Result.map_error ~f:(fun _ ->
          P.Error.invalid_request "invalid recovery capacity limits for resolved target")))
    |> Result.all
  in
  Host.for_enabled_windows ~mode:configuration.mode ~targets:policies
  |> Result.map_error ~f:(fun Host.Duplicate_target ->
    P.Error.invalid_request
      "multiple recovery capacity entries qualify the same resolved target")
;;
