module Compatible_profile = Compatible_profile
module Provider_profiles = Provider_profiles
module Credential_bridge = Credential_bridge
module Provider_configuration = Provider_configuration
open! Core
module R = Inference.Request
module D = Openai.Responses_driver
module Runtime = Inference_runtime
module A = Openai.Inference_adapter
module P = History_entry.Payload.Presence

module Backend = struct
  type t =
    { capture :
        current:R.Target.t option
        -> model:string
        -> settings:D.Setting.t list
        -> (R.Target.t, Runtime.Preparation_error.t) Result.t
    ; capture_profile :
        current:R.Target.t
        -> profile:string
        -> (R.Target.t, Runtime.Preparation_error.t) Result.t
    ; resolve : Runtime.resolver
    ; with_response_limit :
        max_body_bytes:int -> (t, Runtime.Preparation_error.t) Result.t
    }

  let create ~capture_profile ~capture ~resolve ~with_response_limit =
    { capture; capture_profile; resolve; with_response_limit }
  ;;

  let capture_profile t = t.capture_profile
  let capture t = t.capture
  let resolve t = t.resolve
  let with_response_limit t = t.with_response_limit
end

type t =
  { backend : Backend.t
  ; default_model : string
  ; identity : Chat_response.Neutral_turn.Identity.t
  }

let document_limits = Document_schema.Limits.default

let invalid result =
  Result.map_error result ~f:(fun _ -> Runtime.Preparation_error.Invalid_preparation)
;;

let request_error result =
  Result.map_error result ~f:(fun error ->
    Runtime.Preparation_error.Invalid_request error)
;;

let next_exn counter =
  let rec advance () =
    let current = Atomic.get counter in
    if current = Int.max_value then failwith "host inference identity exhausted";
    if Atomic.compare_and_set counter current (current + 1) then current else advance ()
  in
  advance ()
;;

let invariant_exn result =
  Result.map_error result ~f:(fun _ -> "invalid host inference identity")
  |> Result.ok_or_failwith
;;

let make_identity namespace =
  let open Result.Let_syntax in
  let%bind source = Transcript.Source_id.of_string namespace |> invalid in
  let id kind n = namespace ^ ":" ^ kind ^ ":" ^ Int.to_string n in
  let%bind _ =
    Inference.Observation.Observation_id.of_string (id "preparation" Int.max_value)
    |> invalid
  in
  let%bind _ =
    Inference.Observation.Observation_id.of_string (id "usage" Int.max_value) |> invalid
  in
  let preparations = Atomic.make 0
  and attempts = Atomic.make 0 in
  Ok
    Chat_response.Neutral_turn.Identity.
      { new_preparation_id = (fun () -> id "preparation" (next_exn preparations))
      ; with_attempt =
          (fun _prepared ~relation ~f ->
            let n = next_exn attempts in
            let attempt =
              Transcript.Attempt_id.of_string (Int.to_string n) |> invariant_exn
            in
            let scope =
              Transcript.Scope.create ~source ~attempt ~relation |> invariant_exn
            in
            let accounting_id =
              Inference.Observation.Observation_id.of_string (id "usage" n)
              |> invariant_exn
            in
            f ~scope ~accounting_id)
      }
;;

let create_with_backend backend ~default_model ~namespace =
  let open Result.Let_syntax in
  let%bind () =
    if String.is_empty default_model || not (Stdlib.String.is_valid_utf_8 default_model)
    then Error Runtime.Preparation_error.Invalid_preparation
    else Ok ()
  in
  let%map identity = make_identity namespace in
  { backend; default_model; identity }
;;

let create
      ?(transport_policy = Inference.Observation.Transport_policy.Http_sse)
      driver
      ~profile
      ~profile_revision
      ~auth
      ~default_model
      ~namespace
      ~limits
  =
  let open Result.Let_syntax in
  let rec backend driver =
    let%map adapter =
      A.create driver ~profile ~profile_revision ~auth:(A.Auth_source.Static auth) ~limits
    in
    let resolve target =
      Runtime.Context.create adapter ~target
      |> Result.map ~f:(fun context ->
        Runtime.Context.with_transport_policy context transport_policy)
    in
    Backend.create
      ~capture_profile:(fun ~current ~profile:requested ->
        if String.equal requested (D.Profile.id profile)
        then Ok current
        else Error Runtime.Preparation_error.Target_unavailable)
      ~capture:(fun ~current ~model ~settings ->
        let%bind () =
          match current with
          | None -> Ok ()
          | Some target -> Result.map (resolve target) ~f:(fun _ -> ())
        in
        A.capture_target
          profile
          ~profile_revision
          ~model
          ~settings
          ~limits:document_limits)
      ~resolve
      ~with_response_limit:(fun ~max_body_bytes ->
        let%bind driver = D.with_response_limit driver ~max_body_bytes |> invalid in
        backend driver)
  in
  let%bind _ =
    A.capture_target
      profile
      ~profile_revision
      ~model:default_model
      ~settings:[]
      ~limits:document_limits
  in
  let%bind backend = backend driver in
  create_with_backend backend ~default_model ~namespace
;;

let with_response_limit t ~max_body_bytes =
  let open Result.Let_syntax in
  let%map backend = t.backend.with_response_limit ~max_body_bytes in
  { t with backend }
;;

let capture_config_using t ~current config =
  let open Result.Let_syntax in
  let%bind settings =
    Chat_response.Inference_config.settings config ~limits:document_limits
  in
  let%bind settings =
    List.map settings ~f:(fun setting ->
      let value =
        match R.Setting.value setting with
        | Absent -> Openai.Responses_codec.Request.Field.Absent
        | Null -> Null
        | Value value -> Value value
      in
      D.Setting.create ~name:(R.Setting.name setting) ~value ~provenance:Captured_prompt
      |> Result.map_error ~f:(fun _ -> Runtime.Preparation_error.Unsupported_setting))
    |> Result.all
  in
  t.backend.capture
    ~current
    ~model:(Option.value config.model ~default:t.default_model)
    ~settings
;;

let capture_config t config = capture_config_using t ~current:None config

let override_config current config =
  Chat_response.Inference_config.apply_overrides current config ~limits:document_limits
;;

let resolve t target = t.backend.resolve target

let recapture_config t ~current config =
  let open Result.Let_syntax in
  let%bind _ = resolve t current in
  let%bind captured = capture_config_using t ~current:(Some current) config in
  let%bind () =
    if
      String.equal (R.Target.adapter current) (R.Target.adapter captured)
      && String.equal (R.Target.profile current) (R.Target.profile captured)
      && Option.equal String.equal (R.Target.account current) (R.Target.account captured)
      && String.equal (R.Target.endpoint current) (R.Target.endpoint captured)
      && P.equal
           R.Auth_binding.equal
           (R.Target.auth_binding current)
           (R.Target.auth_binding captured)
    then Ok ()
    else Error Runtime.Preparation_error.Target_mismatch
  in
  let%bind current =
    R.Target.with_model current ~model:(R.Target.model captured) ~limits:document_limits
    |> request_error
  in
  List.fold_result
    Chat_response.Inference_config.setting_names
    ~init:current
    ~f:(fun current name ->
      let selected =
        List.find (R.Target.settings captured) ~f:(fun setting ->
          String.equal (R.Setting.name setting) name)
      in
      let value, provenance =
        match selected with
        | Some setting -> R.Setting.value setting, R.Setting.provenance setting
        | None -> P.Absent, R.Setting.Captured_prompt
      in
      R.Target.with_setting current ~name ~value ~provenance ~limits:document_limits
      |> request_error)
;;

let identity t = t.identity
let capture_profile t ~current ~profile = t.backend.capture_profile ~current ~profile
