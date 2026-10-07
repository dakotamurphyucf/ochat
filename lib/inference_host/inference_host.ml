open! Core
module R = Inference.Request
module D = Openai.Responses_driver
module Runtime = Inference_runtime
module A = Openai.Inference_adapter
module P = History_entry.Payload.Presence

type t =
  { driver : D.t
  ; auth : D.Auth.resolver
  ; limits : Runtime.Limits.t
  ; profile : D.Profile.t
  ; profile_revision : string option
  ; default_model : string
  ; adapter : Runtime.Adapter.t
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
      ; new_attempt =
          (fun _prepared ~relation ->
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
            scope, accounting_id)
      }
;;

let create driver ~profile ~profile_revision ~auth ~default_model ~namespace ~limits =
  let open Result.Let_syntax in
  let%bind _ =
    A.capture_target
      profile
      ~profile_revision
      ~model:default_model
      ~settings:[]
      ~limits:document_limits
  in
  let%bind identity = make_identity namespace in
  let%map adapter = A.create driver ~profile ~profile_revision ~auth ~limits in
  { driver; auth; limits; profile; profile_revision; default_model; adapter; identity }
;;

let with_response_limit t ~max_body_bytes =
  let open Result.Let_syntax in
  let%bind driver = D.with_response_limit t.driver ~max_body_bytes |> invalid in
  let%map adapter =
    A.create
      driver
      ~profile:t.profile
      ~profile_revision:t.profile_revision
      ~auth:t.auth
      ~limits:t.limits
  in
  { t with driver; adapter }
;;

let capture_config t config =
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
  A.capture_target
    t.profile
    ~profile_revision:t.profile_revision
    ~model:(Option.value config.model ~default:t.default_model)
    ~settings
    ~limits:document_limits
;;

let override_config current config =
  Chat_response.Inference_config.apply_overrides current config ~limits:document_limits
;;

let resolve t target = Runtime.Context.create t.adapter ~target

let recapture_config t ~current config =
  let open Result.Let_syntax in
  let%bind _ = resolve t current in
  let%bind captured = capture_config t config in
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
