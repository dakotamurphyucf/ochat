open! Core
module R = Inference.Request
module P = Agent_protocol
module C = Inference.Observation.Configuration

let invalid message = P.Error.invalid_request message

let lower result =
  Result.map_error result ~f:(fun _ -> invalid "invalid configuration value")
;;

let target state =
  match Inference.Selection.view state.Session_state.spec.inference_target with
  | Captured target -> Ok target
  | Unresolved ->
    Error
      (P.Error.create
         Migration_required
         ~message:"session configuration requires explicit migration"
         ~retryable:false
         ())
;;

let compatible_identity current ~proposed =
  String.equal (R.Target.adapter current) (R.Target.adapter proposed)
  && String.equal (R.Target.endpoint current) (R.Target.endpoint proposed)
  && Option.equal String.equal (R.Target.account current) (R.Target.account proposed)
  && R.Presence.equal
       R.Auth_binding.equal
       (R.Target.auth_binding current)
       (R.Target.auth_binding proposed)
;;

let apply current ~patch ~profile_target =
  let open Result.Let_syntax in
  let%bind base =
    match P.Session_configuration.Patch.profile patch, profile_target with
    | None, None -> Ok current
    | Some profile, Some proposed
      when String.equal profile (R.Target.profile proposed)
           && compatible_identity current ~proposed ->
      R.Target.with_profile_from
        current
        ~approved:proposed
        ~limits:Document_schema.Limits.default
      |> lower
    | None, Some _ | Some _, None | Some _, Some _ ->
      Error
        (P.Error.create
           Permission_denied
           ~message:
             "profile change must preserve authorized account, endpoint and credential \
              binding"
           ~retryable:false
           ())
  in
  let%bind base =
    match P.Session_configuration.Patch.model patch with
    | None -> Ok base
    | Some model ->
      R.Target.with_model base ~model ~limits:Document_schema.Limits.default |> lower
  in
  List.fold
    (P.Session_configuration.Patch.settings patch)
    ~init:(Ok base)
    ~f:(fun acc setting ->
      let%bind target = acc in
      R.Target.with_setting
        target
        ~name:(R.Setting.name setting)
        ~value:(R.Setting.value setting)
        ~provenance:Execution_override
        ~limits:Document_schema.Limits.default
      |> lower)
;;

let safe_view target =
  C.of_target
    target
    ~preparation_id:"selected-intent"
    ~transport:Unknown_transport
    ~capabilities:[]
    ~limits:Document_schema.Limits.default
  |> Result.map_error ~f:(fun _ -> invalid "configuration safe projection unavailable")
;;

let redact configuration =
  match C.to_json configuration with
  | `Object fields ->
    let fields =
      List.map fields ~f:(fun (name, value) ->
        match name with
        | "adapter" | "profile" -> name, `String "withheld"
        | "profile_revision" | "account" -> name, `Null
        | _ -> name, value)
    in
    C.of_json (`Object fields) ~limits:Document_schema.Limits.default
    |> Result.map_error ~f:(fun _ -> invalid "configuration redaction failed")
  | _ -> Error (invalid "configuration projection is not an object")
;;

let redact_identity (view : P.Session_configuration.t) =
  let open Result.Let_syntax in
  let%bind selected =
    match view.selected with
    | None -> Ok None
    | Some c -> Result.map (redact c) ~f:Option.some
  in
  let%map capture =
    match view.capture with
    | None -> Ok None
    | Some capture ->
      Result.map (redact capture.configuration) ~f:(fun configuration ->
        Some { capture with configuration })
  in
  { view with selected; capture }
;;
