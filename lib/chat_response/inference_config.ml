open! Core
module R = Inference.Request

let setting_names = [ "max_output_tokens"; "temperature"; "reasoning" ]

let request_error result =
  Result.map_error result ~f:(fun error ->
    Inference_runtime.Preparation_error.Invalid_request error)
;;

let number_of_float value ~limits =
  let number = Float.to_string value in
  let number = if String.is_suffix number ~suffix:"." then number ^ "0" else number in
  let json = `Number number in
  Document_schema.Json.validate ~limits json
  |> Result.map_error ~f:(fun error ->
    Inference_runtime.Preparation_error.Invalid_request (R.Error.Json error))
  |> Result.map ~f:(fun () -> json)
;;

let settings (config : Config.t) ~limits =
  let open Result.Let_syntax in
  let%bind temperature =
    match config.temperature with
    | None -> Ok None
    | Some value -> number_of_float value ~limits |> Result.map ~f:Option.some
  in
  let values = [] in
  let values =
    Option.value_map config.max_tokens ~default:values ~f:(fun n ->
      ("max_output_tokens", `Number (Int.to_string n)) :: values)
  in
  let values =
    Option.value_map temperature ~default:values ~f:(fun value ->
      ("temperature", value) :: values)
  in
  let values =
    Option.value_map config.reasoning_effort ~default:values ~f:(fun effort ->
      ("reasoning", `Object [ "effort", `String effort; "summary", `String "detailed" ])
      :: values)
  in
  List.map values ~f:(fun (name, value) ->
    R.Setting.create ~name ~value:(Value value) ~provenance:Captured_prompt ~limits
    |> request_error)
  |> Result.all
;;

let apply_overrides current (config : Config.t) ~limits =
  let open Result.Let_syntax in
  let%bind current =
    match config.model with
    | None -> Ok current
    | Some model -> R.Target.with_model current ~model ~limits |> request_error
  in
  let%bind settings = settings config ~limits in
  List.fold_result settings ~init:current ~f:(fun target setting ->
    R.Target.with_setting
      target
      ~name:(R.Setting.name setting)
      ~value:(R.Setting.value setting)
      ~provenance:(R.Setting.provenance setting)
      ~limits
    |> request_error)
;;
