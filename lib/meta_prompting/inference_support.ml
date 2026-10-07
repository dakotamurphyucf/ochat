open! Core

exception Configuration_required

let require = function
  | Some inference -> inference
  | None -> raise Configuration_required
;;

let setting name value =
  Inference.Request.Setting.create
    ~name
    ~value:(Value value)
    ~provenance:Execution_override
    ~limits:Transcript.Admission.default
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let complete inference ?model ~settings ~messages () =
  Eio.Switch.run (fun sw ->
    Inference_client.Execution.complete_text inference ~sw ?model ~settings ~messages ())
;;

let score text ~max:ceiling =
  if String.length text > 256
  then None
  else (
    match Jsonaf.parse (String.strip text) with
    | Ok (`Number number as json) ->
      (match Document_schema.Json.validate json ~limits:Transcript.Admission.default with
       | Error _ -> None
       | Ok () ->
         (match Float.of_string_opt number with
          | Some value
            when Float.is_finite value && Float.(value >= 0. && value <= ceiling) ->
            Some value
          | Some _ | None -> None))
    | Ok (`Null | `False | `True | `String _ | `Object _ | `Array _) | Error _ -> None)
;;
