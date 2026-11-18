open! Core
module O = Inference.Observation

let absent record =
  match O.Attempt_record.state record with
  | Prepared -> O.Count.Not_submitted
  | Running -> Not_reported
  | Interrupted _ -> Interrupted
  | Terminal terminal ->
    (match Inference.Event.Terminal.delivery terminal with
     | Definitely_not_submitted -> Not_submitted
     | Possibly_submitted | Response_started -> Not_reported)
;;

let count record component =
  List.find_map (O.Attempt_record.observations record) ~f:(fun observation ->
    match O.payload observation with
    | Usage usage -> Some (O.Count.view (O.Usage.count usage component))
    | Context_estimate _ | Configuration _ | Transport_selection _ | Diagnostic _ -> None)
  |> Option.value ~default:(O.Count.Unknown (absent record))
;;
