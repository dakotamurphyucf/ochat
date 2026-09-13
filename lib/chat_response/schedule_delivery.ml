open Core
module P = Agent_protocol
module V = Chatml.Chatml_value_codec
module L = Chatml.Chatml_lang

let tag = "__Ochat_timer_delivery_v1"

let validate (schedule : P.Schedule.t) =
  let open Result.Let_syntax in
  let%bind () =
    P.Schedule.validate schedule
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
  in
  match schedule.ownership, schedule.status with
  | Some _, Delivering -> Ok ()
  | _ -> Error "timer delivery requires an owned, claimed schedule"
;;

let capture schedule =
  let open Result.Let_syntax in
  let%map () = validate schedule in
  (* Keep protocol integers and JSON number spellings exact. The public Json.t
     projection uses floats and must not normalize durable identity metadata. *)
  L.VVariant (tag, [ L.VString (Jsonaf.to_string (P.Schedule.to_json schedule)) ])
;;

let decode = function
  | L.VVariant (name, args) when String.equal name tag ->
    let open Result.Let_syntax in
    let%bind json =
      match args with
      | [ L.VString payload ] ->
        Result.try_with (fun () -> Jsonaf.of_string payload)
        |> Result.map_error ~f:(fun _ -> "invalid timer delivery JSON")
      | _ -> Error "invalid timer delivery envelope"
    in
    let%bind schedule =
      P.Schedule.of_json json |> Result.map_error ~f:(fun error -> error.P.Error.message)
    in
    let%map () = validate schedule in
    Some schedule
  | _ -> Ok None
;;

let script_event value =
  let open Result.Let_syntax in
  let%map schedule = decode value in
  match schedule with
  | None -> value
  | Some schedule -> L.VVariant ("Internal_event", [ V.jsonaf_to_value schedule.payload ])
;;
