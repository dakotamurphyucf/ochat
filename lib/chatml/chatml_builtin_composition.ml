open Core
open Chatml_lang
module Callback = Chatml_builtin_callback

let outcome value ~name =
  match value with
  | VVariant ("Ok", [ value ]) -> Ok value
  | VVariant ("Error", [ error ]) -> Error error
  | _ -> failwith (name ^ ": expected `Ok(_) or `Error(_)")
;;

let option value ~name =
  match value with
  | VVariant ("None", []) -> None
  | VVariant ("Some", [ value ]) -> Some value
  | _ -> failwith (name ^ ": expected `None or `Some(_)")
;;

module Outcome = struct
  let map value ~f =
    let name = "Outcome.map" in
    match outcome value ~name with
    | Ok payload -> VVariant ("Ok", [ Callback.apply f ~name [ payload ] ])
    | Error _ -> value
  ;;

  let bind value ~f =
    let name = "Outcome.bind" in
    match outcome value ~name with
    | Ok payload ->
      let result = Callback.apply f ~name [ payload ] in
      ignore (outcome result ~name : (value, value) Result.t);
      result
    | Error _ -> value
  ;;

  let map_error value ~f =
    let name = "Outcome.map_error" in
    match outcome value ~name with
    | Ok _ -> value
    | Error payload -> VVariant ("Error", [ Callback.apply f ~name [ payload ] ])
  ;;
end

module Option = struct
  let map value ~f =
    let name = "Option.map" in
    match option value ~name with
    | Some payload -> VVariant ("Some", [ Callback.apply f ~name [ payload ] ])
    | None -> value
  ;;

  let bind value ~f =
    let name = "Option.bind" in
    match option value ~name with
    | Some payload ->
      let result = Callback.apply f ~name [ payload ] in
      ignore (option result ~name : value option);
      result
    | None -> value
  ;;
end

let filter_map values ~f =
  let name = "Array.filter_map" in
  Array.filter_map values ~f:(fun value ->
    option (Callback.apply f ~name [ value ]) ~name)
;;
