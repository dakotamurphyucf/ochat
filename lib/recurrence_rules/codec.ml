open Core

let invalid message = Error (Timing_error.Invalid message)

let object_fields json ~required =
  match json with
  | `Object fields ->
    let rec validate remaining seen = function
      | [] ->
        if Int.equal remaining 0
        then Ok fields
        else invalid "missing, duplicate or unknown fields"
      | _ when Int.equal remaining 0 -> invalid "missing, duplicate or unknown fields"
      | (key, _) :: rest ->
        if
          List.mem seen key ~equal:String.equal
          || not (List.mem required key ~equal:String.equal)
        then invalid "missing, duplicate or unknown fields"
        else validate (remaining - 1) (key :: seen) rest
    in
    validate (List.length required) [] fields
  | _ -> invalid "expected object"
;;

let field fields name =
  let rec find remaining = function
    | [] -> invalid ("missing field: " ^ name)
    | _ when Int.equal remaining 0 -> invalid "too many object fields"
    | (key, value) :: rest ->
      if String.equal key name then Ok value else find (remaining - 1) rest
  in
  (* Discriminant lookup can precede exact shape validation. Every recurrence
     object has fewer than sixteen fields; never scan an unbounded input here. *)
  find 16 fields
;;

let string = function
  | `String value -> Ok value
  | _ -> invalid "expected string"
;;

let int64 = function
  | `Number encoded when String.length encoded <= 20 ->
    (match Int64.of_string_opt encoded with
     | Some value when String.equal encoded (Int64.to_string value) -> Ok value
     | Some _ | None -> invalid "expected canonical int64")
  | _ -> invalid "expected integer"
;;

let int json =
  let open Result.Let_syntax in
  let%bind value = int64 json in
  match Int64.to_int value with
  | Some value -> Ok value
  | None -> invalid "integer is outside platform range"
;;

let number value = `Number (Int64.to_string value)

let list decode = function
  | `Array values ->
    let rec bounded remaining = function
      | [] -> true
      | _ :: rest -> remaining > 0 && bounded (remaining - 1) rest
    in
    if bounded 64 values
    then List.map values ~f:decode |> Result.all
    else invalid "too many set entries"
  | _ -> invalid "expected array"
;;

let date json =
  let open Result.Let_syntax in
  let%bind encoded = string json in
  if String.length encoded <> 10
  then invalid "expected YYYY-MM-DD date"
  else (
    match Or_error.try_with (fun () -> Date.of_string encoded) with
    | Ok date when String.equal encoded (Date.to_string date) -> Ok date
    | Ok _ | Error _ -> invalid "expected YYYY-MM-DD date")
;;

let ofday json =
  let open Result.Let_syntax in
  let%bind encoded = string json in
  if String.is_empty encoded || String.length encoded > 18
  then invalid "invalid local ofday"
  else (
    match Or_error.try_with (fun () -> Time_ns.Ofday.of_string encoded) with
    | Ok value when Time_ns.Ofday.compare value Time_ns.Ofday.start_of_next_day < 0 ->
      Ok value
    | Ok _ | Error _ -> invalid "invalid local ofday")
;;

let nullable decode = function
  | `Null -> Ok None
  | value -> Result.map (decode value) ~f:Option.some
;;

let version fields =
  let open Result.Let_syntax in
  let%bind encoded = field fields "version" in
  let%bind value = int encoded in
  if Int.equal value 1 then Ok () else invalid "unsupported recurrence codec version"
;;
