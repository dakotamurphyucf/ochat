open Core

type t =
  { major : int
  ; minor : int
  }
[@@deriving compare, equal, sexp]

let initial = { major = 1; minor = 0 }
let ingress_minimum = { major = 1; minor = 1 }
let current = ingress_minimum

let create ~major ~minor =
  if major < 0 || minor < 0
  then Error (Protocol_error.invalid_request "protocol versions must be non-negative")
  else Ok { major; minor }
;;

let incompatible message =
  Protocol_error.create Protocol_error.Incompatible_protocol ~message ~retryable:false ()
;;

let negotiate ~client_min ~client_max ~supported =
  if compare client_min client_max > 0
  then Error (incompatible "client protocol range is inverted")
  else if client_min.major <> client_max.major
  then Error (incompatible "client protocol range crosses a major version")
  else (
    let compatible version =
      version.major = client_min.major
      && compare version client_min >= 0
      && compare version client_max <= 0
    in
    match List.filter supported ~f:compatible |> List.max_elt ~compare with
    | Some version -> Ok version
    | None -> Error (incompatible "no compatible protocol version"))
;;

let is_feature_segment_char = function
  | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false
;;

let is_valid_feature feature =
  String.split feature ~on:'.'
  |> List.for_all ~f:(fun segment ->
    (not (String.is_empty segment)) && String.for_all segment ~f:is_feature_segment_char)
;;

let validate_feature feature =
  if String.is_empty feature || not (is_valid_feature feature)
  then
    Error (Protocol_error.invalid_request "feature must be a lowercase dotted identifier")
  else Ok feature
;;

let to_json t =
  `Object
    [ "major", `Number (Int.to_string t.major); "minor", `Number (Int.to_string t.minor) ]
;;

let fields_of_json = function
  | `Object fields ->
    let names = List.map fields ~f:fst in
    (match List.find_a_dup names ~compare:String.compare with
     | None -> Ok fields
     | Some name ->
       Error (Protocol_error.invalid_request ("duplicate version field: " ^ name)))
  | _ -> Error (Protocol_error.invalid_request "protocol version must be an object")
;;

let decode_component fields name =
  match List.Assoc.find fields name ~equal:String.equal with
  | None -> Error (Protocol_error.invalid_request ("missing version field: " ^ name))
  | Some (`Number value) ->
    (match Int.of_string_opt value with
     | Some value -> Ok value
     | None -> Error (Protocol_error.invalid_request ("invalid version field: " ^ name)))
  | Some _ ->
    Error (Protocol_error.invalid_request ("version field must be an integer: " ^ name))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = fields_of_json json in
  let%bind major = decode_component fields "major" in
  let%bind minor = decode_component fields "minor" in
  create ~major ~minor
;;
