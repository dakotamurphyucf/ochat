open! Core
module P = Protocol_error
module Request = Model_spawn_request
module Target = Inference.Request.Target

type t =
  { request : Jsonaf.t
  ; target : Target.t
  }

let request t = t.request
let target t = t.target

let to_json t =
  `Object
    [ "schema_version", `Number "1"
    ; "recipe", `String Request.recipe_name
    ; "request", t.request
    ; "target", Target.to_json t.target
    ]
;;

let validate_envelope json ~limits =
  Document_schema.Json.validate ~limits json
  |> Result.map_error ~f:(function
    | Document_schema.Error.Limit_exceeded _ ->
      P.create
        P.Resource_limit
        ~message:"Developer model call exceeds admitted JSON limits."
        ~retryable:false
        ()
    | _ -> P.invalid_request "Developer model call JSON is invalid.")
;;

let create ~request ~target ~limits =
  let open Result.Let_syntax in
  let value = { request; target } in
  let%bind () = validate_envelope (to_json value) ~limits in
  let%bind _ = Request.admit request ~limits in
  let%map () =
    Target.validate target ~limits
    |> Result.map_error ~f:(fun _ -> P.invalid_request "Invalid model target.")
  in
  value
;;

let of_json json ~limits =
  let open Result.Let_syntax in
  let%bind () = validate_envelope json ~limits in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed fields [ "schema_version"; "recipe"; "request"; "target" ]
  in
  let%bind _version =
    Json_codec.required_as fields "schema_version" (Json_codec.bounded_int ~min:1 ~max:1)
  in
  let%bind recipe = Json_codec.required_as fields "recipe" Json_codec.string in
  let%bind () =
    if String.equal recipe Request.recipe_name
    then Ok ()
    else Error (P.invalid_request "Unsupported developer model recipe.")
  in
  let%bind request = Json_codec.required fields "request" in
  let%bind target =
    Json_codec.required fields "target"
    |> Result.bind ~f:(fun json ->
      Target.of_json json ~limits
      |> Result.map_error ~f:(fun _ -> P.invalid_request "Invalid model target."))
  in
  create ~request ~target ~limits
;;
