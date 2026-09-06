open Core

module Request_id = struct
  type t = Jsonaf.t [@@deriving sexp]

  let of_json = function
    | (`String _ | `Number _) as value -> Ok value
    | _ -> Error (Protocol_error.invalid_request "request id must be a string or number")
  ;;

  let to_json t = t
  let compare left right = String.compare (Jsonaf.to_string left) (Jsonaf.to_string right)
end

type request =
  { id : Request_id.t
  ; method_ : string
  ; params : Jsonaf.t
  }
[@@deriving sexp]

type notification =
  { method_ : string
  ; params : Jsonaf.t
  }
[@@deriving sexp]

type response =
  { id : Request_id.t
  ; outcome : (Jsonaf.t, Protocol_error.t) result
  }
[@@deriving sexp]

type t =
  | Request of request
  | Notification of notification
  | Response of response
[@@deriving sexp]

let request ~id ~method_ ?(params = `Object []) () = Request { id; method_; params }
let notification ~method_ ?(params = `Object []) () = Notification { method_; params }
let success ~id result = Response { id; outcome = Ok result }
let failure ~id error = Response { id; outcome = Error error }

let to_json = function
  | Request request ->
    `Object
      [ "jsonrpc", `String "2.0"
      ; "id", Request_id.to_json request.id
      ; "method", `String request.method_
      ; "params", request.params
      ]
  | Notification notification ->
    `Object
      [ "jsonrpc", `String "2.0"
      ; "method", `String notification.method_
      ; "params", notification.params
      ]
  | Response { id; outcome = Ok result } ->
    `Object [ "jsonrpc", `String "2.0"; "id", Request_id.to_json id; "result", result ]
  | Response { id; outcome = Error error } ->
    `Object
      [ "jsonrpc", `String "2.0"
      ; "id", Request_id.to_json id
      ; "error", Protocol_error.to_json error
      ]
;;

let decode_jsonrpc fields =
  let open Result.Let_syntax in
  let%bind jsonrpc = Json_codec.required fields "jsonrpc" in
  let%bind jsonrpc = Json_codec.string jsonrpc in
  if String.equal jsonrpc "2.0"
  then Ok ()
  else Error (Protocol_error.invalid_request "jsonrpc must equal 2.0")
;;

let decode_params fields =
  match Json_codec.optional fields "params" with
  | None -> Ok (`Object [])
  | Some (`Object _ as params) -> Ok params
  | Some _ -> Error (Protocol_error.invalid_request "params must be an object")
;;

let decode_method fields =
  let open Result.Let_syntax in
  let%bind method_ = Json_codec.required fields "method" in
  let%bind method_ = Json_codec.string method_ in
  if String.is_empty method_
  then Error (Protocol_error.invalid_request "method must be nonempty")
  else Ok method_
;;

let decode_call fields =
  let open Result.Let_syntax in
  let%bind method_ = decode_method fields in
  let%bind params = decode_params fields in
  match Json_codec.optional fields "id" with
  | None -> Ok (Notification { method_; params })
  | Some id ->
    Result.map (Request_id.of_json id) ~f:(fun id -> Request { id; method_; params })
;;

let decode_response fields id =
  let open Result.Let_syntax in
  match Json_codec.optional fields "result", Json_codec.optional fields "error" with
  | Some result, None -> Ok (Response { id; outcome = Ok result })
  | None, Some error ->
    let%map error = Protocol_error.of_json error in
    Response { id; outcome = Error error }
  | _ ->
    Error
      (Protocol_error.invalid_request
         "response must contain exactly one of result or error")
;;

let decode fields =
  let open Result.Let_syntax in
  let%bind () = decode_jsonrpc fields in
  match Json_codec.optional fields "method" with
  | Some _ -> decode_call fields
  | None ->
    let%bind id = Json_codec.required fields "id" in
    let%bind id = Request_id.of_json id in
    decode_response fields id
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  decode fields
;;
