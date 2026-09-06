open! Core
module P = Piaf

let connection_header = "ochat-connection-id"
let protocol_version_header = "ochat-protocol-version"
let error code message = Agent_protocol.Error.create code ~message ~retryable:false ()
let content_type_parts value = String.split value ~on:';' |> List.map ~f:String.strip

let charset_parameter parameter =
  match String.lsplit2 parameter ~on:'=' with
  | Some (name, value) when String.Caseless.equal (String.strip name) "charset" ->
    Some (String.strip value |> String.strip ~drop:(Char.equal '"'))
  | _ -> None
;;

let require_json_content_type request =
  match P.Headers.get_multi (P.Request.headers request) "content-type" with
  | [] -> Error (error Invalid_request "content-type must be application/json")
  | [ value ] ->
    (match content_type_parts value with
     | media_type :: parameters when String.Caseless.equal media_type "application/json"
       ->
       (match List.filter_map parameters ~f:charset_parameter with
        | [] -> Ok ()
        | charsets when List.for_all charsets ~f:(String.Caseless.equal "utf-8") -> Ok ()
        | _ -> Error (error Invalid_request "JSON charset must be UTF-8"))
     | _ -> Error (error Invalid_request "content-type must be application/json"))
  | _ -> Error (error Invalid_request "content-type must appear exactly once")
;;

let require_protocol_version request =
  match P.Headers.get_multi (P.Request.headers request) protocol_version_header with
  | [ "1" ] | [ "1.0" ] -> Ok ()
  | [ _ ] -> Error (error Incompatible_protocol "unsupported HTTP protocol version")
  | [] -> Error (error Incompatible_protocol "HTTP protocol version is required")
  | _ -> Error (error Invalid_request "HTTP protocol version must appear exactly once")
;;

let bearer_token request =
  match P.Headers.get_multi (P.Request.headers request) "authorization" with
  | [] -> Ok None
  | [ value ] ->
    (match String.lsplit2 (String.strip value) ~on:' ' with
     | Some (scheme, token)
       when String.Caseless.equal scheme "Bearer"
            && (not (String.is_empty token))
            && String.for_all token ~f:(Fn.non Char.is_whitespace) -> Ok (Some token)
     | _ -> Error ())
  | _ -> Error ()
;;

let content_length_exceeds request max_body_bytes =
  P.Headers.get (P.Request.headers request) "content-length"
  |> Option.exists ~f:(fun encoded ->
    Option.value_map (Int.of_string_opt encoded) ~default:false ~f:(fun length ->
      length > max_body_bytes))
;;

let request_body ~max_body_bytes request =
  if max_body_bytes <= 0
  then Error (error Internal_error "HTTP body limit must be positive")
  else if content_length_exceeds request max_body_bytes
  then Error (error Resource_limit "HTTP request body exceeds the configured limit")
  else (
    match P.Body.to_string (P.Request.body request) with
    | Error failure -> Error (error Invalid_request (P.Error.to_string failure))
    | Ok body when String.length body > max_body_bytes ->
      Error (error Resource_limit "HTTP request body exceeds the configured limit")
    | Ok body -> Ok body)
;;

let event_cursor request =
  let header = P.Headers.get (P.Request.headers request) "last-event-id" in
  let query = Uri.get_query_param (P.Request.uri request) "after_sequence" in
  if Option.both header query |> Option.exists ~f:(fun (a, b) -> not (String.equal a b))
  then Error (error Invalid_request "event cursors disagree")
  else (
    match Option.first_some header query with
    | None -> Ok None
    | Some encoded ->
      (match Int64.of_string_opt encoded with
       | Some value when Int64.(value >= zero) -> Ok (Some value)
       | _ -> Error (error Invalid_request "event cursor is invalid")))
;;
